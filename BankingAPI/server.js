// server.js
const express = require("express")
const cors = require("cors")
const { exec } = require("child_process")
const fs = require("fs")
const path = require("path")

const app = express()
const port = 3001

app.use(cors())
app.use(express.json())

const root = path.join(__dirname, "..")
const dispatcherPath = path.join(root, "bin", "DISPATCHER")
const jsonOutputPath = path.join(root, "files", "tables.json")
const cobLibraryPath = path.join(root, "lib")

const execOptions = {
  cwd: root,
  env: {
    ...process.env,
    RUN_DBNAME: "pocbanking@127.0.0.1:5432",
    RUN_USERNAME: "postgres",
    RUN_PASSWD: "postgres",
    COB_LIBRARY_PATH: cobLibraryPath,
  },
}

/* ========================= */
/* Helpers pt query & paginare */
/* ========================= */
const one = (v, dflt = "") => (Array.isArray(v) ? (v[0] ?? dflt) : (v ?? dflt))
const getPage = (v) => {
  const s = String(one(v, "1")).trim()
  const n = /^\d+$/.test(s) ? Number.parseInt(s, 10) : 1
  return Math.max(1, Math.min(99999, n))
}
// Normalizează instant query-urile din Express (ex: ?userId=1&userId=1)
app.use((req, _res, next) => {
  for (const k of Object.keys(req.query)) {
    const v = req.query[k]
    if (Array.isArray(v)) req.query[k] = v[0]
  }
  next()
})

/* ========================= */
/* Fixed-width builders */
/* ========================= */
const LZ = (v, n) =>
  String(v ?? "")
    .trim()
    .padStart(n, "0") // left pad cu 0
const R = (v, n) =>
  String(v ?? "")
    .slice(0, n)
    .padEnd(n, " ") // right pad cu spații
const M8 = (s) => R(String(s).toUpperCase(), 8) // METHOD X(08)
const O10 = (s) => R(String(s).toUpperCase(), 10) // OBJECT X(10)
const PAGE_MAX = 99999
const PAGE_SIZE = 10

// Header: 5-digit bank user id + 1 spațiu + method(8) + object(10)
const header = (bankUserId5, method, object) => `${LZ(bankUserId5 || 0, 5)} ${M8(method)}${O10(object)}`

// PIC 9(08)V99 helpers (mereu 10 digits, cenți)
const PIC_9_08_V99_MAX = 99999999.99
const toPic_9_08_V99 = (n) => {
  const x = Math.max(0, Math.min(Number(n) || 0, PIC_9_08_V99_MAX))
  const cents = Math.round(x * 100)
  return String(cents).padStart(10, "0")
}
const parsePic_9_08_V99 = (v) => {
  if (v == null) return 0
  const s = String(v).trim()
  if (!s) return 0
  if (/^\d+$/.test(s)) return Number.parseInt(s, 10) / 100
  return Number.parseFloat(s.replace(",", ".")) || 0
}

// Command builders pt COBOL
const cmdLogin = (username, password) =>
  `${header("00000", "POST", "LOGIN")}${R(username, 50)}${R(password, 60)}${R("", 84)}`

// CUSTOMERS
const cmdGetCustomers = (caller5, page = 1) => `${header(caller5, "GET", "CUSTOMERS")}${LZ(page, 5)}${R("", 189)}`

const cmdGetCustomer = (caller5, custId5, page = 1) =>
  `${header(caller5, "GET", "CUSTOMER")}${LZ(custId5, 5)} ${LZ(page, 5)}${R("", 183)}`

const cmdPostCustomer = (caller5, assignedBankUserId5, username, address) =>
  `${header(caller5, "POST", "CUSTOMER")}${LZ(assignedBankUserId5, 5)}${R(username, 50)}${R(address, 50)}${R("", 89)}`

const cmdPutCustomer = (caller5, custId5, username, address) =>
  `${header(caller5, "PUT", "CUSTOMER")}${LZ(custId5, 5)}${R(username, 50)}${R(address, 50)}${R("", 89)}`

const cmdDelCustomer = (caller5, custId5) => `${header(caller5, "DELETE", "CUSTOMER")}${LZ(custId5, 5)}${R("", 189)}`

// --- builders ACCOUNTS ---
// by customer: CUSTID(5) + ' ' + PAGE(5) + filler(183)
const cmdGetAccountsByCustomer = (caller5, custId5, page = 1) =>
  `${header(caller5, "GET", "ACCOUNTS")}${LZ(custId5, 5)} ${LZ(page, 5)}${R("", 183)}`

// all accounts: CUSTID blank(5) + ' ' + PAGE(5) + filler(183)  |  fără pagină: filler(194)
const cmdGetAccountsAll = (caller5, page = null) => {
  if (page != null) {
    return `${header(caller5, "GET", "ACCOUNTS")}${R("", 5)} ${LZ(page, 5)}${R("", 183)}`
  }
  return `${header(caller5, "GET", "ACCOUNTS")}${R("", 194)}`
}

const cmdPostAccount = (caller5, custId5, iban, currency) =>
  `${header(caller5, "POST", "ACCOUNT")}${LZ(custId5, 5)} ${R(String(iban).replace(/\s+/g, ""), 30)} ${R(String(currency).toUpperCase(), 3)}${R("", 154)}`

const cmdPutAccount = (caller5, accId5, balance) =>
  `${header(caller5, "PUT", "ACCOUNT")}${LZ(accId5, 5)} ${toPic_9_08_V99(balance)}${R("", 178)}`

const cmdDelAccount = (caller5, accId5) => `${header(caller5, "DELETE", "ACCOUNT")}${LZ(accId5, 5)}${R("", 189)}`

// TRANSACTIONS
const cmdGetTrans = (caller5, custId5, accId5, page) => {
  const pg = Math.max(1, Math.min(PAGE_MAX, Number.parseInt(String(page || "1"), 10) || 1))
  return `${header(caller5, "GET", "TRANS")}${LZ(custId5, 5)}${LZ(accId5, 5)} ${LZ(pg, 5)}${R("", 178)}`
}

const cmdPostTrans = (caller5, custId5, accId5, type10, amount) =>
  `${header(caller5, "POST", "TRANS")}${LZ(custId5, 5)}${LZ(accId5, 5)}${R(String(type10).toUpperCase(), 10)}${toPic_9_08_V99(amount)}${LZ(caller5, 5)}${R("", 159)}`

// TRANSFERS
const stripIban = (s) => String(s || "").replace(/\s+/g, "")
const cmdTransfer = (caller5, fromIban, toIban, amount) => {
  const from = stripIban(fromIban)
  const to = stripIban(toIban)
  return `${header(caller5, "POST", "TRANSFER")}${R(from, 30)} ${R(to, 30)} ${toPic_9_08_V99(amount)}${R("", 122)}`
}

// BANKUSER (BUSRBO)
const ROLE_MAP = { BaAd: "ADMIN", BaTe: "TELLER", BaCl: "CLIENT" }
const ROLE_CODE_MAP = {
  ADMIN: "BaAd",
  TELLER: "BaTe",
  CLIENT: "BaCl",
  BAAD: "BaAd",
  BATE: "BaTe",
  BACL: "BaCl",
}
const toRoleCode = (v) => {
  const s = String(v || "")
    .trim()
    .toUpperCase()
  if (ROLE_CODE_MAP[s]) return ROLE_CODE_MAP[s]
  if (/^BA(AD|TE|CL)$/.test(s)) return s[0] + s[1].toLowerCase() + s.slice(2) // BaAd/BaTe/BaCl
  throw new Error("Invalid role. Use ADMIN/TELLER/CLIENT or BaAd/BaTe/BaCl")
}

const cmdGetBankUsers = (caller5, page = 1) => `${header(caller5, "GET", "USERS")}${LZ(page, 5)}${R("", 189)}`

const cmdPostBankUser = (caller5, username, password, roleCode) =>
  `${header(caller5, "POST", "USER")}${R(username, 50)} ${R(password, 60)} ${R(roleCode, 4)}${R("", 78)}`

const cmdPutBankUser = (caller5, id5, username, roleCode) =>
  `${header(caller5, "PUT", "USER")}${LZ(id5, 5)} ${R(username, 50)} ${R(roleCode, 4)}${R("", 133)}`

const cmdDelBankUser = (caller5, id5) => `${header(caller5, "DELETE", "USER")}${LZ(id5, 5)}${R("", 189)}`

/* ========================= */
/* Logging utilities */
/* ========================= */
const VERBOSE = process.env.VERBOSE_LOG !== "0"
let REQ_COUNTER = 0

const colors = {
  dim: "\x1b[2m",
  gray: "\x1b[90m",
  cyan: "\x1b[36m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  red: "\x1b[31m",
  magenta: "\x1b[35m",
  reset: "\x1b[0m",
  bold: "\x1b[1m",
}
const ts = () => new Date().toISOString()
const clip = (s, n = 1200) => (s && s.length > n ? s.slice(0, n) + "…[truncated]" : s || "")
const log = (...args) => {
  if (VERBOSE) console.log(...args)
}
const logReq = (id, msg, extra) =>
  log(
    `${colors.cyan}[${ts()}] [REQ ${id}]${colors.reset} ${msg}` +
      (extra ? ` ${colors.gray}${clip(JSON.stringify(extra))}${colors.reset}` : ""),
  )
const logRes = (id, status, ms) =>
  log(`${colors.green}[${ts()}] [RES ${id}]${colors.reset} status=${status} in ${ms.toFixed(1)}ms`)
const logCmd = (id, phase, msg) => log(`${colors.magenta}[${ts()}] [CMD ${id}]${colors.reset} ${phase}: ${msg}`)
const maskSecrets = (obj) => {
  try {
    const c = JSON.parse(JSON.stringify(obj || {}))
    for (const k of Object.keys(c || {})) if (k.toLowerCase().includes("pass")) c[k] = "*****"
    return c
  } catch {
    return {}
  }
}

// request correlation
app.use((req, res, next) => {
  const id = String(++REQ_COUNTER).padStart(6, "0")
  req.__id = id
  const started = process.hrtime.bigint()
  logReq(id, `${req.method} ${req.originalUrl}`, { query: req.query, body: maskSecrets(req.body) })
  res.on("finish", () => {
    const durMs = Number(process.hrtime.bigint() - started) / 1e6
    logRes(id, res.statusCode, durMs)
  })
  next()
})

/* ========================= */
/* COBOL error handling utils */
/* ========================= */
class CobolError extends Error {
  constructor(code, rawMsg, publicMessage, status = 400) {
    super(publicMessage || rawMsg || code || "Operation failed")
    this.name = "CobolError"
    this.code = code || ""
    this.rawMsg = rawMsg || ""
    this.publicMessage = publicMessage || this.message
    this.status = status
    this.__cobol = true
  }
}
function parseCobolStdout(stdout) {
  const errMatch = stdout.match(/<ERR_START>([\s\S]*?)<ERR_END>/)
  const errRaw = (errMatch?.[1] || "").trim() || "0000"
  const codeMatch = errRaw.match(/\b([A-Z]{2}\d{2})[A-Z]*/)
  const errCode = codeMatch?.[1] || (errRaw === "0000" ? "0000" : errRaw)

  let errMsg = ""
  if (errRaw && errRaw !== "0000") {
    const parts = errRaw.split(":")
    errMsg = parts.length > 1 ? parts.slice(1).join(":").trim() : errRaw
  }

  const oMatch = stdout.match(/<O_START>([\s\S]*?)<O_END>/)
  const payloadRaw = oMatch?.[1] || null

  return { errCode: errCode, errMsg, payloadRaw, errRaw }
}
function statusForCobolCode(code) {
  if (!code || code === "0000") return 200
  if (/^US0[23]$/.test(code)) return 401
  if (/^(AU|DP05)/.test(code)) return 403
  if (/^AC/.test(code)) return 400
  if (/^TF/.test(code)) return 400
  if (/^(US06|US07)/.test(code)) return 400
  if (code === "DB90") return 404
  if (/^DB|^SQL/.test(code)) return 500
  if (/^DP|^G9/.test(code)) return 500
  return 400
}
const COBOL_MESSAGE_MAP = {
  AC01: "Amount not numeric.",
  AC02: "Transaction type '%1' not allowed for this operation.",
  AC03: "Insufficient funds. Balance: %1, attempted withdrawal: %2.",
  AC04: "Transaction list is full. Cannot retrieve more records.",
  AC05: "Acting client has no linked customer row.",
  AC06: "Permission denied. Action of Teller to another Teller not allowed.",
  AC07: "Account doesn't belong to the logged in client.",
  AC08: "Page number contains invalid characters. It must be numeric. <00001-99999>",
  CU01: "Method '%1' not allowed for this object.",
  US01: "Method '%1' not allowed for this operation.",
  US02: "User '%1' has not been found.",
  US03: "Bad password for user '%1'.",
  US04: "Permission denied.",
  US05: "You can only get user details or login.",
  US06: "Unknown role.",
  US07: "Changing role not allowed.",
  TF01: "Transfer method not allowed.",
  TF02: "Currency mismatch between source and destination.",
  TF03: "Insufficient balance for transfer.",
  DB90: "The searched object was not found in the database or the table is empty.",
  DB91: "Operation failed: Multiple records found for a unique ID.",
  DB92: "Database environment variables are missing or invalid.",
  DB93: "Unique constraint violation.",
  DB97: "Database connection error. Check credentials and server status.",
  DB98: "An internal error occurred in the data access module.",
  DB99: "A general SQL query error occurred in database.",
  DP01: "The requested object is not valid.",
  DP02: "Unable to process the request for object '%1'.",
  DP03: "USERID at beginning non-existent.",
  DP04: "Error while logging in.",
  DP05: "No permission to execute the requested operation.",
  AU01: "Permission denied. Action for a user with role '%1' not allowed.",
  AU02: "Permission denied. Action of Teller to Admin not allowed",
  AU03: "Permission denied. Action of Teller to another Teller not allowed.",
  AU04: "Permission denied. Action of Teller to '%1' not allowed.",
  AU05: "Permission denied. Action of Client to any other user than self not allowed.",
  G90: "No data found for the JSON file.",
  G91: "Some data is being handled wrongly for the writting of JSON File.",
  G92: "Data not written in JSON File.",
  SQL01: "Error executing SQL command: '%1'",
}
function fillTemplate(template, rawMsg) {
  if (!template) return rawMsg || ""
  const args = Array.from(String(rawMsg || "").matchAll(/'([^']+)'/g)).map((m) => m[1])
  let out = template
  args.forEach((a, i) => {
    out = out.replace(new RegExp(`%${i + 1}`, "g"), a)
  })
  return out
}
function mapCobolError(errCode, errMsg, errRaw) {
  if (!errCode || errCode === "0000") return null
  const template = COBOL_MESSAGE_MAP[errCode]
  const friendly = template ? fillTemplate(template, errRaw || errMsg) : errMsg || errRaw || `Error ${errCode}`
  const status = statusForCobolCode(errCode)
  return new CobolError(errCode, errMsg, friendly, status)
}
function handleRouteError(res, e) {
  if (e && e.__cobol) {
    return res.status(e.status || 400).json({ message: e.publicMessage, code: e.code })
  }
  return res.status(500).json({ message: e?.message || "Internal server error" })
}

/* ========================= */
/* Exec & serialization lock */
/* ========================= */
let chain = Promise.resolve()
function runExclusive(fn) {
  const p = chain.then(fn, fn)
  chain = p.catch(() => {})
  return p
}
function executeDispatcherAndGetJSON(command, ctxId = "GLOBAL") {
  return runExclusive(
    () =>
      new Promise((resolve, reject) => {
        logCmd(ctxId, "LOCK", "acquired")
        try {
          fs.writeFileSync(jsonOutputPath, "")
        } catch (_) {}

        const t0 = process.hrtime.bigint()
        logCmd(ctxId, "EXEC", `${colors.dim}${command}${colors.reset}`)
        exec(command, execOptions, (error, stdout, stderr) => {
          const execMs = Number(process.hrtime.bigint() - t0) / 1e6
          if (stdout?.trim()) logCmd(ctxId, "STDOUT", "\n" + clip(stdout.trim()))
          if (stderr?.trim()) logCmd(ctxId, "STDERR", "\n" + clip(stderr.trim()))
          logCmd(ctxId, "EXEC", `done in ${execMs.toFixed(1)}ms`)

          if (error) {
            logCmd(ctxId, "ERROR", error.message)
            return reject(new Error(stderr || error.message))
          }

          const { errCode, errMsg, errRaw } = parseCobolStdout(String(stdout || ""))
          const cobErr = mapCobolError(errCode, errMsg, errRaw)
          if (cobErr) return reject(cobErr)

          fs.readFile(jsonOutputPath, "utf8", (err, data) => {
            if (err) return reject(err)
            if (!data) return reject(new Error("COBOL produced no output"))
            try {
              const json = JSON.parse(data)
              resolve(json)
            } catch {
              reject(new Error("Failed to parse COBOL JSON"))
            }
          })
        })
      }),
  )
}

/* ========================= */
/* Normalizers */
/* ========================= */
const pad5 = (v) => LZ(one(v, ""), 5)

function normalizeUserRow(r) {
  const id5 = pad5(r.ID)
  return { ID: id5, BANKUSERID: id5, USERNAME: String(r.USERNAME).trim(), ROLE: ROLE_MAP[r.ROLE] ?? "UNKNOWN" }
}
function normalizeCustomerRow(r) {
  let rawBU = r.BANKUSERID ?? r.BANKUSER ?? r.USERID ?? r.BUSRID ?? r.bankuserid ?? r.busrid
  if (!rawBU && typeof r === "object") {
    const allKeys = Object.keys(r)
    for (const key of allKeys) {
      const value = r[key]
      if (typeof value === "string" && value.length >= 5) {
        const match = value.match(/(\d{5})/)
        if (match && match[1] !== "00000" && match[1] !== r.ID) {
          rawBU = match[1]
          break
        }
      }
    }
  }
  const hasValidBU = rawBU != null && String(rawBU).trim() !== "" && !/^0+$/.test(String(rawBU).trim())
  const bu5 = hasValidBU ? pad5(rawBU) : "00000"
  return {
    ID: pad5(r.ID),
    USERNAME: String(r.USERNAME).trim(),
    ADDRESS: String(r.ADDRESS).trim(),
    BANKUSERID: bu5,
  }
}
function normalizeAccountRow(r) {
  return {
    ID: pad5(r.ID ?? r.ACCOUNTID ?? r["ACCOUNT ID"]),
    CUSTOMERID: pad5(r.CUSTOMERID ?? r.CUSTOMER ?? r["CUSTOMER ID"]),
    IBAN: String(r.IBAN ?? "").trim(),
    CURRENCY: String(r.CURRENCY ?? r.CUR ?? "USD")
      .trim()
      .toUpperCase(),
    BALANCE: parsePic_9_08_V99(r.BALANCE ?? r.ACCBALANCE ?? r["ACC BALANCE"] ?? r["BALANCE"] ?? 0),
  }
}

/* ========================= */
/* Paged low-level getters */
/* ========================= */
async function getCustomersPageRaw(caller5, page = 1, ctx = "SYS") {
  const cmd = `${dispatcherPath} "${cmdGetCustomers(caller5, page)}"`
  logCmd(ctx, "GET_CUSTOMERS_CMD", cmd)
  const data = await executeDispatcherAndGetJSON(cmd, ctx)
  return Array.isArray(data.records) ? data.records : []
}
async function getUsersPageRaw(caller5, page = 1, ctx = "SYS") {
  const cmd = `${dispatcherPath} "${cmdGetBankUsers(caller5, page)}"`
  logCmd(ctx, "GET_USERS_CMD", cmd)
  const data = await executeDispatcherAndGetJSON(cmd, ctx)
  return Array.isArray(data.records) ? data.records : []
}
async function getAccountsByCustomerPageRaw(caller5, cust5, page = 1, ctx = "SYS") {
  const cmd = `${dispatcherPath} "${cmdGetAccountsByCustomer(caller5, cust5, page)}"`
  logCmd(ctx, "GET_ACCOUNTS_CMD", cmd)
  const data = await executeDispatcherAndGetJSON(cmd, ctx)
  return Array.isArray(data.records) ? data.records : []
}
async function getAccountsAllPageRaw(caller5, page = null, ctx = "SYS") {
  const cmd = `${dispatcherPath} "${cmdGetAccountsAll(caller5, page)}"`
  logCmd(ctx, "GET_ACCOUNTS_ALL_CMD", cmd)
  const data = await executeDispatcherAndGetJSON(cmd, ctx)
  return Array.isArray(data.records) ? data.records : []
}

/* ========================= */
/* Helpers pentru scanări */
/* ========================= */
async function scanCustomers(caller5, ctx, visitRow, maxPages = 200) {
  for (let p = 1; p <= maxPages; p++) {
    const rows = await getCustomersPageRaw(caller5, p, ctx)
    if (!rows.length) break
    for (const r of rows) {
      const stop = await visitRow(r, p)
      if (stop === true) return true
    }
    if (rows.length < PAGE_SIZE) break
  }
  return false
}
async function scanAccountsForCustomer(caller5, cust5, ctx, visitRow, maxPages = 200) {
  for (let p = 1; p <= maxPages; p++) {
    const rows = await getAccountsByCustomerPageRaw(caller5, cust5, p, ctx)
    if (!rows.length) break
    for (const r of rows) {
      const stop = await visitRow(r, p)
      if (stop === true) return true
    }
    if (rows.length < PAGE_SIZE) break
  }
  return false
}
async function getAccountByIbanNormalized(caller5, iban, ctx = "SYS") {
  const tgt = stripIban(iban).toUpperCase()
  let found = null
  await scanCustomers(caller5, ctx, async (custRow) => {
    const custId5 = pad5(custRow.ID)
    await scanAccountsForCustomer(caller5, custId5, ctx, (accRow) => {
      const acc = normalizeAccountRow(accRow)
      if (stripIban(acc.IBAN).toUpperCase() === tgt) {
        found = acc
        return true
      }
      return false
    })
    return !!found
  })
  return found
}
async function getAccountByIdAcrossCustomers(caller5, accountId5, ctx = "SYS") {
  let found = null
  await scanCustomers(caller5, ctx, async (custRow) => {
    const custId5 = pad5(custRow.ID)
    await scanAccountsForCustomer(caller5, custId5, ctx, (accRow) => {
      const acc = normalizeAccountRow(accRow)
      if (pad5(acc.ID) === pad5(accountId5)) {
        found = acc
        return true
      }
      return false
    })
    return !!found
  })
  return found
}

/* ====== */
/* Routes */
/* ====== */
app.get("/healthz", (_req, res) => res.status(200).send("ok"))

/* LOGIN */
app.post("/login", async (req, res) => {
  const rid = req.__id
  try {
    const { username, password } = req.body || {}
    if (!username || !password) return res.status(400).json({ message: "Username and password are required" })

    const cmd = `${dispatcherPath} "${cmdLogin(username, password)}"`
    logCmd(rid, "EXEC", cmd)

    exec(cmd, execOptions, (error, stdout, stderr) => {
      if (stderr?.trim()) logCmd(rid, "STDERR", "\n" + stderr.trim())
      if (error) {
        logCmd(rid, "ERROR", error.message)
        return res.status(500).json({ message: "Login failed to execute." })
      }
      if (stdout?.trim()) logCmd(rid, "STDOUT", "\n" + stdout.trim())

      const { errCode, errMsg, errRaw, payloadRaw } = parseCobolStdout(String(stdout || ""))
      const cobErr = mapCobolError(errCode, errMsg, errRaw)
      if (cobErr) return res.status(cobErr.status).json({ message: cobErr.publicMessage, code: cobErr.code })
      if (!payloadRaw) return res.status(500).json({ message: "Login succeeded but no payload was returned." })

      const id4 = payloadRaw.slice(0, 4).trim()
      const user = payloadRaw.slice(4, 54).trim()
      const role4 = payloadRaw.slice(54).trim()

      return res.json({
        bankUserId: String(id4 || "").padStart(5, "0"),
        username: user,
        role: ROLE_MAP[role4] || role4 || "USER",
      })
    })
  } catch (e) {
    return res.status(500).json({ message: "Login failed. Please check credentials or contact support." })
  }
})

/* BANK USERS */
app.get("/users", async (req, res) => {
  try {
    const caller5 = pad5(one(req.query.userId) || "00001")
    const page = getPage(req.query.page)
    const rows = await getUsersPageRaw(caller5, page, req.__id)
    const items = rows.map(normalizeUserRow)
    res.json({
      page,
      pageSize: PAGE_SIZE,
      count: items.length,
      hasPrev: page > 1,
      hasNext: items.length === PAGE_SIZE,
      items,
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.post("/users", async (req, res) => {
  const rid = req.__id
  try {
    const { USERNAME, PASSWORD, ROLE, userId } = req.body || {}
    if (!USERNAME || !PASSWORD || !ROLE)
      return res.status(400).json({ message: "USERNAME, PASSWORD și ROLE sunt obligatorii" })

    const caller5 = pad5(userId || "00001")
    let roleCode
    try {
      roleCode = toRoleCode(ROLE)
    } catch {
      return res.status(400).json({ message: "Invalid role. Use ADMIN/TELLER/CLIENT or BaAd/BaTe/BaCl" })
    }

    const cmd = `${dispatcherPath} "${cmdPostBankUser(caller5, USERNAME, PASSWORD, roleCode)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)

    res.status(201).json({ USERNAME, ROLE: ROLE_MAP[roleCode] || roleCode })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.put("/users/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const { USERNAME, ROLE, userId } = req.body || {}
    if (!USERNAME || !ROLE) return res.status(400).json({ message: "USERNAME și ROLE sunt obligatorii" })

    const caller5 = pad5(userId || "00001")
    let roleCode
    try {
      roleCode = toRoleCode(ROLE)
    } catch {
      return res.status(400).json({ message: "Invalid role. Use ADMIN/TELLER/CLIENT or BaAd/BaTe/BaCl" })
    }

    const cmd = `${dispatcherPath} "${cmdPutBankUser(caller5, id5, USERNAME, roleCode)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)

    res.json({ ID: id5, USERNAME, ROLE: ROLE_MAP[roleCode] || roleCode })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.delete("/users/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const caller5 = pad5(one(req.query.userId) || "00001")
    const cmd = `${dispatcherPath} "${cmdDelBankUser(caller5, id5)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)
    res.status(204).end()
  } catch (e) {
    return handleRouteError(res, e)
  }
})

/* CUSTOMERS */
app.get("/customers", async (req, res) => {
  const rid = req.__id
  try {
    const caller5 = pad5(one(req.query.userId) || "00001")
    const page = getPage(req.query.page)

    // Pasul 1: Preluăm toți utilizatorii de bancă, la fel ca înainte.
    const allBankUsersRaw = await getAllUsersRaw(caller5, rid)
    const bankUsers = allBankUsersRaw.map(normalizeUserRow)

    // ======================== MODIFICARE CHEIE ========================
    // Pasul 2: Creăm un Map pentru căutare rapidă, dar de data aceasta
    // cheia va fi USERNAME-ul (convertit la litere mici pentru siguranță),
    // nu ID-ul.
    const usersByUsernameMap = new Map(bankUsers.map((user) => [user.USERNAME.toLowerCase(), user]))
    // =================================================================

    // Pasul 3: Preluăm pagina curentă de clienți.
    const custRowsRaw = await getCustomersPageRaw(caller5, page, rid)

    // Pasul 4: Îmbogățim fiecare client cu datele utilizatorului asociat prin USERNAME.
    const items = custRowsRaw.map((rawRow) => {
      const customer = normalizeCustomerRow(rawRow)

      // ======================== MODIFICARE CHEIE ========================
      // Căutăm utilizatorul în Map folosind USERNAME-ul clientului.
      const assignedUser = usersByUsernameMap.get(customer.USERNAME.toLowerCase()) || null
      // =================================================================

      return {
        ...customer,
        // Actualizăm BANKUSERID-ul clientului cu ID-ul găsit, dacă există.
        // Acest lucru repară datele pentru frontend.
        BANKUSERID: assignedUser ? assignedUser.ID : customer.BANKUSERID,
        bankUser: assignedUser
          ? {
              username: assignedUser.USERNAME,
              role: assignedUser.ROLE,
            }
          : null,
      }
    })

    res.json({
      page,
      pageSize: PAGE_SIZE,
      count: items.length,
      hasPrev: page > 1,
      hasNext: items.length === PAGE_SIZE,
      items,
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.post("/customers", async (req, res) => {
  const rid = req.__id
  try {
    const { USERNAME, ADDRESS, PASSWORD, ROLE = "CLIENT", userId } = req.body || {}
    if (!USERNAME || !ADDRESS || !PASSWORD) {
      return res.status(400).json({ message: "USERNAME, ADDRESS, and PASSWORD are required" })
    }

    const caller5 = pad5(userId || "00001")

    // Step 1: Create the bank user first
    let roleCode
    try {
      roleCode = toRoleCode(ROLE)
    } catch {
      return res.status(400).json({ message: "Invalid role. Use ADMIN/TELLER/CLIENT or BaAd/BaTe/BaCl" })
    }

    const userCmd = `${dispatcherPath} "${cmdPostBankUser(caller5, USERNAME, PASSWORD, roleCode)}"`
    logCmd(rid, "CREATE_USER", userCmd)

    try {
      await executeDispatcherAndGetJSON(userCmd, rid)
    } catch (e) {
      if (e && e.__cobol) {
        return res.status(e.status).json({
          message: `Failed to create user: ${e.publicMessage}`,
          code: e.code,
          stage: "CREATE_USER",
        })
      }
      return res.status(500).json({
        message: `Failed to create user: ${e?.message || "Unknown error"}`,
        stage: "CREATE_USER",
      })
    }

    // Step 2: Find the created user to get their ID
    let createdUserId = "00000"
    try {
      const allUsers = await getAllUsersRaw(caller5, rid)
      const createdUser = allUsers.find((u) => u.USERNAME.toLowerCase() === USERNAME.toLowerCase())
      if (createdUser) {
        createdUserId = pad5(createdUser.ID)
      }
    } catch (e) {
      logCmd(rid, "WARNING", "Could not retrieve created user ID, using default")
    }

    // Step 3: Create the customer with the user ID
    const customerCmd = `${dispatcherPath} "${cmdPostCustomer(caller5, createdUserId, USERNAME, ADDRESS)}"`
    logCmd(rid, "CREATE_CUSTOMER", customerCmd)

    try {
      await executeDispatcherAndGetJSON(customerCmd, rid)
    } catch (e) {
      if (e && e.__cobol) {
        return res.status(e.status).json({
          message: `User created but customer creation failed: ${e.publicMessage}`,
          code: e.code,
          stage: "CREATE_CUSTOMER",
          userId: createdUserId,
        })
      }
      return res.status(500).json({
        message: `User created but customer creation failed: ${e?.message || "Unknown error"}`,
        stage: "CREATE_CUSTOMER",
        userId: createdUserId,
      })
    }

    return res.status(201).json({
      USERNAME,
      ADDRESS,
      BANKUSERID: createdUserId,
      ROLE: ROLE_MAP[roleCode] || roleCode,
      message: "User and customer created successfully",
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.put("/customers/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const { USERNAME, ADDRESS, userId } = req.body || {}
    if (!USERNAME || !ADDRESS) return res.status(400).json({ message: "USERNAME and ADDRESS are required" })

    const caller5 = pad5(userId || "00001")
    const cmd = `${dispatcherPath} "${cmdPutCustomer(caller5, id5, USERNAME, ADDRESS)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)
    res.json({ ID: id5, USERNAME, ADDRESS })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.delete("/customers/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const caller5 = pad5(one(req.query.userId) || "00001")
    const cmd = `${dispatcherPath} "${cmdDelCustomer(caller5, id5)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)
    res.status(204).end()
  } catch (e) {
    return handleRouteError(res, e)
  }
})
// Adaugă această funcție nouă în server.js
async function getAllUsersRaw(caller5, ctx = "SYS") {
  const allUsers = []
  for (let p = 1; p <= 200; p++) {
    // Limită la 200 de pagini pentru siguranță
    const pageRows = await getUsersPageRaw(caller5, p, ctx)
    if (pageRows.length === 0) {
      break // Nu mai sunt pagini
    }
    allUsers.push(...pageRows)
    if (pageRows.length < PAGE_SIZE) {
      break // Ultima pagină
    }
  }
  return allUsers
}
/* ACCOUNTS */
app.get("/accounts", async (req, res) => {
  try {
    const caller5 = pad5(one(req.query.userId) || "00001")
    const rawCustomer = one(req.query.customerId)
    const hasCustomer = /^\d+$/.test(String(rawCustomer || "").trim())
    const page = req.query.page != null ? getPage(req.query.page) : null

    // DEBUG: vezi comanda exactă ce pleacă spre DISPATCHER
    const previewCmd = hasCustomer
      ? `${dispatcherPath} "${cmdGetAccountsByCustomer(caller5, pad5(rawCustomer), page ?? 1)}"`
      : `${dispatcherPath} "${cmdGetAccountsAll(caller5, page)}"`
    logCmd(req.__id, "GET_ACCOUNTS_CMD_PREVIEW", previewCmd)

    const rows = hasCustomer
      ? await getAccountsByCustomerPageRaw(caller5, pad5(rawCustomer), page ?? 1, req.__id)
      : await getAccountsAllPageRaw(caller5, page, req.__id)

    const items = rows.map(normalizeAccountRow)
    return res.json({
      page: page ?? 1,
      pageSize: PAGE_SIZE,
      count: items.length,
      hasPrev: (page ?? 1) > 1,
      hasNext: items.length === PAGE_SIZE,
      items,
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})

app.post("/accounts", async (req, res) => {
  const rid = req.__id
  const isCobolErr = (e) => e && e.__cobol === true

  try {
    const { CUSTOMERID, IBAN, CURRENCY = "USD", BALANCE = "0", userId } = req.body || {}
    if (!CUSTOMERID || !IBAN) return res.status(400).json({ message: "CUSTOMERID and IBAN are required" })

    const caller5 = pad5(userId || "00001")
    const cust5 = pad5(CUSTOMERID)

    const createCmd = `${dispatcherPath} "${cmdPostAccount(caller5, cust5, IBAN, CURRENCY)}"`
    logCmd(rid, "BUILD", createCmd)

    try {
      await executeDispatcherAndGetJSON(createCmd, rid)
    } catch (e) {
      if (isCobolErr(e)) {
        return res.status(e.status).json({
          message: `Failed to create account: ${e.publicMessage}`,
          code: e.code,
          stage: "CREATE_ACCOUNT",
        })
      }
      return res.status(500).json({
        message: `Failed to create account: ${e?.message || "Unknown error"}`,
        stage: "CREATE_ACCOUNT",
      })
    }

    const balNum = Number(BALANCE)
    if (balNum > 0) {
      let created = null
      await scanAccountsForCustomer(caller5, cust5, rid, (accRow) => {
        const acc = normalizeAccountRow(accRow)
        if (stripIban(acc.IBAN) === stripIban(IBAN)) {
          created = acc
          return true
        }
        return false
      })

      if (created?.ID) {
        const putCmd = `${dispatcherPath} "${cmdPutAccount(caller5, pad5(created.ID), balNum)}"`
        logCmd(rid, "BUILD", putCmd)
        try {
          await executeDispatcherAndGetJSON(putCmd, rid)
        } catch (e) {
          if (isCobolErr(e)) {
            return res.status(e.status).json({
              message: `Account created but setting initial balance failed: ${e.publicMessage}`,
              code: e.code,
              stage: "SET_INITIAL_BALANCE",
              accountId: pad5(created.ID),
            })
          }
          return res.status(500).json({
            message: `Account created but setting initial balance failed: ${e?.message || "Unknown error"}`,
            stage: "SET_INITIAL_BALANCE",
            accountId: pad5(created?.ID || ""),
          })
        }
      }
    }

    const reply = {
      CUSTOMERID: cust5,
      IBAN: String(IBAN).trim(),
      CURRENCY: String(CURRENCY).toUpperCase(),
      BALANCE: balNum > 0 ? balNum : parsePic_9_08_V99(BALANCE),
    }
    return res.status(201).json(reply)
  } catch (e) {
    if (e && e.__cobol) {
      return res
        .status(e.status || 500)
        .json({ message: e.publicMessage || e.message, code: e.code || "UNKNOWN", stage: "UNCAUGHT" })
    }
    return res.status(500).json({ message: e?.message || "Internal server error", stage: "UNCAUGHT" })
  }
})
app.put("/accounts/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const { BALANCE, userId } = req.body || {}
    if (BALANCE == null) return res.status(400).json({ message: "BALANCE is required" })

    const caller5 = pad5(userId || "00001")
    const cmd = `${dispatcherPath} "${cmdPutAccount(caller5, id5, BALANCE)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)
    res.json({ ID: id5, BALANCE: Number(BALANCE) })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.delete("/accounts/:id", async (req, res) => {
  const rid = req.__id
  try {
    const id5 = pad5(req.params.id)
    const caller5 = pad5(one(req.query.userId) || "00001")
    const cmd = `${dispatcherPath} "${cmdDelAccount(caller5, id5)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)
    res.status(204).end()
  } catch (e) {
    return handleRouteError(res, e)
  }
})

/* TRANSFERS */
app.post("/transfers", async (req, res) => {
  const rid = req.__id
  try {
    const { fromIban, toIban, amount, userId } = req.body || {}
    if (!fromIban || !toIban || amount == null)
      return res.status(400).json({ message: "fromIban, toIban și amount sunt obligatorii" })
    if (stripIban(fromIban) === stripIban(toIban))
      return res.status(400).json({ message: "Nu poți transfera către același cont" })

    const amt = Number(amount)
    if (!Number.isFinite(amt) || amt <= 0) return res.status(400).json({ message: "Amount trebuie să fie > 0" })

    const caller5 = pad5(userId || "00001")

    const src = await getAccountByIbanNormalized(caller5, fromIban, rid)
    const dst = await getAccountByIbanNormalized(caller5, toIban, rid)

    if (!src || !dst) return res.status(400).json({ message: "IBAN invalid (sursă sau destinație inexistent)" })
    if (src.CURRENCY !== dst.CURRENCY)
      return res.status(400).json({ message: "Nu poți transfera între monede diferite" })
    if (Number(src.BALANCE) < amt) return res.status(400).json({ message: "Fonduri insuficiente în contul sursă" })

    const cmd = `${dispatcherPath} "${cmdTransfer(caller5, fromIban, toIban, amt)}"`
    logCmd(rid, "BUILD_TRANSFER", cmd)

    exec(cmd, execOptions, (error, stdout, stderr) => {
      if (stderr?.trim()) logCmd(rid, "STDERR", "\n" + stderr.trim())
      if (error) {
        logCmd(rid, "ERROR", error.message)
        return res.status(500).json({ message: "Eroare la executarea transferului" })
      }
      if (stdout?.trim()) logCmd(rid, "STDOUT", "\n" + stdout.trim())

      const { errCode, errMsg, errRaw } = parseCobolStdout(String(stdout || ""))
      const cobErr = mapCobolError(errCode, errMsg, errRaw)
      if (cobErr) return res.status(cobErr.status).json({ message: cobErr.publicMessage, code: cobErr.code })

      return res.status(201).json({
        success: true,
        fromIban: stripIban(fromIban),
        toIban: stripIban(toIban),
        amount: amt,
        currency: src.CURRENCY,
      })
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})

/* TRANSACTIONS */
app.get("/transactions", async (req, res) => {
  try {
    const page = getPage(req.query.page)
    const caller5 = pad5(one(req.query.userId) || "00001")
    const c5 = pad5(one(req.query.customerId))
    const a5 = pad5(one(req.query.accountId))

    if (!c5 || !a5) return res.status(400).json({ message: "customerId and accountId are required" })

    const cmd = `${dispatcherPath} "${cmdGetTrans(caller5, c5, a5, page)}"`
    const data = await executeDispatcherAndGetJSON(cmd, req.__id)

    const items = (data.records || []).map((r) => ({
      accountId: String(r.ACCOUNT ?? r.ACCOUNTID ?? r["ACCOUNT ID"] ?? "").trim(),
      operationType: String(r.OPERATION ?? r["OPERATION TYPE"] ?? r["TRANS TYPE"] ?? "")
        .trim()
        .toUpperCase(),
      amount: parsePic_9_08_V99(r.AMOUNT),
      balance: parsePic_9_08_V99(r.BALANCE ?? r["ACC BALANCE"]),
      timestamp: r.TIMESTAMP ?? r.TRANTS ?? null,
    }))

    res.json({
      page,
      pageSize: PAGE_SIZE,
      count: items.length,
      hasPrev: page > 1,
      hasNext: items.length === PAGE_SIZE,
      items,
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})
app.post("/transactions", async (req, res) => {
  const rid = req.__id
  try {
    const { ACCOUNTID, AMOUNT, TYPE = "DEPOSIT", CUSTOMERID, userId } = req.body || {}
    if (!ACCOUNTID || AMOUNT == null) return res.status(400).json({ message: "ACCOUNTID and AMOUNT are required" })

    const caller5 = pad5(userId || "00001")
    const a5 = pad5(ACCOUNTID)
    let c5 = CUSTOMERID ? pad5(CUSTOMERID) : null

    if (!c5) {
      const acc = await getAccountByIdAcrossCustomers(caller5, a5, rid)
      if (!acc) return res.status(400).json({ message: "Unable to infer CUSTOMERID for account " + ACCOUNTID })
      c5 = pad5(acc.CUSTOMERID)
    }

    const cmd = `${dispatcherPath} "${cmdPostTrans(caller5, c5, a5, TYPE, AMOUNT)}"`
    logCmd(rid, "BUILD", cmd)
    await executeDispatcherAndGetJSON(cmd, rid)

    res.status(201).json({ ACCOUNTID: a5, CUSTOMERID: c5, TYPE: String(TYPE).toUpperCase(), AMOUNT: Number(AMOUNT) })
  } catch (e) {
    return handleRouteError(res, e)
  }
})

/* STATISTICS ENDPOINTS */
app.get("/stats/totals", async (req, res) => {
  const rid = req.__id
  try {
    const caller5 = pad5(one(req.query.userId) || "00001")

    // Get total customers count
    let totalCustomers = 0
    for (let p = 1; p <= 200; p++) {
      const rows = await getCustomersPageRaw(caller5, p, rid)
      if (!rows.length) break
      totalCustomers += rows.length
      if (rows.length < PAGE_SIZE) break
    }

    // Get total users count
    let totalUsers = 0
    for (let p = 1; p <= 200; p++) {
      const rows = await getUsersPageRaw(caller5, p, rid)
      if (!rows.length) break
      totalUsers += rows.length
      if (rows.length < PAGE_SIZE) break
    }

    // Get total accounts count and total balance
    let totalAccounts = 0
    let totalBalance = 0
    for (let p = 1; p <= 200; p++) {
      const rows = await getAccountsAllPageRaw(caller5, p, rid)
      if (!rows.length) break
      totalAccounts += rows.length

      // Calculate balance for this page
      for (const row of rows) {
        const acc = normalizeAccountRow(row)
        totalBalance += acc.BALANCE
      }

      if (rows.length < PAGE_SIZE) break
    }

    res.json({
      totalCustomers,
      totalUsers,
      totalAccounts,
      totalBalance,
    })
  } catch (e) {
    return handleRouteError(res, e)
  }
})

app.listen(port, () => {
  console.log(`Banking API listening on http://localhost:${port}`)
})
