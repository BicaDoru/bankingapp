# poc-gnucobol

## Prerequisites

> Make sure that path to repository/workspace does not have any spaces. E.g: **"C/My Path/cobollearning" - is not good**

- Git
- VSCode + recommended extensions
  - COBOL (Identifier: bitlang.cobol)
  - Markdown Preview Enhanced (Identifier: shd101wyy.markdown-preview-enhanced)
  - markdownlint (Identifier: davidanson.vscode-markdownlint)
  - Prettier (Identifier: esbenp.prettier-vscode)
  - Todo Tree (Identifier: gruntfuggly.todo-tree)
  - Remote explorer (Identifier: ms-vscode-remote.vscode-remote-extensionpack)
- WSL
- GNU Cobol

### WSL [source](https://learn.microsoft.com/en-us/windows/wsl/install#install-wsl-command)

Open CMD and write: `wsl --install`

The above command only works if WSL is not installed at all. If you run `wsl --install` and see the WSL help text, please try running `wsl --list --online` to see a list of available distros and run `wsl --install -d <DistroName>`. In that case install latest LTS version of Ubuntu

After install a restart may be required.
Afterwards, to start wsl write `wsl` in cmd.

On first run you will be required to set an User & Password.

### Install GNU Cobol on WSL [source](https://installati.one/install-gnucobol-ubuntu-22-04/#google_vignette)

Open WSL and execute following commands:
`sudo apt update`
`sudo apt -y install gnucobol`

### Install PostgreSQL on WSL [source](https://learn.microsoft.com/en-us/windows/wsl/tutorials/wsl-database#install-postgresql)

Open WSL and execute following command: `sudo apt install postgresql postgresql-contrib`
Confirm installation and get the version number: `psql --version`

Enter the command: `sudo passwd postgres`
You will get a prompt to enter your new password.
Close and reopen your terminal.

Start the database: `sudo service postgresql start`
Checking the status: `sudo service postgresql status`

Update password for the postgres (needed for COBOL programs to work properly): `sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'your_password_here';"` (Not just copypaste, replace your_password_here with your actual password :))

#### Creating the database

To launch postgresql use command: `su - postgres`, to switch to postgres user. Terminal will look something like this: `postgres@RO-SOMEIDL~$`
Create new database with command: `createdb testdb`
Write `psql` in terminal. (Must be using postgres user)
Command `\l` will display all the databases.
Use `exit` to return.

### Install OCESQL on WSL [source](https://bigdanzblog.wordpress.com/2020/10/28/embedded-sql-for-gnucobol-using-ocesql/)

Download OCESQL latest release from [here](https://github.com/opensourcecobol/Open-COBOL-ESQL/releases)

Copy the archive to the WSL home path (`cd ~`) and unzip it with command: `unzip <archive_name>`

CD into the unzipped folder and do the following commands:

```bash
sudo apt update
sudo apt install build-essential autotools-dev autoconf g++ automake libpq5 libpq-dev libtool pkg-config bison byacc flex
./configure
make
sudo make install
```

Now OCESQL should be installed. Verify it with command: `ocesql`.

Restart the VSCode (to be on the safer side restart the machine) for the ocesql settings to take effect. (If while running an SQL program such an error is received: `error while loading shared libraries: libocesql.so.0: cannot open shared object file`. Then, try restarting again)

## Compile and run a program

In clone location $APP_HOME:

- run the command `. scripts/compile program_name`
  > replace program_name with the actual name of the cobol file you want to compile and run(e.g languages, numbers, fizzbuzz). there are some predefined arguments to run the code, you can edit them in `scripts/compile`

## Notes

- For correct working of the scripts (future scripts), be sure to be located in the root directory of this repository
- Make sure that all the text files have EOL set to be LF (not CRLF). WSL is picky about executing files with CRLF, there may be errors or unexpeced behaviour.

## TODO

[ ] - have EOL for files as LF instead of CRLF, by default, somehow - [source](https://docs.github.com/en/get-started/git-basics/configuring-git-to-handle-line-endings) [source2](https://stackoverflow.com/questions/10418975/how-to-change-line-ending-settings)
