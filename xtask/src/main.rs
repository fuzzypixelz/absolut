use std::{env, path::PathBuf};

use anyhow::Context;
use toml_edit::{value, DocumentMut};

use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    let flags = flags::Xtask::from_env()?;

    let sh = &Shell::new()?;
    sh.change_dir(project_root());

    match flags.subcommand {
        flags::XtaskCmd::Release(cmd) => cmd.run(sh)?,
    }

    Ok(())
}

impl flags::Release {
    fn run(&self, sh: &Shell) -> anyhow::Result<()> {
        let flags::Release {
            version,
            crates_io_token,
        } = self;

        let version = version
            .to_str()
            .context("version number is not a valid string")?;

        let mut virtual_manifest = sh.read_file("Cargo.toml")?.parse::<DocumentMut>()?;
        let worspace = &mut virtual_manifest["workspace"];
        worspace["package"]["version"] = value(version);
        worspace["dependencies"]["absolut-macros"]["version"] = value(version);
        sh.write_file("Cargo.toml", virtual_manifest.to_string())?;

        let message = format!("chore: Bump version to {version}");
        cmd!(sh, "git commit Cargo.toml --message {message}").run()?;
        cmd!(sh, "git tag {version} --message v{version}").run()?;

        let mut login = cmd!(sh, "cargo login {crates_io_token}");
        login.set_secret(true);
        login.run()?;
        cmd!(sh, "cargo publish --package absolut-macros").run()?;
        cmd!(sh, "cargo publish --package absolut").run()?;

        Ok(())
    }
}

mod flags {
    use std::path::PathBuf;

    xflags::xflags! {
        cmd xtask {
            cmd release {
                required --version number: PathBuf
                required --crates-io-token token: PathBuf
            }
        }
    }
}

fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}
