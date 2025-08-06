use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::io::{stdin, stdout, Read, Write};

fn read_cmd() -> std::io::Result<Option<Vec<u8>>> {
    let len = match stdin().read_u16::<BigEndian>() {
        Ok(len) => len as usize,
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    };
    
    let mut buf = vec![0u8; len];
    stdin().read_exact(&mut buf)?;
    Ok(Some(buf))
}

fn write_cmd(data: &[u8]) -> std::io::Result<()> {
    stdout().write_u16::<BigEndian>(data.len() as u16)?;
    stdout().write_all(data)?;
    stdout().flush()
}

fn main() -> std::io::Result<()> {
    loop {
        match read_cmd()? {
            Some(cmd) => {
                match cmd[0] {
                    1 => {
                        // 处理命令 1
                        let result = cmd[1] + 1;
                        write_cmd(&[result])?;
                    },
                    2 => {
                        // 处理命令 2
                        let result = cmd[1] * 2;
                        write_cmd(&[result])?;
                    },
                    _ => break,
                }
            },
            None => break, // EOF reached, exit gracefully
        }
    }
    Ok(())
}