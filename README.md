# timeout - GNU Coreutils Compatible timeout Command

![](https://repository-images.githubusercontent.com/1054909255/1301f2f4-d0bb-4a52-b940-7f072a2be467)

*Image by [T5](https://weibo.com/579027700), source: https://www.pixiv.net/artworks/108988374*

A standalone implementation of the `timeout` command.

## Purpose

macOS does not include the GNU Coreutils `timeout` command by default. While you can install the full coreutils package via Homebrew, this introduces many unnecessary commands and potential path conflicts.

This project provides a standalone, lightweight `timeout` command implementation that allows you to use timeout functionality without installing the entire GNU Coreutils suite.

## Installation

### Via homebrew

```bash
brew install aisk/homebrew-tap/timeout
```

### From source

```bash
make
sudo cp timeout /usr/local/bin/
```

## Usage Examples

```bash
# Run command with 5 second timeout
timeout 5s long-running-command

# 2 minute timeout
timeout 2m backup-script.sh
```

## Building

```bash
make        # Compile
make clean  # Clean build artifacts
make format # Format code
```
