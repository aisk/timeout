# Haskell 实现与 GNU timeout 的差异

本文档记录当前 Haskell 实现与 GNU Coreutils 官方 `timeout.c` 之间的差异。

## 1. 进程组管理（重要）

**GNU 实现：**
- 非 foreground 模式下使用 `setpgid(0, 0)` 创建新的进程组
- 超时后向整个进程组发送信号（`kill(0, sig)`）
- 能确保杀死子进程的所有后代进程

**Haskell 实现：**
- 只向单个进程发送信号（`signalProcess`）
- 子进程的后代进程不会被终止

**影响：** 如果命令启动了子进程，超时后只有直接子进程被杀死，孙子进程会继续运行。

---

## 2. `--kill-after` 行为

**GNU 实现：**
- 发送第一个信号后，检查进程是否仍在运行
- 只有进程仍存活时，才启动 KILL 信号的定时器

**Haskell 实现：**
- ~~顺序执行：发送第一个信号 → 等待 `kill-after` 时间 → 发送 KILL~~
- ~~不检查进程是否已经退出~~
- 发送第一个信号后，等待 `kill-after` 时间
- 检查进程是否仍在运行，只有存活时才发送 SIGKILL

**状态：** ✅ 已修复

**影响：** 无（行为与 GNU 实现一致）。

---

## 3. 支持的信号范围

**GNU 实现：**
- 支持所有终止信号的名称和编号
- 支持 POSIX 实时信号（`SIGRTMIN` 到 `SIGRTMAX`）
- 使用 `sig2str` 和 `operand2sig` 进行信号名/编号转换

**Haskell 实现：**
- 仅支持：TERM、KILL、INT、HUP、USR1、USR2
- 支持数字形式的信号编号

**影响：** 无法使用其他信号如 QUIT、ALRM 等。

---

## 4. 退出码处理

**GNU 实现：**
- 被信号杀死的进程返回 `128 + 信号编号`
- 例如被 SIGKILL 杀死返回 137（128 + 9）
- 超时后被 KILL 杀死时，`--preserve-status` 自动生效

**Haskell 实现：**
- 未正确处理被信号杀死的情况
- 退出码处理逻辑不完整

**影响：** 退出码与 shell 和 GNU timeout 不一致。

---

## 5. 超时值为 0 的语义

**GNU 实现：**
- `0` 表示禁用超时，命令可以无限运行
- 文档明确说明 "A duration of 0 disables the associated timeout"

**Haskell 实现：**
- `0` 被当作 0 微秒，会立即触发超时

**影响：** `timeout 0 command` 行为不一致。

---

## 6. SIGCONT 处理

**GNU 实现：**
- 发送终止信号后，额外发送 `SIGCONT`
- 确保被停止（stopped）的进程能收到终止信号

**Haskell 实现：**
- 没有发送 `SIGCONT` 的逻辑

**影响：** 如果子进程处于停止状态（如被 Ctrl+Z 暂停），可能无法被正确终止。

---

## 7. 父进程死亡信号（Parent Death Signal）

**GNU 实现：**
- 子进程使用 `prctl(PR_SET_PDEATHSIG, term_signal)` 设置父进程死亡信号
- 如果 timeout 进程异常终止，子进程也会被终止

**Haskell 实现：**
- 无此功能

**影响：** timeout 进程崩溃时，子进程可能成为孤儿进程继续运行。

---

## 8. Core Dump 处理

**GNU 实现：**
- 检测并报告被监控命令是否产生了 core dump
- 尝试禁用 timeout 进程自身的 core dump

**Haskell 实现：**
- 无此功能

**影响：** 缺少诊断信息。

---

## 9. Foreground 模式的 TTY 信号处理

**GNU 实现：**
- 忽略 `SIGTTIN` 和 `SIGTTOU` 信号
- 防止后台子进程访问 TTY 时导致 timeout 进程停止

**Haskell 实现：**
- 无此处理

**影响：** 在某些终端场景下可能出现意外行为。

---

## 10. Verbose 输出格式

**GNU 实现：**
```
sending signal TERM to command 'sleep'
```

**Haskell 实现：**
```
sending signal 15 to process 12345
```

**影响：** 输出格式不同，但功能等价。

---

## 11. 时间解析精度

**GNU 实现：**
- 支持纳秒级精度的浮点数
- 使用 `dtotimespec` 和 `dtimespec_bound` 进行精确转换

**Haskell 实现：**
- 支持 `ms`、`s`、`m`、`h`、`d` 后缀
- 内部使用微秒精度

**影响：** 精度略有差异，但对大多数使用场景影响不大。

---

## 12. 信号屏蔽和竞态条件处理

**GNU 实现：**
- 使用复杂的信号屏蔽机制防止竞态条件
- 使用 `sigsuspend` 等待信号
- 在 `waitpid` 和信号处理之间避免竞态

**Haskell 实现：**
- 使用 `MVar` 和 `threadDelay` 的简单模型
- 没有处理复杂的信号竞态条件

**影响：** 在极端情况下可能出现不一致行为。

---

## 总结

| 差异项 | 严重程度 | 是否影响核心功能 |
|--------|----------|------------------|
| 进程组管理 | 高 | 是 |
| --kill-after 行为 | 中 | ~~部分~~ ✅已修复 |
| 信号范围 | 中 | 部分 |
| 退出码处理 | 中 | 是 |
| 超时值 0 语义 | 高 | 是 |
| SIGCONT 处理 | 低 | 边缘情况 |
| 父进程死亡信号 | 低 | 边缘情况 |
| Core Dump 处理 | 低 | 否 |
| TTY 信号处理 | 低 | 边缘情况 |
| Verbose 格式 | 低 | 否 |
| 时间精度 | 低 | 否 |
| 信号竞态处理 | 低 | 边缘情况 |
