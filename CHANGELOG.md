# Changelog

本仓库变更记录。版本基准从 V11–V12 向 Wolfram 13/14/15 迁移，详见 [`ASSESSMENT.md`](ASSESSMENT.md)。

## [Unreleased] — 2026-06

### 结构与卫生
- 移除被跟踪的 `.DS_Store`（5 个），完善 `.gitignore`（macOS/Windows/Wolfram/编辑器临时文件）。
- 重写 `README.md`：新增目录、环境要求、仓库结构表、使用说明、版本基准与许可证指引。
- 新增 `LICENSE`（MIT）、`ASSESSMENT.md`（现状评估与计划）、`CHANGELOG.md`。

### Tools 现代化（静态更新，按 13/14/15）
- 新增不带版本后缀的现代版脚本：`Tools/NetModelDownloader.wls`、`Tools/ModelInstaller/NetModelInstaller.wls`。
  - 去除对私有符号（`ResourceSystemClient`Private`*`、`LocalObjects`LocalObject`Dump`*`）的依赖。
  - 改用文档化机制：本地对象库 `$LocalBase`、`LocalObjects[]`、`ResourceObject`/`ResourceRemove`。
- 保留旧脚本作历史参考，并标注"已弃用"。
- 新增 `Tools/README.md`：各脚本用途、适用版本、使用方法。

### 笔记现代化建议（静态）
- 新增 `MODERNIZATION.md`：给出 13/14/15 下 `Classify`/`Predict`/`ClassifierMeasurements`/`Tabular`/`NetModel` 等的等价或更优写法。

> 二进制 `.nb` 笔记本未在未实机验证条件下直接改写；现代化以新增配套文档/脚本落地。
