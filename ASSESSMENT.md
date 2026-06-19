# 仓库评估与现代化计划（MathematicaMachineLearning）

> 评估日期：2026-06-19 ｜ 基准内核：本机 **Wolfram 15.0**（`wolframscript` 可用）
> 更新范围：仓库已有内容，结合 Mathematica **13 / 14 / 15** 三个大版本的变化做**静态更新**（不实际运行验证）。

---

## 1. 现状概览

仓库是一份个人 Mathematica 机器学习资料集，约 52 个被跟踪文件，主体为 `.nb` 笔记本，配少量 `.md` 笔记、`.wl/.m/.wls` 脚本与数据。内容创建于 **V11.1 ~ V12.0**（2017–2019 年间），距当前 15.0 已跨 **3 个大版本**。

目录结构：

| 目录 | 内容 | 主要陈旧点 |
|---|---|---|
| `Articles/Notes@MachineLearningInAction/Chapter2` | 《机器学习实战》KNN 笔记（md + nb） | 硬编码 macOS 绝对路径 |
| `Tools` | NetModel 下载/安装/帮助、Markdown 转换器 | 依赖 V11.3/12.0 私有符号；版本号写进文件名 |
| `MNIST` | MNIST 分类/聚类/SVM 示例 | V11.1 创建，API 可现代化 |
| `ImageProcessing` | 图像分类模板、人脸检测 | 绝对路径、旧 NetModel 用法 |
| `LogisticRegression` | 逻辑回归检查、FTRL 模型文件 | — |
| `ZhiHu` | 知乎专栏配套（自编码器、混淆矩阵、归一化聚类） | 旧 API |
| `Examples` / `Bugs` / `Captcha` / `CommonNotebooks` | 杂项示例与 bug 复现 | — |

---

## 2. 问题清单（按优先级）

### A. 仓库结构与卫生（高，且低风险）

1. **`.DS_Store` 被纳入版本控制**：根目录、`ImageProcessing/`、`ImageProcessing/Data/`、`MNIST/`、`Tools/` 共 5 个。`.gitignore` 已有 `*.DS_Store`，但这些文件**已被跟踪**，忽略规则对已跟踪文件无效，需 `git rm --cached`。
2. **硬编码绝对路径（含个人信息泄露）**，散布于 14 个文件：
   - macOS：`/Users/hypergroups/Documents/...`、`/Users/hypergroups/Nutstore/...`
   - Windows：`D:\wolfram\NetModels`
   这些使代码不可移植，且暴露本地用户名。应改为 `NotebookDirectory[]` / `$TemporaryDirectory` / 相对路径或可配置变量。
3. **文件名把版本号写死**：`NetModelHelp@V11.3.nb`、`NetModelHelp@V12.0.nb`、`NetModelInstaller@V12.0.nb`、`NetModelInstaller_12.0.wls`、`Data/modelnames@V11.2.txt` 等。版本一升级即过时，建议去掉版本后缀、改用文档内"适用版本"标注。
4. **README 缺结构**：无目录、无"如何使用/环境要求/许可证"、外链多为社群链接（QQ 群、知乎、语雀），无版本基准说明。
5. **缺少 `LICENSE`**：公开仓库无许可证，他人无法合法复用。

### B. Tools 工具代码（高，中风险——只能静态更新）

6. **`NetModelInstaller_12.0.wls` 依赖未公开私有符号**，跨版本已基本失效：
   - `ResourceSystemClient`Private`importRaw`
   - `LocalObjects`LocalObject`Dump`format`、`...filebytecount`
   - 硬编码 `"LanguageVersion" -> 11.3`、写死的 cloud 资源相对路径 `download/EvaluationNet/41f18e3b11f833a6/data`
   该"手工改资源缓存文件"的离线安装思路在 12.1+ 已无必要。**现代做法**：NetModel 自动缓存到本地对象库 `$LocalBase`，用 `LocalObjects[]` 列出、`ResourceRemove` 清除；离线/内网分发可走文档化的 `ResourceObject` 机制，ONNX 模型可用 13.2+ 的 `NetExternalObject`。
7. **`NetModelDownloader@V11.3.wls`**：核心逻辑（循环 `NetModel[i]` 触发下载 + 失败重试）思路仍可用，但应：去版本后缀、加 `$LocalBase` 缓存说明、用更稳健的错误处理与进度反馈。
8. **`Data/modelnames@V11.2.txt` 仅 15 个模型**：当前 Wolfram 神经网络仓库已有 200+ 模型。该清单严重过时；静态条件下无法重新生成（需联网枚举 `NetModel[]`），应标注并提供重生成脚本。
9. **`MyMarkDown.wl`（笔记本→Markdown 转换器）**：依赖 `FrontEnd`ExportPacket` 等前端私有接口，13/14/15 仍存在但脆弱。功能可保留，建议加"适用版本"声明与已知风险注释；长期可评估改用文档化导出。

### C. ML 笔记与示例（中，静态更新为主）

10. 笔记本以 V11.1–12.0 编写，可结合新版本能力刷新说明（**不改二进制 nb 的前提下，以文档/注释形式给出现代化建议**）：
    - **Tabular（14/15）**：`Classify`/`Predict`/`ClassifierMeasurements`/`FindClusters`/`DimensionReduce`/`NetTrain` 等现已支持新的 `Tabular` 数据结构，示例可演示。
    - **`ClassifierMeasurements`（14.x）**：支持不确定度与多类平均；`"NeuralNetwork"` 方法用自归一化网络大幅改进。
    - **`NetExternalObject`（13.2+）**：ONNX 互操作。
    - KNN 笔记中的 `For` 循环计数可用 `Nearest`/`NearestNeighbors`/`Counts`/`Ordering` 更地道地表达（部分已在 `knn.*.md` 体现）。

---

## 3. 分阶段计划

- **阶段一 · 结构与卫生**（本次执行）：移除 `.DS_Store` 跟踪并修正 `.gitignore`；重写 `README.md`（加目录/环境/用法/版本基准/许可证指引）；新增 `LICENSE`、`CHANGELOG.md`。绝对路径以"集中说明 + 逐文件标注"处理（nb 为二进制，优先在配套 md/脚本中修正）。
- **阶段二 · Tools 现代化**（本次执行，静态）：新增不带版本后缀的现代版脚本——`Tools/NetModelDownloader.wls`、`Tools/ModelInstaller/NetModelInstaller.wls`，去除私有符号、改用 `$LocalBase`/`LocalObjects[]`/`ResourceObject` 文档化机制；保留旧脚本并在头部标注"已弃用，仅作历史参考"。附 `Tools/README.md` 说明各脚本用途与适用版本。
- **阶段三 · 笔记现代化建议**（本次执行，静态）：为各主题补 `MODERNIZATION.md`/章节说明，给出 13/14/15 的等价/更优写法；二进制 nb 不直接改，避免无法运行验证下损坏。

> 因选择**只做静态更新**：凡涉及联网下载模型、重生成模型清单、或依赖前端/内核内部状态的部分，均**标注"需在 15.0 实机验证"**，不擅自断言其可运行。

---

## 4. 风险与边界

- `.nb` 是 Wolfram 二进制/盒式结构文件，**不在静态、无法运行验证的条件下直接编辑**，以免破坏。现代化以新增配套文档/脚本的方式落地。
- 私有符号（`*`Private`*`、`FrontEnd`*`）的替代写法在静态条件下无法 100% 确认，相关改动均显式标注待实机验证。
- 模型清单、联网安装等需要 `wolframscript` 实跑的环节，提供脚本但不声称"已验证"。
