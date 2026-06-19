# Tools

Mathematica 机器学习相关的辅助脚本。**基准版本：Wolfram 13 / 14 / 15。**

## NetModel 相关

| 文件 | 状态 | 说明 |
|---|---|---|
| `NetModelDownloader.wls` | ✅ 现代版 | 批量预下载神经网络模型并缓存到 `$LocalBase`。支持命令行参数指定单个模型；带重试与超时。 |
| `ModelInstaller/NetModelInstaller.wls` | ✅ 现代版 | 离线/内网分发模型：预下载+复制缓存、导出/导入 `.wlnet`、ONNX 经 `NetExternalObject` 加载。 |
| `NetModelDownloader@V11.3.wls` | ⚠️ 已弃用 | 历史参考（V11.3）。 |
| `ModelInstaller/NetModelInstaller_12.0.wls` | ⚠️ 已弃用 | 历史参考（V12.0），依赖已失效的私有符号。 |
| `NetModelHelp@V11.3.nb` / `@V12.0.nb` | 📓 历史笔记 | 旧版本探索记录。 |
| `NetModelDownload.nb` / `MathematicaNetModel文件Copy.nb` / `获取NetModel...nb` | 📓 历史笔记 | 旧版本笔记本。 |
| `Data/modelnames@V11.2.txt` / `modelNames.reltion.V11.2.txt` | 🗂️ 过时数据 | 仅 15 个模型；当前仓库已有 200+。重生成见下。 |

### 用法

```bash
# 下载全部模型（联网，需 Wolfram 账户）
wolframscript -file NetModelDownloader.wls

# 只下载指定模型
wolframscript -file NetModelDownloader.wls "Inception V1 Trained on ImageNet Competition Data"

# 查看本地缓存信息 / 安装器用法
wolframscript -file ModelInstaller/NetModelInstaller.wls
```

### 重新生成模型清单（替代过时的 `Data/*V11.2*`）

```wl
(* 联网枚举当前版本所有 NetModel 名称 *)
Export["Data/modelnames.txt", StringRiffle[NetModel[], "\n"], "Text"]
```

### 关键机制（13/14/15）

- `NetModel[name]` 自动联网下载，缓存到本地对象库 **`$LocalBase`**。
- `LocalObjects[]` 列出缓存；`ResourceRemove[ResourceObject[name]]` 清除。
- 离线分发：在联网机预下载后，复制整个 `$LocalBase` 目录到离线机同名路径。
- ONNX 模型：`NetExternalObject["model.onnx"]`（13.2+）。

> ⚠️ 涉及联网下载、跨机器复制缓存、前端私有接口的环节，在静态条件下未实机验证，请在 15.0 实跑确认。

## Markdown 转换

| 文件 | 说明 |
|---|---|
| `MyMarkDown.wl` / `.m` / `.nb` | 笔记本 → Mathematica StackExchange / Markdown 转换器。依赖 `FrontEnd`ExportPacket` 等前端接口（13/14/15 仍存在但脆弱）。 |
