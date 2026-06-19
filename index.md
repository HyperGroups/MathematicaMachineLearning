# MathematicaMachineLearning

> 用 Wolfram Language / Mathematica 做机器学习的个人资料集：课程与书籍代码、工作与作业实践、图像处理与神经网络模型工具等。
>
> **基准版本：Wolfram 14 / 15**（仓库历史内容创建于 V11–V12，正按 13/14/15 逐步现代化）

## 简介

围绕 Mathematica 机器学习的笔记与代码整理，参考来源包括《机器学习实战》《数学之美》《PRML》、Wolfram 帮助文档、网络课程与文章、Mathematica StackExchange 帖子及习题解答等。

项目分三块：

1. **博文/笔记** —— 知乎专栏、语雀等平台上关于 Mathematica 机器学习的笔记与文档。
2. **代码资源** —— 上述博文配套及独立的代码（本仓库主体）。
3. **数据资源** —— 较大的数据集另存于网盘/群文件，仓库内只保留小样例。

目标是积累可复用的**代码模板**、整齐的**数据集**与有趣的**结论**，例如：某模板用 PCA+SVM 在 MNIST 上达到 ~0.98，欢迎直接跑、调参、在其他数据集上复现，并做性能测试。

## 环境要求

- **Wolfram Language / Mathematica 14 或 15**（历史笔记本可在 13+ 打开；个别用到新结构如 `Tabular` 的示例需 14+）。
- 部分工具需 `wolframscript`（命令行）与 **Wolfram 账户登录**（NetModel 联网下载）。
- 神经网络模型按需联网下载，并缓存到本地对象库 `$LocalBase`（用 `LocalObjects[]` 查看、`ResourceRemove` 清除）。

## 仓库结构

| 目录 | 内容 |
|---|---|
| `Articles/` | 博文/读书笔记（如《机器学习实战》Chapter 2 KNN） |
| `Tools/` | NetModel 下载/安装/帮助脚本、笔记本→Markdown 转换器 |
| `MNIST/` | MNIST 分类、聚类、SVM 示例 |
| `ImageProcessing/` | 图像分类模板、卷积/人脸检测 |
| `LogisticRegression/` | 逻辑回归检查与模型文件 |
| `ZhiHu/` | 知乎专栏配套（自编码器、混淆矩阵、归一化与聚类） |
| `Examples/` `Bugs/` `Captcha/` `CommonNotebooks/` | 杂项示例、bug 复现、验证码、常用片段 |

## 如何使用

- **打开笔记本**：用 Mathematica 直接打开对应 `.nb`。
- **运行脚本**：命令行 `wolframscript -file Tools/NetModelDownloader.wls`。
- **路径说明**：历史代码中曾硬编码绝对路径；现代化后改用 `NotebookDirectory[]` / 可配置变量，请按本机环境调整。

## 相关链接

- **GitHub 仓库**：[HyperGroups/MathematicaMachineLearning](https://github.com/HyperGroups/MathematicaMachineLearning)
- 语雀文档：[mathematica/machinelearning](https://www.yuque.com/mathematica/machinelearning)
- 知乎专栏：[MathematicaMachineLearning](https://zhuanlan.zhihu.com/MathematicaMachineLearning)
- [Wolfram 神经网络仓库](https://resources.wolframcloud.com/NeuralNetRepository/)
- 相关 Mathematica.SE 提问：[154862](https://mathematica.stackexchange.com/q/154862/6648) · [154661](https://mathematica.stackexchange.com/q/154661/6648) · [154479](https://mathematica.stackexchange.com/q/154479/6648)

## 贡献

欢迎 Issue / PR：补充代码模板、整理数据集、复现公开数据集上的常见模型、做性能测试。

## 许可证

MIT License
