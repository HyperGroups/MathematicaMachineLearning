# 现代化指南：Wolfram 13 / 14 / 15

本文给出仓库内各主题在新版本下的**等价或更优写法**。原 `.nb` 笔记本（V11–V12）不直接改写，本文作为配套对照。

> 所有代码为**静态更新**，未在本机 15.0 实跑验证；用作迁移参考，落地前请实机确认。

## 目录
- [跨版本要点](#跨版本要点)
- [KNN（Articles/.../Chapter2）](#knn)
- [MNIST 分类 / 聚类 / SVM](#mnist)
- [图像分类模板（ImageProcessing）](#图像分类)
- [混淆矩阵与评估（ZhiHu）](#评估与混淆矩阵)
- [NetModel 用法](#netmodel)

---

## 跨版本要点

| 能力 | 版本 | 说明 |
|---|---|---|
| `Tabular` 数据结构 | 14 / 15 | `Classify`/`Predict`/`ClassifierMeasurements`/`PredictorMeasurements`/`FindClusters`/`LearnDistribution`/`FeatureExtraction`/`DimensionReduce`/`FeatureSpacePlot`/`NetTrain`/`NetMeasurements` 均已支持 `Tabular`。 |
| `ClassifierMeasurements` 增强 | 14.x | 支持不确定度、多类平均；`"NeuralNetwork"` 方法改用自归一化网络，大幅改进。 |
| `Classify` 特征提取器可取出 | 13.0 | 可获取构造好的 feature extractor 用于其他数据；支持 SHAP 解释特征影响。 |
| `NetExternalObject`（ONNX） | 13.2+ | 在 WL 内运行外部 ONNX 网络，便于离线/互操作。 |
| LLM 函数族 | 14.0+ | `LLMFunction`/`LLMSynthesize`/`ChatEvaluate` 等（与本仓库 ML 主题相邻，按需了解）。 |

---

## KNN

原笔记用 `For` 循环手工累计类别票数。新版可用更地道、可扩展的写法。

```wl
(* 数据 *)
SeedRandom[1];
dataSet = RandomReal[10, {20, 2}];
labels  = RandomChoice[{"A", "B", "C", "D"}, 20];
vec     = RandomReal[10, 2];

(* 写法一：Nearest（自动建 KD-Tree，适合大数据） *)
nf = Nearest[dataSet -> labels];      (* NearestFunction，可复用 *)
TakeLargestBy[Counts[nf[vec, 10]], Identity, All]   (* k=10 的多数投票 *)

(* 写法二：内置分类器（生产首选） *)
c = Classify[dataSet -> labels, Method -> "NearestNeighbors"];
c[vec]
c[vec, "Probabilities"]
```

要点：`Nearest`/`NearestFunction` 内部用空间索引，远胜手写 `Ordering` 全量距离；`Classify[..., Method -> "NearestNeighbors"]` 直接给出概率与可评估对象。

---

## MNIST

原示例（V11.1）思路可保留，新版可简化数据获取与评估。

```wl
(* 标准数据集（ResourceData，13/14/15） *)
train = ResourceData["MNIST", "TrainingData"];
test  = ResourceData["MNIST", "TestData"];

(* 直接分类 *)
c = Classify[train, Method -> "NeuralNetwork"];   (* 14.x 自归一化网络，效果更好 *)

(* 评估：14.x 支持多类平均与不确定度 *)
cm = ClassifierMeasurements[c, test];
cm["Accuracy"]
cm["ConfusionMatrixPlot"]
cm["F1Score", "Averaging" -> "Macro"]
```

PCA+SVM 模板（README 提到的 ~0.98）可表述为：

```wl
dr = DimensionReduce[train[[All, 1]], 50, Method -> "PrincipalComponentsAnalysis"];
(* 或在 Classify 中用 FeatureExtractor 组合 PCA 后接 SVM *)
c = Classify[train, Method -> "SupportVectorMachine",
    FeatureExtractor -> {"StandardizedVector", "DimensionReduction" -> 50}];
```

---

## 图像分类

`ImageProcessing/ImageClassify` 模板可直接用现成网络做迁移/推理：

```wl
net = NetModel["ResNet-50 Trained on ImageNet Competition Data"];
net[Import["Data/good.jpg"]]

(* 取特征做下游分类（13.0+ 可取 feature extractor） *)
feat = NetModel["ResNet-50 ...", "EvaluationNet"];
fe = FeatureExtraction[images, FeatureExtractor -> feat];
```

大批量预测（README 的"百万图片 1 小时"目标）：用 `net[images, TargetDevice -> "GPU", BatchSize -> n]` 与 `OutOfCoreTraining`/分块推理（见 `Examples/Example@OutOfCoreTraining.nb` 的现代等价）。

---

## 评估与混淆矩阵

`ZhiHu/ConfusionMatrixPlot.nb` 的自定义绘制，新版多数可由内置替代：

```wl
cm = ClassifierMeasurements[c, test];
cm["ConfusionMatrixPlot"]            (* 内置混淆矩阵图 *)
cm["Report"]                          (* 综合报告 *)
cm[{"Precision", "Recall", "F1Score"}, "Averaging" -> "Macro"]   (* 14.x *)
```

归一化与聚类（`ZhiHu/数据集列归一化与聚类示例.nb`）：

```wl
norm = Standardize[data];             (* 列标准化 *)
FindClusters[norm, 4]                  (* 14/15 支持 Tabular 输入 *)
FeatureSpacePlot[norm]                 (* 降维可视化 *)
```

---

## NetModel

详见 [`Tools/README.md`](Tools/README.md)。要点：

- `NetModel[name]` 自动缓存到 `$LocalBase`；`LocalObjects[]` 查看，`ResourceRemove` 清除。
- 离线分发：联网机预下载后复制 `$LocalBase`，或导出 `.wlnet`（见 `Tools/ModelInstaller/NetModelInstaller.wls`）。
- ONNX：`NetExternalObject["model.onnx"]`（13.2+）。
- 旧的"手工改资源缓存文件"离线安装法（`NetModelInstaller_12.0.wls`）已弃用。
