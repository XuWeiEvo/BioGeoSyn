# iBiogeobears 中文用户测试教程

版本：面向 `0.2.1-alpha` 之后的普通用户测试  
适用对象：会打开 R 或 RStudio、但不一定熟悉 R 包开发或 BioGeoBEARS 细节的用户

这份教程的目标是让测试用户完成四件事：

1. 安装 `iBiogeobears` 和必要依赖。
2. 打开 Shiny 图形界面。
3. 用内置示例数据跑通一次分析。
4. 用自己的 tree、geography、regions 文件创建项目，并知道怎么看结果、怎么反馈错误。

## 一、你可以直接转发给测试用户的话

请先安装 R 和 RStudio，然后按下面教程操作：

```text
iBiogeobears 中文用户测试教程：
https://github.com/XuWeiEvo/iBioGeoBEARS/blob/main/docs/user-tutorial.zh-CN.md
```

推荐先运行内置示例，不要一开始就用自己的数据。示例能跑通，说明软件安装和本机环境基本正常。

如果失败，请把以下内容发回：

- R 控制台里的完整错误信息。
- `diagnostics_zip` 生成的诊断压缩包。
- 如果没有生成诊断压缩包，请发 `analysis.yml`、`tables/model_run_status.csv`、`tables/input_validation.csv` 和 `logs/` 文件夹。

## 二、安装前准备

用户电脑需要：

- R，建议 4.3 或更新版本。
- RStudio，推荐但不是必须。
- 能访问 GitHub 和 CRAN 的网络。
- 一个容易找到的英文路径，例如 `C:/iBiogeobears_test` 或桌面上的测试文件夹。

真实 BioGeoBEARS 分析还需要：

- BioGeoBEARS R 包。
- Shiny 图形界面需要 `shiny` 包。
- HTML 报告建议安装 Quarto。
- PDF 报告还需要 TinyTeX、MiKTeX 或 TeX Live；测试阶段可以先只用 HTML 报告。

## 三、安装 iBiogeobears

打开 R 或 RStudio，运行：

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS")
```

加载软件：

```r
library(iBiogeobears)
```

检查本机环境：

```r
check_installation()
```

这个表会告诉你哪些组件已经可用，哪些还缺失。第一次测试时，先不要急着解释结果，先确认环境检查能看懂。

如果想打开包内中文快速开始：

```r
open_user_guide(language = "zh-CN")
```

## 四、安装 BioGeoBEARS

`iBiogeobears` 不自带 BioGeoBEARS。真实运行 DEC、DEC+J、DIVALIKE、DIVALIKE+J、BAYAREALIKE、BAYAREALIKE+J 前，必须另外安装 BioGeoBEARS。

先查看安装计划：

```r
biogeobears_install_plan()
```

确认后执行安装：

```r
install_biogeobears(execute = TRUE)
```

安装完成后检查：

```r
check_biogeobears(required = FALSE)
check_installation()
```

如果 BioGeoBEARS 仍然显示不可用，先不要运行真实分析。请把 `check_installation()` 和 `check_biogeobears(required = FALSE)` 的输出发回。

## 五、打开 Shiny 图形界面

如果还没有安装 Shiny：

```r
install.packages("shiny")
```

启动图形界面：

```r
library(iBiogeobears)
launch_app()
```

浏览器会打开 `iBiogeobears` 页面。第一次打开时，软件会自动准备一个临时示例项目。

界面左侧是操作区，右侧是结果区。新版界面默认比较简单，普通用户优先看中文入口：

- `首页`：看“下一步”提示，按提示一步一步操作。
- `结果`：只看最重要的结果，包括祖先重建图、模型比较表和事件统计。
- `环境检查`：检查安装环境。
- `高级结果`：查看完整表格、图形和高级细节。
- `排错`：排查 warning、失败模型、日志和路径。

普通用户主要用这几个按钮：

- `1 创建示例项目`：创建内置示例项目。
- `创建自己的分析项目`：用自己的数据创建项目。
- `2 检查输入`：检查输入文件是否合格。
- `3 运行流程`：运行 dry run 或真实分析。
- `4 生成报告`：生成报告。
- `生成结果压缩包`：生成可分享的结果压缩包。
- `生成诊断压缩包`：生成诊断压缩包，方便排查错误。

## 六、先用内置示例数据测试

这是推荐的第一次操作流程。

1. 打开 R 或 RStudio。
2. 运行：

```r
library(iBiogeobears)
launch_app()
```

3. 在 Shiny 左侧 `开始` 区域点击 `1 创建示例项目`。
4. 如果需要检查安装环境，打开 `环境检查`，点击 `刷新环境检查`。
5. 点击 `2 检查输入`。
6. 保持 `Dry run` 勾选。
7. 点击 `3 运行流程`。

Dry run 不会真正运行 BioGeoBEARS，只检查项目、输入文件和输出目录。Dry run 通过后：

1. 确认 BioGeoBEARS 已安装。
2. 取消勾选 `Dry run`。
3. 如果界面里有 `真实运行时要求 BioGeoBEARS 可用`，真实运行时建议勾选。
4. 点击 `3 运行流程`。
5. 等六个模型运行完成。
6. 点击 `4 生成报告`，报告格式选 `html`。
7. 打开 `结果`，先看祖先重建图、模型比较表和事件统计。
8. 如果需要打包，打开左侧 `导出和排错`，点击 `生成结果压缩包`。
9. 如需反馈问题，点击 `生成诊断压缩包`。

如果不想用图形界面，也可以在 R 里直接运行：

```r
library(iBiogeobears)

project <- create_example_project("ibgb_example")

dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)

result <- run_workflow(project$config, dry_run = FALSE)

report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)

report
results_zip
diagnostics_zip
```

## 七、准备自己的输入数据

自己的分析至少需要三个文件。

### 1. 系统树文件

Newick 格式，例如：

```text
((sp1:1,sp2:1):1,(sp3:1,(sp4:1,sp5:1):1):1);
```

注意：

- tip 名称必须和 geography CSV 里的物种名完全一致。
- 大小写、下划线、空格都必须一致。
- 建议先避免中文物种名和特殊符号。

### 2. geography CSV

第一列是物种名，可以叫 `species` 或 `taxon`。后面每一列是一个地理区域，用 `0` 或 `1` 表示该物种是否在该区域出现。

示例：

```csv
species,A,B,C
sp1,1,0,0
sp2,1,1,0
sp3,0,1,0
sp4,0,1,1
sp5,0,0,1
```

要求：

- 每个物种一行。
- 区域列只能用 `0` 或 `1`。
- 每个物种至少有一个区域为 `1`。
- geography 里的物种必须和树的 tip 完全匹配。

### 3. regions CSV

`region` 列必须覆盖 geography CSV 里的所有区域列。

示例：

```csv
region,label,color
A,Region A,#1b9e77
B,Region B,#d95f02
C,Region C,#7570b3
```

要求：

- `region` 里的值要和 geography 的区域列名一致。
- `label` 是报告和图形里显示的区域名称。
- `color` 可以先照着示例写，也可以后续调整。

## 八、用 Shiny 导入自己的数据

推荐普通用户用这个流程。

1. 打开 Shiny：

```r
library(iBiogeobears)
launch_app()
```

2. 在左侧找到 `使用自己的数据`。
3. 在 `项目名` 填项目名，例如 `my_clade`。
4. 在 `项目保存位置` 填项目保存位置。
5. 上传：
   - `系统树文件`
   - `分布矩阵 CSV`
   - `区域信息 CSV`
6. 看 `上传预览` 表，确认三个文件都是 `可读取`。
7. 设置 `最大分布区数量`。
8. 选择要运行的模型。第一次建议保留全部六个模型。
9. 点击 `创建自己的分析项目`。
10. 点击 `2 检查输入`。
11. 如果验证失败，先看右侧或 `tables/input_validation.csv` 的修复建议。
12. 保持 `Dry run` 勾选，点击 `3 运行流程`。
13. Dry run 通过后，取消 `Dry run`，点击 `3 运行流程` 跑真实分析。
14. 点击 `4 生成报告`。
15. 点击 `生成结果压缩包`。

如果验证失败，不建议勾选 `Force execution after validation failure`。这个选项只适合确认问题可以忽略的高级用户。

## 九、用 R 命令导入自己的数据

如果用户更习惯 R 命令，也可以这样做：

```r
library(iBiogeobears)

project <- create_analysis_project(
  path = "my_ibgb_project",
  project_name = "my_clade",
  tree_file = "my_tree.nwk",
  geography_file = "my_geography.csv",
  regions_file = "my_regions.csv",
  max_range_size = 3
)
```

先 dry run：

```r
dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

再真实运行：

```r
result <- run_workflow(project$config, dry_run = FALSE)
```

生成报告和压缩包：

```r
report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)
```

## 十、怎么看分析结果

分析完成后，结果通常在：

```text
results/<project_name>/
```

重点看这些文件：

```text
reports/summary_report.html
tables/shiny_run_summary.csv
tables/model_comparison.csv
tables/event_summary.csv
tables/best_fit_events.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
figures/
```

### 1. HTML 报告

优先打开：

```text
reports/summary_report.html
```

报告里会汇总：

- 成功运行了几个模型。
- 哪个模型统计拟合最好。
- 祖先分布重建图。
- 事件汇总。
- 是否触发 `+J` 模型解释 caution。
- 是否有 warning 或失败模型。
- 关键表格和图形。

### 2. model_comparison.csv

这个表用于看模型拟合结果。重点列包括：

- `model`
- `logLik`
- `num_params`
- `AICc`
- `delta_aicc`
- `aicc_weight`
- `caution_flag`
- `interpretation_note`

注意：最低 AICc 是统计拟合结果，不等于自动的生物学结论。

### 3. event_summary.csv

这个表用于快速看祖先状态变化对应的事件概览。常见类别包括：

- `Range expansion`
- `Local extinction`
- `Range shift`
- `Range origin from null`
- `No range change`

注意：这个表是根据最高概率祖先状态沿分支变化推导出来的概览，不是 stochastic mapping 的正式事件计数。

### 4. model_sensitivity.csv

这个表用于看 `+J` 模型是否影响解释。尤其要看：

- 最优模型是不是 `+J` 模型。
- 是否有 `+J` 模型进入 `delta_aicc <= 2`。
- 非 `+J` 模型和 `+J` 模型之间是否需要谨慎比较。

如果报告里出现 `+J caution`，这不是程序错误，而是提醒解释时要保守。

### 5. model_run_status.csv

这个表用于排查模型是否成功。重点列包括：

- `model`
- `status`
- `run_action`
- `warnings`
- `error_message`
- `log_file`
- `result_file`

如果某个模型失败，先看 `error_message` 和 `log_file`。

## 十一、如何把结果发给别人

如果分析成功，优先发结果压缩包：

```r
results_zip <- bundle_results(result, overwrite = TRUE)
results_zip
```

如果分析失败或有 warning，优先发诊断压缩包：

```r
diagnostics_zip <- bundle_diagnostics(result, overwrite = TRUE)
diagnostics_zip
```

诊断压缩包通常更适合排查问题，因为它包含配置、状态表、日志和 session 信息，但不会包含完整的大型原始结果对象。

## 十二、常见问题

### 1. 提示找不到 BioGeoBEARS

运行：

```r
biogeobears_install_plan()
install_biogeobears(execute = TRUE)
check_biogeobears(required = FALSE)
```

如果仍失败，把输出发回。

### 2. Shiny 打不开

运行：

```r
install.packages("shiny")
library(iBiogeobears)
launch_app()
```

如果还是失败，把 R 控制台错误信息发回。

### 3. HTML 报告没有生成

运行：

```r
check_report_environment()
```

如果 Quarto 不可用，先只查看 `reports/summary_report.qmd` 和 `tables/` 里的结果表。

### 4. 输入检查失败

打开：

```text
tables/input_validation.csv
```

看 `status`、`detail` 和 `repair_step`。常见原因是：

- 树的 tip 名称和 geography 物种名不一致。
- geography 区域值不是 `0` 或 `1`。
- 某个物种没有任何分布区域。
- `max_range_size` 大于区域总数。
- regions CSV 没有覆盖所有 geography 区域列。

### 5. 某个模型失败

打开：

```text
tables/model_run_status.csv
```

找到失败模型对应的 `log_file`。如果想只重跑失败模型：

```r
result <- run_workflow(
  project$config,
  dry_run = FALSE,
  retry_failed_only = TRUE
)
```

### 6. 第二次运行为什么很快

软件默认会复用已经完成、且输入没有变化的模型结果。这是正常行为。

如果想强制全部重跑：

```r
result <- run_workflow(
  project$config,
  dry_run = FALSE,
  resume_completed_models = FALSE
)
```

## 十三、测试用户反馈模板

请测试用户按这个格式反馈：

```text
操作系统：
R 版本：
iBiogeobears 安装方式：
是否能打开 Shiny：
是否能跑通内置示例 dry run：
是否能跑通内置示例真实分析：
是否能生成 HTML 报告：
是否使用了自己的数据：
失败步骤：
完整错误信息：
是否生成 diagnostics_zip：
补充说明：
```

如果测试自己的数据，请同时说明：

```text
树的 tip 数量：
地理区域数量：
max_range_size：
运行了哪些模型：
是否有 +J caution：
```

## 十四、测试阶段建议

第一轮普通用户测试建议按顺序做：

1. 只测试安装。
2. 打开 Shiny。
3. 跑内置示例 dry run。
4. 跑内置示例真实分析。
5. 生成 HTML 报告。
6. 打包结果和诊断文件。
7. 最后再测试自己的数据。

如果前面任何一步失败，请先停下并反馈，不要继续叠加更多操作。
