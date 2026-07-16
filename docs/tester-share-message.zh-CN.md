# BioGeoSyn 测试用户简短说明

请先安装 R 和 RStudio，然后在 RStudio 里运行：

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/BioGeoSyn")
library(BioGeoSyn)
create_windows_launcher()
launch_app()
```

Windows 用户运行 `create_windows_launcher()` 后，之后可以直接双击桌面上的
`start-BioGeoSyn.bat` 打开软件。

第一次测试建议只跑内置示例数据：

1. 打开软件后，在 `首页` 看“下一步”提示。
2. 点击 `1 创建示例项目`。
3. 点击 `2 检查输入`。
4. 保持 `Dry run` 勾选，点击 `3 运行流程`。
5. 如果 dry run 通过，并且 BioGeoBEARS 已安装，取消 `Dry run`。
6. 再点一次 `3 运行流程`，等待真实分析完成。
7. 点击 `4 生成报告`。
8. 打开 `结果`，主要看三部分：祖先分布重建图、模型比较表、事件统计。
9. 点击 `生成结果压缩包`，用于分享结果。
10. 如果报错，点击 `生成诊断压缩包`，把压缩包和错误截图发回。

如果要测试自己的数据，请在左侧 `使用自己的数据` 上传三个文件：

- 系统树文件：Newick 格式。
- 分布矩阵 CSV：第一列是物种或样本名，后面每一列是地理区域，通常用 0/1 表示是否分布。
- 区域信息 CSV：区域编号和区域名称。

上传后先看 `上传预览`。三个文件都显示 `可读取` 后，再点击 `创建自己的分析项目`，然后按首页的“下一步”继续。

更完整的中文教程在：

```text
https://github.com/XuWeiEvo/BioGeoSyn/blob/main/docs/user-tutorial.zh-CN.md
```
