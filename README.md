# iMast

**iMast** is a free, open-source, graphical statistical software for **reference material research and clinical measurement statistics**, built as an extension of [jamovi](https://www.jamovi.org). It implements 153 statistical methods across 15 self-developed R modules, covering reference-material characterization, commutability, homogeneity and stability, measurement uncertainty (GUM/JCGM 100), and the relevant Clinical and Laboratory Standards Institute (CLSI) evaluation protocols (EP05, EP06, EP09, EP10, EP14, EP15, EP17, EP21, EP24, EP30).

iMast is published in the *Journal of Statistical Software*; see **Citation** below.

## Key features

- **Reference-material-centered clinical statistics** — commutability (EP14 / EP30 / IFCC), characterization and value assignment, between-unit homogeneity, long-term stability, measurement uncertainty (GUM/JCGM 100), plus supporting precision, linearity and method-comparison procedures.
- **Graphical user interface** — spreadsheet data editor, point-and-click analyses, exportable tables and plots, built on the mature jamovi GUI framework.
- **Assumption checks before analysis** — distributional and homogeneity tests are run automatically to guide method choice.
- **AI-assisted workflow** — an optional large-language-model assistant (DeepSeek) helps with data import, variable-type inference, and analysis guidance. Statistical computation remains fully deterministic in the R engine; the LLM only assists natural-language-to-workflow conversion.
- **Docker-based deployment** — zero-configuration installation on Windows via an Electron desktop wrapper that imports the Docker image.

## Repository layout

```
imast/
├── modules/            # 15 jamovi R modules (DESCRIPTION + R/*.b.R, *.h.R)
│   ├── ReferenceMaterial/        # commutability, homogeneity, stability,
│   │                             #   characterization, uncertainty, equivalence
│   ├── CommutabilityAndStability/
│   ├── Appraisal/                # appraisal ratio studies
│   ├── ANOVA, Descriptive, Estimation, Regression, ChiSquare, ...
├── llm/                # Python AI service (FastAPI)
│   ├── server.py                 # LLM bridge, tool calling, skills
│   ├── page.html                 # chat UI
│   ├── skills/                   # preset reference-material analysis workflows (JSON)
│   └── workflows/                 # workflow parameter templates (JSON)
├── desktop/            # Electron desktop wrapper (main.js, preload.js, loading.html)
├── data/               # example clinical / reference-material datasets (CSV)
├── replication/        # standalone R replication script for the paper
└── LICENSE             # GNU AGPL v3
```

## Installation

iMast runs as a Docker container that wraps the jamovi engine, launched by an Electron desktop application.

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) and start it.
2. Install the iMast desktop application (Windows installer provided separately).
3. Launch iMast; on first run the Docker image `jamovi/jamovi:2.7.2-imast` is loaded and the container is started automatically.
4. The application opens at `http://127.0.0.1:41337`.

To run the LLM assistant, set your DeepSeek API key in the application **Setup** dialog (or via the `DEEPSEEK_API_KEY` environment variable). The AI feature is optional and is not required to run any statistical analysis.

## Reproducing the paper's results

The numerical results and figures in the JSS article can be reproduced with base R and two CRAN packages only — iMast itself is not required:

```r
install.packages(c("outliers", "mcr"), repos = "https://cloud.r-project.org/")
setwd("replication")
source("imast_replication.R")
```

This reproduces the three case studies (EP05 precision, EP09/EP14-A3 method comparison and commutability, GUM uncertainty). See `replication/REPRODUCIBILITY.md` for expected output values.

## Citation

If you use iMast in your research, please cite the JSS article:

> Z. Zhou, Y. Yin, and J. Chen. (year). iMast: Integrated Statistical Software for Reference Material Research with Assumption-Guarded Workflows and a Metrological Equivalence Model. *Journal of Statistical Software*, **vol.**(issue), pages. URL https://jstatsoft.org/...

(Bibliographic details are completed at publication.)

## License

iMast is free software released under the **GNU Affero General Public License v3.0** (AGPL-3.0), the same license as jamovi. See [`LICENSE`](LICENSE) for the full text. Some upstream jamovi components are GPL-2+, which is compatible with AGPL-3.0.

## Authors

- **Zhiwei Zhou** — Peking University Cancer Hospital & Institute
- **Yongfeng Yin** — Beihang University
- **Jinfeng Chen** (corresponding) — Peking University Cancer Hospital & Institute, <chenjinfengdoctor@bjmu.edu.cn>
