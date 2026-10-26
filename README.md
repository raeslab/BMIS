# Scripts for the BMIS project

### Files description
1. [fitnsim_multinom.R] Fit multinomial model and create simulated dataset with fitted parameters (Language: R) 
2. [fitnsim.example_input_data.tsv] Example dataset.

## Companion website and supplementary data

Companion website and supplementary data for:

> Vieira-Silva S, Falony G, Belda E, Nielsen T, Aron-Wisnewsky J, Chakaroun R, Forslund SK, Assmann K, Valles-Colomer M, Nguyen TTD, Proost S, Prifti E, others, MetaCardis Consortium, Clément K, Raes J. **Statin therapy is associated with lower prevalence of gut microbiota dysbiosis**. *Nature* 581(7808):310–315 (2020). [doi:10.1038/s41586-020-2269-x](https://doi.org/10.1038/s41586-020-2269-x)

It contains the quantitative microbiome profiling (QMP) matrices of the BMIS dataset (MetaCardis): genus abundances and gut-specific metabolic module (GMM) abundances. Metadata are available as Extended Data 1 of the article.

**Website:** https://raeslab.github.io/BMIS/ (GitHub Pages, served from `docs/` on the `master` branch)

This material was previously hosted on the Raes Lab website at `raeslab.org/software/BMIS/` and has been migrated to this repository.

### Contents

| Path | Size | Description |
|------|------|-------------|
| `supplementary/bmis.zip` | 630 KB | Archive with all supplementary files |
| `supplementary/qmp_GMM_abundances_BMIS.tsv` | 1.2 MB | QMP gut metabolic module (GMM) abundance matrix of the BMIS dataset. |
| `supplementary/qmp_genera_abundances_BMIS.tsv` | 536 KB | QMP genus abundance matrix of the BMIS dataset. |

Total supplementary data: 2.3 MB.

### Repository layout

```
docs/                 Static, self-contained website (GitHub Pages source; no external CSS/JS/fonts)
  index.html
  assets/             Stylesheet, script, logo, figure previews
supplementary/        Downloadable data files (Git LFS)
.gitattributes        Git LFS tracking rules
```

#### Git LFS

All files in `supplementary/` are tracked with [Git LFS](https://git-lfs.com). Install it before cloning
(`git lfs install`), or download individual files from the website. Because GitHub Pages does not serve LFS
objects, the website's download buttons point to `https://github.com/raeslab/BMIS/raw/master/supplementary/...`;
when `docs/index.html` is opened from a local checkout the links switch to the relative `../supplementary/` paths.

#### Publishing

In the repository settings, enable GitHub Pages with source *Deploy from a branch* → `master` / `/docs`.

### How to cite

If you use these data, please cite:

> Vieira-Silva S, Falony G, Belda E, Nielsen T, Aron-Wisnewsky J, Chakaroun R, Forslund SK, Assmann K, Valles-Colomer M, Nguyen TTD, Proost S, Prifti E, others, MetaCardis Consortium, Clément K, Raes J. **Statin therapy is associated with lower prevalence of gut microbiota dysbiosis**. *Nature* 581(7808):310–315 (2020). [doi:10.1038/s41586-020-2269-x](https://doi.org/10.1038/s41586-020-2269-x)

```bibtex
@article{vieirasilva2020statin,
  title   = {{Statin therapy is associated with lower prevalence of gut microbiota dysbiosis}},
  author  = {Vieira-Silva, S. and Falony, G. and Belda, E. and Nielsen, T. and Aron-Wisnewsky, J. and Chakaroun, R. and Forslund, S. K. and Assmann, K. and Valles-Colomer, M. and Nguyen, T. T. D. and Proost, S. and Prifti, E. and others and {MetaCardis Consortium} and Clément, K. and Raes, J.},
  journal = {Nature},
  year    = {2020},
  volume  = {581},
  number  = {7808},
  pages   = {310--315},
  doi     = {10.1038/s41586-020-2269-x}
}
```

### Contact

Raes Lab, VIB–KU Leuven Center for Microbiology – Prof. Jeroen Raes (jeroen.raes@kuleuven.be).
