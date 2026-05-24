## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  fig.width = 6,
  fig.height = 4,
  fig.align = 'center'
)

## ----library------------------------------------------------------------------
library(TukeyC)

## ----crd-quick----------------------------------------------------------------
data(CRD1)

tk1 <- with(CRD1,
            TukeyC(y ~ x,
               data = dfm,
               which = 'x'))
summary(tk1)

## ----crd-plot, fig.cap="CRD1: treatment means with min-max dispersion bars and TukeyC groups."----
plot(tk1,
     dispersion = 'mm',
     d.col = 'steelblue')

## ----input-classes------------------------------------------------------------
## From: aov
av1 <- with(CRD1, aov(y ~ x, data = dfm))
tk2 <- TukeyC(av1, which = 'x')
summary(tk2)

## From: lm
lm1 <- with(CRD1, lm(y ~ x, data = dfm))
tk3 <- TukeyC(lm1, which = 'x')
summary(tk3)

## ----crd-unbalanced-----------------------------------------------------------
## Remove the first observation to create an unbalanced dataset
u_tk1 <- with(CRD1,
              TukeyC(y ~ x,
                 data = dfm[-1, ],
                 which = 'x'))
summary(u_tk1)

## ----plot-unbal, fig.cap="CRD1 (unbalanced): adjusted means with SD bars."----
plot(u_tk1, dispersion = 'sd', d.col = 'tomato')

## ----rcbd---------------------------------------------------------------------
data(RCBD)

tk4 <- with(RCBD,
            TukeyC(y ~ blk + tra,
               data = dfm,
               which = 'tra'))
summary(tk4)

## ----rcbd-plot, fig.cap="RCBD: treatment means with CI bars."-----------------
plot(tk4,
     dispersion = 'ci',
     d.col = 'darkgreen',
     d.lty = 2)

## ----sig-level----------------------------------------------------------------
## alpha = 0.01 (stricter)
tk_01 <- with(RCBD,
              TukeyC(y ~ blk + tra,
                 data = dfm,
                 which = 'tra',
                 sig.level = 0.01))

## alpha = 0.10 (looser)
tk_10 <- with(RCBD,
              TukeyC(y ~ blk + tra,
                 data = dfm,
                 which = 'tra',
                 sig.level = 0.10))

cat('--- sig.level = 0.01 ---\n')
summary(tk_01)

cat('--- sig.level = 0.10 ---\n')
summary(tk_10)

## ----fe-main------------------------------------------------------------------
data(FE)

## Main effect: factor N
tk5 <- with(FE,
            TukeyC(y ~ blk + N*P*K,
               data = dfm,
               which = 'N'))
summary(tk5)

## ----fe-nested----------------------------------------------------------------
## Nested: levels of N within level 1 of P
tk6 <- with(FE,
            TukeyC(y ~ blk + N*P*K,
               data = dfm,
               which = 'P:N',
               fl1 = 1))
summary(tk6)

## Nested: levels of N within level 2 of P
tk7 <- with(FE,
            TukeyC(y ~ blk + N*P*K,
               data = dfm,
               which = 'P:N',
               fl1 = 2))
summary(tk7)

## ----spe----------------------------------------------------------------------
data(SPE)

## Sub-plot factor SP (residual error, default)
tk8 <- with(SPE,
            TukeyC(y ~ blk + P*SP + Error(blk/P),
               data = dfm,
               which = 'SP'))
summary(tk8)

## Whole-plot factor P (must specify the blk:P error term)
tk9 <- with(SPE,
            TukeyC(y ~ blk + P*SP + Error(blk/P),
               data = dfm,
               which = 'P',
               error = 'blk:P'))
summary(tk9)

## ----plot-crd2, fig.width=8, fig.height=5, fig.cap="CRD2: 45 treatment means with pooled CI bars."----
data(CRD2)

tk10 <- with(CRD2,
             TukeyC(y ~ x,
                data = dfm,
                which = 'x'))

plot(tk10,
     id.las = 2,
     yl = FALSE,
     dispersion = 'cip',
     d.col = 'steelblue')

## ----plot-four, fig.width=8, fig.height=7, fig.cap="The four dispersion options applied to CRD1. (A) mm: min-max range; (B) sd: standard deviation; (C) ci: individual confidence interval; (D) cip: pooled confidence interval."----
op <- par(mfrow = c(2, 2), mar = c(4, 3, 4, 1))

plot(tk1, dispersion = 'mm',  d.col = 'steelblue')
mtext('(A)', side = 3, adj = 0, line = 2, font = 2)

plot(tk1, dispersion = 'sd',  d.col = 'tomato')
mtext('(B)', side = 3, adj = 0, line = 2, font = 2)

plot(tk1, dispersion = 'ci',  d.col = 'darkgreen')
mtext('(C)', side = 3, adj = 0, line = 2, font = 2)

plot(tk1, dispersion = 'cip', d.col = 'purple')
mtext('(D)', side = 3, adj = 0, line = 2, font = 2)

par(op)

## ----boxplot, fig.cap="CRD1: boxplot with TukeyC group labels and means (red line)."----
## boxplot.TukeyC re-evaluates the data argument from the original call;
## pass CRD1$dfm directly so it is findable in any environment.
tk1_bp <- TukeyC(y ~ x,
             data = CRD1$dfm,
             which = 'x')

boxplot(tk1_bp,
        mean.col = 'red',
        mean.lwd = 2,
        args.legend = list(x = 'topright'))

## ----xtable, results='asis'---------------------------------------------------
library(xtable)

tb <- xtable(tk4,
             caption = 'RCBD: Tukey HSD grouping of treatment means.',
             digits = 3)
print(tb,
      type = 'html',
      html.table.attributes = 'border="1" style="border-collapse:collapse; padding:4px;"',
      caption.placement = 'top',
      include.rownames = FALSE)

## ----lmer, eval=requireNamespace('lme4', quietly=TRUE)------------------------
library(lme4)

data(RCBD)

lmer1 <- with(RCBD,
              lmer(y ~ (1|blk) + tra,
                   data = dfm))

tk11 <- TukeyC(lmer1, which = 'tra')
summary(tk11)

