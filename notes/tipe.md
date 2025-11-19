---
title: TIPE - $\boxed{\Phi(p \rightarrow q) \overset{?}= \Phi(p) \Rightarrow \Phi(q)}$
subtitle:  _il va falloir rapidement trouver un titre_
author: Victor R
---

\newpage

# 1. Le $\lambda$-calcul

## 1.1. Cadre et géneralités

**TODO: mettre une phrase d'introduction/de motivation sur le lambda calcul**

::: {.box .def title="Définition 1. L'ensemble $\Lambda$"}

Soit $\mathcal{V}$ un ensemble de variables.
On définit l'ensemble $\Lambda$ par induction:

(i) $\forall v\in\mathcal{V},\ v\in\Lambda$
(ii) $\forall (x, s)\in\mathcal{V}\times\Lambda,\ (\lambda x.\ s)\in\Lambda$ *($\lambda$-abstraction)*
(iii) $\forall (s, t)\in\Lambda^2,\ (s\ t) \in\Lambda$ *(application)*

\
Les élements de $\Lambda$ sont appelés *$\lambda$-termes*.

:::

::: {.box .rem title="Remarques."}

La notation $\lambda x.\ X$ (ii) est l'équivalent en langage mathématiques de $x\mapsto X$ avec $X$ qui dépend de $x$
\

La notation $(s\ t)$ (iii) correspond à la composition de $s$ par $t$

:::

## 1.2. $\alpha$-équivalence

::: {.box .rem title="Rappels. (Relation binaire)"}

Une *relation binaire* sur $\Lambda$ est la donnée d'une partie $\mathcal{R}\subseteq\Lambda\times\Lambda$.

Pour tout $(s, t)\in\mathcal{R}$, on dit que *s est en relation avec t* et on note $s\mathcal{R}t$.

:::

::: {.box .def title="Définition 3. $\alpha$-équivalence"}

On définit l'$\alpha$-équivalence par induction:

(i) $\forall x\in\Lambda,\ x \equiv_\alpha x$
(ii) $\forall (s,t), (u,v)\in\Lambda^2,\ (s\ u)\equiv_\alpha (t\ v) \iff s \equiv_\alpha t\textit{ et }u\equiv_\alpha v$
(iii) Pour tous $s,t\in\Lambda$ avec $s \equiv \lambda x.\ X$ et $t \equiv \lambda y.\ Y$, on a $s\equiv_\alpha t$ si, et seulement si, il existe $z$ qui n'apparait ni dans $X$ ni dans $Y$ telle que $X[x\rightarrow z] \equiv_\alpha Y[y\rightarrow z]$

:::

## 1.3. $\beta$-réduction

::: {.box .def title="Définition k. $\beta$-réduction"}

TODO

:::

# 2. Les assistants de preuves

# 2.1. Contexte et cadre

# 2.2. Exemples

# 3. Le lien entre 1. et 2.

# 3.1.

# 3.2.

# 3.3. Présentation de mon travail

