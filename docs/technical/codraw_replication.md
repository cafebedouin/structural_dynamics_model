# Co-draw replication — checking whether a cross-story finding survives redraw

**Reader and decision.** You hold a finding that composes **more than one generated story** — a
kernel's pairwise Jaccard, an H¹-across-readings, a cross-reading divergence pattern, any
"reading A and reading B relate like X" — and you are deciding whether it can carry weight in an
essay, a paper claim, or an OQ resolution. This runbook converts that check from a project into a
decision. Everything here was executed on 2026-08-14; the worked example is
`audits/2026-08-14_cheap_confession_codraw_replication/`.

**Why this class specifically.** A per-story figure is one draw of one thing. A cross-story figure
composes N independently generated stories, so its stability is the *product* of theirs — and it is
the shape most easily mistaken for a measurement. Witnessed: a pairwise-Jaccard structure that did
not survive k=3 while its per-pair agreement counts were rock stable (62 in all four runs). The
numbers looked reproducible. The *structure* was not.

---

## Step 0 — Pre-register. This is the load-bearing step, not the run.

**The failure mode of a cheap replication is a menu.** Run it three times without a verdict rule
fixed in advance and you get three numbers, read whichever way you were already leaning. The
mechanics below are the easy half.

Write the pre-registration to a scratch path, `md5sum` it, and only then run. Move it into the
audit dir unchanged afterward — the hash is what makes "written before" checkable.

**(a) Pin an ORDINAL claim, not a number.** The numeric version usually has a denominator built
from the same churning units it is measuring. `cs_kernel_registry.pl:130`:
`Jaccard = AgreeN / (2·NCtx − AgreeN)`, where `NCtx` counts only *comparable* contexts — so it
shrinks when a reading abstains, and abstention is itself draw-dependent. The baseline 0.270 was
62/(2·146−62); had one more reading abstained, the same agreement would have printed a different
number. Ordinal claims ("exactly one of six pairs is non-zero, and it is A×B") are invariant to
that. **State explicitly that no verdict may be read off the magnitude.**

**(b) Fix the verdict rule before the run.** The OQ-264 standard: replicated iff present in **all
k**; 1–(k−1) is an *observation*, not a replication; 0 is collapse. Write the three limbs and what
each does to the claim.

**(c) Handle absence tokens explicitly.** `pair_reading_agreement/7` yields `J = null` for a pair
with no comparable context — deliberately not 1.0, and equally **not 0**. Pre-commit that a null
makes the draw *uncountable* toward either limb and reduces k, rather than counting as
falsification. Same for a draw that loses a story entirely (see below): a claim quantified over 6
pairs cannot be evaluated on 3.

**(d) Name the confound the co-draw does NOT close.** Redrawing re-authors everything, so anything
determined by authoring survives untouched. Example: a `‡` institutional seat is role-authored
(OQ-188), and two readings can agree there for that reason in *every* draw. Write the secondary
partition that would separate it, and record it as not-run if the primary collapses first.

**(e) If a threshold comes from another experiment, state its distributional assumption.** A
borrowed interval imports the lending run's shape silently, and the error leaves no trace.
Witnessed in the same session: a repair-check sourced its "noise floor" from these very co-draws
and returned a clean LEXICAL verdict — the interval [0.15, 0.61] was not noise at all but a
**bimodal switch** between two authoring modes, and the compared value sat between them, belonging
to neither. The verdict had to be voided. Sourcing a threshold from a sibling run is good practice
*and* owes an explicit check that the lender's distribution is the shape you are assuming.

---

## Step 1 — Validate the read path BEFORE spending on draws

Two controls, both free. If either fails, stop: you cannot read the draws you are about to buy.

**Control 1 — the predicate reproduces the report.** Call the statistic directly and confirm it
matches what the rendered report already says:

```bash
cd prolog && swipl -g "
[stack], corpus_loader:load_all_testsets,
cs_kernel_registry:compare_kernel_readings(<kernel_id>, _P, PS),
forall(member(pair(R1,R2)-stats(J,A,D), PS),
   ( R1=_-C1, R2=_-C2, format('~w | ~w | J=~w agree=~w diverge=~w~n',[C1,C2,J,A,D]) )),
halt" -t "halt(1)"
```

**Control 2 — a small isolate reproduces the full corpus.** Copy the kernel's stories (plus its
`*_contradictions.pl`) to a scratch dir, overlay `corpus_path`, and confirm byte-identical output.
This is genuinely two-sided: the signature layer carries corpus-relative inputs, so a 273→5
reduction *could* move the numbers. If it does, the isolate design is invalid and you need
full-corpus draws.

```bash
cd prolog && swipl -g "
[stack],
asserta(config:param(corpus_path, '/abs/path/to/isolate')),
corpus_loader:load_all_testsets, ... " -t "halt(1)"
```

**`asserta`, never `assertz`** — `config.pl` defines the default `corpus_path` as the first clause
and the loader takes the first solution, so an appended override is silently ignored and you load
the default corpus while the count looks plausible.

---

## Step 2 — Run k draws into run-tagged subdirs

`--run-tag` writes to `json/<tag>/` + `prolog/testsets/<tag>/`, and the corpus glob is
non-recursive, so **nothing enters the live corpus**. Combined with `--close-gaps`, the empty
tagged dirs make every seed in the frozen manifest a gap, so the whole set regenerates *together*
— which is the point: each reading is drawn in the presence of its siblings.

```bash
M=agent/decompose_manifests/flat/<family>_<timestamp>.manifest.json

# free confirmation of the seed set — no LLM spend
python3 agent/c-orchestrator.py --close-gaps --dry-run --run-tag codraw_01 --manifest-file $M \
  | grep -E '^\[close_gaps\]'

for N in 01 02 03; do
  python3 agent/c-orchestrator.py --close-gaps --no-commit --run-tag codraw_$N --manifest-file $M
done
```

`--close-gaps` also avoids the topic argument entirely (it skips research and decompose). Run the
draws **sequentially**; do not parallelize them against each other.

**A draw may lose a story.** This is a base rate, not an anomaly — 3 of 4 runs of one manifest each
dropped a *different* declared story (schema violation, JSON parse error). Since 2026-08-15 the
orchestrator reconciles declared-vs-landed and exits non-zero, so a short draw announces itself.
Per (c), a short draw is uncountable, not a failure of the claim.

## Step 3 — Read each draw

Same predicate as Control 1, with `corpus_path` overlaid onto each tagged dir. Tabulate all k
against the baseline and apply the rule from (b) **as written** — including when the rule's inputs
turn out to be shakier than assumed. If an input was mis-specified, void the verdict and say so;
do not re-read the rule to fit the result.

## Step 4 — Land it

Move the tagged dirs into `audits/<date>_<slug>/evidence/` and delete them from the live tree —
generation is stochastic, so these artifacts are **irreproducible** and the audit is uncheckable
without them. Verify by content (`diff -rq`), not by file count, before deleting. The audit-cite
gate requires frozen evidence to be committed, so `git add` the audit dir; an uncommitted citation
is a promise, not a record.

---

## What this does not do

It re-runs the *generation*, holding the manifest frozen. It therefore controls for
manifest-determined structure rather than measuring it, and says nothing about whether the
decomposition into those readings was right in the first place. It also cannot distinguish "the
finding was an artifact" from "the finding is real but the instrument is too noisy to show it" —
both present as collapse.
