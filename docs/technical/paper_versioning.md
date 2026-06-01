# Paper Versioning — How to Identify the Canonical Version

Generated 2026-05-28. Documents a mistake made in-session so future models don't repeat it.

---

## The Mistake to Avoid

There are multiple versions of `deferential_realism_paper_v*.md` in `docs/`. In one
session, the model (Haiku 4.5) identified v6.13.1 as canonical based on file size:
v6.13.1 was 1043 lines, v7 was 173 lines, so v7 was dismissed as a draft. This was
wrong. v7 is the canonical framework paper.

**Do not infer canonical status from file size.**

---

## Why Size Is a Misleading Signal Here

Each version of this paper is *additive* on the previous one. v7 explicitly states:
"Axioms 1–6 are unchanged from v6.13 and are not restated here." It adds Axiom 7,
Theorems 7–8, §4.5, and §5.11 — and references rather than restates the unchanged
content. A paper that builds on its predecessor without copying it will always be
shorter than one that restates everything. The canonical version is the one with the
highest version number that contains a complete abstract and final section structure,
regardless of length.

---

## How to Identify the Canonical Paper

1. **Read CLAUDE.md first.** The "Critical Distinctions" section names the canonical
   paper explicitly. Check it before looking at the files themselves — it will be more
   reliable than any heuristic applied to the files.

2. **If CLAUDE.md is stale, use the highest version number.** The versioning scheme is
   monotone: higher number = later = canonical. The sequence is:
   `v6.md → v6.2 → ... → v6.13 → v6.13.1 → v7.md`. v7 supersedes all of them for
   the full framework.

3. **Watch for parallel amendment files.** `v6.13.1` exists alongside `v7` — it is
   *not* a newer version of v7. It is an amendment to the v6.x observer-only line
   (Axiom 2 updated for OQ-26). Two files with similar high version numbers may be
   tracking different things (the full framework vs. a specific axiom amendment).
   Read the abstract of each to distinguish.

4. **Check the abstract for scope.** The canonical full-framework paper says something
   like "extends DR from one axis to two" or "Axioms 1–N, Theorems 1–M." A file that
   says "Version: v6.13.2" (Axiom 2 amendment plus the FNL signature-behavior errata) is a
   point release, not the canonical top-level paper.

---

## Current State (as of 2026-05-28)

| File | Role |
|------|------|
| `docs/deferential_realism_paper_v7.md` | **Canonical framework paper** — full two-axis DR (Axioms 1–7, Theorems 1–8) |
| `docs/deferential_realism_paper_v6.13.1.md` | Observer-only Axiom 2 amendment (OQ-26: ε is reading-relative across generation runs). Header now **v6.13.2** (FNL signature-behavior errata, OQ-49); filename retains `_v6.13.1` for reference stability |
| `docs/deferential_realism_paper_v6.13.md` | Superseded by v6.13.1 and v7 |
| Earlier versions | Superseded |

---

## The Corpus Rebuild Connection

v7 §5.11 ("corpus provenance and single-run coherence") explains why the main-corpus
constraint count is 223, not 3,337. The reduction was deliberate: exploratory
committer-axis generation runs created a chimera (reused constraint IDs, conflicting ε),
which was cleaned up and reduced to a single coherent run (kernel_run_03: 109 CS
readings, plus ~114 observer-axis constraints). A model that reads the 223 count without
this context will think the corpus is small; it is not small, it is clean.

If the constraint count in CLAUDE.md ever looks surprisingly low, check whether a corpus
rebuild has occurred before assuming something is wrong.
