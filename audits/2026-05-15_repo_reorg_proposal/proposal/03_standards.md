# 03 — Standards Survey

This document distinguishes established conventions from judgment calls. No "best practice"
claims are made without a source.

---

## Established Python Conventions

These are widely adopted and documented in Python ecosystem standards:

**PEP 518 (`pyproject.toml` at root):** This repo uses pyproject.toml — correct.

**`requirements.txt` at root:** Common pattern for pinning dependencies; this repo uses it.

**Flat layout (all source in one directory, not under `src/`):** This repo uses flat layout
with `python/` as the module directory. Flat layout is common for research code and scripts
and is explicitly supported by modern packaging tools. There is no standard that mandates
`src/` layout; it is a preference.

**`tests/` or test co-location:** Python ecosystem convention is to put tests in a `tests/`
directory or alongside source as `*_test.py`. This repo has no `tests/` directory; Prolog
`validation_suite.pl` serves the testing role.

**`docs/`, `examples/` at root:** Common and present in this repo.

**`LICENSE` at root:** Standard; present.

---

## Emerging AI-Agent Conventions

**`CLAUDE.md` / `AGENTS.md` at root:** Adopted by Anthropic (Claude Code) and OpenAI (Codex)
as the convention for AI-agent project instructions. This repo uses `CLAUDE.md` at root —
correct and current. There is no formal standard body for this; it is an industry practice
that has emerged since late 2024. The key property is that the file must be at root and must
be named CLAUDE.md or AGENTS.md for automatic discovery by CLI tools.

**Per-directory README.md files:** Useful for navigation in large repos, especially when AI
agents use directory trees as orientation. Not formally standardized, but increasingly common.
This repo has no per-directory READMEs (except the root README.md), which is a gap for a
10,000+ file repo.

---

## Judgment Calls (Labeled as Such)

The following are design choices I propose, not standards. Reasonable alternatives exist.

**I propose** that generated output and source code should be separable at the directory level
so that a fresh clone plus a pipeline run can regenerate outputs. This repo approximates this
(json/ is inputs, outputs/ is outputs) but several exceptions exist: docs/results/ receives
Python script output; agent/analysis/essays/ holds generated essays inside a source tree;
scs_*.pl generated files live at prolog/ root.

**I propose** that multiple redundant copies of the same file (sotu/pl/ = prolog/testsets_sotu/)
should have an explicit documented relationship, ideally with one being the canonical location
and the other being a symlink or documented copy. Neither currently has a README explaining the
relationship.

**I propose** that archive content should be visually distinct from active content at first
glance — either via directory naming (archives/, legacy/, deprecated/) or via a README that
clearly labels the scope. `prolog/archives/` (405 MB) has the right name but no README, and
its existence is not mentioned in the repo-level README or CLAUDE.md.

**I propose** that a per-directory README is warranted in directories with more than ~30 files
and non-obvious internal structure. Candidates: `prolog/`, `python/`, `docs/`, `agent/`, `sotu/`.
This is entirely my judgment — the project has functioned without them.

---

## What Is Not a Standard

**"AI-parseable repo standards"** beyond CLAUDE.md/AGENTS.md: There is no published standard
for structuring repos for AI model navigation. Recommendations in this proposal that go beyond
CLAUDE.md/AGENTS.md are design choices, not compliance requirements.

**Specific directory names** (e.g., audit_history/, corpus/, src/): None of the reorganization
proposals in doc 04 reflect an external standard. They reflect the goal of making the repo's
structure self-explanatory at first glance for a model that has read CLAUDE.md but nothing else.
