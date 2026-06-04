# audits/ — consolidated audit archive

**Mandate (2026-06-04): every completed audit lives here, in one subdirectory per audit,
named `<YYYY-MM-DD>_<slug>/`.** The subdirectory holds the writeup AND its evidence
artifacts (probe scripts, raw JSON/TSV/logs) together. Do not scatter audit writeups into
`docs/` or leave findings only in `outputs/` — `outputs/` is gitignored, so findings left
there are unversioned and disappear on a fresh clone.

Conventions:

- **`outputs/` is the live workspace; `audits/` is the dated archive.** Audit *scripts* stay
  in `python/audits/` (or `prolog/`) and keep reading/writing `outputs/` — re-running a
  producer regenerates the workspace copy. When an audit pass completes, move (or copy) the
  writeup + evidence into its `audits/<date>_<slug>/` directory. Probe `.pl`/`.py` snippets
  written for one audit are archived in its subdirectory.
- **Date** = the audit's execution date (from the writeup header), not the consolidation
  date. **Slug** = short kebab/snake topic name.
- **Point-in-time documents are not retro-edited.** Archived writeups may reference paths as
  they were at execution time (e.g. `outputs/...` footers); only *live* pointers (ISSUES.md,
  KNOWN_STATE.md, code comments, orientation docs) are kept current.
- Audits follow the methodology in `CLAUDE.md` → Audit Methodology (recon → proposal →
  execution → writeup) and cite the pipeline manifest of the run they measured.

Consolidated 2026-06-04 from: `docs/*.md` audit writeups, `docs/audits/`,
`docs/technical/schema_drift_audit.md`, `outputs/` audit families, root `audit/`,
`audit_data/`, `audit_proposal/` (+ its `audit/agy/` variant), and `phase1/`
(see KNOWN_STATE.md 2026-06-04 entry for the move map and fork notes).
