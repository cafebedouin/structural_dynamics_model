# Archive — pre-2026-07-25 SCOPE decomposition manifests (NOT a read surface)

**Status: archive-not-read-surface (operator ruling, OQ-254, 2026-07-25).**

These 515 manifests are the Q-choice records (axis selection, deferred axes with
deferral reasons, kernel verdicts, fracture scans) written by the decomposition step
between 2026-06-05 and 2026-07-25, copied verbatim from gitignored `outputs/`
(`kernel_manifests/`, `decompose/manifests/`, `kernel_first_phase0/` — relative
structure preserved; originals untouched). Copying is not backfill: nothing was
synthesized — but these files have **no self-provenance stamp** (`_provenance`) and
**no story-side joins** (no live story's `epsilon_provenance/5` arg 4 names them), so
they must never be treated as an authoritative joinable surface.

- The **live** read surface is the sibling dirs under `agent/decompose_manifests/`
  (`flat/`, `<run_tag>/`, `decompose/`), where manifests carry `_generation_run_id`
  (== filename stem) and `_provenance`, written by the post-2026-07-25 generators.
- `python/q_provenance_readout.py` DOES scan this archive, but a resolution here emits
  its own token `joined_archive_not_authoritative` — never `joined` (that would certify
  a join against a surface declared non-authoritative) and never
  `run_id_authored_manifest_unreachable` (the manifest exists; the Pattern-6 bucket
  would be a false alarm).

Historical record only. Provenance: `audits/2026-07-25_oq254_q_provenance/`.
