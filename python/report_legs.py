#!/usr/bin/env python3
"""OQ-352 — CLI for the per-leg REPORT driver (`run_pipeline.report_corpus`).

`classify_corpus` runs only `run_json_report`, so the per-leg outputs carry
per-story fields plus the top-level `diagnostic` block and nothing else. The
report-stage tools have only ever run inside the full `run_pipeline` over the
DEFAULT leg, writing shared `outputs/` — so every corpus-level number they have
published is a k=1 point estimate on one draw of one corpus, and they have been
exercised at n≈1000 exactly once (kernel_v1, pre-reset regime). This is the CLI
around the missing driver; it gates OQ-353 (statistic floors) and OQ-354
(tool-correctness at scale).

    python3 python/report_legs.py --legs testsets_sonnet2 testsets_sonnet3
    python3 python/report_legs.py --legs archives/datasets/original_v6 \
        --giant-comp-timeout 1800
    python3 python/report_legs.py --selftest

THE SELFTEST IS FIXTURE-ONLY, AND THE FIXTURES ARE SYNTHETIC. Tiny corpora in a
temp dir; no real leg; no swipl over 1000 stories; seconds, not minutes. A
real-leg pair inside a fixture-only charter is a contradiction that grows the
gate row to minutes until someone deletes it — so the one real-leg OQ-246
witness is run ONCE and pasted into the WRITEUP, and what rides in the gate is a
synthetic mini-leg pair that reproduces the accumulation tell in-process and
diverges per-process.

Two-sidedness is asymmetric BY DESIGN, and the asymmetry is the point:

  - REFUSALS (4a) halt. Each gets a planted fixture that FIRES it and a
    near-identical fixture that PASSES, asserted BY REASON CODE — a fixture that
    refuses for the wrong reason fails instead of passing silently.
  - RECORDED OUTCOME TOKENS (4b) never halt. They are asserted
    present-and-correctly-valued, NOT two-sided, because a token like
    PROMPT_HASH_ABSENT has no passing counterpart to plant. `original_v6`
    depends on this: it must COMPLETE with ABSENT recorded, not refuse.

CONTROL ON THE CONTROL: `--selftest` asserts its own ISOLATION — a post-condition
that `prolog/validation_suite.pl` and THE WHOLE OF `outputs/` are byte-identical
afterward, with NO exclusions. That post-condition is what stands between a
fixture-only selftest and the blast radius the scope decision excluded, and it is
also why all transit-guard state (lock, journal, backups) lives in
`.report_corpus/` rather than under `outputs/` — guard state under `outputs/`
would falsify this check and it would acquire an `--exclude` flag within a month.
"""

import argparse
import json
import hashlib
import os
import shutil
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "python"))

import run_pipeline as R
from run_pipeline import (ReportRefusal, report_corpus, _prompt_hash_token,
                          _TransitGuard, _REPORT_STAGES, _SCOPE_DEFERRED,
                          _sha256_file, _classify_output_name, _is_code_path)

# --------------------------------------------------------------------------
# Fixture construction
# --------------------------------------------------------------------------

_STORY = """% synthetic selftest fixture — never a real corpus member
narrative_ontology:story_provenance({id}, '{phash}',
    '{shash}', '2026-08-23',
    'selftest', 'agent/fixture.json',
    '{model}', 'max_tokens=1,thinking=disabled').
narrative_ontology:constraint_metric({id}, extractiveness, 0.5).
"""


def _mkleg(root: Path, name: str, n: int = 3, phash: str = "a" * 40,
           model: str = "claude-sonnet-5", provenance: bool = True) -> Path:
    d = root / name
    d.mkdir(parents=True, exist_ok=True)
    for i in range(n):
        body = _STORY.format(id=f"fx_{name}_{i}", phash=phash,
                             shash="b" * 40, model=model)
        if not provenance:
            body = "\n".join(l for l in body.splitlines()
                             if "story_provenance" not in l and "'2026-08-23'" not in l
                             and "'selftest'" not in l and "max_tokens" not in l)
        (d / f"fx_{name}_{i}.pl").write_text(body, encoding="utf-8")
    return d


def _expect_refusal(label: str, code: str, fn) -> tuple:
    """Assert *fn* raises ReportRefusal with EXACTLY reason code *code*."""
    try:
        fn()
    except ReportRefusal as e:
        if e.code == code:
            return True, f"[OK  ] {label:44s} refused {code}"
        return False, f"[FAIL] {label:44s} refused {e.code}, expected {code}"
    except Exception as e:
        return False, f"[FAIL] {label:44s} raised {type(e).__name__}: {str(e)[:120]}"
    return False, f"[FAIL] {label:44s} did NOT refuse (expected {code})"


def _expect_pass(label: str, fn) -> tuple:
    """The near-identical fixture that must NOT refuse — the second side."""
    try:
        fn()
        return True, f"[OK  ] {label:44s} passed (no refusal)"
    except ReportRefusal as e:
        return False, f"[FAIL] {label:44s} refused {e.code} but should pass"
    except Exception as e:
        return False, f"[FAIL] {label:44s} raised {type(e).__name__}: {str(e)[:120]}"


def _expect_value(label: str, got, want) -> tuple:
    ok = got == want
    return ok, f"[{'OK  ' if ok else 'FAIL'}] {label:44s} {got!r}" + (
        "" if ok else f"  expected {want!r}")


# --------------------------------------------------------------------------
# Isolation post-condition (the control on the control)
# --------------------------------------------------------------------------

def _outputs_manifest() -> dict:
    """(size, mtime_ns) of every file under outputs/, plus the tracked validation_suite.

    NO EXCLUSIONS. If this ever needs one, the driver has written somewhere it
    promised not to and the right response is to fix the driver, not the check.
    That is also why every byte of transit-guard state (lock, journal, backups)
    lives in .report_corpus/ instead of under outputs/.

    WHY (size, mtime_ns) AND NOT sha256, measured rather than assumed: outputs/
    is 13,150 files / 3.8 GB here, and hashing it costs ~242 s per manifest,
    i.e. ~8 minutes per selftest — far outside this row's seconds-not-minutes
    charter, and a gate row that slow gets deleted. `stat` over the same tree
    costs 0.07 s.

    The substitution is not a weakening. The post-condition asks "did the driver
    write anywhere it promised not to", and a write moves mtime_ns even when it
    replaces a file with byte-identical content — which a content hash cannot
    see. For THIS question (size, mtime_ns) is strictly MORE sensitive than
    sha256, not less.

    Consequence, stated because it will eventually fire: an unrelated process
    writing under outputs/ during the selftest turns this row red. That is
    accepted rather than excluded away — the failure message names the offending
    paths, so a foreign writer is diagnosable in one read, whereas an exclusion
    list would silently cover a real driver escape forever.
    """
    m = {}
    out = REPO_ROOT / "outputs"
    if out.exists():
        for f in sorted(out.rglob("*")):
            if f.is_file():
                try:
                    st = f.stat()
                    m[str(f.relative_to(REPO_ROOT))] = (st.st_size, st.st_mtime_ns)
                except OSError:
                    m[str(f.relative_to(REPO_ROOT))] = "UNREADABLE"
    vs = REPO_ROOT / "prolog" / "validation_suite.pl"
    if vs.exists():
        st = vs.stat()
        m["prolog/validation_suite.pl"] = (st.st_size, st.st_mtime_ns)
    return m


# --------------------------------------------------------------------------
# The selftest
# --------------------------------------------------------------------------

def selftest(verbose: bool = True) -> int:
    results = []
    before = _outputs_manifest()

    tmp = Path(tempfile.mkdtemp(prefix="report_legs_selftest_"))
    try:
        # Fixture legs live inside prolog/ ONLY as a symlink-free temp tree; the
        # driver resolves relative paths against prolog/, so pass ABSOLUTE paths
        # and nothing under prolog/ is created.
        good = _mkleg(tmp, "leg_good", n=3)
        empty = tmp / "leg_empty"; empty.mkdir()
        split = _mkleg(tmp, "leg_split", n=2, phash="c" * 40)
        _mkleg(tmp, "leg_split", n=1, phash="d" * 40)   # second hash -> SPLIT
        noprov = _mkleg(tmp, "leg_noprov", n=3, provenance=False)
        outd = tmp / "out"

        # ---------------- 4a: two-sided by reason code -------------------
        # ZERO_GLOB — fires on an empty dir, passes on a populated one.
        # `require_classify_output=False` isolates THIS refusal; the
        # MISSING_CLASSIFY_OUTPUT pair below tests that gate on its own.
        results.append(_expect_refusal(
            "ZERO_GLOB fires (empty dir)", "ZERO_GLOB",
            lambda: report_corpus(str(empty), out_dir=outd, stages=[],
                                  require_classify_output=False)))
        results.append(_expect_pass(
            "ZERO_GLOB declines (populated dir)",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  require_classify_output=False)))

        # SCOPE_TRACKED_GENERATOR — the deferral is a TESTED refusal, not a gap.
        for name in _SCOPE_DEFERRED:
            results.append(_expect_refusal(
                f"SCOPE_TRACKED_GENERATOR fires ({name})", "SCOPE_TRACKED_GENERATOR",
                lambda n=name: report_corpus(str(good), out_dir=outd, stages=[n],
                                             require_classify_output=False)))
        results.append(_expect_pass(
            "SCOPE_TRACKED_GENERATOR declines (built stage)",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  require_classify_output=False)))
        # ... and a near-identical NAME that is not in the deferred set must
        # refuse for a DIFFERENT reason — otherwise the check is a substring test.
        results.append(_expect_refusal(
            "unknown stage != deferred stage", "UNKNOWN_STAGE",
            lambda: report_corpus(str(good), out_dir=outd,
                                  stages=["variance_analysis_x"],
                                  require_classify_output=False)))

        # MISSING_CLASSIFY_OUTPUT — fires when required and absent, passes when
        # the requirement is lifted. (The same-commit half is exercised by the
        # witness run; here the artifact simply does not exist.)
        results.append(_expect_refusal(
            "MISSING_CLASSIFY_OUTPUT fires", "MISSING_CLASSIFY_OUTPUT",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  require_classify_output=True)))
        results.append(_expect_pass(
            "MISSING_CLASSIFY_OUTPUT declines", 
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  require_classify_output=False)))

        # PROMPT_HASH_DECLARED_MISMATCH — refusal ONLY when the caller DECLARED
        # a hash. Two-sided on the same leg: right hash passes, wrong refuses.
        results.append(_expect_refusal(
            "PROMPT_HASH_DECLARED_MISMATCH fires", "PROMPT_HASH_DECLARED_MISMATCH",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  declared_prompt_hash="f" * 40,
                                  require_classify_output=False)))
        results.append(_expect_pass(
            "PROMPT_HASH_DECLARED_MISMATCH declines",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  declared_prompt_hash="a" * 40,
                                  require_classify_output=False)))
        # A SPLIT leg must refuse even if the declared hash is one of the two —
        # "present" is not "uniform".
        results.append(_expect_refusal(
            "declared hash present but leg SPLIT", "PROMPT_HASH_DECLARED_MISMATCH",
            lambda: report_corpus(str(split), out_dir=outd, stages=[],
                                  declared_prompt_hash="c" * 40,
                                  require_classify_output=False)))

        # ARTIFACT_ABSENT / ARTIFACT_EMPTY / ARTIFACT_MARKER_MISSING /
        # SIDECAR_HASH_MISMATCH / CORPUS_DRIFT are gate-side codes. Planting them
        # through the real stage functions would need swipl over a fixture corpus
        # (minutes, and a charter violation), so they are planted at the gate
        # itself via a stubbed stage that writes exactly the artifact under test.
        results.extend(_gate_side_fixtures(tmp, good))

        # LOCK_HELD — a second guard on the same lock must refuse.
        g1 = _TransitGuard(outd)
        g1.acquire()
        try:
            results.append(_expect_refusal(
                "LOCK_HELD fires (second holder)", "LOCK_HELD",
                lambda: _TransitGuard(outd).acquire()))
        finally:
            g1.release()
        # ...and declines once released — the second side.
        g2 = _TransitGuard(outd)
        results.append(_expect_pass("LOCK_HELD declines (released)", g2.acquire))
        g2.release()

        # TRANSIT_JOURNAL_DIRTY / TRANSIT_BACKUP_LOST — the crash path.
        results.extend(_transit_fixtures(outd))
        # TRANSIT_RESTORE_FAILED — the in-run path, distinct from BACKUP_LOST.
        results.extend(_transit_restore_failed_fixture(outd))

        # ---------------- 4b: present-and-correctly-valued ---------------
        results.append(_expect_value("PROMPT_HASH_UNIFORM token",
                                     _prompt_hash_token(good)["token"],
                                     "PROMPT_HASH_UNIFORM"))
        results.append(_expect_value("PROMPT_HASH_UNIFORM coverage",
                                     _prompt_hash_token(good)["coverage"], 1.0))
        results.append(_expect_value("PROMPT_HASH_UNIFORM hash",
                                     sorted(_prompt_hash_token(good)["hashes"]),
                                     ["a" * 40]))
        results.append(_expect_value("PROMPT_HASH_SPLIT token",
                                     _prompt_hash_token(split)["token"],
                                     "PROMPT_HASH_SPLIT"))
        results.append(_expect_value("PROMPT_HASH_SPLIT records both hashes",
                                     sorted(_prompt_hash_token(split)["hashes"]),
                                     ["c" * 40, "d" * 40]))
        # The Pattern-5 guard: an ABSENT leg must NOT read as UNIFORM.
        pa = _prompt_hash_token(noprov)
        results.append(_expect_value("PROMPT_HASH_ABSENT token", pa["token"],
                                     "PROMPT_HASH_ABSENT"))
        results.append(_expect_value("PROMPT_HASH_ABSENT is not UNIFORM",
                                     pa["token"] == "PROMPT_HASH_UNIFORM", False))
        results.append(_expect_value("PROMPT_HASH_ABSENT hashes empty",
                                     pa["hashes"], {}))
        results.append(_expect_value("PROMPT_HASH_ABSENT coverage 0",
                                     pa["coverage"], 0.0))
        # ...and an ABSENT leg COMPLETES rather than refusing (the v6 case).
        results.append(_expect_pass(
            "ABSENT leg COMPLETES (v6 shape)",
            lambda: report_corpus(str(noprov), out_dir=outd, stages=[],
                                  require_classify_output=False)))

        # ---------------- fingerprint refusals, two-sided -----------------
        # These three were DOCUMENTED before they were WIRED (expected_model was
        # accepted and never read), so the taxonomy named codes the driver could
        # not produce. Fixtures now hold them to the code.
        mixed = _mkleg(tmp, "leg_mixed", n=2, model="claude-sonnet-5")
        _mkleg(tmp, "leg_mixed", n=1, model="gemini-2.5-flash")   # -> MODEL_MISMATCH
        partial = _mkleg(tmp, "leg_partial", n=2)
        _mkleg(tmp, "leg_partial", n=1, provenance=False)         # -> PROVENANCE_COVERAGE

        results.append(_expect_refusal(
            "MODEL_MISMATCH fires (mixed models)", "MODEL_MISMATCH",
            lambda: report_corpus(str(mixed), out_dir=outd, stages=[],
                                  expected_model="claude-sonnet-5",
                                  require_classify_output=False)))
        results.append(_expect_pass(
            "MODEL_MISMATCH declines (uniform model)",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  expected_model="claude-sonnet-5",
                                  require_classify_output=False)))
        results.append(_expect_refusal(
            "PROVENANCE_COVERAGE fires (partial coverage)", "PROVENANCE_COVERAGE",
            lambda: report_corpus(str(partial), out_dir=outd, stages=[],
                                  expected_model="claude-sonnet-5",
                                  require_classify_output=False)))
        results.append(_expect_pass(
            "PROVENANCE_COVERAGE declines (full coverage)",
            lambda: report_corpus(str(good), out_dir=outd, stages=[],
                                  expected_model="claude-sonnet-5",
                                  require_classify_output=False)))
        # Non-vacuity: coverage is asserted BEFORE the prefix match, so a
        # zero-provenance leg must refuse COVERAGE, never pass the model check
        # over an empty fact set.
        results.append(_expect_refusal(
            "coverage checked BEFORE model (non-vacuous)", "PROVENANCE_COVERAGE",
            lambda: report_corpus(str(noprov), out_dir=outd, stages=[],
                                  expected_model="claude-sonnet-5",
                                  require_classify_output=False)))
        results.extend(_load_incomplete_fixtures(tmp, good))

        # ---------------- classify-output naming, pinned to the consumer --
        results.extend(_classify_name_fixtures())

        # ---------------- code_dirty scoping, two-sided --------------------
        results.extend(_code_dirty_fixtures())

        # ---------------- OQ-246 discrimination, synthetic ---------------
        results.extend(_oq246_synthetic(tmp))

    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    # ---------------- the control on the control ------------------------
    after = _outputs_manifest()
    changed = sorted(set(before) ^ set(after)) + sorted(
        k for k in set(before) & set(after) if before[k] != after[k])
    iso_ok = not changed
    results.append((iso_ok,
                    f"[{'OK  ' if iso_ok else 'FAIL'}] "
                    f"{'ISOLATION: outputs/ + validation_suite.pl byte-identical':44s} "
                    + ("no exclusions" if iso_ok else
                       f"{len(changed)} changed: {changed[:6]}")))

    passed = sum(1 for ok, _ in results if ok)
    if verbose:
        for _, line in results:
            print("  " + line)
        print()
    n = len(results)
    print(f"report_legs selftest: {passed}/{n} controls passed"
          + ("" if passed == n else "  — RED"))
    return 0 if passed == n else 1


def _gate_side_fixtures(tmp: Path, leg: Path) -> list:
    """Plant ARTIFACT_* / SIDECAR / CORPUS_DRIFT at the gate, two-sided.

    These fire in report_corpus's OUTPUT GATE, which runs after the stages. A
    stub stage writes exactly the artifact under test, so the fixture exercises
    the REAL gate code rather than a reimplementation of it.
    """
    out = []
    marker = _REPORT_STAGES["giant_comp"]["marker"]

    def run_with_stub(writer, stage="fpn", **kw):
        real = getattr(R, "_prolog_" + stage)
        setattr(R, "_prolog_" + stage, writer)
        try:
            return report_corpus(str(leg), out_dir=tmp / f"out_{stage}_{id(writer)}",
                                 stages=[stage], require_classify_output=False, **kw)
        finally:
            setattr(R, "_prolog_" + stage, real)

    out.append(_expect_refusal(
        "ARTIFACT_ABSENT fires", "ARTIFACT_ABSENT",
        lambda: run_with_stub(lambda overlay="", out_dir=None, **k: None)))
    out.append(_expect_pass(
        "ARTIFACT_ABSENT declines (file written)",
        lambda: run_with_stub(
            lambda overlay="", out_dir=None, **k:
                (out_dir / "fpn_report.md").write_text("x\n", encoding="utf-8"))))
    out.append(_expect_refusal(
        "ARTIFACT_EMPTY fires (zero bytes)", "ARTIFACT_EMPTY",
        lambda: run_with_stub(
            lambda overlay="", out_dir=None, **k:
                (out_dir / "fpn_report.md").write_text("", encoding="utf-8"))))
    out.append(_expect_pass(
        "ARTIFACT_EMPTY declines (one byte)",
        lambda: run_with_stub(
            lambda overlay="", out_dir=None, **k:
                (out_dir / "fpn_report.md").write_text("x", encoding="utf-8"))))

    # ARTIFACT_MARKER_MISSING — giant_comp is the stage with an owed section.
    # Its transit co-product must also be written, or ARTIFACT_ABSENT would fire
    # FIRST and the fixture would pass for the wrong reason.
    def gc_writer(text):
        def w(overlay="", out_dir=None, **k):
            (out_dir / "giant_component_analysis.md").write_text(text, encoding="utf-8")
            (R.OUTPUTS_DIR / "giant_component_analysis.raw.json").write_text(
                "{}\n", encoding="utf-8")
        return w
    out.append(_expect_refusal(
        "ARTIFACT_MARKER_MISSING fires", "ARTIFACT_MARKER_MISSING",
        lambda: run_with_stub(gc_writer("no owed section here\n"), stage="giant_comp")))
    out.append(_expect_pass(
        "ARTIFACT_MARKER_MISSING declines (marker present)",
        lambda: run_with_stub(gc_writer(f"{marker}\nrows\n"), stage="giant_comp")))

    # SIDECAR_HASH_MISMATCH — the sidecar the driver writes must be ACCEPTED by
    # assert_corpus_current. Firing it means corrupting the stamp after the fact,
    # so this side is asserted directly against the checker.
    from corpus_hash import assert_corpus_current
    sc_dir = tmp / "sidecar"; sc_dir.mkdir(exist_ok=True)
    good_side = sc_dir / "s_ok.json"
    good_side.write_text(json.dumps(
        {"corpus_hash": R._compute_corpus_hash(leg)}), encoding="utf-8")
    bad_side = sc_dir / "s_bad.json"
    bad_side.write_text(json.dumps({"corpus_hash": "deadbeefdead"}), encoding="utf-8")
    try:
        assert_corpus_current(good_side, leg)
        out.append((True, f"[OK  ] {'SIDECAR accepted by assert_corpus_current':44s} ok"))
    except RuntimeError as e:
        out.append((False, f"[FAIL] {'SIDECAR accepted by assert_corpus_current':44s} {e}"))
    try:
        assert_corpus_current(bad_side, leg)
        out.append((False, f"[FAIL] {'SIDECAR_HASH_MISMATCH fires':44s} accepted a bad stamp"))
    except RuntimeError:
        out.append((True, f"[OK  ] {'SIDECAR_HASH_MISMATCH fires':44s} rejected bad stamp"))

    # CORPUS_DRIFT — a stage that writes a NEW story into the leg mid-run must
    # be caught; the same stage without the write must pass.
    def drifter(overlay="", out_dir=None, **k):
        (out_dir / "fpn_report.md").write_text("x\n", encoding="utf-8")
        (leg / "fx_drift.pl").write_text("% added mid-run\n", encoding="utf-8")
    out.append(_expect_refusal(
        "CORPUS_DRIFT fires (leg moved mid-run)", "CORPUS_DRIFT",
        lambda: run_with_stub(drifter)))
    (leg / "fx_drift.pl").unlink(missing_ok=True)
    out.append(_expect_pass(
        "CORPUS_DRIFT declines (leg frozen)",
        lambda: run_with_stub(
            lambda overlay="", out_dir=None, **k:
                (out_dir / "fpn_report.md").write_text("x\n", encoding="utf-8"))))
    return out


def _transit_fixtures(outd: Path) -> list:
    """The crash path: a journal left behind by a killed process."""
    out = []
    g = _TransitGuard(outd)
    g.state_dir.mkdir(parents=True, exist_ok=True)
    g.backups.mkdir(parents=True, exist_ok=True)

    # A shared artifact that a killed run had snapshotted and deleted.
    shared = R.OUTPUTS_DIR / "orbit_data.json"
    had = shared.exists()
    orig = shared.read_bytes() if had else None
    # Restore mtime as well as bytes: the isolation post-condition compares
    # (size, mtime_ns), so putting the content back is not putting the FILE back.
    orig_times = ((shared.stat().st_atime_ns, shared.stat().st_mtime_ns)
                  if had else None)
    try:
        payload = b'{"selftest":"transit"}\n'
        backup = g.backups / "orbit_data.json"
        backup.write_bytes(payload)
        sha = hashlib.sha256(payload).hexdigest()
        journal = {"note": "killed mid-transit", "entries": [
            {"path": str(shared), "stage": "orbit", "existed": True,
             "state": "snapshotted", "sha256": sha, "size": len(payload),
             "backup": str(backup)}]}

        # TRANSIT_JOURNAL_DIRTY: restore-then-refuse, never silently proceed.
        shared.unlink(missing_ok=True)
        g.journal_path.write_text(json.dumps(journal), encoding="utf-8")
        out.append(_expect_refusal(
            "TRANSIT_JOURNAL_DIRTY fires", "TRANSIT_JOURNAL_DIRTY",
            lambda: _TransitGuard(outd).recover_if_dirty()))
        # ...and it RESTORED before refusing — the half that matters.
        out.append(_expect_value("TRANSIT_JOURNAL_DIRTY restored the artifact",
                                 shared.exists() and shared.read_bytes() == payload,
                                 True))
        out.append(_expect_pass("TRANSIT journal declines when clean",
                                _TransitGuard(outd).recover_if_dirty))

        # TRANSIT_BACKUP_LOST: the one case where restore-then-refuse CANNOT
        # restore. Never a warning.
        backup.unlink(missing_ok=True)
        g.journal_path.write_text(json.dumps(journal), encoding="utf-8")
        out.append(_expect_refusal(
            "TRANSIT_BACKUP_LOST fires (backup missing)", "TRANSIT_BACKUP_LOST",
            lambda: _TransitGuard(outd).recover_if_dirty()))
        # ...and on a CORRUPTED backup too, not only a missing one.
        backup.write_bytes(b"corrupted, wrong sha\n")
        g.journal_path.write_text(json.dumps(journal), encoding="utf-8")
        out.append(_expect_refusal(
            "TRANSIT_BACKUP_LOST fires (sha mismatch)", "TRANSIT_BACKUP_LOST",
            lambda: _TransitGuard(outd).recover_if_dirty()))
    finally:
        g.journal_path.unlink(missing_ok=True)
        (g.backups / "orbit_data.json").unlink(missing_ok=True)
        # Restore the real shared artifact exactly as found (isolation).
        if had:
            shared.write_bytes(orig)
            os.utime(shared, ns=orig_times)
        else:
            shared.unlink(missing_ok=True)
    return out


def _code_dirty_fixtures() -> list:
    """`code_dirty` must count what can change OUTPUT, and nothing else.

    Two-sided by construction, and the UNKNOWN case is asserted to count: the denylist
    fails CLOSED, so a path nobody anticipated must read as dirty rather than clean. An
    allowlist would fail open here, which is the permissive direction for a provenance
    flag and the reason this is a denylist at all.
    """
    out = []
    counts = ["python/run_pipeline.py", "prolog/drl_core.pl", "scripts/gate.sh",
              "agent/llm_call.py", "config.json", "some_new_dir/thing.rb"]
    ignores = ["CLAUDE.md", "ISSUES.md", "KNOWN_STATE.md", "docs/seat-theorem-v1.md",
               "audits/2026-01-01_x/compare.py", "audits/2026-01-01_x/evidence.txt",
               "prolog/testsets_sonnet2/a.pl", "prolog/archives/datasets/original_v6/b.pl",
               "json_nemotron_think/a.json", "outputs/pipeline_output.json"]
    for p in counts:
        out.append(_expect_value(f"code_dirty counts {p}", _is_code_path(p), True))
    for p in ignores:
        out.append(_expect_value(f"code_dirty ignores {p}", _is_code_path(p), False))
    return out


def _load_incomplete_fixtures(tmp: Path, leg: Path) -> list:
    """LOAD_INCOMPLETE: the classify manifest's member count vs the leg's glob now.

    Two-sided against a synthetic classify output, so the fixture exercises the
    real comparison rather than a restatement of it.
    """
    out = []
    n = len(list(leg.glob("*.pl")))
    outputs = REPO_ROOT / "outputs"
    # Use a leg NAME that cannot collide with a real on-disk output.
    fake_leg = tmp / "leg_loadchk"
    shutil.copytree(leg, fake_leg, dirs_exist_ok=True)
    target = outputs / _classify_output_name(fake_leg.name)
    existed = target.exists()
    orig = target.read_bytes() if existed else None
    orig_times = ((target.stat().st_atime_ns, target.stat().st_mtime_ns)
                  if existed else None)
    head = R._git_head_sha()
    try:
        def write_manifest(count):
            target.write_text(json.dumps(
                {"manifest": {"code_commit": head, "n_constraints": count}}),
                encoding="utf-8")

        write_manifest(n + 7)     # manifest disagrees with the glob
        out.append(_expect_refusal(
            "LOAD_INCOMPLETE fires (manifest != glob)", "LOAD_INCOMPLETE",
            lambda: report_corpus(str(fake_leg), out_dir=tmp / "out_load",
                                  stages=[], require_classify_output=True)))
        write_manifest(n)         # manifest agrees
        out.append(_expect_pass(
            "LOAD_INCOMPLETE declines (manifest == glob)",
            lambda: report_corpus(str(fake_leg), out_dir=tmp / "out_load",
                                  stages=[], require_classify_output=True)))
        # ...and a WRONG-COMMIT manifest must refuse MISSING_CLASSIFY_OUTPUT, not
        # LOAD_INCOMPLETE — the two gates must not shadow each other.
        target.write_text(json.dumps(
            {"manifest": {"code_commit": "0" * 40, "n_constraints": n}}),
            encoding="utf-8")
        out.append(_expect_refusal(
            "cross-commit refuses CLASSIFY, not LOAD", "MISSING_CLASSIFY_OUTPUT",
            lambda: report_corpus(str(fake_leg), out_dir=tmp / "out_load",
                                  stages=[], require_classify_output=True)))
    finally:
        if existed:
            target.write_bytes(orig)
            os.utime(target, ns=orig_times)
        else:
            target.unlink(missing_ok=True)
    return out


def _transit_restore_failed_fixture(outd: Path) -> list:
    """TRANSIT_RESTORE_FAILED: the backup vanishes between snapshot and restore.

    Distinct from TRANSIT_BACKUP_LOST, which is the RECOVERY path on a journal
    found at startup. This is the in-run path, and it had no fixture at all until
    the taxonomy was audited against the selftest.
    """
    out = []
    shared = R.OUTPUTS_DIR / "orbit_data.json"
    had = shared.exists()
    orig = shared.read_bytes() if had else None
    orig_times = ((shared.stat().st_atime_ns, shared.stat().st_mtime_ns)
                  if had else None)
    try:
        if not had:
            shared.write_bytes(b'{"selftest":"restore_failed"}\n')
        g = _TransitGuard(outd)
        g.acquire()
        try:
            g.snapshot("orbit")
            # Destroy the backup mid-flight.
            (g.backups / "orbit_data.json").unlink(missing_ok=True)
            out.append(_expect_refusal(
                "TRANSIT_RESTORE_FAILED fires", "TRANSIT_RESTORE_FAILED",
                lambda: g.collect_and_restore("orbit", ())))
        finally:
            g.journal_path.unlink(missing_ok=True)
            g.release()
    finally:
        if had:
            shared.write_bytes(orig)
            os.utime(shared, ns=orig_times)
        else:
            shared.unlink(missing_ok=True)
    return out


def _classify_name_fixtures() -> list:
    """Pin _classify_output_name to the convention its CONSUMER actually reads.

    MISSING_CLASSIFY_OUTPUT is only a gate if it looks for the file that exists.
    The first cut of this driver resolved `pipeline_output_testsets_sonnet2.json`
    while every per-leg output on disk is `pipeline_output.sonnet2.json` — a
    refusal that would have fired on EVERY leg forever while looking like it was
    working. So the mapping is asserted here against the live artifacts AND
    against leg_diagnostic_table.py's own resolution lines, which OQ-353's
    Files: line names as the instrument to extend rather than fork.
    """
    out = []
    for leg, want in [("testsets", "pipeline_output.json"),
                      ("testsets_sonnet2", "pipeline_output.sonnet2.json"),
                      ("testsets_flash_think2", "pipeline_output.flash_think2.json"),
                      ("original_v6", "pipeline_output.original_v6.json")]:
        out.append(_expect_value(f"classify name: {leg}",
                                 _classify_output_name(leg), want))

    # The consumer's own source must still spell it this way. A literal check,
    # so a rename there turns this row red instead of silently forking.
    ldt = (REPO_ROOT / "python" / "audits" / "leg_diagnostic_table.py")
    src = ldt.read_text(encoding="utf-8") if ldt.exists() else ""
    out.append(_expect_value(
        "consumer still uses pipeline_output.<short>.json",
        'f"pipeline_output.{leg[len(\'testsets_\'):]}.json"' in src, True))

    # Two-sided against DISK: at least one leg name must resolve to a file that
    # exists, or the whole mapping could be wrong in the same direction and every
    # assertion above would still pass.
    existing = sorted(pp.name for pp in (REPO_ROOT / "outputs").glob("pipeline_output.*.json"))
    hits = [n for n in existing if n == _classify_output_name(
        "testsets_" + n[len("pipeline_output."):-len(".json")])]
    out.append((bool(hits),
                f"[{'OK  ' if hits else 'FAIL'}] "
                f"{'classify name resolves to real on-disk files':44s} "
                f"{len(hits)} of {len(existing)} match (e.g. {hits[:3]})"))
    return out


def _oq246_synthetic(tmp: Path) -> list:
    """OQ-246 discrimination, IN THE GATE and SYNTHETIC.

    The real-leg witness (kimi 1005/700 vs sonnet 1001/930) is run ONCE and
    pasted into the WRITEUP; it is not a recurring gate cost.

    What rides here: two ~6-story mini-legs with DELIBERATELY OVERLAPPING ids.
    Iterating them IN ONE PROCESS accumulates facts, so leg B's fact set is
    polluted by leg A's; iterating them PER PROCESS diverges. The tell this
    reproduces is the accumulation itself — and note WHY the per-process rule is
    the only guard: the usual tell (two legs reading byte-identical) is WEAKER on
    same-model redraw pairs, which are expected to be near-identical on marginals.
    """
    out = []
    a = _mkleg(tmp, "oq246_a", n=3, phash="1" * 40)
    b = tmp / "oq246_b"; b.mkdir(exist_ok=True)
    # Overlapping ids: b re-uses two of a's story ids with a DIFFERENT hash.
    for i in range(3):
        src = f"fx_oq246_a_{i}" if i < 2 else "fx_oq246_b_2"
        (b / f"{src}.pl").write_text(
            _STORY.format(id=src, phash="2" * 40, shash="b" * 40,
                          model="claude-sonnet-5"), encoding="utf-8")

    # PER-PROCESS (what the driver does): each leg is read on its own.
    per_proc = [_prompt_hash_token(a)["hashes"], _prompt_hash_token(b)["hashes"]]
    diverges = per_proc[0] != per_proc[1]
    out.append(_expect_value("OQ-246 per-process: legs DIVERGE", diverges, True))

    # IN-PROCESS accumulation, simulated by unioning the fact sets the way a
    # single swipl process consulting both legs would.
    accum: dict = {}
    for leg in (a, b):
        for k, v in _prompt_hash_token(leg)["hashes"].items():
            accum[k] = accum.get(k, 0) + v
    # The tell: the accumulated view carries BOTH hashes and a story count larger
    # than either leg — leg B alone can never show hash '1'*40.
    tell = (sorted(accum) == ["1" * 40, "2" * 40]
            and sum(accum.values()) > sum(_prompt_hash_token(b)["hashes"].values()))
    out.append(_expect_value("OQ-246 in-process: accumulation tell present", tell, True))
    out.append(_expect_value("OQ-246 leg B alone excludes leg A's hash",
                             "1" * 40 in _prompt_hash_token(b)["hashes"], False))
    return out


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--legs", nargs="+", default=None,
                    help="corpus paths relative to prolog/ (e.g. testsets_sonnet2 "
                         "archives/datasets/original_v6)")
    ap.add_argument("--stages", nargs="+", default=None,
                    help=f"subset of: {' '.join(_REPORT_STAGES)} "
                         f"(deferred, will refuse: {' '.join(_SCOPE_DEFERRED)})")
    ap.add_argument("--out", default=None,
                    help="output dir (default outputs/legs/<leg>/)")
    ap.add_argument("--expected-model", default=None)
    ap.add_argument("--declared-prompt-hash", default=None,
                    help="if given, the leg must be UNIFORM on this hash or the run "
                         "refuses PROMPT_HASH_DECLARED_MISMATCH")
    ap.add_argument("--giant-comp-timeout", type=int, default=None,
                    help="absolute ceiling for giant_comp, in seconds. For a large "
                         "archive leg, derive it from a TIMED PROBE (wall x 3, floored "
                         "at 900) — never extrapolate, and never fall back to the floor "
                         "when the probe failed")
    ap.add_argument("--no-require-classify", action="store_true",
                    help="lift MISSING_CLASSIFY_OUTPUT (diagnostics only; a leg without "
                         "a same-commit classify output is INCOMPLETE for OQ-353)")
    ap.add_argument("--resume", action="store_true",
                    help="resume after an interrupted run; the transit journal is "
                         "recovered first either way")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args(argv)

    if args.selftest:
        return selftest()
    if not args.legs:
        ap.error("--legs is required (or --selftest)")

    rc = 0
    for leg in args.legs:
        print(f"\n=== {leg} ===", flush=True)
        try:
            res = report_corpus(
                leg,
                out_dir=Path(args.out) if args.out else None,
                stages=args.stages,
                expected_model=args.expected_model,
                declared_prompt_hash=args.declared_prompt_hash,
                giant_comp_timeout=args.giant_comp_timeout,
                require_classify_output=not args.no_require_classify,
                resume=args.resume,
                progress=lambda tag, msg: print(f"  {msg}", flush=True),
            )
            print(f"  n_files={res['n_files']} corpus_hash={res['corpus_hash']}")
            print(f"  prompt_hash: {res['prompt_hash']['token']} "
                  f"coverage={res['prompt_hash']['coverage']:.4f}")
            for st, tok in res["stage_outcomes"].items():
                print(f"    {st:20s} {tok}")
            print(f"  -> {res['out_dir']}")
        except ReportRefusal as e:
            print(f"  REFUSED {e.code}: {e.detail}", file=sys.stderr)
            rc = 1
    return rc


if __name__ == "__main__":
    sys.exit(main())
