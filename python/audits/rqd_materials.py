#!/usr/bin/env python3
"""rqd_materials.py — the 10 design specs and their ground truth for RQ-d.

RQ-d / Prediction 1: recognition vs enumeration. Each spec carries BOTH an
OMISSION (a required element the design never states) and an ERROR (a stated
element that is wrong). The dissociation is therefore WITHIN artifact: same
text, same prompt, two defect kinds. That is what controls the effort/verbosity
confound — an "enumeration just produces more words" account predicts a lift on
both kinds; the hypothesis predicts a lift on OMISSION only.

ALTITUDE OF THE MATERIALS — stated here so it cannot be lost downstream. These
specs are AUTHORED, which is the floor of the discrimination ladder ("shows only
that authored decoys get rejected"). They are lifted off the floor, but not to
the top, by drawing every omission SHAPE from a witnessed case in this
repository's own record (the `source` field on each item): judged-not-shown,
production-without-consumption, input-gates-only, a self-check that cannot
consume its own format. The claim this can license is therefore about defect
SHAPES that have really occurred here, instantiated in fresh text — not about
naturally-arising defects in the wild. Report at that altitude.

Ground truth per item:
  omission_keys / error_keys — concept phrases, any one of which identifies the
  defect. Scoring additionally requires a GAP MARKER in the same sentence (see
  rqd_scorer.py); a response that merely uses the word does not count, and a
  response that silently SUPPLIES a missing element without marking it does not
  count either — surfacing it to the sender is the whole value being measured.
"""

SPECS = [
    {
        "id": "judged_not_shown",
        "source": "OQ-277: a preregistration that pinned what is judged and never what is shown",
        "text": """
DESIGN — Cross-coder agreement study

Two independent coders classify each of 40 incident records into exactly one of
five mechanism classes. Agreement is measured as Cohen's kappa over the 40
paired assignments. A kappa at or above 0.60 is recorded as substantial
agreement; below 0.60 the taxonomy is reported as not transferring.

Coders are drawn from fresh instances with no prior exposure to the taxonomy
document. Each coder returns a single class label per record and a one-sentence
justification. Justifications are retained for the writeup but do not enter the
kappa computation.

Records are drawn from the incident archive in directory order, first 40 with a
non-empty summary field. Ties in the class assignment are impossible by
construction since exactly one label is required.

The kappa is computed by the analysis stage and written to matrices/kappa.json
alongside the per-record disagreement list. A kappa above 0.60 closes the
question; a kappa below 0.60 opens a follow-up on which classes collide.
""",
        "omission_keys": ["what the coder", "what is shown", "shown to", "presented to",
                          "coder sees", "coders see", "input to the coder", "record content",
                          "what material", "which fields", "packet", "prompt the coder"],
        "error_keys": ["directory order", "first 40", "not random", "non-random",
                       "selection bias", "sampling", "arbitrary order", "biased sample"],
    },
    {
        "id": "production_not_consumption",
        "source": "RQ2 attempt: 219 calls made, nothing scoreable — the scorer existed in neither code nor design",
        "text": """
DESIGN — Redraw stability measurement

For each of 12 source topics, the generator is invoked three times on identical
input to produce three independent draws. Each draw yields a manifest listing
the readings it produced, with a subject and a stance per reading.

All 36 manifests are written to responses/<topic>_<draw>.json immediately on
receipt, before any parsing, so that a run which dies partway leaves recoverable
data rather than nothing.

A feature counts as replicated when it appears in all three draws of a topic.
Features appearing in one or two draws are recorded as observations, not
replications. The replication rate is reported per topic and pooled across the
12 topics.

The driver gates on: three responses captured per topic before parsing begins,
each response non-empty, and each parsing to the expected manifest schema.
Out-of-vocabulary stance values are reported rather than coerced.

Results land in outputs/redraw_stability.json with a run manifest stamping the
model, the prompt commit, and the wall-clock start time.
""",
        "omission_keys": ["how features are matched", "matching", "same feature",
                          "identity across draws", "compare", "comparison rule",
                          "name-blind", "equivalence", "what counts as the same",
                          "how to determine", "scorer", "scoring"],
        "error_keys": ["pooled", "pooling", "denominator", "unit-built",
                       "inherits the churn", "per topic and pooled", "aggregate"],
    },
    {
        "id": "input_gates_only",
        "source": "OQ-277: three sound gates, all on what the pipeline CONSUMES; no code path wrote responses",
        "text": """
DESIGN — Corpus refresh driver

The driver reads the seed list, validates that every seed has a non-empty topic
and a declared model, and refuses to start if any seed is malformed. It then
counts the seeds and compares the count against the manifest's expected total,
aborting on mismatch.

For each seed the driver issues one generation call. Before parsing any
response it verifies that the number of captured payloads equals the number of
seeds issued, and that fixtures are counted separately from live seeds so a
fixture cannot inflate the total.

Parsed stories are appended to the corpus directory. A summary line reports the
number of stories generated and the number of seeds consumed.

The run is considered successful when all three gates pass and the summary line
prints the expected totals. Failures at any gate abort the run with a non-zero
exit and a named reason.
""",
        "omission_keys": ["output gate", "verify the write", "after writing",
                          "wrote anything", "files exist", "on disk", "persisted",
                          "persistence", "write-then-verify", "count from the artifact",
                          "check the corpus", "confirm the file"],
        "error_keys": ["count of seeds", "seeds consumed", "not the same as stories",
                       "len(", "from the loop", "counts the loop", "sourced from",
                       "claim about persistence"],
    },
    {
        "id": "selfcheck_format",
        "source": "OQ-277: a stated self-check command that could not consume its own specified format",
        "text": """
DESIGN — Enumeration sentinel guard

Every guarded enumeration in the specification carries a sentinel block written
as an HTML comment immediately above it, of the form:

    <!-- ENUM:name=drift_terminals count=4 -->

The checker extracts each sentinel with the command

    grep -o 'ENUM:[a-z_]*' spec.md | cut -d: -f2

and compares the extracted names against the checker manifest. A name present
in the manifest but absent from the specification turns the gate red, and a
sentinel whose name is absent from the manifest also turns it red.

The manifest lives at python/enum_manifest.json and lists one entry per guarded
enumeration, each with a name and the source symbol the count is pinned to.

New enumerations are unguarded until both the sentinel block and the manifest
entry land in the same change.
""",
        "omission_keys": ["count is never", "count not checked", "count field",
                          "ignores the count", "only the name", "names only",
                          "never compares the count", "count unused", "drops the count"],
        "error_keys": ["cut -d: -f2", "grep -o", "cannot", "will not extract",
                       "returns", "regex", "character class", "does not match",
                       "malformed", "wrong field"],
    },
    {
        "id": "failure_semantics",
        "source": "House pattern: a gate defined without what happens when it fires",
        "text": """
DESIGN — Reading totality gate

Before the parallel analysis block runs, a sequential fail-fast step executes
the registry-driven suite tests/test_reading_totality.pl. The suite asserts
that every registered reading predicate is exactly-one on its declared domain.

The registry lists each reading predicate with its declared domain. Registration
is opt-in: a reading that is not registered is not checked.

The suite is run with the extended load chain named in its own file header,
which differs from the default chain — running it under the default chain
produces spurious failures that read as regressions rather than as a load error.

The gate runs on every pipeline invocation and its result is recorded in the run
manifest as reading_totality_status.
""",
        "omission_keys": ["what happens when", "if it fails", "on failure", "when red",
                          "abort", "halt", "continue", "does the pipeline stop",
                          "failure semantics", "consequence of failure", "red result",
                          "blocking or advisory", "fail-closed"],
        "error_keys": ["opt-in", "unregistered", "escapes", "silently", "not checked",
                       "registration", "gap in coverage", "unguarded"],
    },
    {
        "id": "empty_case",
        "source": "Pattern 5: a quantifier that passes vacuously on an empty table",
        "text": """
DESIGN — Beneficiary compliance certification

A constraint certifies as natural-law when the beneficiary count is zero — that
is, when no party is recorded as gaining from the constraint's operation. The
count is taken over the intent_power_change facts authored for that constraint.

Certification is computed per constraint during the classification pass and
serialized as natural_law_certified alongside the constraint's type.

The certification is reported in the per-constraint report and feeds the
signature layer, where it can raise a correction-grade verdict.

Constraints whose beneficiary count exceeds zero are not certified and carry the
count in their diagnostic block for inspection.
""",
        "omission_keys": ["no facts", "absent", "never authored", "empty",
                          "zero because", "not authored", "no data", "unauthored",
                          "distinguish", "authored zero", "missing vs", "vacuous",
                          "establish the datum", "fail-closed"],
        "error_keys": ["feeds the signature", "correction-grade", "raise a verdict",
                       "override", "too strong", "consequence", "grade"],
    },
    {
        "id": "ordering",
        "source": "House pattern: two stages that must be ordered, order never stated",
        "text": """
DESIGN — Index regeneration and tracker edit

The derived router at issues/INDEX.md is generated from the tracker by the
resolver's index command, with a JSON twin at issues/INDEX.json. The gate runs
the resolver's index --check, which turns red when the index is stale relative
to the tracker.

A session that edits the tracker regenerates the index so the router reflects
the edit. A session that regenerates the index commits both the index and its
JSON twin.

The router is never authoritative; the tracker is. The router exists so an
arbitrary question can be routed to two or three tracker entries without a
whole-file read, which is a witnessed failure mode.

Both files are committed together with the tracker edit that occasioned them.
""",
        "omission_keys": ["order", "before or after", "sequence", "which first",
                          "regenerate before", "regenerate after", "when to run",
                          "timing", "at what point", "ordering"],
        "error_keys": ["never authoritative", "two or three", "whole-file",
                       "contradic", "but the gate", "authoritative"],
    },
    {
        "id": "retire_predecessor",
        "source": "OQ-277: a replacement landed, the old control stayed, four green lines wired to nothing",
        "text": """
DESIGN — Replacement coverage assertion

The new coverage assertion checks that every registered surface reports both a
scored count and a total count, so a descriptive statistic can never be read
without its coverage. It is called from the report builder immediately before
serialization.

The assertion carries four selftest lines exercising it against a fixture with
full coverage, partial coverage, zero coverage, and a surface missing its total.

The assertion supersedes the older coverage warning, which emitted a log line
when a surface reported a scored count below its total. The older warning's
selftests remain in the suite.

The change lands with the assertion, its selftests, and the report-builder call
site in one commit.
""",
        "omission_keys": ["retire", "remove the old", "orphan", "still called",
                          "no longer called", "dead", "unwired", "wired to nothing",
                          "delete", "predecessor", "old warning", "superseded but"],
        "error_keys": ["immediately before serialization", "too late", "after",
                       "call site", "wrong place", "already", "ordering of the call"],
    },
    {
        "id": "denominator",
        "source": "House pattern: a rate reported without naming its denominator",
        "text": """
DESIGN — Catch-rate readout

Every new audit writeup carries a catch bit recording whether the audit's
apparatus fired: live when a control fired or a verdict changed, latent when a
real defect was found conditional on an unproduced input, and no when the audit
was pure confirmation.

The instrument reads the bits across writeups and reports the rolling rate in
the gate output. The rate is reported, never gated — reading it is the
operator's seat.

The readout prints one line summarising the distribution across the three
values, and is refreshed on every gate invocation.

The bit is forward-only: writeups predating its adoption are exempt and are not
counted as missing.
""",
        "omission_keys": ["denominator", "out of how many", "how many writeups",
                          "total", "population", "base", "over what",
                          "how many carry", "coverage", "n =", "sample size",
                          "how many audits"],
        "error_keys": ["exempt", "forward-only", "not counted", "excluded",
                       "predating", "selection", "biased", "only new"],
    },
    {
        "id": "owner_unspecified",
        "source": "House pattern: an artifact required by a process with no stage that produces it",
        "text": """
DESIGN — Manifest-bearing corpus comparison

A comparison across corpus legs requires each leg's run to carry a manifest
sidecar stamping the leg name, the story count, the code commit, and the model
fingerprint. The comparison stage asserts that all sidecars share a run
identifier before joining, and refuses the join otherwise.

The comparison reports per-leg counts and the cross-leg divergence, and writes
the result to outputs/leg_comparison.json.

Legs whose fingerprint names a model different from the leg's directory name are
reported as a provenance mismatch rather than pooled, since a leg's model is not
its directory name.

The refusal on mismatched run identifiers is what prevents a stale leg from
being compared against a fresh one.
""",
        "omission_keys": ["who writes", "which stage", "produced by", "generates the sidecar",
                          "where does the sidecar", "no stage", "creates the manifest",
                          "emits the sidecar", "how is the sidecar", "written by",
                          "responsible for"],
        "error_keys": ["share a run identifier", "same run", "different runs",
                       "legitimate", "too strict", "never share", "separate runs",
                       "cannot be compared", "impossible"],
    },
]


def by_id(spec_id):
    for s in SPECS:
        if s["id"] == spec_id:
            return s
    raise KeyError(spec_id)


if __name__ == "__main__":
    print(f"{len(SPECS)} specs")
    for s in SPECS:
        print(f"  {s['id']:26} omission_keys={len(s['omission_keys']):2}  "
              f"error_keys={len(s['error_keys']):2}")
