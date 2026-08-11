# HANDOFF — escape-check extraction (n=8). Read ONLY this file and the sources it names.

**Written:** 2026-08-11, by the twins-and-driver instance, under operator condition **C1**
(`RULING_2026-08-11_freeze_scope.md` §1.1).
**You are the EXTRACTOR. You never code.** If you find yourself assigning a failure class to a
unit, you have broken the experiment.

## The blindness rule — this is why you exist as a separate instance

The operator's condition: *the escape units are extracted under the same conventions as the
primary units, by an extractor who has not seen the primary sample's results.*

**Do NOT open, glob, `git show`, `git log`, or grep the bodies of any of these:**

| do not open | why |
|---|---|
| `packets/our_units/*.json` | the 26 primary-sample units |
| `packets/wu_units.json`, `packets/coder_direction_i.json` | direction-(i) units |
| `controls/anchors.json`, `controls/decoys.json`, `controls/planted.json` | control units, several quoting incidents |
| `controls/redaction_twins_direction_i.json` | unredacted twin arms |
| `responses/`, `matrices/` | coding results (currently empty — keep it that way) |
| `RULING_2026-08-11_freeze_scope.md` §2.3 onward, and `controls/redaction_pair_selection_defect.md` | **these two QUOTE primary unit bodies** — the oq93 collision check states unit `05`'s symptom verbatim and unit `07`'s in summary. Added 2026-08-11 by their author, who wrote unit content into a rulings document and only noticed when briefing you. Read their §1 / headline verdict if you need the scope ruling; stop at the collision sections. |

The prior instance read one primary unit in full and disqualified itself in writing rather than
claim the condition on its wording. Do the same if you slip: **declare it, do not quietly
continue.** Reading `RECON.md`, `HANDOFF.md`, or `verdict_grammar_amendment.md` is fine — they
carry rulings, not units — but skim past any passage quoting a unit body.

`git log`/`git show` is a **live leak channel** in this arc (`verdict_grammar_amendment.md` §J): a
commit message once summarized two unit bodies. Do not read commit bodies for this audit.

## What the escape check is, and why it is not the same job as the primary sample

`docs/amnesiac_institution/amnesiac_institution_v0.3.md` §4.5 reports that **73 of 174** audit
directories describe a silent-failure incident, via a **keyword proxy**. The escape check audits
that proxy's **recall**: it samples directories the proxy classified as *non*-incident-bearing and
asks whether any of them in fact reports one.

So your question per directory is **not** "extract the incident" — it is **"is there an incident
here at all?"** Most should be NO-UNIT. **A directory yielding no unit is the expected result and a
successful outcome, not a failed extraction.** Do not manufacture a unit to have something to hand
in; a fabricated unit here would relabel the paper's central statistic.

**Asymmetric cost, which is why the gate exists** (`verdict_grammar_amendment.md` §H): a confirmed
hit means the 73 numerator is an **undercount** and 42% is a lower bound. At n=8 with threshold ≥1,
a single false positive would move a published figure. Therefore:

- **You PROPOSE. The operator RULES.** Every candidate hit is QUARANTINED until the operator reads
  the source directory and confirms. This is not a formality you can pre-empt.
- **The quarantine mechanism has never been exercised** (`§M`: k=0 to date). If you propose a hit,
  it will be the first live test of a control nobody knows works. Say so in your notes.

Zero confirmed hits licenses **only** "keyword-proxy miss rate ≤ 31.2% at 95%" (1 − 0.05^(1/8)).
It does **not** license "the proxy is complete." Do not write a sentence that implies it does.

## Your 8 directories (`frame/sample.json` → `escape_sample`, seed 20260810, drawn from 101)

```
1  audits/2026-02-25_spectral_laplacian
2  audits/2026-05-30_authoring_closure_fabricated_defaults
3  audits/2026-06-04_oq71_depth_lineage
4  audits/2026-06-10_signature_liveness_crosscorpus
5  audits/2026-06-11_oq46_close
6  audits/2026-06-11_oq98_verdict_join
7  audits/2026-07-01_oq197_source_h1_crosstab
8  audits/2026-07-24_oq153_update_authority_step2
```

The frame is frozen; do not redraw, substitute, or extend it. Entry files are heterogeneous — a
directory's largest `.md` is sometimes not where the content is. **Read enough to answer the
question honestly**; a thin read biases toward NO-UNIT in exactly the direction that would make the
proxy look better than it is.

## The boundary rule — fixed 2026-08-11, before your extraction (`verdict_grammar_amendment.md` §H.1)

> **UNIT** iff the prose **REPORTS an incident** anywhere in the document, regardless of whether it
> is the document's subject.
> **NO-UNIT** only if the prose **DISCUSSES the concept** without reporting an instance.

A directory whose subject is something else entirely but which reports a defect it tripped over en
route **is a UNIT**. A directory that theorizes about silent failure without an instance is
**NO-UNIT**. Record which clause you applied, per directory, every time — including for NO-UNITs.

## Unit format — identical to the primary sample

Write one JSON per directory to `packets/escape_units/NN_<slug>.json`:

```json
{
  "source_dir": "2026-06-11_oq46_close",
  "extractor": "escape",
  "role": "escape",
  "matrix_unit": false,
  "escape_candidate": true,
  "files_read": ["..."],
  "symptom": "...", "mechanism_as_described": "...",
  "detection_path": "...", "consequence": "...",
  "extraction_notes": {"boundary_rule_applied": "UNIT — prose reports an instance at ...", "selection_rule_applied": "..."},
  "metadata": {"incident_location": "subject|incidental|self_audit_subsection", "incident_location_note": "..."}
}
```

- **`matrix_unit` is ALWAYS `false`** for escape units — they can never enter a cell. Quarantine
  keys on this field and nothing else (§I.2).
- **NO-UNIT directories still get a file**, with `"escape_candidate": false`, the four coder-facing
  fields omitted, and `extraction_notes.boundary_rule_applied` stating which clause and on what
  evidence. A silent absence is indistinguishable from an unexamined directory — the whole subject
  of this audit.
- **Unit = INCIDENT, never manifestation.** One incident per directory; if a directory reports
  several, apply the primary sample's rule — *fullest DESCRIBED mechanism*, since a unit can only
  carry the mechanism the prose states — and record the alternatives you did not extract.
- **`incident_location`** is recorded per unit and is NOT pooled across the primary and escape
  strata (§N).

## Redaction — do it as you write, not after

Strip the P-lexicon and source-identifying vocabulary from the four coder-facing fields. **Do NOT
strip shared subject matter** (`silent`, `never fired`, `green`, `empty`, `stale`, `fallback`) —
it belongs to neither taxonomy and removing it destroys codeability, which is the bias control (c)
exists to measure. Over-redaction corrupts that control.

Check yourself as you go — this needs no coding and no model call:

```
python3 python/audits/oq277_lexicon.py --sweep audits/2026-08-10_oq277_rq2_crosscoding/packets/escape_units/NN_x.json --direction ii
```

Note the matcher's known behaviour before you react to it: bare `P1`/`P2`/`P3` in these directories
is often that directory's **own local numbering** (probe names, field arms) and not our taxonomy —
see `controls/redaction_pair_selection_defect.md`. It is still stripped from coder-facing text (a
coder cannot tell either), but do not conclude from a `P1` hit that the directory discusses our
patterns.

## Done means

1. 8 files in `packets/escape_units/`, one per directory, UNIT or NO-UNIT, each naming the boundary
   clause applied.
2. Leak sweep clean over every coder-facing field.
3. **An ORDERED LIST of every file you opened, written AS YOU GO, not reconstructed at the end**
   (operator ruling, 2026-08-11), in `EXTRACTION_NOTES.md`. A declared slip is a self-report, and
   this design does not accept self-reports where a witness is available; the list is auditable
   against the do-not-open table above.
   **Its limit, stated so nobody treats it as closing the question:** the list is authored by the
   party it constrains, so it converts an assurance into a *checkable* assurance, not into proof.
   **The independent instrument already exists** — the four floor comparisons collapse toward zero
   if blindness broke, so the churn floor is what actually tests this and the file list is the cheap
   corroborating one.
4. A short `packets/escape_units/EXTRACTION_NOTES.md`: the UNIT/NO-UNIT split, which clause fired
   for each, any directory you found genuinely ambiguous (escalate rather than decide), and an
   explicit statement of whether the blindness rule held — **by what route you know it held**, not
   an assurance.
5. **No model call.** `payloads/` and `responses/` stay empty until the operator's spend-go at
   prereg freeze. If you think a live call is required, **stop and ask** — do not decide it.

Then hand back. Packet assembly, the driver, and the prereg are the other instance's.
