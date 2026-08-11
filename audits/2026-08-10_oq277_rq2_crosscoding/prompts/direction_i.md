You will read one description of a software failure and assign it exactly one category.

Here are the six categories, plus a seventh for incidents that fit none of them. All seven are
equally available answers.

P1 — Produced-but-not-consumed.
Data is generated and written, and nothing reads it back into the thing that needs it. A producer
is not done until something consumes its output. This also covers consumed-once-but-not-kept-fresh:
a post-process that is never re-run again goes silently stale while everything downstream keeps
reading it as current.

P2 — One-canonical-thing-became-two.
A file gets copied to a scratch or test location and edited; two versions now exist with no
queryable fact saying which one is canonical. The defect is the absence of a checked canonicity —
"which one is real" lives in someone's memory rather than in a path, a check, or a record.

P3 — Destructive-replace without proof.
Something is deleted, retired, or overwritten that another part of the system relies on, without
running old and new side by side and showing the outputs are identical or justifying every
difference. Believing two things are equivalent by reading them is not the same as demonstrating
it; the comparison is the proof, and it was not made.

P4 — Recap-as-witness substitution.
A summary claim — "done", "verified", "working", "complete" — stands in for the evidence that would
discharge it. Only the actual pasted output discharges such a claim. Reporting N pieces of work
completed while showing evidence for fewer than N is the defect; the missing evidence reads as
present because the summary asserts it.

P5 — Absence satisfies the gate.
A gate, threshold, or quantifier passes because its input is *missing*, not because a condition was
*checked*. A count equals zero when no records exist at all; a "for all X, Y" is trivially true over
an empty table; a ceiling comparison passes on a value that was never really supplied. The check
reports success without ever having examined anything.

P6 — Success-shaped absorption.
An aggregation or a channel cannot distinguish *measured-empty* from *never-looked*, and emits
success-shaped output either way. Each component is individually sound, so no check at any single
site catches it — the defect lives where the components compose. A failed computation and a real
measurement of nothing arrive at the read site as the same value.

other — The incident's mechanism is not one of the six above.
This is a substantive answer, not a leftover. Choose it whenever the mechanism described is
genuinely a different kind of thing, including when the description is too thin to identify a
mechanism at all.

---

Assign the single category that best matches the MECHANISM of the incident below — how the failure
worked, not where it happened or how severe it was.

INCIDENT
--------
Symptom:
{symptom}

Mechanism as described:
{mechanism_as_described}

How it was detected:
{detection_path}

Consequence:
{consequence}

---

Reply with exactly one of these tokens and nothing else:

P1  P2  P3  P4  P5  P6  other

No explanation, no confidence, no punctuation, no other words.
