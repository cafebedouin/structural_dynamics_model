# Generation Prompt Addition — Kernels and Readings (Committer Frame)

> INTEGRATION NOTE (for the human): Insert this section into
> constraint_story_generation_prompt_json.md immediately AFTER the "Directionality
> Overrides" section (~line 549, before the "UKE_SCOPE Integration" section). It is
> modeled on that section's when-to-use / when-NOT format. It is additive: a constraint
> generated with no kernel context behaves exactly as today.

---

## Kernels and Readings (Committer Frame)

Some constraints you generate are **one reading of a contested kernel**. A kernel is a
single persisting commitment that different parties read differently, where each reading
instantiates a *different* constraint. The Constitution is a kernel; originalist and living
readings emit different constraints from the same text. The personhood boundary is a kernel;
the conception reading and the birth reading emit constraints with different victim sets.
Two jurors equal on every observer dimension reach different verdicts because they hold
different readings of the legal kernel.

When the SCOPE manifest hands you an axis tagged with a `kernel_id` and a `reading_id`, you
are generating ONE reading. Three rules govern this, and the first is the one that matters
most.

### Rule 1 — Generate the one reading as a clean, ε-invariant constraint.

Generate the constraint for *your* reading only. Do NOT describe the contest inside the
constraint. Do NOT list the other readings in the narrative, do NOT hedge ε across the
readings, do NOT average over them. Your reading instantiates one specific constraint with
one stable ε, one beneficiary/victim structure, one type — exactly as DP-001 (ε-invariance)
requires. The other readings are *other constraints* (other files, the sibling readings in
your manifest entry); they are not part of this one.

This is the same discipline as the closed context tuple: just as you must not add a fifth
argument to context/4, you must not fold alternative readings into one constraint's
classification. One reading, one constraint, one ε.

### Rule 2 — Route the committer content to omega variables.

The committer structure — which kernel this is, which reading you are instantiating, what
the sibling readings would change, where the disagreement is located — does NOT go in the
standard fields and does NOT get its own invented field. It goes in **omega variables**, the
engine's existing channel for structure it cannot otherwise hold. Write one or more omegas
that record:

- that this constraint is one reading of `kernel_id`, naming the reading;
- what a sibling reading would change structurally (e.g. "under the conception_reading the
  fetus enters the victim set; this birth_reading excludes it");
- where the disagreement is *located* — the specific structural element the readings differ
  on (victim-set membership, authority grounding, threshold placement).

If you find yourself wishing for a field the schema does not have to express committer
structure, that wish IS the omega. Write it as an omega rather than inventing the field.
These omegas flow into the enhanced report and give the downstream essay model the
relational framing — "this is one reading of a contested kernel, here is what the others
change" — which is the entire point of the committer frame.

### Rule 3 — Record the reading in `kernel_context` (optional free-text).

If the schema's optional `commentary.kernel_context` field is present, write a short
free-text note there naming the kernel, your reading, and the sibling readings, parallel to
how `directionality_logic` documents the directionality choices. This is for findability,
not structure — prose, not IDs-and-relations. If the field is absent, the omegas alone
carry it.

### When NOT to use the committer frame

- When the manifest entry has NO `kernel_id` / `reading_id` — the topic is an ordinary
  single constraint. Generate it exactly as you would any constraint. Do NOT invent a kernel.
  Most constraints are not readings of kernels.
- As a substitute for declaring beneficiary/victim. Declare the structural data for YOUR
  reading first; the committer omegas annotate it, they do not replace it.
- To describe a mere difference of opinion. If two parties agree on what the constraint is
  and only disagree about whether it is good, that is the observer axis, not a kernel — and
  SCOPE should not have tagged it. If you receive a kernel tag but the readings would emit
  the *same* constraint, say so in an omega (the readings collapse — this is not a real
  kernel contest) and generate the single constraint.

### Temporal kernels

Some kernels (the US Constitution) have readings whose force shifts over time without the
text changing. For now, generate the **present-day** reading as your constraint, and note
any temporal drift (a reading whose emitted constraint has decayed or accreted over time) in
an omega. Do not attempt to model the kernel's full history in one constraint — that is
future work, and cramming it in would break ε-invariance.
