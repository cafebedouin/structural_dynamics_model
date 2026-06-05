# SCOPE Prompt Addition — Kernel Decomposition (Committer Frame)

> INTEGRATION NOTE (for the human, not the model): This text extends the EXISTING
> `commitment_system_recognition` object already defined in uke_scope_v2_json.md (§5,
> lines ~307-323). It does NOT introduce a new top-level structure. Insert §1.3-K below
> into §1.3 (commitment-system identification), and replace the existing
> `commitment_system_recognition` object spec with the expanded version below.
> Everything here is additive: a manifest with no kernel decomposition is still valid,
> exactly as today. Ordinary (non-kernel) topics are unaffected.

---

## §1.3-K  Kernel / Reading Decomposition

Some topics are not single constraints but **contested kernels**: one persisting
commitment that different parties *read* differently, where each reading instantiates a
genuinely different constraint. The disagreement is not about where an observer stands
relative to a fixed constraint (that is the observer axis the engine already handles) —
it is about *which constraint exists to be observed at all.*

Two jurors, equal in power and exit, reach different verdicts because they hold different
**readings** of the same legal kernel — one a legal realist, one a positivist. They are
not two observers of one constraint; they instantiate two constraints from one kernel.
Pro-life and pro-choice are not two positions on one abortion constraint — they read the
*personhood-boundary kernel* differently, and the readings emit constraints with different
victim sets (the conception reading places the fetus in the victim set; the birth reading
does not).

### The kernel/not-kernel decision

Most topics are NOT kernels. "Alberta separatism," "the 8K TV saturation limit," "a
Markov absorbing-state trap" are single constraints — decompose them into axes as usual
and emit NO kernel structure. A topic is a kernel only when ALL of:

- there is one **shared commitment** that all parties are arguing *about* (a substrate they
  hold in common), AND
- parties **read that commitment differently** in a way that changes what it constrains, AND
- the readings would emit **structurally different constraints** (different type,
  beneficiary/victim structure, or base extractiveness) — not merely different opinions
  about the same constraint.

If the parties agree on what the constraint is and only disagree about whether it is good,
that is NOT a kernel — it is one constraint read from different observer positions. Use the
ordinary axis decomposition.

### When the topic IS a kernel

Decompose into **readings, not flat axes.** Each reading becomes one entry in
`generation_sequence`, tagged with its `reading_id` and `kernel_id`. The readings are the
axes. Identify 2-4 readings (the structural budget of three still applies to what proceeds
to generation; document additional readings as deferred).

Name readings in **free text** — there is no controlled vocabulary. Name each reading by
the interpretive commitment it encodes (`conception_reading`, `viability_reading`,
`realist_reading`, `originalist_reading`). Link sibling readings of the same kernel to one
another the way axes link via `downstream_of` — every reading lists its sibling readings so
the generator knows it is instantiating one of a set.

### Three primitives to record

For a recognized kernel, characterize:

- **kernel** — the persisting shared commitment (the Constitution; the personhood boundary;
  acceptable-risk-for-energy). It has identity that persists while its instantiation varies.
- **authority structure** — what grounds the commitment's force under each reading (text,
  lineage/precedent, practice, expertise, distributed consensus). Different readings often
  ground authority differently; that difference is itself part of the contest.
- **drift** — how the commitment's instantiation changes over time while its identity
  persists. Some kernels are temporal (the US Constitution's readings shift across
  amendments; a reading's force can decay or accrete without the text changing). For now,
  SCOPE only *flags* drift_status; the present-day reading is what proceeds to generation,
  and temporal structure is noted, not modeled.

### Expanded `commitment_system_recognition` object

Replace the existing object with this superset. All new fields are optional; the original
four fields are unchanged, so a recognition that does not decompose into readings is still
valid.

```json
"commitment_system_recognition": {
  "kernel_description": "one sentence describing the stabilized shared commitment",
  "authority_description": "what grounds interpretive legitimacy",
  "drift_status": "functioning | partial | absent",
  "candidate_pattern": "<one of six CS pattern names> | uncertain",

  "is_contested_kernel": true,
  "kernel_id": "personhood_boundary",
  "readings": [
    {
      "reading_id": "conception_reading",
      "commitment": "moral status begins at conception",
      "authority_grounding": "natural-law / religious",
      "sibling_readings": ["viability_reading", "birth_reading"],
      "expected_structural_delta": "fetus enters victim set; high suppression of alternatives"
    },
    {
      "reading_id": "viability_reading",
      "commitment": "moral status begins at viability",
      "authority_grounding": "medical / consequentialist",
      "sibling_readings": ["conception_reading", "birth_reading"],
      "expected_structural_delta": "graduated victim status; threshold contested"
    },
    {
      "reading_id": "birth_reading",
      "commitment": "moral status begins at birth",
      "authority_grounding": "positivist / autonomy",
      "sibling_readings": ["conception_reading", "viability_reading"],
      "expected_structural_delta": "fetus not in victim set; mother sole rights-holder"
    }
  ]
}
```

When `is_contested_kernel` is true, each reading in `readings` must correspond to one entry
in `generation_sequence`, and that entry carries `kernel_id` and `reading_id` so the
generation step knows it is producing one reading of a set. When `is_contested_kernel` is
absent or false, ignore all of this and decompose into ordinary axes — the default, correct
for most topics.

### Coherence is the model's signal, not a gate

If, in trying to name the readings, you find that two supposed readings would emit the
*same* constraint (same type, same victim set, same ε), they are not two readings — they
are one reading named twice, and the apparent disagreement is not a real kernel contest.
Record this: set `is_contested_kernel` false and note in an omega that the topic looked
like a kernel but the readings collapse. Conversely, if the supposed readings share no
common substrate at all — no overlapping affected population, no shared commitment — the
topic may be several distinct kernels, not one read many ways. Record that too. These
collapse/incoherence observations are valuable findings, the committer-axis analog of the
sheaf coherence the engine already checks on the observer axis. Do not force a kernel that
does not cohere.
