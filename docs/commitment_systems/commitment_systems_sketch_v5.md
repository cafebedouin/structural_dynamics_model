# Commitment Systems and the Architecture of Drift — v5

*Superseded by `commitment_systems_sketch_v6.md` (2026-08-06); retained as history.*

*Revision of v4. This version makes three marked changes to the pattern taxonomy and its operationalization, all of them small and evidence-supported, and records one larger hypothesis as an explicit null. The v4 theory is otherwise carried forward unchanged; only the sections that change are reproduced here, with the rest of v4 remaining authoritative.*

*A note on scope, because the path to this version matters for reading it. An extended analysis explored whether the framework needed a second axis — a "committer" or "ground" dimension distinct from the observer positions the companion Deferential Realism (DR) engine already indexes. That exploration produced an elaborate and internally coherent structure. An empirical check against the constraint corpus found essentially no support for it (see §5, the null). What remains in the body of v5 is only what the corpus backs. The discarded structure is recorded as a null rather than deleted, so that the same ground is not re-explored from scratch later without the result attached.*

---

## 1. What changed from v4

Three marked revisions, all to the interpretive-accretion pattern and its schema/engine encoding, and one recorded null.

1. **Interpretive accretion gains a second sub-species.** The pattern is redefined substrate-neutrally; a second kernel-encoding is admitted as a legitimate instance.
2. **The interpretation-layer condition is corrected.** A three-layer inconsistency (schema / generation prompt / Prolog classifier) is documented and the intended condition stated.
3. **A redundant proposed refinement is retired.** The `noncanonical_formalized` sharpening considered during development is shown to be redundant against the existing field enum and is dropped.
4. **A committer/ground axis is recorded as a null** (§5), not adopted.

Each is developed below.

---

## 2. Interpretive accretion, redefined substrate-neutrally

### The defect in v4

v4 described interpretive accretion as: *"The text is fixed. Authority is grounded in continuity with the founding text. The formal mechanism for changing the text does not function or does not exist. Drift migrates entirely into interpretation."* It listed common-law jurisprudence among the canonical cases.

This is internally inconsistent. Common law has no fixed text. Its kernel is a formalized apparatus — multi-factor tests, holdings, elements, the Restatements as persuasive synthesis — developed through a precedential lineage, with no canonical founding document that the apparatus elaborates. The v4 definition opens with "the text is fixed," which is false for the case v4 itself names as canonical. The opening sentence bakes in a textuality assumption that the pattern does not actually require.

The invariant that interpretive accretion actually depends on is not textuality. It is: **a stabilized kernel that cannot be revised directly, paired with a lineage-grounded authority structure, where drift is absorbed into interpretation because the kernel's formal revision mechanism does not function or does not exist.** Whether the kernel is encoded as a fixed text or as a formalized-but-uncanonical apparatus is a second, independent axis. It does not change the mechanism.

### The revision

Interpretive accretion is redefined:

> *Interpretive accretion.* A stabilized kernel that cannot be revised directly is paired with a lineage-grounded authority structure. The formal mechanism for changing the kernel does not function or does not exist, so drift migrates into interpretation: everyone insists the kernel controls while the operational meaning shifts substantially. Authority is grounded in continuity with the kernel, whatever its encoding.

The pattern has **two sub-species**, distinguished only by how the kernel is held:

- **Text-anchored accretion.** The kernel is a fixed text — a constitution, a scripture, a founding statute — whose meaning develops through an interpretive lineage. Brahmanical commentary on the Vedas, Catholic doctrinal development, and constitutional originalism (despite its claim to fixity) work this way.
- **Principle-anchored accretion.** The kernel is a formalized principle with formal apparatus (tests, elements, holdings) but no canonical text, developed through a precedential lineage. Common-law doctrine is the canonical case; the evolution of attorney-client privilege waiver is a worked example.

Both are interpretive accretion. The mechanism — lineage-grounded authority over an unrevisable kernel, with drift absorbed into interpretation — is identical across them. Only the encoding substrate differs. (The substrate axis is the same one v4's appendix already named as the "kernel encoding substrate" cross-cutting feature; this revision connects the two.)

The naming is deliberately substrate-neutral. The legal term "precedent" is avoided in the sub-species label because the pattern is cross-domain: principle-anchored accretion includes scientific paradigms extended through canonical methodological inheritance and standards bodies whose rulings accumulate into quasi-doctrine, not only common-law precedent. Common law is the canonical *example* of principle-anchored accretion, not its definition.

### Why the two sub-species fail differently

v4's "Why systems fail" section gave interpretive accretion a single failure mode. The two sub-species fail differently, and v4's existing language already contains both clauses without having assigned them:

- **Text-anchored accretion** fails through *semantic detachment*: interpretation visibly detaches from the text until the gap between text and operational meaning becomes too large to plausibly reinterpret. The cover story is "we are faithful to the text."
- **Principle-anchored accretion** fails through *coherence-absorption strain*: the precedent or doctrine graph grows too large or too internally contradictory to maintain coherence. The cover story is "we are applying established doctrine."

These are the two clauses v4 already stated ("when the interpretive layer cannot absorb the operational drift" and "when accretion becomes so distant from the kernel that participants notice the gap") — now assigned to their respective sub-species rather than left as one undifferentiated failure mode.

### Operational encoding

In `prolog/cs_pattern_detection.pl`, the classifier currently has a clause for `(fixed_text, lineage) → interpretive_accretion` but **no clause for `(formalized, lineage)`**. A constraint coded `(formalized, lineage)` — principle-anchored accretion, the common-law case — falls through to the catch-all and classifies as `no_pattern_match, [anomalous_field_combination]`. This is not latent: the corpus constraint `privilege_waiver_threshold` is coded `(formalized, lineage)` and currently misclassifies as anomalous. The canonical example of one of the framework's own patterns is, in the present operationalization, unreachable.

The fix is a single clause, added beside the existing one:

```prolog
cs_classify(_, formalized, lineage, interpretive_accretion,
            [kernel_formalized, authority_lineage]) :- !.
```

No existing clause head matches `(formalized, lineage)`, so this addition shadows nothing. It can sit adjacent to the `(fixed_text, lineage)` clause.

A second clauseless combination surfaced during the same check: `(distributed, extraction)`, carried by the corpus constraint `drift_denial_authority_structure`, also falls through to `no_pattern_match`. This is a distinct gap — distributed kernel with extraction-grounded authority is a coherent configuration the taxonomy has no cell for — and it is **not** resolved in v5. It is recorded in the shakeout document as an open item, pending an example and analysis of its own, rather than bundled into this revision.

---

## 3. The interpretation-layer condition: a three-layer inconsistency

The `interpretation_layer_present` field is governed by three layers that currently encode three different rules:

- **Schema** (`constraint_story_schema.json`, the `allOf` rule): `interpretation_layer_present = true → kernel_codification = formalized`. (Kernel only; no condition on authority.)
- **Generation prompt**: `interpretation_layer_present` is "optional, only when `kernel_codification = formalized` AND `authority_grounding = extraction`." (Both.)
- **Prolog classifier**: `cs_interp_layer/1` is consulted in exactly one place — the `(formalized, extraction) → anchored_fixity_with_accretion` fork. It is never read in either interpretive-accretion clause.

The consequences:

1. The schema and prompt disagree on whether `authority_grounding = extraction` is required.
2. A constraint coded `(formalized, lineage, interp_layer = true)` — such as `privilege_waiver_threshold` — **passes schema validation but classifies as `no_pattern_match`** in the Prolog. Schema-valid and engine-anomalous at once. This inconsistency is not documented in the integration audit.
3. Because the interpretive-accretion clauses never consult `cs_interp_layer/1`, the flag has *no classification effect* for accretion constraints under the current Prolog. It is load-bearing only at the anchored-fixity fork. So the present defect is an **annotation asymmetry**: under the schema gate, a `(formalized, lineage)` accretion constraint can declare `interp_layer = true` (formalized passes the gate) but its structural twin `(fixed_text, lineage)` cannot (fixed_text fails the gate) — even though both are, by the theory, the paradigm functioning interpretation layer.

### The intended condition

An interpretation layer is licensed by what *grounds* it, not by the kernel's encoding. The condition should key off authority:

> `interpretation_layer_present = true` is licensed when `authority_grounding = lineage` (any kernel encoding) OR (`kernel_codification = formalized` AND `authority_grounding = extraction`).

This permits both interpretive-accretion sub-species to annotate the layer they definitionally have, retains the anchored-fixity-with-accretion case, and correctly blocks the flag on marked-revision, diffuse-reconstruction, and implicit-practice configurations, where an interpretation layer is meaningless.

A note on the deeper option, recorded but not adopted: because the flag is documentary-only for accretion under the current Prolog, the cleaner long-run shape is to stop treating `interp_layer` as a free authored field for accretion at all — derive it as implied-true for accretion, and keep it a free variable only at the anchored-fixity fork where it actually discriminates. That is a schema-shape change, not a v5 patch, and is deferred to the shakeout document.

---

## 4. A retired refinement

During development, a sharpening was proposed: rename `formalized` to `noncanonical_formalized` to mark that the principle-anchored kernel has no canonical anchor token. This is redundant. The `kernel_codification` field is a single-valued enum (`none | formalized | fixed_text | distributed | implicit`) in which `formalized` and `fixed_text` are mutually exclusive. `formalized` therefore *already means* "a stabilized apparatus with no canonical anchoring text" — if there were a canonical text, the field would read `fixed_text`. The rename adds a word and no distinction. It is retired and recorded as retired so it is not re-proposed.

---

## 5. Recorded null: the committer / ground axis

An extended analysis during this revision pursued the hypothesis that the framework was missing a structural dimension distinct from the DR engine's observer positions. The intuition arose from a real observation — that disagreement about a commitment system (e.g. legal realism vs. originalism vs. positivism about the same legal order) seemed not to reduce to differences in observer power-position. The hypothesis developed in several stages: that there is a *ground* axis (what an observer takes the kernel to be); that there is a *response-mode* axis (live-with-it / change-it / leave); that "ground" reduces to kernel-level instances of the engine's existing signatures (natural law as a mountain-claim adjudicated by the False-Natural-Law override; realism as a drift-claim); and finally that the whole thing is a second "committer" sheaf on a space of possible kernels, with a drifting reachable-window governed by use-decay and finite carrying capacity — a theory that reached into cultural transmission, language death, and the non-transmissibility of tacit catastrophe-knowledge across generations.

The structure was internally coherent and is, as a theory of culture, possibly correct. It has **no fingerprint in the constraint corpus.**

An omega taxonomy was run to test it. The reasoning: if the engine encountered committer-axis content it structurally could not resolve, that content should accumulate in the omega variables (the catch-all for unresolved analytical content). The omegas were classified against pre-defined categories — observer-sheaf residue (O), ordinary empirical (E), and four committer-candidate categories: kernel-alternatives (K), population-level/nesting (P), constraint-space contingency / WEIRD-boundedness (C), and beneficiary-less path-naturalization (N).

Result, against a classifiable base of 319 omegas (the corpus is 86% collapsed-template observer-sheaf boilerplate, correctly classified O without contributing signal):

- **O (observer-sheaf): ~64% of classifiable; ~96% of total.**
- **E (empirical): ~31% of classifiable.**
- **K: 4 omegas (1.25% of classifiable), all from AI-alignment files where "kernel" is object-level vocabulary — all self-flagged for domain tautology.**
- **P: 0. C: 0. N: 0.**

The pre-registered kill condition K1 (committer fraction small and stretched → no fingerprint) fired decisively. Non-tautological committer content — committer omegas from domains *not* about commitment systems — was zero. The path-naturalization category (N), which the analysis had treated as its strongest prediction, was empty; the one candidate had an identifiable beneficiary and was ordinary False-Natural-Law territory. Population-nesting (P) and constraint-space contingency (C), the two categories that the cross-domain examples (abortion, gay rights, the WEIRD critique) had made feel most obviously real, were empty.

**Conclusion.** The committer/ground axis is not refuted as a theory of culture, and the language/transmission program it suggests may be worth pursuing in its own (linguistic, diachronic) data. But it has no support in the constraint corpus, and it does not enter the framework. The exit-atom ambiguity that originally seeded the idea (the dissociation recon's ~118 "which exit atom binds this actor" omegas) is, on this evidence, observer-sheaf residue, not committer residue.

One fair test remains unrun: the SOTU corpus (~918 omegas, U.S. policy history, a non-commitment-systems domain where "kernel" is not native vocabulary) was only sampled (3 files), not fully classified. It is the most plausible home for non-tautological P/K content. A full SOTU omega pass is the single follow-up that could move this verdict — and the disciplined prior, after this result, is that it will return another null. It is worth running for the clean null as much as for the long-shot signal.

This null is recorded so that the committer axis is not rebuilt from first principles later without the result attached.

---

## 6. What is unchanged from v4

Everything else. The five response patterns (marked revision, interpretive accretion as now revised, diffuse reconstruction, implicit practice, anchored fixity ± accretion) stand. The three primitives (kernel, authority structure, drift), the structural problem, the positional analysis, the self-application, the mathematics demonstration, and the appendix of provisional refinements are carried forward from v4 unchanged. v5 changes only the interpretive-accretion definition and its encoding, corrects the interpretation-layer condition, retires one redundant refinement, and records one null.

The framework's own revision discipline is worth noting in passing: this version is a marked revision of the theory document — the framework's own preferred acknowledgment mode, applied to itself. The change is legible and dated rather than absorbed silently into the prose.
