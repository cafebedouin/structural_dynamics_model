# Commitment Systems and the Architecture of Drift — v5.1

*Superseded by `commitment_systems_sketch_v6.md` (2026-08-06); retained as history.*

*Revision of v5. This version makes one marked change: it corrects the §5 committer-axis null, which v5 recorded as a single undifferentiated negative result over the whole committer apparatus. A predicate-level audit of the DR engine's directionality channel found that one narrow component of the committer axis — the beneficiary/victim distinction — does leave a measurable, ε-independent fingerprint in classification, while the rest of the apparatus the null covered remains exactly as unsupported as v5 recorded it. The correction graduates the one component the evidence backs and quarantines the remainder more sharply than the bundle-null did. All other sections of v5 are carried forward unchanged and remain authoritative; only §5 is reproduced here in revised form, with the new evidence and a re-drawn boundary around what did and did not graduate.*

*A note on why this is a marked revision and not a silent edit. v5's §5 recorded a null over a bundle: committer dimension, possibility-space apparatus, reachable window, carrying capacity, all reported under one verdict. The audit decomposed that bundle. The honest revision is not "the null was wrong" — it was right for almost everything it covered — but "the null was reported at the wrong granularity, and one member of the bundle has since been shown to carry classification weight." Recording this as a marked, dated change rather than rewriting §5 in place is the framework's own preferred acknowledgment mode applied to itself, and it keeps the original bundle-null legible as the thing the decomposition corrected.*

---

## 1. What changed from v5

One marked revision, to §5 only.

The v5 §5 null is **split into a graduated component and a standing null.** The beneficiary/victim distinction — the narrowest operationalizable fragment of the committer axis — is shown by a directionality-channel audit to produce real, ε-independent classification flips at one observer position. It graduates into the framework as an acknowledged, bounded feature. Everything else §5's null covered — the possibility-space sheaf, the reachable window, use-decay, carrying capacity, path-naturalization as a distinct mechanism, the population-nesting and constraint-space-contingency categories — remains null, now on sharper grounds: the audit built the directionality instrument the broader apparatus was supposed to underlie, and the broader apparatus was never needed to make it work.

Sections 2, 3, 4, and 6 of v5 are unchanged. Section 5 is replaced by §§5.1–5.4 below.

---

## 5.1 The original null, and what it correctly covered

v5 §5 recorded that an extended analysis had pursued a *committer / ground axis* distinct from the DR engine's observer positions — a second dimension indexing not *where the observer stands* but *which commitment, out of a space of possible commitments, exists to be observed.* The analysis developed an elaborate structure: a committer sheaf over a space of possible kernels, a drifting reachable-window governed by use-decay and finite carrying capacity, and a reduction of "ground" disagreement (realism vs. originalism vs. positivism about the same legal order) to kernel-level instances of the engine's existing signatures.

An omega taxonomy tested it. Against a classifiable base of 319 omegas, the committer-candidate categories returned: K (kernel-alternatives) 4, all tautological self-flags from AI-alignment files where "kernel" is object-level vocabulary; P (population-nesting) 0; C (constraint-space contingency) 0; N (beneficiary-less path-naturalization) 0. The pre-registered kill condition fired. The null was recorded over the whole apparatus.

**That null was correct for almost everything it covered, and v5.1 does not disturb it.** The possibility-space formalism, the window, carrying capacity, and the path-naturalization mechanism have no corpus support and none has since been found. What v5.1 corrects is narrower: the null was reported as a single verdict over a bundle that contained one separable, testable component, and that one component was not actually adjudicated by the omega test — because the omega instrument was the wrong instrument to detect it.

## 5.2 The wrong-instrument problem, made specific

The omega taxonomy looked for committer content in the *residue* — the unresolved analytical material the engine could not classify and routed to omega variables. The reasoning was that committer-axis content the engine structurally could not handle would accumulate there. This was a reasonable place to look and it came back empty.

But the omega residue is the wrong place to look for the one committer component that is *already wired into the engine's directionality computation.* The DR engine computes experienced extractiveness as χ = ε · f(d(P, E)) · σ(S), where the directionality term d is set, for constraints carrying beneficiary/victim structure, by a `power_role_heuristic` that reads exactly one bit of committer content: **does this constraint have victims, yes or no** (and, at the institutional power level, the symmetric bit: does it have beneficiaries). That bit is not residue. It is a live input to classification. An instrument that searches the omega catch-all for committer fingerprints is, by construction, blind to a committer signal that has already been consumed upstream as a directionality input — the signal never reaches the residue because the engine already used it.

This is the same failure shape the original analysis itself identified for path-naturalization: a beneficiary-indexed detector cannot see a beneficiary-less phenomenon. Here, an omega-residue detector cannot see a committer signal the engine has already absorbed into d. The omega null is real, but it is a null about *unabsorbed* committer content, not about committer content as such.

## 5.3 What graduated: the beneficiary/victim bit

A predicate-level audit of the directionality channel tested whether the beneficiary/victim bit carries classification weight *independent of base extractiveness ε.* The test held ε fixed and asked whether the presence of victim structure — which moves d from 0.65 to 0.70 at the moderate observer via `power_role_heuristic(moderate, _, true, 0.70)` versus `(_, false, 0.65)` — was enough to change a constraint's type.

The procedure was pre-registered, with a deflationary kill condition (no flips, or flips attributable to a cascade gate other than the χ threshold → INERT) defined before any classification ran, and an isolation requirement: a flip counts only if it survives reverting d to its canonical value with every other input (suppression, theater, immutability, the cross-context `snare_immutability_check`) held identical, so that the type change is attributable to the committer bit and nothing it merely co-occurred with.

Result, on 66 clean candidate constraints at the moderate observer (ε in the flip-zone, no directionality override): **20 flipped, all snare → tangled_rope, all χ-gate-driven, zero confounded.** The mechanism is uniform: under the victim bit, d = 0.70, f(d) ≈ 1.107, χ ≈ 0.686, which clears the 0.66 snare floor; with the bit absent, d = 0.6459, f(d) ≈ 1.000, χ ≈ 0.620, which falls below the floor and the constraint drops to tangled_rope. Only d changed in the isolation swap; every gate that could have confounded the result was pinned and none did.

**This graduates a real, ε-independent committer signal into the framework.** Two constraints with identical base extractiveness can classify differently solely because one has victim structure and the other does not. That is a committer-axis fact — it is about a property of the commitment itself, not about the observer's position — and it is operationalized, live, and load-bearing in the engine.

The boundaries of what graduated, stated precisely so the bundle does not reassemble:

- **It is one bit wide.** The signal is a binary has-victims / has-beneficiaries flag. The corpus stores richer beneficiary/victim content — named roles, multiple victims and beneficiaries per constraint — but the engine collapses all of it to presence/absence. Whether the discarded richness carries classification weight beyond the one bit is *not* established by this result and remains open.

- **It is calibration-dependent in magnitude, structural only in existence.** The 20 flips sit at ε ≈ 0.62, all of one type, riding a single hand-set 0.05 constant at the one observer nearest the sigmoid's steep region. That the channel *exists* is structural. *How wide it is* is an artifact of the sigmoid steepness (k = 6.0, d₀ = 0.50) and the specific heuristic constants; a different calibration could widen or erase it. The framework should claim the existence, not the magnitude. (This is the same discipline the site-stability analysis applied: distinguish a structural invariant from a calibration-dependent quantity, and never let the second masquerade as the first.)

- **A symmetric channel is un-audited.** `power_role_heuristic` reads the victim bit at non-institutional power and switches to the beneficiary bit at institutional power. The audit tested the victim bit at moderate. The institutional beneficiary channel is its structural twin and has not been tested; the prior is that it is dormant, because institutional g-path directionality sits at d ≈ 0.07–0.12, which drives χ negative and pins the institutional observer to rope regardless of the bit. That dormancy is plausible but unconfirmed.

## 5.4 What did not graduate, and now on sharper grounds

Everything else §5 covered remains null. The correction does not soften this — it strengthens it. The original null said the committer apparatus was "unsupported, possibly wrong corpus." After the audit the verdict is more diagnostic: **the framework now contains the operationalized committer instrument the broader apparatus was supposed to ground, and the broader apparatus was never required to build or run it.**

- **The possibility-space structure** — the sheaf over kernels, adjacency, morphisms, the Dirac-sea background — contributed nothing to the working channel. The directionality heuristic classifies by a flat lookup over (power × bit); it needs no adjacency relation, no topology, no "near" between kernels. The channel works as an unstructured classification grid, which is empirical evidence for the deflationary horn of the original analysis's own fork: the kernel space behaves like a set with a grid over it, not a site with morphisms.

- **The reachable window, use-decay, finite carrying capacity, and the catastrophe theorem** are a theory of *diachronic cultural dynamics* — how commitments change, decay, and are lost over time. The graduated result is *synchronic*: it is about how a fixed constraint classifies, not about how the space of constraints evolves. The dynamics remain untested by anything here, and an independent adversarial review (eight models, red-teaming the dynamics directly) found the strong forms of use-decay-as-reachability, culture-level finite capacity, and the catastrophe theorem each break against documented cases. The dynamics are not part of the framework and on current evidence the strong forms are false; what survives is a mechanism-level account (decay-by-disuse, the symbol/commitment split) that is not a possibility-space theory and needs no geometry.

- **Path-naturalization as a distinct mechanism** (naturalization with no beneficiary, as opposed to a hidden beneficiary behind a cover story) remains conceptually live but **empirically homeless**: the adversarial review could not produce a single uncontaminated case — every candidate, on inspection, had an identifiable beneficiary — and the corpus carries no instance the engine reads as beneficiary-less. It is retained as a conceptual distinction worth keeping, on probation, with zero validated instances.

The rich committer dimension — *which kernel was committed to, out of what space of rejected alternatives* — is the part the possibility-space theory most cared about, and it is the part with the least support. The corpus's structured "rejected alternative" content (the `intent_viable_alternative` family) is unpopulated in the live corpus, exists only in archived datasets under older schema, and is not propagated to the classification pipeline. Whether that content would carry weight if populated is untested. The graduated bit is the *thinnest* committer signal — a single beneficiary/victim flag — and it should not be read as evidence for the rich dimension. The audit bought the committer *axis* one bit of credence. It bought the committer *space* nothing.

## 5.5 Standing follow-ups

Two tests would move this verdict, each with a pre-registered deflationary outcome that is the disciplined prior:

1. **The institutional beneficiary channel** (the symmetric twin of the graduated victim channel). Prior: dormant, because the institutional observer is pinned to rope by negative χ. A confirmed dormancy would show the graduated channel is special to the moderate observer's sigmoid position rather than general — itself worth knowing.

2. **The full SOTU omega pass** (carried forward from v5: ~918 omegas, a non-commitment-systems domain where "kernel" is not native vocabulary, the most plausible home for non-tautological P/K content). Prior, after the moderate-channel result and the adversarial review: another null. Worth running for the clean null as much as for the long-shot signal.

Both are recorded so the committer axis is not re-explored from first principles without the results attached, and so the boundary drawn here — one graduated bit, everything else standing null — is the thing any future work starts from rather than re-litigates.

---

## 6. What is unchanged from v5

Everything except §5. Sections 2 (interpretive accretion redefined substrate-neutrally), 3 (the interpretation-layer three-layer inconsistency and its intended condition), and 4 (the retired `noncanonical_formalized` refinement) stand as written in v5. The five response patterns, the three primitives (kernel, authority structure, drift), the structural problem, the positional analysis, the self-application, the mathematics demonstration, and the appendix of provisional refinements are carried forward from v4 through v5 unchanged.

v5.1 changes only the committer-axis verdict: it graduates the beneficiary/victim bit as a bounded, calibration-dependent, ε-independent classification signal, and re-draws the boundary around the standing null so that the possibility-space apparatus, the diachronic dynamics, and the rich committer dimension are quarantined more sharply than the original bundle-null allowed — not "unsupported pending a better corpus," but "the instrument they were meant to ground was built without them."
