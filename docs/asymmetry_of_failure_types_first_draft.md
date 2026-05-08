# The Asymmetry of Failure Types: Detection, Architecture, and Discipline in
# Analytical Validation

## Abstract

A prior essay (debugging_philosophy.md, December 2025) articulated three
failure types in analytical work: Type A (frame drift), Type B (structural
inconsistency), Type C (indexical underspecification). Five months of
developing a formal validation apparatus around the Deferential Realism
framework produced a finding the original essay did not anticipate: the
three types are not symmetric in how they admit detection. Type B failures
can be caught by formal computation against structural axioms because their
signatures are mathematical. Type C failures cannot be caught after the
fact because they are failures of specification — they are addressed by
requiring complete specification at indexing time, not by detection at
validation time. Type A failures cannot be caught by an apparatus operating
under the drifted frame, because the apparatus is itself running under that
frame and cannot see the drift it is part of.

This asymmetry has consequences for how analytical apparatus should be
designed. Type B admits formal detection and benefits from redundancy —
multiple subsystems checking the same structural property catch
false-negatives that single-mechanism detection would miss. Type C admits
architectural treatment — the apparatus is built so that underspecified
analyses cannot run, rather than detecting underspecification afterward.
Type A admits no formal detection from inside the apparatus and requires
paired mechanisms — a synchronic discipline (preventing frame drift during
analysis), a diachronic detection method (catching frame drift across
time), and external review (catching frame drift the apparatus itself
cannot detect even diachronically).

The DR apparatus exemplifies all three treatments. The trifurcation
diagnoses what kinds of failure can occur; the apparatus implements
appropriate treatment for each kind. The pairing of taxonomy and apparatus
suggests a more general principle: formal analytical apparatus should
match treatment strategy to failure type, with explicit recognition that
some failure types do not admit formal detection and require external
discipline.

## 1. The Original Trifurcation

The December 2025 essay articulated three types of analytical failure...

[Brief recap of debugging_philosophy.md, ~400 words. Type A: frame drift.
Type B: structural inconsistency. Type C: indexical underspecification.
The original essay treated these as three categories of failure that
analytical work should be alert to, without distinguishing them by
detection properties.]

## 2. The Asymmetry Discovered Through Implementation

Implementing formal validation across the three types revealed an
asymmetry the original taxonomy did not articulate. The asymmetry concerns
how each type admits detection.

### Type B: formal detection through axiomatic checking

Type B failures — structural inconsistency between a constraint's claimed
classification and the structural properties its perspectives, beneficiary
structure, or directionality require — admit formal detection. The
signatures of structural inconsistency are mathematical: cohomological
obstructions to perspective gluing, gauge-orbit variance under indexing
transformations, Boltzmann compliance violations against thermodynamic
floors, divergence between Dirac orbit calculations under directionality
override and at canonical values.

Because Type B signatures are mathematical, multiple independent detectors
can target the same kind of failure through different formal mechanisms.
The DR apparatus implements at least five Type B detection subsystems with
distinguishable axiomatic foundations. Redundancy here is a feature: a
constraint that passes one Type B check but fails another reveals
structural inconsistency that single-mechanism detection would miss. Type
B detection benefits from multiple subsystems because Type B failures can
take different mathematical forms while remaining structurally
inconsistent.

### Type C: architectural treatment through specification requirement

Type C failures — indexical underspecification, where the analysis has
not specified the position-relative scope, time horizon, or observer set
at which the constraint applies — do not admit detection at validation
time. By the time validation runs, the indexing is either complete or
incomplete; if incomplete, the validation cannot proceed because the
inputs it requires are missing.

The DR apparatus addresses Type C architecturally. The schema for
constraint stories requires complete indexing fields before validation
can run. The Prolog engine refuses to validate stories with missing
indexing; the JSON validator catches incomplete indexing at authoring
time. Type C is not caught by detection; it is prevented by making
underspecification structurally impossible at the validation step.

The architectural treatment has implications. The treatment works only if
the indexing requirements specified at authoring time match the indexing
requirements the analysis actually needs. If the schema requires four
positions when the constraint involves six structurally distinct
positions, the apparatus will validate stories that are formally complete
but indexically inadequate to their subject. Type C admits architectural
prevention but not architectural verification — the apparatus cannot
check whether the indexing chosen matches the constraint's intrinsic
structural complexity.

### Type A: undetectable from inside, paired mechanisms required

Type A failures — frame drift, where the analytical frame shifts during
the analysis, where routing inputs leak into authority claims, or where
the same constraint is being analyzed under inconsistent indexings —
cannot be caught by an apparatus operating under the drifted frame. The
apparatus is itself running under whatever frame is currently operative;
its formal computations execute in that frame; it cannot see the drift
because the drift is the condition of its operation.

This is a structural property of formal validation, not a quirk of any
particular apparatus. Any system that validates outputs by formal
computation against axioms is computing under whatever frame the axioms
encode. If the axioms have drifted relative to their original meaning, or
if the inputs to the computation have been transformed in ways the
computation does not see, the computation cannot detect the drift it is
part of.

Type A requires paired mechanisms with different operational logics:

A synchronic discipline that prevents frame drift during analysis. The DR
apparatus implements this through the metrics-as-routing principle —
threshold positions and classifications are treated as routing decisions
rather than authority claims, evaluated by whether they enable better
decisions rather than by whether they match objective truth. The
discipline is enforced through code review and architectural conventions,
not through runtime detection. It prevents Type A by structuring how the
apparatus is allowed to make claims about its own outputs.

A diachronic detection method that catches frame drift across time. The
DR apparatus implements this through drift detection — tracking how
constraint classifications, network contagion, and structural properties
evolve across time points. Drift detection cannot catch frame drift while
operating under the drifted frame, but it can catch frame drift after the
fact by comparing the apparatus's outputs from different moments. Frame
drift becomes visible as classifications shifting on constraints whose
underlying properties have not changed.

External review that catches frame drift the apparatus cannot detect
even diachronically. The DR apparatus implements this through periodic
gate audits — humans examining the apparatus from outside its current
operating frame, looking for places where routing inputs have leaked into
authority claims, where validation gates have been compromised by
implementation drift, where the apparatus's behavior has shifted in ways
neither synchronic discipline nor diachronic detection caught. The April
2026 gate audit found three such cases, all latent — corpus-protected
through redundancy rather than caught by other mechanisms.

The three Type A mechanisms are not redundant. They cover different
classes of frame drift. Synchronic discipline prevents drift within an
analysis; diachronic detection catches drift across analyses; external
review catches drift the apparatus's own operations cannot see. None of
the three alone is sufficient.

## 3. Why the Asymmetry Matters

The asymmetry between failure types has implications for how analytical
apparatus should be designed and evaluated.

For apparatus design, the asymmetry suggests that the question "how does
this apparatus catch failures?" should be answered separately for each
failure type. A single answer — "formal validation" — papers over the
fact that formal validation works for Type B, must be supplemented by
architecture for Type C, and is structurally inadequate for Type A. An
apparatus that claims to provide formal validation across all three
types either has a Type A blind spot it has not recognized, or has
extended "formal validation" to include architectural and external
mechanisms in ways that obscure what each mechanism actually does.

For apparatus evaluation, the asymmetry suggests that reliability claims
should be qualified by which failure types the apparatus is reliable
against. The DR apparatus is reliable against Type B failures because
multiple independent detectors provide cross-validation. It is reliable
against Type C failures because architectural requirements prevent them
from arising at validation time. It is reliable against Type A failures
only insofar as the synchronic discipline is enforced, the diachronic
detector runs, and external review is performed at adequate frequency —
none of which are guaranteed by the apparatus itself.

For framework presentation, the asymmetry has implications for what kinds
of claims can be made about an analytical framework's outputs. Outputs
that depend on Type B reliability can be claimed with confidence
proportional to the number and independence of detection mechanisms.
Outputs that depend on Type C completeness can be claimed only as far as
the architectural requirements actually match the analytical need.
Outputs that depend on Type A reliability — frame stability across time
and across operating contexts — should be qualified by acknowledgment
that frame drift is undetectable from inside the apparatus and is
mitigated only through paired mechanisms with their own failure modes.

## 4. Open Questions

[This section is honest about what's not settled. Three questions:]

The asymmetry was discovered in implementing one apparatus. Whether other
analytical apparatus exhibit the same asymmetry, or whether different
domains have different detectability profiles, is not established. The
claim here is grounded in the DR case; generalization to other frameworks
requires comparable analysis of those frameworks' apparatus.

The Type C architectural treatment depends on the schema's indexing
requirements matching the analysis's actual indexing needs. How to
verify that match — beyond expert judgment at schema design time — is
not addressed by the current apparatus and is an open methodological
question.

The Type A external review mechanism currently operates ad-hoc, triggered
by accumulated drift events or unrelated investigations that surface
inconsistencies. A more systematic specification of when external review
should occur, what its scope should be, and what its outputs should
specify for apparatus modification is not currently articulated. The
April 2026 audit was effective; future audits' triggering conditions and
operating procedures are not formalized.

## 5. The Original Trifurcation, Extended

The December 2025 essay articulated three failure types as a taxonomy
analytical work should be alert to. The implementation work that followed
revealed that the three types differ in their detectability properties
and therefore admit different treatment strategies. This is not a
correction to the original taxonomy; it is an extension of it.

The trifurcation specifies what failures can occur. The asymmetry
specifies how each occurs and how each can be caught. Together they
constitute a more complete picture of analytical validation than either
provides alone.

[Closing paragraph connecting this back to the original essay's spirit
— debugging analytical work requires recognizing not just what can go
wrong but what kinds of detection each kind of going-wrong admits.]
