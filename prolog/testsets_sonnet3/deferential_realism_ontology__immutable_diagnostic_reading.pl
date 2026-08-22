% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Typology as Fixed-Referent Diagnostic Instrument
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the immutable-diagnostic reading of the
 *   deferential-realism-ontology kernel: the claim that the constraint
 *   typology functions as a neutral measuring instrument with fixed
 *   referents, such that mountain-hood, snare-hood, and misclassification are
 *   all questions of better observation rather than contested framing. As a
 *   reading in its own right, it is authored here as a tangled rope — it does
 *   real coordination work (a shared adjudication vocabulary across disputing
 *   parties) while also functioning as an extraction mechanism that relocates
 *   normative contest into metric selection controlled by an unaccountable
 *   expert layer. This is distinct from the hybrid_pragmatic_reading (which
 *   concedes a contested periphery) and the rhetorical_scaffold_reading
 *   (which denies discoverability altogether and treats the vocabulary as
 *   frankly persuasive); those are separate constraints with their own
 *   epsilon values, linked here via network.affects_constraints, not folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - framework_engineers: agenda_setter (institutional/arbitrage) — design and control the observable set
 *   - credentialed_classifiers: beneficiary (organized/mobile) — derive professional standing from instrument operation
 *   - institutions_seeking_neutral_cover: beneficiary/agenda_setter (powerful/arbitrage) — use classification outputs to legitimate contested arrangements
 *   - contesting_stakeholders_with_novel_framings: payer (moderate/constrained) — disadvantaged when their claims fall outside approved observables
 *   - movements_relying_on_normative_vocabulary: payer (organized/constrained) — lose rhetorical leverage when declaration is recast as error
 *   - domain_experts_outside_the_measurement_apparatus: excluded (moderate/trapped) — situated knowledge has no standing in the apparatus
 *   - analytical_observers: observer (analytical/analytical) — study the instrument itself as artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.71).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Typology as Fixed-Referent Diagnostic Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '4f3e24de-3e8c-4e34-8e20-63d9ecb57c02').
narrative_ontology:cs_kernel_codification('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', formalized).
narrative_ontology:cs_authority_grounding('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', expertise).
narrative_ontology:cs_interpretation_layer_present('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02').
narrative_ontology:cs_reading_relation('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', foundational, epsilon_is_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_is_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', epsilon_is_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', foundational, misclassification_is_correctable_measurement_error).
narrative_ontology:cs_axiom_status(misclassification_is_correctable_measurement_error, holdable).
narrative_ontology:cs_axiom_grounding('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', misclassification_is_correctable_measurement_error, empirically_contingent).
narrative_ontology:cs_reference_frame('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', observational_instrument_with_fixed_referents).
narrative_ontology:cs_drift_state('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', contemporary_classification_disputes, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4f3e24de-3e8c-4e34-8e20-63d9ecb57c02', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contesting_stakeholders_with_novel_framings).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, movements_relying_on_normative_vocabulary).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, domain_experts_outside_the_measurement_apparatus).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_is_discoverable_not_constructed).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, misclassification_is_measurement_error).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the classification engine, decide which metrics count as observables, and adjudicate disputed classifications by appeal to 'better measurement.' Their authority rests on the claim that epsilon and the other atoms are discoverable facts about the world rather than framing choices they themselves make when selecting what counts as a beneficiary, a victim, or a scope.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers, agenda_setter,
    institutional, generational, arbitrage, global).

% Apply the typology professionally — in policy analysis, institutional audits, academic papers — and derive standing from being the ones who can correctly read the instrument. Their expertise is valuable precisely because the instrument is presented as having fixed referents that require trained observation rather than negotiated judgment.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers, beneficiary,
    organized, biographical, mobile, global).

% Commission or cite classification analyses to legitimate contested arrangements as 'measured to be Rope' or 'measured to be Mountain,' converting what would otherwise be a normative fight over legitimacy into a technical dispute about observation accuracy. This shields the institution's position from having to defend the values embedded in its metric choices.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover, agenda_setter).

% Attempt to argue that a given arrangement is extractive using vocabulary or evidence the instrument's fixed-referent reading does not recognize as a valid observable. Because the reading treats classification disputes as resolvable by 'better observation' within the existing metric set, framings that would require expanding or renegotiating the metric set are treated as noise or error rather than legitimate contest, and are structurally disadvantaged in any dispute the instrument is used to settle.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contesting_stakeholders_with_novel_framings, payer,
    moderate, biographical, constrained, national).

% Historically used terms like 'snare' or 'extraction' as persuasive, declarative moves in political argument — naming a mechanism as illegitimate to mobilize opposition. Under the immutable-diagnostic reading, their usage is recast as a category error unless it can be cashed out in the instrument's approved observables, which forecloses the rhetorical function that made the vocabulary useful to them in the first place.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, movements_relying_on_normative_vocabulary, payer,
    organized, generational, constrained, national).

% Possess situated knowledge about a specific arrangement's history and effects but lack standing within the classification apparatus because their knowledge does not translate into the instrument's declared observables (power atom, exit atom, scope atom, epsilon). Their objections are not represented in the classification process at all.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, domain_experts_outside_the_measurement_apparatus, excluded,
    moderate, biographical, trapped, regional).

% Study the typology itself as a social and epistemic artifact — asking whether treating epsilon as discoverable is itself a defensible metaphysical claim or a convenient posture that forecloses contest over how the metrics were chosen in the first place.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and procedure so that disputes about whether an arrangement is coercive or coordinating can be settled by appeal to a common set of observables rather than by raw political power — genuinely useful when parties disagree about facts rather than values.
% TRANSFER_FUNCTION: Moves the burden of proof in classification disputes onto whichever party can express its claim in the instrument's approved observable vocabulary, and moves interpretive authority from those with situated or normative knowledge to those credentialed to operate the measurement apparatus.
% ABSENT_VOICES: Domain experts with situated historical knowledge, and movements whose critique is fundamentally normative (this mechanism serves illegitimate beneficiaries) rather than metric (this mechanism scores high on epsilon), are structurally unable to contest a classification on the instrument's own terms — they would object that the 'observation' was never neutral, but that objection is exactly the move the immutable-diagnostic reading treats as a category error.
% DISAPPEARANCE_RATIONALE: Framework engineers and institutions relying on the typology as neutral cover would say the world rearranges badly — classification disputes would revert to unmediated power contests with no shared reference point. Contesting stakeholders and normative-vocabulary movements would say the world barely changes, or improves, because the diagnostic framing was itself suppressing the normative contest that determines legitimacy, and its removal would simply surface a fight that was happening anyway under a technical veneer.
% FOUNDING_PROBLEM: Classification of institutional arrangements (is this coordination or extraction?) was previously settled by whoever had rhetorical or political power to declare a mechanism 'necessary' or 'exploitative,' with no shared procedure for adjudicating disagreement or accumulating comparable cases across domains.
% FOUNDING_PROBLEM_CORROBORATION: Framework engineers and credentialed classifiers attest the founding problem (lack of a shared adjudication procedure) remains live and the instrument solves it. Analytical observers, writing from outside both the beneficiary set and the movements that use the vocabulary rhetorically, note that the instrument's own metric-selection process reintroduces exactly the political contest it claims to have resolved — the choice of what counts as an observable is unaudited by any party outside the framework engineers themselves, which is independent corroboration that the founding problem has not been dissolved so much as relocated one level up, into metric selection.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) and suppression (0.71) are both mid-to-high because the reading's core move — treating epsilon as discoverable rather than authored — genuinely does two things at once: it gives disputing parties a shared procedure (coordination) and it forecloses a class of legitimate objection (this classification embeds a value choice about what counts as an observable) by recasting that objection as a category error rather than a contest. Theater ratio rises across the interval (0.18 to 0.42) as the instrument's use in institutional legitimation outpaces its use in genuine dispute resolution — more classification exercises are commissioned to produce a cover verdict than to adjudicate a live disagreement. Accessibility collapse (0.66) reflects that, once an arrangement is 'measured' under this reading, alternative normative framings become very hard to reintroduce into the conversation. Resistance (0.55) is substantial because contesting stakeholders and normative-vocabulary movements actively push back against the diagnostic framing, but their pushback is structurally disadvantaged by the very instrument they are contesting.
 *
 * PERSPECTIVAL GAP:
 *   From the framework-engineer and institutional-beneficiary seats, this reading is close to Rope or even Mountain: a neutral instrument doing needed coordination work, resisted only by parties who want to relitigate settled facts. From the contesting-stakeholder and excluded-expert seats, the same structure computes as Tangled Rope shading toward Snare: real coordination benefit for some, but persistent extraction of interpretive authority from those whose knowledge does not fit the approved observable set, maintained by treating that extraction as measurement error rather than a live dispute over legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework engineers and credentialed classifiers sit near the beneficiary end: they set the terms of the instrument and derive standing or legitimating cover from its outputs, so their directionality is low. Contesting stakeholders and normative-vocabulary movements sit near the target end: the instrument's fixed-referent posture directly disadvantages the kind of claim they need to make, and their exit (arguing outside the instrument's vocabulary entirely) is constrained because doing so forfeits any purchase on institutional decision processes that already defer to the instrument. Domain experts outside the apparatus are trapped rather than merely constrained — they have no channel into the classification process at all, which is why they are marked excluded rather than payer despite bearing real costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unmediated power contests over legitimacy with no shared adjudication procedure — was real. But the immutable-diagnostic reading resolves it by declaring metric selection itself outside the scope of legitimate contest, which quietly imports a substantive answer (epsilon is discovered, not authored) to what remains, structurally, an open question (per the ε-invariance principle itself: the framework's own authoring rules require decomposing contested observables into separate constraints rather than averaging over them). The mandatrophy risk is that the instrument's coordination function (settling factual disputes) is used to launder what is actually a normative victory (settling values disputes) as if it were mere improved observation. This is not resolved within this reading — resolving it requires either the hybrid_pragmatic_reading's concession of a contested periphery or an external audit of metric-selection legitimacy, neither of which this reading's own procedures can generate from inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_discoverable_or_authored,
    'Is epsilon (base extractiveness) a property the classification instrument discovers in the world, or a property the instrument''s designers author through their choice of what counts as a beneficiary, victim, or observable?',
    'Compare classification outcomes across independently constructed observable sets for the same underlying arrangement: if different reasonable metric choices converge on the same epsilon, discoverability is supported; if they diverge substantially and the divergence tracks the metric designer''s institutional position, authorship is supported.',
    'If epsilon is authored rather than discovered, the immutable-diagnostic reading''s central claim collapses into either the hybrid_pragmatic_reading (fixed core, contested periphery) or the rhetorical_scaffold_reading (frankly persuasive vocabulary), and this story''s own claimed_type would likely shift toward snare from the excluded-experts'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discoverable_or_authored, conceptual, 'Whether the reading''s central discoverability claim about epsilon is itself defensible or a framing choice.').

omega_variable(
    committer_kernel_location_of_dispute,
    'Where exactly does the kernel dispute among the three readings (immutable_diagnostic, hybrid_pragmatic, rhetorical_scaffold) live structurally — is it a dispute about the typology''s metaphysics (what mountains and snares ARE), about its epistemic status (how classification disputes get resolved), or about its political function (who benefits from which resolution procedure)?',
    'Trace, across a corpus of contested classifications, whether disputants who reject a given verdict are disputing the metric values (epistemic), the category boundaries (metaphysical), or the standing of the classifier (political) — the dominant dispute type would locate where the kernel actually splits.',
    'If disputes are predominantly political (who gets to classify), the immutable-diagnostic reading''s insistence on discoverability is itself the extractive move; if predominantly epistemic (better data would resolve it), the reading is closer to defensible as stated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_location_of_dispute, conceptual, 'Locating the structural site of disagreement among sibling readings of the deferential_realism_ontology kernel.').

omega_variable(
    metric_selection_accountability,
    'Who, if anyone, holds framework engineers accountable for the choice of which observables count as valid classification evidence, and is that accountability structure itself subject to the typology''s own diagnostic gaze?',
    'Audit whether any classification dispute has ever been resolved by successfully contesting the observable set itself, rather than contesting a value within an already-accepted observable set.',
    'Absence of any such successful contest would corroborate the tangled_rope reading authored here (extraction hidden behind coordination framing); presence of successful metric-set contests would push the reading toward genuine rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_selection_accountability, empirical, 'Whether metric-selection authority is itself accountable or insulated from contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the deferential_realism_ontology kernel. immutable_diagnostic_reading (this file) claims fixed referents and discoverable epsilon; hybrid_pragmatic_reading concedes a contested periphery while preserving a fixed core (mountains, ropes); rhetorical_scaffold_reading denies discoverability entirely and treats classification as frankly persuasive declaration. Each reading is authored as an independently ε-stable constraint per the ε-invariance principle; they are linked here rather than merged because their epsilon values, beneficiary/victim structures, and claimed types differ materially across the three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
