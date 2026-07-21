% ============================================================================
% CONSTRAINT STORY: filter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_filter_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: filter_reading
 *   human_readable: Assertion-as-Sorting-Instrument (Filter Reading)
 *   domain: social epistemology / signaling theory / conflict economics
 *
 * SUMMARY:
 *   The natural-language label 'why does the speaker keep asserting X after
 *   being challenged' covers several structurally distinct claims about what
 *   the assertion is doing. This story instantiates the filter/sorting
 *   reading: the assertion-and-escalation pattern functions as an
 *   audience-partition instrument. Its diagnostic signature is that the SAME
 *   claim is delivered identically across audiences and, critically,
 *   escalates (rather than retreats) specifically under expert challenge —
 *   because the challenge event, not the claim's content, is the informative
 *   signal being processed. This is orthogonal to whether the speaker's
 *   self-model is calibrated, drifted, or absent (the
 *   stance/register/drift/impression-management readings address that
 *   question); the filter reading treats belief-formation as a separate,
 *   decoupled question from the sorting function the assertion performs.
 *
 * KEY AGENTS:
 *   - claim_originator: sets the sorting event in motion by escalating under challenge rather than conceding — collects coalition loyalty regardless of claim truth-value
 *   - inner_circle_deferential_nodes: primary beneficiaries of the sort — rewarded with proximity and status for signaling deference at the moment of challenge
 *   - expert_challengers: bear the cost of performing the sorting function — reputational damage for having tested the claim on its merits
 *   - social_epistemology_observers: analytical seat that sees the escalation-under-challenge pattern across many instances and can separate the sorting function from the belief-formation question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(filter_reading, 0.68).
domain_priors:suppression_score(filter_reading, 0.71).
domain_priors:theater_ratio(filter_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(filter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(filter_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(filter_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(filter_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(filter_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(filter_reading, tangled_rope).
narrative_ontology:human_readable(filter_reading, "Assertion-as-Sorting-Instrument (Filter Reading)").
narrative_ontology:topic_domain(filter_reading, "social epistemology / signaling theory / conflict economics").

domain_priors:requires_active_enforcement(filter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(filter_reading, '223d9158-119a-441f-a27f-38902dc834a7').
narrative_ontology:cs_kernel_codification('223d9158-119a-441f-a27f-38902dc834a7', distributed).
narrative_ontology:cs_authority_grounding('223d9158-119a-441f-a27f-38902dc834a7', distributed).
narrative_ontology:cs_reading_relation('223d9158-119a-441f-a27f-38902dc834a7', unsettled_claim_ontology__stance_reading, coexists_with).
narrative_ontology:cs_reading_relation('223d9158-119a-441f-a27f-38902dc834a7', unsettled_claim_ontology__register_reading, coexists_with).
narrative_ontology:cs_reading_relation('223d9158-119a-441f-a27f-38902dc834a7', unsettled_claim_ontology__drift_reading, influences).
narrative_ontology:cs_reading_relation('223d9158-119a-441f-a27f-38902dc834a7', unsettled_claim_ontology__impression_management_reading, coexists_with).
narrative_ontology:cs_axiom('223d9158-119a-441f-a27f-38902dc834a7', foundational, truth_indifference_is_instrumental_not_diagnostic).
narrative_ontology:cs_axiom_status(truth_indifference_is_instrumental_not_diagnostic, holdable).
narrative_ontology:cs_axiom_grounding('223d9158-119a-441f-a27f-38902dc834a7', truth_indifference_is_instrumental_not_diagnostic, empirically_contingent).
narrative_ontology:cs_axiom('223d9158-119a-441f-a27f-38902dc834a7', foundational, challenge_event_is_the_informative_signal_not_claim_content).
narrative_ontology:cs_axiom_status(challenge_event_is_the_informative_signal_not_claim_content, holdable).
narrative_ontology:cs_axiom_grounding('223d9158-119a-441f-a27f-38902dc834a7', challenge_event_is_the_informative_signal_not_claim_content, empirically_contingent).
narrative_ontology:cs_reference_frame('223d9158-119a-441f-a27f-38902dc834a7', calibration_transparency_norm).
narrative_ontology:cs_drift_state('223d9158-119a-441f-a27f-38902dc834a7', contemporary_high_challenge_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('223d9158-119a-441f-a27f-38902dc834a7', '').
narrative_ontology:cs_kernel_id(filter_reading, unsettled_claim_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(filter_reading, claim_originator).
narrative_ontology:constraint_beneficiary(filter_reading, inner_circle_deferential_nodes).
narrative_ontology:constraint_victim(filter_reading, expert_challengers).
narrative_ontology:constraint_victim(filter_reading, peripheral_audience_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts a claim and, when challenged, escalates rather than retreats — not because the claim is believed with higher confidence, but because the escalation event is what sorts the audience. Those who defer under pressure are identified as reliable in-group nodes; those who press for evidence are identified as out-group. The originator collects loyalty, attention, and coalition membership from the sorting outcome regardless of whether the underlying claim is true.
narrative_ontology:constraint_stakeholder(filter_reading, claim_originator, agenda_setter,
    powerful, biographical, arbitrage, national).

% Signal deference at the moment of challenge and are rewarded with continued proximity, status, and information access inside the coalition. Their material position improves precisely because they declined to test the claim; leaving the deferential posture risks expulsion from the sorted-in group.
narrative_ontology:constraint_stakeholder(filter_reading, inner_circle_deferential_nodes, beneficiary,
    organized, biographical, constrained, regional).

% Press the claim on evidentiary grounds and are met with escalation rather than concession — the escalation itself functions to expel them from the coalition rather than to answer them. They bear reputational cost (branded hostile or bad-faith) for having performed the sorting function the constraint depends on; they can exit the interaction but the sorting outcome (their exclusion) still registers publicly.
narrative_ontology:constraint_stakeholder(filter_reading, expert_challengers, payer,
    moderate, biographical, mobile, national).

% Watch the challenge-and-escalation exchange without direct stake in it, but are implicitly sorted by their own reaction — silence or agreement reads as deference, questioning reads as defection. They have no forum in which the sorting mechanism itself is up for debate; their only choices are the two outcomes the mechanism recognizes.
narrative_ontology:constraint_stakeholder(filter_reading, peripheral_audience_members, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(filter_reading, peripheral_audience_members, excluded).

% Would argue that the claim's persistence under challenge is evidence about the speaker's confidence or self-model — the standard calibration frame. That frame is structurally excluded from the interaction itself: the sorting function operates whether or not anyone present is running a calibration analysis, and the coalition has no incentive to entertain it.
narrative_ontology:constraint_stakeholder(filter_reading, belief_calibration_theorists, excluded,
    analytical, generational, analytical, national).

% Study the pattern across many instances: identical claims delivered without variation across audiences, escalation rather than retreat specifically under expert challenge, and stable audience partition outcomes. They can see that truth-value of the claim is orthogonal to its function and that the belief-formation question is a different, decoupled question from the sorting question.
narrative_ontology:constraint_stakeholder(filter_reading, social_epistemology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(filter_reading, claim_originator).
narrative_ontology:fixing_cost_class(filter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-cost, repeatable signal that lets a coalition rapidly distinguish members who will hold the line under external pressure from those who will not — a genuine coordination problem (who can be trusted to not defect under challenge) is being solved.
% TRANSFER_FUNCTION: Moves loyalty, attention, and coalition status toward deferential nodes and away from challengers; moves reputational cost onto challengers regardless of the claim's evidentiary merit; moves the burden of resolving the object-level truth question off the table entirely.
% ABSENT_VOICES: Belief-calibration theorists and anyone trying to litigate the claim on evidentiary terms are structurally absent from the mechanism's own logic — the sorting function has no slot for 'the claim was actually checked and found true or false.' Peripheral audience members have no channel to object to being sorted at all.
% DISAPPEARANCE_RATIONALE: If the sorting function vanished, challenges would have to be met with either evidence or concession rather than escalation; coalition membership would have to be established through some other costly signal; the audience partition into deferential/non-deferential nodes would dissolve and re-form around whatever new sorting mechanism emerged, materially changing who holds proximity to the originator.
% FOUNDING_PROBLEM: Coalitions facing external pressure need a cheap, fast way to identify which members will hold firm and which will break — verifying loyalty through costly, slow means (track record, tested commitment) is expensive, so an assertion-and-escalation ritual substitutes as a rapid proxy test.
% FOUNDING_PROBLEM_CORROBORATION: Expert challengers and social_epistemology_observers, both outside the beneficiary set, corroborate that the pattern persists specifically under expert-level challenge (rather than softening) — the escalation-under-challenge signature that a calibration-only account would not predict. No corroboration is offered by claim_originator or inner_circle_deferential_nodes, who have no incentive to describe the mechanism this way.
narrative_ontology:disappearance_verdict(filter_reading, world_rearranges).
narrative_ontology:founding_problem_status(filter_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(filter_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(filter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(filter_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(filter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(filter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(filter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-not-extreme level (0.68 at interval end) because the mechanism does perform a real coordination function (identifying reliable coalition members cheaply) alongside the extraction (reputational cost imposed on challengers, attentional/status capture by the originator). Suppression is high (0.71) because the mechanism's persistence depends on escalation actively punishing dissent rather than merely being unconvincing — the cost imposed on expert_challengers is not incidental, it is the enforcement mechanism that keeps the sort meaningful. Theater ratio is moderate (0.42): some of what looks like defensive argumentation is genuinely functional to the sort (a low-cost signal must be visibly costly to discriminate), while a rising share over time is pure performance as the pattern becomes ritualized. All three metrics share one time grid across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From claim_originator's seat, the pattern looks like coordination: a fast, cheap way to know who can be trusted under pressure — arguably rope-like. From expert_challengers' seat, the identical structure is extraction: they performed a real epistemic service (testing a claim) and were punished for it via reputational cost and coalition exclusion, which looks like tangled_rope or snare depending on how total the exclusion is. This divergence is exactly the seat-relative computation the engine is built to surface — the filter reading does not resolve it in either direction; it names the mechanism as one that generates this divergence structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   claim_originator sits near the beneficiary end: institutional/organized power, arbitrage-grade exit (can walk from any single interaction having already collected the sort), and no cost borne from being wrong about the object-level claim. inner_circle_deferential_nodes are secondary beneficiaries — they pay a small autonomy cost (declining to test the claim) for a status gain. expert_challengers are the clearest targets: moderate power, mobile exit from the interaction itself, but the reputational cost of having triggered the sort follows them regardless of exit. peripheral_audience_members are powerless and constrained — they are sorted without ever being asked whether they consent to the sorting mechanism operating on them at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cheap loyalty verification under external pressure) remains live — coalitions genuinely need fast trust signals, so this does not read as pure mandatrophy/zombie persistence. But the corroboration is asymmetric: only outside observers (expert_challengers, social_epistemology_observers) attest to the escalation-under-challenge signature; the beneficiary set has no incentive to describe the mechanism this way at all, since doing so would name the extraction. This keeps founding_problem_status at live rather than dead, but flags the self-reinforcing risk that the sorting function could persist as ritual long after any genuine loyalty-verification need has passed, without the beneficiary seats ever surfacing that shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sorting_vs_stance_underdetermination,
    'Given a single observed instance of escalation-under-challenge, can the filter (sorting) reading be distinguished from the stance reading (genuine rigid epistemic commitment) using only the behavior at that instance, or does distinguishing them require a track record across varied audiences?',
    'Compare escalation intensity across audience composition: the filter reading predicts escalation scales with the SORTING VALUE of the audience (does this challenge threaten coalition cohesion) rather than with the challenger''s evidentiary quality; the stance reading predicts escalation intensity is invariant to audience composition and tracks only challenge quality. Requires multi-instance data per speaker.',
    'If escalation intensity tracks audience sorting-value rather than challenge quality, the filter reading is supported and the stance reading is not needed to explain the same data; if escalation tracks challenge quality regardless of audience, the filter reading loses its distinguishing evidence and collapses toward the stance reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sorting_vs_stance_underdetermination, empirical, 'Whether filter and stance readings are behaviorally distinguishable from single-instance data.').

omega_variable(
    which_reading_is_dominant_vs_coextensive,
    'Is the filter reading the SOLE mechanism operating, or does it coexist with stance, register, drift, or impression-management mechanisms operating simultaneously in the same claim-assertion events, with filter merely dominant?',
    'This is inherently a framing question about how many distinct causal mechanisms to posit for one observed behavior pattern, not resolvable by a single decisive experiment — different observers reasonably attribute different weights to co-occurring explanations.',
    'If filter is sole, the sibling readings are simply wrong for this phenomenon class. If coextensive, all five readings are partially true simultaneously and the network of sibling constraints models five real, overlapping mechanisms rather than five competing hypotheses about one mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_dominant_vs_coextensive, conceptual, 'Whether the kernel''s readings are mutually exclusive hypotheses or overlapping partial mechanisms.').

omega_variable(
    coalition_benefit_durability,
    'Do inner_circle_deferential_nodes actually receive durable material benefit from their deference, or is the benefit illusory/short-lived, making them misclassified beneficiaries who are actually delayed victims?',
    'Longitudinal tracking of deferential-node outcomes (status, resource access, continued coalition membership) against non-deferential departures from the same coalition over multi-year horizons.',
    'If deferential-node benefit proves durable, the tangled_rope classification (genuine coordination benefit for some) holds. If the benefit is illusory or the coalition eventually extracts from deferential nodes too, the constraint is better classified as snare with a temporarily-deceived beneficiary class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_benefit_durability, empirical, 'Whether coalition membership benefit to deferential nodes is durable or illusory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(filter_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(filt_tr_t0, filter_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(filt_tr_t0, observed).
narrative_ontology:measurement(filt_tr_t4, filter_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(filt_tr_t4, observed).
narrative_ontology:measurement(filt_tr_t8, filter_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(filt_tr_t8, observed).
narrative_ontology:measurement(filt_tr_t12, filter_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(filt_tr_t12, observed).
narrative_ontology:measurement(filt_tr_t16, filter_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(filt_tr_t16, observed).
narrative_ontology:measurement(filt_tr_t20, filter_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(filt_tr_t20, observed).
narrative_ontology:measurement(filt_tr_t24, filter_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(filt_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(filt_be_t0, filter_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(filt_be_t0, observed).
narrative_ontology:measurement(filt_be_t4, filter_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(filt_be_t4, observed).
narrative_ontology:measurement(filt_be_t8, filter_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(filt_be_t8, observed).
narrative_ontology:measurement(filt_be_t12, filter_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(filt_be_t12, observed).
narrative_ontology:measurement(filt_be_t16, filter_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(filt_be_t16, observed).
narrative_ontology:measurement(filt_be_t20, filter_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(filt_be_t20, observed).
narrative_ontology:measurement(filt_be_t24, filter_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(filt_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(filt_su_t0, filter_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(filt_su_t0, observed).
narrative_ontology:measurement(filt_su_t4, filter_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement_basis(filt_su_t4, observed).
narrative_ontology:measurement(filt_su_t8, filter_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(filt_su_t8, observed).
narrative_ontology:measurement(filt_su_t12, filter_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(filt_su_t12, observed).
narrative_ontology:measurement(filt_su_t16, filter_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(filt_su_t16, observed).
narrative_ontology:measurement(filt_su_t20, filter_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(filt_su_t20, observed).
narrative_ontology:measurement(filt_su_t24, filter_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(filt_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(filter_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(filter_reading, 0.08).
narrative_ontology:affects_constraint(filter_reading, stance_reading).
narrative_ontology:affects_constraint(filter_reading, register_reading).
narrative_ontology:affects_constraint(filter_reading, drift_reading).
narrative_ontology:affects_constraint(filter_reading, impression_management_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language label 'why does a speaker keep asserting X after being challenged' into structurally distinct mechanisms per the ε-invariance principle. filter_reading claims the assertion functions as an audience-sorting instrument; stance_reading claims it tracks genuine (possibly rigid) epistemic commitment; register_reading claims it is context-calibrated performance; drift_reading claims it reflects uncorrected belief drift; impression_management_reading claims it optimizes observer impressions of competence. Each sibling carries its own ε, beneficiary/victim structure, and claimed_type — they are linked here rather than merged because measuring the same surface behavior under different causal attributions yields different extraction profiles (filter_reading's ε is driven by sorting-cost imposed on challengers; stance_reading's would be driven by miscalibration cost; register_reading's by audience-manipulation cost; etc.). Do not average across siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
