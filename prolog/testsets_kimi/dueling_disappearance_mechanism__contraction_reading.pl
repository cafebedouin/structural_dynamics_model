% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dueling Disappearance via Dignity-Culture Contraction
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The contraction reading of the dueling-disappearance kernel holds that
 *   dueling did not merely decline due to legal prohibition or institutional
 *   substitution, but became culturally unthinkable because dignity-culture
 *   axioms displaced honor-culture axioms entirely. Honor-culture
 *   practitionersâaristocratic males whose identity was fused with the code
 *   of honorâbecame victims of a new cultural substrate that rendered their
 *   framework illegible. The reading claims mountain (irreversible,
 *   naturalized substrate) but the structural data reveal substantial
 *   extraction concentrated on identity-locked honor practitioners, producing
 *   a false-summit signature.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Primary target (moderate/identity_locked) â bears extraction as their cultural framework becomes illegible
 *   - modernizing_elite: Primary beneficiary (powerful/mobile) â gains monopoly on status adjudication and institutional recourse
 *   - historical_sociologists: Analytical observer (analytical/analytical) â sees the displacement from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.71).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.35).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling Disappearance via Dignity-Culture Contraction").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '63add0e5-e5e0-45aa-bd5c-f2e8f8563d81').
narrative_ontology:cs_kernel_codification('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', implicit).
narrative_ontology:cs_authority_grounding('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', expertise).
narrative_ontology:cs_interpretation_layer_present('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81').
narrative_ontology:cs_reading_relation('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', foundational, dignity_culture_irreversibly_subsumes_honor_recourse).
narrative_ontology:cs_axiom_status(dignity_culture_irreversibly_subsumes_honor_recourse, holdable).
narrative_ontology:cs_axiom_grounding('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', dignity_culture_irreversibly_subsumes_honor_recourse, empirically_contingent).
narrative_ontology:cs_axiom('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', secondary, institutional_recourse_obviates_bodily_status_recovery).
narrative_ontology:cs_axiom_status(institutional_recourse_obviates_bodily_status_recovery, holdable).
narrative_ontology:cs_axiom_grounding('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', institutional_recourse_obviates_bodily_status_recovery, instrumental).
narrative_ontology:cs_reference_frame('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', dignity_culture_substrate).
narrative_ontology:cs_drift_state('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', post_victorian_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('63add0e5-e5e0-45aa-bd5c-f2e8f8563d81', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, modernizing_elite).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic and gentry males whose identity and status-recourse were organized around the honor code. Dueling was the thinkable, legitimate mechanism for grievance recovery. As dignity-culture axioms displaced honor culture, their framework became illegibleâdueling was not merely criminalized but rendered absurd. Exit would require dissolving the identity structure itself, which is experienced as existential unthinkability rather than mere legal constraint.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, generational, identity_locked, national).

% Bourgeois, professional, and state actors for whom dignity cultureâinstitutional recourse, psychological self-management, and state monopoly on violenceâwas the native framework. They benefit from the delegitimation of aristocratic honor violence because it transfers status-adjudication monopoly to courts, bureaucracies, and professional credentialing systems they control.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, modernizing_elite, beneficiary,
    powerful, generational, mobile, continental).

% Analyze the transition from honor to dignity culture from an external, comparative perspective. They document the structural costs of displacement but do not bear them; their classifications are part of the engine, not the constraint.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, modernizing_elite).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the honor regime, dueling coordinated credible commitment to status-defense among aristocratic males by providing a scripted, reciprocal, bodily mechanism for grievance resolution that preserved relative rank. Under the contraction reading, this coordination function was not replaced but erased: dignity culture solves status management through institutional and psychological mechanisms that make the prior coordination problem unintelligible.
% TRANSFER_FUNCTION: Social capital, grievance-recourse, and masculine identity-formation were transferred from honor-culture practitioners to the emerging dignity-culture order. The modernizing elite gained monopoly on legitimate status adjudication while honor practitioners lost the cultural vocabulary to claim injury.
% ABSENT_VOICES: Honor-culture practitioners are excluded from post-displacement discourse; their objections are pathologized as anachronism or toxicity. Cross-cultural anthropologists who treat honor cultures as coherent moral systems are marginal in the modern sociological canon that treats dignity culture as progressive teleology.
% DISAPPEARANCE_RATIONALE: From the dignity-culture seat, dueling's disappearance is substrateâif the constraint vanished, the world would remain unchanged because dignity culture is the taken-for-granted background. From the residual honor-culture seat, the disappearance removed the fundamental architecture of masculine status and grievance, which would cause total rearrangement.
% FOUNDING_PROBLEM: Public insult among aristocratic males created a collective-action problem: without a credible, reciprocal, bodily mechanism to restore honor, status hierarchy destabilized and feuds escalated.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists such as Elias and contemporary cultural theorists attest the founding problem's obsolescence from outside the honor-culture beneficiary set. Contemporary honor-culture practitioners no longer exist as a live social formation capable of corroboration, so no contemporaneous external corroborator from within the victim framework remains.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.71, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71) because the displacement imposes severe identity and recourse costs on honor-culture practitioners who cannot exit without self-dissolution. Suppression is moderate-low (0.35) because by the interval end the constraint is internalized rather than actively enforced; the combination of low suppression with high extraction is the mountain-false-summit signature. Accessibility collapse is near-total (0.92) because dueling is not merely illegal but unthinkable. Resistance is negligible (0.12) because the honor framework has been rendered pathological. Theater ratio declines from 0.45 to 0.20 as the constraint naturalizes.
 *
 * PERSPECTIVAL GAP:
 *   The modernizing elite and the honor-culture practitioner compute radically different seats: for the former, dignity culture is the transparent substrate of modernity; for the latter, it is an extractive structure that annihilates their identity. The engine derives this divergence from the identity_locked exit of the victims versus the mobile exit of beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing elite sits near the beneficiary pole (d low) because the constraint subsidizes their cultural hegemony and status-adjudication monopoly. Honor-culture practitioners sit near the full-target pole (d high) because they are identity-locked into a framework the constraint renders obsolete; their spatial scope is national but their exit is fused to a dissolved identity. Historical sociologists are analytical (d neutral).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcredible honor-status recoveryâis dead, and the constraint persists not as a piton (there is an active beneficiary) but as a falsely naturalized mountain. The classification prevents mislabeling the persistence as mere inertia because the modernizing elite continues to benefit from the dignity-culture substrate; it also prevents mislabeling it as pure coordination because honor-culture practitioners are identifiable victims of the displacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_naturalization,
    'Is dignity culture a genuine natural-law-like cultural substrate, or a constructed normative order that benefits the modernizing elite?',
    'Comparative historical anthropology: determine whether all modernizing societies independently converge on dignity-culture axioms, or whether the displacement is path-dependent and tracks the interests of specific beneficiary classes.',
    'If constructed and beneficiary-laden, the constraint reclassifies from mountain to tangled_rope or snare; if genuinely convergent and irreversible, the mountain claim survives but the victim set requires re-theorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether the mountain claim is a false summit generated by naturalization of a constructed order').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression low because the constraint is genuinely internalized (mountain), or because the suppression mechanism shifted from external coercion to internalized identity regulation?',
    'Examine whether honor-culture practitioners experience the constraint even in private, unobserved settings; if the suppression persists without external monitoring, it is internalized and the effective suppression is higher than the structural measure.',
    'If internalized, the constraint''s directionality for honor practitioners rises further, strengthening the snare-like seat classification; if merely absent, the mountain classification gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, conceptual, 'Structural vs internalized suppression mechanism in cultural displacement').

omega_variable(
    extraction_without_active_enforcement,
    'Can a constraint with negligible active enforcement and no centralized agenda-setter still extract at high levels from identity-locked agents?',
    'Cross-reference with interpersonal identity-coordination constraints in the corpus; if high extraction with low enforcement is recurrent for identity_locked exits, the pattern validates the contraction reading''s structural logic.',
    'If validated, the framework must accept ''naturalized extraction'' as a structural possibility distinct from both mountain and snare; if not, the high extractiveness metric is suspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_without_active_enforcement, conceptual, 'Whether naturalized cultural substrates can structurally extract without active enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(duel_tr_t25, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(duel_tr_t50, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(duel_tr_t75, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement(duel_tr_t100, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(duel_be_t25, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(duel_be_t50, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(duel_be_t75, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(duel_be_t100, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(duel_su_t25, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(duel_su_t50, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(duel_su_t75, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(duel_su_t100, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dueling_disappearance_mechanism kernel. The kernel decomposes into three structurally distinct claims: contraction_reading (cultural axiomatic displacement, claimed mountain), institutional_displacement_reading (institutional substitution, likely rope or tangled_rope), and overdetermined_composite_reading (causal overdetermination, type depends on component weights). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked via cs_structure.reading_relations, not network edges, because they are epistemic siblings rather than causal influencers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
