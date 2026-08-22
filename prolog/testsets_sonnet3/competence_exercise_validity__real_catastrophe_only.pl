% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Doctrine of Competence Validity
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one reading within a three-way contest over what
 *   validly exercises safety competence: whether simulation counts
 *   (simulation_as_proxy), whether simulation is necessary but requires
 *   continuous refresh (continuous_refresh_hybrid), or — as authored here —
 *   whether only lived real catastrophe truly exercises competence, rendering
 *   simulation structurally insufficient regardless of fidelity. As real
 *   catastrophes grow rarer (a success of system redundancy), the doctrine
 *   increasingly functions to preserve the credentialing monopoly of those
 *   who happened to have lived through an incident, while simulation
 *   investment and simulation-trained staff are perpetually devalued as
 *   unproven. The doctrine's coordination function (preventing false
 *   confidence from classroom-only training) is real but is now substantially
 *   riding alongside extraction: scarce real-incident experience becomes a
 *   rent-generating credential, and the growing rarity of real catastrophes
 *   (the system's success) paradoxically increases the doctrine's gatekeeping
 *   power rather than triggering its revision.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.58).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.51).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Doctrine of Competence Validity").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6').
narrative_ontology:cs_kernel_codification('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', distributed).
narrative_ontology:cs_authority_grounding('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', practice).
narrative_ontology:cs_interpretation_layer_present('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6').
narrative_ontology:cs_reading_relation('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', foundational, authentic_stakes_irreplaceable).
narrative_ontology:cs_axiom_status(authentic_stakes_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', authentic_stakes_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', secondary, simulation_fidelity_asymptotically_insufficient).
narrative_ontology:cs_axiom_status(simulation_fidelity_asymptotically_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', simulation_fidelity_asymptotically_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', pre_simulation_apprenticeship_model).
narrative_ontology:cs_drift_state('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', high_fidelity_simulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('395ecfd7-ea02-4c8d-8ffe-725e6d2c04d6', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, post_incident_review_boards).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_training_programs).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, junior_safety_staff).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, authentic_stakes_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold institutional standing earned through having personally managed a real catastrophe. Their authority to judge who is 'truly competent' rests on the claim that simulation cannot substitute for real exposure. They set promotion and credentialing criteria that privilege real-incident experience over drill performance, and they administer the doctrine that keeps their scarce credential valuable.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, agenda_setter).

% Convene only after real disasters, and their findings carry outsized institutional weight precisely because the doctrine treats real events as the only valid data. Their relevance and budget depend on the continued primacy of post-catastrophe review over simulation-derived findings.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, post_incident_review_boards, beneficiary,
    institutional, generational, constrained, national).

% Spend years drilling in simulators, tabletop exercises, and red-team scenarios, yet under this doctrine remain classified as 'unproven' until they have survived a real catastrophe. This blocks career advancement, denies them credibility in decision-making during actual incidents, and forces them to defer to commanders whose real-event credential may be decades stale.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Invest heavily in high-fidelity simulation infrastructure that the doctrine treats as inherently insufficient regardless of fidelity improvements. Their budgets are perpetually vulnerable to the argument that no amount of simulation spend can substitute for the real thing, undermining the case for continued investment even as simulation quality rises.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_training_programs, payer,
    organized, biographical, constrained, national).

% Enter high-consequence roles with only simulation-based training available to them because real catastrophes are rare by design (the system is built not to fail). They bear the anxiety and liability of being treated as unvalidated even after extensive drilling, and have no path to the credential except waiting for a disaster to occur on their watch.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, junior_safety_staff, payer,
    powerless, immediate, trapped, local).

% Built redundancy and fail-safes precisely so that real catastrophes would be rare. Their success in preventing catastrophe is invisible under this doctrine, which requires catastrophe to validate anyone — their engineering work is structurally excluded from the credentialing conversation even though it is arguably the reason competence has gone untested.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, system_designers, excluded,
    organized, generational, constrained, national).

% Review safety records and credentialing criteria across the industry. They can see that low incident rates are being read as validation of competence when they may equally reflect system redundancy, luck, or reduced exposure — and can compel disclosure of what the safety record actually demonstrates.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, regulatory_auditors, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard for when personnel are 'proven' competent under extreme stress, preventing organizations from certifying untested staff as ready for high-consequence roles based on paperwork or classroom performance alone.
% TRANSFER_FUNCTION: Moves credibility, promotion opportunity, and decision-making authority from operators with only simulation experience to commanders and review boards whose standing derives from having weathered a real catastrophe; also moves training budget away from simulation investment toward waiting-for-the-real-thing postures.
% ABSENT_VOICES: System designers whose redundancy engineering prevented catastrophes are not consulted on competence credentialing, despite their work being the reason real catastrophes are rare; simulation researchers whose fidelity work approaches real-world stress conditions are treated as making an ontologically impossible claim and are rarely invited to defend their methodology.
% DISAPPEARANCE_RATIONALE: If the real-catastrophe-only doctrine vanished, credentialing and promotion would shift toward simulation performance metrics, training budgets would flow to simulation fidelity rather than post-incident review infrastructure, and veteran commanders would lose their principal source of institutional leverage over less-experienced staff.
% FOUNDING_PROBLEM: Early safety-critical industries (aviation, nuclear, emergency response) observed personnel freeze, panic, or make catastrophic errors under real stress despite passing classroom and tabletop assessments — a genuine gap between simulated and lived performance under extreme uncertainty and consequence.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory auditors and simulation researchers attest that high-fidelity simulation (full-motion simulators, live-fire exercises, adversarial red-teaming) has substantially closed the original gap in many domains, and that low real-incident rates increasingly reflect system redundancy rather than untested competence; veteran commanders and review boards, who benefit from the doctrine's persistence, attest the founding problem remains live and that no simulation can replicate authentic mortal stakes.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) and rising: as real incidents become rarer due to redundancy engineering, the doctrine's practical effect shifts from a genuine competence check toward a scarcity-driven credentialing monopoly for veteran commanders. Theater ratio is high and climbing (0.66) because 'waiting for the real thing' produces increasingly performative deference to real-incident experience even as that experience ages and its relevance to current systems and hazards decays. Suppression is moderate (0.51): the doctrine does not physically bar simulation training, but it structurally devalues it, denying simulation-trained staff decision authority and advancement — a suppression of legitimate alternative validation paths rather than of the training itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran incident commanders and post-incident review boards are structural beneficiaries: their institutional standing and authority derive directly from the doctrine's claim that only real catastrophe validates competence, a claim only they satisfy. Frontline operators, simulation programs, and junior safety staff are targets: they bear the cost of perpetual 'unproven' status and diverted investment despite doing everything short of surviving an actual disaster. System designers are excluded from the credentialing conversation entirely, an irony given that their redundancy work is what makes real catastrophes — and thus valid credentialing events, on this reading — increasingly rare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine gap between classroom competence and stress performance) was real and remains partially live, which is why this is not classified as pure snare. But the doctrine's persistence in an era of high-fidelity simulation and declining catastrophe frequency (itself a safety success) suggests the mandate has partially outlived its original function and now operates substantially as gatekeeping. Classifying as tangled_rope rather than snare or mountain preserves the genuine coordination residue (something IS lost when only classroom-trained staff manage real crises) while flagging the asymmetric extraction now riding on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_catastrophe_only_vs_simulation_proxy,
    'Is real-catastrophe-only a structurally correct account of competence validity, or is it a self-serving doctrine maintained by those whose credential depends on scarcity of the qualifying event?',
    'Compare post-incident performance outcomes of simulation-only-trained personnel against real-incident-experienced personnel in matched-severity events, controlling for system redundancy and time-since-last-incident staleness of real-experience credentials.',
    'If simulation-trained personnel perform comparably in matched real events, the doctrine is substantially extractive gatekeeping over a credential that no longer tracks actual competence; if a genuine performance gap persists, the coordination function is real and the classification should weight toward rope rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_catastrophe_only_vs_simulation_proxy, empirical, 'Whether the real-catastrophe-only claim tracks genuine competence gaps or protects a scarce credential.').

omega_variable(
    safety_record_as_proof_vs_luck,
    'Does a low real-incident rate under this doctrine reflect proven organizational competence, or does it reflect system redundancy and luck that has never actually stress-tested the personnel the doctrine claims to validate?',
    'Near-miss and close-call analysis: examine whether near-catastrophes that were contained by redundant systems (not by human competence) are being miscounted as evidence of validated competence.',
    'If near-misses are predominantly resolved by system redundancy rather than human intervention, the doctrine''s own evidentiary basis (rare real catastrophes = validated competence) collapses, since the untested-competence delta this reading predicts would be confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_record_as_proof_vs_luck, empirical, 'Whether rarity of catastrophe is evidence of competence or evidence of untested competence masked by redundancy.').

omega_variable(
    kernel_framing_choice_commentary,
    'Is the credentialing-authority framing (veteran commanders as agenda-setters) the right lens, or is the safety-record-legitimation framing (the doctrine as a story institutions tell regulators about why low incident counts should be trusted) the more consequential one?',
    'Trace which framing regulatory auditors actually rely on when approving safety certifications — the credential-based framing or the aggregate-safety-record framing.',
    'If regulators lean on the aggregate safety-record framing, the extraction is less about individual career gatekeeping and more about institutional legitimacy laundering, which would raise suppression and lower the coordination weight further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_commentary, conceptual, 'Alternative framing of the doctrine''s primary extraction target — individual credentialing vs. institutional legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__real_catastrophe_only, theater_ratio, 8, 0.44).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__real_catastrophe_only, theater_ratio, 16, 0.51).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__real_catastrophe_only, theater_ratio, 24, 0.58).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__real_catastrophe_only, theater_ratio, 32, 0.63).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.66).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(comp_su_t32, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the competence_exercise_validity kernel. simulation_as_proxy authors low extraction (simulation adequately exercises competence, minimal gatekeeping). continuous_refresh_hybrid authors moderate extraction with a stronger coordination residue (repeated drilling is a genuine ongoing coordination cost, not a scarcity play). real_catastrophe_only (this story) authors the highest extraction and theater ratio because it is the reading most structurally dependent on an increasingly rare qualifying event, converting scarcity into gatekeeping power. All three share the same underlying kernel text (what validly exercises competence) but diverge sharply in epsilon and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
