% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the competence_reading of the preparedness_commitment
 *   kernel. It treats disaster-preparedness routinesâdrills, exercises,
 *   after-action reviews, and structured turnover protocolsâas genuine
 *   coordination that maintains adaptive operational capacity across
 *   generations. The kernel is contested: the husk_reading sees the same
 *   routines as memorial performance lacking operational competence, while
 *   the hybrid_reading treats them as a layered system where memorial and
 *   competence elements coexist. This story instantiates ONLY the competence
 *   reading, with metrics authored independently of the claim.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: Primary agenda-setter and secondary beneficiary (institutional/constrained) â designs and administers the exercise regime
 *   - frontline_responders: Primary beneficiary (organized/constrained) â receives transferable competence through drill participation
 *   - disaster_exposed_communities: Diffuse beneficiary (moderate/constrained) â receives protective externality of maintained responder capacity
 *   - training_accreditation_bodies: Secondary beneficiary (institutional/mobile) â certifies competence and gains authority from the system's legitimacy
 *   - competence_theorists: Analytical observer (analytical/analytical) â evaluates whether exercised knowledge produces genuine adaptive capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.2).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '5ab2f4c2-44a1-481c-bd84-1eeffaee61a1').
narrative_ontology:cs_kernel_codification('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', implicit).
narrative_ontology:cs_authority_grounding('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', practice).
narrative_ontology:cs_interpretation_layer_present('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1').
narrative_ontology:cs_reading_relation('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', foundational, live_exercise_over_memorial_preservation).
narrative_ontology:cs_axiom_status(live_exercise_over_memorial_preservation, holdable).
narrative_ontology:cs_axiom_grounding('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', live_exercise_over_memorial_preservation, empirically_contingent).
narrative_ontology:cs_axiom('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', foundational, d5_break_avoidable_through_turnover_protocols).
narrative_ontology:cs_axiom_status(d5_break_avoidable_through_turnover_protocols, holdable).
narrative_ontology:cs_axiom_grounding('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', d5_break_avoidable_through_turnover_protocols, empirically_contingent).
narrative_ontology:cs_reference_frame('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', live_competence_basal_state).
narrative_ontology:cs_drift_state('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', current_operational_period, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5ab2f4c2-44a1-481c-bd84-1eeffaee61a1', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, disaster_exposed_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, training_accreditation_bodies).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, generational_knowledge_transfer_hypothesis).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, operational_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, funds, and administers recurring exercise routines that stress-test decision-making under uncertainty. Validates operational standards and absorbs personnel turnover through structured onboarding. Receives reduced liability exposure and proven incident-response capability in return.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, emergency_management_agencies, beneficiary).

% Participate in drills that simulate novel contingencies and require adaptive judgment. Invest time and physical risk in training, but gain transferable competence, team coordination clarity, and improved survival odds during actual events.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    organized, biographical, constrained, regional).

% Receive the protective externality of maintained operational capacity. Cannot easily exit geographic hazard exposure, so their welfare depends on responder competence surviving generational turnover in nearby agencies.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, disaster_exposed_communities, beneficiary,
    moderate, generational, constrained, local).

% Certify drill curricula and individual responder qualifications. Their authority and revenue flow from the perceived legitimacy of the competence-transfer system; they benefit when the market treats exercised knowledge as the gold standard.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_accreditation_bodies, beneficiary,
    institutional, generational, mobile, national).

% Study whether repeated exercise produces genuinely adaptive decision-making or merely reinforces institutional narrative. Evaluate longitudinal evidence on skill decay across cohort transitions.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, competence_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves operational competence across inevitable personnel turnover and long inter-event intervals by converting institutional memory into exercised, adaptive decision-making capacity rather than inert procedural recall.
% TRANSFER_FUNCTION: Moves experiential knowledge, validated response patterns, and stress-tested judgment from experienced practitioners to incoming personnel through repeated drills and after-action synthesis.
% ABSENT_VOICES: Communities with no recent disaster experience that undervalue preparedness investment; theorists who argue that just-in-time digital knowledge bases or decentralized mutual-aid networks could substitute for centralized drill regimes.
% DISAPPEARANCE_RATIONALE: If the exercised-knowledge routines disappeared overnight, response agencies would lose the mechanism that converts procedural manuals into adaptive capability during generational turnover. Within one turnover cycle, decision-making would degrade to rote recall without judgment, and disaster mortality and institutional failure rates would rise.
% FOUNDING_PROBLEM: Disaster response organizations face inevitable personnel turnover and long inter-event intervals; without active exercise, competence atrophies into dead memorial knowledge that cannot adapt to novel contingencies (the D5 break).
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-sociology research (e.g., Dynes, Quarantelli) and external after-action reviews from major incidents (Katrina, Fukushima) attest that operational failures correlate with competence gaps and memorized-but-untested protocols; these sources are outside the direct beneficiary set of training institutions.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.20) because the constraint's primary effect is coordination surplus: it solves the genuine collective-action problem of competence preservation across long inter-event intervals. Suppression is low (0.10) because participation is driven by net benefit rather than coercion; alternatives (different training methods, digital aids) are not actively suppressed. Theater ratio is very low (0.08) because the drills are authored as testing real decision-making rather than performative ritual. Accessibility collapse is moderate (0.30): once the risk of generational decay is understood, opting out of preparedness exercise becomes less viable, though alternative protocols remain thinkable. Resistance is near-zero (0.05) because the arrangement delivers visible protective value. Temporal measurements show slow, slight increase in extraction and theater over a 40-year generational interval, consistent with normal institutional accretion rather than functional degradation.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading and the husk reading evaluate the same drills from opposite epistemic frames: one sees adaptive judgment under stress, the other sees performative ritual. The engine computes this divergence from structural data (theater ratio, suppression, resistance) rather than from the authored claim. An agenda-setter seated in the competence frame experiences the constraint as rope; an observer seated in the husk frame would compute higher theater and rising extraction, trending toward piton or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are net beneficiaries or symmetrically positioned. Emergency management agencies invest resources but receive liability reduction and proven capability; frontline responders invest time but receive transferable competence; communities receive protection; accreditation bodies receive authority. No stakeholder is structurally targeted for extraction. The low extractiveness combined with beneficiary declarations places directionality near the subsidy end for all parties, producing uniformly low effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâgenerational competence decay during long inter-event intervalsâis still live because personnel turnover and hazard exposure persist. The arrangement has not outlived its function. A mandatrophy-resolved flag would be inappropriate here; instead, the R5 genealogy confirms the problem is corroborated by independent disaster research and post-incident reviews outside the beneficiary set. If the founding problem were dead, the same routines would risk piton classification; the competence reading prevents that mislabeling by demonstrating live coordination value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exercise_fidelity_empirical,
    'Do preparedness drills under this arrangement actually test adaptive decision-making under uncertainty, or have they become scripted performances that protect institutional narrative?',
    'Independent audit of drill design against variance in actual incident profiles; measurement of unscripted decision-branching during exercises.',
    'If drills are scripted, the competence reading is overstated, base_extractiveness and theater_ratio rise, and the constraint migrates toward the husk_reading classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exercise_fidelity_empirical, empirical, 'Whether exercised knowledge retains adaptive decision-making or has become performative.').

omega_variable(
    kernel_reading_contest,
    'Is the preparedness_commitment kernel best interpreted as live competence, memorial husk, or hybrid layering?',
    'Comparative longitudinal analysis of response outcomes across jurisdictions that emphasize exercised knowledge versus those that rely on procedural documentation.',
    'Resolution would select among the three kernel readings; this story''s classification as rope is conditional on the competence reading being structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between competence, husk, and hybrid readings of the same preparedness routines.').

omega_variable(
    generational_transfer_efficacy,
    'Does the generational knowledge-transfer mechanism actually prevent the D5 break, or does competence still decay within one turnover cycle regardless of drill frequency?',
    'Track operational-decision quality metrics across cohort transitions in response agencies.',
    'If decay occurs regardless, the coordination function fails and the constraint is either a scaffold (failed transition) or a piton (theatrical maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transfer_efficacy, empirical, 'Whether generational turnover is successfully absorbed by the training routine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__competence_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__competence_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
