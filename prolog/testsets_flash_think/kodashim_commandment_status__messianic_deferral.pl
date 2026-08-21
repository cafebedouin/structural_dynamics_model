% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious/halakhic_theory
 *
 * SUMMARY:
 *   This constraint describes the status of Kodashim (Temple service
 *   commandments) within a specific reading of Halakhic theory: they are
 *   temporally suspended due to the absence of the Temple, but not obsolete.
 *   Instead, their study and the maintenance of readiness for their future
 *   restoration in the messianic era are considered active forms of
 *   observance. This reading positions the constraint as a 'scaffold' – a
 *   temporary support structure for a future, restored state, with a clear
 *   (though indeterminate) sunset clause.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.6).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.55).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic_theory").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'b1e46106-1416-4257-b7c8-d16d5dcb64ee').
narrative_ontology:cs_kernel_codification('b1e46106-1416-4257-b7c8-d16d5dcb64ee', fixed_text).
narrative_ontology:cs_authority_grounding('b1e46106-1416-4257-b7c8-d16d5dcb64ee', lineage).
narrative_ontology:cs_interpretation_layer_present('b1e46106-1416-4257-b7c8-d16d5dcb64ee').
narrative_ontology:cs_reading_relation('b1e46106-1416-4257-b7c8-d16d5dcb64ee', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('b1e46106-1416-4257-b7c8-d16d5dcb64ee', kodashim_commandment_status__study_as_performance, influences).
narrative_ontology:cs_axiom('b1e46106-1416-4257-b7c8-d16d5dcb64ee', foundational, divine_commandment_eternal).
narrative_ontology:cs_axiom_status(divine_commandment_eternal, holdable).
narrative_ontology:cs_axiom_grounding('b1e46106-1416-4257-b7c8-d16d5dcb64ee', divine_commandment_eternal, theological).
narrative_ontology:cs_axiom('b1e46106-1416-4257-b7c8-d16d5dcb64ee', foundational, messianic_restoration_imminent).
narrative_ontology:cs_axiom_status(messianic_restoration_imminent, holdable).
narrative_ontology:cs_axiom_grounding('b1e46106-1416-4257-b7c8-d16d5dcb64ee', messianic_restoration_imminent, theological).
narrative_ontology:cs_reference_frame('b1e46106-1416-4257-b7c8-d16d5dcb64ee', halakhic_continuity_through_exile).
narrative_ontology:cs_drift_state('b1e46106-1416-4257-b7c8-d16d5dcb64ee', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b1e46106-1416-4257-b7c8-d16d5dcb64ee', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, future_messianic_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_scholars).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, community_members).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_redemption_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, halakhic_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the halakhic tradition, interpreting the status of commandments and guiding the community in their observance. They benefit from the intellectual engagement and the authority derived from preserving the tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, rabbinic_scholars, beneficiary).

% Bear the opportunity costs of prioritizing study and readiness for future ritual over other present-day needs or pursuits. They gain spiritual merit and a sense of communal identity and purpose through this commitment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, community_members, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, community_members, beneficiary).

% The ultimate beneficiaries of the present generation's efforts to maintain readiness for the restoration of the Temple service and its associated commandments. This entity represents the idealized future state.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, future_messianic_community, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, future_messianic_community).

% Represents the collective needs and priorities of the current generation (e.g., material welfare, social justice, secular integration) that may be subordinated or deprioritized in favor of future-oriented ritual preparation. These needs are not actively voiced within the framework of the constraint.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, present_generation_needs).

% Observes the community's practices from an external perspective, often perceiving the opportunity costs and resource allocation without necessarily understanding or valuing the spiritual or messianic justifications.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, secular_society, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual, intellectual, and communal efforts of the Jewish people towards maintaining the knowledge and readiness required for the future restoration of the Temple service and its associated commandments (Kodashim).
% TRANSFER_FUNCTION: Transfers significant intellectual and spiritual resources, time, and communal focus from present-day, potentially more immediate, needs towards the study and anticipation of future ritual practice. It also transfers the burden of maintaining a complex halakhic tradition across generations.
% ABSENT_VOICES: The immediate material and social needs of the present generation are often implicitly subordinated to the long-term messianic vision. Voices advocating for a re-prioritization towards contemporary social welfare or secular integration might be excluded from the core discourse on commandment status.
% DISAPPEARANCE_RATIONALE: If the belief in the messianic deferral and future restoration of Kodashim vanished, the entire framework of traditional Jewish religious life, rabbinic authority, and communal identity would undergo a profound reorientation. The purpose of extensive halakhic study would be fundamentally altered, leading to a rearrangement of educational priorities and spiritual focus.
% FOUNDING_PROBLEM: To preserve the integrity and relevance of divine commandments pertaining to the Temple service (Kodashim) during periods of exile and Temple destruction, ensuring their continuity and readiness for future restoration in the messianic era.
% FOUNDING_PROBLEM_CORROBORATION: The continuous rabbinic tradition, spanning millennia, consistently attests to the ongoing nature of this problem. Historical and sociological analyses of Jewish communities confirm the persistent theological and practical challenge of maintaining these commandments in diaspora, supporting the claim that the founding problem remains live within the community's self-understanding.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) due to the significant opportunity costs borne by community members who dedicate time and resources to study and anticipation, potentially diverting from other needs. Suppression is moderate (0.55) as communal norms and rabbinic authority strongly encourage this orientation, creating social pressure, though not state-level coercion. Theater ratio is low (0.10) because the study is genuinely understood as a vital, functional preparation for a future reality, not mere performance. Accessibility collapse is moderate (0.65) as alternative religious or secular paths exist, but are constrained by the deep-seated communal commitment to this messianic vision. Resistance is low (0.15) as this is an internal religious commitment, not typically met with active internal opposition.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic scholars, as agenda-setters and beneficiaries, perceive this constraint as a vital, continuous link in the chain of tradition, ensuring the spiritual survival and future redemption of the community. Community members, while also beneficiaries of spiritual connection, experience it as a payer, bearing the opportunity costs of prioritizing future-oriented study. Secular society, as an observer, might view the resource allocation as inefficient or irrational, highlighting the gap between internal religious logic and external utilitarian assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The future messianic community and rabbinic scholars are the primary beneficiaries: the former as the ultimate recipients of the preserved tradition, the latter through their role in its maintenance and interpretation. Community members are both beneficiaries (spiritual connection, identity) and payers (opportunity costs). Present-generation needs are effectively victims, as they are subordinated. The constraint's active enforcement comes from communal norms, rabbinic authority, and the deep-seated identity-lock of community members.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy (becoming a piton) because its mandate, though deferred, is actively maintained and considered 'live' through continuous study and anticipation. The 'sunset clause' of the messianic era, while indeterminate, provides a clear future goal that justifies the present-day efforts, preventing the function from atrophying into mere theatrical maintenance. The ongoing intellectual engagement and spiritual commitment ensure it remains a scaffold, actively supporting a transition, rather than an inert relic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'How can the true opportunity cost borne by community members be quantitatively measured, considering both material and non-material (e.g., social integration) dimensions?',
    'Longitudinal sociological studies comparing resource allocation and life outcomes in communities adhering to this reading versus those with alternative orientations, combined with economic modeling of foregone opportunities.',
    'A higher quantified opportunity cost would amplify the constraint''s effective extraction, potentially shifting its classification towards a Snare for community members. A lower cost would reinforce its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantification of the real-world costs of prioritizing future-oriented religious study.').

omega_variable(
    messianic_timing_ambiguity,
    'Does the indeterminate nature of the ''messianic era'' (the sunset clause) undermine the ''scaffold'' classification by making the transition effectively infinite, thus blurring the line with a permanent arrangement?',
    'Theological and philosophical analysis of the concept of ''imminence'' within messianic thought, and its practical impact on communal behavior and resource allocation. If ''imminence'' is purely theoretical with no practical effect, the scaffold claim weakens.',
    'If the sunset is deemed effectively infinite, the ''scaffold'' classification might degrade towards a ''tangled_rope'' or even ''piton'', as the ''temporary'' aspect becomes nominal rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timing_ambiguity, conceptual, 'The impact of indeterminate sunset timing on the ''scaffold'' classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., lack of viable alternative communal structures) or internalized (e.g., deep-seated identity fusion with the tradition making exit unthinkable)?',
    'Post-exit trajectory analysis of individuals who leave the community: if the pressure to conform or the sense of loss persists after structural barriers are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making the constraint more resilient and extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism within the religious community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.1).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.1).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.1).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.1).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.59).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__messianic_deferral, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__messianic_deferral, suppression_requirement, 60, 0.54).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__messianic_deferral, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. Each reading offers a distinct structural interpretation of the commandment's status in the absence of the Temple, with differing implications for present-day practice and resource allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
