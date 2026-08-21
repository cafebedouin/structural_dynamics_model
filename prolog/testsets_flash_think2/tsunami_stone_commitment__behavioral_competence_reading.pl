% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Commitment: Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes the 'behavioral competence' reading of the
 *   tsunami stone commitment kernel. It posits that ancient stone
 *   inscriptions, coupled with robust intergenerational oral traditions,
 *   actively maintained a behavioral norm of avoiding settlement in
 *   tsunami-prone areas. This reading emphasizes the constraint's continued
 *   functional efficacy and low-cost persistence through cultural
 *   transmission, leading to a classification as a Piton with very low
 *   extraction and high accessibility collapse, reflecting a deeply ingrained
 *   and effective safety protocol.
 *
 * KEY AGENTS:
 *   - coastal_communities: Payer/Beneficiary (moderate/identity_locked) — adhere to norms, gain safety
 *   - elders_and_storytellers: Agenda-setter (organized/identity_locked) — transmit norms, ensure adherence
 *   - future_generations: Payer/Beneficiary (powerless/identity_locked) — inherit norms, gain safety
 *   - disaster_anthropologists: Observer (analytical/analytical) — study efficacy and persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.15).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Commitment: Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, 'a2a28b63-70e9-4199-8e80-62b05dad1bc9').
narrative_ontology:cs_kernel_codification('a2a28b63-70e9-4199-8e80-62b05dad1bc9', fixed_text).
narrative_ontology:cs_authority_grounding('a2a28b63-70e9-4199-8e80-62b05dad1bc9', practice).
narrative_ontology:cs_interpretation_layer_present('a2a28b63-70e9-4199-8e80-62b05dad1bc9').
narrative_ontology:cs_reading_relation('a2a28b63-70e9-4199-8e80-62b05dad1bc9', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('a2a28b63-70e9-4199-8e80-62b05dad1bc9', tsunami_stone_commitment__catastrophe_validation_axis, coexists_with).
narrative_ontology:cs_axiom('a2a28b63-70e9-4199-8e80-62b05dad1bc9', foundational, intergenerational_transmission_ensures_competence).
narrative_ontology:cs_axiom_status(intergenerational_transmission_ensures_competence, holdable).
narrative_ontology:cs_axiom_grounding('a2a28b63-70e9-4199-8e80-62b05dad1bc9', intergenerational_transmission_ensures_competence, empirically_contingent).
narrative_ontology:cs_axiom('a2a28b63-70e9-4199-8e80-62b05dad1bc9', foundational, inscription_carries_active_force).
narrative_ontology:cs_axiom_status(inscription_carries_active_force, holdable).
narrative_ontology:cs_axiom_grounding('a2a28b63-70e9-4199-8e80-62b05dad1bc9', inscription_carries_active_force, conventional).
narrative_ontology:cs_reference_frame('a2a28b63-70e9-4199-8e80-62b05dad1bc9', ancestral_safety_protocol).
narrative_ontology:cs_drift_state('a2a28b63-70e9-4199-8e80-62b05dad1bc9', contemporary_coastal_development_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a2a28b63-70e9-4199-8e80-62b05dad1bc9', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, future_generations).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to the ancestral norm of not building below the tsunami stones, bearing the cost of foregoing prime coastal land for settlement or development. They benefit directly from the safety and continuity of their community.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities, beneficiary).

% Are the primary custodians and transmitters of the oral traditions and historical knowledge associated with the tsunami stones. They actively enforce the norm through education, storytelling, and social guidance, ensuring its intergenerational continuity.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, elders_and_storytellers, agenda_setter,
    organized, generational, identity_locked, local).

% Inherit the behavioral norm and the associated safety. They bear the implicit cost of adhering to the land-use restrictions without having participated in their initial establishment, but benefit from protection against future disasters.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, future_generations, payer,
    powerless, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, future_generations, beneficiary).

% Study the efficacy and persistence of indigenous disaster mitigation strategies, including the tsunami stone commitments. They analyze the cultural mechanisms of norm transmission and their impact on community resilience.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational land-use patterns to prevent settlement in historically tsunami-prone zones, ensuring long-term community safety and survival.
% TRANSFER_FUNCTION: Transfers vital ecological knowledge, behavioral norms, and safety from past generations to future ones, effectively transferring risk avoidance across time.
% ABSENT_VOICES: Developers or economic actors who might prioritize short-term economic gain from coastal land development, or individuals who lack awareness of the historical context and scientific basis for the warnings.
% DISAPPEARANCE_RATIONALE: If the behavioral force of the tsunami stone commitment vanished, coastal communities would gradually resettle in dangerous zones, increasing their vulnerability to future tsunamis and leading to catastrophic loss of life and culture.
% FOUNDING_PROBLEM: Recurrent devastating tsunamis in coastal regions, leading to repeated loss of life and destruction of settlements.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence of past tsunamis, historical records, and contemporary disaster science corroborate the ongoing threat and the efficacy of the ancestral warning system. Independent researchers and disaster relief organizations attest to the problem's continued relevance.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The very low extractiveness (0.05) reflects that the constraint imposes minimal ongoing costs beyond foregoing high-risk land, and no party extracts rents from its operation. Suppression (0.15) is low, representing diffuse social pressure and cultural reinforcement rather than coercive enforcement. Theater ratio (0.10) is low because the constraint is genuinely functional and effective in guiding behavior, not primarily performative. Accessibility collapse (0.85) is high due to the deep cultural embedding of the norm, making alternatives unthinkable. Resistance (0.05) is negligible as the norm is widely accepted. The 'piton' classification, as interpreted for this reading, signifies a highly stabilized and effective coordination mechanism that functions through inertia and cultural transmission, having achieved its primary purpose of ensuring safety without requiring high-cost active maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the coastal communities, the constraint is a vital, low-cost safety mechanism. From an external, purely economic perspective, it might be seen as an inefficient use of coastal resources. This reading emphasizes the internal, cultural perspective of competence and functionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities and future generations are both payers (forego land) and beneficiaries (gain safety), with their directionality leaning towards beneficiary due to the net positive outcome. Elders and storytellers act as agenda-setters, facilitating the norm's transmission without extracting from it. All involved parties are identity-locked, as adherence to the norm is deeply intertwined with their cultural identity and survival.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    piton_functionality_ambiguity,
    'Is the ''live behavioral force'' truly a sign of an actively functioning constraint, or is it the inertia of a piton that has achieved its purpose and now persists with minimal active maintenance?',
    'Longitudinal ethnographic studies observing the explicit efforts required for norm transmission versus the spontaneous adherence, and the community''s adaptive capacity to new coastal pressures.',
    'If it''s truly active, the constraint might be reclassified as a Rope; if it''s primarily inertial, the Piton classification holds, but with a unique profile of high functionality and low theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_functionality_ambiguity, conceptual, 'Ambiguity regarding the active vs. inertial nature of the constraint''s persistence.').

omega_variable(
    commemorative_husk_distinction,
    'To what extent does the ''behavioral competence'' reading genuinely differ from the ''commemorative_husk_reading'' in its empirical consequences?',
    'Comparative analysis of settlement patterns and disaster outcomes in communities with similar stone inscriptions but varying levels of active intergenerational transmission.',
    'If no significant behavioral difference is observed, the ''behavioral competence'' reading''s claim of active force is weakened, potentially collapsing into the ''commemorative_husk_reading'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemorative_husk_distinction, empirical, 'Distinction between active behavioral force and inert symbolism.').

omega_variable(
    catastrophe_validation_relevance,
    'How does the ''catastrophe_validation_axis'' (e.g., the 2011 tsunami) inform or challenge the ''behavioral competence'' reading?',
    'Analysis of post-2011 tsunami community responses and adherence to stone warnings in affected areas, correlating with pre-existing transmission strength.',
    'Strong correlation would empirically validate the behavioral competence; weak correlation would suggest other factors are at play or that the competence was less robust than claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_validation_relevance, empirical, 'Relevance of empirical disaster events to the claimed behavioral competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tsun_tr_t25, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(tsun_tr_t50, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(tsun_tr_t75, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t25, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(tsun_be_t50, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(tsun_be_t75, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(tsun_su_t25, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(tsun_su_t50, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(tsun_su_t75, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 75, 0.15).
narrative_ontology:measurement(tsun_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
