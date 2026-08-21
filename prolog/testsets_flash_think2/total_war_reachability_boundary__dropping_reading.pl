% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Probability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint describes the total war reachability boundary as
 *   maintained by nuclear deterrence, from the 'dropping probability'
 *   reading. This reading posits that while the probability of total war has
 *   decreased since the Cold War, it remains a reachable outcome, and
 *   deterrence functions as a coordination equilibrium (a 'rope'). The
 *   constraint is actively enforced through military readiness and signaling.
 *   The presence of 'victims' (populations under nuclear threat) and 'active
 *   enforcement' on a 'rope' claim is a deliberate tension for the engine to
 *   analyze, reflecting the inherent risks and costs of maintaining this form
 *   of stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.45).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.75).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'be8322a7-dcbe-4571-a5ec-0592dc01b1d4').
narrative_ontology:cs_kernel_codification('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', implicit).
narrative_ontology:cs_authority_grounding('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', practice).
narrative_ontology:cs_interpretation_layer_present('be8322a7-dcbe-4571-a5ec-0592dc01b1d4').
narrative_ontology:cs_reading_relation('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', foundational, mutual_assured_destruction_principle).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_principle, holdable).
narrative_ontology:cs_axiom_grounding('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', mutual_assured_destruction_principle, empirically_contingent).
narrative_ontology:cs_axiom('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', foundational, rational_actor_assumption).
narrative_ontology:cs_axiom_status(rational_actor_assumption, holdable).
narrative_ontology:cs_axiom_grounding('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', rational_actor_assumption, empirically_contingent).
narrative_ontology:cs_reference_frame('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', cold_war_bipolar_deterrence).
narrative_ontology:cs_drift_state('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be8322a7-dcbe-4571-a5ec-0592dc01b1d4', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, international_stability_advocates).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and signal their nuclear arsenals, setting the terms of deterrence. They benefit from the strategic leverage and relative stability deterrence provides, but also bear the immense responsibility and risk of managing these weapons.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the reduced probability of total war and the resulting global stability. However, they also bear the risk of nuclear conflict without having a direct say in its management, and may face pressure to align with nuclear powers.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer).

% Live under the constant, albeit low, existential threat of nuclear annihilation. They bear the ultimate cost if deterrence fails, with no direct agency in the strategic decisions that shape their fate. Their 'payment' is the inherent risk to their lives and future.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, immediate, trapped, global).

% Work to reinforce the deterrence boundary through diplomacy, arms control, and non-proliferation efforts. They benefit from the stability deterrence provides but are acutely aware of its fragility and the risks it entails.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, international_stability_advocates, observer,
    organized, generational, analytical, global).

% Actors who might seek to challenge or exploit the deterrence boundary, potentially through proliferation or brinkmanship. They are excluded from the established deterrence framework but their actions can destabilize it, increasing risk for all.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, rogue_actors, excluded,
    moderate, immediate, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state actors to avoid direct military confrontation that could escalate to total war, by making the costs of such escalation unacceptable for all parties.
% TRANSFER_FUNCTION: Transfers the existential risk of nuclear conflict to global populations, while providing strategic leverage and a fragile form of stability to nuclear powers.
% ABSENT_VOICES: Future generations, who will inherit the nuclear legacy, and populations in non-nuclear states who bear the risk without direct representation in nuclear policy decisions.
% DISAPPEARANCE_RATIONALE: If the deterrence boundary vanished overnight, the probability of total war would immediately increase, leading to widespread instability, rapid proliferation, and a high likelihood of catastrophic conflict. The global strategic landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The existential threat of total war in the nuclear age, requiring a mechanism to prevent its occurrence.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, strategic analysts, UN reports on disarmament, and historical crises (e.g., Cuban Missile Crisis) corroborate the ongoing nature of the problem and the necessity of deterrence.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).
:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate (0.45) because while deterrence provides stability, it does so by imposing an existential risk on populations, which is a form of extraction. `suppression` is high (0.75) as the core function of deterrence is to suppress the option of total war. `theater_ratio` is moderate (0.4) reflecting the blend of real military capability and performative signaling inherent in deterrence. The `claimed_type` is 'rope' as per the prompt, but the structural data (victims, active enforcement) will likely lead to a different computed type for certain seats, highlighting the contested nature of deterrence's classification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, deterrence is a necessary, albeit risky, coordination mechanism (rope). From the perspective of populations under threat, it is a system that extracts existential risk from them, making it feel more like a tangled rope or even a snare. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are beneficiaries and agenda-setters, gaining strategic leverage and stability (low d). Non-nuclear states are also beneficiaries of stability but bear indirect risks (moderate d). Populations under nuclear threat are clear victims, bearing the ultimate cost if deterrence fails (high d). International stability advocates observe and work to reinforce the system, while rogue actors are excluded and challenge the boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_classification_ambiguity,
    'Is nuclear deterrence fundamentally a ''rope'' (coordination equilibrium) or a ''tangled_rope'' (coordination with asymmetric extraction)?',
    'Analysis of the distribution of benefits vs. risks across all stakeholders, particularly the ''populations_under_nuclear_threat''. If the existential risk to populations is deemed a ''cost'' rather than a ''shared burden of coordination'', it shifts towards tangled_rope.',
    'If reclassified as a tangled_rope, it implies a higher effective extraction and a more coercive underlying structure than the ''rope'' claim suggests, particularly for the victim seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_classification_ambiguity, conceptual, 'Ambiguity in deterrence''s classification due to inherent risks to populations.').

omega_variable(
    total_war_reachability_kernel_reading,
    'This constraint is the ''dropping_reading'' of the ''total_war_reachability_boundary'' kernel. How would the classification change under sibling readings?',
    'Comparing the structural properties and metric values of the ''contraction_reading'' (which would likely have lower extractiveness and higher accessibility collapse for total war) and the ''contingent_reachability_reading'' (which might show more dynamic temporal measurements and omegas on technological shifts).',
    'The ''contraction_reading'' would likely classify the boundary as a Mountain or a Rope with lower extraction, while the ''contingent_reachability_reading'' might highlight a Piton or Scaffold if capabilities are seen as atrophied or transitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(total_war_reachability_kernel_reading, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1960, total_war_reachability_boundary__dropping_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(tota_tr_t1975, total_war_reachability_boundary__dropping_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(tota_tr_t1990, total_war_reachability_boundary__dropping_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__dropping_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1960, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(tota_be_t1975, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(tota_be_t1990, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1960, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(tota_su_t1975, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(tota_su_t1990, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
