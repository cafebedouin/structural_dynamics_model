% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Cognitive Inconceivability of Dueling (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'contraction_reading' of the
 *   honor_settlement_legitimacy kernel, asserting that dueling became
 *   cognitively unthinkable through a profound cultural framework
 *   transformation. It is no longer merely prohibited but has exited the
 *   normative possibility space. The metrics reflect this: extremely low
 *   extractiveness, suppression, and theater, coupled with very high
 *   accessibility collapse and negligible resistance, consistent with a
 *   naturalized social fact. The claimed type is 'mountain' to reflect this
 *   deep, almost irreversible cultural shift, triggering False Summit
 *   Mountain detection due to declared beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Cognitive Inconceivability of Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '036b4db7-8222-4011-9b03-ddec2e98ff6f').
narrative_ontology:cs_kernel_codification('036b4db7-8222-4011-9b03-ddec2e98ff6f', implicit).
narrative_ontology:cs_authority_grounding('036b4db7-8222-4011-9b03-ddec2e98ff6f', practice).
narrative_ontology:cs_interpretation_layer_present('036b4db7-8222-4011-9b03-ddec2e98ff6f').
narrative_ontology:cs_reading_relation('036b4db7-8222-4011-9b03-ddec2e98ff6f', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('036b4db7-8222-4011-9b03-ddec2e98ff6f', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('036b4db7-8222-4011-9b03-ddec2e98ff6f', foundational, honor_settlement_is_state_monopoly).
narrative_ontology:cs_axiom_status(honor_settlement_is_state_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('036b4db7-8222-4011-9b03-ddec2e98ff6f', honor_settlement_is_state_monopoly, conventional).
narrative_ontology:cs_axiom('036b4db7-8222-4011-9b03-ddec2e98ff6f', foundational, private_violence_is_illegitimate).
narrative_ontology:cs_axiom_status(private_violence_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('036b4db7-8222-4011-9b03-ddec2e98ff6f', private_violence_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('036b4db7-8222-4011-9b03-ddec2e98ff6f', post_enlightenment_civility).
narrative_ontology:cs_drift_state('036b4db7-8222-4011-9b03-ddec2e98ff6f', historical_peak_of_dueling, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('036b4db7-8222-4011-9b03-ddec2e98ff6f', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of dueling as a legitimate means of dispute resolution, leading to reduced violence and greater social stability. The cultural shift is deeply internalized, making dueling unthinkable.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, society_at_large, beneficiary,
    moderate, generational, mobile, global).

% No longer burdened with prosecuting dueling or adjudicating honor disputes through violent means. The cultural transformation reinforces the state's monopoly on legitimate violence.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_system, beneficiary,
    institutional, civilizational, analytical, national).

% Individuals whose social identity and means of dispute resolution were tied to honor culture now find their practices culturally illegitimate and unthinkable. Their worldview is no longer viable within the dominant normative framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, former_honor_culture_adherents, excluded,
    powerless, biographical, identity_locked, local).

% Analyze the historical processes and cultural mechanisms through which dueling became cognitively unthinkable, documenting the transformation of honor settlement norms.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, non-violent means of resolving disputes and maintaining social order by making dueling culturally illegitimate and unthinkable.
% TRANSFER_FUNCTION: Transferred the burden of honor settlement from individual violent confrontation to internalized social norms and legal processes, effectively eliminating the practice.
% ABSENT_VOICES: Those who might still cling to a romanticized notion of honor dueling are culturally marginalized to the point of irrelevance; their perspective is no longer part of the legitimate discourse on dispute resolution.
% DISAPPEARANCE_RATIONALE: If the cognitive inconceivability of dueling vanished overnight, the world would remain unchanged because dueling is already not a live option for honor settlement. The cultural framework has so thoroughly transformed that its 'disappearance' would simply mean the absence of a non-existent practice.
% FOUNDING_PROBLEM: The problem of violent, often fatal, private disputes over honor that undermined state authority, social stability, and public order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists attest to the decline of dueling and the profound shift in social norms, supported by historical legal records, sociological analyses, and literary evidence of changing cultural values. No significant contemporary voices advocate for dueling as a legitimate practice.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness and suppression scores reflect that the constraint's persistence is due to internalized cultural norms rather than active coercion or rent-seeking. The high accessibility_collapse and low resistance indicate that dueling is no longer a viable or even conceivable option for honor settlement. The 'emerges_naturally: true' for a claimed mountain is justified by interpreting the cultural transformation as a sociological process of naturalization, where the new framework becomes a fixed feature of social reality. The measurement series show a decline in active enforcement and extraction as the cultural shift solidified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the transformed society, the constraint is a natural and beneficial aspect of social order. From the historical perspective of those who once adhered to honor culture, it represents a loss of a legitimate social practice, though this perspective is now largely marginalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and the legal system are beneficiaries, as they gain from the absence of violent honor disputes. Former adherents of honor culture are excluded, as their traditional means of resolving disputes are no longer culturally legitimate. The constraint's operation is so deeply embedded that it primarily benefits the collective through the absence of a problematic practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_naturalization_ambiguity,
    'Is the cognitive inconceivability of dueling a genuine ''natural law'' of social evolution, or a constructed cultural framework that benefits identifiable agents?',
    'Comparative historical analysis of other honor cultures that did not undergo similar transformations, or counterfactual sociological modeling to identify contingent factors.',
    'If genuinely natural, the classification as Mountain holds. If constructed, it would be reclassified as a Piton or Rope, reflecting its social origin and persistence through inertia or coordination, and the FSM trigger would be confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_naturalization_ambiguity, conceptual, 'Ambiguity between naturalized cultural fact and social construct.').

omega_variable(
    mechanism_of_unthinkability,
    'What were the precise cognitive and social mechanisms that rendered dueling ''unthinkable'' rather than merely ''prohibited''?',
    'Detailed historical-sociological studies focusing on changes in moral psychology, social scripts, and the emotional economy of honor.',
    'A clearer understanding of the mechanism would refine the ''accessibility_collapse'' metric and potentially differentiate this reading more sharply from the ''drop_reading'' by identifying the specific points of cultural transformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_of_unthinkability, empirical, 'The specific mechanisms of cultural framework transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1740, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1740, 0.03).
narrative_ontology:measurement(hono_tr_t1780, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1780, 0.02).
narrative_ontology:measurement(hono_tr_t1820, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1820, 0.01).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1860, 0.01).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.01).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1740, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1740, 0.1).
narrative_ontology:measurement(hono_be_t1780, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1780, 0.08).
narrative_ontology:measurement(hono_be_t1820, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1820, 0.06).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1860, 0.05).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(hono_su_t1740, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1740, 0.15).
narrative_ontology:measurement(hono_su_t1780, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hono_su_t1820, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1820, 0.08).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1860, 0.06).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
