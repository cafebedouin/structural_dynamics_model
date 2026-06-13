% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'strict pacifist' reading of Article 9 of
 *   the Japanese Constitution, which interprets the textual language 'never
 *   be maintained' as a categorical prohibition on any armed forces,
 *   including those for defensive purposes. Under this reading, Japan's
 *   renunciation of war is absolute, and self-defense must be achieved
 *   through non-military means or reliance on external alliances. This
 *   reading is highly extractive, as it fundamentally limits the state's
 *   security autonomy and creates an identity-locked situation for military
 *   personnel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.85).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.9).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, snare).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '1379754b-f544-4473-a060-804838dd1e06').
narrative_ontology:cs_kernel_codification('1379754b-f544-4473-a060-804838dd1e06', fixed_text).
narrative_ontology:cs_authority_grounding('1379754b-f544-4473-a060-804838dd1e06', lineage).
narrative_ontology:cs_interpretation_layer_present('1379754b-f544-4473-a060-804838dd1e06').
narrative_ontology:cs_reading_relation('1379754b-f544-4473-a060-804838dd1e06', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('1379754b-f544-4473-a060-804838dd1e06', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('1379754b-f544-4473-a060-804838dd1e06', foundational, armed_forces_categorically_prohibited).
narrative_ontology:cs_axiom_status(armed_forces_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('1379754b-f544-4473-a060-804838dd1e06', armed_forces_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('1379754b-f544-4473-a060-804838dd1e06', foundational, war_renunciation_absolute).
narrative_ontology:cs_axiom_status(war_renunciation_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1379754b-f544-4473-a060-804838dd1e06', war_renunciation_absolute, deontological).
narrative_ontology:cs_reference_frame('1379754b-f544-4473-a060-804838dd1e06', post_war_absolute_pacifism).
narrative_ontology:cs_drift_state('1379754b-f544-4473-a060-804838dd1e06', contemporary_geopolitical_shifts, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1379754b-f544-4473-a060-804838dd1e06', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, anti_militarist_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_military_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_industry).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading imposes a severe limitation on national sovereignty regarding defense, forcing dependence on others. Suppression (0.90) is also very high, as this interpretation actively suppresses any domestic efforts to develop a conventional military capacity, relying on constitutional and legal enforcement. Theater ratio (0.10) is low because the constraint is genuinely enforced and its effects are real, not merely performative, though the existence of the Self-Defense Forces creates some performative tension. Accessibility collapse (0.95) is near total for military options, and resistance (0.70) is high due to ongoing political and public debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pacifist advocates, this is a foundational 'rope' that coordinates national identity around peace. From the perspective of the state and military personnel, it is a 'snare' that extracts sovereign defense capacity and creates an existential crisis for their roles. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist advocates and anti-militarist movements are beneficiaries/agenda-setters, as this reading aligns with and empowers their ideological goals. The Japanese state's security autonomy, military personnel, and defense industry are victims/payers, bearing the direct costs of restricted defense capabilities and ambiguous legal status. Allied nations are indirect beneficiaries, gaining influence from Japan's security dependence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the textual language ''never be maintained'' an absolute prohibition on all armed forces, or does it permit a minimum necessary force for self-defense?',
    'Supreme Court ruling explicitly clarifying the scope of ''armed forces'' and ''war renunciation'' in Article 9, or a constitutional amendment.',
    'If interpreted to permit defensive forces, the extractiveness and suppression of this reading would significantly decrease, potentially reclassifying it from a snare to a tangled rope or even a rope, depending on the scope of permitted forces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional text regarding military capacity.').

omega_variable(
    security_autonomy_vs_pacifism,
    'Is the complete renunciation of military capacity a viable and desirable path to national security in the current geopolitical environment, or does it create a security vacuum?',
    'Empirical analysis of Japan''s security outcomes under this reading compared to counterfactuals with a more robust defense, and shifts in public and expert opinion on the efficacy of absolute pacifism.',
    'If deemed unviable, the ''victim'' status of state security autonomy would be amplified, and the ''beneficiary'' status of pacifist advocates would be challenged, potentially shifting the constraint''s overall classification towards a more severe snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_autonomy_vs_pacifism, empirical, 'The practical efficacy and desirability of absolute pacifism for national security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.8).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1980, 0.83).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.85).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.87).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, japan_us_security_alliance).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Article 9 of the Japanese Constitution, each with different structural implications for national security and military policy. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
