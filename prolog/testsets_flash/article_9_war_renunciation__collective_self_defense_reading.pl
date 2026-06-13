% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Japan's Article 9: Collective Self-Defense Interpretation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'collective self-defense' reading of
 *   Japan's Article 9, which interprets the constitutional renunciation of
 *   war as permitting military action to defend allies when Japan's survival
 *   is threatened, even without direct attack on Japan. This reading allows
 *   for overseas deployments and joint operations, expanding the mission
 *   scope of the Self-Defense Forces (SDF). It is a contested interpretation
 *   that has incrementally absorbed geopolitical pressures, leading to an
 *   elastic constraint that balances constitutional text with security
 *   policy.
 *
 * KEY AGENTS:
 *   - japanese_government: Agenda setter (institutional/generational) — interprets and implements Article 9.
 *   - us_military_alliance: Beneficiary (institutional/generational) — benefits from expanded SDF mission scope.
 *   - japanese_taxpayers: Payer (moderate/biographical) — bear the costs of increased defense spending and potential military engagement.
 *   - pacifist_citizens: Payer (powerless/biographical) — bear the cost of perceived constitutional erosion and increased militarization.
 *   - regional_stability_advocates: Victim (organized/generational) — see the expanded interpretation as destabilizing the region.
 *   - constitutional_scholars: Observer (analytical/civilizational) — analyze the legal and historical implications of the interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.65).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.7).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Japan's Article 9: Collective Self-Defense Interpretation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'c625b2b1-8279-4ce6-a64f-32c09e7bfa68').
narrative_ontology:cs_kernel_codification('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', fixed_text).
narrative_ontology:cs_authority_grounding('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', lineage).
narrative_ontology:cs_interpretation_layer_present('c625b2b1-8279-4ce6-a64f-32c09e7bfa68').
narrative_ontology:cs_reading_relation('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', foundational, collective_self_defense_is_inherent).
narrative_ontology:cs_axiom_status(collective_self_defense_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', collective_self_defense_is_inherent, conventional).
narrative_ontology:cs_axiom('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', foundational, survival_threat_justifies_allied_defense).
narrative_ontology:cs_axiom_status(survival_threat_justifies_allied_defense, holdable).
narrative_ontology:cs_axiom_grounding('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', survival_threat_justifies_allied_defense, instrumental).
narrative_ontology:cs_reference_frame('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', post_cold_war_alliance_adaptation).
narrative_ontology:cs_drift_state('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', contemporary_geopolitical_realignment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c625b2b1-8279-4ce6-a64f-32c09e7bfa68', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_military_alliance).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industry).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the expanded interpretation allows for significant resource allocation to defense and potential overseas deployments, shifting costs to taxpayers and potentially involving Japan in conflicts not directly threatening its territory. Suppression (0.70) is high due to the government's active efforts to legitimize this interpretation through legal re-readings and policy changes, often overriding public dissent and constitutional challenges. The theater ratio (0.20) is moderate; while the SDF performs genuine defense functions, a portion of its expanded activities serves to symbolically affirm the collective self-defense doctrine and alliance commitments, rather than direct, immediate defense of Japan. The accessibility collapse (0.40) is moderate, as alternative interpretations (strict pacifism, inherent right) are still debated but face significant institutional barriers. Resistance (0.55) is also moderate, with ongoing public protests and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   The Japanese government and its US military allies perceive this interpretation as a necessary and legitimate adaptation to modern security challenges, ensuring collective security. For pacifist citizens and regional stability advocates, it represents an erosion of constitutional principles and a dangerous shift towards militarization, imposing costs and risks they oppose. The engine will compute different classifications for these seats based on their declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government and the US military alliance are clear beneficiaries, gaining increased security flexibility and alliance strength (low d). Japanese taxpayers and pacifist citizens are payers, bearing the financial and ideological costs (high d). Regional stability advocates are victims, as their preferred stable, non-interventionist framework is undermined (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates collective security with allies (a coordination function) but does so with asymmetric extraction, imposing costs on taxpayers and those who prefer a narrower constitutional interpretation. The 'survival threatened' trigger is elastic, allowing for incremental mission expansion. The classification prevents mislabeling this as a pure Rope (ignoring extraction) or a Snare (ignoring the coordination function with allies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_self_defense_legitimacy,
    'Is the collective self-defense interpretation a legitimate extension of Article 9''s inherent self-defense right, or a reinterpretation driven by geopolitical pressures?',
    'Constitutional court ruling on the scope of ''survival threatened'' and ''collective self-defense'' under Article 9, or a national referendum on constitutional amendment.',
    'If deemed illegitimate, the constraint would revert to a narrower interpretation, limiting overseas deployments and joint operations. If affirmed, it solidifies the expanded mission scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_defense_legitimacy, conceptual, 'Ambiguity of collective self-defense under Article 9.').

omega_variable(
    victim_set_stability,
    'How stable is the victim set, particularly those relying on a narrower interpretation of Article 9 for regional stability?',
    'Longitudinal study of public opinion, regional diplomatic responses, and legal challenges to SDF deployments.',
    'If the victim set expands or becomes more vocal, it could increase resistance and challenge the legitimacy of the collective self-defense reading, potentially shifting the constraint towards a Snare or increasing its theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_stability, empirical, 'Impact of mission expansion on those relying on Article 9''s stability.').

omega_variable(
    kernel_reading_identification,
    'This constraint is a ''collective_self_defense_reading'' of the ''article_9_war_renunciation'' kernel. What structural elements would change if the ''strict_pacifist_reading'' or ''inherent_right_reading'' were adopted?',
    'Analysis of legal precedent, government policy, and public discourse under alternative readings.',
    'The ''strict_pacifist_reading'' would eliminate the Self-Defense Forces entirely, making the constraint a Mountain (physical/logical limit on military action). The ''inherent_right_reading'' would limit military action to direct defense of Japan, reducing extractiveness and suppression by narrowing mission scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural changes under sibling readings of Article 9.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t5, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(arti_tr_t15, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(arti_be_t5, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(arti_su_t5, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_us_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_maritime_security_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'article_9_war_renunciation' kernel. Each reading represents a distinct constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
