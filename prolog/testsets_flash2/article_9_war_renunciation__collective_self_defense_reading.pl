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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9: Collective Self-Defense Interpretation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'collective self-defense' reading of
 *   Japan's Article 9, which interprets the constitutional war-renunciation
 *   clause to permit military action to defend allies even without a direct
 *   attack on Japan, provided Japan's survival is threatened. This reading
 *   expands the mission scope of the Self-Defense Forces, allowing for
 *   overseas deployments and joint operations. It is a contested
 *   interpretation that has evolved over time, absorbing incremental
 *   expansions of military activity while maintaining the facade of
 *   constitutional adherence. The claimed type is 'tangled_rope' because it
 *   serves a coordination function (alliance security) but involves
 *   significant extraction from those who prefer a narrower interpretation.
 *
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
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9: Collective Self-Defense Interpretation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '513a068e-187b-431c-9638-52942c286631').
narrative_ontology:cs_kernel_codification('513a068e-187b-431c-9638-52942c286631', fixed_text).
narrative_ontology:cs_authority_grounding('513a068e-187b-431c-9638-52942c286631', lineage).
narrative_ontology:cs_interpretation_layer_present('513a068e-187b-431c-9638-52942c286631').
narrative_ontology:cs_reading_relation('513a068e-187b-431c-9638-52942c286631', article_9_war_renunciation__strict_pacifist_reading, influences).
narrative_ontology:cs_reading_relation('513a068e-187b-431c-9638-52942c286631', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('513a068e-187b-431c-9638-52942c286631', foundational, collective_self_defense_is_inherent_right).
narrative_ontology:cs_axiom_status(collective_self_defense_is_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('513a068e-187b-431c-9638-52942c286631', collective_self_defense_is_inherent_right, conventional).
narrative_ontology:cs_axiom('513a068e-187b-431c-9638-52942c286631', foundational, threat_to_allies_is_threat_to_japan).
narrative_ontology:cs_axiom_status(threat_to_allies_is_threat_to_japan, holdable).
narrative_ontology:cs_axiom_grounding('513a068e-187b-431c-9638-52942c286631', threat_to_allies_is_threat_to_japan, empirically_contingent).
narrative_ontology:cs_reference_frame('513a068e-187b-431c-9638-52942c286631', post_cold_war_alliance_adaptation).
narrative_ontology:cs_drift_state('513a068e-187b-431c-9638-52942c286631', contemporary_regional_tensions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('513a068e-187b-431c-9638-52942c286631', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_military_alliance).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, japanese_pacifist_public).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_strict_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_self_defense_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit collective self-defense, expanding the scope of military operations and justifying overseas deployments. Benefits from increased strategic flexibility and alliance strength. Faces domestic political resistance but controls legislative agenda.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from Japan's expanded military capabilities and willingness to participate in collective security operations, strengthening regional deterrence and burden-sharing. This interpretation aligns with its strategic interests.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_military_alliance, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the cost of increased military spending, potential involvement in foreign conflicts, and the erosion of a core national identity rooted in pacifism. Expresses resistance through protests and electoral opposition, but lacks direct power to reverse the interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_pacifist_public, payer,
    organized, generational, constrained, national).

% Adheres to a strict textual reading of Article 9, viewing any military capacity beyond minimal self-defense as unconstitutional. Their intellectual authority is challenged by the evolving interpretation, leading to a loss of influence and perceived legitimacy of the constitutional framework.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_strict_interpretation, payer,
    moderate, civilizational, identity_locked, national).

% Benefits from expanded mission scope, increased budget, and greater integration with allied forces. Its operational mandate is directly shaped by this interpretation, allowing for more proactive roles in regional security. Administers the expanded military activities.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_self_defense_forces, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, japanese_self_defense_forces, beneficiary).

% Observe Japan's evolving security posture with mixed reactions: some welcome increased regional stability, others express concern over potential remilitarization. Their strategic calculations are influenced by this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, neighboring_states_regional_security, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Japan to coordinate its defense policy more closely with allies, particularly the US, by permitting military action in response to threats to allied nations, thereby enhancing regional security cooperation.
% TRANSFER_FUNCTION: Transfers strategic flexibility and military burden-sharing capacity from the Japanese public (via constitutional reinterpretation and increased risk) to the ruling coalition and its allies, in exchange for enhanced collective security.
% ABSENT_VOICES: The 'strict pacifist' reading's proponents, who argue for an absolute prohibition on any military force, are largely excluded from the policy-making discourse, their constitutional arguments marginalized by the dominant interpretation.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan's military would revert to a strictly territorial defense posture, severely impacting its alliance commitments and regional security architecture. The US-Japan alliance would require renegotiation, and Japan's strategic influence would diminish, leading to a significant rearrangement of power dynamics in East Asia.
% FOUNDING_PROBLEM: The post-WWII constitutional framework, particularly Article 9, created a tension between Japan's sovereign right to self-defense and its renunciation of war, leading to ambiguity regarding its role in international security.
% FOUNDING_PROBLEM_CORROBORATION: The ruling coalition and US alliance partners attest that the problem of balancing constitutional constraints with modern security needs is live, citing evolving regional threats. Constitutional scholars and pacifist groups argue the problem is manufactured to justify remilitarization, but acknowledge the ongoing debate.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because this interpretation imposes significant costs on the pacifist public and strict constitutionalists, who see it as a betrayal of the original constitutional intent and a move towards remilitarization. Suppression (0.70) is also high, as the ruling coalition actively enforces this interpretation through legislative changes and executive actions, marginalizing dissenting voices and limiting avenues for legal challenge. Theater ratio (0.20) is moderate; while there's genuine strategic coordination, the continuous reinterpretation to fit new military roles without formal amendment introduces an element of performative constitutionalism. The increasing trend in extractiveness and suppression reflects the gradual expansion of military scope and the hardening of the interpretive stance over time.
 *
 * PERSPECTIVAL GAP:
 *   The ruling coalition and US alliance perceive this interpretation as a necessary and legitimate adaptation to modern security challenges, a 'rope' that coordinates defense. Conversely, the pacifist public and strict constitutional scholars experience it as a 'snare' that extracts from their constitutional values and peace dividend. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling coalition and the US military alliance are clear beneficiaries, gaining strategic flexibility and enhanced security cooperation. The Japanese pacifist public and constitutional scholars advocating for a strict interpretation are victims, bearing the costs of increased military engagement and the erosion of constitutional principles. The Japanese Self-Defense Forces benefit from an expanded mandate and resources, acting as both a beneficiary and an agenda-setter in implementing this interpretation. Neighboring states are observers, their positions varying based on their own security interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this as pure coordination (a 'rope') by highlighting the asymmetric extraction from those who adhere to the original constitutional intent. It also avoids mislabeling it as a 'snare' by acknowledging the genuine, albeit contested, coordination function it serves for alliance security. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict further underscore the ongoing tension between the original mandate and the current interpretation, indicating a potential for mandatrophy if the coordination function becomes purely a cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_amendment_necessity,
    'Is the current interpretation of Article 9, allowing collective self-defense, a legitimate constitutional evolution or does it fundamentally require a formal constitutional amendment?',
    'A Supreme Court ruling explicitly addressing the constitutionality of collective self-defense, or a national referendum on amending Article 9.',
    'If an amendment is deemed necessary, the current interpretation would be reclassified as a ''snare'' due to its lack of formal legitimacy. If upheld as legitimate evolution, its ''tangled_rope'' status would be reinforced, but with higher scrutiny on its coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_necessity, conceptual, 'Legitimacy of constitutional reinterpretation versus formal amendment.').

omega_variable(
    threat_perception_objectivity,
    'Is the ''threat to Japan''s survival'' trigger for collective self-defense objectively verifiable, or is it subject to political manipulation and broad interpretation?',
    'Establishment of an independent, non-political body to assess national security threats and their direct impact on Japan''s survival, with transparent criteria.',
    'If the trigger is found to be easily manipulated, the constraint''s extractiveness and suppression would be re-evaluated upwards, potentially shifting it closer to a ''snare'' by exposing the coordination story as cover. If objectively verifiable, the coordination function''s legitimacy would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_objectivity, empirical, 'Objectivity of the ''survival threat'' trigger for military action.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of pacifist voices structural (e.g., media control, legal barriers) or internalized (e.g., self-censorship, social pressure)?',
    'Analysis of public discourse and media freedom indices, combined with sociological studies on civic engagement and dissent in Japan. If suppression persists after formal barriers are removed, it indicates internalized mechanisms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the ''snare'' aspects of the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_us_security_treaty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its interpretation of collective self-defense directly influences the operational scope of the Self-Defense Forces and the stability of the US-Japan security alliance. It coexists with and influences other readings of Article 9.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
