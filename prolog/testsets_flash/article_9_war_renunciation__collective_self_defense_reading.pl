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
 *   Japan's Article 9, which interprets the constitutional renunciation of
 *   war to permit military action to defend allies even without direct attack
 *   on Japan, provided Japan's survival is threatened. This reading expands
 *   the mission scope of Japan's Self-Defense Forces, allowing for overseas
 *   deployments and joint operations. It is a contested interpretation, with
 *   significant political and social resistance, but has been incrementally
 *   adopted by the Japanese government to adapt to evolving security
 *   environments and alliance commitments. The constraint is claimed as a
 *   Rope by its proponents (a necessary adaptation for collective security)
 *   but operates with substantial extraction and suppression, making it a
 *   Tangled Rope in practice.
 *
 * KEY AGENTS:
 *   - japanese_government: Agenda-setter (institutional/generational) — drives the interpretation and benefits from enhanced security.
 *   - us_military_alliance: Beneficiary (institutional/generational) — benefits from Japan's expanded military role.
 *   - strict_pacifist_advocates: Payer (organized/generational) — bears the cost of constitutional erosion and militarization, identity-locked to a strict reading.
 *   - inherent_right_advocates: Payer (moderate/biographical) — bears the cost of mission creep beyond direct self-defense.
 *   - taxpayers: Payer (powerless/immediate) — bear financial costs of increased defense spending.
 *   - regional_stability_advocates: Payer (organized/generational) — bear the cost of increased regional tensions.
 *   - constitutional_scholars: Observer (analytical/civilizational) — analyze legal implications.
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
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'b81b907f-4d94-46af-b391-4ccd8bff18d5').
narrative_ontology:cs_kernel_codification('b81b907f-4d94-46af-b391-4ccd8bff18d5', fixed_text).
narrative_ontology:cs_authority_grounding('b81b907f-4d94-46af-b391-4ccd8bff18d5', lineage).
narrative_ontology:cs_interpretation_layer_present('b81b907f-4d94-46af-b391-4ccd8bff18d5').
narrative_ontology:cs_reading_relation('b81b907f-4d94-46af-b391-4ccd8bff18d5', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('b81b907f-4d94-46af-b391-4ccd8bff18d5', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('b81b907f-4d94-46af-b391-4ccd8bff18d5', foundational, collective_self_defense_is_inherent_right).
narrative_ontology:cs_axiom_status(collective_self_defense_is_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('b81b907f-4d94-46af-b391-4ccd8bff18d5', collective_self_defense_is_inherent_right, conventional).
narrative_ontology:cs_axiom('b81b907f-4d94-46af-b391-4ccd8bff18d5', foundational, survival_threat_justifies_alliance_action).
narrative_ontology:cs_axiom_status(survival_threat_justifies_alliance_action, holdable).
narrative_ontology:cs_axiom_grounding('b81b907f-4d94-46af-b391-4ccd8bff18d5', survival_threat_justifies_alliance_action, instrumental).
narrative_ontology:cs_reference_frame('b81b907f-4d94-46af-b391-4ccd8bff18d5', post_wwii_constitutional_order).
narrative_ontology:cs_drift_state('b81b907f-4d94-46af-b391-4ccd8bff18d5', contemporary_geopolitical_realities, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b81b907f-4d94-46af-b391-4ccd8bff18d5', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_military_alliance).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, inherent_right_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit collective self-defense, expanding the scope of military operations and justifying increased defense spending. Benefits from enhanced security posture and alliance commitments, but faces domestic political resistance.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from Japan's expanded military capabilities and willingness to participate in collective security operations, strengthening regional deterrence and burden-sharing. Exerts diplomatic pressure for this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_military_alliance, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of increased militarization and the perceived erosion of Japan's pacifist identity. They are deeply committed to a literal reading of Article 9 and view any military action as a violation. Their resistance is primarily political and ideological.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_advocates, payer,
    organized, generational, identity_locked, national).

% While accepting a limited right to self-defense, they view the collective self-defense interpretation as an overreach that destabilizes the constitutional order and risks entanglement in foreign conflicts. They bear the cost of this expanded mission scope.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, inherent_right_advocates, payer,
    moderate, biographical, constrained, national).

% Bear the financial cost of increased defense spending and potential military deployments. Their ability to influence policy is diffuse and limited.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, taxpayers, payer,
    powerless, immediate, trapped, national).

% Concerned that Japan's expanded military role could provoke regional arms races or destabilize existing security architectures. They bear the cost of increased regional tensions and potential conflict.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates, payer,
    organized, generational, constrained, regional).

% Analyze the legal and historical implications of the collective self-defense interpretation, assessing its consistency with constitutional principles and international law. Their influence is primarily intellectual.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Japan's security policy with its allies, particularly the US, by allowing for joint military operations and mutual defense, thereby enhancing regional deterrence and collective security.
% TRANSFER_FUNCTION: Transfers the burden of collective security from allies to Japan, and transfers financial resources from domestic programs to defense spending. It also transfers interpretive authority over Article 9 from a strict textual reading to a more flexible, policy-driven one.
% ABSENT_VOICES: Future generations of Japanese citizens, who will inherit the consequences of an expanded military role and potential foreign entanglements, are absent from the current debate. Also, a truly neutral international legal body, whose interpretation might differ from national interests, is not a direct participant.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan's security posture would revert to a narrower self-defense doctrine, significantly altering its alliance commitments and regional security dynamics. Defense spending would likely decrease, and diplomatic relations would shift, leading to a substantial reorganization of security policy.
% FOUNDING_PROBLEM: The original Article 9 was designed to prevent Japan from ever engaging in aggressive warfare again after WWII, ensuring a pacifist state. The collective self-defense interpretation addresses the problem of how a pacifist constitution can adapt to modern security threats and alliance obligations.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and its allies attest that the problem of adapting to contemporary security threats is live, citing regional geopolitical instability. Strict pacifist advocates and many constitutional scholars, however, argue that the original problem of preventing aggressive war remains paramount and that the current interpretation creates new risks, not solutions.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the significant shift in resources and constitutional interpretation away from a purely defensive posture, imposing costs on those who prefer a narrower reading or oppose militarization. Suppression (0.70) is high due to the government's active efforts to overcome domestic opposition and constitutional challenges to this interpretation, often through legislative means that limit public debate or judicial review. The theater ratio (0.20) is moderate; while the 'self-defense' justification is partly performative to legitimize expanded roles, there are genuine security concerns driving the policy. The increasing extractiveness and suppression over time reflect the gradual, contested expansion of this interpretation since the post-WWII era.
 *
 * PERSPECTIVAL GAP:
 *   The Japanese government and its US allies perceive this interpretation as a necessary and beneficial adaptation (Rope-like coordination for security). However, strict pacifist and inherent-right advocates experience it as a highly extractive and suppressive mechanism that erodes constitutional principles and increases national risk (Snare-like extraction). The engine's computation of per-seat classifications will reflect this divergence based on their declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government and the US military alliance are clear beneficiaries, as this interpretation enables their strategic goals (low directionality). Strict pacifist advocates, inherent-right advocates, taxpayers, and regional stability advocates are targets, bearing the costs of militarization, constitutional erosion, and increased regional tensions (high directionality). The 'identity_locked' exit option for strict pacifist advocates further amplifies their effective extraction, as their commitment to the pacifist ideal makes exit from the debate unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates Japan's security with its allies (a coordination function) but does so with significant asymmetric extraction from domestic groups and constitutional principles, requiring active enforcement to maintain. It prevents mislabeling by acknowledging both the coordination aspect (addressing modern security threats) and the extractive aspect (imposing costs on those who adhere to a stricter constitutional interpretation). The 'contested' status of the founding problem further highlights the ongoing tension between the original mandate and its current interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_legitimacy,
    'Is the collective self-defense interpretation a legitimate evolution of Article 9''s intent, or a fundamental re-writing of the constitution through re-interpretation?',
    'A constitutional amendment process, or a definitive Supreme Court ruling that explicitly addresses the scope of Article 9 in relation to collective self-defense.',
    'If deemed a legitimate evolution, the constraint''s perceived legitimacy would increase, potentially reducing resistance. If deemed an illegitimate re-writing, it would face severe challenges to its authority, potentially leading to a constitutional crisis or a re-classification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_legitimacy, conceptual, 'Ambiguity regarding the constitutional legitimacy of the collective self-defense interpretation.').

omega_variable(
    regional_security_vs_destabilization,
    'Does Japan''s expanded role in collective self-defense genuinely enhance regional security, or does it contribute to an arms race and destabilize the region?',
    'Long-term empirical analysis of regional military buildups, diplomatic relations, and conflict incidence following Japan''s policy shifts, corroborated by independent geopolitical experts.',
    'If it demonstrably enhances security, the coordination function is strengthened, potentially reducing perceived extraction for some regional actors. If it destabilizes, the extractive nature (imposing risks on the region) would be amplified, pushing the classification further towards Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_security_vs_destabilization, empirical, 'Uncertainty about the actual impact of Japan''s collective self-defense policy on regional stability.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''article_9_war_renunciation'' kernel. What would a ''strict_pacifist_reading'' or ''inherent_right_reading'' change structurally?',
    'Analysis of the structural deltas between this reading and its siblings, as defined in their respective constraint stories.',
    'A ''strict_pacifist_reading'' would drastically reduce extractiveness and suppression, likely classifying as a Mountain or Rope. An ''inherent_right_reading'' would reduce extractiveness and suppression relative to this reading, but still allow for a defensive military, likely classifying as a Rope or less extractive Tangled Rope. The disagreement is located in the scope of permissible military action and the interpretation of ''war renunciation''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is a specific reading of Article 9, with other readings having different structural implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(arti_tr_t2005, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(arti_be_t2005, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(arti_su_t2005, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_defense_budget_allocation).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_maritime_patrols).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'article_9_war_renunciation' kernel. The other readings are 'strict_pacifist_reading' and 'inherent_right_reading', each representing a distinct structural claim about Article 9's meaning and implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
