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
 *   human_readable: Article 9: Collective Self-Defense Reading
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'collective self-defense' reading of
 *   Japan's Article 9, which interprets the constitutional renunciation of
 *   war as permitting military action to defend allies, even without direct
 *   attack on Japan, when Japan's survival is threatened. This reading
 *   expands the mission scope of Japan's Self-Defense Forces, allowing for
 *   overseas deployments and joint operations. It is a contested
 *   interpretation that has evolved over decades, absorbing incremental
 *   expansions of military capability and mission. The classification as a
 *   Tangled Rope reflects its genuine coordination function (alliance
 *   security) intertwined with asymmetric extraction (from pacifist citizens
 *   and constitutional stability).
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
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9: Collective Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'df9621cf-1fb1-4462-91d9-aed6c86c96bf').
narrative_ontology:cs_kernel_codification('df9621cf-1fb1-4462-91d9-aed6c86c96bf', fixed_text).
narrative_ontology:cs_authority_grounding('df9621cf-1fb1-4462-91d9-aed6c86c96bf', lineage).
narrative_ontology:cs_interpretation_layer_present('df9621cf-1fb1-4462-91d9-aed6c86c96bf').
narrative_ontology:cs_reading_relation('df9621cf-1fb1-4462-91d9-aed6c86c96bf', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('df9621cf-1fb1-4462-91d9-aed6c86c96bf', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('df9621cf-1fb1-4462-91d9-aed6c86c96bf', foundational, collective_self_defense_is_inherent_right).
narrative_ontology:cs_axiom_status(collective_self_defense_is_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('df9621cf-1fb1-4462-91d9-aed6c86c96bf', collective_self_defense_is_inherent_right, conventional).
narrative_ontology:cs_axiom('df9621cf-1fb1-4462-91d9-aed6c86c96bf', foundational, survival_threat_justifies_collective_action).
narrative_ontology:cs_axiom_status(survival_threat_justifies_collective_action, holdable).
narrative_ontology:cs_axiom_grounding('df9621cf-1fb1-4462-91d9-aed6c86c96bf', survival_threat_justifies_collective_action, empirically_contingent).
narrative_ontology:cs_reference_frame('df9621cf-1fb1-4462-91d9-aed6c86c96bf', post_war_security_adaptation).
narrative_ontology:cs_drift_state('df9621cf-1fb1-4462-91d9-aed6c86c96bf', contemporary_geopolitical_realities, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('df9621cf-1fb1-4462-91d9-aed6c86c96bf', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_alliance_partners).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_strict_pacifist).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit collective self-defense, expanding Japan's military role. Benefits from increased geopolitical influence and strengthened alliances. Faces domestic political resistance but controls legislative agenda.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from expanded mission scope, increased budget, and opportunities for overseas deployments and joint operations. Their institutional mandate is broadened by this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_establishment, beneficiary,
    organized, generational, constrained, national).

% Benefits from Japan's increased military contributions to regional security, reducing their own burden and strengthening deterrence. Exerts diplomatic pressure for this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_alliance_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the costs of increased military spending, potential involvement in overseas conflicts, and the erosion of Japan's post-war pacifist identity. Their deeply held values are challenged by this reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens, payer,
    powerless, biographical, identity_locked, national).

% Argue that this interpretation fundamentally violates the plain text and original intent of Article 9, undermining constitutional integrity. Their academic authority is challenged by the political reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_strict_pacifist, payer,
    moderate, generational, constrained, national).

% Fear that Japan's expanded military role could destabilize the region, provoking an arms race or increasing the risk of conflict. Their advocacy for diplomatic solutions is undermined.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates, payer,
    organized, generational, constrained, regional).

% Monitor Japan's constitutional interpretation and military posture in relation to international law and norms of self-defense. Their analysis informs global discourse on constitutional evolution.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, international_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Japan's security policy with its allies, allowing for joint military operations and a more robust response to regional threats, thereby enhancing collective deterrence.
% TRANSFER_FUNCTION: Transfers the burden of collective security from alliance partners to Japan, and transfers constitutional stability (as understood by pacifists) into geopolitical flexibility for the ruling coalition.
% ABSENT_VOICES: Citizens and scholars advocating for a strict pacifist interpretation are present in public discourse but are structurally excluded from the decision-making process that reinterprets the constitution. Their arguments are acknowledged but not determinative.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan's security policy would revert to a narrower self-defense posture, impacting alliance commitments, regional power dynamics, and domestic political alignments. The defense establishment would face a reduced mandate, and the ruling coalition would lose a key policy tool.
% FOUNDING_PROBLEM: The post-WWII need to establish a new national identity for Japan, renouncing war and preventing future militarism, while ensuring its security in a volatile region.
% FOUNDING_PROBLEM_CORROBORATION: The ruling coalition and defense establishment argue the founding problem of security in a volatile region is still live, requiring this interpretation. Pacifist citizens and constitutional scholars argue the problem of preventing militarism is paramount and this interpretation undermines it; historical analysis and public opinion polls from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because this interpretation shifts the burden of constitutional stability from the state to its citizens, who must accept a redefinition of pacifism. Suppression (0.70) is significant as the ruling coalition actively enforces this interpretation through legislative action and diplomatic pressure, despite strong public and academic resistance. Theater ratio (0.20) is moderate; while the security rationale is real, the 'self-defense' framing for collective action involves a degree of performative justification. The increasing extractiveness and suppression over time reflect the gradual, politically driven reinterpretation of Article 9.
 *
 * PERSPECTIVAL GAP:
 *   The ruling coalition and defense establishment perceive this as a necessary adaptation for national security and alliance coordination, a 'rope' that strengthens Japan. Pacifist citizens and constitutional scholars experience it as a 'snare' that extracts constitutional integrity and peace, enforced by political power. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling coalition, defense establishment, and US alliance partners are beneficiaries, gaining geopolitical flexibility and security contributions. Pacifist citizens, strict pacifist constitutional scholars, and regional stability advocates are victims, bearing the costs of constitutional erosion, increased military risk, and challenged values. International law observers maintain an analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted from its original post-war intent of absolute pacifism. The 'collective self-defense' reading attempts to resolve this by re-framing the mandate as 'active contribution to international peace and security,' but this re-framing itself is the source of extraction from those who adhere to the original mandate. The classification as Tangled Rope prevents mislabeling it as pure coordination (a 'rope') by highlighting the asymmetric extraction and active enforcement required to maintain the expanded interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_amendment_necessity,
    'Is the ''collective self-defense'' interpretation a legitimate constitutional evolution, or does it require a formal amendment to Article 9?',
    'A Supreme Court ruling explicitly affirming or rejecting the constitutionality of collective self-defense without amendment, or a national referendum on amending Article 9.',
    'If an amendment is deemed necessary, the current interpretation is a ''snare'' of political power over constitutional text. If affirmed as legitimate evolution, it moves closer to a ''tangled_rope'' or even ''rope'' for the ruling coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_necessity, conceptual, 'Whether the interpretation is within the bounds of constitutional evolution or requires formal revision.').

omega_variable(
    threat_perception_objectivity,
    'Is the ''Japan''s survival threatened'' trigger for collective self-defense an objective, empirically verifiable condition, or is it subject to political discretion and manipulation?',
    'Independent, non-partisan security assessments and international legal opinions on specific scenarios where the trigger is invoked. Analysis of historical instances of invocation.',
    'If subjective, the trigger becomes a ''snare'' for political expansion of military action. If objective, it strengthens the ''rope'' aspect of genuine collective security coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_objectivity, empirical, 'Objectivity of the ''survival threatened'' trigger for military action.').

omega_variable(
    reading_legitimacy_source,
    'What is the primary source of legitimacy for this ''collective self-defense'' reading: evolving security needs, political will, or a re-evaluation of original intent?',
    'Analysis of legislative debates, judicial opinions, and public discourse over time, tracing the arguments used to justify the interpretation. Comparative constitutional analysis.',
    'If primarily political will, the reading is more extractive. If grounded in a re-evaluation of original intent or genuinely evolving security needs, it may be perceived as more legitimate by some stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_source, conceptual, 'Source of legitimacy for the collective self-defense interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_defense_budget_allocation).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_overseas_military_deployments).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_treaty_obligations).

% DUAL FORMULATION NOTE:
% This is one reading of the Article 9 war-renunciation kernel. Other readings (strict_pacifist_reading, inherent_right_reading) represent different constraints with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
