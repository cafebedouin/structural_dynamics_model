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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9: Collective Self-Defense Interpretation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'collective self-defense' reading of
 *   Japan's Article 9, which interprets the constitutional renunciation of
 *   war as permitting military action to defend allies when Japan's survival
 *   is indirectly threatened. This reading has led to a significant expansion
 *   of Japan's military scope, including overseas deployments and joint
 *   operations, moving away from a strictly individual self-defense posture.
 *   The constraint is presented as a necessary adaptation to modern security
 *   challenges, but it involves substantial costs and suppression of
 *   dissenting views.
 *
 * KEY AGENTS:
 *   - Japanese Government: Primary agenda-setter, interprets and enforces the reading.
 *   - Allied Nations: Primary beneficiaries, gain from Japan's expanded military role.
 *   - Japanese Taxpayers: Primary payers, bear financial and potential human costs.
 *   - Pacifist Citizens: Payer/Excluded, bear moral costs and are marginalized.
 *   - Inherent Right Proponents: Payer/Beneficiary, benefit from defense but pay for mission creep.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.75).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.8).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9: Collective Self-Defense Interpretation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'e47d6e26-db7b-4dd1-9b0a-47965fe014d3').
narrative_ontology:cs_kernel_codification('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', fixed_text).
narrative_ontology:cs_authority_grounding('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', lineage).
narrative_ontology:cs_interpretation_layer_present('e47d6e26-db7b-4dd1-9b0a-47965fe014d3').
narrative_ontology:cs_reading_relation('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', foundational, national_survival_paramount).
narrative_ontology:cs_axiom_status(national_survival_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', national_survival_paramount, instrumental).
narrative_ontology:cs_axiom('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', foundational, collective_security_imperative).
narrative_ontology:cs_axiom_status(collective_security_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', collective_security_imperative, conventional).
narrative_ontology:cs_reference_frame('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', post_wwii_pacifist_constitution).
narrative_ontology:cs_drift_state('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', contemporary_geopolitical_context, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e47d6e26-db7b-4dd1-9b0a-47965fe014d3', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, allied_nations).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industry).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, inherent_right_proponents).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, inherent_right_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit collective self-defense, expanding Japan's military role. Benefits from stronger alliances and increased geopolitical influence, but faces domestic political and legal challenges.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from Japan's expanded military capabilities and willingness to participate in collective security operations, reducing their own burden in regional defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, allied_nations, beneficiary,
    institutional, generational, mobile, global).

% Bear the financial costs of increased defense spending, potential for military entanglement, and the social costs of a shift away from a strictly pacifist national identity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_taxpayers, payer,
    organized, biographical, constrained, national).

% Deeply committed to the strict pacifist interpretation of Article 9. They bear the moral and social costs of the expanded military role and feel their constitutional values are being eroded, with limited avenues for effective political resistance.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, pacifist_citizens, excluded).

% Directly benefits from increased government procurement of military equipment and technology, driven by the expanded scope of defense operations.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% Believe in Japan's inherent right to individual self-defense but are wary of collective self-defense due to the risks of entanglement in foreign conflicts. They benefit from a strong national defense but pay the cost of mission creep and potential loss of autonomy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, inherent_right_proponents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, inherent_right_proponents, beneficiary).

% Concerned that Japan's expanded military role, particularly collective self-defense, could destabilize regional security dynamics and provoke an arms race. They bear the risk of increased regional tensions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates, excluded).

% Analyze the legal and historical implications of the collective self-defense interpretation, often providing critical perspectives on its constitutionality and impact on democratic norms.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, japanese_government).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Japan to participate in collective security arrangements with allies, coordinating defense efforts against shared threats and contributing to regional stability from the perspective of its proponents.
% TRANSFER_FUNCTION: Transfers increased defense spending and potential military risk from allied nations to Japanese taxpayers, in exchange for enhanced security guarantees and geopolitical alignment.
% ABSENT_VOICES: Strict pacifist groups and some constitutional scholars are often marginalized in policy debates, their arguments for a narrower interpretation of Article 9 overridden by geopolitical imperatives and government interpretations. Neighboring nations with historical grievances may also feel excluded from the interpretive process.
% DISAPPEARANCE_RATIONALE: If the collective self-defense interpretation vanished overnight, Japan's defense policy would revert to a strictly individual self-defense posture, significantly altering its alliances, defense budget, and regional security role. Allied nations would need to re-evaluate their security strategies, and Japan's geopolitical standing would shift dramatically.
% FOUNDING_PROBLEM: The original Article 9 was established post-WWII to permanently renounce war and the maintenance of military forces, aiming to prevent future Japanese aggression and ensure lasting peace.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and allied nations argue that the founding problem (ensuring peace) is still live, but requires a more robust, collective defense posture in the face of modern threats. Pacifist groups and some legal scholars, from outside the benefiting parties, contend that the original problem of preventing military adventurism is being re-introduced by this expanded interpretation, and that the founding problem of absolute war renunciation is being undermined.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high due to increased defense spending, potential for entanglement in foreign conflicts, and the erosion of a pacifist identity. Suppression is high because the government actively marginalizes or overrides constitutional challenges and public dissent to maintain this interpretation. Theater ratio is moderate; while genuine defense needs exist, some justifications for expanded roles serve political alignment rather than strict necessity. Accessibility collapse is moderate as alternative interpretations are still debated but face significant institutional barriers. Resistance is high, reflecting ongoing public and academic opposition.
 *
 * PERSPECTIVAL GAP:
 *   The Japanese government and allied nations perceive this interpretation as a necessary and beneficial coordination for regional security. In contrast, Japanese taxpayers, pacifist citizens, and inherent right proponents experience it as an extractive mechanism that imposes significant costs and risks, eroding constitutional principles. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese government and allied nations are beneficiaries, gaining flexibility and security contributions. Japanese taxpayers, pacifist citizens, and regional stability advocates are targets, bearing the financial, moral, and geopolitical risks. Inherent right proponents are mixed, benefiting from defense but targeted by the expansion beyond individual self-defense.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the collective self-defense interpretation as a pure Rope (simple coordination) by highlighting the asymmetric extraction and active suppression involved. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, albeit contested, coordination function it claims to serve for allied defense. The rising extractiveness and suppression over time indicate a drift towards greater extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint a valid interpretation of Article 9, or a re-interpretation that fundamentally alters its original intent?',
    'Long-term judicial review by the Supreme Court, or a constitutional amendment process that explicitly clarifies Article 9''s scope.',
    'If deemed a fundamental alteration, the legitimacy of the current defense policy would be severely undermined, potentially leading to reclassification as a Snare. If validated, its coordination function would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity regarding the constitutional legitimacy of the collective self-defense interpretation.').

omega_variable(
    survival_threat_ambiguity,
    'What constitutes a ''threat to Japan''s survival'' that justifies collective self-defense, and is this threshold being applied consistently and objectively?',
    'Establishment of clear, publicly debated, and judicially reviewable criteria for ''survival threat,'' with independent oversight of its application.',
    'If the threshold is vague or arbitrarily applied, it increases the risk of entanglement in conflicts not directly vital to Japan, amplifying extractiveness and reducing the perceived coordination benefit. If clear, it could reduce perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threat_ambiguity, empirical, 'Ambiguity in the trigger condition for collective self-defense.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of pacifist and strict-interpretation voices structural (institutional barriers) or internalized (social pressure, identity fusion)?',
    'Analysis of public discourse and policy outcomes: if dissent remains marginalized despite legal avenues, internalized suppression is likely higher. Post-policy-change trajectory: if resistance persists after policy shifts, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as citizens carry the suppression with them. If purely structural, removing barriers could more easily enable alternative interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(arti_tr_t1998, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(arti_tr_t2006, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2006, 0.3).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(arti_be_t1998, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(arti_be_t2006, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2006, 0.65).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(arti_su_t1998, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(arti_su_t2006, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2014, 0.75).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_us_security_alliance).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_security_architecture).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_defense_budget_allocation).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 9 war renunciation kernel. Its interpretation of collective self-defense influences the 'inherent_right_reading' by expanding the scope of permissible action, and forecloses the 'strict_pacifist_reading' by directly contradicting its absolute prohibition on military forces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
