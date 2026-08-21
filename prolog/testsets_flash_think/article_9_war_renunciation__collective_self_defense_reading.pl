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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint represents the interpretation of Japan's Article 9 (war
 *   renunciation clause) that permits military action for collective
 *   self-defense when Japan's survival is threatened, even without direct
 *   attack on Japan. This reading allows for overseas deployments and joint
 *   operations with allies. It is one reading of the
 *   'article_9_war_renunciation' kernel, expanding the mission scope beyond a
 *   strict self-defense posture. The classification as a Tangled Rope
 *   reflects its dual function: coordinating collective security with allies
 *   while extracting resources and legitimacy from a narrower, pacifist
 *   interpretation of the constitution.
 *
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
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '3324a2ca-e8f3-457b-876d-4b033e3e12a2').
narrative_ontology:cs_kernel_codification('3324a2ca-e8f3-457b-876d-4b033e3e12a2', fixed_text).
narrative_ontology:cs_authority_grounding('3324a2ca-e8f3-457b-876d-4b033e3e12a2', lineage).
narrative_ontology:cs_interpretation_layer_present('3324a2ca-e8f3-457b-876d-4b033e3e12a2').
narrative_ontology:cs_reading_relation('3324a2ca-e8f3-457b-876d-4b033e3e12a2', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('3324a2ca-e8f3-457b-876d-4b033e3e12a2', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('3324a2ca-e8f3-457b-876d-4b033e3e12a2', foundational, collective_self_defense_is_inherent_right_of_sovereign_state).
narrative_ontology:cs_axiom_status(collective_self_defense_is_inherent_right_of_sovereign_state, holdable).
narrative_ontology:cs_axiom_grounding('3324a2ca-e8f3-457b-876d-4b033e3e12a2', collective_self_defense_is_inherent_right_of_sovereign_state, deontological).
narrative_ontology:cs_axiom('3324a2ca-e8f3-457b-876d-4b033e3e12a2', foundational, threat_to_allies_can_threaten_japan_survival).
narrative_ontology:cs_axiom_status(threat_to_allies_can_threaten_japan_survival, holdable).
narrative_ontology:cs_axiom_grounding('3324a2ca-e8f3-457b-876d-4b033e3e12a2', threat_to_allies_can_threaten_japan_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('3324a2ca-e8f3-457b-876d-4b033e3e12a2', post_wwii_constitutional_order).
narrative_ontology:cs_drift_state('3324a2ca-e8f3-457b-876d-4b033e3e12a2', contemporary_geopolitical_shifts, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3324a2ca-e8f3-457b-876d-4b033e3e12a2', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industry).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, allied_nations).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, military_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, opposition_parties).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, flexible_constitutional_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and implements the interpretation that Article 9 permits collective self-defense, leveraging it to strengthen alliances and project a more robust security posture. Benefits from increased geopolitical influence and perceived national security.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, ruling_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives increased contracts and funding due to the expanded scope of military operations and procurement needs under the collective self-defense interpretation. Profits directly from the reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industry, beneficiary,
    organized, biographical, mobile, national).

% Benefit from Japan's expanded security contributions and participation in joint military exercises, enhancing their own defense capabilities and regional stability efforts. They actively encourage this interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, allied_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain expanded mission scope, increased resources, and enhanced professional development opportunities, but also face increased risks associated with overseas deployments and collective defense operations. Their identity is tied to national defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, military_personnel, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the financial costs of increased defense spending, military procurement, and potential overseas operations resulting from the expanded interpretation of Article 9. Their ability to influence policy is diffuse.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_taxpayers, payer,
    moderate, biographical, constrained, national).

% Experience the erosion of their preferred strict pacifist interpretation of Article 9, facing increased political marginalization and the undermining of a core national value. They actively resist the reinterpretation through protests and legal challenges.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_advocates, payer,
    organized, generational, constrained, national).

% Oppose the reinterpretation on constitutional grounds and policy implications, arguing it deviates from Japan's post-war identity and risks entanglement in foreign conflicts. They bear the political cost of losing ground on this issue.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, opposition_parties, payer,
    organized, biographical, constrained, national).

% Are concerned that Japan's expanded military role could destabilize the region, trigger an arms race, or lead to increased tensions. They bear the cost of heightened regional insecurity and diplomatic friction.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates, payer,
    organized, biographical, constrained, regional).

% Analyze the legal and historical implications of the collective self-defense interpretation, often offering critical perspectives on its consistency with the original intent of Article 9 and its long-term constitutional impact.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Are directly affected by Japan's changing military posture and its implications for regional security, but have no direct voice or influence in Japan's domestic constitutional interpretation process. Their concerns are often expressed through diplomatic channels but are not determinative.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors, excluded,
    institutional, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable Japan to participate in collective security arrangements with allied nations, addressing regional threats that may not directly target Japanese territory but are deemed to impact its survival and broader security interests.
% TRANSFER_FUNCTION: Transfers national resources (defense budget, military personnel risk, diplomatic capital) from domestic priorities and a purely self-defense-only posture to broader collective security operations and alliance commitments.
% ABSENT_VOICES: Citizens and political groups advocating for strict adherence to Article 9's original pacifist interpretation are often marginalized in the policy-making process. Additionally, regional neighbors who fear re-militarization are not directly consulted in Japan's internal constitutional debates.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, Japan would revert to a strictly self-defense-only posture, significantly altering its alliances, regional security dynamics, and its role in international security. Allied nations would need to re-evaluate their defense strategies, and Japan's defense budget and operational scope would likely shrink, leading to a major geopolitical reorganization.
% FOUNDING_PROBLEM: To address the perceived limitations of a purely self-defense-only interpretation of Article 9 in the face of evolving regional security threats (e.g., from North Korea, China) and the need for stronger, more integrated alliances.
% FOUNDING_PROBLEM_CORROBORATION: The ruling coalition and allied nations attest that regional security threats are live and necessitate this interpretation for national survival. Opposition parties and pacifist groups contest this, arguing that the founding problem is either exaggerated or does not justify reinterpreting the constitution. Independent security analysts offer varied assessments, some corroborating the threat, others questioning the necessity or wisdom of collective self-defense as a response.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.75) due to the significant reallocation of national resources towards military expansion and the reinterpretation of a foundational national principle, imposing costs on taxpayers and those who adhere to a strict pacifist view. Suppression is also high (0.80) as this interpretation actively suppresses alternative constitutional readings and public dissent regarding military expansion, requiring continuous political and legal defense. The theater ratio is moderate (0.40) because while there are genuine security concerns, some justifications for expanded 'defensive' actions may be performative, masking a broader strategic shift. The metrics show a clear upward trend over the interval, reflecting the gradual expansion of this interpretation's scope and impact.
 *
 * PERSPECTIVAL GAP:
 *   The ruling coalition and allied nations perceive this interpretation as a necessary and legitimate adaptation to modern security challenges, a coordination mechanism for collective defense. In contrast, pacifist advocates and opposition parties view it as an extractive reinterpretation that undermines Japan's constitutional identity and imposes undue costs and risks. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling coalition, defense industry, allied nations, and military personnel are beneficiaries, gaining geopolitical influence, contracts, security, and expanded roles, respectively. Japanese taxpayers, pacifist advocates, opposition parties, and regional stability advocates are victims, bearing financial costs, the erosion of constitutional principles, political losses, and heightened regional tensions. Regional neighbors are excluded, directly affected but without a voice in the interpretive process.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the collective self-defense interpretation as a pure Rope (genuine coordination) by highlighting the asymmetric extraction from those who adhere to a narrower constitutional reading and bear the costs without full consent. It also prevents mislabeling as a pure Snare by acknowledging the genuine coordination function with allied nations, even if that function is layered with extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_reinterpretation,
    'Is the collective self-defense interpretation a legitimate evolution of constitutional understanding or an unconstitutional amendment by reinterpretation?',
    'A national referendum on Article 9''s scope, or a definitive Supreme Court ruling that directly addresses the constitutionality of collective self-defense.',
    'If deemed unconstitutional, the constraint''s legitimacy would collapse, leading to a reclassification towards Snare or Piton. If affirmed, its perceived legitimacy would increase, potentially shifting it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_reinterpretation, conceptual, 'Ambiguity regarding the constitutional legitimacy of the collective self-defense interpretation.').

omega_variable(
    threat_justification_validity,
    'Are the perceived threats to Japan''s survival, used to justify collective self-defense, genuinely existential and requiring this expanded military role?',
    'Independent, international security assessments that are widely accepted across political divides, or a significant de-escalation of regional tensions over a sustained period.',
    'If threats are found to be exaggerated or manageable without collective self-defense, the justification for extraction would weaken, pushing the constraint towards Snare. If threats are unequivocally confirmed, the coordination aspect would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_justification_validity, empirical, 'Uncertainty about the empirical basis for the ''survival threatened'' trigger.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of pacifist alternatives structural (political/legal barriers) or internalized (social pressure, perceived inevitability)?',
    'Analysis of public discourse and political participation trends following a hypothetical removal of formal barriers to pacifist advocacy; if advocacy remains low, internalized suppression is higher.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the extractive nature of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pacifist and opposition voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(arti_tr_t2005, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(arti_tr_t2025, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(arti_be_t2005, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(arti_be_t2025, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(arti_su_t2005, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement(arti_su_t2025, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_us_security_alliance).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_maritime_security_protocols).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, defense_budget_allocation_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'article_9_war_renunciation' kernel. It focuses on the collective self-defense interpretation, which differs significantly in scope and impact from the 'strict_pacifist_reading' and 'inherent_right_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
