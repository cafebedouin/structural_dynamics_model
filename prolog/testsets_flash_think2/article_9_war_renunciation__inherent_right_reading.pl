% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9: Inherent Right to Self-Defense Reading
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'inherent right to self-defense' reading
 *   of Japan's Article 9, which renounces 'war' but is interpreted to permit
 *   a Self-Defense Force (SDF) for territorial defense, limited to a 'minimum
 *   necessary' capacity. This reading balances the constitutional text with
 *   the practical necessity of national security in a complex geopolitical
 *   environment. It is one of several competing interpretations of Article 9.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.45).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.6).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9: Inherent Right to Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '51952c90-3d23-4048-a91a-bbecbc6afc0b').
narrative_ontology:cs_kernel_codification('51952c90-3d23-4048-a91a-bbecbc6afc0b', fixed_text).
narrative_ontology:cs_authority_grounding('51952c90-3d23-4048-a91a-bbecbc6afc0b', lineage).
narrative_ontology:cs_interpretation_layer_present('51952c90-3d23-4048-a91a-bbecbc6afc0b').
narrative_ontology:cs_reading_relation('51952c90-3d23-4048-a91a-bbecbc6afc0b', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('51952c90-3d23-4048-a91a-bbecbc6afc0b', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('51952c90-3d23-4048-a91a-bbecbc6afc0b', foundational, inherent_sovereign_right_to_self_defense).
narrative_ontology:cs_axiom_status(inherent_sovereign_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('51952c90-3d23-4048-a91a-bbecbc6afc0b', inherent_sovereign_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('51952c90-3d23-4048-a91a-bbecbc6afc0b', foundational, war_renunciation_is_aggressive_action_only).
narrative_ontology:cs_axiom_status(war_renunciation_is_aggressive_action_only, holdable).
narrative_ontology:cs_axiom_grounding('51952c90-3d23-4048-a91a-bbecbc6afc0b', war_renunciation_is_aggressive_action_only, conventional).
narrative_ontology:cs_reference_frame('51952c90-3d23-4048-a91a-bbecbc6afc0b', post_wwii_demilitarization_framework).
narrative_ontology:cs_drift_state('51952c90-3d23-4048-a91a-bbecbc6afc0b', contemporary_geopolitical_shifts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51952c90-3d23-4048-a91a-bbecbc6afc0b', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_self_defense_principle).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, national_security_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit a 'minimum necessary' self-defense capacity, balancing constitutional text with security needs. It funds and directs the Self-Defense Forces (SDF) and seeks to maintain international legitimacy for its defense posture.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from national security and territorial integrity provided by the SDF. They also bear the financial cost of maintaining the SDF through taxes and may experience a tension between pacifist ideals and security realities.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_citizens, payer).

% Bear the financial burden of maintaining the SDF, which has grown in capability and budget over time. Their ability to influence this cost is primarily through electoral politics.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers, payer,
    moderate, immediate, constrained, national).

% Are legitimized and funded by this interpretation, allowing them to operate as a modern military force within constitutional limits. Their institutional identity is deeply tied to their defensive mandate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, self_defense_forces, agenda_setter).

% Analyze and debate the legal and historical interpretations of Article 9, influencing public discourse and judicial opinions on the scope of Japan's defense capabilities.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Oppose the maintenance of any military forces, viewing it as a violation of Article 9's spirit. They are compelled to fund the SDF through taxes and find their strict interpretation marginalized in official policy debates.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, pacifist_advocates, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Japanese state's need for national security and territorial defense with the constitutional mandate to renounce 'war' by defining a permissible 'minimum necessary' defensive capacity.
% TRANSFER_FUNCTION: Transfers financial resources from Japanese taxpayers to the Self-Defense Forces for their maintenance and operation, in exchange for national security and deterrence.
% ABSENT_VOICES: The strict pacifist interpretation, which argues for the complete absence of any armed forces, is largely excluded from mainstream policy-making, despite its constitutional grounding. Regional adversaries, whose actions are deterred by the SDF, have no voice in this internal constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, Japan would either revert to a strict pacifist stance (leaving it vulnerable) or fully remilitarize without constitutional constraint (fundamentally altering regional security). The SDF's legal basis and operational scope would collapse, leading to a profound reorganization of Japan's security policy and international relations.
% FOUNDING_PROBLEM: To prevent Japan from engaging in aggressive warfare again, while ensuring its ability to protect its sovereignty and citizens in a post-WWII world.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and many security analysts argue the problem of national security remains live, necessitating the SDF. Pacifist groups and some constitutional scholars contend the original problem of aggressive war has been solved, and the current interpretation oversteps the constitutional intent. International observers (e.g., UN, US allies) generally corroborate the need for defensive capacity but monitor its scope.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the cost of maintaining a defense force that some citizens oppose, but also the genuine security benefits. Suppression (0.60) is significant, as the state actively enforces this interpretation against stricter pacifist views and manages the scope of SDF operations. Theater ratio (0.20) is low, as the SDF performs a genuine defensive function, though some performative aspects exist in maintaining the 'minimum necessary' narrative. Accessibility collapse (0.40) is moderate; while a strict pacifist stance is difficult to achieve, political advocacy for it persists. Resistance (0.30) is also moderate, primarily from pacifist and anti-militarist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Japanese state, this interpretation is a pragmatic necessity for national survival, a legitimate exercise of sovereignty. From the perspective of pacifist advocates, it represents a betrayal of Article 9's original intent and an ongoing extraction of resources for an unnecessary military. The engine will compute these divergent classifications based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese state and its citizens are primary beneficiaries, gaining security and stability. Japanese taxpayers and pacifist advocates are victims, bearing the financial and ideological costs of maintaining a defense force they may oppose. The SDF itself is a beneficiary, gaining legitimacy and resources. The constraint's active enforcement ensures the 'minimum necessary' interpretation holds, preventing both full remilitarization and complete demilitarization.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_ambiguity,
    'What constitutes ''minimum necessary'' defensive capacity in a rapidly evolving geopolitical landscape?',
    'Ongoing legislative debate, judicial review, and international security assessments. Resolution would involve a clearer, possibly quantitative, definition of permissible military capabilities and roles.',
    'If ''minimum necessary'' is interpreted more broadly, the constraint''s extractiveness and suppression could increase as the SDF expands. If interpreted more narrowly, it could reduce these metrics but potentially increase perceived vulnerability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_ambiguity, conceptual, 'Ambiguity in defining the scope of permissible defense.').

omega_variable(
    war_vs_self_defense_distinction,
    'Is the distinction between ''war'' (aggressive action) and ''self-defense'' sufficiently clear and stable to prevent mission creep?',
    'Analysis of SDF deployments and international engagements: if defensive actions consistently expand into areas traditionally considered ''warfare,'' the distinction is eroding.',
    'If the distinction blurs, the constraint''s claimed type could shift towards a Snare or Tangled Rope with higher extraction, as the coordination story (pure defense) becomes cover for broader military action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_vs_self_defense_distinction, empirical, 'Clarity of the ''war'' vs. ''self-defense'' distinction.').

omega_variable(
    constitutional_text_vs_security_imperative,
    'To what extent does the current interpretation prioritize geopolitical security imperatives over the strict textual meaning of Article 9?',
    'Comparative legal analysis with other constitutional pacifism clauses, historical review of drafting intent, and public opinion surveys on the perceived balance.',
    'If security imperatives are found to consistently override textual meaning, the constraint''s legitimacy could erode, increasing resistance and potentially leading to calls for constitutional amendment or reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_vs_security_imperative, conceptual, 'Tension between constitutional text and security needs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.4).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_us_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, regional_security_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_9_war_renunciation' kernel. Its ε value differs significantly from the 'strict_pacifist_reading' and 'collective_self_defense_reading' due to differing interpretations of permissible military capacity and scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
