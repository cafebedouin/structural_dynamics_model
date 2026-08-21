% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDCs (Sovereigntist Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereigntist reading' of Article
 *   4 of the Paris Agreement, which frames Nationally Determined
 *   Contributions (NDCs) as voluntary, self-determined pledges designed to
 *   preserve national energy sovereignty. In this reading, states retain full
 *   freedom to set and revise their climate targets, and global enforcement
 *   mechanisms are minimal or atrophy, ensuring that fossil-dependent
 *   economies can preserve their development pathways. This reading
 *   emphasizes the non-binding nature of NDCs and the primacy of national
 *   decision-making in climate policy.
 *
 * KEY AGENTS:
 *   - Nation_states: Primary agenda-setters and beneficiaries (institutional/mobile)
 *   - Fossil_fuel_dependent_economies: Beneficiaries (organized/constrained)
 *   - Global_climate_advocates: Payers/Excluded (moderate/constrained)
 *   - International_climate_institutions: Observers (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.15).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.1).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDCs (Sovereigntist Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '987ac698-2481-4174-ad15-2f7816c0e126').
narrative_ontology:cs_kernel_codification('987ac698-2481-4174-ad15-2f7816c0e126', fixed_text).
narrative_ontology:cs_authority_grounding('987ac698-2481-4174-ad15-2f7816c0e126', lineage).
narrative_ontology:cs_interpretation_layer_present('987ac698-2481-4174-ad15-2f7816c0e126').
narrative_ontology:cs_reading_relation('987ac698-2481-4174-ad15-2f7816c0e126', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('987ac698-2481-4174-ad15-2f7816c0e126', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('987ac698-2481-4174-ad15-2f7816c0e126', foundational, national_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('987ac698-2481-4174-ad15-2f7816c0e126', national_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('987ac698-2481-4174-ad15-2f7816c0e126', foundational, self_determination_of_contributions).
narrative_ontology:cs_axiom_status(self_determination_of_contributions, holdable).
narrative_ontology:cs_axiom_grounding('987ac698-2481-4174-ad15-2f7816c0e126', self_determination_of_contributions, conventional).
narrative_ontology:cs_reference_frame('987ac698-2481-4174-ad15-2f7816c0e126', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('987ac698-2481-4174-ad15-2f7816c0e126', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('987ac698-2481-4174-ad15-2f7816c0e126', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, nation_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, developed_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, global_climate_advocates).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_sovereignty).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As signatories to the Paris Agreement, nation states determine their own Nationally Determined Contributions (NDCs), preserving their right to set domestic energy and climate policy without external imposition. They benefit from the flexibility and non-binding nature of the pledges.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, nation_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, nation_states, beneficiary).

% These economies benefit from the voluntary and self-determined nature of NDCs, which allows them to prioritize economic development and energy security based on their existing fossil fuel infrastructure, without being forced into rapid, costly transitions by international mandates.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_dependent_economies, beneficiary,
    organized, biographical, constrained, global).

% Developed nations benefit from the flexibility to define their climate actions and contributions, allowing them to balance domestic economic interests with international climate goals, and to avoid legally binding targets that might be politically or economically challenging.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developed_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Developing nations benefit from the principle of self-determination, which allows them to pursue economic growth and poverty eradication without being constrained by stringent, externally imposed climate targets that might hinder their development pathways.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developing_nations, beneficiary,
    institutional, generational, constrained, global).

% These groups advocate for stronger, more binding climate action and bear the perceived cost of insufficient global ambition resulting from the voluntary nature of NDCs. Their calls for supranational enforcement or more aggressive targets are not directly incorporated into this sovereigntist interpretation of the constraint.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, global_climate_advocates, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, global_climate_advocates, excluded).

% Bodies like the UNFCCC Secretariat facilitate the NDC process, collect submissions, and track progress, but their role is primarily facilitative and observational, not enforcement-oriented, in line with the sovereigntist interpretation of the Agreement.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, international_climate_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global framework for all nations to voluntarily declare and pursue climate action, ensuring a coordinated, albeit flexible, response to climate change while respecting national sovereignty and diverse circumstances.
% TRANSFER_FUNCTION: Facilitates a voluntary exchange of climate commitments and information among nations, with no direct financial or resource transfer mandated by the NDC mechanism itself, beyond what states choose to include in their pledges.
% ABSENT_VOICES: Supranational enforcement bodies, global citizens' assemblies, and climate justice advocates who prioritize legally binding, top-down targets and accountability mechanisms would object, as their preferred mechanisms are explicitly excluded by the sovereigntist interpretation's emphasis on national control.
% DISAPPEARANCE_RATIONALE: If the NDC framework, even in its voluntary form, vanished overnight, the primary global structure for coordinated climate action would disappear. Nations would likely revert to purely unilateral actions or bilateral agreements, leading to a more fragmented, less transparent, and potentially less ambitious global response to climate change.
% FOUNDING_PROBLEM: How to achieve meaningful global climate action and cooperation without infringing on the fundamental principle of national sovereignty, given the diverse economic, social, and developmental stages of nations worldwide.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, national governments (particularly those from developing economies or those with significant fossil fuel interests), and legal experts in international treaty law corroborate that the tension between national sovereignty and global climate imperatives remains a central and live challenge in international climate governance.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect the voluntary and self-determined nature of NDCs in this reading. States are not coerced into specific actions, and they retain significant exit options and revision freedom. The theater ratio is low (0.05) because the constraint genuinely facilitates national action, even if that action is not externally mandated. Accessibility collapse is low (0.20) as states have many alternatives for their energy and climate policies. Resistance is low (0.10) because the framework accommodates national interests, reducing direct opposition to the mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   This sovereigntist reading stands in stark contrast to the 'supranational' and 'equity' readings of the same kernel. From the supranational perspective, the voluntary nature of NDCs would be seen as a critical flaw leading to high extraction from the global climate system. From the equity perspective, the lack of differentiated responsibilities would be seen as extracting from developing nations. This story, however, strictly adheres to the internal logic of the sovereigntist frame, where states are net beneficiaries of the flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Nation states, particularly those with significant fossil fuel interests or developmental needs, are clear beneficiaries (low directionality) as they retain full control over their climate policies and development trajectories. Global climate advocates, while not directly 'victims' of the constraint in this reading, bear the 'cost' of what they perceive as insufficient global action, placing them closer to the payer/excluded end of the spectrum.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the NDC framework, in this sovereigntist reading, is to enable global climate action while upholding national sovereignty. This mandate remains live, as the tension between global environmental imperatives and national self-determination is ongoing. Therefore, the constraint is not experiencing mandatrophy; it is actively fulfilling its intended function within this specific interpretive frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_climate_effectiveness,
    'Does the preservation of national energy sovereignty, as emphasized by this reading, lead to insufficient global climate action, thereby imposing a diffuse, long-term ''extraction'' on the global climate system and future generations?',
    'Empirical analysis of global emissions trajectories and climate impacts over time, compared against IPCC targets and the aggregate ambition of NDCs under this interpretation.',
    'If global climate targets are missed due to the voluntary nature of NDCs, the effective extractiveness of this constraint (from a global/future perspective) would be significantly higher, potentially reclassifying it as a ''snare'' or ''tangled_rope'' when viewed through a planetary lens, despite its ''rope'' classification from the national seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_climate_effectiveness, empirical, 'The tension between national sovereignty and the effectiveness of global climate action.').

omega_variable(
    reading_framing_impact,
    'Is this sovereigntist reading a genuine interpretation of the Paris Agreement''s intent, or a strategic framing by states to avoid more stringent obligations?',
    'Analysis of negotiating history, state declarations, and legal scholarship on treaty interpretation, alongside the observed behavior of states in implementing (or not implementing) ambitious NDCs.',
    'If primarily a strategic framing, the ''rope'' classification would be a cover story, and the constraint would structurally operate closer to a ''snare'' or ''tangled_rope'' from the perspective of global climate goals, with states acting as agenda-setters extracting flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Whether the sovereigntist reading reflects genuine intent or strategic maneuvering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.05).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 12, 0.05).
narrative_ontology:measurement(pari_tr_t18, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 18, 0.05).
narrative_ontology:measurement(pari_tr_t24, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement(pari_be_t18, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 18, 0.13).
narrative_ontology:measurement(pari_be_t24, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 6, 0.1).
narrative_ontology:measurement(pari_su_t12, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(pari_su_t18, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 18, 0.1).
narrative_ontology:measurement(pari_su_t24, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 24, 0.1).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, global_carbon_markets).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, national_energy_policy).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the Paris Agreement's Article 4 NDCs, each with different structural implications for state obligations and global accountability. This sovereigntist reading emphasizes national autonomy and voluntary contributions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
