% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope: Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'territorial sovereignty' reading of GDPR
 *   Article 3's scope, which asserts that a state's regulatory authority is
 *   primarily bounded by its physical borders. It views the GDPR's
 *   extraterritorial application as exceeding legitimate regulatory
 *   authority, leading to jurisdictional conflicts and data localization as a
 *   resistance mechanism. This reading benefits non-EU states and data
 *   processors by limiting EU regulatory reach, while imposing costs on EU
 *   regulators and citizens by creating enforcement gaps.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.6).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.4).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'cb5ab527-97d5-42ad-a1c9-b864b4634c5c').
narrative_ontology:cs_kernel_codification('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', fixed_text).
narrative_ontology:cs_authority_grounding('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', lineage).
narrative_ontology:cs_interpretation_layer_present('cb5ab527-97d5-42ad-a1c9-b864b4634c5c').
narrative_ontology:cs_reading_relation('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', foundational, state_sovereignty_over_territory_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_over_territory_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', state_sovereignty_over_territory_is_absolute, deontological).
narrative_ontology:cs_axiom('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', secondary, extraterritorial_application_requires_explicit_consent_or_treaty).
narrative_ontology:cs_axiom_status(extraterritorial_application_requires_explicit_consent_or_treaty, holdable).
narrative_ontology:cs_axiom_grounding('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', extraterritorial_application_requires_explicit_consent_or_treaty, conventional).
narrative_ontology:cs_reference_frame('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb5ab527-97d5-42ad-a1c9-b864b4634c5c', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their exclusive right to regulate data processing within their borders, viewing the GDPR's extraterritorial reach as an infringement on their sovereignty. They benefit from resisting EU regulatory overreach, preserving their own regulatory space and potentially fostering domestic tech industries under different rules.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, beneficiary,
    institutional, generational, mobile, global).

% Companies operating outside the EU, particularly those not directly targeting EU residents, benefit from a strict territorial interpretation of jurisdiction. This limits their compliance burden and avoids conflicting legal obligations, allowing them to operate under their domestic laws without EU interference.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors, beneficiary,
    powerful, biographical, constrained, global).

% EU regulators, particularly national data protection authorities, face challenges in enforcing the GDPR against entities that assert a strict territorial defense. This reading increases their enforcement costs and limits their effective reach, forcing them to rely on international cooperation mechanisms which can be slow or ineffective.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, payer,
    institutional, generational, constrained, continental).

% EU citizens whose data is processed by non-EU entities that successfully invoke territorial sovereignty may find their GDPR rights unenforceable. This reading effectively reduces their privacy protections when their data leaves the EU, making them victims of the jurisdictional conflict.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens, payer,
    powerless, biographical, identity_locked, continental).

% Academics and legal experts who analyze the evolution of international jurisdiction and regulatory authority. They observe the tension between traditional territorial principles and modern extraterritorial regulatory claims, often advocating for clarity or new frameworks.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the principle of state sovereignty in international law, ensuring that each state has the exclusive right to regulate within its own territory and preventing overlapping or conflicting jurisdictional claims.
% TRANSFER_FUNCTION: Transfers regulatory authority and enforcement power from the EU to non-EU states regarding data processing activities outside the EU's physical borders, effectively limiting the scope of EU privacy protections.
% ABSENT_VOICES: Global privacy advocates and human rights organizations, who would argue that privacy is a universal right not bounded by national borders, and that a strict territorial reading undermines effective protection in a globalized digital economy.
% DISAPPEARANCE_RATIONALE: If this reading of territorial sovereignty vanished, EU regulators would assert broader extraterritorial jurisdiction more aggressively, leading to increased compliance burdens for non-EU entities and potentially more robust privacy protections for EU citizens abroad. Non-EU states would lose a key legal argument for resisting EU regulatory influence.
% FOUNDING_PROBLEM: The historical problem of preventing states from unilaterally imposing their laws on the sovereign territory of other states, leading to international conflict and legal uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: International law bodies, non-EU governments, and legal scholars consistently affirm the foundational importance of territorial sovereignty in international relations, citing ongoing disputes over jurisdiction in various domains (e.g., taxation, environmental law, cybersecurity) as evidence that the problem remains live.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost imposed on EU regulators and citizens by limiting the GDPR's reach, effectively 'extracting' the ability to enforce privacy rights globally. Suppression (0.4) is moderate, as non-EU states actively resist EU enforcement but cannot entirely suppress the GDPR's influence. Resistance (0.7) is high, driven by non-EU states' and companies' active legal and political challenges to extraterritoriality. Accessibility collapse (0.3) is low, as alternative regulatory frameworks (domestic laws) remain viable for non-EU entities. Theater ratio (0.1) is low, as the assertion of territorial sovereignty is a genuine legal and political stance, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-EU states, this reading is a 'rope' that upholds a foundational principle of international law, preventing regulatory overreach. From the perspective of EU regulators and citizens, it functions as a 'snare' that undermines privacy protections by creating jurisdictional loopholes for data processors outside the EU.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU states and data processors are beneficiaries (d near 0.0) as this reading protects their regulatory autonomy and reduces compliance burdens. EU regulators and citizens are victims (d near 1.0) as it limits their ability to enforce privacy rights and leaves EU citizens exposed to data practices outside EU control. International law scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territoriality_vs_digital_reality,
    'Is the traditional principle of territorial sovereignty adequate for regulating data in a globalized digital economy, or does it create an unresolvable regulatory gap?',
    'Development of new international treaties or customary international law specifically addressing digital jurisdiction, or a clear shift in state practice towards either strict territoriality or extraterritoriality.',
    'If inadequate, this reading would be reclassified as a ''piton'' or ''snare'' for global privacy, as it fails to address the actual problem. If adequate, it reinforces the ''rope'' aspect for state sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territoriality_vs_digital_reality, conceptual, 'The fit of territorial sovereignty to digital data flows.').

omega_variable(
    data_localization_efficacy,
    'To what extent does data localization genuinely protect national sovereignty and privacy, versus merely creating economic barriers and fragmenting the internet?',
    'Empirical studies on the effectiveness of data localization mandates in achieving their stated goals (e.g., national security, privacy protection) compared to their economic and technical costs.',
    'If data localization is found to be ineffective or counterproductive, the ''beneficiary'' status of non-EU states and data processors would be undermined, potentially shifting the constraint towards a ''snare'' for global commerce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_localization_efficacy, empirical, 'The actual impact of data localization as a resistance mechanism.').

omega_variable(
    jurisdictional_conflict_escalation,
    'Will the assertion of strict territorial sovereignty lead to increased international legal conflicts and regulatory fragmentation, or will it force a more harmonized approach to international data governance?',
    'Observation of trends in international litigation, diplomatic disputes, and the formation of new multilateral agreements or bilateral data transfer frameworks.',
    'Increased conflict would highlight the ''tangled rope'' nature, while harmonization would suggest a path towards a ''rope'' for international cooperation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisdictional_conflict_escalation, empirical, 'The long-term impact of this reading on international legal stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2016, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.08).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2016, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2016, 0.5).
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.59).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2016, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.39).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, us_cloud_act_scope).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, china_cybersecurity_law_data_localization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the GDPR Article 3 scope kernel. It represents the traditional international law perspective emphasizing territorial sovereignty, in contrast to readings that prioritize effects or market access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
