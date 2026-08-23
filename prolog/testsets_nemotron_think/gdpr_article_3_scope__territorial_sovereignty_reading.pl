% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Extraterritorial Scope (Territorial Sovereignty Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story models the GDPR Article 3 extraterritorial scope as
 *   experienced through the territorial sovereignty reading: jurisdiction is
 *   bounded by territory, and EU assertion of authority over non-EU entities
 *   processing EU resident data exceeds legitimate regulatory authority. The
 *   constraint is the standing arrangement — GDPR's extraterritorial
 *   application — assessed by this reading's lights. Extraction is high
 *   (0.72) because non-EU entities bear substantial compliance costs without
 *   representation; suppression is substantial (0.68) because enforcement
 *   (fines up to 4% global turnover, market access conditioning) actively
 *   coerces compliance; theater is moderate (0.42) because genuine privacy
 *   coordination exists but an increasing share of enforcement serves
 *   regulatory projection. Resistance is high (0.75) as non-EU states enact
 *   blocking statutes, data localization mandates, and rival regimes (PIPL,
 *   LGPD, state-level US laws). The measurement series tracks the
 *   constraint's evolution from 2018 (entry into force) to 2030 (projected),
 *   showing rising extractiveness and theater as the Brussels Effect matures
 *   into regulatory imperialism from this reading's perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, snare).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Extraterritorial Scope (Territorial Sovereignty Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '354f47ef-6f79-44f9-bd49-541203446956').
narrative_ontology:cs_kernel_codification('354f47ef-6f79-44f9-bd49-541203446956', formalized).
narrative_ontology:cs_authority_grounding('354f47ef-6f79-44f9-bd49-541203446956', lineage).
narrative_ontology:cs_interpretation_layer_present('354f47ef-6f79-44f9-bd49-541203446956').
narrative_ontology:cs_reading_relation('354f47ef-6f79-44f9-bd49-541203446956', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('354f47ef-6f79-44f9-bd49-541203446956', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('354f47ef-6f79-44f9-bd49-541203446956', foundational, territorial_sovereignty_exclusive_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_sovereignty_exclusive_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('354f47ef-6f79-44f9-bd49-541203446956', territorial_sovereignty_exclusive_jurisdiction, deontological).
narrative_ontology:cs_axiom('354f47ef-6f79-44f9-bd49-541203446956', foundational, extraterritorial_regulation_illegitimate).
narrative_ontology:cs_axiom_status(extraterritorial_regulation_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('354f47ef-6f79-44f9-bd49-541203446956', extraterritorial_regulation_illegitimate, deontological).
narrative_ontology:cs_reference_frame('354f47ef-6f79-44f9-bd49-541203446956', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('354f47ef-6f79-44f9-bd49-541203446956', digital_economy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('354f47ef-6f79-44f9-bd49-541203446956', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_entities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, global_technology_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_digital_service_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, global_technology_companies).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, territorial_sovereignty_exclusive_jurisdiction).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, extraterritorial_regulation_illegitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and enforce GDPR Article 3(2) extraterritorial scope through the Commission, EDPB, and CJEU. Collect regulatory authority and fines from non-EU entities. Justify extraterritoriality as necessary to protect EU residents' fundamental rights in the digital economy. Can modify the regulation through EU legislative process.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receive privacy protections and rights (access, erasure, portability) against non-EU controllers targeting the EU market. Benefit from the extraterritorial reach without bearing compliance costs. Exit is constrained by residency — they cannot opt out of the protection nor easily escape EU jurisdiction.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Non-EU companies offering goods/services to EU residents or monitoring their behavior. Bear full GDPR compliance costs (DPO, representatives, impact assessments, data transfer mechanisms) without EU legislative representation. Exit options: withdraw from EU market (revenue loss), implement data localization (costly), or comply (ongoing cost). Most choose constrained compliance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_entities, payer,
    moderate, biographical, constrained, global).

% Large platforms (US/Chinese tech giants) with deep EU market dependence. Bear massive compliance infrastructure costs but also gain regulatory moat — GDPR raises barriers to entry for smaller competitors. Exit is constrained by EU revenue share; they lobby for adequacy decisions and transfer mechanisms rather than withdrawal. Benefit incidentally from harmonized standard.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, global_technology_companies, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, global_technology_companies, beneficiary).

% Smaller non-EU SaaS, cloud, and digital service providers with EU customers. Lack resources for full compliance infrastructure. Face existential threat: comply at unsustainable cost, exit EU market (losing customers), or operate in legal grey zone. Many implement geo-blocking or data localization as survival strategies. Trapped by market dependence and resource asymmetry.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_digital_service_providers, payer,
    moderate, immediate, trapped, global).

% Sovereign states (US, China, India, Brazil, etc.) asserting regulatory independence against EU extraterritorial reach. Enact blocking statutes, data localization laws, and adequacy negotiations to resist GDPR's projection. Benefit from the territorial sovereignty reading as normative cover for resistance measures. Mobile exit: can choose confrontation, negotiation, or parallel regime-building.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, beneficiary,
    institutional, generational, mobile, national).

% Analyze jurisdictional conflict between territorial sovereignty and effects-based jurisdiction. Produce frameworks (Brussels Effect, digital sovereignty, jurisdictional pluralism) that shape state practice. Neither collect nor pay; their authority is epistemic. Exit is analytical — they shift frameworks, not positions.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified data protection standard for EU residents against global data processors, solving the coordination problem of fragmented national laws and ensuring baseline rights in cross-border digital services.
% TRANSFER_FUNCTION: Moves compliance costs, legal liability, and operational burden from EU regulators and data subjects to non-EU entities processing EU resident data, while transferring regulatory authority from non-EU states to EU institutions over extraterritorial data flows.
% ABSENT_VOICES: Small non-EU digital service providers in developing economies (trapped, resource-constrained), non-EU data subjects whose data is processed under GDPR but who lack EU citizenship and thus full procedural standing, and future generations subject to path-dependent jurisdictional architectures being locked in today.
% DISAPPEARANCE_RATIONALE: If GDPR extraterritorial scope vanished overnight, non-EU entities would immediately cease compliance investments, EU would lose regulatory leverage over global data flows, non-EU states would accelerate sovereign data regimes, and the global privacy governance landscape would fragment into competing national standards — the Brussels Effect regulatory gravity would collapse.
% FOUNDING_PROBLEM: Pre-GDPR, EU data protection law (Directive 95/46) applied only to controllers established in the EU, creating a regulatory gap: non-EU companies targeting EU residents operated without EU oversight, and data exported from the EU lost protection. The founding problem was the territorial loophole in the digital economy.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions attest the problem remains live (digital economy expands, new tracking technologies emerge). Non-EU states and international law scholars attest the founding problem is substantially solved for established players but the extraterritorial mechanism now overreaches — legislative history of Article 3(2) drafting (recitals 22-25) shows intent to close the loophole, not assert universal jurisdiction; US CLOUD Act and China PIPL enactments corroborate the jurisdictional conflict reading.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Claimed type is snare: from the territorial sovereignty reading, the coordination story (protecting EU residents) is real but the extraterritorial mechanism extracts compliance rents from non-EU entities and regulatory sovereignty from non-EU states. The constraint persists through active EU enforcement and the structural dependency of global firms on EU market access. Alternatives (data localization, market exit) are suppressed by economic necessity. Beneficiaries (EU institutions, EU data subjects) are identifiable and collect rents/rights; victims (non-EU entities, especially smaller providers) are identifiable and bear asymmetric costs. The metrics describe the constraint's actual operation; the claim reflects this reading's structural assessment.
 *
 * PERSPECTIVAL GAP:
 *   The territorial sovereignty reading computes the constraint as snare from non-EU payer seats (high extraction, suppressed alternatives, active enforcement). The EU agenda_setter seat would compute it as rope (genuine coordination, net benefit to EU residents, minimal coercion for those inside the framework). The engine computes this divergence from the structural data — the authored claim (snare) reflects the reading's seat, not a consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions are agenda_setters with institutional power and arbitrage-grade exit (they write the rules) — d near 0.0 (beneficiary). EU data subjects are beneficiaries with organized power but constrained exit — d ~0.2. Non-EU entities and global tech companies are payers with moderate/powerful power but constrained exit (market dependence) — d ~0.7-0.8. Non-EU digital service providers are payers with moderate power but trapped exit — d ~0.9. Non-EU states are beneficiaries of THIS READING (it validates their resistance) but payers of the constraint itself — dual position captured in stakeholder roles. International legal scholars are analytical observers — d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (territorial loophole in Directive 95/46) was live in 2016. By 2024, major non-EU processors have compliance infrastructure; the loophole is closed for established players. Yet the extraterritorial mechanism expands (new guidance on Art 3(2), ePrivacy proposal, AI Act extraterritoriality). The arrangement persists beyond its founding justification — mandatrophy unresolved. The territorial sovereignty reading exposes this: the constraint now serves EU regulatory power projection, not merely loophole closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the territorial_sovereignty_reading instantiate a distinct constraint from the effects_jurisdiction_reading and market_access_reading, or are they observably the same constraint evaluated differently?',
    'Apply ε-invariance test: if measuring the constraint via territorial sovereignty metrics (state resistance, blocking statutes, data localization costs) yields ε=0.72, but measuring via effects jurisdiction metrics (EU resident protection coverage, adequacy decisions) yields ε=0.35, then they are distinct constraints. The kernel label ''GDPR Article 3 scope'' conflates them.',
    'If distinct, each reading gets its own constraint story with independent ε, stakeholders, and classification. The territorial_sovereignty_reading classifies as snare; effects_jurisdiction_reading may classify as rope or tangled_rope. The engine must not average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints per ε-invariance principle.').

omega_variable(
    extraterritorial_legitimacy_boundary,
    'Where does legitimate regulatory authority end and illegitimate extraterritorial overreach begin in digital data flows?',
    'Customary international law evolution: state practice and opinio juris on digital jurisdiction (ICJ advisory opinions, UN GGE reports, regional court judgments). Track whether a consensus emerges on a ''digital territoriality'' principle.',
    'If a stable boundary emerges (e.g., ''targeting test'' accepted as legitimate), this reading''s foundational axiom loses holdable status → overridden. If boundary dissolves into power politics, the reading''s snare classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_legitimacy_boundary, conceptual, 'The irreducible normative disagreement on jurisdictional legitimacy in cyberspace.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine privacy coordination function be separated from the extraterritorial extraction mechanism, or are they structurally fused?',
    'Natural experiment: adequacy decisions and transfer mechanisms (SCCs, BCRs) that preserve privacy coordination without full extraterritorial scope. If third countries achieve ''essentially equivalent'' protection without submitting to GDPR extraterritoriality, the functions are separable.',
    'If separable, the measured extraction (0.72) overstates the constraint''s necessary cost — part is monopoly rent. If fused, the extraction is the price of coordination and the snare classification softens toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether GDPR''s coordination and extraction components are structurally separable.').

omega_variable(
    resistance_effectiveness_trajectory,
    'Will non-EU state resistance (data localization, blocking statutes, rival regimes) successfully fragment the Brussels Effect, or will EU regulatory gravity absorb them?',
    'Track adequacy decision flow, data localization law enactment rate, and cross-border data flow volume over 2024-2030. Fragmentation = rising localization, declining adequacy grants, stable/falling EU-bound data flows. Absorption = expanding adequacy network, localization laws becoming compliance theater, growing EU-bound flows.',
    'Fragmentation → constraint''s suppression requirement falls, resistance rises, theater rises (piton trajectory). Absorption → extractiveness stabilizes high, suppression becomes normalized, constraint hardens as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_effectiveness_trajectory, empirical, 'Trajectory of jurisdictional conflict between EU extraterritoriality and non-EU sovereign resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2018, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2028, 0.45).
narrative_ontology:measurement(gdpr_art3_terr_sov_tr_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2030, 0.48).

% Extraction over time
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2026, 0.72).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2028, 0.74).
narrative_ontology:measurement(gdpr_art3_terr_sov_be_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2030, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2028, 0.7).
narrative_ontology:measurement(gdpr_art3_terr_sov_su_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2030, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, eu_adequacy_decision_regime).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, us_cloud_act_extraterritoriality).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, china_pipl_data_localization).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, global_cross_border_data_flow_architecture).

% DUAL FORMULATION NOTE:
% This constraint family (gdpr_article_3_scope) decomposes the natural-language concept 'GDPR extraterritorial scope' into three structurally distinct readings with different ε values, stakeholder structures, and classifications. The territorial_sovereignty_reading (this story) sees a snare (ε=0.72) extracting from non-EU entities/states. The effects_jurisdiction_reading sees a rope/tangled_rope (ε~0.35) coordinating EU resident protection. The market_access_reading sees a scaffold/tangled_rope (ε~0.50) as conditional market access. They are linked via affects_constraints because the EU's enforcement of one reading creates the structural conditions the others describe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, powerful, 0.65).
constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
