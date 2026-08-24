% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities (Debtor Extraction Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities — the policy matrices attached to
 *   IMF and World Bank lending — are presented as technical coordination
 *   mechanisms ensuring fiscal sustainability and market confidence. This
 *   reading (debtor_extraction_reading) treats them as a snare: a coercive
 *   extraction regime whose persistence depends on suppressing alternatives
 *   (capital controls, sovereign default, regional monetary cooperation) and
 *   whose operation violently dismantles domestic social contracts to
 *   guarantee creditor returns. The constraint has evolved from 1980s crisis
 *   management into a permanent architecture of neo-colonial resource
 *   transfer. The claimed type is snare; the metrics describe high
 *   extraction, high suppression, moderate theater (increasing 'country
 *   ownership' rhetoric masking unchanged substance), high accessibility
 *   collapse (alternatives delegitimized as 'populism'), and sustained
 *   resistance (food riots, labor strikes, electoral backlash).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.78).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, 'b985fde0-90f8-4a55-a4bf-9208190625a9').
narrative_ontology:cs_kernel_codification('b985fde0-90f8-4a55-a4bf-9208190625a9', formalized).
narrative_ontology:cs_authority_grounding('b985fde0-90f8-4a55-a4bf-9208190625a9', extraction).
narrative_ontology:cs_interpretation_layer_present('b985fde0-90f8-4a55-a4bf-9208190625a9').
narrative_ontology:cs_reading_relation('b985fde0-90f8-4a55-a4bf-9208190625a9', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b985fde0-90f8-4a55-a4bf-9208190625a9', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('b985fde0-90f8-4a55-a4bf-9208190625a9', foundational, conditionalities_are_extractive_by_design).
narrative_ontology:cs_axiom_status(conditionalities_are_extractive_by_design, holdable).
narrative_ontology:cs_axiom_grounding('b985fde0-90f8-4a55-a4bf-9208190625a9', conditionalities_are_extractive_by_design, empirically_contingent).
narrative_ontology:cs_axiom('b985fde0-90f8-4a55-a4bf-9208190625a9', foundational, social_contract_dismantling_is_the_mechanism).
narrative_ontology:cs_axiom_status(social_contract_dismantling_is_the_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b985fde0-90f8-4a55-a4bf-9208190625a9', social_contract_dismantling_is_the_mechanism, deontological).
narrative_ontology:cs_reference_frame('b985fde0-90f8-4a55-a4bf-9208190625a9', bretton_woods_original_mandate).
narrative_ontology:cs_drift_state('b985fde0-90f8-4a55-a4bf-9208190625a9', post_washington_consensus_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('b985fde0-90f8-4a55-a4bf-9208190625a9', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, multilateral_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, vulnerable_groups).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, neoliberal_fiscal_discipline_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, market_fundamentalism).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_seniority_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, negotiates, and enforces conditionality packages as prerequisite for balance-of-payments support and debt restructuring. Staff missions dictate fiscal targets, privatization schedules, labor market reforms, and subsidy eliminations. The institutions' lending capacity and seal of approval gate access to all other official and private finance. They collect no direct rents but their organizational mandate, budget, and institutional relevance depend on maintaining the conditionality regime.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank, agenda_setter,
    institutional, generational, arbitrage, global).

% Commercial banks and bondholders hold sovereign debt claims that conditionality protects. IMF programs prioritize debt service over domestic spending, effectively insuring creditor returns with public funds. They participate in Paris Club and London Club restructuring only under IMF-certified programs. Their exit is costless — they diversify portfolios and price risk — but the regime guarantees seniority and reduces haircuts.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Multinational corporations gain market access, privatized assets, and deregulated environments through conditionality-mandated liberalization. They extract resource concessions, service contracts, and consumer markets opened by force. Their exit is trivial — they deploy capital globally — but the regime lowers entry barriers and political risk premiums in targeted economies.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the costs of austerity: user fees for health and education, removal of food and fuel subsidies, public sector wage freezes, and currency devaluation that erodes real incomes. No meaningful exit — emigration is restricted, informal survival strategies are criminalized, and political protest is met with repression enabled by 'stability' conditionalities. The social contract (state provides basic welfare in exchange for legitimacy) is unilaterally voided.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_populations, payer,
    powerless, biographical, trapped, national).

% Face mass retrenchment, wage compression, and pension cuts mandated by public expenditure ceilings. Unions are weakened by labor 'flexibility' conditionalities. Some exit to informal sector or emigration, but professional specificity and family ties constrain mobility. They organize resistance but face state capacity built by the same conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    moderate, biographical, constrained, national).

% Women, children, elderly, indigenous communities, and rural poor experience disproportionate harm: maternal mortality rises when clinics close; school enrollment drops when fees appear; traditional lands are privatized. Zero exit capacity — they lack documents, capital, and networks. Their voices are structurally absent from conditionality negotiations.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, vulnerable_groups, payer,
    powerless, immediate, trapped, local).

% Formally sovereign bodies reduced to ratifying externally drafted policy matrices. 'Prior actions' must be legislated before program approval; parliamentary debate is performative. Rejection triggers financial cutoff. Some legislators capture rents from privatization, creating complicity. Genuine legislative autonomy is the excluded alternative.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_legislatures, excluded,
    moderate, biographical, constrained, national).

% NGOs, unions, and community groups are consulted in 'participatory' processes that do not alter outcomes. Poverty Reduction Strategy Papers (PRSPs) were designed to legitimize, not empower. Funding dependence on Northern donors creates co-optation. They document harm but cannot veto conditions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_organizations, excluded,
    moderate, biographical, constrained, national).

% Produce the evidence base that both legitimizes and critiques conditionalities. Mainstream development economics provided the intellectual architecture; heterodox critics document extraction. Their analytical exit is complete — they choose frameworks — but career incentives align with institutional consensus.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, development_economists, observer,
    analytical, generational, analytical, global).

% Special rapporteurs and treaty bodies document conditionalities' incompatibility with economic, social, and cultural rights. Their findings have no enforcement power over Bretton Woods institutions. They provide normative counterweight but lack structural leverage.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, un_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates creditor claims on debtor resources through formalized conditionality frameworks, replacing ad hoc negotiations with standardized extraction protocols that synchronize multilateral, bilateral, and commercial creditor demands.
% TRANSFER_FUNCTION: Moves fiscal space, public assets, policy autonomy, and social provision from debtor populations to creditor institutions and transnational capital via debt service prioritization, privatization mandates, liberalization requirements, and austerity conditionalities.
% ABSENT_VOICES: Domestic legislatures, organized labor, indigenous communities, informal sector workers, and vulnerable populations who bear the costs but are excluded from conditionality negotiations — their opposition is managed through 'participatory' rituals that change nothing.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, debtor states would regain fiscal autonomy to redirect resources from debt service to social provision; creditor claims would face restructuring on debtor terms; public assets would revert to public control; the global financial architecture would reorganize around sovereign policy space rather than creditor discipline.
% FOUNDING_PROBLEM: The 1980s debt crisis created a coordination problem: how to ensure commercial banks and official creditors would continue lending to developing countries without ad hoc bailouts, and how to prevent disorderly defaults that threatened the international financial system.
% FOUNDING_PROBLEM_CORROBORATION: IMF Independent Evaluation Office (2003, 2011) acknowledges conditionalities expanded far beyond crisis resolution into structural transformation; UNCTAD Trade and Development Reports document the shift from liquidity provision to policy discipline; even creditor country treasuries (US Treasury, German Finance Ministry) in internal memos admit the regime now serves structural market creation, not crisis management.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.82) is high because conditionalities systematically redirect resources from social provision to debt service and asset transfer — the fiscal space captured exceeds any coordination benefit to debtors. Suppression (0.78) is high because the regime actively destroys alternatives: capital controls are prohibited, sovereign default is punished with market exclusion, regional financial arrangements are undermined. Theater (0.42) rises over time as 'country-driven' PRSPs and 'social spending floors' create performative participation while macro-frameworks remain immutable. Accessibility collapse (0.74) reflects the ideological closure: heterodox policies are treated as analytically illegitimate, not merely politically difficult. Resistance (0.68) is sustained but fragmented — food riots (1980s-90s), anti-privatization uprisings (2000s), recent debt justice movements — yet each wave is contained by the same enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor/agenda-setter seat, the constraint appears as necessary coordination — a rope that solves the collective action problem of sovereign lending. From the payer seats, it is pure extraction enforced by the threat of financial asphyxiation — a snare. The engine computes this divergence from the structural data: same constraint, opposite lived types. The 'coordination' story is real for creditors (they coordinate on seniority); for debtors, coordination is the cover story for extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   IMF/World Bank (agenda_setter) sits near symmetric (d~0.5): they administer extraction but their organizational survival depends on the regime — they are trapped in their own machinery. Creditor banks and transnational capital (beneficiaries) have d near 0.0: they collect guaranteed returns with arbitrage-grade exit. Debtor populations, public sector workers, vulnerable groups (payers) have d near 1.0: they bear full costs with trapped exit. Domestic legislatures and civil society (excluded) have d~0.7: they bear political costs without influence. The directionality gradient from creditor to debtor is the structural engine of the snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s liquidity crisis) is dead — resolved by Brady Bonds, HIPC, and the shift to bond finance. Yet the arrangement persists and intensifies. Mandatrophy is unresolved: the mandate has outlived its function by decades, but the institutions that administer it extract organizational relevance from its continuation. The mandate's 'death' is precisely what makes the constraint a snare rather than a scaffold — there is no sunset because the extractive function replaced the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement,
    'Is the structural_adjustment_conditionalities kernel a coordination mechanism, an extraction regime, or a selectively applied hybrid — and does the answer depend on which reading''s structural premises one accepts?',
    'Compare the three readings'' empirical predictions: creditor_coordination_reading predicts conditionalities correlate with improved market access and growth; debtor_extraction_reading predicts they correlate with social immiseration and resource outflow; hybrid_selectivity_reading predicts geopolitical alignment predicts conditionality severity. Longitudinal data on conditionality content, enforcement, and outcomes across 100+ programs since 1980.',
    'If debtor_extraction_reading is structurally accurate, the kernel is a snare and the coordination story is cover. If creditor_coordination_reading holds, the kernel is a rope with side effects. If hybrid_selectivity_reading holds, the kernel is a tangled rope with geopolitical modulation. Classification follows the reading''s structural truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement, conceptual, 'The kernel reading contest: which structural description matches the constraint''s actual operation?').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (financial cutoff, legal barriers, military-backed austerity) or internalized (policy elites'' ideological capture, epistemic closure among technocrats, normalized ''there is no alternative'')?',
    'Post-exit suppression trajectory: examine cases where countries escaped conditionality (e.g., Malaysia 1998 capital controls, Argentina 2002 default, Bolivia 2006 gas renationalization). If suppression persists after formal exit (ideological capture of successor governments, continued policy convergence), internalized component is significant.',
    'If internalized, effective suppression exceeds structural measure — the constraint colonizes the cognitive space of alternatives. This would raise the constraint''s extraction efficiency without increasing visible coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in debtor states under conditionality').

omega_variable(
    extraction_boundary,
    'Where does the genuine coordination function (if any) end and pure extraction begin? Is there a minimal conditionality set that solves the coordination problem without extraction?',
    'Counterfactual decomposition: identify which specific conditions (fiscal targets, privatization, liberalization, governance) correlate with restored market access vs. which correlate with resource transfer to creditors. Compare programs with narrow vs. broad conditionality.',
    'If a minimal coordination core exists, the constraint is a tangled rope (coordination + extraction). If no coordination core survives decomposition, it is a pure snare. The boundary determines whether any reform can salvage the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary, conceptual, 'The coordination-extraction boundary within conditionality packages').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'structural adjustment conditionalities' label into three structurally distinct readings. The debtor_extraction_reading (this story) identifies a snare: high extraction, coercive enforcement, victims are domestic populations. The creditor_coordination_reading identifies a rope: genuine coordination of creditor claims with mutual benefit. The hybrid_selectivity_reading identifies a tangled rope: coordination function exists but extraction is geopolitically modulated. They share the kernel but have different ε, different beneficiaries/victims, different types. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
