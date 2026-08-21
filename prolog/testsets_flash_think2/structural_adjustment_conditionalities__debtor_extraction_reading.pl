% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities (Debtor Extraction Reading)
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   This constraint story analyzes structural adjustment conditionalities
 *   from the perspective of debtor nations, framing them as extractive
 *   neo-colonial instruments. These conditionalities, imposed by
 *   international financial institutions (IFIs) in exchange for loans,
 *   mandate austerity, privatization, and market liberalization. This reading
 *   emphasizes how these policies violently dismantle existing social
 *   contracts, leading to severe social and economic costs for domestic
 *   populations, while primarily benefiting transnational capital and
 *   creditor banks. The constraint is characterized by high coercion and a
 *   clear transfer of wealth and policy autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.9).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.95).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, 'a7481e73-b1b9-4d8e-94d3-ea197a101006').
narrative_ontology:cs_kernel_codification('a7481e73-b1b9-4d8e-94d3-ea197a101006', formalized).
narrative_ontology:cs_authority_grounding('a7481e73-b1b9-4d8e-94d3-ea197a101006', extraction).
narrative_ontology:cs_interpretation_layer_present('a7481e73-b1b9-4d8e-94d3-ea197a101006').
narrative_ontology:cs_reading_relation('a7481e73-b1b9-4d8e-94d3-ea197a101006', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7481e73-b1b9-4d8e-94d3-ea197a101006', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('a7481e73-b1b9-4d8e-94d3-ea197a101006', foundational, debt_servicing_prioritizes_creditor_profit).
narrative_ontology:cs_axiom_status(debt_servicing_prioritizes_creditor_profit, holdable).
narrative_ontology:cs_axiom_grounding('a7481e73-b1b9-4d8e-94d3-ea197a101006', debt_servicing_prioritizes_creditor_profit, instrumental).
narrative_ontology:cs_axiom('a7481e73-b1b9-4d8e-94d3-ea197a101006', foundational, sovereignty_subordinate_to_debt_obligations).
narrative_ontology:cs_axiom_status(sovereignty_subordinate_to_debt_obligations, holdable).
narrative_ontology:cs_axiom_grounding('a7481e73-b1b9-4d8e-94d3-ea197a101006', sovereignty_subordinate_to_debt_obligations, conventional).
narrative_ontology:cs_reference_frame('a7481e73-b1b9-4d8e-94d3-ea197a101006', post_bretton_woods_consensus).
narrative_ontology:cs_drift_state('a7481e73-b1b9-4d8e-94d3-ea197a101006', contemporary_global_south_resistance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a7481e73-b1b9-4d8e-94d3-ea197a101006', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nations_domestic_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, social_service_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the brunt of austerity, privatization, and reduced social services. Their social contract is dismantled, with little to no say in the conditionalities, leading to widespread poverty and inequality.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nations_domestic_populations, payer,
    powerless, generational, trapped, national).

% Benefits from market liberalization, privatization of state assets, and access to new markets in debtor nations, often at fire-sale prices and with reduced labor protections.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital, beneficiary,
    powerful, generational, arbitrage, global).

% Ensure repayment of loans, often with high interest, and benefit from the enforcement of policies that favor their financial interests, minimizing their risk exposure.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks, beneficiary,
    institutional, biographical, arbitrage, global).

% Impose and monitor conditionalities, acting as enforcers for creditor interests. They frame these policies as necessary for 'fiscal discipline' and 'development', despite evidence of negative social impacts.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Are compelled to implement conditionalities to avoid default and maintain access to further credit, often against the will of their populations. Their policy sovereignty is severely curtailed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_governments, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_governments, agenda_setter).

% Suffer from job losses, wage freezes, and reduced benefits due to privatization and austerity measures mandated by conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    powerless, biographical, constrained, national).

% Experience direct loss of access to essential services like healthcare, education, and subsidies as public spending is cut to meet fiscal targets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, social_service_recipients, payer,
    powerless, immediate, trapped, national).

% Critique the conditionalities as neo-colonial and extractive, advocating for debt relief, reparations, and alternative development models that prioritize human rights and ecological sustainability.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, global_south_advocacy_groups, observer,
    powerless, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates fiscal policy and economic reforms in debtor nations to ensure debt repayment and 'sustainable' economic growth, as defined by international financial institutions and creditor nations.
% TRANSFER_FUNCTION: Transfers wealth, public assets, and policy autonomy from debtor nations (their populations and public sectors) to transnational capital and creditor banks through forced privatization, market liberalization, and austerity measures.
% ABSENT_VOICES: The domestic populations of debtor nations, particularly those most affected by cuts to social services and public sector jobs, are largely excluded from the negotiation and implementation of conditionalities. Their protests are often met with state repression, indirectly supported by the conditionalities' enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If structural adjustment conditionalities vanished overnight, debtor nations would regain policy space, potentially re-nationalizing privatized assets, restoring social services, and pursuing alternative development paths. This would significantly alter global capital flows, challenge the power of international financial institutions, and force a fundamental renegotiation of international debt architecture, leading to a more equitable global economic order.
% FOUNDING_PROBLEM: To address sovereign debt crises in developing countries by imposing fiscal discipline and market-oriented reforms, framed as necessary for economic stability and growth, particularly in the wake of the 1970s oil shocks and rising interest rates.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and creditor nations assert the problem is still live, citing ongoing fiscal imbalances and the need for 'responsible' governance. Debtor nations, civil society organizations, and independent economists widely contest this, arguing the original problem was misdiagnosed or has been superseded by the extractive and neo-colonial nature of the conditionalities themselves, with corroboration from historical economic data, social impact assessments, and UN reports.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.9) due to the significant transfer of public assets and wealth from debtor nations to foreign entities, coupled with the imposition of policies that depress wages and reduce social safety nets. Suppression is also very high (0.95) because debtor nations face severe penalties (e.g., default, exclusion from future credit) if they do not comply, leaving them with virtually no viable alternatives. The 'violent dismantling' refers to the social and economic upheaval caused by these policies, often leading to protests and state repression. Theater ratio is low (0.1) because the enforcement is direct and the extractive function is central, not merely performative; the coordination narrative serves as a thin cover for the underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IFIs and creditor nations (as captured by the 'creditor_coordination_reading' sibling), these conditionalities are presented as necessary coordination mechanisms for fiscal sustainability and market confidence. However, from the 'debtor_extraction_reading' perspective, the same structure operates as a coercive regime designed to extract wealth and impose a specific economic model, with devastating consequences for the populations involved. The engine's computation of per-seat classifications will highlight this fundamental divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational capital and creditor banks are clear beneficiaries, receiving direct financial gains and expanded market access (low directionality). International financial institutions act as agenda-setters, enforcing the conditionalities that serve these beneficiaries. Debtor nations' domestic populations, public sector workers, and social service recipients are the primary targets, bearing the costs of austerity and privatization (high directionality). Debtor nation governments are caught in a constrained position, compelled to implement policies that harm their citizens to avoid national default.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was ostensibly to stabilize economies and promote growth in debtor nations. However, this reading argues that the founding problem is now 'contested' and largely 'dead' in its original, benign form. The persistence of conditionalities, despite widespread evidence of their failure to achieve equitable development and their role in exacerbating inequality, indicates a shift towards a purely extractive function. The high extractiveness and suppression, coupled with the contested founding problem status, strongly suggest a snare, where the coordination story is a cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_debt_origin,
    'To what extent is the sovereign debt itself legitimate, given historical contexts of colonial exploitation, predatory lending practices, and often corrupt regimes that incurred the debt?',
    'Historical and legal audits of debt origination, including analysis of odious debt claims and the role of external actors in facilitating unsustainable borrowing.',
    'If a significant portion of the debt is deemed illegitimate, it would fundamentally undermine the moral and legal basis for conditionalities, reclassifying them as pure coercion rather than a response to legitimate obligations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_debt_origin, conceptual, 'Ambiguity regarding the moral and legal legitimacy of the underlying sovereign debt.').

omega_variable(
    efficacy_of_reforms_vs_extraction,
    'Do the market-oriented reforms mandated by conditionalities genuinely lead to sustainable and equitable economic growth, or do they primarily facilitate wealth transfer and exacerbate poverty and inequality?',
    'Longitudinal comparative studies of countries that implemented conditionalities versus those that pursued alternative development paths, controlling for other economic factors, and focusing on social indicators (poverty, inequality, health, education).',
    'Empirical evidence strongly disproving the efficacy claims would further solidify the ''snare'' classification by removing any remaining coordination justification, highlighting the purely extractive nature. Evidence of genuine, equitable growth would challenge this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_of_reforms_vs_extraction, empirical, 'Whether conditionalities achieve their stated development goals or primarily serve extractive interests.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the high suppression experienced by debtor nations primarily due to external structural coercion (e.g., threat of default, exclusion from credit) or has it become internalized (e.g., a belief among policymakers that ''there is no alternative'' to IFI policies)?',
    'Analysis of policy discourse and decision-making processes within debtor governments, alongside the actual availability and viability of alternative financing and development strategies. If alternatives are structurally available but not pursued due to perceived inevitability, internalized suppression is present.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as it persists even if external barriers are partially relaxed, making exit more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism for debtor nations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(stru_tr_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(stru_tr_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(stru_tr_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(stru_tr_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.1).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(stru_be_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1986, 0.83).
narrative_ontology:measurement(stru_be_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1992, 0.86).
narrative_ontology:measurement(stru_be_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1998, 0.88).
narrative_ontology:measurement(stru_be_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2004, 0.89).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(stru_su_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1986, 0.88).
narrative_ontology:measurement(stru_su_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1992, 0.91).
narrative_ontology:measurement(stru_su_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1998, 0.93).
narrative_ontology:measurement(stru_su_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2004, 0.94).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, global_debt_architecture).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, sovereign_debt_markets).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, privatization_of_public_assets).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, global_south_economic_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'structural_adjustment_conditionalities' kernel. This 'debtor_extraction_reading' emphasizes the coercive and extractive nature of conditionalities, contrasting with the 'creditor_coordination_reading' (which frames them as necessary for fiscal stability) and the 'hybrid_selectivity_reading' (which focuses on differential application).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
