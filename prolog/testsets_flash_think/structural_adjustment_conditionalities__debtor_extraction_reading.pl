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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities (Debtor Extraction Reading)
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   Structural Adjustment Conditionalities (SACs) are policy prescriptions
 *   imposed by International Financial Institutions (IFIs) like the IMF and
 *   World Bank on debtor nations in exchange for loans or debt relief. This
 *   'debtor extraction' reading frames SACs as neo-colonial instruments
 *   designed to dismantle social contracts, privatize public assets, and open
 *   markets for the benefit of transnational capital and creditor banks,
 *   rather than genuinely fostering sustainable development. The high
 *   extractiveness and suppression reflect the coercive nature of these
 *   policies and the limited alternatives available to indebted states.
 *
 * KEY AGENTS:
 *   - International Financial Institutions (IMF, World Bank): Agenda-setter (institutional/arbitrage) — imposes conditionalities.
 *   - Creditor Banks: Beneficiary (institutional/mobile) — receives repayment, benefits from market access.
 *   - Transnational Capital: Beneficiary (organized/arbitrage) — benefits from privatization, deregulation.
 *   - Debtor Nations' Governments: Payer (institutional/constrained) — implements austerity, faces domestic unrest.
 *   - Domestic Populations (Debtor Nations): Payer/Victim (powerless/trapped) — bears the brunt of austerity.
 *   - Civil Society Organizations (Debtor Nations): Excluded (organized/constrained) — resists conditionalities, limited influence.
 *   - Analytical Observers: Observer (analytical/analytical) — critiques the extractive nature of SACs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.85).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.9).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '07f554f1-f080-4f28-b628-70951c4af8b9').
narrative_ontology:cs_kernel_codification('07f554f1-f080-4f28-b628-70951c4af8b9', formalized).
narrative_ontology:cs_authority_grounding('07f554f1-f080-4f28-b628-70951c4af8b9', extraction).
narrative_ontology:cs_interpretation_layer_present('07f554f1-f080-4f28-b628-70951c4af8b9').
narrative_ontology:cs_reading_relation('07f554f1-f080-4f28-b628-70951c4af8b9', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('07f554f1-f080-4f28-b628-70951c4af8b9', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('07f554f1-f080-4f28-b628-70951c4af8b9', foundational, debt_as_leveraged_control).
narrative_ontology:cs_axiom_status(debt_as_leveraged_control, holdable).
narrative_ontology:cs_axiom_grounding('07f554f1-f080-4f28-b628-70951c4af8b9', debt_as_leveraged_control, instrumental).
narrative_ontology:cs_axiom('07f554f1-f080-4f28-b628-70951c4af8b9', foundational, social_contract_subordination_to_creditor_interests).
narrative_ontology:cs_axiom_status(social_contract_subordination_to_creditor_interests, holdable).
narrative_ontology:cs_axiom_grounding('07f554f1-f080-4f28-b628-70951c4af8b9', social_contract_subordination_to_creditor_interests, conventional).
narrative_ontology:cs_reference_frame('07f554f1-f080-4f28-b628-70951c4af8b9', post_bretton_woods_creditor_hegemony).
narrative_ontology:cs_drift_state('07f554f1-f080-4f28-b628-70951c4af8b9', contemporary_debt_crisis_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('07f554f1-f080-4f28-b628-70951c4af8b9', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nations_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations_debtor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes conditionalities on debtor nations, dictating economic policy reforms. Benefits from the enforcement of these policies, ensuring debt repayment and expanding their influence over global economic governance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives repayment of loans, often with favorable terms secured by conditionalities. Benefits from market liberalization and privatization in debtor nations, creating new investment opportunities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks, beneficiary,
    institutional, biographical, mobile, global).

% Benefits from the opening of markets, privatization of state-owned enterprises, and deregulation in debtor nations, leading to increased profits and access to cheap labor and resources.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital, beneficiary,
    organized, biographical, arbitrage, global).

% Accepts conditionalities to avoid sovereign default and maintain access to international finance. Implements austerity measures, privatizes public services, and deregulates markets, often facing severe domestic political and social unrest.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nations_governments, payer,
    institutional, immediate, constrained, national).

% Bears the direct costs of conditionalities through cuts to social services (health, education), job losses from privatization, increased cost of living, and erosion of social safety nets. Has no direct voice in the policy formulation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations_debtor_nations, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations_debtor_nations, excluded).

% Advocates for alternative development policies, debt cancellation, and protection of public services. Actively resists conditionalities through protests and advocacy but has limited institutional power to influence the process.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_organizations_debtor_nations, excluded,
    organized, biographical, constrained, national).

% Critiques structural adjustment conditionalities as perpetuating neo-colonial power dynamics and exacerbating inequality. Provides independent analysis of their economic and social impacts.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate fiscal stability and market integration for debtor nations, ostensibly to restore economic health and ensure repayment capacity. From this reading, its primary function is to coordinate the extraction of wealth and resources for creditors.
% TRANSFER_FUNCTION: Transfers public assets, social services, and national sovereignty from debtor nations to transnational capital and creditor institutions through forced privatization, deregulation, and austerity measures.
% ABSENT_VOICES: Domestic populations, labor unions, and local civil society organizations in debtor nations are largely excluded from the negotiation process. They would advocate for sovereign development paths, debt cancellation, and protection of public services, but their input is systematically marginalized.
% DISAPPEARANCE_RATIONALE: If conditionalities and their enforcement vanished overnight, debtor nations would regain policy space, potentially re-nationalize privatized assets, re-invest in social services, and pursue alternative, nationally-tailored development models. This would fundamentally alter global financial power dynamics, capital flows, and the economic sovereignty of developing nations.
% FOUNDING_PROBLEM: To address sovereign debt crises in developing nations by imposing fiscal discipline and market-oriented reforms, ostensibly to restore economic stability and ensure repayment capacity for international lenders.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists, development scholars, and civil society groups from both debtor and some creditor nations corroborate that the original problem of fiscal indiscipline has been substantially superseded by the mechanism's extractive function. The international financial institutions and creditor banks, however, maintain the problem is still live and the conditionalities are necessary.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant transfer of wealth and policy autonomy from debtor nations to creditors and transnational capital. Suppression (0.90) is severe due to the lack of viable alternatives for indebted states, which face severe economic consequences if they refuse conditionalities. The low theater ratio (0.10) indicates that the extractive function is direct and functional, with the 'coordination' narrative serving as a thin cover for the actual transfers. The increasing extractiveness over the interval reflects the deepening and hardening of these policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor nations' populations and critical observers, SACs are a snare, coercively extracting resources and dismantling social protections. From the perspective of IFIs and creditor banks (as captured in the 'creditor coordination' reading), these are necessary, albeit difficult, measures for fiscal discipline and market efficiency. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International Financial Institutions, creditor banks, and transnational capital are clear beneficiaries (low d) as they gain financially and politically. Debtor nations' governments are payers (high d) as they implement policies that harm their populations. Domestic populations are victims (highest d) as they bear the direct costs with no exit. Civil society organizations are excluded, their resistance highlighting the suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the original mandate of SACs (fiscal stability) has atrophied or was always a cover, and the constraint now primarily serves an extractive function. The 'dead' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, strongly suggests a snare or a highly extractive tangled rope, preventing mislabeling it as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the primary function of structural adjustment conditionalities genuine fiscal coordination and stability, or is it primarily an instrument of wealth extraction and neo-colonial control?',
    'Comparative analysis of long-term economic outcomes in countries that adopted SACs versus those that pursued alternative, sovereign development paths, controlling for initial conditions. Also, detailed forensic accounting of capital flows and asset transfers.',
    'If primarily extractive, the constraint is a snare; if primarily coordination, it would be a rope or tangled rope. The ''debtor extraction'' reading asserts the former.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, conceptual, 'The fundamental structural ambiguity of SACs'' true purpose.').

omega_variable(
    founding_problem_status_contest,
    'Is the founding problem of fiscal indiscipline and lack of market integration in debtor nations still ''live'', or has it been ''dead'' for decades, with conditionalities persisting for other reasons?',
    'Independent, non-IFI-funded audits of debtor nations'' fiscal health and market functionality, combined with historical analysis of the evolution of global financial architecture and power dynamics.',
    'If ''dead'', it strengthens the argument for SACs as a snare or piton, persisting due to inertia or for extractive purposes. If ''live'', it lends more credence to the ''creditor coordination'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_contest, empirical, 'Contest over the continued relevance of SACs'' original justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(stru_tr_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1986, 0.13).
narrative_ontology:measurement(stru_tr_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(stru_tr_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1998, 0.11).
narrative_ontology:measurement(stru_tr_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.1).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(stru_be_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1986, 0.75).
narrative_ontology:measurement(stru_be_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1992, 0.8).
narrative_ontology:measurement(stru_be_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1998, 0.83).
narrative_ontology:measurement(stru_be_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2004, 0.84).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(stru_su_t1986, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1986, 0.8).
narrative_ontology:measurement(stru_su_t1992, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1992, 0.85).
narrative_ontology:measurement(stru_su_t1998, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1998, 0.88).
narrative_ontology:measurement(stru_su_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2004, 0.89).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
