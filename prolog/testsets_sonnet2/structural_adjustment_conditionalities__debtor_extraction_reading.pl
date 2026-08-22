% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Structural Adjustment Conditionalities (Debtor-Extraction Reading)
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This story authors the debtor-extraction reading of the
 *   structural_adjustment_conditionalities kernel: conditionality packages
 *   imposed as loan preconditions by international lending institutions are
 *   read here as a coercive extraction regime in which the stated
 *   coordination rationale (orderly creditor recovery) functions as cover for
 *   a durable transfer from domestic populations in debtor states to
 *   transnational creditor banks, bondholders, and creditor-state commercial
 *   interests. The referent for extractiveness is the standing conditionality
 *   arrangement as this reading sees it operating — not the debt-relief or
 *   sovereign-discretion alternative this reading would prefer. Two sibling
 *   readings of the same kernel are authored as separate constraints:
 *   creditor_coordination_reading treats the identical arrangement as
 *   necessary fiscal discipline with low extraction, and
 *   hybrid_selectivity_reading treats enforcement as asymmetric across
 *   strategically important versus peripheral debtors. All three share the
 *   same kernel text (loan conditionality) but diverge sharply in authored ε,
 *   beneficiary/victim structure, and claimed type — exactly the
 *   decomposition the ε-invariance principle requires rather than a single
 *   averaged story.
 *
 * KEY AGENTS:
 *   - transnational_creditor_banks: Primary beneficiary (institutional/arbitrage) — collects continued debt service protected by imposed fiscal discipline
 *   - lending_institution_agenda_setter: Sets and enforces conditionality terms (institutional/analytical) — controls disbursement leverage
 *   - domestic_public_sector_workers, rural_subsistence_populations, urban_poor_service_dependents: Primary targets (powerless/trapped) — bear job loss, service withdrawal, and price shocks
 *   - debtor_state_government: Intermediary agenda-setter and payer (moderate/constrained) — administers imposed terms under external leverage, absorbing domestic political cost
 *   - development_economists_and_auditors: Analytical observer — compiles the cross-country empirical record cited by all sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.86).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.79).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor-Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '65111c7b-7c38-4d5a-9bc8-e83911819d43').
narrative_ontology:cs_kernel_codification('65111c7b-7c38-4d5a-9bc8-e83911819d43', formalized).
narrative_ontology:cs_authority_grounding('65111c7b-7c38-4d5a-9bc8-e83911819d43', extraction).
narrative_ontology:cs_interpretation_layer_present('65111c7b-7c38-4d5a-9bc8-e83911819d43').
narrative_ontology:cs_reading_relation('65111c7b-7c38-4d5a-9bc8-e83911819d43', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('65111c7b-7c38-4d5a-9bc8-e83911819d43', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('65111c7b-7c38-4d5a-9bc8-e83911819d43', foundational, conditionality_functions_as_repayment_priority_device).
narrative_ontology:cs_axiom_status(conditionality_functions_as_repayment_priority_device, holdable).
narrative_ontology:cs_axiom_grounding('65111c7b-7c38-4d5a-9bc8-e83911819d43', conditionality_functions_as_repayment_priority_device, empirically_contingent).
narrative_ontology:cs_axiom('65111c7b-7c38-4d5a-9bc8-e83911819d43', secondary, sovereign_fiscal_discretion_is_a_protected_domestic_entitlement).
narrative_ontology:cs_axiom_status(sovereign_fiscal_discretion_is_a_protected_domestic_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('65111c7b-7c38-4d5a-9bc8-e83911819d43', sovereign_fiscal_discretion_is_a_protected_domestic_entitlement, deontological).
narrative_ontology:cs_reference_frame('65111c7b-7c38-4d5a-9bc8-e83911819d43', post_bretton_woods_creditor_priority_regime).
narrative_ontology:cs_drift_state('65111c7b-7c38-4d5a-9bc8-e83911819d43', post_debt_crisis_accumulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65111c7b-7c38-4d5a-9bc8-e83911819d43', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, bondholder_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_export_sectors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_subsistence_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_dependents).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, national_industrial_base).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sovereign debt instruments and receive continued debt service because conditionality programs prioritize repayment over domestic spending. They face no conditionality themselves, can restructure exposure across many debtor countries simultaneously, and exit any single relationship without consequence to their portfolio.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Holds tradeable sovereign bonds whose value is protected by austerity commitments that preserve debt-servicing capacity. Can sell positions instantly if a country appears likely to default or resist terms, moving capital to the next opportunity with no exposure to the domestic consequences of the program it demanded.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, bondholder_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from currency devaluation and trade liberalization conditions that open debtor markets to their exports and give them privileged access to newly privatized assets and resource concessions at depressed prices.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_export_sectors, beneficiary,
    organized, generational, mobile, global).

% Designs, imposes, and monitors the conditionality package as a precondition for disbursement — mandated fiscal targets, privatization schedules, subsidy removal, currency liberalization. Frames the program as necessary discipline; controls the metrics by which compliance is judged and can withhold tranches unilaterally, giving it leverage no debtor institution can match.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, lending_institution_agenda_setter, agenda_setter,
    institutional, civilizational, analytical, global).

% Lose jobs, wages, or pensions as mandated public-sector wage freezes and headcount reductions are enforced to meet fiscal targets. Have no vote in the negotiation, no legal standing before the lending institution, and no realistic option to relocate employment across borders.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers, payer,
    powerless, biographical, trapped, national).

% Lose agricultural subsidies and price supports mandated for removal under conditionality, exposing them directly to volatile global commodity prices they cannot hedge against. Geographic isolation and lack of capital make relocation or occupational shift effectively impossible.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, rural_subsistence_populations, payer,
    powerless, biographical, trapped, national).

% Depend on subsidized healthcare, education, and food programs that are cut or fee-ized under conditionality's fiscal consolidation requirements. Bear the health and mortality consequences of service withdrawal with no alternative provider and no capacity to exit the jurisdiction.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_dependents, payer,
    powerless, biographical, trapped, national).

% Domestic manufacturers lose tariff protection and directed credit mandated for removal, and are frequently undercut by subsidized imports from creditor-state exporters entering under the same liberalization terms. Some firms can partially relocate operations or seek foreign partnership; most cannot survive the transition.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, national_industrial_base, payer,
    moderate, generational, constrained, national).

% Signs and administers the conditionality agreement because the alternative — default and exclusion from capital markets — is treated as unacceptable by its own political and economic elites. Implements the mandated cuts and privatizations domestically, absorbing the political cost of decisions made under external leverage, while its own leadership sometimes benefits from advisory posts or asset transfers tied to implementation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government, payer).

% Mobilize against austerity terms through strikes and protest but are not party to the negotiation between the lending institution and the debtor government. Their objections are documented in aftermath reporting but were never part of the conditionality design process.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_and_labor_movements, excluded,
    organized, biographical, trapped, national).

% Study conditionality outcomes across decades of case data — poverty indicators, growth trajectories, debt-service ratios — and produce the empirical record used, contested, and selectively cited by all other parties to the dispute.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, development_economists_and_auditors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, conditionality coordinates expectations between a sovereign borrower unable to service its obligations and a fragmented creditor pool that would otherwise engage in a destructive scramble to be repaid first — a genuine collective-action problem among creditors that conditionality nominally solves by imposing uniform terms.
% TRANSFER_FUNCTION: Moves continued debt service and market access to transnational creditor banks and bondholders, moves market entry and asset acquisition opportunities to creditor-state export and investment sectors, and extracts the shortfall from domestic public-sector employment, subsidized services, and protected industry inside the debtor state.
% ABSENT_VOICES: Public-sector workers, rural populations, and urban service-dependents whose livelihoods and health outcomes are the direct object of the cuts are never present at the negotiating table; civil society and labor movements protest afterward but have no seat in conditionality design. Parliamentary bodies in the debtor state are frequently bypassed via emergency executive authority to meet disbursement deadlines.
% DISAPPEARANCE_RATIONALE: If conditionality enforcement vanished, debtor governments would regain discretion over fiscal composition, could reverse privatization and subsidy cuts, and creditor recovery would depend on renegotiation rather than externally imposed leverage — public spending patterns, currency regimes, and ownership of formerly state-run assets would visibly reorganize within a single budget cycle.
% FOUNDING_PROBLEM: Sovereign debt crises historically produced uncoordinated creditor grabs and chaotic default cascades; conditionality was framed as a mechanism to restore fiscal credibility and orderly repayment while preventing a spiral into deeper crisis.
% FOUNDING_PROBLEM_CORROBORATION: The lending institution and creditor banks attest the founding problem — fiscal indiscipline and credit risk — remains live in each debtor case. Independent development economists, UN human rights rapporteurs, and post-program audits from outside the creditor coalition attest that decades of repeated, near-identical conditionality packages across dissimilar economies indicate the arrangement now functions primarily to secure repayment priority and market access rather than to solve country-specific fiscal problems.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.86, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.86 at interval end) because, under this reading, the fiscal targets embedded in conditionality packages are set to prioritize continued external debt service over domestic social provisioning, producing a durable transfer whose magnitude is decoupled from any plausible efficiency gain to the debtor economy. Suppression is authored high (0.79) because compliance is not voluntary — disbursement withholding, credit-rating consequences, and capital-market exclusion function as coercive levers with no meaningful appeal mechanism available to the populations who bear the cost. Theater ratio rises across the interval (0.20 to 0.42) reflecting this reading's view that as repeated program failures accumulated evidence against the stated developmental rationale, an increasing share of program design shifted toward performative safeguards (poverty and social protection floors, gender-responsive budgeting language) layered onto substantively unchanged fiscal targets. Accessibility collapse (0.72) and resistance (0.68) are both authored moderately-high: alternatives (unilateral default, regional financing arrangements, debt restructuring outside the conditionality framework) exist in principle but are foreclosed in practice by capital-market exclusion threats, while resistance from domestic labor and civil-society movements is sustained but structurally unable to alter negotiated terms.
 *
 * PERSPECTIVAL GAP:
 *   The lending-institution seat and the debtor-population seats should compute to different types from the same structural facts: from the institutional seat, uniform conditionality genuinely solves a creditor coordination-failure problem and looks rope-like; from the trapped domestic-population seats, the identical mechanism operates as enforced, non-consensual transfer with no meaningful alternative and looks snare-like. This divergence is exactly what the engine's per-seat computation is designed to surface, and it is why this reading is authored as its own constraint rather than blended with the creditor_coordination_reading sibling.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational creditor banks and bondholders sit at the extreme beneficiary end: they are structurally insulated from the domestic consequences of the program, retain arbitrage-grade exit (can sell exposure and move to other debtor relationships), and their d is derived low from the beneficiary declaration plus their global/arbitrage positioning. Domestic public-sector workers, rural populations, and urban service-dependents sit at the extreme target end: trapped exit, powerless power atom, and national/local scope combine with their victim declaration to push derived d high — they cannot exit the jurisdiction or the labor market segment being cut, and effective extraction is amplified accordingly by the engine. The debtor state government occupies a genuinely dual position — nominally an agenda-setter administering the program domestically, but itself constrained and partly a payer of the arrangement's political costs, which is why it carries both agenda_setter and payer roles rather than being forced into one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — chaotic, uncoordinated creditor recovery attempts destabilizing both debtor and creditor interests — was genuinely live in early crisis episodes. This reading holds that the problem has since been substantially resolved or superseded by other multilateral mechanisms (collective action clauses, sovereign debt restructuring frameworks), yet the conditionality apparatus persists and has, in this reading's account, been repurposed primarily as a repayment-priority and market-access mechanism. The founding_problem_status is authored contested rather than flatly dead because the lending institution's own economists continue to document genuine fiscal fragility in specific debtor cases — the ambiguity is real, not manufactured, and is why founding_problem_status is deliberately not resolved to 'dead' despite this reading's extractive framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_extraction_boundary,
    'Is the conditionality mechanism''s coordination function (preventing chaotic multi-creditor grabs) structurally separable from its extractive function (prioritizing repayment over domestic provisioning), or are they the same mechanism viewed from different seats?',
    'Comparative case analysis: debt restructurings conducted under non-conditional frameworks (e.g., some regional financing arrangements or unilateral moratoria) versus IMF/World-Bank-style conditional programs, holding initial fiscal distress constant, to see whether coordination benefits persist without the extractive fiscal-target apparatus.',
    'If separable, this reading''s snare classification targets only the extractive layer, and a genuine rope-like coordination core could in principle be extracted and preserved; if inseparable, the entire mechanism is extractive by construction and no reform short of abolition addresses the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_extraction_boundary, conceptual, 'Whether conditionality''s coordination and extraction functions can be structurally disentangled.').

omega_variable(
    debtor_government_complicity_or_capture,
    'Is the debtor state government a genuinely coerced intermediary with no real alternative, or is it partially captured — its elites benefiting from privatization proceeds, advisory sinecures, or asset transfers tied to program implementation — such that the ''external coercion'' framing understates domestic elite complicity?',
    'Trace beneficial ownership of privatized assets and post-program career paths of debtor-state officials who negotiated and implemented conditionality terms across multiple country cases.',
    'If capture is substantial, part of the extraction attributed to external creditors should instead be attributed to a domestic elite beneficiary class, changing the beneficiary declaration and reducing the purity of the neo-colonial extraction framing without eliminating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debtor_government_complicity_or_capture, empirical, 'Degree of domestic elite capture versus genuine external coercion in program implementation.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Under the CS-framing under-determination guidance, is the debtor-extraction reading''s selection over the creditor-coordination reading justified by the observable long-run poverty and de-industrialization record, or does that same record admit the hybrid-selectivity reading''s account equally well?',
    'Cross-reading comparison of outcome data disaggregated by debtor geopolitical strategic value — if outcomes are uniformly poor regardless of strategic importance, debtor_extraction_reading is better supported; if outcomes vary sharply by strategic importance with waivers concentrated among allies, hybrid_selectivity_reading is better supported.',
    'Determines which sibling reading the empirical record most strongly corroborates, without changing this story''s own authored ε (each reading''s ε is fixed independent of this comparison per the ε-invariance principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Whether the extraction framing or the selectivity framing better fits the disaggregated outcome record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 40, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the structural_adjustment_conditionalities kernel, each authored as a separate constraint per the ε-invariance principle. debtor_extraction_reading (this file, ε=0.86, snare) treats the arrangement as coercive extraction with domestic populations as victims and transnational capital as beneficiary. creditor_coordination_reading treats the identical kernel text as necessary fiscal coordination with low ε. hybrid_selectivity_reading treats enforcement as geopolitically selective, applied harshly to weak debtors and waived for strategic ones. The three do not average into one constraint; they are linked via network.affects_constraints and share no ε value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
