% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are loan-disbursement requirements
 *   imposed by the IMF and World Bank on debtor governments, mandating
 *   privatization, subsidy cuts, trade liberalization, labor deregulation,
 *   and reduced public spending. This constraint represents ONE READING of
 *   the contested conditionality kernel: the debtor-extraction reading, which
 *   frames conditionalities as coercive mechanisms that transfer wealth from
 *   domestic populations to transnational creditors and foreign investors by
 *   dismantling public services, suppressing wages, and capturing state
 *   assets. The alternate readings (creditor-coordination reading,
 *   hybrid-selectivity reading) are distinct constraints with different
 *   beneficiary/victim structures and different ε values; they are not
 *   alternatives within this constraint but separate analyses. This
 *   constraint is authored from the structural position of debtor populations
 *   and their analysts who experience the regime as extraction enforced by
 *   capital flight threats, IMF loan cancellation, and international
 *   isolation.
 *
 * KEY AGENTS:
 *   - IMF/World Bank leadership (agenda-setter, institutional power): sets loan conditions, administers tranches, maintains discretionary authority over stringency and selectivity
 *   - Transnational creditor banks (beneficiary, powerful): recoup loans through forced privatization, asset capture, and wage suppression; operate outside cost-bearing jurisdiction
 *   - Foreign investors (beneficiary, powerful): acquire state assets at distressed prices; exploit labor and environmental deregulation; exit when profitable
 *   - Domestic poor and working classes (victim, powerless): lose access to public goods, face wage cuts, unemployment, subsidy removal, and out-of-pocket service costs; geographic immobility traps them in cost-bearing jurisdiction
 *   - Public sector workers (victim, moderate power): face layoffs, wage freezes, pension cuts; lose job security and collective bargaining protections
 *   - Small farmers (victim, powerless): lose subsidy access, face land seizure, forced into debt-peonage; trapped in geography with no alternative income
 *   - Debtor state government (constrained payer/observer): formally 'chooses' conditionalities but faces capital flight threat and isolation if it refuses; becomes instrument for wealth transfer
 *   - Domestic poor organizers (excluded, powerless): criminalized, detained; their presence in negotiations would disrupt the extraction regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.89).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '59960e72-0fbe-45aa-8105-69772e8a4b4d').
narrative_ontology:cs_kernel_codification('59960e72-0fbe-45aa-8105-69772e8a4b4d', formalized).
narrative_ontology:cs_authority_grounding('59960e72-0fbe-45aa-8105-69772e8a4b4d', extraction).
narrative_ontology:cs_interpretation_layer_present('59960e72-0fbe-45aa-8105-69772e8a4b4d').
narrative_ontology:cs_reading_relation('59960e72-0fbe-45aa-8105-69772e8a4b4d', structural_adjustment_conditionalities__creditor_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('59960e72-0fbe-45aa-8105-69772e8a4b4d', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('59960e72-0fbe-45aa-8105-69772e8a4b4d', foundational, debtor_populations_bear_adjustment_costs).
narrative_ontology:cs_axiom_status(debtor_populations_bear_adjustment_costs, holdable).
narrative_ontology:cs_axiom_grounding('59960e72-0fbe-45aa-8105-69772e8a4b4d', debtor_populations_bear_adjustment_costs, empirically_contingent).
narrative_ontology:cs_axiom('59960e72-0fbe-45aa-8105-69772e8a4b4d', foundational, conditionality_extraction_decoupled_from_coordination).
narrative_ontology:cs_axiom_status(conditionality_extraction_decoupled_from_coordination, holdable).
narrative_ontology:cs_axiom_grounding('59960e72-0fbe-45aa-8105-69772e8a4b4d', conditionality_extraction_decoupled_from_coordination, empirically_contingent).
narrative_ontology:cs_axiom('59960e72-0fbe-45aa-8105-69772e8a4b4d', secondary, creditor_enforcement_via_financing_threat).
narrative_ontology:cs_axiom_status(creditor_enforcement_via_financing_threat, holdable).
narrative_ontology:cs_axiom_grounding('59960e72-0fbe-45aa-8105-69772e8a4b4d', creditor_enforcement_via_financing_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('59960e72-0fbe-45aa-8105-69772e8a4b4d', creditor_collective_action_solving).
narrative_ontology:cs_drift_state('59960e72-0fbe-45aa-8105-69772e8a4b4d', contemporary_neoliberal_era_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('59960e72-0fbe-45aa-8105-69772e8a4b4d', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank_bureaucracy).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_investors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, import_competing_domestic_elites).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_poor_and_working_classes).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, small_farmers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_governments_oecd).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the conditionality framework, designs loan disbursement tranches keyed to specific policy reforms, and administers ongoing surveillance of compliance. Justifies conditionalities as technical requirements for fiscal sustainability and macroeconomic stability. Maintains discretionary authority over which governments face stringent conditions and which receive favorable treatment.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank_leadership, agenda_setter,
    institutional, generational, analytical, global).

% Recoup loans through IMF-backed restructuring that prioritizes debt service over domestic needs; pressure debtor states to liberalize markets, privatize public assets, and reduce labor protections — all of which create new profit opportunities for foreign firms. Operate outside the territory where costs are imposed; capital mobility permits exit from any single debtor relationship.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Gain access to privatized state assets (utilities, telecommunications, natural resources, water systems) at depressed prices; exploit labor market deregulation and currency devaluation to lower production costs; benefit from tariff reduction and capital account liberalization. Exit when conditions deteriorate; the debtor state bears the stranded asset cost.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from trade liberalization by accessing cheaper imported inputs and foreign financing; accumulate local monopolies in liberalized sectors; collaborate with conditionality architects to entrench their market position at the expense of domestic producers and the poor. Can exit through capital flight or relocation when conditions shift.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, import_competing_domestic_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the immediate costs of privatization, subsidy cuts, public sector retrenchment, and labor market deregulation. Education, health, water, and transport services are dismantled or commodified; users shift from citizens to consumers. Wages fall as labor protections erode and unemployment rises; out-of-pocket costs for basic services spike while formal employment contracts disappear. Geographic and skill immobility prevent exit; citizenship ties them to the jurisdiction even as the state stops provisioning public goods.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_poor_and_working_classes, payer,
    powerless, biographical, trapped, national).

% Faced with public sector wage freezes, layoffs, and pension cuts mandated by conditionalities. Their employment is conditional on compliance; resistance invokes loan cancellation threats. Skills are often context-specific to public institutions; private-sector exit is difficult. Collective action (unions) is frequently criminalized as part of the conditionality package.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    moderate, biographical, constrained, national).

% Lose access to subsidized inputs and credit; face land seizure by agribusiness as subsidy removal and devaluation reduce competitiveness; forced into debt-peonage relationships with agribusiness firms or moneylenders. Agricultural liberalization opens markets to dumped crops from OECD countries with export subsidies still in place; small-farm exit from agriculture is the only option, but urban informal-sector work offers no alternative income floor.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, small_farmers, payer,
    powerless, biographical, trapped, national).

% Formally 'chooses' to accept conditionalities but faces capital flight, no alternative financing, and explicit threat of international isolation if it refuses. Implements reforms that undermine its own legitimacy and revenue base (privatization of revenue-generating state firms, erosion of tax base through capital flight); loses policy autonomy over domestic priorities. The government becomes an instrument for transferring wealth outward rather than a vehicle for social provisioning.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_government, observer).

% Organize resistance to conditionalities and defend public services; criminalized, detained, or attacked by security forces (often funded by external donors as part of 'governance' conditionalities). Their presence in conditionality negotiation would be disruptive to the extraction regime; they are structurally excluded from loan negotiation by design.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_poor_organizers_civil_society, excluded,
    powerless, biographical, trapped, national).

% China, regional development banks, and other sources of non-conditional financing are structurally excluded from mediation by the IMF/World Bank's institutional position. When alternative financing appears, conditionality regimes are attacked as 'watered down' and geopolitical leverage is mobilized to reassert the original framework.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, alternative_lenders, excluded,
    powerful, biographical, trapped, global).

% Benefit from export opportunities created by market liberalization, privatization of state assets to their firms, and the disciplinary effect of conditionalities on labor organization globally. Control IMF/World Bank voting structures and set the institutional agenda; coordinate conditionality frameworks across jurisdictions to prevent regulatory escape.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_governments_oecd, beneficiary,
    institutional, generational, analytical, global).

% Surveys the structural relationships and temporal dynamics to evaluate the extraction claim, the beneficiary/victim asymmetry, and the persistence mechanisms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the genuine creditor-coordination problem of loan default and currency flight: prevents individual creditors from racing to demand repayment first (the classic collective action problem in sovereign debt). But this coordination function is decoupled from the extraction architecture — the extraction persists even when the coordination problem is solved.
% TRANSFER_FUNCTION: Transfers wealth from domestic populations (via privatization revenue, import tariffs, public sector wage suppression, subsidy removal) to transnational creditors, foreign investors, and allied domestic elites. The transfer is mediated through mandatory policy reforms that commodify public goods, suppress wages, and suppress taxation of capital.
% ABSENT_VOICES: Domestic poor and working classes, small farmers, public sector unions, and indigenous groups with land claims are structurally excluded from conditionality negotiation. Labor movements that organized resistance are criminalized. Alternative lenders (China, regional development banks) whose presence would offer exit options are diplomatically sidelined. Women disproportionately bear subsidy removal costs through unpaid care work; gender-disaggregated impact analysis is absent from conditionality design.
% DISAPPEARANCE_RATIONALE: If the conditionality framework vanished, debtor states would immediately reorient toward public provisioning, asset reacquisition, and labor reprotection; creditor banks would face actual default risk and lose the extraction channel; foreign investors would lose privatization opportunities and deregulated labor access; import-competing elites would lose their market position to domestic producers. The global distribution of income would shift; capital flight from debtor countries would decelerate. The regime's disappearance would be catastrophic for creditors and require their active defense.
% FOUNDING_PROBLEM: The 1980s debt crisis: borrowing countries faced simultaneous balance-of-payments collapse, currency depreciation, and sovereign default risk. Large private banks with exposure to developing-country debt sought a mechanism to enforce repayment and prevent contagion.
% FOUNDING_PROBLEM_CORROBORATION: IMF economists and creditor governments attest the problem is still live, citing persistent fiscal deficits and moral hazard. Independent economic analysis from Zambian, Sri Lankan, and Argentine governments; civil society organizations (Jubilee Debt Campaign, Oxfam); former IMF economist Dani Rodrik; and cross-national studies (Korpi, Teulings, IMF's own independent evaluation office reports) document that conditionalities persist long after the founding debt crisis, are applied selectively by geopolitical interest, and continue to extract even when debt ratios are sustainably lower. The founding problem has been superseded by an extraction mechanism dressed in a coordination costume.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.87 reflects that conditionalities operate primarily as a mechanism for wealth transfer rather than coordination. The founding coordination problem (sovereign default during the 1980s debt crisis) was genuine but is no longer the primary driver — the regime persists decades after debt ratios stabilized because the extraction machinery proved too valuable to dismantle. Suppression at 0.89 is extremely high because the regime's persistence depends entirely on active enforcement: capital flight threats, IMF loan cancellation, international isolation, and domestic security force control (often externally funded). Without continuous surveillance, threat issuance, and willingness to detonate economies that deviate, the regime would collapse — debtor populations have no reason to consent. Theater ratio at 0.62 and rising reflects that IMF/World Bank justifications have become increasingly divorced from functional necessity: 'growth-oriented adjustment' language persists even as decades of adjustment yield persistent poverty, deindustrialization, and social service collapse. The measurement series runs on a single shared time grid (44-year interval, sampled at 1980, 1990, 2000, 2010, 2020, 2024) so every metric is authored at every examined point. The rising trajectory of all three metrics reflects extraction accumulation: the regime tightened its grip over time, requiring ever more suppression and ever more theatrical justification as functional necessity eroded.
 *
 * PERSPECTIVAL GAP:
 *   The creditor/beneficiary seats and the victim seats compute radically different constraint types. From the IMF/World Bank institutional position the arrangement is technical necessity and coordination machinery that disciplines fiscal profligacy; from the debtor-population position it is coercive wealth transfer enforced by threat of financial starvation. The engine computes this divergence from the structural data (who benefits, who bears costs, who can exit, who faces suppression). The authored claim does not adjudicate the gap; the metrics and beneficiary/victim structure ground the computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational creditor banks are full beneficiaries (d ≈ 0.0): they recoup loans, face no cost from conditionality implementation, and can exit any specific debtor relationship through capital mobility. Foreign investors similarly benefit without bearing costs (d ≈ 0.0). The debtor-state government faces a twisted structural position: formally a 'chooser' but genuinely trapped by capital flight threat and financing alternatives collapse (d ≈ 0.7). Domestic poor are full targets (d ≈ 1.0): they bear the entire cost burden (subsidy removal, service cuts, wage suppression), cannot exit their geography or income-dependence on services, and have zero structural exit options. Suppression compounds their extraction: they cannot organize resistance without facing criminalization; they cannot voice their interests because excluded from conditionality negotiation. Public sector workers sit at d ≈ 0.9: they face direct job loss, wage cuts, and pension erosion, with constrained but not trapped exit (education and context-specificity limit private-sector mobility).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s debt crisis) was a real collective-action failure: banks that had overlent to developing countries faced potential simultaneous default. Conditionality architecture solved this by giving the IMF enforcement authority over debtor-state policy. But the constraint has persisted through multiple regime shifts where the founding problem no longer drives the architecture. Structural adjustment conditionalities remain on countries whose debt is sustainable, whose growth rates are reasonable, and whose default risk is low — because the extraction is too valuable to release. The mandatrophy verdict is RESOLVED DEATH: the founding problem died (debt sustainability improved, default risk fell) but the extraction mechanism persists and has calcified. The theater-ratio trajectory confirms this: as the functional necessity eroded, the justificatory apparatus intensified (growth-oriented adjustment, poverty-reduction strategy papers, SDGs integration), suggesting the regime is now largely performative in legitimating what is structurally pure extraction. The constraint should be classified as dead-problem zombified extraction, and the mandatrophy resolution is ACTIVE.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (capital flight threats, financing collapse, direct state repression) or substantially internalized (debtor-state technocrats believe liberalization is necessary, not coerced)?',
    'Counterfactual experiment: remove capital flight threats (capital controls) and alternative financing (China, regional banks) while keeping ideological opposition to liberalization. If compliance collapses, suppression is structural; if compliance persists, suppression is substantially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — populations carry the suppression internally even after coercive mechanisms are removed. This would support a classification shift toward Snare (internalized coercion is the purest form of extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether suppression is backed by material threats or by absorbed ideological necessity.').

omega_variable(
    coordination_vs_extraction_decoupling,
    'Could the genuine coordination problem (preventing sovereign default races) be solved without the extraction architecture (privatization, subsidy cuts, wage suppression)?',
    'Design thought experiment: a loan-restructuring regime that prevented default races but prohibited privatization mandates, did not require subsidy cuts or wage suppression, and allowed public reinvestment from the freed fiscal space. If such a regime could solve the coordination problem at substantially lower extractive cost, then the current extraction is decoupled from coordination necessity.',
    'If decoupled, the extraction is pure rent-seeking rather than coordination overhead. The constraint would reclassify from Tangled Rope (hybrid) toward pure Snare if the coordination element is substantively eliminated by design alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decoupling, conceptual, 'Whether the measured extraction is the cost of coordination or is structurally separable from it.').

omega_variable(
    beneficiary_saturation_and_accumulation,
    'Has the extraction accumulated to the point where further tightening produces diminishing returns for creditors and accelerating resistance costs?',
    'Empirical analysis of debt service burdens, default rates, civil unrest, and creditor recovery rates across conditionality intensities. If creditors recover more from lighter touch arrangements (fewer defaults, lower political risk, faster growth) than from maximum tightening, the regime has entered a saturation region.',
    'If saturation is present, the regime''s persistence depends not on functional necessity but on institutional inertia and creditor status-quo preferences. This would support a Piton classification (degraded function, maintained by performance and institutional habit rather than continued value extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_saturation_and_accumulation, empirical, 'Whether conditionality intensity tracks value extraction or has become performative.').

omega_variable(
    reading_committer_kernel_distinction,
    'Is the distinction between this reading and the creditor-coordination reading a difference in what the kernel DOES, or a difference in interpretation of the same institutional arrangement?',
    'Textual analysis: do IMF/World Bank founding documents and policy manuals describe conditionality as coordinating creditor action or as enforcing debtors? The documents describe both — which suggests the readings are alternative interpretations layered over a single kernel, not discoveries of distinct constraints.',
    'If the readings are interpretations of the same kernel rather than discoveries of structurally distinct constraints, then the Snare/Rope classification difference reflects a fundamental contestation about the institution''s function, not an objective difference in what it does. This is the case documented by the ''kernel contest'' framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_kernel_distinction, conceptual, 'Whether this reading and the creditor-coordination reading are alternative framings of one institution or evidence of two structurally distinct constraints.').

omega_variable(
    selective_enforcement_and_geopolitical_capture,
    'Are conditionalities enforced uniformly across debtors, or are they selectively waived for geopolitically strategic nations while harshly imposed on weak states?',
    'Comparative case analysis: IMF/World Bank treatment of Egypt (US strategic ally), Pakistan (strategic to India containment), Ukraine (geopolitically contested), versus Zambia, Sri Lanka, or small Caribbean nations (low geopolitical value). Document the gap between stated conditionality requirements and actual enforcement.',
    'If selectivity is present, the hybrid-selectivity reading (the third sibling) is descriptively accurate and this reading''s universalist beneficiary/victim structure needs differentiation by geopolitical position. The Snare classification holds but is modulated by geopolitical interest rather than uniform extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_and_geopolitical_capture, empirical, 'Whether conditionality regime is uniform or selectively enforced by geopolitical interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.62).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.87).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.25).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, sovereign_debt_collective_action).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, currency_devaluation_distributional_shock).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, import_tariff_elimination_domestic_production).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'structural_adjustment_conditionalities'. The creditor_coordination_reading frames the same institutional arrangement as a Rope or Tangled Rope (genuine coordination with possible asymmetric extraction). The hybrid_selectivity_reading frames it as selectively applied — a Tangled Rope for geopolitically weak states, a Rope for strategic allies. All three readings share the same kernel (IMF/World Bank conditionality authority) but attribute different functions and different beneficiary structures. The debtor_extraction_reading has the highest measured extractiveness and the most victim-focused structure. The three readings together map the contested terrain of what structural adjustment actually does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
