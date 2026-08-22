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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Structural Adjustment Conditionalities as Debtor Extraction
 *   domain: economic/political
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are coercive policy prescriptions
 *   imposed by IMF and World Bank on debtor nations as a condition of
 *   accessing credit refinancing. This reading frames them as extractive
 *   neo-colonial instruments: policy sovereignty is transferred to creditor
 *   technocrats, public goods are dismantled to service debt, and domestic
 *   populations bear costs while foreign capital captures gains. The
 *   constraint persists despite decades of empirical evidence that it
 *   produces neither growth nor poverty reduction — it persists because the
 *   extraction mechanism works for its beneficiaries. This is the
 *   debtor-extraction reading: the constraint is a snare, not a coordination
 *   mechanism. The claim/metric divergence is real: creditor institutions
 *   claim coordination (the creditor_coordination_reading), but this
 *   reading's authored metrics describe extractive operation sustained by
 *   suppression. The kernel contest is between the readings, not the referent
 *   — all readings assess the standing arrangement of conditional debt, not
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Transnational creditor banks and IFIs: institutional beneficiaries, set and enforce conditionality terms, profit from debt service and privatization
 *   - Foreign capital owners: powerful beneficiaries, acquire public assets at distressed prices, extract rents from deregulation and monopolies
 *   - Debtor populations and public workers: powerless payers, bear direct costs of austerity, lose services and employment
 *   - Debtor governments: organized payers, forced to implement policies they know harm constituents, under threat of credit cutoff
 *   - Debtor nation sovereignty and democracy: non-agent payers, policy autonomy and electoral legitimacy are extracted to creditor control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.79).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Debtor Extraction").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "economic/political").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '0af157fe-35bc-4cda-a47c-28cf8a5b30bf').
narrative_ontology:cs_kernel_codification('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', fixed_text).
narrative_ontology:cs_authority_grounding('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', extraction).
narrative_ontology:cs_interpretation_layer_present('0af157fe-35bc-4cda-a47c-28cf8a5b30bf').
narrative_ontology:cs_reading_relation('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', structural_adjustment_conditionalities__creditor_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', foundational, conditionalities_coercive_extraction_not_coordination).
narrative_ontology:cs_axiom_status(conditionalities_coercive_extraction_not_coordination, holdable).
narrative_ontology:cs_axiom_grounding('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', conditionalities_coercive_extraction_not_coordination, empirically_contingent).
narrative_ontology:cs_axiom('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', foundational, debtor_populations_are_victims_not_beneficiaries).
narrative_ontology:cs_axiom_status(debtor_populations_are_victims_not_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', debtor_populations_are_victims_not_beneficiaries, deontological).
narrative_ontology:cs_reference_frame('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', creditor_capital_accumulation_regime).
narrative_ontology:cs_drift_state('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', contemporary_resistance_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0af157fe-35bc-4cda-a47c-28cf8a5b30bf', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_capital_owners).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, international_finance_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, subsistence_farmers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_sovereign_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, global_financial_system_creditor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_government_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the conditionality terms as loan officers determining what debtor governments must do to access credit. Enforce compliance through IMF and World Bank structural programs. Profit directly from debt service payments extracted from debtor treasuries, and indirectly from the asset sales and privatization of public goods that conditionalities force.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary).

% Administer the conditionality regimes on behalf of creditor bank consortia. Design the policy prescriptions (fiscal austerity, liberalization, privatization, labor deregulation), audit compliance, and gate access to refinancing. Frame conditionalities as necessary discipline for 'fiscal responsibility' and market confidence, shielding the extraction mechanism with technocratic legitimacy.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, international_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Acquire privatized public assets (utilities, mining concessions, telecoms, land) at distressed prices when conditionalities force their sale. Benefit from deregulation that removes environmental, labor, and consumer protections. Extract rents from monopolies and oligopolies created by the privatization process.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the direct costs of austerity: eliminated health and education subsidies, reduced public service employment, frozen wages, removal of price controls on food and fuel, and loss of access to essential services that were previously state-provided. The cost of living rises while income falls. Exit is impossible — they cannot leave their nation's territory or currency zone in response to conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations, payer,
    powerless, biographical, trapped, national).

% Face mass retrenchment mandates as conditionalities require government downsizing. Pensions are cut, wages frozen, and entire ministries shuttered. Professional mobility is limited to the domestic labor market, which conditionalities simultaneously depress through austerity contraction. Organized resistance is branded as 'special interest obstruction' by creditors.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    moderate, biographical, constrained, national).

% Are displaced when conditionalities mandate agricultural commercialization and land privatization. Smallholder subsidy removal and mandatory export-crop focus undermine food security. Loss of communal land rights and traditional farming structures eliminates their exit option of subsistence production. Urban migration becomes forced rather than chosen.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, subsistence_farmers, payer,
    powerless, biographical, trapped, local).

% The governance capacity and policy autonomy of the debtor state. Conditionalities transfer macroeconomic policy-making from the elected government to IMF/World Bank technocrats. Fiscal policy, monetary policy, trade policy, labor standards, environmental regulation — all become non-negotiable prescription rather than democratic choice. The state becomes an administrative unit implementing external policy rather than a sovereign polity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_sovereign_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_sovereign_capacity).

% Are forced to implement policies they know will harm their electorate, under threat of credit cutoff and economic collapse. They sign conditionality agreements under duress — the alternative is immediate debt default and financial system meltdown. Their political legitimacy erodes as they preside over austerity they did not choose and cannot modify without losing refinancing access.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_government_officials, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_government_officials, excluded).

% Development strategies based on state-led industrialization, import substitution, progressive taxation, or counter-cyclical spending are ruled out by conditionality mandates. The policy space collapses to a single neoliberal framework. Dissenting economists and policymakers are sidelined as ideologically unsound, and their alternative models are never implemented on a scale large enough to generate competing evidence.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, alternative_policy_paradigms, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__debtor_extraction_reading, alternative_policy_paradigms).

% Electoral politics becomes decoupled from policy-making. Governments can be voted in and out, but the fundamental policy constraints remain fixed by creditor demands. Conditionality clauses survive government turnover — no new government can renegotiate them without financial isolation. Democratic choice becomes theatrical; the real decisions are made by creditor technocrats.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_democracy, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_nation_democracy).

% The wealthy nations whose banks dominate the creditor consortia and whose corporations benefit from privatization and market access. They capture the gains from debtor extraction while bearing none of the political cost — the cost falls entirely on debtor populations. Their governments support the conditionality system through voting power in IMF/World Bank governance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, global_financial_system_creditor_states, beneficiary,
    institutional, generational, analytical, global).

% Conditionalities mandate natural-resource extraction and agricultural commercialization to generate export revenue for debt service. Forests are logged, fisheries depleted, and soil degraded to service external debt. Environmental regulation is dismantled as a 'constraint on competitiveness.' The degradation is externalized onto global commons and future generations.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, environmental_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__debtor_extraction_reading, environmental_commons).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading denies the coordination framing. Creditors could achieve debt stability through grants, debt forgiveness, or reduced interest rates. The coercive conditionality mechanism is not necessary for coordination; it is chosen because it extracts rents. Alternative coordination structures (debtor-creditor joint governance, democratized IFI decision-making) exist but are ruled out by creditor power.
% TRANSFER_FUNCTION: Transfers macroeconomic policy authority from elected debtor governments to IMF/World Bank technocrats (policy sovereignty); transfers future public revenues to creditor debt service (monetary value); transfers public assets to foreign private owners (privatization flow); transfers environmental and labor regulation capacity away from debtor states (regulatory authority); transfers growth and employment opportunities to foreign capital (economic benefit).
% ABSENT_VOICES: Debtor-nation parliaments, trade unions, peasant organizations, environmental movements, and heterodox development economists. These constituencies are excluded from conditionality design and implementation. Their proposed alternatives (capital controls, state-led industrialization, land reform, environmental protection, social spending) are not seated at the negotiating table; instead, they are told the conditionality terms are non-negotiable.
% DISAPPEARANCE_RATIONALE: If conditionalities disappeared, debtor governments would immediately re-expand public services, reduce debt service payments (through renegotiation or default), reinstitute capital controls and trade protections, undertake land reform and nationalization of key assets, and pursue alternative development strategies (import substitution, state-led industrialization). The political economies of debtor nations would reorganize entirely. Foreign capital would lose the legal framework and creditor protection for privatized monopolies. Environmental standards would rise as debt pressure to liquidate natural resources fell. Within 5 years, the structural profiles of debtor nations would shift significantly away from neoliberal homogeneity toward heterogeneous national strategies. The removal is catastrophic for creditors and foreign capital; it is liberatory for debtor populations.
% FOUNDING_PROBLEM: External debt crises of the 1980s (oil shocks, rising US interest rates, commodity price collapse) left debtor nations unable to service existing debt from export revenues. Debt payments were consuming 20–50% of export earnings in some nations. Default or renegotiation threatened the financial system's stability and creditor banks' balance sheets.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions claim the founding problem persists (debt levels remain elevated, fiscal discipline remains necessary for market confidence). Independent economic analysis from outside creditor constituencies documents that: (1) debt-to-GDP ratios have fallen significantly since the 1980s crisis; (2) most debtor nations run primary surpluses (revenues exceed non-debt spending, meaning they are not borrowing for current consumption); (3) export earnings have risen in real terms and as a share of GDP; (4) debt crises are now driven by currency composition and refinancing risks, not unsustainable debt levels. CEPAL, UNDP, and academic economists outside the beneficiary set corroborate that the founding problem is substantially solved for most debtor nations. Yet conditionalities persist unchanged — the persistence is not explained by ongoing crisis. This is mandatrophy: the mandate is dead; the institution persists.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82 by 2024) and rising because the conditionality regime captures resources (debt service plus privatization revenues) and transfers them to creditors, while imposing costs (austerity, unemployment, service loss) on debtor populations. The extraction is sustained, not transient. Suppression is also high (0.79) because the regime persists despite decades of documented harm and organized resistance — its persistence depends on creditor institutional power (control of credit access) and on the collapse of alternative policy spaces. Theater is moderate (0.48) and rising: the conditionality system presents itself as technical discipline for 'fiscal responsibility,' but as extraction accumulates the theater work intensifies — new rebranding (poverty reduction strategy papers, debt sustainability frameworks) recycles the same core regime. The measurement trajectory from 1982 to 2024 shows extraction and suppression rising in tandem, with theater layering on top — the constraint's function shifted from emergency lending to institutionalized rent extraction. Accessibility collapse is particularly severe at the structural level (0.81 by 2024): the policy space globally is collapsed into a single neoliberal framework that is presented as inevitable, technical, and without alternative. Alternatives exist in theory but are foreclosed in practice by the creditor power to isolate nations that attempt them. Resistance is growing (from 0.42 to 0.52 at the structural level, from 0.48 to 0.62 at the individual level) but remains suppressed by the creditor's control of credit access — a debtor nation that resists loses its capacity to refinance, which makes organized, sustained resistance costly and shapes it toward shorter-term political cycles.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different types for different seats. From the creditor institutional seat, the constraint may compute as rope or scaffold (coordination function present, beneficiary without being trapped). From the debtor population seat, it computes as snare (high extraction, high suppression, trapped exit). From the debtor government seat, it may compute as tangled rope (coordinates creditor preferences, extracts from its own population, active enforcement required). The authored claim is snare; the structural data supports divergent computations per seat. This divergence is the measurement the corpus takes — the engine's job is to show how the same constraint appears different from different seats. The divergence is not an error; it is a feature of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational creditor banks and IFIs are the structural beneficiaries — they control the rules (agenda-setter role), profit from the extraction (direct debt service revenues, indirect gains from privatization), and can exit the arrangement without cost (they can always refuse to lend or forgive debt; their choice not to is what enables the snare to close). Their directionality sits at the beneficiary end (d near 0.0): the constraint subsidizes them in the form of captured external resources. Debtor populations are the structural targets — they are trapped (identity_locked by citizenship and currency; cannot leave their nation zone in response to externally-imposed austerity), bear costs directly (service loss, unemployment, inflation from price decontrol), and have no exit option. Their directionality sits at the target end (d near 1.0): the constraint extracts from them. Debtor governments sit in an intermediate, contradictory position: they are payers (forced to implement austerity) but are also nominally agents of their populations. Their directionality is constrained (d near 0.7–0.8): they are targets of the creditor's enforcement power, but they retain formal policy authority even as its exercise is constrained. Foreign capital owners are powerful beneficiaries (arbitrage exit: if conditions worsen, they move capital elsewhere; their exit cost is low) — directionality near 0.1. Global creditor-nation states are institutional beneficiaries (analytical exit: they hold voting power in the IFI governance structures; their exit from the system is implausible). The directionality structure is highly asymmetric: beneficiaries can exit cheaply; victims are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing acute debt crises in the 1980s) is defunct. Debt-to-GDP ratios in most debtor nations have fallen to sustainable levels. Many debtor nations run primary surpluses (revenues exceed non-debt spending). By objective fiscal metrics, the original problem is solved. Yet conditionalities persist unchanged — the regime has accumulated theater (new names, new rationalization frameworks) while the core extraction mechanism continues unchanged. This is mandatrophy: the mandate (emergency debt stabilization) is dead, but the institution persists by inventing new justifications (poverty reduction, sustainable development, climate adaptation). The policy prescriptions are identical to 1982 — liberalization, privatization, deregulation, austerity — despite decades of evidence that these prescriptions do not produce the promised outcomes. The persistence despite goal death and evidence failure indicates the constraint's real function is extraction, not coordination. The theater (sustainability frameworks, social safeguards language, poverty focus) masks the extraction from public scrutiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_genuine_spillover,
    'Is the high extractiveness measured here a feature of the conditionality regime itself, or a spillover from global inequality structures and debtor nations'' structural position in the world economy?',
    'Counterfactual scenario: construct a case where a debtor nation faced the same external shocks and market conditions but retained policy autonomy (no IMF conditionalities). Empirical comparison of outcomes between conditional and non-conditional debt periods for the same nations. Structural econometric analysis isolating the conditionality effect from global market effects.',
    'If extraction is intrinsic to conditionalities, the reading''s snare classification holds. If extraction is primarily due to structural inequality independent of the conditionality regime, the constraint may compute as a less-extractive coordination mechanism for managing unavoidable global asymmetries — the debtor-extraction reading''s core claim would be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_genuine_spillover, empirical, 'Whether measured extractiveness is conditionality-intrinsic or structurally-determined.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression (measured at 0.79) primarily structural — external barriers to policy autonomy, credit cutoff threats, institutional exclusion — or internalized — debtor governments and populations have internalized the neoliberal paradigm as inevitable and best practice?',
    'Post-exit analysis: when debtor nations do exit the conditionality system (default, renegotiation, or alternative finance), does the suppression persist (indicating internalization) or does policy space rapidly expand (indicating structural coercion)? Qualitative interviews with policymakers in debtor nations regarding perceived constraints. Comparative analysis of policy choices between conditional and non-conditional debtor regimes.',
    'If suppression is primarily structural, the constraint''s potency depends on continued creditor enforcement. If substantially internalized, the constraint''s persistence has lower exit costs than the measured suppression suggests — victims carry the suppression with them even after formal exit. This affects the terminal type and the feasibility of remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is externally enforced or internally incorporated.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''structural adjustment conditionalities'' best understood as a unified policy instrument across its readings, or does each reading essentially refer to a different constraint (creditor prefer to call it coordination, debtors experience it as extraction)?',
    'Examine whether the same policy prescriptions and enforcement mechanisms appear in both creditor and debtor descriptions, or whether they systematically describe different observables. If the same prescriptions are present but valued differently, it is a single kernel with multiple readings. If the observables themselves differ by reading, the decomposition boundary is misdrawn.',
    'If the readings are genuinely about the same kernel (same policies, different valuations), the ε-invariance principle holds and the decomposition is correct. If the readings select different observables (e.g., creditor reading focuses on credit access outcomes, debtor reading focuses on poverty impacts), then the constraint may need further decomposition — each reading may be internally ε-invariant but the readings are not siblings of a unified kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether all readings of structural adjustment refer to the same constraint or to observation-dependent cousins.').

omega_variable(
    counterfactual_resistance,
    'The measured resistance (0.52 at the structural level by 2024) is substantial but not overwhelming. What would resistance levels look like if creditors fully lost enforcement capacity — e.g., if a coalition of debtor nations coordinated default and the IFI credit gate fell away?',
    'Historical analysis of periods when creditor enforcement collapsed (1930s debt crises, 1970s commodity booms, 2001 Argentina default aftermath). Simulation models of debtor coordination under alternative financing regimes (regional development banks, peer lending, debt forgiveness). Analysis of policy shifts in nations that have exited IMF programs.',
    'High counterfactual resistance (above 0.75) would suggest the constraint''s suppressive function is entirely dependent on creditor enforcement. Moderate resistance (0.5–0.7) would suggest some internalization of the neoliberal framework (creditor reading''s framing influences policy preferences). Low resistance (below 0.5) would indicate suppression is entirely structural — resistance rises immediately when enforcement is removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_resistance, empirical, 'How resistance would evolve if creditor enforcement mechanisms failed.').

omega_variable(
    committer_reading_contradiction,
    'Does the debtor-extraction reading''s core axiom (conditionalities are coercive extraction disguised as fiscal coordination) logically foreclose the creditor-coordination reading''s core axiom (conditionalities are necessary coordination for debt stability), or do both readings coexist as live political positions held by different parties?',
    'Examine whether a single actor (e.g., a senior IMF official, a debtor-nation finance minister) could coherently hold both readings simultaneously, or whether holding one requires explicitly rejecting the other. If both can be held without logical contradiction by different parties, they coexist. If holding one requires denying the other''s basic premise, they foreclose.',
    'If the readings foreclose each other (mutual logical exclusivity), the relation in cs_structure.reading_relations should be ''forecloses''. If they coexist as live positions held by different institutional constituencies, the relation is ''coexists_with''. This affects how the engine models the kernel dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_contradiction, conceptual, 'Whether the debtor-extraction and creditor-coordination readings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1982, 0.32).
narrative_ontology:measurement_basis(stru_tr_t1982, observed).
narrative_ontology:measurement(stru_tr_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1995, 0.39).
narrative_ontology:measurement_basis(stru_tr_t1995, observed).
narrative_ontology:measurement(stru_tr_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2005, 0.44).
narrative_ontology:measurement_basis(stru_tr_t2005, observed).
narrative_ontology:measurement(stru_tr_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2015, 0.47).
narrative_ontology:measurement_basis(stru_tr_t2015, observed).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement_basis(stru_tr_t2020, observed).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(stru_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1982, 0.68).
narrative_ontology:measurement_basis(stru_be_t1982, observed).
narrative_ontology:measurement(stru_be_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement_basis(stru_be_t1995, observed).
narrative_ontology:measurement(stru_be_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement_basis(stru_be_t2005, observed).
narrative_ontology:measurement(stru_be_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement_basis(stru_be_t2015, observed).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement_basis(stru_be_t2020, observed).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(stru_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1982, 0.61).
narrative_ontology:measurement_basis(stru_su_t1982, observed).
narrative_ontology:measurement(stru_su_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement_basis(stru_su_t1995, observed).
narrative_ontology:measurement(stru_su_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement_basis(stru_su_t2005, observed).
narrative_ontology:measurement(stru_su_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement_basis(stru_su_t2015, observed).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement_basis(stru_su_t2020, observed).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.79).
narrative_ontology:measurement_basis(stru_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1982, tn=2024
narrative_ontology:measurement(stru_grid_01, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(class), 1982, 0.58).
narrative_ontology:measurement(stru_grid_02, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(class), 2024, 0.75).
narrative_ontology:measurement(stru_grid_03, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(individual), 1982, 0.51).
narrative_ontology:measurement(stru_grid_04, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(individual), 2024, 0.68).
narrative_ontology:measurement(stru_grid_05, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(organizational), 1982, 0.63).
narrative_ontology:measurement(stru_grid_06, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(organizational), 2024, 0.79).
narrative_ontology:measurement(stru_grid_07, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(structural), 1982, 0.72).
narrative_ontology:measurement(stru_grid_08, structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse(structural), 2024, 0.81).
narrative_ontology:measurement(stru_grid_09, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(class), 1982, 0.56).
narrative_ontology:measurement(stru_grid_10, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(class), 2024, 0.75).
narrative_ontology:measurement(stru_grid_11, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(individual), 1982, 0.48).
narrative_ontology:measurement(stru_grid_12, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(individual), 2024, 0.62).
narrative_ontology:measurement(stru_grid_13, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(organizational), 1982, 0.52).
narrative_ontology:measurement(stru_grid_14, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(organizational), 2024, 0.71).
narrative_ontology:measurement(stru_grid_15, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(structural), 1982, 0.42).
narrative_ontology:measurement(stru_grid_16, structural_adjustment_conditionalities__debtor_extraction_reading, resistance(structural), 2024, 0.52).
narrative_ontology:measurement(stru_grid_17, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(class), 1982, 0.61).
narrative_ontology:measurement(stru_grid_18, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(class), 2024, 0.76).
narrative_ontology:measurement(stru_grid_19, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(individual), 1982, 0.58).
narrative_ontology:measurement(stru_grid_20, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(individual), 2024, 0.72).
narrative_ontology:measurement(stru_grid_21, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(organizational), 1982, 0.52).
narrative_ontology:measurement(stru_grid_22, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(organizational), 2024, 0.68).
narrative_ontology:measurement(stru_grid_23, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(structural), 1982, 0.68).
narrative_ontology:measurement(stru_grid_24, structural_adjustment_conditionalities__debtor_extraction_reading, stakes_inflation(structural), 2024, 0.79).
narrative_ontology:measurement(stru_grid_25, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(class), 1982, 0.59).
narrative_ontology:measurement(stru_grid_26, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(class), 2024, 0.76).
narrative_ontology:measurement(stru_grid_27, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(individual), 1982, 0.54).
narrative_ontology:measurement(stru_grid_28, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(individual), 2024, 0.71).
narrative_ontology:measurement(stru_grid_29, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(organizational), 1982, 0.62).
narrative_ontology:measurement(stru_grid_30, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(organizational), 2024, 0.79).
narrative_ontology:measurement(stru_grid_31, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(structural), 1982, 0.65).
narrative_ontology:measurement(stru_grid_32, structural_adjustment_conditionalities__debtor_extraction_reading, suppression(structural), 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, capital_flight_and_tax_evasion).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, debt_denominated_foreign_currency_exposure).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, land_grabbing_privatization_nexus).

% DUAL FORMULATION NOTE:
% This story is one reading of the structural_adjustment_conditionalities kernel. Sibling readings: creditor_coordination_reading (conditionalities as necessary debt stabilization), hybrid_selectivity_reading (conditionalities as selectively enforced discipline). The three readings share referent (the same standing arrangement of conditional external lending) but diverge on function, beneficiary structure, and mechanism. This story instantiates the debtor-extraction reading: high extractiveness, high suppression, snare classification. The readings form a kernel family because they contest whether the same policy regime is primarily coordination (creditor read), primarily extraction (debtor read), or selective enforcement (hybrid read). The creditor reading would show lower extractiveness, lower suppression, rope or tangled-rope classification. The engine computes per-seat classifications; the readings' structural divergence maps to per-seat divergence in computed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
