% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This is the CREDITOR COORDINATION READING of the structural adjustment
 *   conditionalities kernel. This reading frames conditionalities as
 *   solutions to a real collective-action problem among creditors and a real
 *   macroeconomic sustainability problem for debtor states. The coordination
 *   problem: multiple creditors lack a mechanism to enforce a common program
 *   (each has incentive to exit first); debtor governments lack credible
 *   commitment devices for painful reforms; and markets price in default risk
 *   and currency collapse. Conditionalities solve this by (a) bundling
 *   creditor agreement on a program and reform targets, (b) tranching
 *   disbursement to monitor debtor compliance, and (c) signaling market
 *   confidence through IMF/World Bank approval. Victims in this reading are
 *   not the poor or the debtor state generally, but inefficient state sectors
 *   and the prior unsustainable fiscal arrangements they represent. The rope
 *   framing asserts that all parties benefit from the coordination outcome
 *   once implementation costs are absorbed: future taxpayers avoid debt
 *   spirals, markets gain confidence, and the debtor state regains
 *   creditworthiness. The claim/metric relationship is deliberate:
 *   extractiveness reaches 0.58 and suppression 0.62, moderately high for a
 *   rope, because creditor coordination is enforced and asymmetric (debtor
 *   governments accept terms they would not choose unilaterally). The
 *   measurement series show rising extractiveness from 1980–2008 (reflecting
 *   intensified conditionality depth and institutional reach), then plateau
 *   (reflecting market stabilization after 2008 and the constraint reaching a
 *   steady state). The theater ratio climbs from 0.12 to 0.28, indicating
 *   growing performative maintenance: later programs include more rhetorical
 *   framing of 'local ownership' and participatory processes even as creditor
 *   authority over core terms remains intact.
 *
 * KEY AGENTS:
 *   - International creditor consortium (IMF, World Bank, bilateral lenders): sets agenda, enforces tranches, coordinates program design
 *   - Debtor government (constrained payer): must implement reforms, cedes fiscal authority, faces domestic political resistance
 *   - Poor households dependent on subsidies (trapped payer/beneficiary): bear immediate costs of price decontrol; benefit from inflation prevention
 *   - Public-sector workers (organized payer/beneficiary): face wage cuts and layoffs; benefit from fiscal stability and currency preservation
 *   - Private-sector import competitors and multinational acquirers (beneficiaries): benefit from trade liberalization and asset acquisition
 *   - Alternative credit sources and non-aligned development banks (excluded): structurally barred from agenda-setting
 *   - International capital markets (institutional beneficiary): gain confidence signal and lower default risk premium
 *   - Academic observers: assess whether coordination function is real and whether costs reflect structural inefficiency or distributional harm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.58).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.62).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "economic/political").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'a7a71c16-f961-4ecd-a5fe-43bef05b6a27').
narrative_ontology:cs_kernel_codification('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', formalized).
narrative_ontology:cs_authority_grounding('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', expertise).
narrative_ontology:cs_interpretation_layer_present('a7a71c16-f961-4ecd-a5fe-43bef05b6a27').
narrative_ontology:cs_reading_relation('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', foundational, coordination_necessity_for_macro_stability).
narrative_ontology:cs_axiom_status(coordination_necessity_for_macro_stability, holdable).
narrative_ontology:cs_axiom_grounding('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', coordination_necessity_for_macro_stability, empirically_contingent).
narrative_ontology:cs_axiom('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', foundational, technical_expertise_legitimacy_of_conditionality_design).
narrative_ontology:cs_axiom_status(technical_expertise_legitimacy_of_conditionality_design, holdable).
narrative_ontology:cs_axiom_grounding('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', technical_expertise_legitimacy_of_conditionality_design, deontological).
narrative_ontology:cs_reference_frame('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', creditor_coordinated_adjustment_framework).
narrative_ontology:cs_drift_state('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', contemporary_post_2008_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a7a71c16-f961-4ecd-a5fe-43bef05b6a27', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, poor_households_subsidy_dependent).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, private_sector_import_competitors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, multinational_acquirers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, poor_households_subsidy_dependent).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, state_owned_enterprise_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% IMF, World Bank, bilateral creditors, and private lenders collectively set the terms of conditionality packages: fiscal discipline, privatization, trade liberalization, subsidy removal. They justify these as technical requirements for macroeconomic stability, debt servicing capacity, and restoration of market confidence. They enforce conditions through tranched disbursement and approval gates.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditor_consortium, agenda_setter,
    institutional, generational, arbitrage, global).

% Must accept conditionality packages to access financing during crises. Cannot refinance or restructure debt without creditor approval. Faces domestic political cost from implementing unpopular reforms (subsidy removal, public sector wage cuts, privatization) while maintaining creditor confidence. Exit to non-creditor finance is limited by debt overhang and geopolitical constraints.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government, payer,
    moderate, biographical, constrained, national).

% Benefit from the constraint's coordination function: fiscal discipline prevents inflation spirals, currency collapse, and intergenerational debt burden that would devastate their purchasing power. They also bear direct costs: wage freezes, layoffs in overstaffed state sectors deemed inefficient by creditors, elimination of wage indexation. Their exit is limited by public-sector career dependence and labor-market constraints.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers, payer).

% Benefit from the constraint's function: unchecked fiscal deficits and hyperinflation would destroy their savings and food purchasing power far more acutely than subsidy removal. They bear direct costs: elimination of price controls on food, fuel, and medicine raises immediate household expenses. Exit is impossible; they must absorb price shocks or reduce consumption.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, poor_households_subsidy_dependent, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, poor_households_subsidy_dependent, payer).

% Benefit from trade liberalization conditionalities: tariff reduction removes import barriers, allowing domestic producers to supply previously protected markets. They benefit from FDI inflows that follow fiscal stability. Their coordination problem (access to protected markets without fiscal chaos) is solved by the package. Exit is available: they can relocate or source elsewhere if conditions change.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, private_sector_import_competitors, beneficiary,
    powerful, generational, mobile, regional).

% Bears the burden of privatization and restructuring conditionalities. SOEs are identified as sources of fiscal drain and inefficiency; creditors condition financing on their sale or closure. Enterprise management and workers face job loss; the state loses revenue streams (however inefficiently generated). Exit is constrained by debt dependencies that force acceptance of the package.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, state_owned_enterprise_sector, payer,
    moderate, biographical, constrained, national).

% Coordinated through conditionality packages: restoration of fiscal discipline and debt servicing signals lower default risk, raising market confidence and reducing risk premiums on sovereign borrowing. This is an institutional beneficiary entity (not a named seat with veto power) whose coordination problem is solved but which collects no direct transfer.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).

% Analytical beneficiary: they benefit from the constraint's function because conditionalities prevent debt spirals, unsustainable currency depreciation, and intergenerational fiscal burdens that would require even harsher adjustment later. They are unborn and unrepresented in the negotiation; their benefit is structural.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_nation, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_nation).

% Benefit from privatization conditionalities: they acquire state assets at distressed valuations in debtor states seeking urgent financing and creditor approval. FDI inflows and asset sales are explicitly conditioned on privatization targets. Exit is straightforward: they are mobile capital with alternative investment opportunities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, multinational_acquirers, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from the constraint's coordination function: structural adjustment in debtor states preserves the rule-based international lending architecture, maintains debt service discipline, and prevents contagion defaults that could trigger financial crises in creditor-country banking systems. Their benefit is systemic stability.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_country_governments, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_country_governments).

% Non-creditor sources (bilateral development banks, regional financing mechanisms not controlled by IMF/World Bank, or alternative creditor coalitions) are structurally excluded from setting conditionality terms. They are available for debtor-state exit only when debt-overhang and geopolitical constraints permit. Their exclusion from the agenda-setting coalition is what the enforcement machinery maintains.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, alternative_credit_sources_nonaligned, excluded,
    powerful, generational, trapped, global).

% Observational seat: they assess whether conditionalities achieve stated goals (debt sustainability, fiscal discipline, market confidence restoration) and analyze distributional impacts. Under this reading, they examine whether the coordination function is real and whether victims exist or whether costs are transition costs of fixing prior inefficiency.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, academic_observers_development_economics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of multiple creditors and a debtor state around a common macroeconomic program to prevent debt-service default, currency collapse, and contagion crises. The coordination problem: absent a shared framework, each creditor has incentive to front-run (demand immediate repayment or higher risk premiums), debtor governments lack commitment devices to follow unpopular-but-necessary reforms, and markets price in higher default risk. Conditionalities solve this by bundling creditor agreement on a program, debtor commitment to implementation (monitored through tranches), and market signaling that the program is credible and will be enforced.
% TRANSFER_FUNCTION: Moves fiscal authority from the debtor state to the creditor consortium: debtor governments cede design of spending, revenue, and structural reforms to creditor conditions. Moves resources from eliminated state-sector employment and subsidies to debt servicing and private-sector investment. Moves assets from state ownership to private (often foreign) ownership through privatization. The transfer is framed as disciplining inefficient state sectors; this reading asserts the transfer solves the coordination problem rather than merely extracting.
% ABSENT_VOICES: Non-creditor sources (alternative development finance, regional lenders, bilateral partners not party to the agreement) are excluded from setting terms. They would argue for conditional finance with less stringent structural reform requirements, or for debt cancellation as an alternative to adjustment. Their exclusion is structural: creditors maintain control of the most critical financing and approval gates, making exit to alternatives costly for debtor states.
% DISAPPEARANCE_RATIONALE: If conditionality frameworks disappeared, debtor governments would have weak incentive to implement unpopular reforms; capital would exit; debt would be refinanced at higher risk premiums or defaulted; and a rules-based international lending architecture would break down into bilateral haggling and contagion crises. The coordination function is real: the constraint's disappearance would reorganize the entire system of sovereign borrowing and capital flow management.
% FOUNDING_PROBLEM: Latin American debt crises of the 1980s, East Asian crises of the 1990s, repeated balance-of-payment crises: debtor states face sudden capital outflows, exchange-rate collapse, import compression, and inflation when markets lose confidence in their ability to service external debt. Without a coordinated creditor response and committed debtor adjustment, each creditor races to exit, debtor governments lack political commitment to painful reforms, and contagion spreads across countries and asset classes. Conditionality packages emerged as a mechanism to coordinate creditors, signal reform commitment, and prevent systemic collapse.
% FOUNDING_PROBLEM_CORROBORATION: International central bankers, IMF/World Bank economists, and academic scholars of international finance attest the coordination problem is live: sudden stops, currency crises, and debt spirals remain active risks (evidenced by repeated adjustment programs from 1980s to present). Debtor-country governments, civil-society organizations, and heterodox economists attest the founding problem is overstated or solved and the arrangement persists as elite coordination for creditor profit and discipline of sovereign states. Academic empirical work (Kentikelenis et al., Dreher, Asongu) documents mixed outcomes: some adjustment programs restore growth; others deepen inequality and slow convergence, suggesting the founding problem framing is contested.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness of 0.58 reflects moderate but real asymmetry: creditors set terms unilaterally and extract disciplinary authority from the debtor state; however, the extracted authority is justified as solving a coordination problem both parties face (currency collapse, default contagion). Extractiveness is not higher (0.7+) because the constraint's persistence depends on demonstrable credibility with markets, not pure coercion — if programs consistently failed to stabilize macroeconomic conditions, market confidence would erode and the framework would collapse. Suppression of 0.62 reflects enforcement through tranched lending: debtor governments retain the choice to reject the package, but rejection carries the cost of capital flight, currency collapse, and inability to import essential goods — this is coercive but not categorical. Theater ratio of 0.28 (climbing from 0.12) captures increasing rhetorical framing of 'local ownership' and 'participatory design' in programs since the 2000s, even as core terms remain creditor-set. The measurement series use one shared time grid (1980, 1990, 2000, 2008, 2016, 2024) so every metric is authorized at every examination point. The 1980 baseline marks the Latin American debt crisis that catalyzed modern conditionality frameworks. The 1990 and 2000 marks track East Asian crises and post-Cold War consolidation of IMF/World Bank authority. The 2008 mark captures the global financial crisis and the return of large adjustment programs. The 2016 mark reflects post-2008 stabilization. The 2024 mark represents contemporary status. Rising extractiveness to 2008 reflects deepening conditionality (more structural reform requirements, not just macro targets); plateau thereafter reflects a steady-state intensity of oversight.
 *
 * PERSPECTIVAL GAP:
 *   The creditor consortium and the international capital markets perceive this as genuine coordination with spillover benefits: stabilized debtor-state macroeconomics raises confidence, lowers risk premiums, and prevents contagion crises that would harm creditor-country financial systems. The debtor government perceives it as enforced capitulation: authority over core policy (spending, revenues, trade, exchange rates) is transferred to external institutions; domestic political costs fall on elected officials; distributional outcomes (who bears adjustment costs) are not negotiable. Poor households and public-sector workers perceive asymmetric impacts: the coordination benefit (inflation prevention) is diffuse and long-term; the cost (subsidy removal, wage cuts) is immediate and concentrated. The engine computes this divergence from the structural data: directionality differs sharply (creditors near 0.0 beneficiary end, debtor government near 0.9 target end, poor households at ~0.7 target end due to trapped exit). This reading asserts the divergence reflects real coordination asymmetry, not extractive mischaracterization.
 *
 * DIRECTIONALITY LOGIC:
 *   International creditors: d~0.05 (beneficiaries). They set terms, control tranches, and extract fiscal authority; they have arbitrage exit (they can choose not to lend or can exit through syndication). Base derivation would place them near 0.0; no override needed. Debtor government: d~0.85 (targets). They must accept terms to access emergency financing; their exit options are constrained (default means capital flight and currency collapse). Beneficiary status is ambiguous — they benefit from the coordination function (avoiding debt spirals), but the constraint extracts policy authority. The high d reflects the constrained exit and unilateral terms-setting by creditors; this is correct under the creditor-coordination reading. Poor households: d~0.72 (targets). They benefit from inflation prevention (coordination function) but bear immediate costs (subsidy removal, unemployment). Exit is trapped (they cannot leave the debtor state). The balance is negative because immediate costs exceed diffuse long-term benefits from their perspective. Public-sector workers: d~0.68 (targets). They face wage freezes and layoffs but benefit from fiscal stability. Exit is constrained by public-sector career dependence. Private-sector beneficiaries and multinational acquirers: d~0.15-0.25 (near-beneficiary). They benefit from trade liberalization and asset sales; exit is mobile. No overrides needed. The directionality logic under this reading is that the constraint solves a real coordination problem AND extracts asymmetrically — both can be true simultaneously. The rope classification asserts the coordination benefit justifies the asymmetry; alternative readings (debtor-extraction, hybrid-selectivity) dispute this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem framing (debt crises, capital flight, contagion) was live and urgent in 1980–2000. By 2008, the founding problem status enters contested territory: some debtor states had exited high-conditionality dependence (Chile, South Korea), questioning whether conditionality was necessary for growth; others remained trapped (Sub-Saharan Africa, Central America), supporting the founding-problem-still-live reading. By 2024, mandatrophy is substantial but contested: the founding problem (sudden stops, currency crises) persists as a real risk (evident in 2020–2024 episodes: Sri Lanka, Argentina, Tunisia), but many scholars argue it is overstated or that conditionality mechanisms have become outdated or counterproductive. The constraint avoids clean mandatrophy (total goal achievement) because outcomes are mixed: some programs restore growth and creditworthiness; others deepen inequality and slow convergence. The theater ratio climb (0.12 → 0.28) suggests increasing performative maintenance: IMF and World Bank adopt rhetorical framing of 'local ownership,' 'participatory design,' and 'poverty reduction focus' while core creditor authority and conditionality intensity remain intact. This is a signature of mandatrophy drift — the organizing principle (creditor-coordinated adjustment) persists but is increasingly defended through performance and narrative rather than functional necessity. The claim (rope: coordination solution) diverges from the computed type at the payer seats (likely Tangled Rope or Snare from their perspective), which is exactly the measurement the corpus exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_or_solved,
    'Is the founding problem (debt crises, sudden stops, contagion) still a live risk to which conditionalities are a necessary response, or has it been substantially solved (or become obsolete) and conditionalities persist as institutional inertia and creditor disciplinary power?',
    'Longitudinal analysis of (a) frequency and severity of capital-flow crises in debtor states, (b) whether debtor states without conditionality programs experience better or worse macro stability outcomes than those with programs, (c) whether debtor states that exited high conditionality (Chile, South Korea, Botswana) maintained stability or regressed. Cross-country regression on program participation, conditionality intensity, and growth/stability outcomes.',
    'If founding problem is live: the rope framing holds; the constraint solves a real coordination problem. If founding problem is solved or obsolete: the constraint is likely Tangled Rope or Snare; creditor coordination persists as extractive power with weakening functional justification (mandatrophy signal). If outcomes are mixed (some debtor states benefit, others are harmed): the constraint is context-dependent; a single reading may not hold universally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_solved, empirical, 'Whether the macroeconomic coordination problem conditionalities address is still live, solved, or obsolete.').

omega_variable(
    structural_necessity_vs_creditor_power,
    'Are conditionalities structurally necessary (no alternative mechanism could coordinate creditors and commit debtor states as effectively), or are they contingent on creditor power and the absence of alternative coordination mechanisms (that creditors have actively excluded)?',
    'Comparative institutional analysis: (a) do non-conditionality financing mechanisms (regional development banks, South-South lending, bilateral partnerships) produce comparable macro outcomes or fail to coordinate? (b) Natural experiments where debtor states escaped conditionality (currency boards, dollarization, capital controls) and outcomes relative to comparable conditionality-program states. (c) Analysis of whether alternative conditionality designs (less stringent, debtor-negotiated, less privatization-focused) were empirically tested or foreclosed by creditor institutional interests.',
    'If structurally necessary: the coordination function is real and alternatives are not viable; the rope framing is robust. If contingent on creditor power: conditionalities are one possible coordination mechanism among others; creditors maintain them because they concentrate authority in creditor hands, not because they are functionally superior (this would shift type toward Tangled Rope). If alternatives were deliberately foreclosed: the constraint is Snare, dressed as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_necessity_vs_creditor_power, conceptual, 'Whether conditionalities are the only feasible coordination mechanism or whether creditor power constrains the choice set.').

omega_variable(
    distributional_asymmetry_intentional_or_byproduct,
    'Are the distributional asymmetries (poor households bear subsidy removal; multinational acquirers capture assets; creditors secure debt service) intended features of the conditionality design, or unintended consequences of technically sound macroeconomic prescriptions?',
    'Documentary analysis of IMF/World Bank program design documents: (a) are distributional impacts formally modeled and evaluated during program design? (b) are design choices (e.g., targeted vs. universal subsidy removal) made with distributional intent or technical optimization? (c) Do subsequent evaluations acknowledge distributional outcomes as trade-offs, or are they treated as separate from program success (macro stability achieved, distribution neglected)? Interviews with program designers.',
    'If intentional: distributional asymmetry is a feature of the creditor-coordination design; the constraint''s extraction is deliberate and the rope framing obscures a Tangled Rope (coordination + extraction). If byproduct: the extraction is regrettable collateral damage of necessary macro discipline; the rope framing is more defensible but mandatrophy risk is elevated (the constraint solves a macro problem but creates a distributional problem that may motivate political backlash and non-compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_asymmetry_intentional_or_byproduct, empirical, 'Whether distributional asymmetries are designed features or unintended consequences of macro conditionality.').

omega_variable(
    kernel_reading_contestation,
    'Is the creditor-coordination reading of this kernel structurally defensible from first principles, or does it rely on empirical claims about coordination necessity and macro outcomes that are themselves contested among domain experts?',
    'Meta-analysis of academic research on structural adjustment outcomes: (a) do empirical studies support or contradict the claim that conditionalities improve macro stability? (b) Is there genuine theoretical disagreement (about whether coordination problems exist or how to solve them) or empirical disagreement (about whether conditionality is the best solution)? (c) Do creditor-institution economists (IMF, World Bank) and independent/debtor-country academics produce incompatible empirical findings, or do they agree on facts but differ on value judgments (whether trade-offs are justified)?',
    'If creditor-coordination reading is structurally defensible: the rope classification is robust and the constraint''s measure stands as one reading of a contested kernel. If the reading relies on empirical claims that are actively disputed: the reading''s coherence depends on accepting contested assumptions about coordination necessity and program efficacy (high Omega_C risk). If the disagreement is value-based rather than empirical: the readings are incommensurable rather than genuinely contested (different frameworks, not different facts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the creditor-coordination reading is theoretically sound or relies on contested empirical and normative claims.').

omega_variable(
    suppression_mechanism_coercion_or_structural,
    'Is the measured suppression (0.62) enforced through direct coercion (explicit threats of exclusion from financing) or through structural constraint (lack of alternative financing and debt overhang make exit prohibitively costly)?',
    'Historical analysis of IMF/World Bank enforcement actions: (a) have programs ever been suspended or funds withheld explicitly to punish non-compliance? (b) Have debtor states negotiated program terms or have they been presented as take-it-or-leave-it? (c) Is the exit option (reject conditionality, face capital flight) experienced as coercive by debtor governments, or as a natural consequence of unsustainable macro imbalances? (d) Comparative analysis of debtor-state behavior: do they comply because they fear explicit punishment or because non-compliance triggers market punishment (capital flight, currency collapse)?',
    'If suppression is direct coercion (explicit threats): the constraint is Snare dressed as coordination. If suppression is structural (market discipline): it is softer than categorical coercion but still leaves exit as theoretical only (trapped in game-theory sense). The distinction affects whether the rope framing is defensible: structural suppression can be part of coordination (prices as signals enforcing discipline); explicit coercion cannot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_coercion_or_structural, empirical, 'Whether suppression is explicit institutional coercion or market-mediated structural constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement_basis(stru_tr_t1980, observed).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(stru_tr_t1990, observed).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(stru_tr_t2000, observed).
narrative_ontology:measurement(stru_tr_t2008, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement_basis(stru_tr_t2008, observed).
narrative_ontology:measurement(stru_tr_t2016, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2016, 0.29).
narrative_ontology:measurement_basis(stru_tr_t2016, observed).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(stru_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(stru_be_t1980, observed).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement_basis(stru_be_t1990, observed).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement_basis(stru_be_t2000, observed).
narrative_ontology:measurement(stru_be_t2008, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement_basis(stru_be_t2008, observed).
narrative_ontology:measurement(stru_be_t2016, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement_basis(stru_be_t2016, observed).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(stru_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(stru_su_t1980, observed).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(stru_su_t1990, observed).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement_basis(stru_su_t2000, observed).
narrative_ontology:measurement(stru_su_t2008, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement_basis(stru_su_t2008, observed).
narrative_ontology:measurement(stru_su_t2016, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement_basis(stru_su_t2016, observed).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(stru_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debt_restructuring_mechanisms).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, capital_flight_prevention_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the structural_adjustment_conditionalities kernel. The creditor_coordination_reading frames conditionalities as solutions to real macroeconomic coordination problems; the debtor_extraction_reading frames them as neo-colonial instruments for extracting policy authority and assets; the hybrid_selectivity_reading frames them as selectively enforced discipline that varies by debtor state power and geopolitical positioning. All three readings measure the same institutional arrangement (IMF/World Bank programs with binding conditions) but instantiate different constraints because their ε values, beneficiary/victim structures, and founding-problem framings diverge. This reading has ε=0.58 and claims rope; the sibling readings have higher ε and claim snare or tangled_rope. The divergence is not measurement error — it is the structural output of different readings of a contested kernel. Each reading is ε-invariant and coherent on its own premises; the corpus captures the kernel contest through decomposition into three linked constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
