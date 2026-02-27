% ============================================================================
% CONSTRAINT STORY: g7_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_g7_debt_trap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: g7_debt_trap
 *   human_readable: G7 Debt Trap for Developing Nations
 *   domain: economic_policy/development_finance
 *
 * SUMMARY:
 *   The G7 debt trap operates through a structural mechanism: developing
 *   nations require capital for infrastructure and development, but access to
 *   capital is conditional on accepting structural adjustment programs
 *   designed and enforced by IMF and World Bank. These programs mandate
 *   austerity (cutting public spending), privatization (selling state-owned
 *   enterprises to foreign investors), currency devaluation (raising import
 *   costs and suppressing wages), and trade liberalization (exposing domestic
 *   industry to subsidized G7 imports). The constraint is enforced not
 *   through direct coercion but through capital market gatekeeping: countries
 *   that violate conditions lose access to World Bank funding, IMF program
 *   tranches, and rating-agency endorsement, triggering capital flight and
 *   currency collapse. The populations of debtor nations bear the costs
 *   (unemployment from austerity, service cutoffs from privatization, poverty
 *   from devaluation) while creditor nations and multinational corporations
 *   benefit (debt service, profit repatriation, asset acquisitions at
 *   fire-sale prices). The constraint has intensified over 40 years as
 *   emerging alternatives (China's Belt and Road, BRICS development banks)
 *   have reduced but not eliminated dependence on G7 financing. The theater
 *   ratio has risen from 0.42 to 0.58, reflecting that the functional
 *   justification for austerity (stabilization through demand destruction)
 *   has been empirically falsified by decades of research, yet the policy
 *   persists through institutional doctrine and bureaucratic inertia.
 *
 * KEY AGENTS:
 *   - Debtor Nation Populations: Primary victim (powerless/trapped) — bear full cost of austerity, privatization, wage suppression
 *   - Domestic Industry Sectors: Secondary victim (moderate/trapped) — forced into bankruptcy through privatization mandates and tariff reduction
 *   - G7 Creditor Nations: Primary beneficiary (institutional/arbitrage) — capture debt service, foreign direct investment, geopolitical leverage
 *   - Multinational Corporations: Secondary beneficiary (powerful/arbitrage) — direct gainers from privatization and wage suppression
 *   - IMF/World Bank Bureaucracy: Institutional enforcer (institutional/arbitrage) — maintains conditionality through program design and capital rationing, but doctrine is degraded
 *   - G77 Alliance / Debtor Coalition: Organized resistance (organized/constrained) — emerging counterbalance through BRICS alternatives and collective debt restructuring
 *   - Sovereign Fiscal Autonomy: Abstract victim (analytical/analytical) — the constraint destroys state capacity for economic policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(g7_debt_trap, 0.68).
domain_priors:suppression_score(g7_debt_trap, 0.72).
domain_priors:theater_ratio(g7_debt_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(g7_debt_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(g7_debt_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(g7_debt_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(g7_debt_trap, snare).
narrative_ontology:human_readable(g7_debt_trap, "G7 Debt Trap for Developing Nations").
narrative_ontology:topic_domain(g7_debt_trap, "economic_policy/development_finance").

domain_priors:requires_active_enforcement(g7_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(g7_debt_trap, g7_creditor_nations).
narrative_ontology:constraint_beneficiary(g7_debt_trap, multinational_corporations).
narrative_ontology:constraint_beneficiary(g7_debt_trap, imf_world_bank_bureaucracy).
narrative_ontology:constraint_victim(g7_debt_trap, debtor_nation_populations).
narrative_ontology:constraint_victim(g7_debt_trap, domestic_industry_sectors).
narrative_ontology:constraint_victim(g7_debt_trap, sovereign_fiscal_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION POPULATION (SNARE) — Citizens bear full cost of structural adjustment: austerity, privatization of public services, currency devaluation, wage suppression. Exit options are severely constrained — national default triggers sanctions, capital flight, and humanitarian crisis. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.92. Maximum effective extraction; suppression through currency controls, capital controls, and IMF conditionality.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC INDUSTRY SECTORS (SNARE) — Manufacturing, agriculture, and state-owned enterprises are targeted for privatization and forced competition with subsidized G7 imports. Tariff reduction conditions eliminate protections. Local firms cannot exit without bankruptcy. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.94. Structural extraction through forced market liberalization.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: G7 CREDITOR NATIONS (ROPE) — View the constraint as rational coordination mechanism: enforcing property rights, enabling capital flows, and standardizing fiscal discipline. Exit is costless (can cease lending; borrower cannot cease repayment). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; effective extraction is negative (subsidy via interest rates, capital access).
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: G77 ALLIANCE (TANGLED ROPE) — Organized resistance (India, Brazil, South Africa, Nigeria coalitions) sees the constraint as hybrid: coordination function (capital access) mixed with extraction (conditionality). Coalition has some leverage (collective default threat, BRICS alternatives) but is constrained by global capital markets. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.62. Medium effective extraction due to coalition power and emerging alternatives (Belt and Road, China development banks).
constraint_indexing:constraint_classification(g7_debt_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IMF/WORLD BANK BUREAUCRACY (PITON) — Structural adjustment conditionality persists through institutional inertia and doctrine persistence (Washington Consensus) despite decades of evidence that austerity deepens crises. The bureaucratic apparatus performs its role (program design, surveillance, disbursement) but the functional justification (macroeconomic stabilization via demand destruction) has been empirically falsified. theater_ratio=0.58 reflects moderate performativity: surveillance reports are detailed but recommendations are template-driven. d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.05. Piton classification reflects degradation, not beneficiary status.
constraint_indexing:constraint_classification(g7_debt_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTINATIONAL CORPORATIONS (SNARE) — Direct beneficiary of privatization mandates, tariff reduction, currency devaluation (lowers labor costs), and wage suppression. Can exit debtor nations costlessly; domestic firms cannot. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Net beneficiary through forced privatization and liberalization. Snare classification from perspective of structural dependency those firms create in debtor economies.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY VIEW (SNARE) — From a civilizational perspective, the constraint is the destruction of sovereign fiscal capacity: debtor nations lose control over monetary policy (external anchor), fiscal policy (primary balance targets), exchange rates (devaluation mandates), and sector-level investment decisions (privatization). Once lost, sovereignty is not recovered in a generation. d≈0.98, f(d)≈1.43, σ=1.2 → χ≈1.17. This perspective sees the constraint as irreversible extraction of state capacity itself — a Snare on national sovereignty.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(g7_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(g7_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(g7_debt_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(g7_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(g7_debt_trap, TR),
    TR >= 0.70.

:- end_tests(g7_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant rents through debt service (interest payments flowing to G7 banks), asset sales (privatization at depressed prices to foreign investors), and labor suppression (currency devaluation lowers real wages). The extraction is not as severe as pure debt peonage (0.90+) because some debtor nations have escaped (China, Vietnam, India) through alternative models, and the constraint is not backed by physical coercion. But for sub-Saharan Africa and Central America, extractiveness approaches 0.80. Suppression (0.72): High. Exit options are severely restricted. Default triggers capital flight, currency collapse, and humanitarian crisis. Domestic political space is constrained by IMF surveillance and programmatic conditions. Central banks lose monetary policy autonomy. Trade policy is locked in by WTO commitments coerced by previous programs. The suppression is enforced systemically through capital market gatekeeping rather than military force, but the result is structural immobility. Theater ratio (0.58): Moderate. The functional justification for austerity (macroeconomic stabilization) has been repeatedly falsified in peer-reviewed literature (Stiglitz, Rodrik, Galbraith). Yet IMF programs continue to mandate austerity, suggesting institutional persistence rather than evidence-based design. However, theater is not total — some programs (debt relief initiatives, expanded poverty reduction focus) show partial institutional learning. The theater ratio has increased from 0.42 (1980s) to 0.58 (present), indicating growing tension between doctrine and evidence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. Debtor nation populations see pure extraction (Snare) with no coordination benefit — austerity cuts public health and education while debt service increases. Multinational corporations see pure coordination (Rope) — they are solving the legitimate coordination problem of capital allocation and disciplining fiscal policy. The G7 sees rational architecture (Rope) enforcing sovereign debt obligations. The IMF sees degraded institutional process (Piton) — the doctrine persists but the functional justification is empirically questioned. The G77 coalition sees mixed extraction and coordination (Tangled Rope) — the capital is necessary but the conditions are extractive; alternatives are emerging. The analytical sovereignty view sees irreversible destruction of state capacity (Snare on sovereignty itself). The perspectival gap between powerless/trapped (snare) and institutional/arbitrage (rope) is the maximum possible — the same structure appears as pure coordination to the beneficiary and pure extraction to the victim. This gap reveals the constraint's true nature: it is not disagreement about facts, but structural divergence in who bears costs and who captures benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Debtor nation populations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction coefficient; no exit options within the constraint. Domestic industry sectors: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction; forced into bankruptcy. G7 creditor nations: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Maximum beneficiary position; can cease lending costlessly while borrowers cannot cease repayment. Multinational corporations: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Direct beneficiary; can exit markets without cost. G77 coalition: Victim + constrained → d≈0.65, f(d)≈0.95. Significant extraction but coalition has some leverage (BRICS alternatives, collective default threat, geopolitical realignment). IMF/World Bank: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater_ratio gate, not from beneficiary status. Analytical sovereignty observer: Victim + trapped (for a state) → d≈0.98, f(d)≈1.43. Sovereignty is irreversible once lost; no exit path for 20+ years.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in G7 debt trap is the danger of naturalizing contingent institutional arrangements as immutable economic law. The IMF/World Bank narrative frames the constraint as 'rational discipline' (Rope) — debtor nations need conditionality to avoid fiscal irresponsibility and moral hazard. This naturalization masks the extractive function: conditionality enforces a particular model of development (neoliberal) that benefits G7 interests and multinational capital while destroying domestic institutions and human development in debtor nations. The mandate is that 'debt must be repaid, structural adjustment disciplines fiscal policy, and austerity stabilizes economies.' But the mandate is false: (1) debt service is not automatic — countries can restructure or default; (2) austerity in crisis does not stabilize — it deepens recessions (Keynesian empirical consensus); (3) the constraint persists despite falsification, indicating institutional inertia rather than truth. The mandatrophy resolution requires admitting that the constraint is a Snare maintained through capital market power, not a Rope justified by economic science. The classification as Snare with high theater (0.58) reflects exactly this: the constraint operates through institutional performance (IMF surveillance, program reviews, rating announcements) rather than functional economic stabilization. The mandatrophy is resolved by distinguishing the institutional narrative (coordination / discipline) from the structural reality (extraction of fiscal capacity and asset ownership).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_development_pathways,
    'Are alternative development financing models (China Belt and Road, BRICS development banks, Islamic finance) structurally equivalent to G7/IMF loans or do they offer genuinely different conditionality terms?',
    'Comparative analysis of loan terms, enforcement mechanisms, and borrower outcomes across G7 loans vs BRICS Development Bank vs Belt and Road Initiative; longitudinal tracking of GDP growth, inequality, and fiscal autonomy for countries using different financing sources',
    'If truly different: debtor nation coalition has real exit option (arbitrage), reducing d from 0.95→0.70, reclassifying snare to tangled rope. If equivalent: constraint remains snare; alternative narratives are theater. If worse: extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_development_pathways, empirical, 'Whether alternative development financing provides genuine exit from G7 terms').

omega_variable(
    austerity_empirical_failure,
    'Has structural adjustment austerity demonstrably failed to deliver macroeconomic stabilization and growth outcomes compared to alternatives (demand-side stimulus, industrial policy)?',
    'Meta-analysis of IMF programs with measured GDP growth, unemployment, and fiscal outcomes; comparison to counterfactual growth paths; systematic review of retracted or revised IMF forecasts; public admission by IMF leadership of doctrine error',
    'If confirmed failure: IMF/World Bank classification shifts from piton (degraded but maintained) to pure snare (enforcing a falsified theory). Theater_ratio would increase (pure performance). If mixed evidence: tangled rope confirmed. If still supported: doctrine defended.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(austerity_empirical_failure, empirical, 'Whether austerity conditionality achieves stated macroeconomic objectives').

omega_variable(
    debt_sustainability_definitions,
    'Are debt-to-GDP ratios and primary balance targets used to justify conditionality actually mathematically sustainable or do they encode extractive targets independent of national circumstances?',
    'Reconstruction of IMF debt sustainability analyses for 20+ programs; comparison of stated ceilings to historical defaults and crises; analysis of why countries below IMF ceilings still default (flawed model) vs why countries above ceilings stabilize (sovereignty and policy space matter more)',
    'If ceilings are extractive rather than sustainable: suppression metric increases from 0.72→0.85. If ceilings are scientifically justified: snare classification softens to tangled rope. If arbitrary: pure political enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_definitions, empirical, 'Whether debt sustainability targets are mathematically derived or politically enforced').

omega_variable(
    conditionality_enforcement_mechanisms,
    'What are the actual enforcement mechanisms when countries violate IMF conditions? Are there graduated consequences or binary exclusion?',
    'Case study analysis of countries that violated conditionality (Argentina 2001, Greece 2015, Sri Lanka 2022) and consequences (program suspension, capital flight, currency collapse, political instability); mapping of IMF leverage points (program tranches, capital account access, credit ratings)',
    'If enforcement is indirect (capital market gatekeeping): suppression is systemic, not institutional. If enforcement is direct coercion: suppression metric increases. If enforcement is weak: snare classification softens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_enforcement_mechanisms, empirical, 'Mechanisms through which IMF enforces compliance with conditionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(g7_debt_trap, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(g7dt_tr_t0, g7_debt_trap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(g7dt_tr_t20, g7_debt_trap, theater_ratio, 20, 0.5).
narrative_ontology:measurement(g7dt_tr_t40, g7_debt_trap, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(g7dt_be_t0, g7_debt_trap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(g7dt_be_t20, g7_debt_trap, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(g7dt_be_t40, g7_debt_trap, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(g7_debt_trap, enforcement_mechanism).
narrative_ontology:affects_constraint(g7_debt_trap, capital_flight_mechanics).
narrative_ontology:affects_constraint(g7_debt_trap, currency_devaluation_asymmetry).
narrative_ontology:affects_constraint(g7_debt_trap, privatization_fire_sales).

% DUAL FORMULATION NOTE:
% The G7 debt trap can be decomposed into three component constraints with different ε values: (1) debt service enforcement (ε≈0.30, Rope — legitimate coordination of capital repayment), (2) austerity program imposition (ε≈0.75, Snare — extraction of fiscal autonomy), (3) privatization mandates (ε≈0.68, Snare — forced asset sales to foreign investors). The aggregate constraint networks these three, producing the observed ε≈0.68. When debt relief initiatives temporarily suspend (1) or relax (2), the constraint is downgraded, and debtor nations show improved outcomes — confirming that the extraction (2) and (3) are contingent, not immutable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(g7_debt_trap, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
