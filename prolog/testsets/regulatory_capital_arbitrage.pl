% ============================================================================
% CONSTRAINT STORY: regulatory_capital_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capital_arbitrage, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_capital_arbitrage
 *   human_readable: Regulatory Capital Arbitrage Across Jurisdictions
 *   domain: financial_regulation/institutional_economics
 *
 * SUMMARY:
 *   Regulatory capital arbitrage describes the structural phenomenon where
 *   multinational financial firms exploit differences in capital
 *   requirements, reserve rules, and leverage restrictions across
 *   jurisdictions to reduce capital costs while maintaining equivalent
 *   economic exposure. This constraint exhibits the full spectrum of DR
 *   classifications depending on observer position: multinational firms
 *   experience it as rational coordination (Rope), host jurisdiction
 *   regulators see it as a mixed coordination-extraction problem with
 *   constrained exit (Tangled Rope), retail depositors in low-regulation
 *   zones are trapped victims of the arbitrage process (Snare), international
 *   bodies see it as a solvable coordination failure with a sunset
 *   (Scaffold), and the constraint as a whole manifests characteristics of
 *   degraded post-crisis regulatory theater (Piton). The base extractiveness
 *   of 0.58 reflects moderate but significant extraction: firms capture
 *   meaningful capital cost savings while shifting systemic risk to
 *   less-regulated jurisdictions. The suppression of 0.65 indicates strong
 *   structural barriers to exit: depositors cannot easily relocate; host
 *   regulators face capital mobility constraints; domestic competitors cannot
 *   access arbitrage mechanisms without abandoning regulatory compliance.
 *   Theater ratio of 0.48 indicates substantial but not dominant performative
 *   content: stress tests, capital adequacy frameworks, and resolution
 *   mechanisms exist (functional elements) but contain loopholes that
 *   arbitrageurs exploit (performative elements).
 *
 * KEY AGENTS:
 *   - Multinational Financial Firms: Primary beneficiaries (institutional/arbitrage) — capture capital cost savings through regulatory shopping; low suppression of counter-moves due to high geographic mobility
 *   - Retail Depositors in Low-Regulation Zones: Primary victims (powerless/trapped) — structurally immobile; bear concentration risk when arbitrageurs shift capital; lack regulatory safety nets
 *   - Host Jurisdiction Regulators: Secondary actors (organized/constrained) — coordinate financial stability but constrained by capital mobility; face extraction of regulatory authority and deposit base to higher-arbitrage zones
 *   - Domestic Financial Competitors: Secondary victims (moderate/constrained) — disadvantaged vs arbitrageurs but can lobby for harmonization; moderate exit costs through relocation or regulatory reform
 *   - International Regulatory Bodies: Organized coalition (organized/mobile) — Basel Committee, FSB driving standards convergence; see constraint as temporary with sunset mechanism
 *   - Regulatory Agencies (Identity-Locked): Institutional actors (institutional/identity_locked) — constitutive identity as 'competitive financial hub' prevents perception of strict enforcement as compatible with mission
 *   - Shadow Banking Ecosystem: Structural shifter (institutional/mobile) — absorbs arbitrage mechanisms when banking regulations tighten; enables persistent extraction under different guise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capital_arbitrage, 0.58).
domain_priors:suppression_score(regulatory_capital_arbitrage, 0.65).
domain_priors:theater_ratio(regulatory_capital_arbitrage, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capital_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capital_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capital_arbitrage, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capital_arbitrage, tangled_rope).
narrative_ontology:human_readable(regulatory_capital_arbitrage, "Regulatory Capital Arbitrage Across Jurisdictions").
narrative_ontology:topic_domain(regulatory_capital_arbitrage, "financial_regulation/institutional_economics").

domain_priors:requires_active_enforcement(regulatory_capital_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capital_arbitrage, multinational_financial_firms).
narrative_ontology:constraint_beneficiary(regulatory_capital_arbitrage, regulatory_arbitrageurs).
narrative_ontology:constraint_victim(regulatory_capital_arbitrage, host_jurisdiction_financial_stability).
narrative_ontology:constraint_victim(regulatory_capital_arbitrage, retail_depositors_in_low_regulation_zones).
narrative_ontology:constraint_victim(regulatory_capital_arbitrage, competing_domestic_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL DEPOSITORS (SNARE) — Structurally trapped in jurisdictions with weak regulatory frameworks. Cannot exit to higher-security banking regimes without mobility/capital barriers. Bear full extraction risk when firms capture regulatory arbitrage gains and shift losses back to local depositor base through failures or asset transfers. Zero coordination benefit; pure extraction mechanism.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOST JURISDICTION REGULATORS (TANGLED ROPE) — Constrained by capital mobility: if they enforce strict capital requirements, firms relocate to lower-regulation zones. But they also coordinate genuine financial stability goals through regulatory coordination mechanisms (Basel Accords, stress testing). Active enforcement burden: must monitor regulatory gaps while maintaining competitive banking sector. Asymmetric extraction: regulatory burden falls on host jurisdiction; gains accrue to multinational extractors.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MULTINATIONAL FINANCIAL FIRMS (ROPE) — Experience the constraint as pure coordination: moving capital between regulatory regimes to optimize capital cost is efficient allocation. See regulatory framework differences as a functional market segmentation (arbitrage opportunity). High exit options: can relocate operations, shift legal entities, or lobby for favorable regulatory treatment. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC COMPETITORS (TANGLED ROPE) — Constrained by regulatory requirements in their home jurisdiction; cannot easily shift to arbitrage zones without abandoning domestic market access. Benefits from regulatory coordination mechanisms that prevent catastrophic failures, but bears extraction costs through competitive disadvantage vs. regulatory arbitrageurs. Constrained exit: can lobby for regulatory harmonization or relocate, but both are high-cost.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY BODIES (SCAFFOLD) — Organized agents (Basel Committee, FSB, IMF) see regulatory arbitrage as a temporary coordination failure being solved through convergence (Basel III, capital requirements harmonization, resolution frameworks). Active sunset mechanism: regulatory standards are tightening, closing arbitrage gaps. Low effective extraction because the organized coalition has agency and explicit exit path. Theater ratio remains moderate because actual enforcement lags standards development.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCIES (IDENTITY_LOCKED) — Structurally mobile (could harmonize standards, enforce strictly) but identity-locked into competitive regulatory jurisdictions. Regulatory identity is constituted through attracting financial sector activity ('financial hub' status, regulatory innovation). Cannot perceive strict enforcement as compatible with institutional mission — would require abandoning identity as competitive jurisdiction. Exit perceived as loss of regulatory authority and economic competitiveness.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: POST-CRISIS REGULATORY THEATER (PITON) — After 2008 financial crisis, regulatory frameworks expanded (Dodd-Frank, Basel III, Solvency II) but much implementation is performative: stress testing follows prescribed scenarios; risk models are mathematically sophisticated but rely on simplified assumptions; capital requirements exist but are arbitraged through securitization, off-balance-sheet structures, and geographic shifting. The regulatory apparatus persists through political legitimacy and inertia ('we're fixing the problem') despite degraded functional capacity to prevent arbitrage. Theater ratio high relative to actual extraction prevention.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal perspective, capital mobility across regulatory boundaries appears as an immutable feature of modern finance: markets always seek lowest-cost capital, and regulatory differences always create arbitrage. This naturalizes what is a historically contingent institutional arrangement (separate nation-states with independent regulatory authority over banking). However, structural data contradicts the mountain classification — the constraint is maintained through active enforcement failures and regulatory competition, not natural law. False summit: naturalizes contingent institutional design as inevitable.
constraint_indexing:constraint_classification(regulatory_capital_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capital_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capital_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capital_arbitrage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capital_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capital_arbitrage, TR),
    TR >= 0.70.

:- end_tests(regulatory_capital_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing. At interval start (t=0), extractiveness was 0.35 — regulatory arbitrage existed but was smaller in magnitude and less systematized. By t=10, extractiveness reached 0.58 as firms developed sophisticated strategies (securitization, derivative overlays, geographic shifting of legal entities) to exploit regulatory gaps. The increase reflects not new regulation but optimization of existing gaps. Suppression (0.65): High and stable. Retail depositors face immobility (language, capital barriers, institutional friction); host regulators face capital flight threats; competitors face the choice between market access (complying with strict rules in regulated zones) or arbitrage access (relocating). These barriers are structural and persistent. Theater ratio (0.48): Moderate, increasing. Post-2008 regulatory frameworks (Dodd-Frank, Basel III) created visible enforcement apparatus (stress tests, capital frameworks, resolution mechanisms) but implementation contains substantial arbitrage loopholes. Theater increased from 0.25 to 0.48 as regulatory frameworks became more elaborate while gap-closing effectiveness stagnated — more performative apparatus, similar or worse functional outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Multinational firms see a Rope (efficient capital allocation, coordination benefit). Host regulators see Tangled Rope (coordination of safety systems AND extraction of capital and authority). Retail depositors see Snare (pure extraction, no coordination benefit). Competitors see Tangled Rope (some coordination through regulatory frameworks, but asymmetric extraction through arbitrage disadvantage). International bodies see Scaffold (temporary problem with sunset). Regulatory agencies see Rope (coordination with arbitrage as optimization feature) when locked into competitive identity, but would see Tangled Rope if identity shifted to include deposit safety as primary mission. The piton perspective (post-crisis regulatory theater) sees the entire apparatus as degraded — regulatory expansion without functional gap-closure. The analytical observer risks seeing Mountain (capital mobility is a law of markets) but the constraint is manifestly contingent on institutional architecture (separate jurisdictions, capital mobility, regulatory discretion). The perspectival gap reveals fundamental tension between firms' experienced efficiency (Rope) and retail depositors' experienced extraction (Snare) — same constraint, opposite material outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational firms derive d ≈ 0.10-0.20 (beneficiary + arbitrage exit = negative or near-zero f(d)): capital cost reductions and strategic mobility mean the constraint subsidizes them. Host regulators derive d ≈ 0.55-0.70 (victim + constrained exit): capital flight threat and regulatory competitiveness constraints force them to tolerate arbitrage. Retail depositors derive d ≈ 0.90+ (victim + trapped): immobility in low-regulation zones and information asymmetries prevent exit; they bear concentration risk. Domestic competitors derive d ≈ 0.60-0.75 (victim + constrained exit): must choose between strict-regulation compliance (losing margin) or relocation (losing market). International regulatory bodies derive d ≈ 0.45-0.55 (mixed: coordinating but with power to shape standards): their institutional power to standardize means moderate d and lower chi despite being targets of extraction pressure. The directionality spreads reflect real structural differences in exit capacity and benefit flow.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVABLE THROUGH IDENTITY-LOCK AND INSTITUTIONAL REDESIGN: The mandatrophy is not whether regulatory arbitrage is coordination or extraction, but which agents' structural positions define the constraint's primary function. If primary function is capital allocation efficiency (multinational firm perspective, Rope), then extracted deposits and systemic risk are acceptable externalities. If primary function is deposit safety and systemic stability (depositor and host regulator perspective), then the constraint is a Snare mechanism extracting safety for capital cost savings. The constraint's claimed type (Tangled Rope) reflects that BOTH functions exist: genuine coordination (regulatory frameworks, stress tests) AND genuine extraction (asymmetric risk shifting). The mandatrophy resolves by recognizing that regulatory agencies' identity-lock prevents them from perceiving the constraint as primarily extractive even when structural evidence dominates. Decoupling competitive identity from regulatory authority would shift agency perspective from Rope to Tangled Rope (or Snare if depositor vulnerability is centralized). The constraint itself doesn't change — the institutional framing of what should be regulated changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_harmonization_pace,
    'Will international regulatory harmonization (Basel standards, capital requirement convergence) actually close arbitrage opportunities, or will firms continuously innovate new arbitrage pathways faster than regulators can harmonize?',
    'Longitudinal tracking of arbitrage mechanisms: measure frequency of new arbitrage strategies relative to pace of regulatory closure. Historical pattern: securitization, CDS, synthetic CDOs all emerged as arbitrage-closing regulatory innovation.',
    'If harmonization succeeds: scaffold perspective confirmed, constraint has genuine sunset. If firms outpace regulators: arbitrage becomes chronic Snare for depositors and perpetual Tangled Rope for host regulators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_harmonization_pace, empirical, 'Whether regulatory harmonization closes arbitrage faster than firms innovate new pathways').

omega_variable(
    regulatory_identity_lock_durability,
    'Is regulatory agencies'' competitive identity (financial hub status, innovation prestige) an identity lock preventing strict enforcement, or a rational institutional choice that could shift if incentive structures change?',
    'Comparative analysis of regulatory agencies that shifted from competitive to coordinated enforcement (e.g., post-2008 adoption of higher standards). Examine whether shift required identity reconstruction or merely incentive recalibration.',
    'If identity lock: agencies cannot perceive harmonization as compatible with institutional mission — arbitrage persists. If rational choice: coordination becomes possible if incentive structures align.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_identity_lock_durability, conceptual, 'Whether regulatory competition derives from institutional identity or rational incentives').

omega_variable(
    shadow_banking_regulation_lag,
    'Does financial regulation''s focus on banking sector miss the majority of arbitrage activity that has migrated to shadow banking (private equity, hedge funds, money market funds)?',
    'Comparative measurement of capital flows and leverage: regulated banking sector vs shadow banking sector. Assess whether regulatory arbitrage is shifting activity rather than preventing extraction.',
    'If true: regulatory framework addresses only visible extraction (banked system), while core extraction moves to unregulated shadow banking. Arbitrage constraint becomes Snare masked as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shadow_banking_regulation_lag, empirical, 'Whether regulatory focus on banking captures or misses primary arbitrage mechanisms').

omega_variable(
    depositor_mobility_threshold,
    'At what level of regulatory divergence do retail depositors gain meaningful exit options (relocation, alternative financial institutions, cryptocurrency, informal networks)? Below that threshold, are they genuinely trapped?',
    'Survey data on depositor mobility costs: relocation barriers, information asymmetries about regulatory safety, institutional friction. Identify thresholds where alternatives become accessible.',
    'If trapped: depositors are Snare victims. If mobile: classification shifts to Tangled Rope (constrained but not trapped). Reframes distributional justice implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depositor_mobility_threshold, empirical, 'At what regulatory divergence level do depositors gain exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capital_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capital_arbitrage, theater_ratio, 0, 0.25).
narrative_ontology:measurement(regcap_tr_t5, regulatory_capital_arbitrage, theater_ratio, 5, 0.38).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capital_arbitrage, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capital_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t5, regulatory_capital_arbitrage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regcap_be_t10, regulatory_capital_arbitrage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capital_arbitrage, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_capital_arbitrage, 0.12).
narrative_ontology:affects_constraint(regulatory_capital_arbitrage, too_big_to_fail_moral_hazard).
narrative_ontology:affects_constraint(regulatory_capital_arbitrage, shadow_banking_regulatory_gap).
narrative_ontology:affects_constraint(regulatory_capital_arbitrage, international_capital_flows_volatility).

% DUAL FORMULATION NOTE:
% Regulatory capital arbitrage is upstream of several financial stability constraints. The arbitrage mechanism enables moral hazard in TBTF dynamics (firms expand exposure knowing rescue likelihood), creates regulatory gaps that shadow banking exploits, and amplifies capital flow volatility by concentrating leverage in lowest-cost jurisdictions. Each downstream constraint has its own extractiveness value reflecting its specific causal pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capital_arbitrage, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
