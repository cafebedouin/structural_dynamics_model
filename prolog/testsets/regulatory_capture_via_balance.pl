% ============================================================================
% CONSTRAINT STORY: regulatory_capture_via_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_via_balance, []).

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
 *   constraint_id: regulatory_capture_via_balance
 *   human_readable: Regulatory Capture Via Balance Sheet Opacity
 *   domain: economic/regulatory
 *
 * SUMMARY:
 *   Regulatory capture via balance sheet opacity is the structural mechanism
 *   by which financial institutions extract rents from depositors, market
 *   entrants, and the broader financial system while maintaining the
 *   appearance of neutral regulation. The constraint emerges from an
 *   authentic coordination problem — capital allocation across time and risk
 *   requires aggregation of diverse asset exposures into institutional
 *   vehicles. But that coordination function has become entangled with an
 *   extractive mechanism: the informational asymmetry that enables capital
 *   coordination simultaneously enables institutions to obscure true risk
 *   exposure from regulators and depositors. The regulator, originally
 *   designed to supervise capital adequacy, has itself become captured not
 *   through conspiracy but through structural mechanisms — revolving-door
 *   staffing, intellectual capture (economists trained in
 *   markets-discipline-themselves theory), and institutional identity fusion
 *   with the Basel framework and post-2008 reforms. The constraint's theater
 *   ratio (0.68) reflects that contemporary regulation appears comprehensive:
 *   stress testing, liquidity coverage ratios, leverage ratios, resolution
 *   planning, capital buffers. These are real mechanisms. But their
 *   enforcement is attenuated — deprivation of material enforcement capacity
 *   (regulators lack pricing data, cannot easily value illiquid assets, face
 *   political constraint from incumbent-friendly administrations). The
 *   apparatus looks sophisticated; the teeth are missing.
 *
 * KEY AGENTS:
 *   - Systemically Important Financial Institutions (SIFIs): Primary beneficiary (institutional/arbitrage) — implicit government guarantee, regulatory forbearance, balance sheet opacity enables risk-shifting
 *   - Depositors: Primary victim (powerless/trapped) — no exit from deposit function, cannot verify risk, bear tail risk of failure
 *   - Market Entrants (Fintech Startups): Secondary victim (moderate/constrained) — face fixed compliance costs that incumbent banks amortize over scale; barriers to entry extraction is severe
 *   - Regulatory Agency: Institutional actor with captured identity (institutional/identity_locked) — professionally constituted through Basel framework; cannot perceive capture because reform would require institutional self-negation
 *   - Community Banks: Organized secondary victim (organized/constrained) — benefit from regulatory insulation but bear extraction through restricted access to capital markets
 *   - Analytical Observer: Sees full structure (analytical/analytical) — detects both genuine coordination function and extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_via_balance, 0.58).
domain_priors:suppression_score(regulatory_capture_via_balance, 0.65).
domain_priors:theater_ratio(regulatory_capture_via_balance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_via_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_via_balance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_via_balance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_via_balance, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_via_balance, "Regulatory Capture Via Balance Sheet Opacity").
narrative_ontology:topic_domain(regulatory_capture_via_balance, "economic/regulatory").

domain_priors:requires_active_enforcement(regulatory_capture_via_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_via_balance, regulated_financial_institutions).
narrative_ontology:constraint_beneficiary(regulatory_capture_via_balance, incumbent_industry_players).
narrative_ontology:constraint_victim(regulatory_capture_via_balance, depositors_and_creditors).
narrative_ontology:constraint_victim(regulatory_capture_via_balance, market_entrants).
narrative_ontology:constraint_victim(regulatory_capture_via_balance, regulatory_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPOSITOR (SNARE) — No meaningful exit from deposit accounts; cannot independently verify asset quality or risk exposure. Regulatory framework exists but enforcement is asymmetrically captured. Full extraction borne without agency or recourse.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STARTUP FINTECH ENTRANT (SNARE) — Must comply with regulatory requirements designed for incumbent banks. Compliance costs are fixed; incumbents amortize over trillion-dollar balance sheets. Entry barrier extraction is severe, exit is theoretically possible but at ruinous cost.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMUNITY BANK COALITION (TANGLED ROPE) — Benefits from regulatory stability and capital requirements that insulate from global competition. Also bears extraction: access to capital markets restricted by regulations designed for mega-banks. Mixed coordination-extraction at moderate power level.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEMICALLY IMPORTANT FINANCIAL INSTITUTION (ROPE) — Net beneficiary through regulatory forbearance and implicit government backing. Risk is socialized; gains are privatized. Experiences the constraint as coordination: balance sheet complexity serves as coordination mechanism for peer-to-peer capital flows while maintaining information asymmetry from regulators.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (PITON) — Professional identity fused with the regulated industry (revolving-door staffing, regulatory expertise concentrated in incumbent firms). Formally charged with enforcement but structurally captured. The performative regulation persists: stress tests, reporting requirements, capital adequacy frameworks. These are theater that maintains the appearance of supervision while actual enforcement is attenuated. Theater ratio is high because the regulatory machinery looks comprehensive but lacks teeth.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CAPTURED REGULATOR (PITON / IDENTITY_LOCKED) — The regulatory institution's professional identity is constituted through the regulatory framework it inherited (Basel Accords, post-2008 reforms). Exit would mean abandoning the entire legitimacy structure. Career trajectories are built within this system. The regulator sees light-touch enforcement as pragmatism (markets discipline themselves, regulations must be balanced) rather than capture. This is identity lock: the institution cannot perceive its own capture because perception reform would require institutional suicide.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Full structural visibility. The constraint serves genuine coordination functions (capital allocation, price discovery, risk transfer across time horizons). It also extracts asymmetrically (depositors subsidize institutions, entrants subsidize incumbents). Effective extraction is high (chi ≈ 0.72 at this position) because depositors have zero exit and institutions have global arbitrage options. The theatrical regulation persists because it appears balanced while creating asymmetric real-world outcomes.
constraint_indexing:constraint_classification(regulatory_capture_via_balance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_via_balance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_via_balance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_via_balance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_via_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_via_balance, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_via_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderately high. The constraint extracts significantly through implicit guarantee subsidy, entry barrier imposition, and depositor risk-shifting. It is not a pure snare (ε ≥ 0.46 would be required for snare gate alone) because genuine coordination functions exist — capital aggregation, price discovery, risk transfer across time. The extraction is systematic but not total. Suppression (0.65): Moderately high. Barriers to exit include technological lock-in (deposit accounts), regulatory requirement (depositors must use licensed institutions), information asymmetry (true risk exposure is obscured), and political coordination (incumbents have concentrated lobbying power). Theater ratio (0.68): High. Contemporary financial regulation has grown more complex (stress tests, liquidity ratios, resolution plans) but enforcement intensity has declined. The apparatus is substantial; its impact is attenuated. This ratio reflects the gap between regulatory appearance and enforcement reality. The measurement trajectory shows theater rising faster than extractiveness (theater at 0.42→0.68, extractiveness at 0.38→0.58), which is the signature of Goodhart drift — the regulatory apparatus is optimizing for the appearance of supervision rather than actual risk prevention.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the diagnostic power of multi-perspectival classification. The same structural arrangement — balance sheet opacity, implicit guarantees, regulatory complexity — appears as coordinating value creation (from the SIFI view), protection mechanism (from the regulator's captured view), extraction mechanism (from the depositor's view), and barrier (from the entrant's view). Each perspective is analytically correct from its position. The gap between them reveals the constraint's true nature: it is not primarily about market failure (which would suggest Rope) but about asymmetric information and regulatory capture creating extraction channels. The identity_locked perspective on the regulator reveals a specific binding mechanism: the regulator's legitimacy is fused with the regulatory framework itself. Admitting capture would require admitting institutional failure. This is why regulatory reform is so difficult — it requires the regulator to undertake a kind of institutional suicide.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and structural position. SIFIs are beneficiaries with global arbitrage capacity (can exit national jurisdiction, can shift risk through derivatives) → low d (0.10-0.20). Depositors are victims with no exit (cannot switch to alternative banking systems) → high d (0.90-0.95). Entrants are victims with expensive exit (can comply but at prohibitive cost) → high d (0.70-0.80). The regulatory agency occupies a unique position: institutional power but identity-locked exit. The agency is structurally capable of enforcing regulations (has legal authority, staffing, technical capacity) but cognitively captured by the Basel framework. d for the agency ≈ 0.45 (neither full beneficiary nor full victim, but caught between structural position and cognitive frame). The identity-locked classification for the regulatory agency reflects that the binding mechanism is not material (they cannot be fired for light-touch enforcement; there is political support for market-friendly regulation) but cognitive (the regulator's professional identity makes stringent enforcement unthinkable).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through the tangled_rope classification. The constraint has a genuine coordination function (capital allocation, price discovery, risk transfer) which prevents it from being classified as pure snare. But it also has asymmetric extraction (depositors subsidize institutions, entrants subsidize incumbents) which prevents it from being classified as pure rope. The coordination and extraction are structurally entangled — the opacity that enables capital aggregation also enables risk-shifting. The mandatrophy is prevented by the tangled_rope gate: requires beneficiaries (SIFIs, incumbents), victims (depositors, entrants), and active enforcement (the regulatory apparatus). All three conditions are met. The constraint is not a coordination mechanism that happens to have costs (Rope). It is a hybrid where extraction depends on the coordination function to operate — remove the coordination and the extraction mechanism collapses, but the coordination mechanism alone would still require suppression of alternatives (closed deposit accounts, regulatory barriers) to function. This entanglement is the mandatrophy-resolving feature: the constraint is irreducible to either type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accounting_standard_manipulation,
    'Are accounting standards (mark-to-model valuations, loss provisioning methodologies) capturing true economic reality or enabling endemic low-grade misrepresentation?',
    'Post-crisis asset liquidation analysis: compare marked values to realized recovery rates; measure frequency of ''surprise'' impairments that should have been anticipated',
    'If standards capture reality well: balance sheet opacity is coordination cost (raise extractiveness floor to 0.15). If standards systematically understate risk: the opacity is intentional extraction mechanism (keep extractiveness at 0.58+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accounting_standard_manipulation, empirical, 'Whether accounting standards enable systematic understatement of asset risk').

omega_variable(
    regulatory_forbearance_intentionality,
    'Is regulatory light-touch a deliberate choice based on theory (market discipline works) or structural inability to enforce (capture + complexity)?',
    'Archival analysis of regulatory decision-making; interviews with exit-path regulators (those who left for explicit ethical reasons); comparison of enforcement intensity before vs after revolving-door rotation events',
    'If deliberate choice: extract extraction component (claim snare only from depositor view). If structural inability: snare classification broadens to regulatory agency itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_forbearance_intentionality, empirical, 'Whether regulatory forbearance is intentional or structural capture').

omega_variable(
    substitution_velocity_in_entrant_barrier,
    'How quickly can compliant fintech platforms substitute for incumbent bank functions as regulatory complexity becomes prohibitive?',
    'Time series of market share transitions for payments, lending, deposit-taking; measurement of startup-to-scale velocity in jurisdictions with different regulatory burdens',
    'If fast substitution (< 3 years): entrant snare is temporary, scaffold classification applies. If slow (> 10 years): snare persists, extraction is durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_velocity_in_entrant_barrier, empirical, 'Velocity of fintech substitution under regulatory burden').

omega_variable(
    implicit_guarantee_pricing,
    'Do market prices reflect the implicit government guarantee (''too big to fail'') embedded in SIFI debt?',
    'Comparison of CDS spreads on SIFI debt vs equally-risky non-guaranteed debt; measurement of SIFI cost-of-capital benefit attributable to implicit backing',
    'If reflected: the constraint''s coordination function is real but extraction is transparent to markets (downgrade theater and beneficiary claims). If not reflected: extraction is hidden, theater is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_guarantee_pricing, empirical, 'Market pricing of implicit government guarantee for SIFIs').

omega_variable(
    identity_lock_binding_mechanism,
    'What breaks a captured regulator''s identity lock — external shock (crisis forcing reform), generational turnover, or institutional restructuring?',
    'Historical case study of regulatory agency reform: measurement of reform intensity vs explanatory variables (leadership change, political pressure, external shock, succession planning)',
    'If shock-driven: the constraint persists until next crisis. If rotation-driven: gradual reform possible. If structural: reform requires institutional dissolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_binding_mechanism, conceptual, 'Mechanism for breaking identity lock in captured regulatory agencies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_via_balance, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_via_balance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regcap_tr_t8, regulatory_capture_via_balance, theater_ratio, 8, 0.58).
narrative_ontology:measurement(regcap_tr_t16, regulatory_capture_via_balance, theater_ratio, 16, 0.68).
narrative_ontology:measurement(regcap_tr_t24, regulatory_capture_via_balance, theater_ratio, 24, 0.75).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_via_balance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(regcap_be_t8, regulatory_capture_via_balance, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(regcap_be_t16, regulatory_capture_via_balance, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(regcap_be_t24, regulatory_capture_via_balance, base_extractiveness, 24, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_via_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(regulatory_capture_via_balance, 0.2).
narrative_ontology:affects_constraint(regulatory_capture_via_balance, too_big_to_fail_subsidy).
narrative_ontology:affects_constraint(regulatory_capture_via_balance, depositor_insurance_moral_hazard).
narrative_ontology:affects_constraint(regulatory_capture_via_balance, fintech_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% Regulatory capture via balance sheet opacity decomposes into three linked constraints: (1) too-big-to-fail implicit subsidy (ε≈0.30, Rope for institutions, Snare for taxpayers); (2) depositor insurance moral hazard (ε≈0.45, Tangled Rope); (3) fintech regulatory arbitrage (ε≈0.65, Snare for startups). This story focuses on the unified capture mechanism that enables all three to persist. The three downstream constraints have higher extractiveness because they are more visible — this story is about the opacity mechanism that keeps all three hidden.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_via_balance, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
