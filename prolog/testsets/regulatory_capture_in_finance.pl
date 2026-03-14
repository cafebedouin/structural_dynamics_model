% ============================================================================
% CONSTRAINT STORY: regulatory_capture_in_finance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_in_finance, []).

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
 *   constraint_id: regulatory_capture_in_finance
 *   human_readable: Regulatory Capture in Finance
 *   domain: economics/regulatory_affairs
 *
 * SUMMARY:
 *   Regulatory capture in finance represents a structural constraint where
 *   financial institutions systematically shape the rules intended to
 *   constrain them. The constraint exhibits hybrid coordination-extraction:
 *   genuine coordination functions (capital standards, settlement
 *   infrastructure, systemic risk management) coexist with asymmetric
 *   extraction (favorable regulatory treatment, competitive barriers against
 *   smaller competitors, implicit subsidies through too-big-to-fail
 *   guarantees). The constraint's extractiveness has grown substantially over
 *   the measurement interval (0.35 to 0.68), particularly after financial
 *   crises trigger regulatory relaxation cycles. Theater ratio tracks
 *   closely, indicating that performative regulatory activity (stress tests,
 *   compliance reviews, agency pronouncements) increases alongside actual
 *   extraction, masking regulatory degradation. The constraint involves
 *   multiple institutional actors with different structural positions: large
 *   banks enjoy arbitrage options and preferential regulatory access; smaller
 *   competitors face disproportionate compliance burdens; retail consumers
 *   are trapped by systemic dependency; the regulatory agency has experienced
 *   identity capture where its institutional self-concept has fused with
 *   industry success. The analytical observer sees the constraint as genuine
 *   tangled rope: removing coordination entirely would collapse financial
 *   markets, but captured regulation allows extraction to continue.
 *
 * KEY AGENTS:
 *   - Large Financial Institutions: Primary beneficiary (institutional/arbitrage) — shape regulatory preferences through lobbying, revolving door, regulatory expertise capture; benefit from capital-light business models, implicit too-big-to-fail subsidy, favorable resolution frameworks
 *   - Regulatory Agency: Captured institutional actor (institutional/identity_locked) — identity has merged with industry success; maintains performative oversight while substantively approving industry preferences; impossible to reform from within captured identity frame
 *   - Retail Consumers: Primary victim (powerless/trapped) — structurally dependent on banking system for wages, mortgages, payments; no functional exit; bear costs through opaque fees, limited product innovation, systemic risk exposure
 *   - Small/Community Banks: Secondary victim (moderate/constrained) — face compliance costs that large banks amortize across scale; lack lobbying capacity of large banks; squeezed by regulatory preferences favoring systemically important institutions
 *   - Financial System Stability: Abstract victim (powerless/trapped) — regulatory capture reduces actual risk management while theater increases; crisis cycles perpetuate
 *   - Macro-Prudential Oversight Coalition: Organized counter-agent (organized/constrained) — Basel Committee, FSB, IMF building international frameworks to constrain capture; sunset logic assumes frameworks will mature into automatic coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_in_finance, 0.68).
domain_priors:suppression_score(regulatory_capture_in_finance, 0.72).
domain_priors:theater_ratio(regulatory_capture_in_finance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_in_finance, extractiveness, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_in_finance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regulatory_capture_in_finance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_in_finance, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_in_finance, "Regulatory Capture in Finance").
narrative_ontology:topic_domain(regulatory_capture_in_finance, "economics/regulatory_affairs").

domain_priors:requires_active_enforcement(regulatory_capture_in_finance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_in_finance, large_financial_institutions).
narrative_ontology:constraint_beneficiary(regulatory_capture_in_finance, regulatory_agency).
narrative_ontology:constraint_victim(regulatory_capture_in_finance, retail_consumers).
narrative_ontology:constraint_victim(regulatory_capture_in_finance, financial_system_stability).
narrative_ontology:constraint_victim(regulatory_capture_in_finance, competitive_smaller_banks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL CONSUMER (SNARE) — Trapped by systemic dependency; cannot exit the financial system entirely. Bears costs through opaque fees, limited product choices, predatory practices designed to exploit regulatory gaps, and exposure to systemic risk created by under-regulated mega-banks. No real alternatives; suppression is structural and total.
constraint_indexing:constraint_classification(regulatory_capture_in_finance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BANK (TANGLED ROPE) — Constrained by regulatory compliance costs that disproportionately burden smaller institutions while large banks leverage economies of scale and regulatory access. Experiences both coordination (industry-wide capital standards, payment infrastructure) and asymmetric extraction (large banks' regulatory preferences become law, competitive advantage through deregulation capture).
constraint_indexing:constraint_classification(regulatory_capture_in_finance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE BANK (ROPE) — Net beneficiary. Experiences regulation primarily as coordination of shared infrastructure (settlement systems, capital frameworks) with minimal extraction burden. High exit options through regulatory arbitrage (moving operations, regulatory shopping). Coordination benefits exceed extraction costs; constraint appears as natural market structure.
constraint_indexing:constraint_classification(regulatory_capture_in_finance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (PITON) — The regulator's institutional identity has become fused with industry success; its mission of 'ensuring financial stability' has been captured and redefined as 'ensuring large bank profitability.' The agency maintains performative oversight rituals (stress tests, compliance reviews) while substantively approving industry preferences. Theater ratio high; actual regulatory function degraded. Exit options are identity-locked: redefining the agency's role requires the agency to see itself differently, which its captured identity frame prevents.
constraint_indexing:constraint_classification(regulatory_capture_in_finance, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: MACRO-PRUDENTIAL COALITION (SCAFFOLD) — Organized international bodies (Basel Committee, FSB, IMF) recognize regulatory capture as a temporary coordination failure. Counter-mechanisms (countercyclical capital buffers, stress testing standards, resolution authority frameworks) are designed as sunset provisions: as financial stability frameworks mature and coordination becomes automatic, the need for captured discretion diminishes. Low effective extraction because organized actors see an exit path and building alternative structures.
constraint_indexing:constraint_classification(regulatory_capture_in_finance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational analytical position, financial regulation coordinates essential infrastructure (credit allocation, risk distribution, systemic stability) while simultaneously extracting wealth toward concentrated financial power. The constraint is genuine tangled rope: removing regulation entirely would collapse credit markets (coordination loss); leaving it captured allows extraction to continue unchecked (asymmetry persists). No single-axis solution; the analytical perspective reveals the hybrid nature and impossibility of purely extractive or purely coordinative readings.
constraint_indexing:constraint_classification(regulatory_capture_in_finance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_in_finance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_in_finance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_in_finance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_in_finance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_in_finance, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_in_finance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Large financial institutions extract substantial rents through regulatory advantages, implicit subsidies, and preferential treatment. The measurement trajectory shows acceleration post-2008: initial regulatory response (Dodd-Frank era) raised extractiveness modestly (0.35→0.42), but subsequent regulatory relaxation (2016-2024) enabled sharp increases (0.52→0.68). This pattern reflects the capture cycle: crisis triggers reform, but as crisis fades from memory, captured agencies relax rules. Suppression (0.72): Very high. Consumer exit is functionally impossible (structural dependency on financial system). Smaller competitors face regulatory barriers. Alternative market structures (decentralized finance, credit unions, non-bank lenders) exist at margins but cannot compete at scale. Regulatory agency's captured identity prevents internal correction. Theater ratio (0.68): High. Regulatory activity is heavily performative: stress tests assume favorable assumptions; compliance reviews produce boilerplate findings; agency pronouncements claim robust oversight while substantive rules favor industry. The theater increases alongside extraction, suggesting performative activity serves to obscure rather than constrain.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six distinct types from six positions. Large banks see Rope — they are solving a genuine coordination problem (capital standards, payment settlement) and extracting is invisible to them because they are net beneficiaries. The regulatory agency sees Piton — it maintains the performative ritual of oversight while its captured identity prevents recognition that the ritual is degraded. Small banks see Tangled Rope — genuine coordination benefits coexist with severe competitive handicaps. Consumers see Snare — pure extraction, no coordination benefit, no exit. The macro-prudential coalition sees Scaffold — temporary problem being solved by building alternative frameworks. The analytical observer sees Tangled Rope — the constraint is genuinely hybrid, and no single-perspective solution exists. The perspectival gap reveals the core mechanism: each perspective's classification depends on their structural relationship (beneficiary vs victim, arbitrage vs trapped) and what that relationship makes visible/invisible about the constraint's true nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Large banks occupy the position of net beneficiary with high exit options (arbitrage to other jurisdictions, regulatory shopping). The derivation chain: beneficiary status + arbitrage exit → low d → negative f(d) → they experience the constraint as coordination (Rope), not extraction. The regulatory agency is a second institutional beneficiary, but identity-locked: its exit options are constrained by the fact that questioning industry preferences would require the agency to redefine its institutional purpose, which it cannot do from within the captured frame. Derivation: beneficiary status + identity_locked exit → moderate-high d, combined with the piton theater gate, produces piton classification. Small banks are victims + constrained (can exit competitively but at high cost to market position); derivation → high d → tangled rope. Consumers are victims + trapped; derivation → maximum d → snare. The analytical observer is not a structural participant but an external analyzer; derivation uses the canonical analytical d ≈ 0.73, producing moderate chi and tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that regulatory capture exhibits genuine coordination properties (capital standards do reduce systemic fragility) alongside genuine extraction (large banks extract rents through regulatory preferences). The constraint cannot be honestly labeled as pure extraction (Snare from all perspectives) because: (1) some agents genuinely benefit from coordination functions, and (2) removing all regulation would collapse credit markets and make everyone worse off. The constraint also cannot be labeled as pure coordination (Rope from all perspectives) because: (1) large agents extract preferential treatment, (2) extraction is not distributed fairly, and (3) the coordination could be achieved with less asymmetry. Tangled Rope is the accurate classification from the positions that see the full structure (analytical observer, small banks). The false summit detection flags the analytical observer: regulatory capture might appear as an immutable natural law of financial systems ('regulatory bodies are always captured'), but the structural data reveals it as a contingent institutional arrangement subject to reform through counter-mechanisms (macro-prudential oversight, international coordination, personnel rotation policies). The mandatrophy resolution declares both that: (a) the constraint is genuinely hybrid, not a mislabeling of pure extraction as coordination, and (b) reform is possible because the extraction is structural-institutional, not natural-law, and therefore subject to deliberate reconstruction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    captured_regulator_agency_fusion,
    'Is the regulatory agency captured through structural incentives (revolving door, agency budget dependency on industry cooperation) or through identity fusion (the regulator''s institutional self-concept has merged with industry success)?',
    'Historical analysis of regulator decision-making when interests diverged; personnel tracking (revolving door prevalence); stated agency mission evolution; response to external pressure from non-industry stakeholders',
    'If structural: exit is possible through policy reform and personnel change. If identity-locked: reform requires the agency to redefine its own purpose, which it cannot do from within the captured frame. Identity lock is more stable — persists across personnel and policy changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(captured_regulator_agency_fusion, empirical, 'Whether regulatory capture operates through structural incentives or identity fusion').

omega_variable(
    macro_prudential_framework_effectiveness,
    'Do Basel III and post-2008 macro-prudential measures actually constrain large bank behavior or do they remain performative compliance rituals?',
    'Measurement of: (a) actual risk reduction vs stress test assumptions; (b) realized leverage ratios vs nominal capital requirements; (c) correlation of compliance with material regulatory action vs coordination',
    'If effective: scaffold classification confirmed, sunset logic valid. If performative: scaffold is aspirational, and the constraint remains snare/tangled rope indefinitely. Theater ratio would remain high despite macro-prudential overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(macro_prudential_framework_effectiveness, empirical, 'Whether post-2008 macro-prudential frameworks constrain or merely theatrically monitor risk').

omega_variable(
    retail_consumer_systemic_exit_capacity,
    'Can retail consumers exit the banking system functionally, or is structural integration into wage deposit, mortgage, and payment systems so complete that exit is material impossibility?',
    'Empirical mapping of consumer exit costs: non-bank alternatives for wages (crypto, community credit), mortgages (non-bank lenders, informal lending), payments (fee structures, acceptance). Historical shifts post-2008 and post-2020.',
    'If true material impossibility: powerless/trapped classification confirmed; snare from consumer perspective is stable. If partial alternatives exist: consumers are constrained rather than trapped; classification upgrades to constrained exit, reducing experienced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_consumer_systemic_exit_capacity, empirical, 'Whether consumer exit from banking system is functionally possible').

omega_variable(
    large_bank_regulatory_arbitrage_genuine_or_performative,
    'Do large banks genuinely possess regulatory arbitrage options (moving to other jurisdictions, regulatory shopping) or is the appearance of arbitrage itself a coordination fiction that maintains their goodwill with regulators?',
    'Historical data on actual bank relocation; cost-benefit analysis of moving vs capturing; comparative regulatory regime selection; whether threat of relocation is credible or performative signaling',
    'If genuine arbitrage: large bank''s rope classification confirmed; they can exit. If performative: large banks are constrained/trapped by their own systemic importance and regulator dependency, making their perspective closer to tangled rope than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(large_bank_regulatory_arbitrage_genuine_or_performative, empirical, 'Whether large bank regulatory arbitrage is genuine exit or performative posture').

omega_variable(
    extraction_flow_directionality_measurement,
    'What proportion of financial system rents are extracted by large banks toward consumers vs toward regulatory agency employees vs retained as systemic instability risk?',
    'Decomposition of financial services margins: fee structures, interest rate spreads, implicit subsidies from too-big-to-fail guarantee. Cross-reference with agency employee compensation and systemic risk indicators.',
    'If primarily bank→consumer: snare classification confirmed for consumers. If distributed across all three: constraint is more complex hybrid. If extraction flows toward agency: regulator is not merely captured but also extractive agent, complicating the institutional perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_flow_directionality_measurement, empirical, 'Distribution of financial extraction across beneficiary groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_in_finance, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_in_finance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(regcap_tr_t8, regulatory_capture_in_finance, theater_ratio, 8, 0.55).
narrative_ontology:measurement(regcap_tr_t16, regulatory_capture_in_finance, theater_ratio, 16, 0.68).
narrative_ontology:measurement(regcap_tr_t4, regulatory_capture_in_finance, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_in_finance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t8, regulatory_capture_in_finance, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(regcap_be_t16, regulatory_capture_in_finance, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(regcap_be_t4, regulatory_capture_in_finance, base_extractiveness, 4, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_in_finance, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_in_finance, too_big_to_fail_implicit_subsidy).
narrative_ontology:affects_constraint(regulatory_capture_in_finance, small_bank_regulatory_arbitrage).
narrative_ontology:affects_constraint(regulatory_capture_in_finance, consumer_fee_extraction).

% DUAL FORMULATION NOTE:
% Regulatory capture in finance decomposes into three structurally distinct constraints: (1) the implicit too-big-to-fail subsidy (ε≈0.55, snare), (2) the competitive disadvantage for small banks (ε≈0.48, tangled rope), and (3) consumer fee extraction (ε≈0.42, snare). The parent constraint (regulatory_capture_in_finance) represents the institutional mechanism that enables all three downstream constraints. Each downstream constraint has different beneficiary/victim groups and different measurement trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_in_finance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
