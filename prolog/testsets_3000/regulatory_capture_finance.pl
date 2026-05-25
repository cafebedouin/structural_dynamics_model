% ============================================================================
% CONSTRAINT STORY: regulatory_capture_finance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_finance, []).

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
 *   constraint_id: regulatory_capture_finance
 *   human_readable: Regulatory Capture in Financial Services
 *   domain: financial_regulation/political_economy
 *
 * SUMMARY:
 *   Regulatory capture in financial services is a structural constraint where
 *   financial institutions extract economic rents by controlling the
 *   regulatory apparatus nominally designed to constrain them. The constraint
 *   exhibits multiple classification types across different structural
 *   positions: retail consumers experience it as a snare with no exit;
 *   competing firms experience mixed coordination and extraction; the
 *   regulator experiences identity-locked institutional capture; and the
 *   analytical observer risks naturalizing what is a contingent institutional
 *   arrangement as an inevitable law of political economy. The constraint
 *   involves both genuine coordination (rule-setting, safety standards,
 *   information standards) and asymmetric extraction (competitors priced out
 *   via compliance moats, systemic risk built into deregulatory cycles,
 *   consumer protections hollowed of enforcement). Theater is high (0.65)
 *   because regulatory apparatus maintains elaborate performative compliance
 *   frameworks (Dodd-Frank structure, stress testing rituals, consumer
 *   protection agencies) while core enforcement capacity atrophies under
 *   industry lobbying. Extractiveness has trended upward from 0.35 to 0.62
 *   over the measurement interval, reflecting the accumulation of regulatory
 *   concessions and erosion of post-2008 enforcement momentum.
 *
 * KEY AGENTS:
 *   - Large Financial Institutions (JPMorgan, Bank of America, Goldman Sachs): Primary beneficiaries (institutional/arbitrage) — extract economic rents through regulatory moats, preferential access to regulators, favorable rule interpretation, and light-touch enforcement
 *   - Retail Consumers: Primary victims (powerless/trapped) — face predatory lending, hidden fees, adverse contract terms with no viable alternatives in concentrated markets
 *   - Regulatory Agencies (Federal Reserve, OCC, SEC): Captured institutions (institutional/identity_locked) — nominally independent but structurally fused with industry through revolving-door employment, industry participation in rulemaking, and funding/staffing dependency on industry-friendly policies
 *   - Competitive Entrants (Community Banks, Fintechs): Secondary victims (moderate/constrained) — face regulatory moats designed by larger competitors; must navigate compliance costs that favor incumbent scale; some can arbitrage regulatory gaps but lack the exit option of relocation or political influence
 *   - Financial System Stability: Systemic victim (powerless/trapped) — macro-prudential regulation is undermined during profit cycles and restored only after crisis; the constraint builds instability into the regulatory cycle
 *   - Reform Coalition (Consumer Advocates, Progressive Legislators, Academic Economists): Organized challengers (organized/constrained) — perceive genuine coordination mechanisms but also the extraction; lack political power to overcome industry lobbying; constrained by electoral cycles and budgetary limits
 *   - Analytical Observer (Political Economists, Public Choice Theorists): Risks naturalizing capture as inevitable law of regulatory dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_finance, 0.62).
domain_priors:suppression_score(regulatory_capture_finance, 0.68).
domain_priors:theater_ratio(regulatory_capture_finance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_finance, extractiveness, 0.62).
narrative_ontology:constraint_metric(regulatory_capture_finance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_finance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_finance, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_finance, "Regulatory Capture in Financial Services").
narrative_ontology:topic_domain(regulatory_capture_finance, "financial_regulation/political_economy").

domain_priors:requires_active_enforcement(regulatory_capture_finance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_finance, large_financial_institutions).
narrative_ontology:constraint_beneficiary(regulatory_capture_finance, regulatory_agencies).
narrative_ontology:constraint_victim(regulatory_capture_finance, retail_consumers).
narrative_ontology:constraint_victim(regulatory_capture_finance, financial_system_stability).
narrative_ontology:constraint_victim(regulatory_capture_finance, competitive_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Retail consumers face predatory lending, hidden fees, and adverse contract terms with no viable alternatives. Exit options are severely constrained by market concentration and information asymmetry. They perceive the regulatory system as colluding with lenders rather than protecting them. Maximum extraction, full suppression.
constraint_indexing:constraint_classification(regulatory_capture_finance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Systemic financial stability is treated as a victim — premature deregulation and competitive forbearance create tail risks. The regulatory agency is captured such that macro-prudential safeguards are systematized away during profit cycles, then restored only after crisis. The constraint ensures instability is built into the system structure.
constraint_indexing:constraint_classification(regulatory_capture_finance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Smaller financial firms face genuine coordination with regulators (shared information standards, shared safety goals) alongside asymmetric extraction via regulatory moats. Larger competitors lobby for rules that increase compliance costs, pricing out smaller players. Constrained exit — they need banking licenses and regulatory approval to operate, but some can arbitrage geographic or product-specific regulatory gaps.
constraint_indexing:constraint_classification(regulatory_capture_finance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The systemically important financial institution experiences the constraint as pure coordination with regulators. They participate in rule-setting, provide 'industry expertise' to regulatory agencies, and have exit options (relocate to friendlier jurisdictions, offshore operations, regulatory arbitrage across borders). The constraint solves their collective action problem: how to extract economic rents while maintaining the veneer of legitimate regulation.
constraint_indexing:constraint_classification(regulatory_capture_finance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The regulatory agency itself is a victim of capture through institutional identity fusion. The agency's mission, career pathways, hiring pool, and professional culture are constituted through its relationship with the regulated industry. Regulators rotate between agencies and financial sector employers; the regulatory perimeter is defined through industry input; budget and staffing depend on industry-friendly regulations that generate fee revenue for government. Exit (genuine adversarial regulation) would require the agency to become a different institution — destroying the professional identity of existing staff. Identity-locked at institutional power: structurally mobile (could enforce stricter rules) but cognitively captured (cannot perceive enforcement as compatible with legitimate regulation).
constraint_indexing:constraint_classification(regulatory_capture_finance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% Dodd-Frank regulation (2010) and its successors represent a degraded enforcement ritual. The apparatus exists, the rules are written, compliance departments maintain elaborate procedures — but the functional verification capacity has atrophied. Regulators lack adequate funding, technical expertise, and political will to enforce. The theater ratio is high: compliance theater masks weak enforcement. The constraint persists through institutional inertia (lawmakers fear deregulation but lack political power to strengthen oversight) and performative maintenance.
constraint_indexing:constraint_classification(regulatory_capture_finance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Reform-minded actors (consumer protection advocates, some legislators, academic economists) perceive genuine coordination mechanisms — rules against predatory lending, capital adequacy standards, stress testing — that do provide some protection. But they also perceive the extraction mechanism: each reform is watered down through lobbying, enforcement is starved of resources, and compliant industry players capture the rulemaking. Constrained exit: building political power requires decades; direct regulatory action faces industry pushback.
constraint_indexing:constraint_classification(regulatory_capture_finance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational horizon, regulatory capture appears inevitable — 'regulatory agencies always get captured by their industries' is treated as a natural law of political economy, derived from public choice theory and principal-agent logic. But the base structural data contradicts this naturalization. Multiple jurisdictions (Nordic banking models, Canadian regulatory independence, Australian ASIC mandates) show sustained non-capture. The mountain classification is a false summit: capture is contingent on institutional design choices, not inherent to the regulatory function.
constraint_indexing:constraint_classification(regulatory_capture_finance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_finance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_finance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_finance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_finance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_finance, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_finance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not maximal. Financial institutions extract substantial economic rents through regulatory advantages, but the extraction is partial — some consumer protections persist, some enforcement occurs, and crises periodically force resets. The metric reflects accumulated concessions rather than total regulatory capture. The upward trend (0.35→0.62) reflects the post-2008 degradation cycle: initial reform (Dodd-Frank 2010) gave way to gradual erosion through lobbying, budget starvation, and personnel rotation. Suppression (0.68): High. Barriers to exit include market concentration (four banks hold ~40% of deposits), information asymmetry (consumers cannot evaluate complex financial products), network effects (switching banks is costly), and regulatory barriers (charter requirements, capital adequacy that favor incumbents). Retail consumers face near-total suppression; smaller competitors face high but partially surmountable barriers. Theater ratio (0.65): Moderately high. The regulatory apparatus maintains elaborate performance frameworks (stress testing, consumer complaint systems, compliance audits) while enforcement capacity has atrophied. The 2008 crisis forced more rigorous theater; the subsequent period saw gradual reversion to lower-effort compliance. The measurement interval (0–15) captures this degradation: theater rises from 0.40 to 0.72 as the constraint shifts from recovery (2010–2012) through regulatory fatigue (2012–2024) to possible reset (future).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the large institution's rope and the consumer's snare is fundamental: one agent perceives legitimate coordination while the other perceives pure extraction. This is not disagreement about facts but genuinely different structural experiences. The coordinating rule-set (capital adequacy, liquidity ratios, stress testing) does provide the institution with useful signals and collective action benefits. The same rule-set gives consumers no direct benefit — they experience only the extracted rents (higher fees, adverse selection into risky products, predatory lending). The captured regulator bridges these perspectives but is itself captured: they believe they are coordinating (neutral arbiter, technical expert, industry partner) but are actually enforcing extraction. The piton perspective shows that this bridge is increasingly performative — the regulatory apparatus has grown while enforcement has shrunk, suggesting that the coordination function has degraded to theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation follows the beneficiary/victim/exit chain. Large institutions are beneficiaries with arbitrage exit → low d → low/negative χ. Consumers are victims with trapped exit → high d → high χ. Competitive entrants are victims with constrained exit → moderate-high d → moderate χ. The captured regulator is the critical case: they are nominally an institutional agent with the power to enforce rules (could be a powerful beneficiary), but their identity is fused with the industry through career paths and professional culture (identity_locked exit). This identity lock prevents them from experiencing themselves as extractive even though they are enforcing extraction. Their directionality d derives not from their formal power but from their actual structural position: constrained to defend the existing regulatory framework, which benefits large institutions. The directionality override for captured regulators (d ≈ 0.45) reflects this: they are neither pure beneficiaries (institution) nor pure victims (consumer), but captured intermediaries whose agency is hijacked by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. The constraint carries extractiveness of 0.62 but remains classified as tangled_rope rather than snare because it retains genuine coordination mechanisms (capital adequacy standards, stress testing, information disclosure requirements) alongside the extraction. If these were removed, the constraint would be pure snare. However, the mandatrophy question is whether the coordination is *functional* or merely *performative*. The high theater ratio (0.65) and upward extractiveness trend (0.35→0.62) suggest that the coordination is degrading toward pure extraction. A resolved mandatrophy would require evidence that either: (1) the coordination mechanisms are being strengthened (theater drops, enforcement intensity rises, extractiveness plateaus or falls), or (2) they are being eliminated entirely (classification shifts to snare). Current state is a transition: the constraint began as genuine tangled rope (2010, post-Dodd-Frank era with meaningful new rules and initial enforcement). It has degraded toward snare through regulatory capture and enforcement atrophy. The analytical work is to determine whether the degradation is reversible or if the constraint will stabilize as a more extractive snare with vestigial coordination theater (piton dynamics). The reform coalition hypothesis is that crisis moments (financial instability, political shifts) can reset the constraint back toward functional coordination (true tangled rope or even rope). The captured regulator hypothesis is that the degradation is structural and will persist absent institutional redesign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revolving_door_causal_mechanism,
    'Does the revolving door between regulators and industry cause capture, or do pre-captured individuals self-select into those career paths?',
    'Longitudinal career tracking: comparison of former regulators'' positions and advocacy patterns before and after industry employment; institutional design experiment (implement fixed non-mobile regulatory staff vs current revolving-door system) measuring enforcement intensity and rule durability',
    'If causal: regulatory independence requires structural separation of personnel markets (life tenure, sealed-door careers, elevated compensation). If selection: capture could be addressed through better screening and identity-based recruitment. Current analysis assumes bidirectional causation (career incentives select for capture-prone individuals AND the employment pattern reinvokes capture once inside).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_causal_mechanism, empirical, 'Whether revolving door causes capture or selects for pre-captured individuals').

omega_variable(
    industry_expertise_necessity,
    'Is industry input genuinely necessary for regulatory expertise, or does it represent a power asymmetry disguised as technical necessity?',
    'Comparative analysis of regulatory regimes with vs without industry participation (e.g., SEC enforcement division vs CFTC commodity trading divisions); measurement of rule quality and enforcement effectiveness by participation intensity; expert panel review of technical rules to assess whether industry-provided expertise improved outcomes',
    'If necessary: capture is unavoidable cost of effective regulation; mitigations focus on offsetting industry power via counter-expertise (consumer advocacy funding, academic researchers in agencies). If power disguise: industry participation can be minimized; capture can be reduced by cutting these information channels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_expertise_necessity, empirical, 'Whether industry expertise input is technically necessary for regulation').

omega_variable(
    electoral_politics_binding,
    'Is regulatory capture driven by financial industry lobbying power, or is it constrained/enabled by the electoral politics of financial sector campaign contributions?',
    'Causal inference analysis of campaign contribution timing vs regulatory timing; natural experiments (politicians losing industry support, elections removing capture-friendly legislators); jurisdictional comparison (countries with public campaign financing vs private show higher or lower capture rates)',
    'If lobbying-driven: capture is a money-in-politics problem; solutions involve campaign finance reform and political power redistribution. If electoral-driven: capture reflects voter preferences (many voters benefit from financial services growth); solutions require political education or institutional veto points that override electoral majorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_politics_binding, empirical, 'Whether capture is driven by lobbying money or electoral politics').

omega_variable(
    identity_locked_regulator_reversibility,
    'Can a captured regulator reverse course and enforce rules adversarially, or does institutional identity lock make this psychologically/institutionally impossible?',
    'Case studies of regulatory agencies that shifted from captured to independent (SEC post-2008, environmental agencies under enforcement-focused leadership); measurement of internal resistance, staff turnover, and enforcement escalation rates when agency leadership mandates stronger enforcement; structural analysis of whether reversals persist or revert after leadership change',
    'If reversible: capture is contingent on leadership and can be broken by external pressure or change in agency head. If locked: the captured identity persists regardless of formal mandate; institutional reform is required (new agency, personnel replacement, cultural change). Current analysis assumes partial lock that requires external shock to break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_regulator_reversibility, empirical, 'Whether captured regulatory identity can be reversed or is identity-locked').

omega_variable(
    financial_crisis_reset_mechanism,
    'Do financial crises create reset points where regulatory capture temporarily breaks, or is post-crisis regulatory strengthening merely an oscillation that reverts as memories fade?',
    'Historical analysis of 2008 (Dodd-Frank passed, then enforcement degraded), 1990s (S&L crisis led to tighter rules, then deregulation under Clinton), 1980s (Savings & Loan crisis). Measure: enforcement intensity (audit rates, enforcement actions, penalties) across crisis/non-crisis periods; longevity of regulatory changes post-crisis.',
    'If reset: crises provide windows for genuine reform; coalition-building should focus on crisis periods. If oscillation: capture is structural and crises provide only temporary respite; sustained reform requires institutional redesign, not leveraging crisis windows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_crisis_reset_mechanism, empirical, 'Whether financial crises reset capture or create temporary oscillations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_finance, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_finance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(regcap_tr_t5, regulatory_capture_finance, theater_ratio, 5, 0.55).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_finance, theater_ratio, 10, 0.65).
narrative_ontology:measurement(regcap_tr_t15, regulatory_capture_finance, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_finance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t5, regulatory_capture_finance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_finance, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(regcap_be_t15, regulatory_capture_finance, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_finance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_capture_finance, 0.18).
narrative_ontology:affects_constraint(regulatory_capture_finance, too_big_to_fail_moral_hazard).
narrative_ontology:affects_constraint(regulatory_capture_finance, predatory_lending_debt_traps).
narrative_ontology:affects_constraint(regulatory_capture_finance, financial_crime_enforcement_underinvestment).

% DUAL FORMULATION NOTE:
% Regulatory capture operates at the meta-level: it is the constraint on how other financial constraints (too-big-to-fail, predatory lending, crime enforcement) are regulated. This story models the regulatory capture mechanism itself; downstream stories model the specific extraction mechanisms that capture enables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_finance, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
