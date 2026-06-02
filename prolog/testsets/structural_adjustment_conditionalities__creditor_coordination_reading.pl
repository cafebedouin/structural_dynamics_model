% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Structural Adjustment Conditionalities as Creditor Coordination
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities imposed by the IMF and World Bank
 *   are a contested institutional mechanism at the core of international
 *   development finance. This story instantiates ONE reading of the contested
 *   kernel: structural adjustment as a coordination mechanism for solving
 *   creditor-debtor moral hazard, capital-market information asymmetry, and
 *   capital flight risk. From this perspective, conditionalities are
 *   fundamentally a solution to a collective action problem: debtor nations
 *   have incentive to overspend and inflate away debt, capital markets cannot
 *   directly monitor fiscal discipline, and if confidence collapses, the
 *   debtor loses access to financing entirely. The mechanism solves this by
 *   providing a public commitment device: the IMF-monitored fiscal target
 *   becomes a binding rule that the debtor nation's fiscal authorities can
 *   point to when resisting domestic political pressure for deficit spending.
 *   This reading does not deny that conditionalities impose costs on domestic
 *   welfare beneficiaries (wage freezes, program cuts, public sector
 *   reduction). Rather, it frames those costs as necessary adjustment costs
 *   within the debtor nation's own fiscal structure, not as extraction by the
 *   creditor. The contested kernel is whether conditionalities are primarily
 *   (1) creditor coordination (this reading), (2) creditor extraction of
 *   structural rents and institutional profit (debtor_extraction_reading), or
 *   (3) a hybrid mechanism that selectively coordinates on some issues while
 *   extracting on others (hybrid_selectivity_reading). Each reading generates
 *   a different constraint with different ε values, different
 *   beneficiary/victim structures, and different analytical implications.
 *
 * KEY AGENTS:
 *   - Multilateral creditor institutions (IMF/World Bank): Institutional/arbitrage — enforce coordination mechanism; benefit from the conditionality framework maintaining capital-market access for debtor clients
 *   - International capital markets: Institutional/mobile — benefit from coordinated fiscal discipline and confidence signals; have exit option (can reallocate lending portfolio)
 *   - Debtor nation fiscal bureaucracy: Organized/constrained — fiscal authorities welcome conditionalities as political anchor; trade access to IMF financing for conditional commitment rules
 *   - Creditor nation governments: Institutional/arbitrage — benefit from capital-market stability and avoided sovereign defaults in debtor regions
 *   - Domestic welfare beneficiaries in debtor nations: Powerless/trapped — experience cost of adjustment (wage freezes, subsidy cuts, public sector reduction); cannot organize or exit
 *   - Future taxpayers (diffuse): Powerless/mobile — benefit from avoided default and preserved fiscal sustainability, but benefit is diffuse and non-excludable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.35).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities as Creditor Coordination").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '4f99664a-ade8-47b5-af77-32c4b8887a4d').
narrative_ontology:cs_kernel_codification('4f99664a-ade8-47b5-af77-32c4b8887a4d', formalized).
narrative_ontology:cs_authority_grounding('4f99664a-ade8-47b5-af77-32c4b8887a4d', expertise).
narrative_ontology:cs_interpretation_layer_present('4f99664a-ade8-47b5-af77-32c4b8887a4d').
narrative_ontology:cs_reading_relation('4f99664a-ade8-47b5-af77-32c4b8887a4d', debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f99664a-ade8-47b5-af77-32c4b8887a4d', hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('4f99664a-ade8-47b5-af77-32c4b8887a4d', foundational, capital_market_confidence_mechanism).
narrative_ontology:cs_axiom_status(capital_market_confidence_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4f99664a-ade8-47b5-af77-32c4b8887a4d', capital_market_confidence_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('4f99664a-ade8-47b5-af77-32c4b8887a4d', foundational, debtor_moral_hazard_necessity).
narrative_ontology:cs_axiom_status(debtor_moral_hazard_necessity, holdable).
narrative_ontology:cs_axiom_grounding('4f99664a-ade8-47b5-af77-32c4b8887a4d', debtor_moral_hazard_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('4f99664a-ade8-47b5-af77-32c4b8887a4d', sovereign_lending_coordination).
narrative_ontology:cs_drift_state('4f99664a-ade8-47b5-af77-32c4b8887a4d', contemporary_capital_controls_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4f99664a-ade8-47b5-af77-32c4b8887a4d', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_creditor_nations).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MULTILATERAL CREDITOR (ROPE) — IMF/World Bank sees conditionalities as solving a genuine coordination problem: debtor nation moral hazard + creditor-nation portfolio risk + capital-market confidence. The mechanism is coordination, not extraction. Low coercion because the debtor retains exit option (access alternative financing, endure capital flight). Beneficiary position with arbitrage exit (can reallocate lending portfolio, shift terms). Rope classification derives from pure coordination without asymmetric extraction.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: CAPITAL MARKETS (ROPE) — International investors need credible signals that debtor nations will not default through fiscal profligacy. Conditionalities provide those signals — a form of coordination that aligns incentives. The mechanism is public commitment via IMF surveillance, not coercion. Mobile exit (investors can diversify or exit markets). Experiences the constraint as solving the information asymmetry problem of sovereign lending — genuine coordination benefit.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: DEBTOR NATION FISCAL BUREAUCRACY (ROPE) — Central bank and finance ministry see conditionalities as commitment device: politically binding rules that enable orthodox monetary policy despite domestic political pressure for deficit spending. The constraint solves their credibility problem with capital markets. Exit is constrained (conditional access to IMF resources is valuable but costly to lose), but the coordination benefit is real — the bureaucracy voluntarily adopts these rules to gain credibility. This perspective is key to the creditor coordination reading: debtor fiscal authorities often welcome conditionalities as an anchor for their own policy autonomy against populist pressure.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMESTIC WELFARE BENEFICIARIES (TANGLED ROPE → SNARE RISK) — Public sector workers, subsidy recipients, welfare program beneficiaries experience the constraint as extraction: wage freezes, program cuts, reduced public employment. However, this reading does not deny this perspective's reality — it simply represents a different reading of the same kernel. From the creditor coordination perspective, these victims are understood not as extraction targets but as inefficient allocation within the debtor nation itself. This perspective demonstrates the contestation between readings: the victim's experience is real, but the analytical frame determines whether it counts as extraction or as necessary adjustment.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the creditor coordination reading, conditionalities solve a genuine coordination problem: (1) debtor moral hazard — nations have incentive to overspend and inflate away debt; (2) creditor information asymmetry — capital markets cannot directly monitor fiscal discipline; (3) capital flight risk — if confidence fails, debtor loses access to financing and faces crisis. The conditionality mechanism is coordination, not coercion. Theater ratio is low (0.42) because the mechanism operates through transparent fiscal targets and IMF surveillance, not through performative ritual. Extractiveness is moderate-low (0.28) because beneficiaries are broad (future taxpayers, capital markets, fiscal stability) not concentrated (the institution itself takes minimal profit margin).
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__creditor_coordination_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The creditor coordination reading emphasizes that the mechanism solves a genuine collective action problem rather than concentrating rents in the creditor. Beneficiaries are broad (future taxpayers, capital markets, fiscal sustainability) rather than concentrated in the creditor institution itself. The reading does not deny that concentration may occur in practice — that is the domain of the alternative readings — but this reading's structural assumption is that the coordination benefit is primary and widely diffused. Suppression (0.35): Moderate. Debtor nations have exit options: alternative financing sources (other lenders, debt restructuring, capital controls), changes in leadership that repudiate the program, or simply enduring capital flight consequences. The constraint operates through the debtor's desire for capital-market access, not through absolute prohibition. The suppression value reflects that the cost of these exits is real but not prohibitive. Theater ratio (0.42): Moderate-low. The creditor coordination reading emphasizes that the mechanism operates through transparent monitoring and public targets, not through performative ritual. IMF Article IV consultations are substantive surveillance exercises, not theater. Fiscal targets are verifiable metrics. The moderate value (not very low) acknowledges that some performative elements exist (IMF legitimacy maintenance, signaling value) but the core mechanism is functional coordination, not theatrical display.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the debtor nation fiscal bureaucracy's perspective (rope — welcome the commitment device for political protection) and the domestic welfare beneficiary's perspective (tangled_rope approaching snare — experience cost of adjustment without perceived benefit). This gap does not resolve through adding complexity to one reading. Rather, it reveals that the kernel 'structural adjustment conditionalities' admits multiple structural readings: the same institutional arrangement can be coordination for one actor and extraction for another. The creditor coordination reading makes this visible by defining beneficiaries (future taxpayers, capital markets) and explicitly not including domestic welfare beneficiaries as beneficiaries. The alternative readings will include them as victims and reframe the benefit structure. The perspectival gap is intentional — it marks the boundary between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   In the creditor coordination reading, directionality d is computed from the beneficiary/victim declarations and exit options. Creditor institutions benefit from the coordination mechanism (d ≈ 0.15 for institutional arbitrage position) — they experience low or negative effective extraction because their structural role is coordination, not extraction. Future taxpayers benefit from avoided default (d ≈ 0.05, diffuse beneficiary) but have no direct negotiating position. Domestic welfare beneficiaries are excluded from the beneficiary list in THIS reading — they are not counted as beneficiaries but as bearing adjustment costs within the debtor's own fiscal structure. The alternative readings will recategorize them as victims. This is not ambiguity; it is the structure of reading differentiation. Each reading makes different structural claims about who benefits and who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this kernel resolves by recognizing that multiple readings are live and defensible, each producing different classifications. The creditor coordination reading produces rope (low-moderate extractiveness, genuine coordination function, diffuse beneficiaries). The debtor extraction reading produces tangled_rope or snare (higher extractiveness, concentrated creditor benefit, clear victims). The hybrid reading produces tangled_rope (real coordination AND real extraction occurring simultaneously). The apparatus does not resolve which reading is 'correct' — it produces all three and notes the structural differences. This is the Deferential Realism solution to kernel contestation: generate the distinct constraints corresponding to each reading, note the omega-level ambiguities, and let policy analysis select based on empirical evidence and normative commitments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_default_rate,
    'What proportion of debtor nations without conditionalities would have defaulted or experienced capital flight within the same time period?',
    'Comparative historical analysis: countries with IMF programs vs countries without access (e.g., comparison to pre-IMF 1940s sovereign lending, or to nations that rejected IMF programs). Econometric estimation of default probability absent conditionalities.',
    'High counterfactual default rate (>40%): creditor coordination reading gains credibility — conditionalities prevent crises. Low counterfactual default rate (<15%): reading loses force — suggests over-intervention or that debtor self-interest provides sufficient discipline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_default_rate, empirical, 'Counterfactual default risk absent conditionalities').

omega_variable(
    alternative_coordination_mechanisms,
    'Are there structurally equivalent coordination mechanisms that achieve the same capital-market confidence outcome without fiscal conditions (e.g., automatic stabilizers, currency boards, debt-to-GDP ceilings in constitutional law)?',
    'Case studies of successful alternatives (Estonia currency board, Chile constitutional debt limits, Euro Stability and Growth Pact). Comparison of capital-market spreads and default rates across coordination mechanisms.',
    'If alternatives exist and work: conditionalities are one choice among many, not a necessary mechanism. Reduces justification for creditor coordination reading — suggests political choice rather than structural necessity. If no alternatives work: reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether alternative coordination mechanisms exist').

omega_variable(
    creditor_extraction_concealment,
    'Does the creditor coordination reading minimize or conceal extraction that occurs at the margin: conditionality-driven privatization benefiting specific creditor firms, loan-for-reform swaps that funnel debt payments to creditor institutions, or conditionality-enabled capital account liberalization that extracts financial rent?',
    'Structural analysis of IMF program details: frequency of privatization clauses, beneficiary identification in privatizations, debt-to-GDP impact vs creditor profit margins, composition of capital flows post-liberalization. Comparison across IMF programs.',
    'If extraction detected: reading is incomplete — describes coordination while obscuring embedded extraction channels. Moves classification toward tangled_rope or snare. If extraction minimal: reading classification sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creditor_extraction_concealment, conceptual, 'Whether creditor coordination reading obscures institutional extraction').

omega_variable(
    reading_kernel_ambiguity,
    'The kernel ''structural adjustment conditionalities'' is itself contested. This reading instantiates ONE interpretation: coordination mechanism for capital-market confidence. What distinguishes this reading from the debtor_extraction_reading and hybrid_selectivity_reading at the kernel level?',
    'Explicit axiom declaration in cs_structure (foundational: capital_market_confidence_mechanism, capital_flight_prevention_necessity). Sibling readings hold different axioms about whether creditor extraction or selective institutional benefit is the dominant mechanism.',
    'This omega documents the committer frame itself — the framework cannot determine ''which reading is true'' by applying the Deferential Realism apparatus alone. The readings are structurally different constraints generated from the same institutional arrangement, each with its own ε, its own beneficiary/victim structure. The engine produces all three; policy analysis selects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Kernel-level ambiguity: what distinguishes creditor coordination reading from alternatives').

omega_variable(
    beneficiary_diffuseness,
    'Are the declared beneficiaries (future_taxpayers_creditor_nations, international_capital_markets, fiscal_sustainability_stakeholders) genuinely benefiting, or does the diffuseness of these groups obscure that concentration of benefit occurs at the lender or elite-creditor-nation level?',
    'Distribution analysis: who actually benefits from avoided defaults (creditor governments, creditor-nation institutions, or dispersed taxpayers)? Measurement of who bears costs of adjustment (domestic welfare beneficiaries) vs who captures benefits of stability (capital holders, institutions, elite nations).',
    'If diffuse: reading sustains — broad coordination benefit justifies the mechanism. If concentrated: reading should be revised toward hybrid reading or extracted reading — lender benefit is primary, dispersed benefit is secondary or rhetorical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_diffuseness, empirical, 'Whether beneficiaries are genuinely diffuse or benefit is concentrated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_coord_theater_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sac_coord_theater_t5, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(sac_coord_theater_t10, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(sac_coord_extract_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sac_coord_extract_t5, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(sac_coord_extract_t10, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, capital_flight_risk_dynamics).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_moral_hazard_incentive).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_portfolio_concentration).

% DUAL FORMULATION NOTE:
% The structural adjustment conditionalities kernel admits three distinct readings (creditor_coordination_reading, debtor_extraction_reading, hybrid_selectivity_reading), each with its own constraint file and ε value. The readings are not alternative measurements of a single constraint; they are structural disagreements about the causal mechanism and benefit distribution. The creditor coordination reading (this file) emphasizes the coordination problem solved and diffuse beneficiaries (future taxpayers, capital markets). The debtor extraction reading emphasizes institutional profit and forced structural change benefiting specific creditor interests. The hybrid reading combines both. Network edges apply from each reading to shared downstream constraints (capital flight dynamics, moral hazard, portfolio risk), but each reading instantiates different ε values for those downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
