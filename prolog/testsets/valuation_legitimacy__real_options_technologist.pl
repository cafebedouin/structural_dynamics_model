% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__real_options_technologist, []).

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
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: Real Options Valuation Framework — Technological Option Space Legitimacy
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   SpaceX holds a portfolio of real options across segments with radically
 *   different probability and timescale profiles: Starlink (proven, $7.2B
 *   EBITDA), Starship (enabling platform, high-variance probability ~30–50%
 *   for full reusability), orbital compute (addressing 62 GW U.S. power gap,
 *   unproven market but clear demand signal), lunar economy (speculative,
 *   first-mover advantage strong), Mars (civilizational hedge, 50+ year
 *   horizon). Musk and investor coalitions argue the company's $1.75T
 *   valuation is justified by real-options theory: the present value of the
 *   portfolio of optionality, where vertical integration creates compounding
 *   probability increases (success in one segment raises probability of
 *   success in others). The constraint is: valuation legitimacy derives from
 *   technological option space, and vertical integration makes optionality
 *   compoundable. This is one of four contested readings of the kernel
 *   'valuation_legitimacy'; the other readings (DCF-fundamentalist,
 *   governance-skeptic, musk-cult-believer) are separate constraint stories
 *   linked via network.affects_constraints. The real-options reading claims a
 *   mathematical foundation (option-pricing theory from Merton/Black-Scholes
 *   adapted to corporate strategy) and operates as rope in the stakeholder
 *   model: genuine coordination problem (how to justify long-horizon capital
 *   deployment), beneficiary set includes early-stage technology investors
 *   (who gain pricing legitimacy) and humanity-as-civilization (if Mars
 *   options are real). Extraction is present but moderate: governance
 *   concentration (Musk's 82.4% voting control with 42% equity) and employee
 *   identity lock (below-market wages justified by mission, deferred upside
 *   tied to option realization). Suppression is low: investors and
 *   debt-holders have exit options; employees face identity-lock suppression
 *   but measurement registers structural barriers as low because belief in
 *   the mission narrative is the dominant suppression mechanism, not external
 *   coercion.
 *
 * KEY AGENTS:
 *   - Musk/SpaceX management: agenda-setter, controls narrative and capital allocation via voting control; time horizon is civilizational (Mars), exit options are arbitrage (can liquidate at current $1.75T+ valuation)
 *   - Technology investors (VC, growth equity, late-stage private equity): beneficiaries of narrative legitimacy, mobile exit options (sell equity stakes at market prices), biographical time horizon
 *   - Institutional shareholders (index funds, mutual funds, strategic holders): constrained payers/beneficiaries, governance-locked, can exit via selling but lack voting authority to change strategy or enforce minority protection
 *   - Debt investors: payers with fixed claims, downside-protected but upside-capped, exit via secondary debt markets
 *   - SpaceX employees: payers with identity-lock suppression, deferred upside tied to option realization, analytical time horizon is biographical but narrative frame extends to civilizational
 *   - DCF analysts and governance advocates: excluded voices arguing for alternative valuation and governance frameworks; claim the real-options narrative is unfalsifiable and legitimizes control concentration
 *   - Competing space programs: excluded institutional actors with different capital-allocation frameworks (geopolitical mandate vs. profit maximize), face option-value first-mover-advantage competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.31).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.12).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.31).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Valuation Framework — Technological Option Space Legitimacy").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, '86a4ee5c-be73-44ae-b39d-53d9f85465f8').
narrative_ontology:cs_kernel_codification('86a4ee5c-be73-44ae-b39d-53d9f85465f8', fixed_text).
narrative_ontology:cs_authority_grounding('86a4ee5c-be73-44ae-b39d-53d9f85465f8', extraction).
narrative_ontology:cs_interpretation_layer_present('86a4ee5c-be73-44ae-b39d-53d9f85465f8').
narrative_ontology:cs_reading_relation('86a4ee5c-be73-44ae-b39d-53d9f85465f8', constraint_valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('86a4ee5c-be73-44ae-b39d-53d9f85465f8', constraint_valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_reading_relation('86a4ee5c-be73-44ae-b39d-53d9f85465f8', constraint_valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_axiom('86a4ee5c-be73-44ae-b39d-53d9f85465f8', foundational, real_option_values_are_mathematically_defensible).
narrative_ontology:cs_axiom_status(real_option_values_are_mathematically_defensible, holdable).
narrative_ontology:cs_axiom_grounding('86a4ee5c-be73-44ae-b39d-53d9f85465f8', real_option_values_are_mathematically_defensible, empirically_contingent).
narrative_ontology:cs_axiom('86a4ee5c-be73-44ae-b39d-53d9f85465f8', foundational, vertical_integration_creates_compounding_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_creates_compounding_optionality, holdable).
narrative_ontology:cs_axiom_grounding('86a4ee5c-be73-44ae-b39d-53d9f85465f8', vertical_integration_creates_compounding_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('86a4ee5c-be73-44ae-b39d-53d9f85465f8', option_pricing_mathematical_legitimacy).
narrative_ontology:cs_drift_state('86a4ee5c-be73-44ae-b39d-53d9f85465f8', contemporary_ai_and_compute_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86a4ee5c-be73-44ae-b39d-53d9f85465f8', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, technology_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).
:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as rope (genuine coordination function around long-horizon capital deployment) with low extraction (0.31) and low suppression (0.12). The authored metrics reflect the real-options reading's own frame: from the technologist perspective, the option-space thesis solves a real coordination problem (justifying near-term capital deployment on decades-long bets), beneficiaries are clear (investors, humanity), and extraction is limited to the governance concentration and employee deference necessary to execute long-term strategy without quarterly-earnings pressure. Theater is low (0.18): the narrative does work (it legitimizes capital deployment, shapes investor pricing, guides employee recruitment) but is not yet predominantly performative — the constraint performs its stated function (coordinate capital) more than it performs legitimacy for its own sake. Suppression is low because external coercion is minimal: investors and creditors have exit options; employees face identity-lock suppression measured as structural (below-market wages), but the suppression mechanism is primarily internalized (belief in mission) rather than external force. The measurement series trace modest growth: extractiveness increases from 0.18 to 0.31 as the narrative becomes institutional (moving from early-stage VC adoption to institutional venture, index-fund inclusion, debt-market pricing); theater and suppression rise in parallel as the constraint becomes harder to question without appearing anti-innovation. The time grid is shared: every metric is authored at every time point (0, 5, 10, 15, 20, 25), and observed/projected status is marked separately to flag where data ends and forecast begins.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap is between agenda-setter and payer seats. From Musk's institutional position with full control and arbitrage exit, the real-options framework is genuine coordination — it solves the coordination problem of long-horizon capital allocation in technology. From institutional-shareholder and employee positions with constrained governance and identity lock, the same constraint operates as enforced transfer of risk downward while optionality and control stay concentrated at the top. The beneficiary set is narrow and concentrated (Musk, early-stage VC, humanity-as-abstract-good) while the payer set is distributed and heterogeneous (institutional shareholders constrained by governance, employees locked by identity, creditors capped by fixed claims). This asymmetry is structural and will be visible in the engine's per-seat type classifications — no two seats will agree on the type because no two seats have equal structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Musk and SpaceX management are beneficiaries (d near 0.0 or negative — they control narrative, can exit at any time, have upside optionality from valuation realization); early-stage investors and technology VC are beneficiaries (d near 0.15–0.25 — mobile exit, understanding risk/reward, benefit from pricing legitimacy); institutional shareholders are near-symmetric or slight-payer (d near 0.45–0.55 — constrained by governance but participate in upside, carry downside risk if option thesis fails); debt-holders are payers (d near 0.65 — fixed claims, downside risk uncompensated); employees are payers (d near 0.7–0.75 — identity locked despite mobility, deferred upside dependent on realization, below-market wages are real present cost). No overrides are needed: the structural derivation from beneficiary/victim declarations and exit options produces accurate directionality. The seat divergence is large: from Musk's institutional seat the constraint is genuine coordination (option-space justifies long-horizon capital); from institutional-shareholder and debt-holder seats it is enforced transfer of risk downward while optionality is hoarded at the control level; from employee seat it is identity-based extraction masked by mission narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to justify capital deployment on decades-long technological bets without quarterly-earnings pressure) is live and contested. The founding_problem_status is correctly marked 'contested' because technology venture advocates attest the problem is live (SpaceX's multi-decade capital needs are real) while DCF analysts attest the problem is a rationalization for control concentration (any company can justify optionality investments through governance that protects long-term strategy; the real-options framing is unnecessary unless the goal is to avoid governance constraints). The constraint does not show mandatrophy (the function has not outlived its purpose) but it does show tension between the coordination reading (option-space theory legitimizes capital allocation) and the extraction reading (option-space theory legitimizes control concentration without governance oversight). The measurement series show theater rising modestly (0.08 to 0.18) as the constraint becomes more institutionalized and harder to question, but theater remains low because the narrative still performs its stated coordination function (capital is deployed, technological progress occurs). If theater were to reach 0.5+, mandatrophy would be indicated (the constraint would persist mostly through institutional inertia and narrative performance, not through real coordination function). Currently, the constraint shows no mandatrophy but shows rising risk of it: as Starlink becomes commodity-like and Starship's costs rise, the option-space justification for capital concentration may become harder to defend, and theater would rise as the narrative is maintained despite functional decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_probability_assumption_verification,
    'Is the 6% probability assumption for achieving $28.5T TAM across the full portfolio empirically defensible, or does the real-options framework embed speculative probabilities dressed as quantitative rigor?',
    'Explicit probabilistic decomposition by domain: survival probability for Starlink orbital compute (power-grid segment), Starship reusability economics, lunar economy market timing, Mars settlement timeline. Compare to historical base rates for analogous technological transitions (e.g., supersonic flight viability, nuclear fusion timelines, offshore wind adoption curves). Independent Monte Carlo sensitivity analysis on key binomial branches.',
    'If probabilities are well-calibrated to historical base rates, the option-value thesis is sound and the constraint legitimizes multi-decade capital commitment. If probabilities are optimistic overestimates (tail-risk inflation), the constraint operates as extractive narrative masking control concentration — the valuation inflates and governance critique becomes justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_probability_assumption_verification, empirical, 'Whether probability assumptions in the option-value portfolio are empirically grounded or speculative.').

omega_variable(
    vertical_integration_optionality_compounding,
    'Does vertical integration actually create compounding optionality (synergistic probability increases across segments), or is the compounding effect asserted but operationally absent?',
    'Detailed causal analysis of interdependencies: (1) Does Starship success materially increase orbital-compute viability? (2) Does Starlink''s existing infrastructure accelerate lunar operations? (3) Do supply-chain or personnel efficiencies from one segment reduce capital requirements in others? (4) Are there documented instances where success in one segment enabled faster or cheaper progress in another, or is the compounding narrative forward-looking only?',
    'If compounding is real and quantifiable, the option-value thesis gains empirical support and the constraint is legitimate coordination. If compounding is speculative or overstated, the constraint operates as a framing device that justifies lower profitability and higher leverage in stable segments (Starlink) by attributing upside optionality they do not actually produce — governance critique becomes valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vertical_integration_optionality_compounding, empirical, 'Whether vertical integration creates demonstrable synergistic probability compounding or is an unsubstantiated narrative claim.').

omega_variable(
    reading_contingency_on_probability_interpretation,
    'Does this reading''s core premise (real options legitimize valuation) depend on a specific interpretation of what ''probability'' means in the context of one-off or low-frequency events (first Mars settlement, lunar economy emergence)?',
    'Philosophical/epistemological analysis: frequentist vs. Bayesian interpretation of the probability assumptions. Can probabilities for unique, high-variance, long-horizon events be meaningfully assigned using the same mathematical machinery as repeated-trial insurance or pharmaceutical drug approval? If not, does the framework become category error rather than sound valuation?',
    'If the framework depends on a contested interpretation of probability for unique events, then the constraint is held together partly by an unexamined epistemological commitment — resolving the interpretation question could either validate the constraint or expose it as metaphorical rather than mathematical. Governance critique gains force if the mathematical veneer masks fundamental uncertainty that cannot be quantified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_on_probability_interpretation, conceptual, 'Whether the real-options framework applies sound probability methodology to one-off, unique, or low-frequency events, or relies on an unjustified application of frequentist statistics to inherently uncertain futures.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the contested kernel ''valuation_legitimacy''. The kernel is read differently by DCF fundamentalists (valuation = discounted proven cash flows), governance skeptics (valuation = minority-protected governance structures), and Musk-cult believers (valuation = track record of achieving impossible goals). How does this real-options reading differ structurally from its siblings, and what would falsify this reading relative to others?',
    'Structured comparison of the four readings'' core axioms and evidence thresholds: (1) Real-options reading asserts option-value is real, compoundable, and mathematically defensible; fails if probability assumptions prove wildly off-calibrated or compounding effects are null. (2) DCF reading asserts unproven cash flows should not enter valuation; fails if option-theory methods are adopted industry-wide and repricing occurs. (3) Governance-skeptic reading asserts Musk''s control concentration is extractive regardless of technological outcome; fails only if governance structure changes AND valuation premium persists (suggesting governance was not the extraction mechanism). (4) Cult-believer reading asserts Musk''s track record is predictive; fails if major technological bets fail or if leadership transitions and company valuation inverts.',
    'This reading coexists with DCF (alternative valuation frame, neither forecloses the other — they simply compete for acceptance), influences governance-skeptic (option-value justification for control reduces governance advocates'' leverage but does not rule out their framework), and coexists with cult-believer (both pro-Musk narratives, but grounded in different authority: mathematics vs. biography).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'The real-options reading as one interpretation of a contested kernel about what makes valuation legitimate.').

omega_variable(
    suppression_mechanism_identity_lock_coupling,
    'Employee identity lock (career fused with Musk/mission narrative) is measured structurally but may involve both external suppression (non-transferable equity, below-market wages creating exit cost) and internalized suppression (belief in the mission narrative, cosmological significance of space civilization). Are these mechanisms distinct, or does the employee bear the low suppression measure because the identity lock itself prevents them from perceiving the financial extraction?',
    'Post-exit longitudinal study of employees who leave SpaceX: do they report actual wage recovery at comparable firms (indicating suppression was structural cost, now relieved) or continued below-market self-selection into space/Musk-adjacent roles (indicating identity lock persists after exit, suppression was internalized)? Do exit narratives cite financial regret or reflect continued mission alignment?',
    'If suppression is internalized, the measured base_suppression (0.12) understates the constraint''s hold on the employee population — the ''low suppression'' reflects identity fusion that persists independently of structural barriers. Governance critique gains force: concentration of control paired with identity-locked workforce is a more complete extraction mechanism than structural wage suppression alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock_coupling, empirical, 'Whether employee suppression is structural (exit-cost-driven) or internalized (identity-fusion-driven), and whether the distinction affects classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(valu_tr_t5, valuation_legitimacy__real_options_technologist, theater_ratio, 5, 0.11).
narrative_ontology:measurement(valu_tr_t10, valuation_legitimacy__real_options_technologist, theater_ratio, 10, 0.14).
narrative_ontology:measurement(valu_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.16).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.17).
narrative_ontology:measurement(valu_tr_t25, valuation_legitimacy__real_options_technologist, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(valu_be_t5, valuation_legitimacy__real_options_technologist, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(valu_be_t10, valuation_legitimacy__real_options_technologist, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(valu_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(valu_be_t25, valuation_legitimacy__real_options_technologist, base_extractiveness, 25, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__real_options_technologist, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(valu_su_t5, valuation_legitimacy__real_options_technologist, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(valu_su_t10, valuation_legitimacy__real_options_technologist, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(valu_su_t15, valuation_legitimacy__real_options_technologist, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__real_options_technologist, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(valu_su_t25, valuation_legitimacy__real_options_technologist, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, constraint_valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, constraint_valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, constraint_valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% The kernel 'valuation_legitimacy' is decomposed into four structurally distinct constraint stories, each with its own ε-invariant reading of what makes valuation legitimate in the SpaceX context. The real-options-technologist reading (THIS story) coexists with DCF-fundamentalist and musk-cult-believer (alternative frameworks competing for institutional acceptance) and influences governance-skeptic (by providing mathematical cover for control concentration). All four stories are linked via network.affects_constraints; no single story subsumes the others. The engine's contamination analysis will trace how failure or validation of one reading propagates to its network neighbors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
