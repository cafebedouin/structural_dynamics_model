% ============================================================================
% CONSTRAINT STORY: insurance_loss_recognition_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insurance_loss_recognition_timing, []).

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
 *   constraint_id: insurance_loss_recognition_timing
 *   human_readable: Insurance Loss Recognition Timing Constraint
 *   domain: financial_accounting/insurance_regulation
 *
 * SUMMARY:
 *   Insurance loss recognition timing creates a structural tension between
 *   the carrier's need for predictable reserve adequacy and the claimant's
 *   need for timely payment. Accounting rules (SAP, IFRS 17) establish
 *   mandatory delays between loss occurrence, claim notification, and final
 *   recognition on financial statements — delays that create a window during
 *   which the carrier holds reserves representing claimant money but can
 *   invest or deploy those funds. This constraint exhibits hybrid
 *   coordination-extraction characteristics: the timing rules do coordinate
 *   financial stability and prevent reserve depletion charades, but they also
 *   enable carriers to extract value via float and earnings smoothing. The
 *   theater ratio (0.58) reflects that loss recognition appears to be a
 *   precise actuarial process but masks a timing arbitrage opportunity. The
 *   extractiveness trajectory (0.38→0.52 over interval) indicates that as
 *   carriers have grown more sophisticated in reserve management and digital
 *   systems have enabled faster processing, the remaining delay has
 *   increasingly taken on extractive character rather than coordination
 *   necessity.
 *
 * KEY AGENTS:
 *   - Claimant: Primary victim (powerless/trapped) — must wait for carrier's recognition schedule; no alternatives; bears full opportunity cost of delayed payment
 *   - Claims Adjuster: Processing agent (moderate/constrained) — implements timing rules; faces pressure from both claimant and carrier; has some discretion in judgment calls but constrained by regulatory requirements
 *   - Insurance Carrier: Primary beneficiary (institutional/arbitrage) — captures float advantage; uses timing rules for earnings management; can arbitrage across jurisdictions
 *   - Regulatory Authority (Insurance Commission): Enforcer (organized/constrained) — sets timing rules ostensibly for reserve adequacy; constrained by actuarial peer pressure and industry lobbying
 *   - Advocacy Reformers: Organized challenger (organized/constrained) — push for faster settlement; advocate for digital claims automation; see timing rules as outdated
 *   - Accounting Standards Board: Rule-keeper (institutional/arbitrage) — maintains timing rules for consistency and auditability; benefits from industry compliance; arbitrage exit via differential standard application
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees hybrid nature: genuine coordination value + systematic extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insurance_loss_recognition_timing, 0.52).
domain_priors:suppression_score(insurance_loss_recognition_timing, 0.48).
domain_priors:theater_ratio(insurance_loss_recognition_timing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insurance_loss_recognition_timing, extractiveness, 0.52).
narrative_ontology:constraint_metric(insurance_loss_recognition_timing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(insurance_loss_recognition_timing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insurance_loss_recognition_timing, tangled_rope).
narrative_ontology:human_readable(insurance_loss_recognition_timing, "Insurance Loss Recognition Timing Constraint").
narrative_ontology:topic_domain(insurance_loss_recognition_timing, "financial_accounting/insurance_regulation").

domain_priors:requires_active_enforcement(insurance_loss_recognition_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insurance_loss_recognition_timing, insurance_carriers).
narrative_ontology:constraint_beneficiary(insurance_loss_recognition_timing, equity_holders).
narrative_ontology:constraint_victim(insurance_loss_recognition_timing, policyholders).
narrative_ontology:constraint_victim(insurance_loss_recognition_timing, claims_settlements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLAIMANT (SNARE) — Trapped by the timing rules. Cannot exit the insurance system; must wait for carrier's loss recognition schedule regardless of actual financial need. Bears full cost of delayed payment while insurer enjoys float. Maximum experienced extraction — no alternatives, no negotiating power, no exit.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLAIMS ADJUSTER (TANGLED ROPE) — Constrained by regulatory timing rules and reserve adequacy requirements, but also benefits from the coordination function: clear timing rules enable predictable claims workflow and prevent chaotic overpayment. Bears extraction pressure (forced to delay legitimate claims) but also experiences genuine coordination value (structured process). Significant agency gap — caught between claimant pressure and carrier incentives.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURANCE CARRIER (ROPE) — Experiences the timing constraint as pure coordination: predictable loss recognition enables financial planning, reserve setting, and premium calculation. Net beneficiary — gains float advantage and earnings smoothing. Arbitrage exit options enable the carrier to arbitrage regulatory timing differences across jurisdictions.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORMERS (SCAFFOLD) — Organized agents (consumer advocates, insurance commissioners, actuarial standards boards) view timing rules as a temporary coordination problem with a sunset: faster settlement protocols, real-time loss tracking, and digital claims automation are building alternative pathways that reduce timing asymmetry. Sees the constraint as degrading as technology enables immediate recognition. Organized enough to push reform; constrained by industry resistance.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOUNTING STANDARD-SETTING BODY (PITON) — The constraint persists through institutional inertia. IFRS 17 and SAP loss recognition rules are maintained largely for continuity and audit convenience, not because they optimize settlement timing. The theater is high — the rules appear precise and actuarially rigorous, but empirically they create arbitrary delays that have no bearing on actual loss magnitude. Theater ratio reflects the ritual of actuarial review that masks extraction mechanism.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the timing constraint contains both genuine coordination (predictability of reserve requirements) and asymmetric extraction (float advantage captured by carriers). The constraint is neither purely natural law nor purely extractive — it is a hybrid that coordinates financial stability while enabling rent extraction. This is the classified baseline from which other perspectives diverge.
constraint_indexing:constraint_classification(insurance_loss_recognition_timing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insurance_loss_recognition_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insurance_loss_recognition_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insurance_loss_recognition_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insurance_loss_recognition_timing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insurance_loss_recognition_timing, TR),
    TR >= 0.70.

:- end_tests(insurance_loss_recognition_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The carrier captures genuine float value during the recognition lag, with documented interest income from delayed settlement. However, not all delay is extractive — some reserve-setting delay is actuarially justified for loss volatility and inflation adjustment. The 0.52 value reflects that roughly half of measured delay serves coordination (reserve adequacy) and half serves extraction (float capture). The trajectory rising from 0.38 suggests that as systems have matured, the 'necessary' delay component has shrunk while the extractive component persists. Suppression (0.48): Moderate. Claimants face genuine barriers to exit — they are contractually bound to the insurance system and have limited visibility into carrier timing decisions. Suppression is not total because some claimants can force early settlement through legal action, and some carriers offer accelerated claims programs. Theater ratio (0.58): Moderate-high. Loss recognition appears as a rigorous actuarial process with formal reserve adequacy testing, but this theater masks timing discretion — carriers can accelerate or delay recognition within regulatory bounds to smooth earnings. The theater has increased over time as financial reporting has become more complex and actuarial modeling more opaque.
 *
 * PERSPECTIVAL GAP:
 *   The claimant perceives a snare (pure extraction, no escape, maximum cost). The carrier perceives rope (pure coordination, alignment of incentives, mutual benefit). The adjuster perceives tangled rope (mixed: some genuine process value but also pressure to delay). The reformers perceive scaffold (temporary problem being solved by technology and regulation). The accounting board perceives piton (ritual maintained for continuity despite degraded function). The analytical observer perceives tangled rope (hybrid system with both coordination and extraction). The perspectival gap is maximal between claimant and carrier: the same timing rules are experienced as pure extraction from below and as pure coordination from above. This gap is the diagnostic signal of the constraint's hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim structure plus exit options. The claimant (victim/trapped) experiences maximum directionality (d≈0.95) — all flow runs away from them. The carrier (beneficiary/arbitrage) experiences low directionality (d≈0.10) — they have multiple escape routes and benefit from the rule. The adjuster (moderate/constrained) experiences mid-range directionality (d≈0.55) — they are squeezed between pressures but retain some discretionary power. The reformers (organized/constrained) experience moderate directionality (d≈0.50) — they have collective action capacity but face entrenched industry resistance. The accounting board (institutional/arbitrage) experiences low directionality (d≈0.15) — they have exit options and benefit from industry compliance. The analytical observer (analytical/analytical) computes aggregate directionality across the system (d≈0.72) — the structure concentrates extraction away from powerless agents toward institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through its tangled_rope classification in the analytical perspective. The coordination component (reserve adequacy, financial stability) is genuine — eliminating all timing delays would create reserve volatility problems. The extraction component (float capture, earnings smoothing) is equally genuine — carriers systematically benefit from delay. Neither can be labeled as 'really just' the other. The classification resolves the mandatrophy by refusing to reduce the constraint to a single function. The false mountain interpretation ('timing delay is inherent to actuarial science') is exposed by the reformer scaffold perspective — digital systems are enabling faster recognition without compromising reserve adequacy, proving the delay is institutional rather than physical law. The false pure-rope interpretation ('timing is just coordination') is exposed by the claimant snare perspective — claimants bear concentrated extraction cost while carriers capture concentrated benefit, not mutual coordination. The tangled rope classification holds both truths: the timing rules coordinate financial stability AND enable systematic extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    float_value_empirical_measurement,
    'What is the actual monetary value of float advantage to carriers from delayed recognition?',
    'Historical analysis of carrier interest income during claims lag period; comparison of carriers using expedited settlement vs standard timing',
    'If float value > 5% of claim amount: extraction interpretation strengthened. If < 1%: coordination interpretation strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(float_value_empirical_measurement, empirical, 'Quantification of float extraction value').

omega_variable(
    legitimate_reserve_adequacy_requirement,
    'What portion of delay is required for actuarially sound reserve calculation vs what portion is extractive overhang?',
    'Comparison of loss adequacy errors under different recognition timelines; analysis of reserve sufficiency with real-time vs delayed recognition',
    'If most delay is reserve-required: tangled_rope classification confirmed. If most delay is discretionary: snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_reserve_adequacy_requirement, empirical, 'Decomposition of legitimate vs extractive delay components').

omega_variable(
    technology_adoption_trajectory,
    'Are digital claims platforms and real-time loss tracking advancing at rates that would create genuine sunset clause?',
    'Tracking of digital platform adoption rates; timeline for when real-time recognition becomes actuarially feasible across carrier base',
    'If adoption is rapid (5-10 years): scaffold classification confirmed. If adoption is stalling: scaffold is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_adoption_trajectory, empirical, 'Whether technology will enable scaffold sunset').

omega_variable(
    cross_jurisdiction_arbitrage_prevalence,
    'How extensively do carriers exploit differences in loss recognition timing across regulatory jurisdictions?',
    'Analysis of reserve patterns for carriers with operations in multiple jurisdictions; documentation of deliberate timing optimization across borders',
    'If arbitrage is systematic: extraction mechanism is deliberate and sophisticated. If sporadic: carriers are passive rule-followers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_jurisdiction_arbitrage_prevalence, empirical, 'Extent of deliberate regulatory arbitrage on timing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insurance_loss_recognition_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insur_loss_tr_t0, insurance_loss_recognition_timing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(insur_loss_tr_t5, insurance_loss_recognition_timing, theater_ratio, 5, 0.52).
narrative_ontology:measurement(insur_loss_tr_t10, insurance_loss_recognition_timing, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(insur_loss_be_t0, insurance_loss_recognition_timing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(insur_loss_be_t5, insurance_loss_recognition_timing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(insur_loss_be_t10, insurance_loss_recognition_timing, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insurance_loss_recognition_timing, resource_allocation).
narrative_ontology:affects_constraint(insurance_loss_recognition_timing, reserve_adequacy_capital_requirements).
narrative_ontology:affects_constraint(insurance_loss_recognition_timing, claims_settlement_delay_accumulation).

% DUAL FORMULATION NOTE:
% Insurance loss recognition timing decomposes into two structurally distinct constraints: (1) reserve_adequacy_capital_requirements (ε≈0.25, coordination-dominant) — the actuarial problem of setting adequate reserves for uncertain future losses, and (2) claims_settlement_delay_accumulation (ε≈0.68, extraction-dominant) — the strategic delay of recognition beyond actuarial requirement to capture float. This story represents the hybrid system; upstream story covers legitimate reserve function; downstream story covers the degradation into pure timing arbitrage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(insurance_loss_recognition_timing, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
