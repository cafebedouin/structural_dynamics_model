% ============================================================================
% CONSTRAINT STORY: ecb_target_mandate_scope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_target_mandate_scope, []).

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
 *   constraint_id: ecb_target_mandate_scope
 *   human_readable: ECB Target Mandate Scope: Price Stability vs Financial Stability Coordination
 *   domain: monetary_policy/institutional_governance
 *
 * SUMMARY:
 *   The European Central Bank operates under the Treaty on the Functioning of
 *   the European Union, which grants it a primary mandate for price
 *   stability. However, in practice, the ECB has progressively absorbed
 *   financial stability functions — banking supervision, macroprudential
 *   policy, emergency liquidity assistance, and asset purchases justified by
 *   financial stability rationales. This scope expansion occurs through both
 *   explicit institutional reform (Single Supervisory Mechanism post-2014)
 *   and implicit mission creep (quantitative easing programs justified as
 *   financial stability rather than pure monetary expansion). The constraint
 *   generates a structural tension: a single technical institution now
 *   coordinates multiple institutional objectives with different stakeholder
 *   preferences, creating both genuine coordination benefits and asymmetric
 *   extraction mechanisms. The TARGET2 settlement system links all eurozone
 *   central banks into a single liquidity grid; the ECB's expanded mandate
 *   determines how this grid allocates risk and benefit.
 *
 * KEY AGENTS:
 *   - Financial Stability Authorities (EU Member States): Primary beneficiary (institutional/arbitrage) — offload macroprudential and supervisory functions onto ECB technocracy
 *   - TARGET Operating System: Primary victim (powerless/trapped) — absorbs mandate expansion without structural redesign; carries both coordination and extraction load
 *   - Peripheral Eurozone Central Banks: Secondary victim (moderate/constrained) — constrained by ECB coordination requirements; receive benefits through liquidity access but limited independent response capacity
 *   - Monetary Policy Framework (2% inflation target): Institutional actor (institutional/arbitrage) — original narrow mandate that benefits from reduced political pressure, but subject to mission creep
 *   - International Monetary System Observers: Organized beneficiaries (organized/constrained) — benefit from eurozone stability; constrained by unpredictability of ECB asset purchase programs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination problem (multiple authorities needed centralization) and inherent asymmetry (single actor concentrates power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_target_mandate_scope, 0.52).
domain_priors:suppression_score(ecb_target_mandate_scope, 0.65).
domain_priors:theater_ratio(ecb_target_mandate_scope, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_target_mandate_scope, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_target_mandate_scope, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ecb_target_mandate_scope, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_target_mandate_scope, tangled_rope).
narrative_ontology:human_readable(ecb_target_mandate_scope, "ECB Target Mandate Scope: Price Stability vs Financial Stability Coordination").
narrative_ontology:topic_domain(ecb_target_mandate_scope, "monetary_policy/institutional_governance").

domain_priors:requires_active_enforcement(ecb_target_mandate_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_target_mandate_scope, financial_system_stability).
narrative_ontology:constraint_beneficiary(ecb_target_mandate_scope, eurozone_governments).
narrative_ontology:constraint_victim(ecb_target_mandate_scope, monetary_policy_independence).
narrative_ontology:constraint_victim(ecb_target_mandate_scope, inflation_targeting_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET OPERATING SYSTEM (SNARE) — Cannot exit the mandate expansion. The technical framework designed for narrowly scoped price stability now absorbs financial stability obligations with no mechanism for shedding them. Maximum experienced extraction — the system was born from one mandate but now serves two masters with conflicting signals.
constraint_indexing:constraint_classification(ecb_target_mandate_scope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PERIPHERAL EUROZONE CENTRAL BANKS (TANGLED ROPE) — Constrained by ECB coordination requirements and TARGET2 settlement mechanics, but also benefit from access to ECB liquidity facilities and emergency backstops. Experience both genuine coordination (stabilizing cross-border settlements) and asymmetric extraction (constraining independent monetary response to local conditions).
constraint_indexing:constraint_classification(ecb_target_mandate_scope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EUROZONE FINANCIAL STABILITY AUTHORITIES (ROPE) — Experience the mandate expansion as pure coordination gain. The ECB's assumption of financial stability oversight reduces coordination friction between monetary and macroprudential policy. Net beneficiary with high arbitrage options — can reallocate regulatory burden to ECB and adjust own toolkit accordingly.
constraint_indexing:constraint_classification(ecb_target_mandate_scope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL MONETARY COORDINATION FORUMS (SCAFFOLD) — See the expanded mandate as a temporary coordination device during systemic crises (2008 financial crisis, 2020 pandemic) with implicit sunset. As international capital markets stabilize and eurozone governance harmonizes, the emergency financial stability functions can sunset back to national authorities. Theater remains moderate — genuine crisis coordination functions visible within performative normality rhetoric.
constraint_indexing:constraint_classification(ecb_target_mandate_scope, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PRICE STABILITY TARGETING FRAMEWORK (PITON) — The original 2% inflation target and quantitative mandate for price stability has become largely symbolic performance. Real ECB activity increasingly focuses on financial stability, asset purchases, and macro-prudential concerns. The price stability rhetoric persists through institutional inertia despite the functional scope having shifted. Theater ratio driven by gap between official mandate language and actual policy focus.
constraint_indexing:constraint_classification(ecb_target_mandate_scope, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the ECB's scope expansion solves a genuine coordination problem (multiple regulatory authorities with misaligned incentives require a single actor to internalize systemic effects) while creating asymmetric extraction (a single technocratic institution concentrates policy power without complete democratic accountability mechanisms). This is the core structural tension — legitimate coordination plus inherent asymmetry.
constraint_indexing:constraint_classification(ecb_target_mandate_scope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_target_mandate_scope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecb_target_mandate_scope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecb_target_mandate_scope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_target_mandate_scope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecb_target_mandate_scope, TR),
    TR >= 0.70.

:- end_tests(ecb_target_mandate_scope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ECB's mandate scope expansion genuinely solves a coordination problem — separate monetary and macroprudential authorities with misaligned incentives create regulatory arbitrage, procyclical feedback, and delayed crisis response. The expansion to single-actor coordination is efficient. However, this efficiency comes with extraction: the TARGET technical framework was designed for narrow price stability functions and now carries dual mandates with conflicting signals. The institution experiences extractiveness through having to serve two masters. The measurement trajectory (0.28 → 0.52) shows degradation as the financial crisis required progressively more emergency functions to be absorbed into the ECB's scope. Suppression (0.65): High. Peripheral eurozone central banks face substantial barriers to exit — eurozone membership is constitutional commitment; TARGET2 participation is mandatory; ECB coordination requirements are binding. But suppression is not total: some fiscal instruments remain at Member State level, some banking regulation remains national. Theater ratio (0.58): Moderate. The price stability mandate language persists, but ECB communications increasingly justify actions by financial stability and asset purchase rationales. The original 2% inflation target framework performs symbolic function while real policy focuses on macro-prudential stabilization. This is not maximum theater (piton level) because genuine stabilization functions are visible within the performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the full tension between coordination and extraction. Financial stability authorities see rope — genuine problem solved. The TARGET system sees snare — absorbs expanding obligations with no exit. Peripheral banks see tangled rope — benefits from liquidity access + constrained by coordination requirements. The analytical observer sees the core structural reality: this is coordination (legitimate) plus asymmetry (inherent) — tangled rope at systemic level. The piton classification captures real phenomenon (price stability rhetoric divorcing from actual policy focus) but misses that the underlying financial stability functions are genuine, not performative. The false summit (mountain) would assume mandate scope is a law of monetary economics; the actual constraint is a contingent institutional arrangement solving a real problem through technocratic concentration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the mandate expansion mechanism. Financial stability authorities (beneficiaries) have high exit optionality — they can in principle reallocate functions back to national level if ECB integration fails. Their d is low (~0.25), producing negative or minimal chi. The TARGET system (victim) has no exit option — it is the infrastructure layer; trapped exit produces high d (~0.95), high chi. Peripheral central banks face constrained exit (eurozone membership is binding but some policy space remains) — d ~0.65, moderate chi. The analytical observer's position includes both beneficiary insights (genuine coordination) and victim insights (asymmetric concentration), producing moderate d (~0.50) and moderate chi. The piton perspective's d is derived from institutional power + arbitrage, producing low d despite theatrical degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is the scope expansion coordination or extraction?' The resolution is that it is BOTH, and these cannot be separated without losing the coordination function. The ECB cannot coordinate financial stability without concentrating power; it cannot concentrate power without creating asymmetric extraction (the single institution's policy choices affect peripheral economies asymmetrically). The mandatrophy resolves by recognizing this as the fundamental tension in eurozone institutional design: monetary union requires either (a) separate coordination authorities with potential misalignment (solved by ECB scope expansion = extraction route), or (b) full fiscal integration removing need for financial stability coordination through monetary policy (politically infeasible in current eurozone). The tangled rope classification is stable because both the rope and snare components are structurally real, not artifacts of measurement perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_scope_stability_tradeoff,
    'Does expanded mandate scope genuinely reduce systemic financial instability or does it merely displace risk onto the balance sheet of the single coordinator?',
    'Comparative stability metrics: measure systemic volatility 15 years pre-expansion vs post-expansion; decompose ECB asset holdings by stability contribution vs risk concentration',
    'If risk genuinely reduced: coordination benefit outweighs extraction, classify as Rope for institutional beneficiaries. If risk displaced: extraction component rises, confirm Tangled Rope from analytical view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_scope_stability_tradeoff, empirical, 'Whether mandate expansion reduces or displaces financial stability risk').

omega_variable(
    political_economy_constraint_vs_freedom,
    'Is the ECB''s independence from political pressure a structural constraint on its mandate, or does it enable the institution to absorb politically contentious financial stability functions?',
    'Historical analysis of intervention decisions during political crises (Greek crisis, Italian government bond spreads, Hungarian central bank independence); tracking of Member State pressure vs ECB independence claims',
    'If independence is genuine structural constraint: ECB cannot absorb additional political mandates without losing independence (Snare from political perspective). If independence enables political absorption: mandate scope is a political equilibrium, not a technical necessity (Tangled Rope stable classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_constraint_vs_freedom, conceptual, 'Whether ECB independence constrains or enables mandate scope expansion').

omega_variable(
    eurozone_fragmentation_mechanism,
    'Does TARGET2 settlement mechanics and financial stability coordination through ECB create or mitigate economic fragmentation between core and peripheral zones?',
    'Tracking of TARGET2 imbalances and their correlation with credit market fragmentation; measurement of cross-border capital flows and interest rate differentials pre-post mandate expansion',
    'If creates fragmentation: mandate scope becomes extraction mechanism for core zone (higher d → higher chi), Snare classification justified. If mitigates fragmentation: genuine coordination with asymmetric side effects, Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eurozone_fragmentation_mechanism, empirical, 'Whether mandate scope expansion mitigates or creates eurozone fragmentation').

omega_variable(
    inflation_targeting_vs_asset_purchasing,
    'Are the price stability mandate and financial stability mandate compatible objectives within a single institution, or do they generate conflicting policy signals that prevent clear transmission mechanisms?',
    'Decomposition of ECB policy communications: identify statements where price stability objective conflicts with financial stability objective; measure correlation between monetary tightening signals and financial stability countervailing actions',
    'If compatible: theater_ratio should be lower, Rope classification more justified. If conflicting: theater_ratio is accurate, Tangled Rope with high extraction on the technical system confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_targeting_vs_asset_purchasing, empirical, 'Compatibility of price stability and financial stability mandates within single institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_target_mandate_scope, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_target_tr_t0, ecb_target_mandate_scope, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ecb_target_tr_t5, ecb_target_mandate_scope, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ecb_target_tr_t10, ecb_target_mandate_scope, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ecb_target_be_t0, ecb_target_mandate_scope, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ecb_target_be_t5, ecb_target_mandate_scope, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ecb_target_be_t10, ecb_target_mandate_scope, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_target_mandate_scope, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_target_mandate_scope, eurozone_fiscal_coordination).
narrative_ontology:affects_constraint(ecb_target_mandate_scope, banking_union_governance).
narrative_ontology:affects_constraint(ecb_target_mandate_scope, target2_settlement_asymmetry).

% DUAL FORMULATION NOTE:
% The ECB mandate scope constraint is upstream of specific banking regulation and fiscal coordination constraints. The scope expansion creates capacity to coordinate downstream constraints (banking union, TARGET2 risk allocation) but also determines the institutional actor's power and extractiveness in those domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_target_mandate_scope, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
