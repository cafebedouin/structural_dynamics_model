% ============================================================================
% CONSTRAINT STORY: moral_hazard_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_hazard_amplification, []).

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
 *   constraint_id: moral_hazard_amplification
 *   human_readable: Moral Hazard Amplification in Risk-Sharing Systems
 *   domain: economics/institutional_failure
 *
 * SUMMARY:
 *   Moral hazard amplification occurs in risk-sharing systems when guarantees
 *   (explicit or implicit) reduce incentives for loss prevention, causing
 *   risk-takers to engage in progressively riskier behavior. The system
 *   creates a structural bind: guarantees enable beneficial coordination
 *   (access to capital, insurance coverage, systemic stability) while
 *   simultaneously weakening the mechanisms that prevent excessive risk
 *   accumulation. This constraint exemplifies institutional feedback loops
 *   where the solution (guarantee) amplifies the problem (moral hazard) over
 *   time, requiring continuous expansion of suppression mechanisms
 *   (regulation, monitoring, bail-in frameworks) to contain the drift. The
 *   extractiveness trajectory (0.32→0.58 over 15 periods) shows accumulation
 *   of hidden tail risks and regulatory scope expansion. Theater ratio
 *   (0.35→0.55) reflects increasing performative regulatory activity to
 *   maintain confidence in guarantee credibility while actual unpriced risks
 *   grow.
 *
 * KEY AGENTS:
 *   - Risk-Taking Beneficiaries: Institutional actors (institutional/arbitrage) — capture coordination benefits of guarantees while externalizing tail risks onto residual risk bearers
 *   - Residual Risk Bearers: Taxpayers, central bank reserve holders, system stability (powerless/trapped) — absorb unpriced tail risks and systemic failure costs with no exit or renegotiation capacity
 *   - Guarantee-Dependent Institutions: Banks, corporates, insurance firms (moderate/constrained) — need guarantees for access to capital but caught between coordination benefit and moral hazard trap; cannot exit without losing competitive advantage
 *   - Guarantee Administrators: Central banks, deposit insurers, regulatory agencies (institutional/constrained) — genuinely coordinate risk-sharing but also extracted through regulatory expansion, fee structures, and institutional mission creep
 *   - Regulatory Reform Coalition: Macroprudential authorities, Basel Committee, IMF (organized/mobile) — building sunset mechanisms (countercyclical buffers, stress testing, bail-in frameworks) to distribute risk-bearing more symmetrically
 *   - Implicit Guarantee Fiction: Narrative and market-pricing mechanism (institutional/arbitrage) — persists through performative commitment language; markets oscillate between belief and skepticism based on recent crisis history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_hazard_amplification, 0.58).
domain_priors:suppression_score(moral_hazard_amplification, 0.62).
domain_priors:theater_ratio(moral_hazard_amplification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_hazard_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(moral_hazard_amplification, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(moral_hazard_amplification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_hazard_amplification, tangled_rope).
narrative_ontology:human_readable(moral_hazard_amplification, "Moral Hazard Amplification in Risk-Sharing Systems").
narrative_ontology:topic_domain(moral_hazard_amplification, "economics/institutional_failure").

domain_priors:requires_active_enforcement(moral_hazard_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_hazard_amplification, risk_takers).
narrative_ontology:constraint_beneficiary(moral_hazard_amplification, guarantee_administrators).
narrative_ontology:constraint_victim(moral_hazard_amplification, residual_risk_bearers).
narrative_ontology:constraint_victim(moral_hazard_amplification, system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDUAL RISK BEARER (SNARE) — Trapped into bearing tail risks and systemic failure costs. Powerless to exit or renegotiate. When guaranteed risk-takers fail catastrophically, this agent absorbs losses. No coordination benefit; pure extraction. Maximum suppression through legal obligation and institutional architecture.
constraint_indexing:constraint_classification(moral_hazard_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GUARANTEE-DEPENDENT INSTITUTION (TANGLED ROPE) — Faces high barriers to exit (career, funding, institutional identity) but also genuinely benefits from risk-sharing coordination. The guarantee enables it to take risks it otherwise couldn't afford. Extraction exists (elevated moral hazard) but coordination benefit coexists (access to capital/opportunity). Constrained exit due to resource dependency.
constraint_indexing:constraint_classification(moral_hazard_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RISK-TAKING BENEFICIARY (ROPE) — Sees the guarantee as pure coordination mechanism: access to credit/opportunity that enables beneficial activity. Experiences the constraint as solving collective action problem (lending markets would fail without guaranting). Has arbitrage options (can exit to unguaranteed activities or international markets). Net beneficiary with low subjective extraction.
constraint_indexing:constraint_classification(moral_hazard_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GUARANTEE ADMINISTRATOR (TANGLED ROPE) — Faces institutional capture dynamics. Genuinely coordinates risk-sharing (benefits borrowers, lenders, real economy). Also extracts through fee structures, regulatory arbitrage, and agency expansion. Cannot exit without losing institutional legitimacy and purpose. High suppression of alternative risk frameworks through regulatory entrenchment.
constraint_indexing:constraint_classification(moral_hazard_amplification, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized actors (central banks, IMF, Basel Committee) view moral hazard amplification as a temporary coordination failure with a sunset: macroprudential regulations, stress testing, countercyclical capital buffers, and bail-in frameworks are building alternative risk-distribution mechanisms. Has mobility and agency; sees exit path through reformed architecture with lower theater and distributed risk-bearing.
constraint_indexing:constraint_classification(moral_hazard_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: IMPLICIT GUARANTEE FICTION (PITON) — The institutional narrative that 'systemically important institutions are too big to fail' persists through inertia despite periodic disconfirmation (Lehman Brothers). Market participants oscillate between believing in the guarantee and pricing tail risk. The theater is high: policymakers maintain the fiction through vague commitment language, creating performative certainty without real backstop clarity. The constraint persists because no agent coordinating the narrative shift can unilaterally update the fiction.
constraint_indexing:constraint_classification(moral_hazard_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal temporal scope, moral hazard is an irreducible property of any risk-sharing system: the moment you guarantee loss protection, incentives to prevent loss weaken. This appears as natural law — a timeless tradeoff between risk-sharing coordination and incentive preservation. However, the structural data contradicts this: moral hazard amplification is contingent on information asymmetry, measurement difficulty, and institutional design choices. The mountain classification reveals where the analysis risks naturalizing contingent institutional arrangements as inherent limits.
constraint_indexing:constraint_classification(moral_hazard_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_hazard_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_hazard_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_hazard_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_hazard_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_hazard_amplification, TR),
    TR >= 0.70.

:- end_tests(moral_hazard_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The guarantee structure concentrates tail risks onto residual bearers while risk-takers externalize losses. The extraction is not total (there are genuine coordination benefits) but substantial and growing. The 0.32→0.58 trajectory reflects accumulation of hidden leverage and regulatory arbitrage as institutions find new ways to take risks while remaining covered. Suppression (0.62): Moderate-high. Residual risk bearers have no formal exit (regulatory obligation), but informal barriers are even stronger (institutional interdependence, information asymmetry, inability to coordinate collective exit). Guarantee administrators suppress information about tail risk concentration through aggregation, complexity, and regulatory opacity. Theater ratio (0.55): Moderate. The constraint exhibits mixed function: genuine coordination exists (guarantees do enable socially beneficial borrowing), but substantial theater persists (regulatory reports showing 'adequate capital ratios' while tail risks accumulate unseen; implicit guarantees maintained through vague commitment language rather than explicit backstop contracts; stress tests calibrated to show stability rather than to identify breaking points). The theater has grown over time as institutions develop sophisticated compliance theater to manage the gap between implicit guarantee fiction and actual risk concentration.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Risk-taking beneficiaries see coordination (Rope) — the guarantee solves the lending market failure. Residual risk bearers see extraction (Snare) — they absorb tail risks with no decision-making power. Guarantee administrators see themselves as coordinating (Tangled Rope) but also trapped by institutional mission expansion. Regulatory reformers see a temporary problem (Scaffold) — countercyclical regulations and bail-in frameworks will distribute risk more symmetrically. The implicit guarantee narrative sees itself as institutional necessity (Piton) — the fiction persists through inertia despite periodic disconfirmation. The analytical observer risks naturalizing moral hazard as inherent law (Mountain) — 'you cannot have guarantees without incentive distortion' — but the structural data shows this is contingent on information asymmetry and institutional design, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position in the extraction-coordination hybrid. Risk-taking beneficiaries (institutional/arbitrage) have d≈0.15 — low experienced extraction because they capture coordination benefits and have exit options to unguaranteed activities. Residual risk bearers (powerless/trapped) have d≈0.95 — maximum extraction, no exit, full cost absorption. Guarantee administrators (institutional/constrained) have d≈0.50 — intermediate position, genuinely coordinating but also expanding extractive scope. Regulatory reform coalition (organized/mobile) has d≈0.40 — can see and partially exit the system through alternative mechanisms. The perspectival gap between risk-taker (rope) and residual bearer (snare) reveals that the same guarantee structure is experienced as pure coordination by beneficiaries and pure extraction by bearers.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH PERSPECTIVAL DECOMPOSITION: Moral hazard amplification resolves the mandatrophy by showing that coordination and extraction genuinely coexist in the same constraint structure. The guarantee coordinates lending/insurance (solves market failure) while simultaneously extracting through moral hazard (incentive distortion that concentrates unpriced risks). This is not a misclassification — tangled_rope is the correct type. The mandatrophy dissolves when you recognize that 'is this pure coordination or pure extraction?' is the wrong question. The correct question is 'who benefits from coordination and who bears the extraction costs?' The answer: risk-takers benefit from coordination, residual bearers absorb extraction. The constraint's function (coordination) is genuine; its asymmetry (extraction) is also genuine. Regulatory reform coalition perspective confirms the sunset logic: bail-in frameworks, countercyclical buffers, and improved information systems can distribute risks more symmetrically, converting tangled_rope toward rope by reducing the information asymmetry and principal-agent problems that drive moral hazard. This requires active restructuring, not passive reform theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_hazard_threshold_detectability,
    'At what point does moral hazard become empirically detectable versus remaining hidden in noise and incentive variation?',
    'Time-series analysis of risk-taking behavior pre/post guarantee introduction; cross-institutional comparison of moral hazard magnitude across different guarantee structures; principal-agent modeling with calibrated information asymmetries',
    'If threshold is high: current suppression (0.62) underestimates because many extraction mechanisms remain unobserved. If threshold is low: theater_ratio may be overcounted as institutions engage in performative compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_threshold_detectability, empirical, 'Detectability threshold for moral hazard as distinct from noise').

omega_variable(
    guarantee_administrator_capture_mechanism,
    'Does the guarantee administrator extract primarily through explicit fee structures, through regulatory scope expansion, or through institutional fusion with the guaranteed sector?',
    'Comparative analysis of regulatory agencies with different fee structures and mandate boundaries; measurement of scope drift over time; network analysis of personnel circulation between guarantee administrators and guaranteed institutions',
    'If explicit fees dominate: extraction is measurable and potentially correctable. If scope expansion/institutional fusion dominates: the constraint is structurally embedded and requires architectural change, not fee adjustment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guarantee_administrator_capture_mechanism, empirical, 'Primary extraction mechanism of guarantee administrator').

omega_variable(
    systemic_tail_risk_concentration,
    'Is the concentration of unpriced tail risks in the guarantee system growing, stable, or shrinking over time?',
    'Stress testing with varying tail probability assumptions; measurement of system leverage and interconnection over business cycle; comparison of explicit reserves versus implicit contingent liability',
    'If growing: extractiveness (0.58) understates the constraint''s severity — suppress suppression metrics may not bind. If shrinking: regulatory reform coalition perspective (scaffold) is structurally valid; if stable: system has reached equilibrium but at high structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_tail_risk_concentration, empirical, 'Systemic tail risk accumulation trajectory').

omega_variable(
    implicit_guarantee_collapse_dynamics,
    'When implicit guarantees become explicit (crisis revelation), do residual risk bearers bear costs proportional to the hidden extraction, or do they experience cliff-like systemic breakdown?',
    'Historical analysis of guarantee revelation episodes (LTCM, 2008 financial crisis, banking crises in emerging markets); measurement of cost distribution in bailout sequences; analysis of whether bailout costs proportionally match pre-crisis extraction patterns',
    'If proportional: snare perspective is accurate model of the extraction flow. If cliff-like: the system exhibits latent instability and the constraint should reclassify toward higher extractiveness and lower analytical mountaintop validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_guarantee_collapse_dynamics, empirical, 'Cost distribution pattern in implicit guarantee collapse').

omega_variable(
    alternative_risk_distribution_viability,
    'Do emerging alternative mechanisms (mutual insurance, decentralized risk pools, blockchain-based collateral) actually reduce moral hazard or merely displace it to different agents and information asymmetries?',
    'Comparative analysis of moral hazard levels in alternative risk-sharing systems; measurement of information asymmetry and principal-agent problems in blockchain/decentralized systems; long-term tracking of alternative mechanism adoption and failure rates',
    'If viable: scaffold perspective is structurally sound and sunset timeline is real. If displacement-only: alternatives are spectral transformations of the same underlying constraint, and claimed_type should remain tangled_rope or snare indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_risk_distribution_viability, empirical, 'Whether alternative risk mechanisms reduce or displace moral hazard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_hazard_amplification, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(morhaz_tr_t0, moral_hazard_amplification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(morhaz_tr_t5, moral_hazard_amplification, theater_ratio, 5, 0.48).
narrative_ontology:measurement(morhaz_tr_t10, moral_hazard_amplification, theater_ratio, 10, 0.55).
narrative_ontology:measurement(morhaz_tr_t15, moral_hazard_amplification, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(morhaz_be_t0, moral_hazard_amplification, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(morhaz_be_t5, moral_hazard_amplification, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(morhaz_be_t10, moral_hazard_amplification, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(morhaz_be_t15, moral_hazard_amplification, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_hazard_amplification, resource_allocation).
narrative_ontology:affects_constraint(moral_hazard_amplification, regulatory_capture_finance).
narrative_ontology:affects_constraint(moral_hazard_amplification, systemic_risk_accumulation).
narrative_ontology:affects_constraint(moral_hazard_amplification, implicit_guarantee_commitment).

% DUAL FORMULATION NOTE:
% Moral hazard amplification decomposes into three structurally distinct constraints: (1) moral_hazard_amplification itself (ε=0.58, tangled_rope) — the behavioral mechanism of risk-taker response to guarantees; (2) regulatory_capture_finance (ε=0.65, snare) — the guarantee administrator's institutional expansion and fee extraction; (3) implicit_guarantee_commitment (ε=0.72, piton) — the narrative maintenance of 'too big to fail' belief through performative regularity. Each has its own extractiveness because each targets different mechanisms and agents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_hazard_amplification, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
