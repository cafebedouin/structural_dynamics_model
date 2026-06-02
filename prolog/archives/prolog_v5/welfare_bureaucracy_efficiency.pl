% ============================================================================
% CONSTRAINT STORY: welfare_bureaucracy_efficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_welfare_bureaucracy_efficiency, []).

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
 *   constraint_id: welfare_bureaucracy_efficiency
 *   human_readable: Welfare Bureaucracy Efficiency Constraint
 *   domain: social_policy/administrative_systems
 *
 * SUMMARY:
 *   Welfare bureaucracy efficiency creates a structural constraint between
 *   the genuine coordination problem of distributing limited resources fairly
 *   and the institutional incentive to deny claims through administrative
 *   friction. The system exhibits the full range of DR classifications
 *   depending on perspective. From the recipient's view, bureaucratic
 *   requirements function as pure extraction — documentation burdens, means
 *   testing surveillance, and benefit clawback penalties extract compliance
 *   labor and dignity without meaningful coordination function. From the
 *   administrator's view, the same requirements are legitimate coordination
 *   mechanisms preventing arbitrary claiming and ensuring fairness. From the
 *   political perspective, welfare complexity serves gatekeeping functions:
 *   discretionary administrative hurdles reduce claims and containing
 *   'undeserving' recipients becomes possible through procedural tightening.
 *   The constraint's theater ratio (0.68) reflects that significant
 *   administrative activity is performative: fraud detection consumes
 *   resources disproportionate to actual fraud detected; means testing
 *   surveillance demonstrates toughness rather than optimizing benefit
 *   delivery. The measurement trajectory shows both theater and
 *   extractiveness rising over the interval — not because the welfare problem
 *   grew harder to coordinate, but because political pressure for cost
 *   control intensified the administrative burden without removing the
 *   underlying coordination necessity.
 *
 * KEY AGENTS:
 *   - Welfare Recipients: Primary victim (powerless/trapped) — economically dependent, geographically immobile, face documented burden of bureaucratic proof requirements and benefit clawbacks with no exit option
 *   - Administrative Agencies: Primary beneficiary (institutional/arbitrage) — benefit from cost control through friction, institutional legitimacy through rigor, and discretionary gatekeeping authority
 *   - Caseworkers: Secondary victim/moderator (moderate/constrained) — constrained by caseload mandates and metrics rewarding fraud prevention over service delivery; genuinely coordinate resources but subordinated to enforcement logic
 *   - Political Gatekeepers: Secondary beneficiary (organized/mobile) — benefit from ability to restrict welfare access through administrative tightening; can arbitrage between benefit level and procedural burden
 *   - Social Justice Coalition: Organized opposition (organized/mobile) — see current system as temporary, advocating for simplified cash transfers and universal basic income with explicit sunset logic
 *   - Fraud Prevention Apparatus: Institutional actor (institutional/arbitrage) — maintains performative verification machinery through inertia and political theater despite low cost-benefit ratio on actual fraud detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(welfare_bureaucracy_efficiency, 0.58).
domain_priors:suppression_score(welfare_bureaucracy_efficiency, 0.62).
domain_priors:theater_ratio(welfare_bureaucracy_efficiency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(welfare_bureaucracy_efficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(welfare_bureaucracy_efficiency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(welfare_bureaucracy_efficiency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(welfare_bureaucracy_efficiency, tangled_rope).
narrative_ontology:human_readable(welfare_bureaucracy_efficiency, "Welfare Bureaucracy Efficiency Constraint").
narrative_ontology:topic_domain(welfare_bureaucracy_efficiency, "social_policy/administrative_systems").

domain_priors:requires_active_enforcement(welfare_bureaucracy_efficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(welfare_bureaucracy_efficiency, administrative_agencies).
narrative_ontology:constraint_beneficiary(welfare_bureaucracy_efficiency, political_gatekeepers).
narrative_ontology:constraint_beneficiary(welfare_bureaucracy_efficiency, cost_control_interests).
narrative_ontology:constraint_victim(welfare_bureaucracy_efficiency, welfare_recipients).
narrative_ontology:constraint_victim(welfare_bureaucracy_efficiency, program_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE RECIPIENT (SNARE) — Trapped by economic dependency and geographic immobility; cannot exit the system despite high extraction costs through bureaucratic friction. Faces supplication requirements, documentation burdens, means testing surveillance, and benefit clawback penalties. The system extracts compliance time and dignity without meaningful coordination function for the recipient.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CASEWORKER (TANGLED ROPE) — Constrained by caseload mandates and career incentives tied to fraud prevention metrics rather than service delivery. Genuinely coordinates recipient needs with available resources, but this coordination function is subordinated to compliance enforcement. Experiences extraction through workload intensification and responsibility without authority.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE AGENCY (ROPE) — Experiences the constraint as coordination: managing benefit distribution at scale requires verification procedures, documentation standards, and consistency rules. Net beneficiary through cost control, fraud mitigation framing, and institutional legitimacy through demonstrated procedural rigor. Can arbitrage between benefit levels and administrative investment.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL JUSTICE COALITION (SCAFFOLD) — Organized advocates see welfare bureaucracy complexity as a solvable coordination problem with a sunset clause. Universal Basic Income, direct cash transfers, and simplified eligibility pathways represent alternative coordination mechanisms. Theater high (means testing performance) but organizing groups have exit paths and policy influence. Sees the current system as temporary.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FRAUD PREVENTION APPARATUS (PITON) — Institutional machinery that has become substantially performative. Fraud detection consumes significant resources relative to actual fraud detected; the apparatus persists through institutional inertia and political theater around 'welfare queen' narratives rather than through demonstrated efficiency. Theater ratio high — the surveillance and verification rituals are maintained to demonstrate toughness, not because they optimize benefit delivery.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a formal economics perspective, administrative overhead is an immutable cost of any transfer system: verification is logically necessary to prevent arbitrary claiming, and verification inherently requires documentation, proof of eligibility, and review mechanisms. However, the structural data reveals this as a false summit — the 'inherent to administration' framing naturalizes what are actually contingent institutional design choices about how much verification to demand.
constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(welfare_bureaucracy_efficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(welfare_bureaucracy_efficiency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(welfare_bureaucracy_efficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(welfare_bureaucracy_efficiency, TR),
    TR >= 0.70.

:- end_tests(welfare_bureaucracy_efficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that welfare bureaucracy extracts both financial and dignity costs from recipients. The financial extraction includes administrative overhead (staff, systems), benefit clawbacks through income testing, and opportunity costs of time spent on documentation. The dignity extraction includes supplication requirements, surveillance, and humiliation through means testing. However, extractiveness is not maximal (not 0.80+) because some administrative cost is genuinely necessary coordination — verifying eligibility, preventing duplicate claims, and resource allocation at scale require documentation and verification. The constraint is hybrid: real coordination function plus asymmetric extraction. Suppression (0.62): Moderate-high. Recipients face substantial barriers to exit: economic dependency makes welfare necessary; geographic mobility is limited for low-income populations; alternative support systems (family, community) are increasingly unavailable; and documentation requirements create paper-thin barriers that are technically surmountable but practically formidable. Suppression is not total because some recipients do strategically work around the system, navigate it successfully, or advocate for reform. Theater ratio (0.68): High and rising. The measurement trajectory shows theater increasing from 0.52 to 0.68 over 21 periods — proportionally larger than extractiveness increase (0.42 to 0.58). This indicates that the constraint is degrading toward piton territory: verification apparatus is becoming more performative (fraud prevention theater) and less functionally optimized. The rise suggests political pressure for 'toughness' is driving procedural intensification beyond what actual fraud rates would justify.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the recipient (snare: pure extraction, no exit) and the administrator (rope: coordination, net benefit). This gap is NOT a measurement error — it reflects genuine structural difference. The recipient and administrator are solving different problems: the recipient needs benefits; the administrator needs to allocate resources according to eligibility rules and political constraints. Their agreement about the classification would suggest one of them is being denied or misrepresented. The secondary gap is between the administrator's rope (sees coordination) and the fraud prevention apparatus's piton (sees degraded ritual). The administrator still believes verification is functional; the apparatus shows rising theater with declining fraud detection efficiency, indicating institutional inertia. The coalition's scaffold perspective introduces a temporal dimension: they see the current system as a solvable problem with a sunset, not a structural feature. The analytical observer's mountain is revealed as a false summit — 'administration requires overhead' naturalizes what are actually contingent design choices about acceptable verification burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value derives from their structural relationship to the extraction flow. Recipients classified as trapped powerless agents: dependency + no exit = d ≈ 0.95, yielding high f(d) ≈ 1.42, producing high χ. Caseworkers classified as constrained moderate agents: resource constraints + career incentive misalignment + genuine service function = d ≈ 0.60, yielding f(d) ≈ 0.85, producing moderate χ. Administrators classified as beneficiary institutional agents with arbitrage options: cost control benefits + discretionary authority + ability to shift burden = d ≈ 0.15, yielding f(d) ≈ -0.01, producing low or negative χ (they experience coordination, not extraction). Coalition classified as mobile organized agents: exit pathways exist (policy alternatives documented), shared commitment to reform = d ≈ 0.40, yielding f(d) ≈ 0.40, producing moderate χ with sunset perspective. The fraud prevention apparatus classified as institutional arbitrage agent: cost control interest, institutional inertia = d ≈ 0.20, yielding low χ, but piton classification derives from theater gate, not from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how the same structural arrangement (documentation requirements, verification procedures, eligibility determination) functions as coordination for some agents and extraction for others. The mandatrophy resolves through recognition that BOTH perspectives are structurally correct — the system IS solving a coordination problem AND extracting from recipients. The tangled rope classification is the accurate one: genuine coordination function (preventing arbitrary claiming, allocating scarce resources) exists ALONGSIDE asymmetric extraction (recipients bear disproportionate burden of verification). The mandatrophy risk is misclassifying this as either pure rope (ignoring extraction costs and dignity harms) or pure snare (denying that any coordination function exists). The administrative perspective sees only the coordination problem and genuinely solved it according to their criteria. The recipient perspective sees only the extraction and genuinely experiences it as uncompensated burden. Both are correct — the constraint is hybrid. The measurement trajectory showing rising theater with rising extractiveness provides a warning: if theater continues rising and extractiveness continues rising, the constraint may degrade from tangled rope toward snare as the extraction function increasingly dominates the coordination function. The scaffold perspective prevents false naturalization by showing that alternative coordination mechanisms (cash transfers, simplified eligibility) can solve the coordination problem with lower extraction burden, proving that the current extractiveness is not inherent to the coordination task.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fraud_rate_threshold_ambiguity,
    'What fraud rate would justify the current verification burden, and is that burden calibrated to the actual fraud risk or to political theater?',
    'Comparative analysis of welfare fraud rates across OECD systems with different verification regimes; cost-benefit analysis of fraud prevention spending relative to prevented losses',
    'If actual fraud < 5%: current verification burden is uncalibrated extraction (reclassify snare confirmation). If fraud > 15%: verification regimes justified (downgrade extraction severity). Most evidence suggests 2-4% actual fraud, indicating significant over-verification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fraud_rate_threshold_ambiguity, empirical, 'Calibration of verification burden to actual fraud risk').

omega_variable(
    simplification_political_feasibility,
    'Can welfare systems be simplified to universal cash transfers without political collapse of the benefit coalition?',
    'Longitudinal tracking of public support for simplified systems; pilot program outcomes from Alaska Permanent Fund, Finland, Kenya, and Stockton CA experiments',
    'If feasible: scaffold classification confirmed, sunset timeline < 15 years. If infeasible: current system lock-in is structural not institutional (reclassify toward mountain or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simplification_political_feasibility, empirical, 'Political feasibility of welfare system simplification').

omega_variable(
    caseworker_autonomy_suppression_mechanism,
    'Is caseworker behavior driven by genuine legal/technical constraints on benefit disbursement, or by organizational culture and career incentives that could be reformed?',
    'Ethnographic comparison of caseworker behavior under different performance metrics (fraud prevention vs recipient satisfaction); analysis of discretionary authority actually exercised vs formally available',
    'If constraints structural (legal): suppression legitimate cost of verification. If constraints cultural: suppression is organizational extraction from both recipients and caseworkers (reclassify toward snare for both groups).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caseworker_autonomy_suppression_mechanism, empirical, 'Source of caseworker behavior constraints: legal necessity vs organizational incentives').

omega_variable(
    dignity_extraction_quantification,
    'How much of welfare bureaucracy''s extraction is financial (administrative costs, benefit clawbacks) vs dignity/autonomy costs (humiliation, surveillance, supplication rituals)?',
    'Recipient surveys on experienced extraction; comparative analysis of physical health and mental health outcomes between recipients in high-burden vs low-burden verification regimes',
    'If dignity costs dominate: extractiveness calculation may underestimate true burden (reclassify toward pure snare). If financial costs dominate: current calculation justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_extraction_quantification, conceptual, 'Relative magnitude of financial vs dignity extraction costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(welfare_bureaucracy_efficiency, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welfeff_tr_t0, welfare_bureaucracy_efficiency, theater_ratio, 0, 0.52).
narrative_ontology:measurement(welfeff_tr_t7, welfare_bureaucracy_efficiency, theater_ratio, 7, 0.61).
narrative_ontology:measurement(welfeff_tr_t14, welfare_bureaucracy_efficiency, theater_ratio, 14, 0.68).
narrative_ontology:measurement(welfeff_tr_t21, welfare_bureaucracy_efficiency, theater_ratio, 21, 0.72).

% Extraction over time
narrative_ontology:measurement(welfeff_be_t0, welfare_bureaucracy_efficiency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(welfeff_be_t7, welfare_bureaucracy_efficiency, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(welfeff_be_t14, welfare_bureaucracy_efficiency, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(welfeff_be_t21, welfare_bureaucracy_efficiency, base_extractiveness, 21, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(welfare_bureaucracy_efficiency, resource_allocation).
narrative_ontology:boltzmann_floor_override(welfare_bureaucracy_efficiency, 0.18).
narrative_ontology:affects_constraint(welfare_bureaucracy_efficiency, disability_eligibility_gatekeeping).
narrative_ontology:affects_constraint(welfare_bureaucracy_efficiency, unemployment_benefit_access_friction).
narrative_ontology:affects_constraint(welfare_bureaucracy_efficiency, medicaid_enrollment_barriers).

% DUAL FORMULATION NOTE:
% Welfare bureaucracy efficiency decomposes into domain-specific constraint stories: eligibility determination (coordination problem), verification procedures (mixed coordination/extraction), and administrative gatekeeping (institutional inertia). Each has distinct ε values. This story focuses on the system-level constraint; downstream stories address specific benefit program friction points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(welfare_bureaucracy_efficiency, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
