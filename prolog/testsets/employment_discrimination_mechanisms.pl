% ============================================================================
% CONSTRAINT STORY: employment_discrimination_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_discrimination_mechanisms, []).

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
 *   constraint_id: employment_discrimination_mechanisms
 *   human_readable: Employment Discrimination Mechanisms
 *   domain: labor/discrimination/institutional
 *
 * SUMMARY:
 *   Employment discrimination mechanisms create a structural tension between
 *   organizational preferences for cultural homogeneity (presented as
 *   coordination need) and the exclusion and extraction of workers from
 *   discriminated demographic groups. The constraint exhibits core features
 *   of a Tangled Rope: genuine coordination function (organizations do
 *   achieve operational efficiency through selective hiring) combined with
 *   asymmetric extraction (discriminated workers bear concentrated costs).
 *   The mechanism operates through multiple channels: hiring screening (overt
 *   and statistical), promotion gatekeeping, wage suppression, and
 *   psychological burden. The theater ratio (0.55) reflects the gap between
 *   formal anti-discrimination compliance (EEOC filings, diversity
 *   statements, hiring policies) and actual discrimination persistence — the
 *   apparatus maintains legitimacy rituals while enforcement remains weak and
 *   discriminators face limited consequences. Measurements show a gradual
 *   decline in extractiveness (0.70 to 0.62) and rise in theater (0.35 to
 *   0.55) over 40 years, consistent with decreasing overt discrimination
 *   alongside increasing performative compliance — a pattern typical of
 *   constraints under regime transition (moving toward scaffold) but not yet
 *   at the sunset point.
 *
 * KEY AGENTS:
 *   - Discriminated Workers: Primary victims (powerless/trapped) — structurally trapped by employment dependency and identity markers they cannot change; bear wage penalties, opportunity exclusion, psychological costs; high suppression from information asymmetry and retaliation risk
 *   - Privileged Demographic Groups: Primary beneficiaries (institutional/arbitrage) — capture competitive advantage, promotion bias, wage premiums; maintain extractive mechanism through selective enforcement of hiring and promotion norms
 *   - Organizational Gatekeepers: Institutional beneficiaries (institutional/arbitrage) — reduce perceived hiring friction and maintain cultural homogeneity; experience discrimination mechanism as coordination tool; benefit from reduced integration costs
 *   - Allied Workers: Secondary actors (moderate/constrained) — privileged workers who might challenge discrimination norms but face career risk and social penalties for solidarity; also benefit from reduced competition
 *   - Labor Advocacy Coalition: Organized agents (organized/constrained) — unions, civil rights organizations, regulatory bodies building alternative pathways (legal prohibitions, monitoring, norm shifts) with generational sunset logic
 *   - EEO Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains anti-discrimination procedures with significant procedural theater; enforcement gaps persist; retaliation barriers undermine deterrence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes simultaneous coordination and extraction; both functions are structural; classification remains Tangled Rope at all observables
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_discrimination_mechanisms, 0.62).
domain_priors:suppression_score(employment_discrimination_mechanisms, 0.68).
domain_priors:theater_ratio(employment_discrimination_mechanisms, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_discrimination_mechanisms, extractiveness, 0.62).
narrative_ontology:constraint_metric(employment_discrimination_mechanisms, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employment_discrimination_mechanisms, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_discrimination_mechanisms, tangled_rope).
narrative_ontology:human_readable(employment_discrimination_mechanisms, "Employment Discrimination Mechanisms").
narrative_ontology:topic_domain(employment_discrimination_mechanisms, "labor/discrimination/institutional").

domain_priors:requires_active_enforcement(employment_discrimination_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_discrimination_mechanisms, privileged_demographic_groups).
narrative_ontology:constraint_beneficiary(employment_discrimination_mechanisms, organizational_gatekeepers).
narrative_ontology:constraint_victim(employment_discrimination_mechanisms, discriminated_workers).
narrative_ontology:constraint_victim(employment_discrimination_mechanisms, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRIMINATED WORKER (SNARE) — Structurally trapped by employment dependency, identity markers they cannot change, and pervasive screening across employers. Experiences maximum extraction: excluded from opportunities, wage penalties, psychological costs. No exit without bearing severe material and identity costs. High suppression from information asymmetry, proof barriers, and retaliation risk.
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALLIED WORKER (TANGLED ROPE) — Constrained by career risk of challenging discrimination norms and social penalties for solidarity, but also benefits from the discrimination mechanism through reduced competition for promotions and opportunities. Experiences both extraction (career limitation if they challenge the system) and coordination benefit (preference within the constraint). Moderate power to organize but significant cost to exit.
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATIONAL GATEKEEPER (ROPE) — Net beneficiary. Benefits from screening mechanisms that reduce hiring friction (perceived or actual), maintain cultural homogeneity, and concentrate human capital in preferred groups. Experiences the constraint as coordination: the discrimination mechanism enables personnel management and organizational continuity. Can exit by changing hiring standards but retains arbitrage option (selective enforcement).
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR ADVOCACY COALITION (SCAFFOLD) — Organized agents (labor unions, civil rights organizations, regulatory bodies) perceive the constraint as a temporary coordination failure with sunset logic. Legal prohibitions (Title VII, Fair Employment Practices Acts), monitoring mechanisms, and norm evolution are building pathways to replace overt discrimination with alternative coordination. Low effective extraction because the coalition has organizational power and sees an exit horizon. Theater remains moderate as enforcement gaps persist.
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL ANTI-DISCRIMINATION APPARATUS (PITON) — Legal and bureaucratic anti-discrimination frameworks persist through institutional inertia despite significant enforcement gaps. Theater is substantial: Equal Employment Opportunity Commission processes are slow, burden-of-proof standards are high, retaliation is common, and documented discrimination rates do not match enforcement outcomes. The apparatus maintains legitimacy rituals (grievance procedures, formal compliance reviews) while actual discrimination persists. Functional verification and deterrence are degraded.
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ANALYTICAL) — From civilizational/global analysis, employment discrimination is simultaneously a coordination mechanism (screening for cultural fit, reducing integration costs for organizations) and an extraction mechanism (concentrating opportunities and wage premiums in privileged groups). The constraint exhibits genuine coordination function (firms do achieve operational continuity through homogeneity) alongside asymmetric extraction (discriminated workers bear the cost). Requires active enforcement to maintain. Classification at this level is Tangled Rope: both functions are structural, not optional.
constraint_indexing:constraint_classification(employment_discrimination_mechanisms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_discrimination_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employment_discrimination_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employment_discrimination_mechanisms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_discrimination_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employment_discrimination_mechanisms, TR),
    TR >= 0.70.

:- end_tests(employment_discrimination_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint concentrates measurable wage losses, opportunity exclusion, and retaliation costs on discriminated workers. Wage gap residuals (controlling for education/experience) average 10-15% in US labor markets; hiring audit discrimination rates show 20-50% differential callback rates for identical credentials; subjective discrimination reports from affected workers are widespread. This high extraction is partially offset by evidence that some employers have reduced discrimination through norm evolution and competitive pressure (missing workers penalty). Extractiveness is not at maximum (0.70+) because: (a) some discrimination has eroded over time, (b) competitive labor markets should incentivize some employers to hire from discriminated groups, and (c) individual organizational variation is substantial. Suppression (0.68): High. Multiple barriers prevent escape: employment dependency (workers need income), identity markers (cannot be hidden), information asymmetry (discrimination is often covert), retaliation risk (whistleblowers face career penalties), high evidentiary burden for legal claims. Theater ratio (0.55): Moderate. Formal anti-discrimination apparatus (EEOC procedures, diversity statements, hiring policies, training programs) is substantial and visible. Yet enforcement is weak: EEOC case backlog is years long, burden of proof on plaintiff is high, successful discrimination charges are rare relative to complaints filed, and organizational penalties for violations are low relative to benefit from discrimination. Theater has increased over the interval as compliance apparatus expanded while discrimination persisted.
 *
 * PERSPECTIVAL GAP:
 *   Why does the discriminated worker see Snare while the gatekeeper sees Rope? Not because they disagree on facts, but because they occupy structurally opposite positions in the extraction flow. For the worker: discrimination is coercive (no exit option, high cost), asymmetric (they bear cost, gatekeeper benefits), and has no coordination function that benefits them. For the gatekeeper: discrimination is presented as coordination (maintaining cultural continuity), voluntary (they can choose to discriminate or not), and symmetrically beneficial (organizations and privileged workers both benefit). The perspectival gap reveals that 'coordination' is perspectival: it coordinates the gatekeeper and privileged workers while extracting from discriminated workers. The Rope (from gatekeeper view) and Snare (from worker view) are the same mechanism seen from opposite ends.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position and exit options. The discriminated worker (powerless + trapped) occupies maximum directionality (d ≈ 0.95): structurally immobile, subject to discrimination across all employers, unable to exit without abandoning employment altogether. High f(d) produces high experienced extractiveness. The organizational gatekeeper (institutional + arbitrage) occupies minimum directionality (d ≈ 0.05): can selectively apply discrimination or not; can exit to alternative hiring methods while retaining institutional standing; has alternative labor supply options. The allied worker (moderate + constrained) occupies intermediate directionality (d ≈ 0.55): faces costs to exit (social penalties, career damage if they challenge norms) but has options (can advocate, can transfer, can reduce participation in discriminatory decisions). The analytical observer (analytical + analytical) occupies observational directionality (d ≈ 0.72): sees the full network but cannot directly exit the epistemic position. These derived d values feed into the chi formula: higher d for victims produces higher effective χ; lower d for beneficiaries produces lower/negative effective χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that Tangled Rope is the correct classification at the analytical level because BOTH the coordination function and the asymmetric extraction are structurally real. The gatekeeper is not lying that discrimination provides coordination benefit (organizations with similar cultural backgrounds do experience lower integration friction). The worker is not wrong that discrimination is extraction (they bear concentrated costs). The mandatrophy resolution: both observations are correct, which is precisely what Tangled Rope means — genuine coordination function combined with asymmetric extraction. The mistake would be: (a) forcing it into pure Rope by denying the extraction (organizational coordination discourse), or (b) forcing it into pure Snare by denying the coordination function (worker-experience discourse). The constraint's actual structure is hybrid. Suppression enables this hybridity by preventing workers from exercising exit options that would force the cost-benefit calculation to become visible. If suppression were lower (workers could easily switch employers or fields), the extraction would become unsustainable and the constraint would collapse into either pure Snare (if suppression alone sustained it) or reveal itself as unnecessary (if neither coordination nor extraction were real). The persistence of the constraint at high suppression indicates it is genuinely mixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_vs_taste_discrimination,
    'How much of measured employment discrimination is taste-based preference vs. statistical discrimination based on imperfect information?',
    'Controlled experiments (resume studies with identical credentials/demographics), within-firm promotion analysis controlling for actual performance, longitudinal tracking of discriminator learning over multiple interactions',
    'If primarily statistical: discrimination should erode as employers acquire information. Extractiveness should decline over biographical horizon. If primarily taste-based: discrimination persists regardless of performance feedback. Extractiveness remains high at all time horizons. If mixed: decompose into separate constraints — statistical discrimination as temporary scaffold, taste-based as persistent snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statistical_vs_taste_discrimination, empirical, 'Discrimination mechanism: statistical vs taste-based').

omega_variable(
    coordination_function_necessity,
    'Is the cultural homogeneity benefit genuine organizational coordination necessity or a proxy for risk aversion and bias against unfamiliar others?',
    'Comparative analysis of diverse vs homogeneous organizations matched on industry/size/region: productivity metrics, innovation rates, retention costs, conflict resolution effectiveness, crisis response. Control for selection effects (diverse organizations may be self-selected for openness).',
    'If homogeneity is necessary coordination function: Tangled Rope classification sustained — genuine coordination requires some selection. If proxy for bias: beneficiary''s perception of coordination is ideological cover story for extraction. Reclassify as Snare with performative coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether cultural homogeneity provides genuine coordination value').

omega_variable(
    enforcement_effectiveness_frontier,
    'What enforcement intensity (monitoring frequency, penalty severity, retaliation cost) would eliminate overt discrimination vs. which level sustains residual discrimination as optimal strategy for evaders?',
    'Regression discontinuity analysis across jurisdictions with different enforcement regimes; causal inference on penalty changes and discrimination outcome changes; field studies with varying audit frequency and cost signals',
    'If low enforcement eliminates discrimination: suppression is primarily informational/retaliation fear. Increase enforcement → scaffold classification (sunset as enforcement normalizes). If discrimination persists at high enforcement: suppression is taste-based and deeply embedded. Extractiveness remains high despite intervention — reclassify as persistent Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_frontier, empirical, 'Enforcement intensity required to eliminate discrimination').

omega_variable(
    identity_locked_vs_incentive_locked,
    'Are privileged workers locked into discrimination-supporting behavior primarily through identity fusion (in-group/out-group identity) or through incentive alignment (explicit benefit preservation)?',
    'Survey of identity-protective cognition and group attachment strength; comparison with incentive-aligned agents lacking identity fusion; longitudinal tracking of support for anti-discrimination policies as individual incentive alignment changes (e.g., loss of competitive advantage scenario)',
    'If identity-locked: perspective shift requires identity reframe, not just incentive change. Snare classification from allied-worker perspective more entrenched. If incentive-locked: policy changes and benefits restructuring can shift positions. Scaffold classification more viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_incentive_locked, conceptual, 'Identity vs incentive basis for discrimination support').

omega_variable(
    measurement_methodology_selection,
    'Does extractiveness vary significantly by measurement choice (wage gap residuals vs hiring audit discrimination vs promotion pipeline analysis vs subjective discrimination reports)?',
    'Apply multiple measurement methodologies to same population; compare extracted values; identify which methodologies are correlated vs divergent; qualitative analysis of measurement blind spots',
    'If measurement methodologies yield ε within 0.15 range: single constraint. If they diverge by >0.25: decompose into separate constraints (hiring discrimination, promotion discrimination, wage penalty discrimination) with different ε values and structural mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_methodology_selection, empirical, 'Whether measurement methodology choice determines extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_discrimination_mechanisms, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empdisc_tr_t0, employment_discrimination_mechanisms, theater_ratio, 0, 0.35).
narrative_ontology:measurement(empdisc_tr_t20, employment_discrimination_mechanisms, theater_ratio, 20, 0.48).
narrative_ontology:measurement(empdisc_tr_t40, employment_discrimination_mechanisms, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(empdisc_be_t0, employment_discrimination_mechanisms, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(empdisc_be_t20, employment_discrimination_mechanisms, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(empdisc_be_t40, employment_discrimination_mechanisms, base_extractiveness, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_discrimination_mechanisms, resource_allocation).
narrative_ontology:affects_constraint(employment_discrimination_mechanisms, wage_gap_mechanism).
narrative_ontology:affects_constraint(employment_discrimination_mechanisms, occupational_segregation).
narrative_ontology:affects_constraint(employment_discrimination_mechanisms, stereotype_threat_dynamics).

% DUAL FORMULATION NOTE:
% Employment discrimination is upstream of three structurally distinct constraints: wage gaps (ε≈0.45, focused on compensation mechanisms), occupational segregation (ε≈0.58, focused on pipeline effects), and stereotype threat (ε≈0.52, focused on cognitive/identity dynamics). Each has different measurement bases and different resolution mechanisms. The employment discrimination story models the coordination-extraction hybrid across all three, while downstream stories decompose domain-specific manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_discrimination_mechanisms, moderate, 0.55).
constraint_indexing:directionality_override(employment_discrimination_mechanisms, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
