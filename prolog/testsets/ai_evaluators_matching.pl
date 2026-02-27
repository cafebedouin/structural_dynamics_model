% ============================================================================
% CONSTRAINT STORY: ai_evaluators_matching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_evaluators_matching, []).

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
 *   constraint_id: ai_evaluators_matching
 *   human_readable: AI Talent Evaluators: Efficiency vs. Opacity in Automated Hiring
 *   domain: labor_economics/human_resources_technology
 *
 * SUMMARY:
 *   AI talent evaluation systems create a structural tension between
 *   organizational efficiency (legitimate coordination gain) and candidate
 *   access opacity (asymmetric extraction). Hiring organizations and
 *   evaluation vendors benefit from dramatically reduced screening burden and
 *   improved throughput; job candidates lose visibility into evaluation
 *   criteria, face irreversible rejections from opaque algorithms, and
 *   experience suppression of alternative matching pathways as evaluators
 *   consolidate market position. The constraint exhibits multiple
 *   classification types from different perspectives: pure extraction (Snare)
 *   for candidates with no exit, mixed coordination-extraction (Tangled Rope)
 *   for hiring managers with constrained agency, coordination (Rope) for
 *   vendors with arbitrage options, and temporary extraction with regulatory
 *   sunset (Scaffold) from the perspective of transparency advocates. The
 *   rising theater ratio (0.38 → 0.64) reflects vendor deployment of
 *   'explainability' and 'fairness' features as ritual compliance rather than
 *   functional oversight — the public commitment to transparency increases
 *   while actual decision opacity remains unchanged. The rising base
 *   extractiveness (0.35 → 0.58) shows that as AI evaluation systems
 *   consolidate market position and lock organizations into vendor platforms,
 *   the extraction coefficients deepen through reduced switching capacity and
 *   candidate data accumulation.
 *
 * KEY AGENTS:
 *   - Job Candidates: Primary victims (powerless/trapped) — face irreversible rejections from opaque algorithms with no appeal mechanism or alternative pathways
 *   - Hiring Organizations: Primary beneficiaries (institutional/arbitrage) — capture dramatic efficiency gains through automation and volume throughput; maintain exit option via vendor switching
 *   - Evaluation Vendors: Secondary beneficiaries (institutional/arbitrage) — extract rents through lock-in and scale; control access to candidate evaluation tools and organizational data
 *   - Hiring Managers: Secondary actors (moderate/constrained) — benefit from automated screening but lose discretion over hiring decisions and human judgment capabilities
 *   - Regulatory Coalition: Organized opposition (organized/constrained) — labor agencies, civil rights advocates, transparency advocates pushing for algorithmic transparency and explainability mandates
 *   - Labor Market Transparency: Abstract victim (powerless/trapped) — institutional good that cannot organize; bears cost of opaque matching mechanisms that degrade market efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_evaluators_matching, 0.58).
domain_priors:suppression_score(ai_evaluators_matching, 0.68).
domain_priors:theater_ratio(ai_evaluators_matching, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_evaluators_matching, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_evaluators_matching, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_evaluators_matching, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_evaluators_matching, tangled_rope).
narrative_ontology:human_readable(ai_evaluators_matching, "AI Talent Evaluators: Efficiency vs. Opacity in Automated Hiring").
narrative_ontology:topic_domain(ai_evaluators_matching, "labor_economics/human_resources_technology").

domain_priors:requires_active_enforcement(ai_evaluators_matching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, hiring_organizations).
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, evaluation_vendors).
narrative_ontology:constraint_victim(ai_evaluators_matching, candidate_job_access).
narrative_ontology:constraint_victim(ai_evaluators_matching, labor_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB CANDIDATE (SNARE) — Trapped in AI evaluation system with no meaningful exit. Candidates cannot opt out, cannot understand decision criteria, cannot appeal opaque rejections. System extracts economic opportunity with maximum suppression: alternatives are non-existent when majority of employers deploy same AI evaluators.
constraint_indexing:constraint_classification(ai_evaluators_matching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIRING MANAGER (TANGLED ROPE) — Constrained by organizational scaling demands and candidate volume, but also benefits from reduced manual screening burden. Experiences genuine coordination function (faster hiring) alongside extraction (loss of human judgment discretion and implicit bias correction capability). Moderate power and constrained exit: can influence selection within constraints but cannot easily abandon AI evaluation system.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EVALUATION VENDOR (ROPE) — Pure coordination from vendor perspective: system solves the collective action problem of matching large candidate pools to organizational needs. Vendor has arbitrage exit (can switch to other business models) and captures most efficiency gains as revenue. Experiences constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(ai_evaluators_matching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized advocacy groups, labor unions, and regulators see AI hiring systems as temporary coordination failure with mandated sunset: algorithmic transparency requirements, adversarial testing mandates, and explainability standards create alternative verification pathways that bypass vendor opacity. Theater suppression declining as transparency mandates mature. Has sunset clause in principle (regulatory compliance path exists).
constraint_indexing:constraint_classification(ai_evaluators_matching, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESUME FILTERING INDUSTRY (PITON) — Legacy keyword-matching and rule-based filtering persists through institutional inertia despite being demonstrably inferior to modern AI evaluation. Theater ratio high (0.64): vendors market 'explainability' and 'fairness' as ritual compliance while actual decision logic remains opaque. Primary function (filtering high volume) has degraded into theater performance; constraint maintained because organizational alternatives haven't fully displaced legacy systems.
constraint_indexing:constraint_classification(ai_evaluators_matching, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, AI evaluation systems exhibit genuine coordination function (solving matching problem at unprecedented scale) AND asymmetric extraction (vendor capture of efficiency gains, candidate access foreclosure, labor market opacity). The constraint is not a natural law — efficiency vs. opacity is not inherent to AI — but a contingent institutional structure combining real coordination benefit with real power asymmetry. Effective extraction is substantial but bounded by regulatory pressure and transparency mandates.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_evaluators_matching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_evaluators_matching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_evaluators_matching, TR),
    TR >= 0.70.

:- end_tests(ai_evaluators_matching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The evaluation vendor captures substantial efficiency gains while candidates lose economic opportunity and market transparency. However, extraction is not maximal (0.70+) because some organizations can and do switch evaluators, regulatory pressure is mounting, and the underlying matching function provides real coordination value that benefits multiple parties. Base extractiveness reflects the structural asymmetry: vendors have arbitrage exit while candidates have none. Suppression (0.68): High. Candidates cannot opt out of AI evaluation (majority of employers use it), cannot understand rejection criteria, cannot appeal algorithmically-mediated decisions, and face reduced alternative matching pathways as evaluators consolidate. Alternatives (direct applications, recruiter networks, credential verification) are available but increasingly marginalized as employers automate away from them. Theater ratio (0.64): Moderate-high. Vendors increasingly market 'explainability' and 'fairness' features as public commitment to transparency, but underlying decision logic remains proprietary and opaque. The rise in theater ratio (0.38 → 0.64) tracks vendor adoption of compliance theater: fairness audits, bias testing frameworks, and explainability dashboards deployed to satisfy regulatory concern while preserving vendor control over evaluation criteria.
 *
 * PERSPECTIVAL GAP:
 *   The candidate sees pure extraction (Snare) — irreversible loss of opportunity with no appeal mechanism. The hiring manager sees coordination (Tangled Rope) — solving their volume problem but losing discretion. The vendor sees coordination (Rope) — solving the matching problem at scale. The regulatory coalition sees temporary extraction with a sunset (Scaffold) — transparency mandates and algorithmic auditing will eventually close the opacity gap. The legacy industry sees degraded function (Piton) — the constraint persists through institutional inertia despite better alternatives available. The analytical observer sees real coordination function (efficient matching) combined with real power asymmetry (vendor control, candidate foreclosure), classifying as Tangled Rope at civilization scope — the efficiency vs. opacity tension is not inherent to AI but a contingent institutional structure that could be restructured to preserve coordination while reducing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their position in the extraction flow and their exit capacity. Candidates (powerless/trapped) derive high d → high f(d) → high experienced extraction from their total dependence on AI evaluation and complete lack of exit alternatives. Vendors (institutional/arbitrage) derive low d → negative f(d) → negative experienced extraction because they have multiple business models available and capture efficiency gains as revenue. Hiring managers (moderate/constrained) derive moderate d → moderate f(d) → moderate experienced extraction because they benefit from screening efficiency but face organizational and regulatory constraints on discretion. Regulatory actors (organized/constrained) derive moderate-low d because they have leverage through mandates but cannot unilaterally exit the system. The analytical observer derives moderate d reflecting the balanced coordination-extraction structure: real matching value exists, but real asymmetry in capture exists. No agent sees the system as purely extractive except the candidate, and no agent except the vendor sees it as purely beneficial.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the 'efficiency vs. opacity' framing naturalizes what is actually a contingent institutional structure. The true mandatrophy is: 'Is AI evaluation extraction or coordination?' The constraint exhibits both. The vendor efficiency gain (real coordination) and the candidate foreclosure (real extraction) are not trade-offs inherent to AI — they are consequences of institutional choices: vendor control of evaluation criteria, organizational outsourcing of hiring discretion, regulatory lag in algorithmic transparency mandates, and network effects enabling evaluator consolidation. The constraint is Tangled Rope, not Snare masquerading as Rope, because the coordination function is genuinely valuable (employers solve a real matching problem; candidates can be matched more efficiently in principle). However, the current institutional structure captures that coordination value asymmetrically: vendors and organizations capture efficiency gains while candidates bear opacity and access costs. The mandatrophy resolution is structural: decentralized credential systems, algorithmic transparency requirements, and portable evaluation records could preserve the coordination function while reducing extraction. The Scaffold perspective (regulatory sunset) is not aspirational — algorithmic transparency requirements (EU AI Act, EEOC guidance, state transparency laws) are already mandating explainability and algorithmic auditing. The extraction window is closing through regulatory intervention, not through market competition alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_sufficiency_threshold,
    'What degree of algorithmic explainability actually enables meaningful candidate appeal and regulatory audit, vs. cosmetic transparency that provides no real oversight?',
    'Comparison of appeal success rates under different explainability regimes; analysis of whether explanations match actual decision factors; longitudinal tracking of hiring outcome distributions after transparency mandates',
    'If true transparency achievable: constraint shifts from Snare to Tangled Rope for candidates. If explainability is performative theater: constraint remains Snare regardless of regulatory mandates, and suppression persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_sufficiency_threshold, empirical, 'Whether explainability requirements can overcome opacity suppression').

omega_variable(
    alternative_matching_viability,
    'Can decentralized, candidate-controlled credential systems (portfolios, skill badges, open assessments) actually compete with centralized AI evaluators for organizational efficiency?',
    'Empirical comparison of hiring speed and outcome quality under alternative matching architectures; analysis of network effects preventing decentralized alternatives from achieving scale',
    'If alternatives viable: scaffold sunset becomes structural rather than aspirational, and organizational lock-in is relaxed. If alternatives fail: vendor lock-in is durable, and suppression remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_matching_viability, empirical, 'Viability of decentralized alternatives to centralized AI evaluation').

omega_variable(
    bias_vs_discrimination_attribution,
    'Are observed disparities in AI hiring evaluations caused by model bias (remediable through technical intervention) or by structural discrimination baked into training data and evaluation metrics (only remediable through institutional restructuring)?',
    'Decomposition of hiring disparities into model bias, training data bias, and metric bias components; controlled experiments comparing debiased models to original; longitudinal tracking of whether technical debiasing reduces disparities in practice',
    'If technical remediation works: constraint is Tangled Rope with bounded extraction. If structural discrimination dominates: constraint is Snare for marginalized candidates, with high suppression resistant to vendor-side fixes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_vs_discrimination_attribution, empirical, 'Attribution of hiring disparities to remediable bias vs. structural discrimination').

omega_variable(
    vendor_lock_in_depth,
    'How durable is organizational lock-in to specific AI evaluation platforms once hiring data accumulates in vendor systems? Can organizations switch evaluators without losing institutional knowledge?',
    'Analysis of switching costs: time to retrain replacement systems, data extraction friction, organizational inertia. Comparison of churn rates under regulatory pressure vs. market competition.',
    'If lock-in is shallow: candidates and regulators have more leverage, and extraction coefficients are lower. If lock-in is durable: vendor control persists even under regulatory pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_lock_in_depth, empirical, 'Durability of vendor lock-in to specific evaluation systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_evaluators_matching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aiem_tr_t0, ai_evaluators_matching, theater_ratio, 0, 0.38).
narrative_ontology:measurement(aiem_tr_t5, ai_evaluators_matching, theater_ratio, 5, 0.51).
narrative_ontology:measurement(aiem_tr_t10, ai_evaluators_matching, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(aiem_be_t0, ai_evaluators_matching, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aiem_be_t5, ai_evaluators_matching, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(aiem_be_t10, ai_evaluators_matching, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_evaluators_matching, resource_allocation).
narrative_ontology:affects_constraint(ai_evaluators_matching, job_market_search_friction).
narrative_ontology:affects_constraint(ai_evaluators_matching, hiring_bias_perpetuation).
narrative_ontology:affects_constraint(ai_evaluators_matching, labor_data_asymmetry).

% DUAL FORMULATION NOTE:
% AI evaluation systems occupy the top of a constraint family decomposing the labor matching problem. Job search friction (upstream) creates demand for evaluation efficiency. Hiring bias perpetuation (downstream) results from training data biases in evaluation systems. Labor data asymmetry (parallel) reflects vendor control of candidate data accumulated through hiring systems. All three are structurally linked: improvements in evaluation transparency and portability reduce downstream bias and upstream friction simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_evaluators_matching, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
