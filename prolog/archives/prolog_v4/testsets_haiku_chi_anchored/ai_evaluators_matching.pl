% ============================================================================
% CONSTRAINT STORY: ai_evaluators_matching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: labor_economics/human_resources/algorithmic_governance
 *
 * SUMMARY:
 *   Automated AI talent evaluation systems create a structural extraction
 *   mechanism disguised as coordination. Hiring organizations and algorithmic
 *   vendors benefit from reduced screening costs, standardized metrics, and
 *   scale advantages. Job seekers and labor markets bear costs through
 *   opacity, reduced recourse, bias amplification, and information asymmetry.
 *   The constraint exhibits the full DR spectrum: pure extraction (snare)
 *   from the job seeker's perspective (trapped, powerless, no exit); pure
 *   coordination (rope) from the beneficiary's perspective (institutional,
 *   arbitrage, genuine efficiency gains); mixed coordination and extraction
 *   (tangled rope) from the labor market aggregate perspective (moderate,
 *   constrained, benefits from speed but harmed by opacity); temporary
 *   regulatory problem with sunset (scaffold) from the regulatory coalition's
 *   perspective (organized, constrained, building transparency requirements);
 *   degraded institutional narrative (piton) from the efficiency-frame
 *   perspective (theater persists despite contested empirical basis); and
 *   naturalized technical inevitability (mountain, false summit) from the
 *   civilizational analytical view. The theater ratio has increased from 0.35
 *   to 0.61 as controversy has mounted but efficiency narratives have
 *   persisted and strengthened in marketing claims despite deteriorating
 *   empirical defense.
 *
 * KEY AGENTS:
 *   - Job Seekers: Primary victim (powerless/trapped) — face opaque evaluation, cannot challenge algorithms, bear career cost of rejection without recourse or explanation
 *   - Hiring Organizations: Primary beneficiary (institutional/arbitrage) — reduce screening cost, achieve scale, standardize criteria; experience as coordination function; have vendor selection exit options
 *   - Algorithmic Vendors: Primary beneficiary (institutional/arbitrage) — capture switching costs, data advantages, network effects; frame constraint as efficiency solution
 *   - Labor Market Aggregate: Secondary victim (moderate/constrained) — exposed to systemic bias amplification, reduced information, but also benefits from faster hiring and reduced interview burden
 *   - Regulatory Coalition: Organized interveners (organized/constrained) — labor advocates, transparency groups, regulators building accountability frameworks (EU AI Act, EEOC guidance, right-to-explanation); see sunset path
 *   - Market Efficiency Frame: Institutional narrative (institutional/arbitrage) — persistent claim that AI evaluation is 'natural' optimization; theater increases with controversy but narrative persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choices as inevitable technical trade-offs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_evaluators_matching, 0.58).
domain_priors:suppression_score(ai_evaluators_matching, 0.68).
domain_priors:theater_ratio(ai_evaluators_matching, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_evaluators_matching, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_evaluators_matching, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_evaluators_matching, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_evaluators_matching, tangled_rope).
narrative_ontology:human_readable(ai_evaluators_matching, "AI Talent Evaluators: Efficiency vs. Opacity in Automated Hiring").
narrative_ontology:topic_domain(ai_evaluators_matching, "labor_economics/human_resources/algorithmic_governance").

domain_priors:requires_active_enforcement(ai_evaluators_matching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, hiring_organizations).
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, algorithmic_vendors).
narrative_ontology:constraint_victim(ai_evaluators_matching, job_seekers).
narrative_ontology:constraint_victim(ai_evaluators_matching, labor_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JOB SEEKER (SNARE) — Trapped in opaque evaluation system with no exit option. Cannot know scoring criteria, cannot challenge algorithmic decisions, bears full cost of rejection without recourse. Career trajectory constrained by black-box metrics they cannot interrogate. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(ai_evaluators_matching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIRING ORGANIZATION (ROPE) — Benefits from coordination function: AI evaluators reduce hiring time, standardize screening, enable scale. Experiences constraint as solution to real collective action problem (screening cost). Exit option (arbitrage) through vendor selection and competitive hiring market. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(ai_evaluators_matching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALGORITHMIC VENDOR (ROPE) — Primary beneficiary. Captures switching costs, data advantages, and market dominance through network effects. Frames constraint as efficiency gain and coordination enabler. Exit option (arbitrage) through vendor competition and feature development. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(ai_evaluators_matching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR MARKET AGGREGATE (TANGLED ROPE) — Constrained by systemic exposure; cannot exit without losing employment opportunity access. Benefits from some coordination (faster hiring, reduced interview burden). Bears costs through information asymmetry, bias amplification, and reduced transparency. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59. Mixed extraction and coordination.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY COALITION (SCAFFOLD) — Organized actors (labor advocates, regulators, transparency advocates) see algorithmic hiring as a temporary problem with a sunset: transparency requirements, explainability mandates, right-to-explanation laws are building accountability pathways. EEOC guidance, SEC disclosure rules, EU AI Act establish alternative frameworks. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28. Low effective extraction due to coalition agency and regulatory sunset clauses.
constraint_indexing:constraint_classification(ai_evaluators_matching, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MARKET EFFICIENCY FRAME (PITON) — Institutional narrative that AI evaluators are 'natural' market optimization. The frame persists through performative legitimacy (efficiency benchmarks, diversity metrics) despite degraded actual function (bias reproduction, misalignment of metrics with job performance). theater_ratio=0.61 captures persistent narrative legitimacy despite questioned empirical basis. Theater has increased as controversy has mounted but efficiency claims persist.
constraint_indexing:constraint_classification(ai_evaluators_matching, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some opacity is inherent to any automated evaluation at scale: full explainability becomes computationally intractable, perfect fairness is mathematically underdetermined, and measurement always introduces distortion (Goodhart drift). This perspective risks naturalizing what are actually contingent design choices as inevitable trade-offs. The structural data (ε=0.58, suppression=0.68) contradicts this classification — the engine will compute as false summit, revealing that the 'inherent to AI' framing obscures policy choices.
constraint_indexing:constraint_classification(ai_evaluators_matching, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_evaluators_matching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from job seekers (reduced transparency, reduced recourse) and captures it for beneficiaries (reduced screening cost, vendor lock-in). The extraction is not maximum because hiring organizations do derive genuine efficiency gains from AI evaluation, not pure rent-seeking. The value reflects that the coordination function is real but is coupled with asymmetric information extraction. Theater ratio (0.61): Moderate-high. Algorithmic hiring is defended through performative metrics (diversity dashboards, accuracy benchmarks, fairness claims) that persist despite contested empirical foundation. Vendors publish benchmark scores but obscure model details and training data. Theater has increased as criticism has mounted — the efficiency narrative has become more assertive as empirical questions have accumulated. Suppression (0.68): Moderate-high. Significant barriers to transparency include proprietary model architectures, black-box deep learning, switching costs, and regulatory lag. Job seekers have no mechanism to challenge or understand scoring criteria. Alternative hiring approaches exist but face coordination costs and network disadvantages. Suppression is not total because regulatory pressure (EU AI Act, EEOC) is building requirements.
 *
 * PERSPECTIVAL GAP:
 *   The job seeker sees a snare: trapped in an opaque system with no exit and no recourse. The hiring organization sees a rope: solving a genuine coordination problem (screening cost) with a tool that works. The vendor sees a rope: enabling efficient matching at scale. The labor market sees tangled rope: both benefits and costs, both coordination and extraction mixed. The regulatory coalition sees a scaffold: temporary problem with a regulatory sunset and viable alternative frameworks. The market efficiency frame sees a piton: degraded institutional narrative persisting through marketing despite contested empirical basis. The analytical observer risks seeing a mountain: technical inevitability of scale-explainability trade-offs. This perspectival distribution is diagnostic: if all perspectives produced the same classification, the constraint would have no meaningful structure. The gap reveals the constraint is neither pure coordination nor pure extraction, but a hybrid where beneficiaries and victims experience fundamentally different structural realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Job seeker: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from this agent; no exit options, bears full cost of opacity. Hiring organization: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; has vendor selection and competitive market exit options. Algorithmic vendor: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; has competitive exit through market and feature development. Labor market aggregate: Mixed victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction but not maximum; benefits from hiring speed but harmed by opacity and bias; cannot exit without losing employment access (constrained rather than trapped). Regulatory coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Moderate extraction; coalition has agency and builds alternative frameworks; regulatory timeline creates sunset path (constrained, not trapped). Market efficiency frame: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification driven by theater gate (≥0.70 required; actual 0.61 borderline), not from chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification would apply if constraint were truly natural law; structural data contradicts this, triggering false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY: The constraint resolves the false choice between 'coordination' and 'extraction' by showing that AI evaluation systems ARE coordination mechanisms that HAVE BEEN coupled with extraction. The beneficiaries (vendors, hiring organizations) truthfully perceive coordination — screening cost is real, the tool solves a real problem, efficiency gains are genuine. The victims (job seekers) truthfully perceive extraction — opacity is asymmetric, recourse is unavailable, information power is concentrated. Both are correct. The tangled rope classification unifies these truths: the constraint provides genuine coordination (reduced screening cost) AND enforces asymmetric extraction (opacity, lack of recourse). The mandatrophy dissolves when we stop asking 'is this coordination or extraction?' and start asking 'for whom?' The regulatory scaffold perspective shows the exit path: transparency requirements, explainability mandates, and right-to-explanation rules are building alternative coordination mechanisms (matched hiring WITH transparency) that decouple the coordination function from the extraction mechanism. If regulatory sunset succeeds, the constraint bifurcates: efficient hiring remains (rope), but opacity and extraction decline. If regulatory intervention fails, market lock-in accumulates and the constraint hardens toward snare (pure extraction with coordination framing). The theater ratio trajectory is diagnostic: as controversy accumulates, efficiency claims become more assertive and performative (theater rising from 0.35 to 0.61), while empirical defense weakens. This is textbook Goodhart drift — the efficiency narrative persists even as its empirical foundation erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explainability_feasibility_threshold,
    'What level of algorithmic explainability is technically feasible and economically viable without destroying the efficiency gains that justify AI evaluation systems?',
    'Technical analysis of explanation methods (LIME, SHAP, counterfactual generation) against false-positive rates and computational cost; cost-benefit analysis of explainability overhead vs. hiring cost savings',
    'If feasible at <5% efficiency loss: tangled rope classification shifts toward rope; transparency becomes binding constraint. If infeasible or cost-prohibitive (>20% overhead): snare classification hardens; opacity becomes structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explainability_feasibility_threshold, empirical, 'Feasibility of explainability without destroying efficiency gains').

omega_variable(
    algorithmic_bias_correction_sufficiency,
    'Can demographic parity, equalized odds, or other fairness metrics be enforced on AI evaluators without destroying their discriminative power and reverting to random screening?',
    'Empirical testing of fairness-constrained models against unconstrained baselines; measurement of hiring success rates and retention for demographically-balanced cohorts vs. performance-optimized cohorts',
    'If fairness constraints preserve performance: biased extraction can be engineered away; victim costs decline. If fairness-constrained models fail predictively: the constraint becomes a trilemma (fairness, accuracy, explainability — choose two); extraction hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_correction_sufficiency, empirical, 'Whether fairness constraints on AI evaluators are sufficiently powerful').

omega_variable(
    regulatory_enforcement_timeliness,
    'Can transparency and explainability regulations (EU AI Act, EEOC guidance, SEC disclosure) be enforced fast enough to prevent winner-take-most market concentration and vendor lock-in?',
    'Timeline analysis of regulation-to-enforcement, market consolidation rates, and vendor switching costs; comparison of regulatory velocity vs. technology deployment velocity',
    'If enforcement lags 5+ years behind deployment: market lock-in occurs, alternatives are strangled before they mature, scaffold sunset fails and constraint hardens to snare. If enforcement keeps pace: scaffold perspective is validated; regulatory intervention prevents extraction accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_timeliness, empirical, 'Whether regulation can enforce transparency before market lock-in').

omega_variable(
    alternative_hiring_mechanisms_viability,
    'Do alternative hiring approaches (skills-based assessment, apprenticeship pipelines, open hiring) achieve comparable hiring outcomes to AI evaluation without the opacity costs?',
    'Comparative study of hiring success, retention, and career progression across AI-screened vs. skills-based vs. open-hiring cohorts; cost analysis of alternative mechanisms',
    'If alternatives are comparable: the constraint is not a coordination mechanism but pure extraction; rope classification collapses and snare classification strengthens. If alternatives are significantly more costly: rope classification is validated; coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hiring_mechanisms_viability, empirical, 'Viability of alternative hiring mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_evaluators_matching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aiem_tr_t0, ai_evaluators_matching, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aiem_tr_t5, ai_evaluators_matching, theater_ratio, 5, 0.48).
narrative_ontology:measurement(aiem_tr_t10, ai_evaluators_matching, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(aiem_be_t0, ai_evaluators_matching, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aiem_be_t5, ai_evaluators_matching, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(aiem_be_t10, ai_evaluators_matching, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_evaluators_matching, resource_allocation).
narrative_ontology:affects_constraint(ai_evaluators_matching, labor_market_information_asymmetry).
narrative_ontology:affects_constraint(ai_evaluators_matching, algorithmic_bias_reproduction).
narrative_ontology:affects_constraint(ai_evaluators_matching, hiring_discrimination_enforcement).

% DUAL FORMULATION NOTE:
% The AI evaluators constraint is downstream of broader labor market information asymmetry and upstream of specific discrimination enforcement mechanisms. The constraint's ε=0.58 reflects the coupling of genuine coordination (screening cost reduction) with extraction (opacity, reduced recourse). Upstream constraints (information asymmetry, vendor lock-in) have higher ε values (≥0.65) reflecting their more purely extractive structure. Downstream constraints (bias reproduction, discrimination enforcement) have ε values sensitive to regulatory intervention, decomposing into separate stories based on measurement approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
