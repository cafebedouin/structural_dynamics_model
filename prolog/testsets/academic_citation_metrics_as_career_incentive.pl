% ============================================================================
% CONSTRAINT STORY: academic_citation_metrics_as_career_incentive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_citation_metrics_as_career_incentive, []).

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
 *   constraint_id: academic_citation_metrics_as_career_incentive
 *   human_readable: Academic Citation Metrics as Career Incentive
 *   domain: academic/institutional/scientific_governance
 *
 * SUMMARY:
 *   Citation metrics have become the primary mechanism through which academic
 *   institutions measure research productivity and allocate career
 *   advancement. What began as a tool for measuring research impact (the
 *   genuine coordination function) has transformed into the primary
 *   determinant of hiring, promotion, and funding decisions. This
 *   transformation has created a structural extraction mechanism: researchers
 *   optimize for citations rather than intellectual merit, funding flows
 *   toward high-citation fields regardless of social value, and entire
 *   research communities (humanities, applied fields with non-academic
 *   audiences) face systemic disadvantage. The constraint exhibits genuine
 *   coordination function (citations do reflect research visibility and
 *   enable discovery) coupled with significant asymmetric extraction (access
 *   to citations depends on institutional affiliation, publication venue, and
 *   research direction alignment). The theater ratio has increased over time
 *   as the metric system has become more central to decision-making, with
 *   performative elements (citation gaming, salami publication, citation
 *   circles) increasingly visible. The constraint's extractiveness has grown
 *   as metric-optimization has become mandatory rather than optional.
 *
 * KEY AGENTS:
 *   - Early Career Scholar: Primary victim (powerless/trapped) — career survival depends entirely on citation metrics; no exit options within academic system
 *   - Underrepresented Field Researcher: Primary victim (powerless/trapped) — faces systemic citation disadvantage due to field-inherent practices misaligned with metric norms
 *   - Mid-Career Researcher: Secondary victim/beneficiary (moderate/constrained) — experiences both genuine coordination benefit and significant extraction; some agency in research direction
 *   - Elite Research Institution: Primary beneficiary (institutional/arbitrage) — benefits from citation metrics as reinforcement of existing prestige; can modify internal incentive structures
 *   - Citation Metric Vendor: Beneficiary (institutional/arbitrage) — solves legitimate information problem; benefits from institutional dependency on metric platforms
 *   - Citation Metric Reform Coalition: Organized agent (organized/constrained) — recognizes both coordination function and extractive mechanism; pushing toward alternative metrics
 *   - Traditional Peer Review System: Degraded mechanism (institutional/arbitrage) — citation metrics have replaced peer judgment through institutional inertia; maintained through habit
 *   - Analytical Observer: Cross-position view (analytical/analytical) — recognizes genuine coordination and genuine extraction are coupled; cannot be separated without losing both
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_citation_metrics_as_career_incentive, 0.52).
domain_priors:suppression_score(academic_citation_metrics_as_career_incentive, 0.65).
domain_priors:theater_ratio(academic_citation_metrics_as_career_incentive, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, extractiveness, 0.52).
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_citation_metrics_as_career_incentive, tangled_rope).
narrative_ontology:human_readable(academic_citation_metrics_as_career_incentive, "Academic Citation Metrics as Career Incentive").
narrative_ontology:topic_domain(academic_citation_metrics_as_career_incentive, "academic/institutional/scientific_governance").

domain_priors:requires_active_enforcement(academic_citation_metrics_as_career_incentive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, high_citation_researchers).
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, prestige_institutions).
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, citation_metric_vendors).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, early_career_scholars).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, underrepresented_field_researchers).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, quality_scholarship_without_citation_appeal).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER SCHOLAR (SNARE) — Trapped in tenure and promotion systems entirely governed by citation counts. Cannot exit without abandoning academic career. Bears full extraction: must conform research direction to citation-maximization rather than intellectual merit or social value. Zero degrees of freedom within the metric frame.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDERREPRESENTED FIELD RESEARCHER (SNARE) — Researchers in fields with naturally lower citation density (humanities, some social sciences, applied engineering with industrial rather than academic audiences) face systemic disadvantage. Their citation metrics are structurally depressed regardless of scholarship quality. Trapped by departmental and institutional adoption of global citation norms misaligned with field-specific citation practices.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-CAREER RESEARCHER (TANGLED ROPE) — Experiences both genuine coordination benefit (citation metrics enable discovery of relevant work, establish reputation networks) and significant extraction (career advancement depends on metrics that may not reflect actual contribution quality). Constrained by career dependencies but some agency in shaping research direction. Mixed experience with both coordination and asymmetric extraction.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE RESEARCH INSTITUTION (ROPE) — Prestigious universities benefit from citation metrics as pure coordination mechanism: citations reflect their research visibility and impact. High-citation research attracts funding, collaborations, and recruitment opportunities. For elite institutions, the metric system reinforces existing prestige through positive feedback loops. Net beneficiary with arbitrage options — can modify internal incentive structures or selectively deemphasize metrics.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION METRIC REFORM COALITION (TANGLED ROPE) — Organized agents (DORA signatories, funders, journal editors) recognize both the coordination function (metrics do reflect research impact at scale) and the extractive mechanism (over-reliance on metrics distorts research priorities). This perspective sees the constraint as reformable through alternative metrics (altmetrics, journal-specific citation analysis) and sunset logic. Organized agents have agency but face entrenched institutional resistance.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: CITATION METRIC VENDOR (ROPE) — Thomson Reuters, Scopus, Google Scholar, and similar platforms experience citation metrics as pure coordination: they are solving the legitimate information problem of making scholarship discoverable and impact measurable. Beneficiary with arbitrage options — can offer alternative metrics, modify ranking algorithms, or shift business models. Net positive relationship to the constraint system.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL PEER REVIEW SYSTEM (PITON) — Original peer review mechanism was designed to certify quality before publication. Citation metrics were never intended to replace this certification function. But over institutional inertia, citation counts have become proxy for peer judgment. The citation metric system now performs the certification function that peer review once did — albeit imperfectly. High theater ratio (metrics feel like quality certification but operate through gaming and field-specific biases). Maintained through institutional habit rather than functional superiority.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, citation metrics provide genuine coordination benefit: they enable large-scale research discovery, establish objective measures of influence, and reduce subjective gatekeeping. But the system exhibits massive asymmetric extraction: access to citation opportunities depends on institutional affiliation, publication venue, research direction alignment with high-citation fields, and temporal priority. The coordination and extraction are genuinely coupled — cannot separate them without losing both functions.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_citation_metrics_as_career_incentive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_citation_metrics_as_career_incentive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_citation_metrics_as_career_incentive, TR),
    TR >= 0.70.

:- end_tests(academic_citation_metrics_as_career_incentive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The citation metric system extracts through career path dependence and field-specific visibility biases. Researchers cannot compete fairly if their field has low citation density or if their research addresses non-academic audiences. The extraction is not maximal because legitimate research impact does correlate with citations at scale — some of the metric's predictive power reflects real influence. However, the extraction is significant because the relationship is noisy and systematically biased. Suppression (0.65): High. Multiple barriers constrain exit: (1) Institutional adoption of metrics is near-universal in hiring/promotion systems; (2) Alternatives to citation metrics exist (peer review, journal reputation, direct impact assessment) but are politically fragmented and less convenient; (3) Career switching out of academia is costly; (4) Funding agencies increasingly require metric documentation. Researchers experience high suppression despite theoretical alternatives because institutional coordination around metrics is extremely strong. Theater ratio (0.68): High. The theatrical elements are substantial: citation counts are presented as objective quality measures when they actually measure visibility and field popularity; impact factors are treated as research quality proxies despite documented unreliability; the machinery of metric calculation (algorithm opacity, platform vendor control) is largely invisible to end users. The theater has increased over the interval as metrics have become more central and as gaming strategies (self-citation, citation circles, salami publishing) have become more visible. The performative content is not the entire system — there is genuine coordination function underneath — but the theater is substantial.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between those with arbitrage options (elite institutions, metric vendors, high-citation researchers) who see rope or pure benefit, and those with trapped or constrained options (early career scholars, underrepresented field researchers) who see snare or tangled rope. The gap reflects a structural asymmetry: beneficiaries can modify or exit the system; victims cannot. The piton perspective reveals the role of institutional inertia — peer review certification has been replaced by metric gaming, and the old system persists through habit. The reform coalition perspective reveals that the gap could narrow through alternative metrics and field normalization — sunset logic is available if institutions adopt DORA-aligned standards.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by beneficiary/victim status and exit options. Early career scholars in trapped positions with victim status experience maximum directionality (d ≈ 0.95) — the constraint extracts maximum experienced extraction chi. Elite institutions with arbitrage options and beneficiary status experience minimum directionality (d ≈ 0.10) — they benefit through positive feedback loops. Mid-career researchers with moderate power and constrained exit experience medium directionality (d ≈ 0.60) — they experience significant extraction but retain some agency. The analytical observer at civilizational scope derives d from the balanced structural data: citation metrics do correlate with impact, but the correlation is noisy and biased, producing genuine mixed coordination-extraction (d ≈ 0.50-0.55, tangled rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandate paradox by showing that citation metrics genuinely serve a coordination function (making research discoverable at scale) while simultaneously enabling significant asymmetric extraction (career advantage for researchers in high-citation fields and elite institutions). The mandate is to measure research impact; the mechanism inevitably extracts because impact measurement creates prestige gradients, and prestige is scarce. The analytical observer's tangled rope classification is the correct structural diagnosis: the coordination and extraction are inseparable in the current institutional form. However, the constraint could be reformed (sunset trajectory) by adopting field-normalized metrics, open-access funding requirements that delink career advancement from journal prestige, and explicit alternative criteria (peer review, direct impact assessment) for hiring and promotion. The constraint is not a natural law (mountain) — it is a designed institutional arrangement that could be redesigned. The false mountain reading (citing metrics as inherent to science) is a common justification used by beneficiaries; the structural data reveals this as naturalization of a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    citation_amplification_mechanism,
    'Does the citation metric system amplify or merely measure underlying research impact?',
    'Counterfactual analysis: compare citation distributions in pre-metric era (1970-1995) with post-metric era (2000-present) for equivalent research quality. If distribution changed, metric system is amplifying; if stable, it is measuring.',
    'If amplification: extraction is structural and metric-driven. If measurement: extraction reflects pre-existing field hierarchies. Classification remains tangled_rope either way, but causal attribution differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citation_amplification_mechanism, empirical, 'Whether metrics amplify or measure underlying impact').

omega_variable(
    field_citation_density_arbitrage,
    'Is the apparent citation disadvantage of humanities and social sciences due to field-inherent practices or metric system design?',
    'Field-normalized citation analysis: adjust raw citation counts by within-field percentiles. If field-normalized distributions show no disadvantage, the metric design is the problem; if disadvantage persists, it reflects field practices.',
    'If design problem: the constraint could be resolved through norm-adjusted metrics (current DORA trajectory). If field-intrinsic: disadvantage reflects structural differences in how knowledge circulates across disciplines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_citation_density_arbitrage, empirical, 'Source of citation density differences across fields').

omega_variable(
    self_citation_contribution_to_extraction,
    'How much of the extraction mechanism relies on self-citation gaming versus legitimate influence differentiation?',
    'Time-series analysis of self-citation rates by career stage and field; comparison of career outcomes for researchers with high vs low self-citation proportions controlling for publication volume.',
    'If self-citation is critical: suppression can be reduced through citation hygiene policies. If minimal: extraction is driven by legitimate field-specific visibility differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_citation_contribution_to_extraction, empirical, 'Contribution of self-citation gaming to extraction').

omega_variable(
    metric_dependency_lock_in,
    'Has the academic system become structurally dependent on citation metrics for institutional decision-making, such that removing metrics would cause decision paralysis?',
    'Historical analysis of tenure/promotion/funding decisions before widespread metric adoption (pre-1995) versus after (2010-present). Document whether current decision-makers report they lack criteria without metrics.',
    'If lock-in is real: the constraint exhibits supplier-of-last-resort extraction (institutions need metrics more than metrics vendors need institutions). If decision-makers have alternative criteria, they are choosing metrics for convenience rather than necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_dependency_lock_in, empirical, 'Structural dependency of academic institutions on citation metrics').

omega_variable(
    interdisciplinary_citation_cross_pollination,
    'Do citation metrics systematically penalize or reward interdisciplinary work, and does the penalty/reward vary across institutional contexts?',
    'Longitudinal tracking of citation accumulation for explicitly interdisciplinary projects by discipline pair and institutional prestige. Regression analysis of career outcomes by field-switching frequency.',
    'If penalized systematically: the constraint suppresses intellectual novelty and creates pathway lock-in. If rewarded or neutral: interdisciplinarity has survived the metric system. Evidence either way informs whether metrics are constraining research direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interdisciplinary_citation_cross_pollination, empirical, 'Whether metrics systematically penalize or reward interdisciplinary work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_citation_metrics_as_career_incentive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(citm_tr_t0, academic_citation_metrics_as_career_incentive, theater_ratio, 0, 0.32).
narrative_ontology:measurement(citm_tr_t5, academic_citation_metrics_as_career_incentive, theater_ratio, 5, 0.52).
narrative_ontology:measurement(citm_tr_t10, academic_citation_metrics_as_career_incentive, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(citm_be_t0, academic_citation_metrics_as_career_incentive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(citm_be_t5, academic_citation_metrics_as_career_incentive, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(citm_be_t10, academic_citation_metrics_as_career_incentive, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_citation_metrics_as_career_incentive, information_standard).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, journal_prestige_ranking_system).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, research_funding_allocation_by_metric).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, interdisciplinary_research_pathway_suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_citation_metrics_as_career_incentive, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
