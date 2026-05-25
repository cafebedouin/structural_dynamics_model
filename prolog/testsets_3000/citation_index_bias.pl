% ============================================================================
% CONSTRAINT STORY: citation_index_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citation_index_bias, []).

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
 *   constraint_id: citation_index_bias
 *   human_readable: Citation Index Bias in Academic Knowledge Hierarchies
 *   domain: academic/epistemic/institutional
 *
 * SUMMARY:
 *   Citation index bias emerges from the institutional need to measure
 *   research productivity and quality at scale, but the measurement mechanism
 *   itself encodes structural visibility inequalities that distort funding
 *   allocation, hiring, and research direction. This constraint operates
 *   across all academic disciplines but with discipline-specific severity.
 *   The bias is not primarily statistical — citation counts are accurate
 *   measures of how much specific research is cited — but structural and
 *   systemic: the entities that get cited, and the rate at which they
 *   accumulate citations, reflect visibility patterns (institutional
 *   prestige, linguistic dominance, geographic concentration, disciplinary
 *   fashion) that are orthogonal to research quality or truth. The constraint
 *   exhibits all six DR types from different structural positions: powerless
 *   early-career researchers and peripheral scholars experience it as pure
 *   extraction (Snare); established institutions and citation index operators
 *   experience it as coordination (Rope); interdisciplinary researchers
 *   experience it as mixed (Tangled Rope); hiring committees use it as a
 *   performative ritual (Piton); and alternative metrics coalitions see it as
 *   a temporary problem with a visible sunset (Scaffold). The analytical
 *   observer, recognizing both the coordination function (aggregating
 *   information about research influence) and the extractive mechanism
 *   (encoding visibility biases), classifies it as Tangled Rope.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers in Novel Domains: Primary victims (powerless/trapped) — face metrics-driven career termination despite foundational contributions in fields with slow citation accumulation
 *   - Peripheral Scholar Communities: Primary victims (powerless/trapped) — scholars in non-Anglophone regions, teaching institutions, and non-elite universities rendered structurally invisible in citation databases
 *   - Citation Index Publishers (Thomson Reuters, Elsevier, Scopus): Primary beneficiaries (institutional/arbitrage) — operate consolidated systems with vendor lock-in and subscription revenue
 *   - Citation-Dense Research Institutions: Primary beneficiaries (institutional/arbitrage) — elite universities benefit from high citation concentration; metrics reinforce prestige hierarchy
 *   - Interdisciplinary Research Groups: Secondary victims (moderate/constrained) — benefit from cross-domain reach but bear costs of fragmented citations across siloed indices
 *   - Open-Access and Alternative Metrics Coalition: Organized agents (organized/constrained) — arXiv, PLOS, altmetrics platforms building parallel recognition infrastructure with generational sunset
 *   - University Hiring Committees: Institutional actors (institutional/arbitrage) — use citation metrics as performative proxy for quality; maintain ritual through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both genuine coordination (citations aggregate impact information) and significant extraction (visibility bias)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citation_index_bias, 0.58).
domain_priors:suppression_score(citation_index_bias, 0.62).
domain_priors:theater_ratio(citation_index_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citation_index_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(citation_index_bias, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(citation_index_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citation_index_bias, tangled_rope).
narrative_ontology:human_readable(citation_index_bias, "Citation Index Bias in Academic Knowledge Hierarchies").
narrative_ontology:topic_domain(citation_index_bias, "academic/epistemic/institutional").

domain_priors:requires_active_enforcement(citation_index_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citation_index_bias, citation_concentrated_institutions).
narrative_ontology:constraint_beneficiary(citation_index_bias, established_research_agendas).
narrative_ontology:constraint_victim(citation_index_bias, novel_research_directions).
narrative_ontology:constraint_victim(citation_index_bias, peripheral_scholar_communities).
narrative_ontology:constraint_victim(citation_index_bias, interdisciplinary_work).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER IN NOVEL DOMAIN (SNARE) — Trapped within citation metrics that measure productivity against established subdisciplines. Novel research directions accumulate citations slowly; the researcher faces metrics-driven funding denial, hiring rejection, and career termination despite foundational contributions. Cannot exit without abandoning the research program entirely. Maximum extraction — caught between innovation and survival metrics.
constraint_indexing:constraint_classification(citation_index_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERIPHERAL SCHOLAR COMMUNITY (SNARE) — Scholars working outside citation-dense institutional networks (non-Anglophone regions, teaching-focused institutions, non-elite universities) face structural invisibility in citation databases. Their work is not excluded; it is rendered unmeasurable. Trapped by geography and institutional affiliation — no exit path without institutional relocation. The constraint extracts productivity while denying recognition.
constraint_indexing:constraint_classification(citation_index_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERDISCIPLINARY RESEARCH GROUP (TANGLED ROPE) — Benefits from citation concentration of multiple disciplines (increased reach across subdiscipline boundaries) but bears costs of diffuse citation impact across multiple metrics (lower h-index per subdiscipline, fragmentation across databases). Constrained by the fragmentation of interdisciplinary work across siloed citation indices. Real coordination function exists — citations do measure cross-domain impact — but extraction is significant through the fragmentation cost.
constraint_indexing:constraint_classification(citation_index_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CITATION INDEX OPERATORS (ROPE) — Publishers (Thomson Reuters, Elsevier, Scopus) benefit from consolidated citation data, vendor lock-in, and institutional subscription models. Experience the constraint as pure coordination: aggregating citations solves the information discovery problem. Net beneficiary with high exit arbitrage (can transition to alternative index systems). Low experienced extraction.
constraint_indexing:constraint_classification(citation_index_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION-DENSE RESEARCH INSTITUTIONS (ROPE) — Elite research universities concentrate both citing and cited activity. Experience citation metrics as coordination mechanism: high citation counts accurately reflect research quality and impact within established fields. Net beneficiaries with arbitrage (can negotiate favorable terms, exclude low-metric researchers, maintain prestige). Low experienced extraction because their research naturally accumulates citations.
constraint_indexing:constraint_classification(citation_index_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-ACCESS AND ALTERNATIVE METRICS COALITION (SCAFFOLD) — Organized agents (PLOS, arXiv, altmetrics platforms, open citation projects) see citation index bias as a temporary coordination failure with a sunset: alternative metrics (downloads, social media engagement, preprint citations, direct impact measurement) are building parallel recognition systems. The constraint has a visible exit pathway — the coalition sees structured alternatives within a 10-20 year horizon. Sunset clause is structural: open-citation data and alternative metrics infrastructure are mature enough to provide genuine alternatives.
constraint_indexing:constraint_classification(citation_index_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CITATION METRICS AS HIRING RITUAL (PITON) — University hiring committees use citation counts as a performative proxy for quality assessment. The ritual persists (h-index checks, citation thresholds in job descriptions) despite widespread acknowledgment that citations measure visibility, not truth or innovation. Theater ratio is high because committees use metrics as a decision-legitimizing ritual while knowing the metrics are partial. The practice continues through institutional inertia and risk-aversion (metrics provide plausible deniability in hiring decisions) rather than because citations accurately measure research quality. Piton classification derives from high theater, not high extraction.
constraint_indexing:constraint_classification(citation_index_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint has genuine coordination function: citations do aggregate information about research influence and create incentives for rigorous, communicable work. But the extraction is significant: the metrics encode visibility biases (institutional prestige, linguistic dominance, disciplinary fashion) that are orthogonal to the coordination problem being solved. The analytical position requires both elements: real coordination function AND significant extraction.
constraint_indexing:constraint_classification(citation_index_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_index_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citation_index_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citation_index_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(citation_index_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(citation_index_bias, TR),
    TR >= 0.70.

:- end_tests(citation_index_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The bias extracts from peripheral scholars and novel research directions through visibility suppression, but the extraction is not as severe as pure predatory systems because citations do provide genuine information about research reach. The trajectory shows increasing extractiveness over the 20-year interval (0.35 → 0.58), reflecting institutional investment in citation systems for hiring/funding decisions, which amplifies the bias's extractive power. Suppression (0.62): Moderate-high. Scholars in peripheral communities and pursuing novel directions face real barriers: structural exclusion from citation-dense networks, publication bias toward established subdisciplines, institutional funding concentration toward high-citation fields. But suppression is not total — alternative publication channels and emerging citation systems provide partial exit pathways. Theater ratio (0.68): High and increasing. Hiring committees use citation counts as a performative proxy for quality despite acknowledging the metrics' limitations. The ritual legitimizes decisions that would be contested without quantitative cover. Theater increased from 0.52 to 0.68 over the interval, reflecting rising institutional reliance on metrics for defensible decision-making.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the beneficiary's rope and the victim's snare. Elite institutions see citation metrics as solving a genuine coordination problem — how do we measure research quality at scale? Their researchers naturally accumulate citations, so metrics confirm institutional quality intuitions. Peripheral scholars see the same metrics as pure extraction — their work is rendered invisible through no fault of the research quality. The gap is not in the metrics themselves (citations are measured accurately) but in how the distribution reflects institutional visibility patterns rather than research merit. The scaffold perspective from the open-access coalition reveals that this gap is not inevitable — alternative metrics and open-citation infrastructure could distribute recognition differently. But the coalition's proposed exit pathway is still immature; most scholars cannot yet rely on altmetrics for funding or hiring decisions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position and exit capacity. Citation-dense institutions have low d (0.05–0.15) because they are net beneficiaries with high exit capacity (could use alternative quality measures without penalty). Peripheral scholars have high d (0.90–0.95) because they are net targets with no exit (changing how they measure quality doesn't help if funders still use traditional indices). Early-career researchers in novel domains have high d (0.85–0.90) because they are targets of slow citation accumulation, despite some benefit from metric transparency. The interdisciplinary group has intermediate d (0.50–0.65) because they experience mixed costs and benefits. The open-access coalition has constrained d (0.40–0.55) because they are organized but still dependent on institutional adoption of alternatives. Directionality is not ambiguous; it flows directly from structural relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the same metrics system simultaneously provides genuine coordination (aggregating information about research reach) and significant extraction (encoding visibility biases). The apparent contradiction — how can one system both coordinate and extract? — dissolves when we recognize that coordinating via visibility-biased metrics serves the interests of visible institutions while extracting from invisible ones. The beneficiary's experience (Rope: 'This is how we measure quality') and the victim's experience (Snare: 'This is how we're rendered invisible') are both structurally accurate. The mandarophy resolution is: the constraint is Tangled Rope because it has a genuine coordination function (measuring research reach) AND a structural extraction mechanism (visibility bias), operating simultaneously on different agent populations. The alternative metrics coalition (Scaffold) offers a potential exit by decoupling coordination (we can still measure reach) from extraction (but using less biased metrics). The hiring committee's piton classification reveals that the constraint has degraded in its coordination function (committees acknowledge metrics don't measure quality) but persists through ritual rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visibility_vs_quality_conflation,
    'Are citations measuring research quality, research visibility, or research fashion? How much of the measured bias reflects genuine quality differences vs. structural visibility inequalities?',
    'Longitudinal tracking of highly-cited vs. low-cited research; post-hoc impact assessment of ''missed'' research; analysis of citation chasing vs. citation earned in review data',
    'If primarily visibility bias: constraint is purely extractive (Snare from more perspectives). If primarily fashion: constraint is a coordination failure. If mixed: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_vs_quality_conflation, empirical, 'Whether citations measure quality, visibility, or fashion').

omega_variable(
    alternative_metrics_effectiveness,
    'Do alternative metrics (altmetrics, download counts, preprint citations, social media engagement) provide a genuine alternative to traditional citation indices, or do they reproduce the same visibility biases at different scales?',
    'Correlation analysis of alternative metrics with traditional citation counts; assessment of whether alternative metrics improve representation of peripheral communities; longitudinal tracking of visibility distribution',
    'If alternatives are genuinely different: scaffold sunset is structural. If alternatives reproduce the bias: no exit pathway exists and the constraint is entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_metrics_effectiveness, empirical, 'Whether alternative metrics provide genuine alternatives or reproduce bias').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (citation systems force ranking that distorts funding) or internalized (scholars have internalized metrics as legitimate measures of worth)?',
    'Survey analysis of scholar attitudes toward citation metrics; comparison of suppression before/after explicit alternative-metrics training; ethnographic analysis of hiring committee decision-making',
    'If structural: removing metrics systems changes the suppression. If internalized: scholars carry suppression into alternative systems. This affects whether the scaffold exit is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in citation metrics').

omega_variable(
    disciplinary_variance_in_bias,
    'Does citation index bias affect disciplines uniformly, or do high-citation-velocity fields (physics, computer science) experience it differently than low-velocity fields (humanities, social sciences)?',
    'Comparative analysis of citation distribution by discipline; measurement of representation bias in highly-cited vs. lowly-cited subfields; analysis of whether citation concentrates differently across fields',
    'If highly differential: may require discipline-specific constraint stories rather than unified story. If uniform: single story captures all disciplinary contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_variance_in_bias, empirical, 'Whether citation bias affects disciplines uniformly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citation_index_bias, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(citbias_tr_t0, citation_index_bias, theater_ratio, 0, 0.52).
narrative_ontology:measurement(citbias_tr_t10, citation_index_bias, theater_ratio, 10, 0.62).
narrative_ontology:measurement(citbias_tr_t20, citation_index_bias, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(citbias_be_t0, citation_index_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(citbias_be_t10, citation_index_bias, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(citbias_be_t20, citation_index_bias, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citation_index_bias, information_standard).
narrative_ontology:affects_constraint(citation_index_bias, research_funding_concentration).
narrative_ontology:affects_constraint(citation_index_bias, institutional_prestige_hierarchy).
narrative_ontology:affects_constraint(citation_index_bias, publication_bias_toward_established_fields).

% DUAL FORMULATION NOTE:
% Citation index bias is downstream of institutional incentives to measure research productivity at scale (affects_constraints source) but acts as an upstream constraint on research direction and hiring practices. Related constraints: research_funding_concentration (funding flows to high-citation research, amplifying bias); institutional_prestige_hierarchy (citation systems reinforce existing status); publication_bias_toward_established_fields (established fields have higher citation velocity, creating feedback loop).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(citation_index_bias, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
