% ============================================================================
% CONSTRAINT STORY: publication_bias_psychology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publication_bias_psychology, []).

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
 *   constraint_id: publication_bias_psychology
 *   human_readable: Publication Bias in Psychology Research
 *   domain: psychology/research_methodology/epistemology
 *
 * SUMMARY:
 *   Publication bias in psychology represents a structural extraction
 *   mechanism disguised as quality gatekeeping. Researchers reporting
 *   positive findings receive disproportionate publication access,
 *   visibility, and career rewards compared to those reporting null or
 *   negative findings, even when the research quality is identical. This
 *   creates a systematic bias toward false positives in the accumulated
 *   literature and suppresses replication attempts. The constraint exhibits
 *   genuine coordination functions (journals need mechanisms to prioritize
 *   limited publication space; positive findings do carry more information
 *   than null results) but couples these with asymmetric extraction
 *   (replication researchers are penalized; the field accumulates distorted
 *   empirical knowledge; career advancement depends on publication venue
 *   rather than finding validity). The tea leaves reading: extractiveness
 *   increased from 0.35 to 0.58 over the interval as competitive pressures
 *   intensified and impact factors became more weaponized for career
 *   assessment. Theater ratio increased from 0.52 to 0.68 as the performative
 *   elements of impact-factor-driven selection became more visible. Recent
 *   reforms (preregistration, registered reports, open-science venues) have
 *   begun pushing back against the extractiveness, creating a slight
 *   reduction at T=30, but the constraint remains firmly entrenched in
 *   institutional structures.
 *
 * KEY AGENTS:
 *   - Failed Replicator: Primary victim (powerless/trapped) — researcher conducting null replication faces publication barriers and career consequences; no exit option
 *   - Prolific Lab: Primary beneficiary (institutional/arbitrage) — generates positive findings; experiences publication bias as coordination incentive; can exit by publishing null results or preregistering but weak incentive to do so
 *   - Psychology Subfield: Secondary actor (moderate/constrained) — accumulates biased literature; benefits from incentive structure for ambitious hypothesis testing but pays cost in false positives and wasted replication attempts
 *   - Journal Editorial System: Institutional gatekeeper (institutional/arbitrage) — enforces publication bias through selection mechanisms; experiences constraint as solving coordination problem of prioritizing scarce publication slots
 *   - Open Science Movement: Organized coalition (organized/constrained) — building alternative venues (preprint servers, registered reports, OSF) with lower bias; face barrier of institutional inertia in hiring/funding decisions but have agency to shift outcomes
 *   - Impact Factor Metric: Institutional artifact (institutional/arbitrage) — measures journal citation frequency but has become proxy for individual researcher quality and paper reliability; persists through inertia; reinforces publication bias at institutional level
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publication_bias_psychology, 0.58).
domain_priors:suppression_score(publication_bias_psychology, 0.62).
domain_priors:theater_ratio(publication_bias_psychology, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publication_bias_psychology, extractiveness, 0.58).
narrative_ontology:constraint_metric(publication_bias_psychology, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(publication_bias_psychology, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publication_bias_psychology, tangled_rope).
narrative_ontology:human_readable(publication_bias_psychology, "Publication Bias in Psychology Research").
narrative_ontology:topic_domain(publication_bias_psychology, "psychology/research_methodology/epistemology").

domain_priors:requires_active_enforcement(publication_bias_psychology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publication_bias_psychology, researchers_with_positive_results).
narrative_ontology:constraint_beneficiary(publication_bias_psychology, journals_with_impact_metrics).
narrative_ontology:constraint_beneficiary(publication_bias_psychology, funding_agencies_rewarding_novelty).
narrative_ontology:constraint_victim(publication_bias_psychology, field_empirical_reliability).
narrative_ontology:constraint_victim(publication_bias_psychology, unsuccessful_replication_researchers).
narrative_ontology:constraint_victim(publication_bias_psychology, evidence_accumulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAILED REPLICATOR (SNARE) — Graduate student or early-career researcher who invested months conducting a careful replication study, found null results, and faces publication barriers due to bias against negative findings. Trapped by career dependency on publication record; has no alternative outlet for rigorous null results. Suppression is structural: journals reject null results at 5-10x higher rates than positive results, and career committees weight accepted publications, not attempts. Maximum experienced extraction — the researcher's labor has scientific value but the constraint renders it invisible.
constraint_indexing:constraint_classification(publication_bias_psychology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PSYCHOLOGY SUBFIELD (TANGLED ROPE) — Benefits from the coordination function: publication bias creates incentive structure that drives ambitious hypothesis testing and methodological innovation. Genuine coordination problem exists — journals need a way to distinguish groundbreaking results from noise, and positive findings get more scrutiny than null results. BUT this coordination is coupled with asymmetric extraction: the subfield accumulates biased literature, failed replications go unpublished, meta-analyses must correct for selection bias, and the cumulative knowledge base reflects publication venue incentives more than empirical reality. High suppression (replication researchers face barriers) but not total (meta-science now publishes negative results). Significant active enforcement of the bias through editorial decisions and peer review framing.
constraint_indexing:constraint_classification(publication_bias_psychology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROLIFIC LAB (ROPE) — Institutional beneficiary (experienced high-impact researchers with ability to generate positive findings through multiple studies or exploratory hypothesis testing). Experiences publication bias as coordination: the system rewards novel positive findings with visibility and citations, enabling their continued funding and recruitment. Exit option arbitrage available — they can publish null results in secondary venues, can pre-register studies to reduce researcher degrees of freedom, or can shift to meta-science. But they face weak incentive to do so because the current system amplifies their success. Experiences constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(publication_bias_psychology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (Open Science Framework, PLoS ONE, preprint servers, registered reports initiatives) see publication bias as a temporary coordination failure being resolved through structural reforms. Pre-registration reduces researcher degrees of freedom and justifies null results. Registered reports shift editorial decisions before data collection, removing selective outcome reporting. arXiv and PsyArXiv enable null findings to reach audiences without journal gatekeeping. These mechanisms build alternative pathways that bypass traditional journal bias. Theater ratio lower here (pre-registration is functional, not performative) and sunset clause real: as preregistration norms mature and open-science venues accumulate impact, traditional journal bias becomes less extractive. Estimated sunset: 15-25 years for norms to fully mature in academic hiring and funding decisions.
constraint_indexing:constraint_classification(publication_bias_psychology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IMPACT FACTOR SYSTEM (PITON) — The journal impact factor metric was designed to measure journal quality and has mutated into a proxy for individual researcher quality and institutional prestige. Journals use it to justify selection bias toward novel, positive, high-impact-seeming results. But the metric itself has degraded: it measures venue citation frequency, not individual paper reliability, and is easily gamed through self-citations and special issues. The entire institutional apparatus — impact factors, journal tiers, prestige ranking — persists through inertia (university hiring committees use it, funding agencies use it) despite documented failure to predict individual paper reliability or reproducibility. Theater ratio high (0.68): the institutional enforcement of impact-factor-driven selection is substantially performative ritual masking outdated quality metrics.
constraint_indexing:constraint_classification(publication_bias_psychology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, it is tempting to treat publication bias as an immutable feature of how scientific publishing works: signal-to-noise filtering is inherent to any journal system, positive findings are more publishable than null results by nature of information content, and some degree of bias is inevitable. This perspective naturalizes the constraint as a mountain. However, the structural data reveals this as a false summit: publication bias is not a law of nature but a contingent institutional arrangement driven by journal business models, citation metrics, career incentive structures, and lack of infrastructure for null results. These are all changeable — and the open science movement demonstrates they are already changing.
constraint_indexing:constraint_classification(publication_bias_psychology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publication_bias_psychology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publication_bias_psychology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publication_bias_psychology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publication_bias_psychology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publication_bias_psychology, TR),
    TR >= 0.70.

:- end_tests(publication_bias_psychology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from unsuccessful replication researchers (lost opportunity for publication and career advancement) and from the field (distorted empirical base). But the extraction is not maximal (0.70+) because publication bias is not purely extractive — there is a genuine coordination problem (journals cannot publish all submissions) and positive findings do carry more information content. The extractiveness reflects primarily the asymmetry in how journal decisions treat positive vs null results, the lack of alternatives for null result publication, and the career penalty structure. Suppression (0.62): Moderate-high. Structural barriers to publishing null results exist: journal editors and reviewers preferentially accept positive findings at 5-10x higher rates; negative results are 0.5-2% of academic publishing despite representing a large fraction of actual research; career advancement metrics weight acceptance rather than research validity. But suppression is not total (0.70+) because some venues now publish null results (PLoS ONE, meta-science journals, preprint servers). Theater ratio (0.68): High-moderate. Impact factors, journal tiers, and impact metrics function partly as quality signals but have become substantially performative — universities and funding agencies use them as proxies for individual researcher quality despite documented poor correlation with individual paper reliability or reproducibility. The ritual of journal-based gatekeeping through prestige metrics is more theater than function.
 *
 * PERSPECTIVAL GAP:
 *   The prospectival structure is diagnostically clear: those who benefit (prolific researchers, high-impact journals, funding agencies rewarding novelty) classify the constraint as coordination (Rope). Those who are harmed (failed replicators, the empirical literature, early-career researchers outside elite labs) classify it as extraction (Snare). Those with agency to change it (open science coalition, forward-thinking institutions) see it as a temporary problem in transition (Scaffold). The field as a whole experiences it as mixed — gaining coordination benefits (incentive to do rigorous work, prioritization of surprising findings) but paying extraction costs (false positives, suppressed replications, degraded evidence base). The analytical observer's temptation to call it Mountain (inherent to how peer review works) is revealed as naturalization — publication bias is not inherent to peer review, it is a specific feature of journal-based publishing coupled with impact-factor-driven career incentives. Alternative structures (preregistration, registered reports, preprint servers) demonstrably reduce the bias, proving it is contingent, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is derived from the base extractiveness (0.58) scaled by the agent's directionality factor f(d). Beneficiaries with arbitrage exit experience negative or minimal chi — the constraint subsidizes them. Victims with trapped exit experience maximum chi — the constraint extracts from them. The modulation reflects that the same structural constraint (publication bias) is experienced as helpful coordination by some agents and as impossible barrier by others. The open science coalition's lower experienced extraction reflects their organized power and real exit options (building alternative venues), even though the base constraint remains severe for individual researchers still within traditional publishing systems.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that publication bias is a genuine Tangled Rope — it combines coordination function (prioritizing scarce publication slots, incentivizing rigorous work) with asymmetric extraction (penalizing null results, biasing the literature). The temptation to classify it as pure Snare (extraction only) ignores the coordination benefits journals and researchers genuinely receive. The temptation to classify it as pure Rope (coordination only) ignores the suppression and asymmetry. The Tangled Rope classification is correct because both functions are structurally present: the journal system does coordinate research dissemination AND does extract disproportionately from unsuccessful replication researchers. The open science reforms (preregistration, registered reports, open venues) are beginning to reduce the asymmetry by lowering barriers to null result publication and removing researcher degrees of freedom, gradually shifting the constraint from Tangled Rope toward Rope. This shift is measurable in the mild extraction decline at T=30 (0.58 down from 0.62 peak), reflecting early reform impact. The mandatrophy is resolved by acknowledging that both classification tensions (coordination vs extraction, institutional necessity vs arbitrary extraction) are real, and that the movement toward open science is not removing the coordination function but is removing the asymmetric extraction — shifting the constraint closer to pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_effect_size_detectability,
    'How much of the apparent ''bias toward positive results'' reflects genuine lower statistical power for null studies vs. actual editorial/reviewer bias against null findings?',
    'Comparison of effect sizes across published vs unpublished studies; analysis of publication bias in preregistered studies where outcomes cannot be cherry-picked; meta-analytic correction for selection effects',
    'If primarily power issue: suppression metric should be lower (0.40), classification may shift to Rope from some perspectives. If primarily editorial bias: suppression confirmed near 0.62, tangled rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_effect_size_detectability, empirical, 'Whether publication bias is driven by statistical power or editorial gatekeeping').

omega_variable(
    preregistration_adoption_barrier,
    'Does preregistration adoption face fundamental barriers (cost, complexity, discipline heterogeneity) that prevent it from replacing traditional journal submission?',
    'Longitudinal tracking of preregistration rates across psychology subdisciplines; analysis of adoption barriers in low-power research groups; comparison of preregistered vs traditional publication timelines',
    'If barriers are surmountable: open science scaffold is real and sunset is achievable within 15-25 years. If barriers are fundamental: scaffold perspective is aspirational, publication bias remains structural long-term.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preregistration_adoption_barrier, empirical, 'Whether preregistration can scale as alternative to traditional publishing').

omega_variable(
    incentive_alignment_in_institutions,
    'Can academic institutions shift hiring/promotion criteria away from impact-factor-weighted publication counts while maintaining quality gatekeeping?',
    'Survey of institutional hiring policies; longitudinal analysis of researcher quality metrics in institutions that have de-emphasized impact factors; economic modeling of institutional incentive structures',
    'If institutions can shift: open science sunset becomes credible, scaffold and piton perspectives confirmed. If institutional lock-in persists: publication bias becomes harder to disrupt at individual researcher level even as norms change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_alignment_in_institutions, preference, 'Whether institutions can restructure hiring incentives away from publication proxies').

omega_variable(
    null_result_quality_heterogeneity,
    'Are null results systematically lower in research quality than positive results, or is the quality distribution identical across publication bias outcomes?',
    'Comparison of preregistered null vs positive results using standardized quality metrics (sample size, effect size precision, methodology rigor); longitudinal tracking of null result replicability',
    'If null results are systematically lower quality: some degree of publication bias may serve legitimate gatekeeping function. If quality distribution is identical: bias is purely extractive, no gatekeeping benefit exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(null_result_quality_heterogeneity, empirical, 'Whether null results differ systematically in research quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publication_bias_psychology, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubbias_tr_t0, publication_bias_psychology, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pubbias_tr_t10, publication_bias_psychology, theater_ratio, 10, 0.62).
narrative_ontology:measurement(pubbias_tr_t20, publication_bias_psychology, theater_ratio, 20, 0.68).
narrative_ontology:measurement(pubbias_tr_t30, publication_bias_psychology, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(pubbias_be_t0, publication_bias_psychology, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pubbias_be_t10, publication_bias_psychology, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pubbias_be_t20, publication_bias_psychology, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pubbias_be_t30, publication_bias_psychology, base_extractiveness, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publication_bias_psychology, information_standard).
narrative_ontology:boltzmann_floor_override(publication_bias_psychology, 0.1).
narrative_ontology:affects_constraint(publication_bias_psychology, replication_crisis_psychology).
narrative_ontology:affects_constraint(publication_bias_psychology, researcher_incentive_misalignment).
narrative_ontology:affects_constraint(publication_bias_psychology, false_positive_accumulation).

% DUAL FORMULATION NOTE:
% Publication bias in psychology is a constraint family with three distinct stories: (1) journal gatekeeping mechanism (publication_bias_psychology, this file, ε≈0.58, Tangled Rope), (2) individual researcher career incentive structure (researcher_incentive_misalignment, ε≈0.65, Snare), (3) false positive literature accumulation (false_positive_accumulation, ε≈0.72, Snare). Each story has distinct ε values because they measure the constraint through different observables: journal-level coordination, individual-level extraction, and field-level epistemic cost. Decomposition required because ε differs by 0.14 between lowest and highest stories. Linked via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publication_bias_psychology, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
