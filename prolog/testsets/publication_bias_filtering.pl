% ============================================================================
% CONSTRAINT STORY: publication_bias_filtering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publication_bias_filtering, []).

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
 *   constraint_id: publication_bias_filtering
 *   human_readable: Publication Bias Filtering in Scientific Knowledge Production
 *   domain: scientific_publishing/epistemology
 *
 * SUMMARY:
 *   Publication bias filtering is the systematic filtering of research
 *   findings toward statistically significant or novel results, suppressing
 *   null results, failed replications, and contradictory evidence. This
 *   constraint operates at the intersection of scientific practice,
 *   institutional incentives, and epistemic authority. The filtering
 *   mechanism produces genuine coordination benefits (journals curating
 *   interesting findings, researchers focusing effort on promising
 *   directions) while simultaneously extracting from agents whose research
 *   contradicts positive-result bias. The extractiveness has increased over
 *   the interval as research complexity has outpaced peer review capacity and
 *   as publication metrics (impact factor, citation counts) have become more
 *   tightly coupled to career advancement. The theater ratio reflects that
 *   peer review for bias detection is substantially performative — reviewers
 *   assess novelty and plausibility but lack access to raw data, study
 *   protocols, and null results needed to detect selective reporting. The
 *   constraint exhibits all eight perspectives, with the critical
 *   perspectival gap between the journal editorial establishment (rope — the
 *   filtering mechanism enables their coordination function) and null-result
 *   researchers (snare — the same mechanism extracts from them by erasing
 *   valid research). The open science movement's scaffold perspective is
 *   conditional on successfully building alternative publication pathways
 *   (preprints, registered reports, open data) that bypass traditional
 *   journal gatekeeping — a sunset is achievable but requires sustained
 *   institutional change over 15-25 years.
 *
 * KEY AGENTS:
 *   - Null-Result Researchers: Primary victims (powerless/trapped) — career penalties for publishing non-significant findings; no exit route from publication-dependent career systems
 *   - Field Empirical Accuracy: Primary victim (powerless/trapped) — abstract collective good bearing full extraction cost through biased literature base
 *   - Journal Editorial Establishment: Primary beneficiary (institutional/arbitrage) — gatekeeping authority amplified by positive-result bias; maintains institutional prestige through curated selection
 *   - Successful Researchers with Positive Results: Secondary beneficiary (institutional/arbitrage) — citation advantage and funding concentration during positive-result publication windows
 *   - Replication Consortiums: Secondary victim (moderate/constrained) — face barriers to publishing replication and null results but have organized capacity to build alternative infrastructure
 *   - Open Science Movement: Institutional actor (organized/constrained) — building scaffolding (preprints, registered reports, open access) to bypass traditional gatekeeping with defined sunset
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative filtering ritual; benefits from continued centrality while function degrades
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent limits of knowledge production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publication_bias_filtering, 0.52).
domain_priors:suppression_score(publication_bias_filtering, 0.65).
domain_priors:theater_ratio(publication_bias_filtering, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publication_bias_filtering, extractiveness, 0.52).
narrative_ontology:constraint_metric(publication_bias_filtering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(publication_bias_filtering, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publication_bias_filtering, tangled_rope).
narrative_ontology:human_readable(publication_bias_filtering, "Publication Bias Filtering in Scientific Knowledge Production").
narrative_ontology:topic_domain(publication_bias_filtering, "scientific_publishing/epistemology").

domain_priors:requires_active_enforcement(publication_bias_filtering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publication_bias_filtering, journal_gatekeepers).
narrative_ontology:constraint_beneficiary(publication_bias_filtering, successful_researchers).
narrative_ontology:constraint_victim(publication_bias_filtering, null_result_researchers).
narrative_ontology:constraint_victim(publication_bias_filtering, field_empirical_accuracy).
narrative_ontology:constraint_victim(publication_bias_filtering, replication_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NULL-RESULT RESEARCHER (SNARE) — Structurally trapped by career incentives and publication gatekeeping. Cannot exit the field without career penalty; cannot publish findings that contradict positive-result bias; bears full extraction cost without escape route. Maximum experienced suppression — the constraint systematically erases their valid research from the epistemic record.
constraint_indexing:constraint_classification(publication_bias_filtering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EMPIRICAL ACCURACY (SNARE) — Abstract collective good that cannot organize or advocate. Bears the cost of biased literature base. No exit option — the epistemic commons is trapped by the filtering mechanism. Permanent asymmetric extraction from the knowledge base itself.
constraint_indexing:constraint_classification(publication_bias_filtering, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REPLICATION CONSORTIUM (TANGLED ROPE) — Constrained by publication barriers and resource requirements for replication studies, but also benefits from metadata standardization and preprint infrastructure that enables detection of bias. Significant extraction but not maximal — agents have some agency through organized replication networks and alternative publication venues.
constraint_indexing:constraint_classification(publication_bias_filtering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUCCESSFUL RESEARCHER (ROPE) — Benefits from bias filtering that amplifies positive results and creates citation advantage. Experiences the constraint as enabling coordination: publishing success encourages further research and attracts funding. Net beneficiary — the filtering mechanism runs toward this agent.
constraint_indexing:constraint_classification(publication_bias_filtering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL EDITORIAL ESTABLISHMENT (ROPE) — Benefits from positive-result bias that sustains reader interest, citation metrics, and institutional prestige. Experiences filtering as coordination mechanism: curating 'significant' findings maintains journal impact factor and funding. Net beneficiary with low extraction cost — editorial gatekeeping is their core function.
constraint_indexing:constraint_classification(publication_bias_filtering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, open-access journals, registered reports, meta-science initiatives) are building alternative publication pathways with lower bias thresholds. See bias filtering as a temporary institutional arrangement with a sunset: preprints, open data, registered reports, and meta-analyses create parallel verification ecosystems that bypass traditional journal gatekeeping. Estimated sunset: 15-25 years as norms mature.
constraint_indexing:constraint_classification(publication_bias_filtering, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PEER REVIEW THEATER (PITON) — The peer review process for filtering bias is substantially performative: reviewers assess novelty and plausibility but cannot verify data quality or detect selective reporting without access to raw data. Theater ratio (0.58) reflects significant performative content — the review ritual persists through institutional inertia despite well-documented limitations in detecting bias. Theater has increased as research complexity outpaced reviewer capacity.
constraint_indexing:constraint_classification(publication_bias_filtering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some publication bias is structurally inevitable: editors must select among submissions, positive results are inherently more publishable (Bayesian priors), and resource-constrained peer review cannot verify all claims. This perspective sees bias filtering as an immutable consequence of information scarcity. However, the structural data contradicts this naturalization — publication bias is not a natural law but a contingent institutional arrangement.
constraint_indexing:constraint_classification(publication_bias_filtering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publication_bias_filtering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publication_bias_filtering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publication_bias_filtering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publication_bias_filtering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publication_bias_filtering, TR),
    TR >= 0.70.

:- end_tests(publication_bias_filtering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. The filtering mechanism extracts from null-result researchers through career penalty and publication suppression, while benefiting journal editors and successful researchers through gatekeeping authority and citation amplification. The extraction is not as severe as a pure snare (0.72) because some benefits accrue to all researchers during successful publication, and the constraint has genuine coordination function (curating interesting findings). However, extractiveness has increased substantially over the interval (from 0.28 to 0.52) as publication metrics have become tightly coupled to career outcomes and as research complexity has exceeded peer review capacity. Suppression (0.65): High. Barriers to null-result publication include editorial bias against non-significant findings, reviewer skepticism of negative results, career risk from publishing contradictory evidence, and resource concentration in positive-result research. But suppression is not total — some null results are published in specialized journals and preprints, and organized replication networks are reducing barriers. Theater ratio (0.58): Moderate-high. Peer review for bias detection is substantially performative — reviewers evaluate novelty and plausibility but cannot verify data quality, detect selective reporting, or access unpublished findings without raw data access and study protocols. The theater has increased over the interval (from 0.42 to 0.58) as editorial specialization has declined and as publication volume has exceeded reviewer capacity. The remaining 0.42 of theater ratio reflects genuine coordination value: journals do curate novel and interesting findings, which has social value even if imperfect.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark between institutional beneficiaries and trapped victims. Journal editors see rope (the filtering mechanism enables their coordination function and maintains their authority). Successful researchers see rope (positive-result bias amplifies their citations and funding). Null-result researchers see snare (the same mechanism extracts from them through career penalty and publication suppression). Field empirical accuracy sees snare (the biased literature base permanently constrains knowledge). Replication consortiums see tangled rope (they bear extraction costs through publication barriers but gain benefits from infrastructure and methodology development). The open-science movement sees scaffold (they see the current arrangement as temporary, with sunset approaching as preprints and registered reports mature). The peer review system sees itself as piton (reviewers recognize their process as degraded — unable to detect bias reliably — but it persists through institutional inertia). The analytical observer risks seeing mountain (naturalizing publication bias as an inevitable consequence of information scarcity), but the structural data reveals this as a false summit: bias is a contingent institutional arrangement, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across perspectives. Null-result researchers occupy high d (0.95–1.0, full target): they are victims with trapped exit options, experiencing maximum effective extraction through career penalties and publication barriers. Journal editors occupy low d (0.05–0.15, full beneficiary): they are gatekeepers with arbitrage exit options, benefiting from authority centralization and citation metrics. Replication consortiums occupy mid-range d (0.50–0.65): they are constrained but organized, bearing extraction through barriers to publishing replication studies while gaining benefits from methodology development and infrastructure access. The open-science coalition occupies a different dspace (0.45–0.55): they are organized actors with constrained exit, seeing the current filtering mechanism as temporary and working to build alternatives. The engine derives these d values from beneficiary/victim declarations plus exit options: beneficiaries with arbitrage get low d → negative χ; victims with trapped exit get high d → high χ; constrained agents occupy the middle.
 *
 * MANDATROPHY ANALYSIS:
 *   Publication bias filtering resolves the mandatrophy by showing that tangled rope is the primary classification: the constraint has genuine coordination function (curating novel findings) alongside asymmetric extraction (suppressing null results). Both elements are structural and irreducible. The snare perspectives (null researchers, epistemic accuracy) are genuine experiences of trapped agents under the constraint; the rope perspectives (editors, successful researchers) are genuine experiences of beneficiaries. The scaffold and piton perspectives identify real structural transitions: open-science movement is building alternative pathways with defined sunset; peer review is degraded and maintained through inertia. The mountain perspective is a false summit — the naturalizing move that must be recognized as such. No single type is 'correct' — the presheaf over observation positions reveals that institutional beneficiaries and trapped victims experience the same constraint as fundamentally different structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    null_hypothesis_significance_threshold,
    'What statistical threshold distinguishes legitimate selectivity toward significant findings from extractive suppression of valid null results?',
    'Meta-analysis of published vs unpublished null results; comparison of effect sizes in published literature vs registered study protocols; reanalysis controlling for missing data',
    'If threshold is low (p < 0.01): most null results are legitimately non-significant and bias filtering may be appropriate. If threshold is high (p < 0.25): many valid null results are suppressed, revealing extraction rather than coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(null_hypothesis_significance_threshold, empirical, 'Statistical threshold for distinguishing selectivity from suppression').

omega_variable(
    preprint_to_journal_translation_bias,
    'Does the shift from preprint to journal publication introduce additional bias filtering beyond legitimate peer review?',
    'Comparison of findings in preprints vs final journal versions; analysis of editorial desk-reject rates by result direction; tracking of effect sizes and statistical significance across publication stages',
    'If minimal translation bias: preprints provide adequate alternative pathway (scaffold hypothesis confirmed). If substantial: bias is institutionally entrenched beyond peer review (snare extraction deeper).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_to_journal_translation_bias, empirical, 'Whether journal publication adds bias beyond peer review').

omega_variable(
    researcher_strategic_publication_behavior,
    'Do researchers strategically withhold null results, or does bias filtering suppress results researchers would have published?',
    'Survey of researcher publication intentions vs outcomes; analysis of file-drawer hypothesis through comparison of methodologically sound unpublished vs published studies; tracking of researchers who publish primarily null results',
    'If strategic: bias is coordination incentive (rope for successful researchers). If suppression: bias is extraction mechanism (snare for null researchers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(researcher_strategic_publication_behavior, empirical, 'Whether null suppression is researcher choice or institutional filtering').

omega_variable(
    field_recovery_trajectory,
    'How quickly can fields correct bias-distorted literature when open-science practices enable identification of suppressed results?',
    'Longitudinal tracking of fields adopting preprints/registered reports; measurement of literature correction rates; comparison of effect-size trajectories before/after open-science adoption',
    'If recovery is rapid (< 5 years): scaffold sunset is realistic. If slow (> 20 years): institutional inertia is stronger than structural change mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(field_recovery_trajectory, empirical, 'Speed of field correction when bias becomes visible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publication_bias_filtering, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubias_tr_t0, publication_bias_filtering, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pubias_tr_t10, publication_bias_filtering, theater_ratio, 10, 0.52).
narrative_ontology:measurement(pubias_tr_t20, publication_bias_filtering, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(pubias_be_t0, publication_bias_filtering, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pubias_be_t10, publication_bias_filtering, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(pubias_be_t20, publication_bias_filtering, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publication_bias_filtering, information_standard).
narrative_ontology:affects_constraint(publication_bias_filtering, file_drawer_effect).
narrative_ontology:affects_constraint(publication_bias_filtering, citation_metric_gaming).
narrative_ontology:affects_constraint(publication_bias_filtering, peer_review_capacity_bottleneck).

% DUAL FORMULATION NOTE:
% Publication bias filtering is downstream of several related constraints: file-drawer effect (researcher choice to withhold null results), citation metric gaming (institutional incentive to publish high-impact results), and peer-review capacity bottleneck (structural inability to verify claims). Each has distinct extractiveness and should be modeled as a separate story. Publication bias filtering represents the institutional mechanism that aggregates these three constraints into a system-level filtering operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
