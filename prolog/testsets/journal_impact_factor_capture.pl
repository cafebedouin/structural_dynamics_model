% ============================================================================
% CONSTRAINT STORY: journal_impact_factor_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_journal_impact_factor_capture, []).

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
 *   constraint_id: journal_impact_factor_capture
 *   human_readable: Journal Impact Factor Gaming and Capture
 *   domain: academic_publishing/scientometrics
 *
 * SUMMARY:
 *   Journal impact factor capture represents a structural entanglement of
 *   career incentives, institutional rankings, and publisher market power
 *   that transforms a quality-assessment tool into an extraction mechanism.
 *   The constraint exhibits the full taxonomy of DR types depending on
 *   observer position: early-career researchers experience pure extraction
 *   (snare); research institutions experience mixed coordination-extraction
 *   (tangled rope); publishers experience coordination with arbitrary exit
 *   (rope); citation services maintain a degraded metric through inertia
 *   (piton); and the civilizational analytical perspective risks naturalizing
 *   a contingent institutional arrangement as an immutable law (false summit
 *   mountain). The extractiveness has risen from 0.32 to 0.58 over the
 *   interval as gaming strategies (self-citation cartels, review
 *   manipulation, predatory journals) have proliferated and institutional
 *   dependency has deepened. Theater ratio has risen from 0.42 to 0.68,
 *   reflecting increasing performative content relative to genuine quality
 *   assessment — the metric increasingly measures journal market position
 *   rather than research quality.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary victims (powerless/trapped) — cannot advance careers without publishing in high-impact venues; face maximum extraction and suppression
 *   - Marginal Subfield Researchers: Primary victims (powerless/trapped) — niche fields cannot access high-impact journal networks; bear extraction cost without corresponding benefit
 *   - Field Research Quality (Abstract Collective): Victim (powerless/trapped) — bears cost of optimization incentives that degrade methodology, replicate, and relevance; cannot organize or exit
 *   - Research Institutions: Secondary actors (moderate/constrained) — benefit from prestige metrics but constrained by need to manage researcher behavior; experience both coordination and extraction
 *   - Commercial Journal Publishers: Primary beneficiaries (institutional/arbitrage) — capture financial rents from metric-driven researcher behavior; maximum arbitrage exit options
 *   - Citation Tracking Services: Institutional maintainers (institutional/constrained) — maintain degraded metric system through inertia; constrained by institutional dependency but benefit from continued use
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as laws of scientific publishing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(journal_impact_factor_capture, 0.58).
domain_priors:suppression_score(journal_impact_factor_capture, 0.65).
domain_priors:theater_ratio(journal_impact_factor_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(journal_impact_factor_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(journal_impact_factor_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(journal_impact_factor_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(journal_impact_factor_capture, tangled_rope).
narrative_ontology:human_readable(journal_impact_factor_capture, "Journal Impact Factor Gaming and Capture").
narrative_ontology:topic_domain(journal_impact_factor_capture, "academic_publishing/scientometrics").

domain_priors:requires_active_enforcement(journal_impact_factor_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(journal_impact_factor_capture, high_impact_journals).
narrative_ontology:constraint_beneficiary(journal_impact_factor_capture, citation_cartels).
narrative_ontology:constraint_beneficiary(journal_impact_factor_capture, journal_publishers).
narrative_ontology:constraint_victim(journal_impact_factor_capture, field_research_quality).
narrative_ontology:constraint_victim(journal_impact_factor_capture, early_career_researchers).
narrative_ontology:constraint_victim(journal_impact_factor_capture, marginal_subfields).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Trapped by career metrics that require publications in high-impact journals. Cannot exit without abandoning academic advancement prospects. Impact factor becomes a mandatory extraction mechanism: researchers must direct their work toward journal prestige rather than field relevance. Maximum suppression from lack of alternatives for establishing legitimacy.
constraint_indexing:constraint_classification(journal_impact_factor_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINAL SUBFIELD RESEARCHER (SNARE) — Trapped by journal impact factor hierarchy. High-impact journals rarely publish work in specialized areas; alternatives (specialized journals) are viewed as lower-prestige. Researchers in niche fields bear extraction cost without corresponding career benefit. No exit path within traditional metrics system.
constraint_indexing:constraint_classification(journal_impact_factor_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (TANGLED ROPE) — Benefits from journal impact factor prestige (attracts funding, improves rankings) while constrained by need to manage researcher behavior to optimize for metrics. Experiences both coordination function (aggregating research quality signals) and asymmetric extraction (resources devoted to metrics gaming rather than research).
constraint_indexing:constraint_classification(journal_impact_factor_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMERCIAL JOURNAL PUBLISHER (ROPE) — Primary beneficiary with maximum arbitrage exit (can modify metrics, create new journals, exploit market position). Experiences impact factor as a coordination mechanism that enables market segmentation and pricing power. Low extraction cost; high benefit from metric-driven researcher behavior.
constraint_indexing:constraint_classification(journal_impact_factor_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION TRACKING SERVICES (PITON) — Maintain impact factor as a degraded measure despite known manipulation vulnerabilities. Theater ratio high because the metric is performative: impact factor predicts individual paper quality poorly, yet institutional decisions rest on it. Services continue maintaining metric through institutional inertia and dependency, not because it functions well.
constraint_indexing:constraint_classification(journal_impact_factor_capture, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risks naturalizing impact factor as an inevitable consequence of competitive academic publishing. Could frame the metric as an immutable law of how journals must differentiate themselves. However, structural data shows this is a contingent institutional arrangement, not a natural law — alternative metrics (article-level, domain-specific, open-access standards) demonstrate that different coordination mechanisms are possible.
constraint_indexing:constraint_classification(journal_impact_factor_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(journal_impact_factor_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(journal_impact_factor_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(journal_impact_factor_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(journal_impact_factor_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(journal_impact_factor_capture, TR),
    TR >= 0.70.

:- end_tests(journal_impact_factor_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant asymmetric extraction but not maximum. Publishers and institutions capture substantial rents through researcher behavior optimization, but the mechanism retains some coordination function — the metric does correlate imperfectly with research quality and journals do provide real editorial services. The extractiveness has increased from 0.32 as self-citation cartels and predatory journals have proliferated, showing degradation over time. Suppression (0.65): High. Early-career researchers face multiple suppressions: institutional reliance on metrics for hiring/promotion decisions, lack of viable alternatives for establishing legitimacy, journal acceptance barriers for non-prestigious venues, and funding agency metrics that reinforce impact factor dependency. Suppression has increased as metric dependency has deepened across institutional layers. Theater ratio (0.68): Moderate-high. Impact factor increasingly measures journal prestige and market position rather than individual paper quality. The performative content has risen as manipulation strategies have become mainstream. Citation manipulation by journals (self-citation cartels, editorial bias, predatory inclusion) contributes to theater. However, genuine editorial functions (peer review, curation, distribution) remain, preventing pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiaries and victims. Publishers and research institutions see coordination (rope or tangled rope): they solve a real problem of journal differentiation and research evaluation. Early-career and marginal-field researchers see pure extraction (snare): they face career penalties they cannot escape. Citation services see a degraded metric (piton): the tool works poorly but persists through inertia. The analytical observer risks seeing a natural law (mountain): 'competitive journals must differentiate, so impact factor is inevitable.' The structural data contradicts this — alternative metrics, open-access standards, and domain-specific evaluation systems demonstrate that different coordination mechanisms are possible. The gap reveals that impact factor capture is not a law of nature but a contingent institutional arrangement maintained by specific stakeholder interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agent types. Early-career researchers (powerless/trapped/victim) experience high d → high f(d) → high χ: they bear extraction cost with no exit options. Publishers (institutional/arbitrage/beneficiary) experience low d → negative f(d) → negative χ: they are beneficiaries with maximum exit options. Research institutions (moderate/constrained/both beneficiary and victim) experience intermediate d. The institutional power atom is essential here: it reverses the extraction direction compared to powerless agents at the same spatial scope. Research institutions benefit from prestige metrics (they use them for rankings) while simultaneously constraining their researchers (through hiring/promotion policies), creating asymmetric power distribution within each institution.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope, not pure snare: there IS a coordination function (journal quality differentiation, research evaluation signal) alongside extractive overlay (career penalty for publishing outside high-impact venues, self-citation gaming, predatory journals). The coordination function is real but asymmetrically distributed: benefits accrue to publishers and prestigious institutions; costs fall on early-career and marginal-field researchers. The theater ratio (0.68) captures the degradation: as manipulation has proliferated, the metric increasingly measures market position rather than research quality, but the metric persists because institutions remain locked into dependency. The false summit perspective (mountain) naturalizes this arrangement as inevitable, but the constraint decomposition shows it is contingent: removing institutional dependency on impact factor (through alternative evaluation frameworks, open-science adoption, or regulatory intervention) would change the classification toward rope or even dissolve the extractive component entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_citation_manipulation_detection,
    'What proportion of measured impact factor elevation derives from self-citation cartels versus genuine field prestige?',
    'Comparative analysis of self-citation rates across journals; tracking of manipulation penalties applied by citation services; longitudinal study of journal behavior before/after audit interventions',
    'If self-citation dominates: impact factor is primarily extractive theater (snare). If genuine prestige dominates: extractive component is moderate (tangled rope validated). Classification changes based on decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_citation_manipulation_detection, empirical, 'Proportion of impact factor driven by self-citation cartels').

omega_variable(
    institutional_metric_dependency_cascade,
    'How many layers of institutional decision-making (hiring, funding, promotion, resource allocation) depend on impact factor, creating irreversible path dependency?',
    'Audit of institutional policies; analysis of decision reversibility; cost assessment for removing metric from evaluation frameworks',
    'If dependency is one-layer reversible: metric could be abandoned with institutional will. If multi-layer irreversible: suppression is structural and very high; early-career researchers face compounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_metric_dependency_cascade, empirical, 'Degree of institutional dependency on impact factor metrics').

omega_variable(
    alternative_metric_readiness,
    'Do existing alternative metrics (article-level evaluations, domain-specific measures, open-access prestige) have sufficient maturity and institutional acceptance to serve as viable exits from impact factor capture?',
    'Analysis of alternative metric adoption rates; tracking of institutions that have successfully transitioned away from impact factor dependence; assessment of new-metric stability and gaming vulnerability',
    'If alternatives are mature: trapped exit should upgrade to constrained; suppression decreases. If alternatives are nascent: early career researchers remain trapped; suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_metric_readiness, empirical, 'Maturity and viability of alternative metrics').

omega_variable(
    field_research_quality_degradation_measurability,
    'Can research quality degradation caused by impact factor optimization be independently measured and distinguished from normal variation?',
    'Longitudinal analysis of research reproducibility, effect sizes, and methodological rigor; correlation with impact factor optimization incentives; comparison of trends across fields with different metric dependencies',
    'If measurable degradation confirmed: victim status of field epistemic quality is concrete, not speculative. If degradation cannot be isolated: constraint classification becomes more uncertain; omega uncertainty rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(field_research_quality_degradation_measurability, empirical, 'Measurability of research quality degradation from impact factor optimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(journal_impact_factor_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jif_tr_t0, journal_impact_factor_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jif_tr_t5, journal_impact_factor_capture, theater_ratio, 5, 0.55).
narrative_ontology:measurement(jif_tr_t10, journal_impact_factor_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(jif_be_t0, journal_impact_factor_capture, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(jif_be_t5, journal_impact_factor_capture, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(jif_be_t10, journal_impact_factor_capture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(journal_impact_factor_capture, information_standard).
narrative_ontology:affects_constraint(journal_impact_factor_capture, predatory_journal_ecosystem).
narrative_ontology:affects_constraint(journal_impact_factor_capture, citation_cartel_formation).
narrative_ontology:affects_constraint(journal_impact_factor_capture, research_reproducibility_crisis).

% DUAL FORMULATION NOTE:
% Journal impact factor capture is downstream of publisher market consolidation and upstream of research quality degradation. The constraint represents the institutional mechanism through which market power is converted into research behavior modification. See related constraints: predatory_journal_ecosystem (ε≈0.72, pure snare at research community level), citation_cartel_formation (ε≈0.65, tangled rope among publisher coordination), research_reproducibility_crisis (ε≈0.52, snare from field epistemic reliability perspective).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(journal_impact_factor_capture, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
