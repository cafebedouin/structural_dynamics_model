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
 *   constraint_id: academic_citation_metrics_as_career_incentive
 *   human_readable: Academic Citation Metrics as Career Incentive
 *   domain: academic/institutional/scientific_governance
 *
 * SUMMARY:
 *   Citation metrics began as a neutral tool for measuring research
 *   visibility and estimating research impact. Over the past three decades,
 *   they have been weaponized as the primary mechanism for institutional
 *   evaluation, career advancement, and funding allocation. This
 *   transformation created a tangled structure: citation counting genuinely
 *   solves a coordination problem (how to measure research visibility across
 *   dispersed communities), but the same mechanism has become an extraction
 *   system in which researchers optimize for metrics rather than intellectual
 *   merit. The constraint exhibits all characteristics of a Tangled Rope: it
 *   possesses genuine coordination function (researchers genuinely benefit
 *   from knowing which work is widely read and built upon), active
 *   enforcement (hiring committees explicitly gate decisions on h-index and
 *   impact factor), asymmetric extraction (early-career researchers face
 *   coercive pressure while established researchers benefit from accumulated
 *   citation advantage), and structural beneficiaries (citation platforms,
 *   elite institutions, and high-citation researchers). The theater ratio has
 *   increased dramatically as the performance of metrics-chasing (publishing
 *   in high-impact journals, self-citation strategies, citation cartel
 *   formation) has eclipsed the actual evaluation of research quality.
 *   Suppression has intensified: alternative pathways (interdisciplinary
 *   work, methodological rigor over novelty, long-term projects with delayed
 *   payoff) have become structurally penalized.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victim (powerless/trapped) — must optimize for citations to advance; no exit option without abandoning career
 *   - Mid-Career Researchers in Citation-Intensive Fields: Secondary victim (moderate/constrained) — experience mixed extraction and coordination; have some agency but face significant costs to resisting metrics
 *   - Field Epistemic Integrity: Collective victim (abstract, powerless/trapped) — cannot organize or exit; bears cost of incentive-driven research corruption
 *   - Citation Platforms (Web of Science, Scopus): Primary beneficiary (institutional/arbitrage) — profit from institutional subscriptions and market expansion without bearing evaluation costs
 *   - Elite Institutions: Secondary beneficiary (institutional/constrained) — benefit from concentration of high-citation researchers; constrained by peer competition and prestige lock-in
 *   - Non-Citation-Intensive Disciplines: Secondary victim (moderate/constrained) — forced into alien evaluation frameworks; suppression is severe but not total
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — degraded function maintained through inertia; continued performance of legitimacy despite shift to metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_citation_metrics_as_career_incentive, 0.58).
domain_priors:suppression_score(academic_citation_metrics_as_career_incentive, 0.62).
domain_priors:theater_ratio(academic_citation_metrics_as_career_incentive, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, extractiveness, 0.58).
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(academic_citation_metrics_as_career_incentive, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_citation_metrics_as_career_incentive, tangled_rope).
narrative_ontology:human_readable(academic_citation_metrics_as_career_incentive, "Academic Citation Metrics as Career Incentive").
narrative_ontology:topic_domain(academic_citation_metrics_as_career_incentive, "academic/institutional/scientific_governance").

domain_priors:requires_active_enforcement(academic_citation_metrics_as_career_incentive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, high_citation_researchers).
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, citation_tracking_platforms).
narrative_ontology:constraint_beneficiary(academic_citation_metrics_as_career_incentive, elite_institutions).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, field_epistemic_integrity).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, early_career_researchers).
narrative_ontology:constraint_victim(academic_citation_metrics_as_career_incentive, non_citation_intensive_disciplines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Structurally trapped. Must pursue citations to advance; cannot exit academia without abandoning career investment. Suppression is total: hiring committees filter on h-index, tenure depends on citation count, grant funding correlates with citation metrics. No alternative pathway exists. Maximum extraction — the researcher's intellectual labor is redirected toward citation-gaming rather than research quality.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER RESEARCHER (TANGLED ROPE) — Constrained by established citation advantage but benefits from the metric system's coordination function: citations enable collaborative visibility, funding access, and legitimate reputation building. Has some agency (can resist citation-gaming or move to less metric-dependent institutions), but costs are significant (lower status, reduced funding, delayed advancement). Mixed experience: coordination plus extraction.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CITATION PLATFORM (ROPE) — Benefits from the system's expansion without bearing costs. Experiences citation metrics as pure coordination: platforms provide a genuine service (measuring research visibility and impact). Extraction runs toward the platform through licensing fees, institutional subscriptions, and market power. For the platform, this is coordination with positive asymmetry — they profit from solving a real information problem.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FIELD EPISTEMIC INTEGRITY (SNARE) — Structurally victimized. Citation metrics create perverse incentives: sensationalism, questionable research practices, citation cartels, and p-hacking all increase citations without improving knowledge. Cannot organize or exit. Suppression is severe: institutional structures have hardened around citation optimization, making reversal costly. Field quality declines despite (or because of) increased citation activity.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ELITE INSTITUTION (TANGLED ROPE) — Benefits from citation metrics (selective hiring of high-citation researchers concentrates prestige), but also benefits from the coordination function (citations legitimize hiring and funding decisions). Constrained by institutional inertia and peer competition — cannot easily exit citation metrics without losing competitive position relative to peer institutions. Active enforcement required to maintain the system as selection criteria.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PEER REVIEW SYSTEM (PITON) — Citation metrics were meant to supplement peer review evaluation; instead, they have partially replaced it. The peer review system persists through institutional inertia (hiring committees still use narrative letters, but weight them against h-index), but its primary function has degraded. Theater ratio is high: peer review continues performing legitimacy even as evaluation has shifted to metrics. The review system is maintained not because it works but because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, measuring research impact is inherent to science's quality control: some mechanism must exist to distinguish important from trivial work. Citation counting appears immutable — absent an alternative metric, institutions must measure something. However, this naturalizes a contingent choice (counting citations) as inevitable. The engine's false summit detector will reveal the beneficiary structure (citation platforms, elite institutions, high-citation researchers) as the source of the mountain framing.
constraint_indexing:constraint_classification(academic_citation_metrics_as_career_incentive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.58): High-moderate. The measurement interval shows clear escalation from 0.15 (time 0, when citations were one factor among many) to 0.58 (contemporary, when citations dominate career decisions). The current value reflects that citation optimization imposes significant intellectual labor costs and distorts research priorities. However, extractiveness is not maximal (0.80+) because genuine coordination function persists: citations do measure visibility, and visibility is a real research asset. The mixed character (coordination plus extraction) places this in Tangled Rope territory. Suppression (0.62): High. Multiple barriers prevent exit: (1) institutional lock-in — hiring committees use metrics as de facto requirement, (2) funding structure — grant agencies weight citations in peer review, (3) disciplinary norms — colleagues internalize citation importance, (4) employment alternatives — industry/non-academic careers also credential-check on h-index. Suppression has increased over the interval as metrics have hardened into institutional structure. Theater ratio (0.68): High and increasing. Much of contemporary academic evaluation is performative: journal impact factors do not correlate with individual paper quality, citation counts can be inflated through self-citation and citation cartels, and research impact on real-world problems is often uncorrelated with citations. The theater has increased as more actors have learned to game the system.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between beneficiaries' experience (coordination) and victims' experience (extraction). High-citation researchers in well-funded fields see the constraint as legitimate signal of impactful work; early-career researchers see it as coercive optimization divorced from research quality. Citation platforms see themselves as solving a coordination problem; fields see themselves as corrupted by metrics. The mountain classification (natural law) is a false summit because citation metrics are contingent institutional artifacts. Alternative evaluation systems (peer evaluation, pre-registration, open review, replication metrics) would shift the classification entirely. The beneficiary structure (platforms, elite institutions, high-citation researchers) sustains the naturalizing narrative that citation counting is inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural relationship to the extraction flow. Early-career researchers are victims with no exit: d ≈ 0.95 (trapped exit → high f(d)). Citation platforms are beneficiaries with arbitrage exits: d ≈ 0.05 (arbitrage exit → negative f(d)). Elite institutions are beneficiaries but constrained by peer competition: d ≈ 0.25 (constrained exit → low f(d)). Field epistemic integrity is abstract collective with no exit: d ≈ 1.0 (trapped → maximum f(d)). These directionality values, combined with the spatial scope (global) and modest scope modifier (σ ≈ 1.2), produce the computed effective extractiveness χ = 0.58 × f(d) × 1.2 at different perspectives. The gap between base extractiveness (0.58) and the analytical perspective's computed χ (approximately 0.79 with analytical canonical d ≈ 0.73) shows how the universal/global scope amplifies the constraint's effective coercive force.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint genuinely coordinates (researchers benefit from knowing which work is widely read, institutions benefit from having some impact measure), AND it extracts asymmetrically (early-career researchers are forced to optimize for metrics; field quality is corrupted; intellectual labor is redirected). The mandatrophy resolves because both claims are true: the same structure solves a coordination problem AND enables extraction. The resolution is structural, not perspectival — the constraint is not 'really' a Rope or 'really' a Snare, but both simultaneously. The measurability of this dual character is itself the diagnostic: a pure Rope would see beneficiaries and victims experience similar classification (low/moderate extraction for all); a pure Snare would see primarily Snare classifications. Tangled Rope is defined by the persistent gap between beneficiary experience (Rope) and victim experience (Snare) at the same time and institution. This constraint instantiates that gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    citation_vs_quality_decoupling,
    'Are high-citation counts genuinely correlated with research quality and intellectual merit, or have they decoupled into an orthogonal measure of visibility and rhetorical appeal?',
    'Post-hoc analysis of retracted/corrected papers'' citation histories; correlation between citation count and replication success; comparison of pre/post citation explosion field quality metrics (error rates, methodology rigor)',
    'If coupled: citation metrics serve a genuine coordination function (Rope classification from all perspectives). If decoupled: metrics are purely extractive cover story (Snare/Tangled Rope). This resolves the mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citation_vs_quality_decoupling, empirical, 'Whether citation counts correlate with research quality or have become orthogonal').

omega_variable(
    alternative_evaluation_viability,
    'Could peer-based, open-evaluation, or pre-registered methodological assessment provide comparable or superior research quality signals without the citation-gaming incentives?',
    'Comparative trial of alternative evaluation systems (arXiv overlay journals, registered reports, post-publication peer review); measurement of research quality metrics across evaluation methodologies; adoption rate and institutional sustainability of alternatives',
    'If viable alternatives exist: the constraint is institutional lock-in (Scaffold with sunset clause possible). If alternatives prove inferior: citation metrics are the least-bad option (Rope classification gains legitimacy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_evaluation_viability, empirical, 'Whether viable alternative evaluation systems exist').

omega_variable(
    high_citation_researcher_actual_extraction,
    'Do high-citation researchers experience the citation system as extraction (coercive optimization they would exit if possible) or as legitimate reward for impactful work?',
    'Qualitative interviews with high-citation researchers; analysis of researcher choices when citation pressure is removed (sabbaticals, interdisciplinary pivots, exit to industry); comparison of research choices under citation incentives vs funding-body-neutral mechanisms',
    'If experienced as coercion: the system is more snare-like even for beneficiaries. If experienced as legitimate: beneficiary satisfaction suggests genuine coordination function and Rope classification is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_citation_researcher_actual_extraction, empirical, 'Whether high-citation researchers experience the system as coercive or legitimate').

omega_variable(
    disciplinary_variance_in_extractiveness,
    'Does extractiveness vary systematically by discipline? Do non-citation-intensive fields (philosophy, experimental engineering, clinical medicine) experience lower extraction or different constraint type?',
    'Cross-discipline survey of career advancement criteria; comparison of citation-independence in hiring, promotion, and funding across disciplines; analysis of whether alternative metrics (book chapters, experimental breakthroughs, clinical impact) substitute for citations',
    'If variance is high: this constraint may decompose into multiple discipline-specific stories with different ε values (ε-invariance principle). If variance is low: the constraint is institution-wide despite disciplinary resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_variance_in_extractiveness, empirical, 'Whether extractiveness varies significantly by academic discipline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_citation_metrics_as_career_incentive, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acit_tr_t0, academic_citation_metrics_as_career_incentive, theater_ratio, 0, 0.25).
narrative_ontology:measurement(acit_tr_t10, academic_citation_metrics_as_career_incentive, theater_ratio, 10, 0.42).
narrative_ontology:measurement(acit_tr_t20, academic_citation_metrics_as_career_incentive, theater_ratio, 20, 0.65).
narrative_ontology:measurement(acit_tr_t30, academic_citation_metrics_as_career_incentive, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(acit_be_t0, academic_citation_metrics_as_career_incentive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(acit_be_t10, academic_citation_metrics_as_career_incentive, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(acit_be_t20, academic_citation_metrics_as_career_incentive, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(acit_be_t30, academic_citation_metrics_as_career_incentive, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acit_su_t0, academic_citation_metrics_as_career_incentive, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(acit_su_t15, academic_citation_metrics_as_career_incentive, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(acit_su_t30, academic_citation_metrics_as_career_incentive, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_citation_metrics_as_career_incentive, information_standard).
narrative_ontology:boltzmann_floor_override(academic_citation_metrics_as_career_incentive, 0.08).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, journal_impact_factor_gatekeeping).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, p_hacking_incentive_structure).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, replication_crisis_incentive_misalignment).
narrative_ontology:affects_constraint(academic_citation_metrics_as_career_incentive, elite_institution_prestige_concentration).

% DUAL FORMULATION NOTE:
% The citation metric constraint family decomposes into distinct constraint stories by observable: (1) citation_metrics_as_visibility_measure (ε ≈ 0.15, Rope) — citations genuinely track research visibility; (2) citation_metrics_as_career_incentive (ε ≈ 0.58, Tangled Rope) — metrics optimized for career advancement; (3) citation_metrics_as_research_quality_signal (ε ≈ 0.72, Snare) — claimed to measure research quality but decoupled from actual quality. These are three distinct constraints with different ε values. The story here focuses on the career incentive observable. The upstream visibility measure is foundational; the downstream quality-signal claim builds on the career incentive logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_citation_metrics_as_career_incentive, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
