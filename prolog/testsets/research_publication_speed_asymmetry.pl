% ============================================================================
% CONSTRAINT STORY: research_publication_speed_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_research_publication_speed_asymmetry, []).

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
 *   constraint_id: research_publication_speed_asymmetry
 *   human_readable: Research Publication Speed Asymmetry
 *   domain: academic/publishing
 *
 * SUMMARY:
 *   The research publication speed asymmetry creates a structural extraction
 *   mechanism where publication velocity determines career viability, but
 *   velocity is asymmetrically accessible based on institutional resources
 *   and funding. The same research claim faces different publication
 *   timelines (and thus different career value) depending on the originating
 *   group's ability to afford open access fees, preprint marketing, editorial
 *   expediting, and rapid submission cycles. This constraint exhibits
 *   coordination (maintaining research quality standards and visibility)
 *   alongside extraction (speed as a positional good creating artificial
 *   scarcity). The theater ratio has increased over the interval as
 *   traditional peer review persists in justifying delay through quality
 *   claims while actual quality control remains weak. Preprint servers and
 *   open science movements are building exit paths but have not yet achieved
 *   sufficient credential weight to replace journal prestige as a
 *   hiring/funding signal.
 *
 * KEY AGENTS:
 *   - Early Career Researcher: Primary victim (powerless/trapped) — career trajectory depends on publication speed, cannot afford to accelerate publication, no exit option without abandoning field
 *   - Under-Resourced Research Groups: Secondary victim (moderate/constrained) — face resource barriers to rapid publication but also benefit from quality gatekeeping that prevents low-credibility work from entering field
 *   - Well-Funded Research Institutions: Primary beneficiary (institutional/arbitrage) — can afford open access fees and rapid submission cycles; benefit from prestige associated with publication speed
 *   - Commercial Publishers: Primary beneficiary (institutional/arbitrage) — profit from open access fee arbitrage and prestige-based market segmentation; create and maintain speed asymmetries through service tiers
 *   - Open Science Movement: Organized agents (organized/mobile) — building preprint infrastructure and post-publication review systems that reduce speed asymmetry; have exit pathway via institutional bypassing
 *   - Traditional Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative quality control ritual; slows publication in proportion to perceived journal prestige
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent speed asymmetries as inherent research requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(research_publication_speed_asymmetry, 0.52).
domain_priors:suppression_score(research_publication_speed_asymmetry, 0.58).
domain_priors:theater_ratio(research_publication_speed_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(research_publication_speed_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(research_publication_speed_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(research_publication_speed_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(research_publication_speed_asymmetry, tangled_rope).
narrative_ontology:human_readable(research_publication_speed_asymmetry, "Research Publication Speed Asymmetry").
narrative_ontology:topic_domain(research_publication_speed_asymmetry, "academic/publishing").

domain_priors:requires_active_enforcement(research_publication_speed_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(research_publication_speed_asymmetry, high_resource_research_institutions).
narrative_ontology:constraint_beneficiary(research_publication_speed_asymmetry, well_funded_research_groups).
narrative_ontology:constraint_beneficiary(research_publication_speed_asymmetry, commercial_publishers).
narrative_ontology:constraint_victim(research_publication_speed_asymmetry, under_resourced_researchers).
narrative_ontology:constraint_victim(research_publication_speed_asymmetry, early_career_scientists).
narrative_ontology:constraint_victim(research_publication_speed_asymmetry, researchers_in_developing_economies).
narrative_ontology:constraint_victim(research_publication_speed_asymmetry, research_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Trapped in a system where publication speed determines career viability. Faces pressure to publish rapidly to secure positions, but lacks resources (funding, lab infrastructure, editorial connections) to accelerate publication. Cannot exit without abandoning career trajectory. Experiences maximum extraction through the asymmetry.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDER-RESOURCED RESEARCH GROUP (TANGLED ROPE) — Constrained by limited funding for open access fees, preprint servers, and publication marketing. Also benefits from peer review system's gatekeeping function (prevents low-quality work from flooding field) and coordination of research standards. Experiences both extraction and genuine coordination benefit.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELL-FUNDED RESEARCH INSTITUTION (ROPE) — Benefits from coordination mechanism that maintains quality standards and visibility hierarchies. Can afford open access fees, preprint marketing, and rapid submission cycles. Experiences the constraint as pure coordination: publication system enables their research visibility. Net beneficiary.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL PUBLISHER (ROPE) — Primary beneficiary. Sees publication speed asymmetry as coordination of research dissemination and quality control. The speed differential creates profitable market segmentation (regular journals, high-speed tracks, premium services). High margins through subscription and open access fee arbitrage.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized actors (preprint servers, open access mandates, rapid publication platforms) are building alternative publication pathways that reduce speed asymmetry. See the constraint as temporary and solvable through distributed preprints and post-publication peer review. Exit path is clear: arXiv, bioRxiv, medRxiv bypass traditional publication gatekeeping.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL PEER REVIEW SYSTEM (PITON) — Performative verification ritual maintaining institutional legitimacy long after its functional utility has eroded. Slow review cycles justified by quality concerns, but actual quality control is weak (high rejection often reflects editorial taste, not rigor). Theater persists through institutional inertia and prestige economics. Researchers continue submitting because journal brand name still signals credibility, though this is degrading.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal perspective, review and dissemination of research claims necessarily takes time — this is an immutable constraint of knowledge verification. Some speed asymmetry is inherent to any gating mechanism. However, the structural data contradicts pure mountain classification; the engine will identify this as false summit, revealing that the magnitude of the observed asymmetry (2-5 year publication delays varying by funding) exceeds what inherent verification requirements would predict.
constraint_indexing:constraint_classification(research_publication_speed_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(research_publication_speed_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(research_publication_speed_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(research_publication_speed_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(research_publication_speed_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(research_publication_speed_asymmetry, TR),
    TR >= 0.70.

:- end_tests(research_publication_speed_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The asymmetry extracts career value and research visibility from under-resourced researchers through a mechanism (speed-dependent prestige) that is not inherent to knowledge verification. Some delay for peer review is legitimate; the observed 2-5 year variance between institutions and funding levels suggests artificial speed constraints are layered onto legitimate verification requirements. The value reflects that the extraction is real and significant but not maximal—it operates through prestige scarcity rather than total exclusion. Suppression (0.58): Moderate-high. Barriers to rapid publication include: open access fees ($3,000-5,000 per article, prohibitive for under-resourced groups), lack of editorial connections for expedited review, limited infrastructure for manuscript preparation, and career incentive systems that still reward journal prestige. However, suppression is not absolute—preprints provide partial exit, and some journals have reduced review times. Theater ratio (0.65): Moderate-high. Peer review is increasingly performative in large-scale fields: reviewer expertise cannot match research specialization, review often reflects journal taste rather than rigor, rejection is uncorrelated with eventual citation impact. Theater has increased as field size has grown and journal prestige economics have intensified. The claimed quality control function persists through institutional inertia long after its functional utility has declined.
 *
 * PERSPECTIVAL GAP:
 *   The gap between early career (snare) and well-funded institutions (rope) is substantial—the same constraint creates opposite experiences. The early career researcher sees extraction; the well-funded institution sees coordination. This gap reveals that the constraint's classification depends entirely on structural position, not on objective properties. The piton classification of traditional peer review (degraded ritual) contrasts with the rope classification of publishers (genuine coordination of market segmentation), suggesting that different actors experience the same institutional apparatus differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Early career researchers in under-resourced groups are victims with trapped exit—maximum d. Well-funded institutions are beneficiaries with arbitrage exit—low d. Commercial publishers benefit from the speed asymmetry (they profit from open access fee segmentation) and have arbitrage exit—low d. The open science movement is organized and mobile—they have real exit options through preprint infrastructure and can opt out of traditional journal prestige. The traditional peer review system benefits from the prestige it creates (institutional legitimacy, high citation to highly-cited journals) and has arbitrage-like exit (journals can choose to accelerate or decelerate review).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is neither pure coordination (rope) nor pure extraction (snare). It has genuine coordination content—peer review does prevent low-quality work from dominating the literature, and publication standards do coordinate field norms. But it also has significant asymmetric extraction—the speed differential creates positional scarcity that benefits high-resource groups. The constraint cannot resolve into pure rope because the speed asymmetry is not a necessary coordination cost; preprint systems and post-publication review demonstrate that quality coordination can occur with lower speed variance. The constraint cannot resolve into pure snare because the peer review gatekeeping does provide legitimate value (prevents trivial claims from entering record). The mandatrophy is resolved by accepting that the constraint is genuinely hybrid: it coordinates field norms while extracting career value through speed asymmetry. The extraction is not incidental to coordination—it is profitable precisely because coordination creates prestige, and prestige is valuable, and speed determines prestige access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    publication_delay_threshold,
    'What publication delay reflects legitimate peer review requirements versus extractive gatekeeping?',
    'Comparative analysis of review times across journals, institutions, and funding levels; correlation between review duration and ultimate citation impact; historical trend analysis of review timelines',
    'If threshold is 6 months: many quality reviews classified as extraction. If threshold is 18 months: extractive delays persist unchallenged. Current median is 12-24 months in most fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_delay_threshold, empirical, 'Threshold distinguishing legitimate review delay from extractive gatekeeping').

omega_variable(
    resource_dependency_causality,
    'Does publication speed asymmetry cause research inequality or merely reflect pre-existing resource inequality?',
    'Longitudinal tracking of early career researchers with equivalent quality work but different initial resources; analysis of whether publication speed predicts career outcomes controlling for research quality',
    'If causal: publication asymmetry is an independent extraction mechanism. If merely reflective: the constraint is derivative (piton rather than snare). Most likely both effects are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_dependency_causality, empirical, 'Whether publication speed asymmetry causes or merely reflects resource inequality').

omega_variable(
    preprint_credibility_gap,
    'Do preprints establish sufficient credibility for hiring, funding, and career advancement decisions to enable meaningful exit from traditional publication gatekeeping?',
    'Survey of hiring committees, funding agencies, and promotion committees on preprint weight; analysis of career outcomes for researchers using preprint-first strategies; longitudinal career tracking',
    'If yes: scaffold exit path is real and functional. If no: preprints are performative (rope-like coordination without true exit). Current answer varies dramatically by field and institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preprint_credibility_gap, empirical, 'Whether preprints provide credible exit from journal-based gatekeeping').

omega_variable(
    reviewer_capacity_binding,
    'Is publication speed asymmetry driven by reviewer shortage (structural bottleneck) or by artificial supply constraints (journal prestige maximization)?',
    'Analysis of reviewer pool availability, time spent on reviews, and review cycle time across journals with different review models (traditional anonymous peer review vs post-publication review vs instant publication)',
    'If structural bottleneck: some asymmetry is unavoidable (mountain-like). If artificial constraint: asymmetry is pure extraction mechanism (snare-like). Journal economics suggests both are operating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reviewer_capacity_binding, empirical, 'Whether speed asymmetry reflects reviewer shortage or artificial prestige constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(research_publication_speed_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpsa_tr_t0, research_publication_speed_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(rpsa_tr_t10, research_publication_speed_asymmetry, theater_ratio, 10, 0.58).
narrative_ontology:measurement(rpsa_tr_t20, research_publication_speed_asymmetry, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(rpsa_be_t0, research_publication_speed_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rpsa_be_t10, research_publication_speed_asymmetry, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rpsa_be_t20, research_publication_speed_asymmetry, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(research_publication_speed_asymmetry, information_standard).
narrative_ontology:affects_constraint(research_publication_speed_asymmetry, research_inequality_perpetuation).
narrative_ontology:affects_constraint(research_publication_speed_asymmetry, early_career_career_precarity).
narrative_ontology:affects_constraint(research_publication_speed_asymmetry, journal_prestige_economics).

% DUAL FORMULATION NOTE:
% The publication speed asymmetry is upstream of several institutional constraints. The career precarity of early-career scientists is partially caused by speed asymmetry but has additional structural components (funding scarcity, position shortage). Journal prestige economics is both upstream (prestige drives willingness to pay for expedited review) and downstream (speed asymmetry reinforces prestige hierarchies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(research_publication_speed_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
