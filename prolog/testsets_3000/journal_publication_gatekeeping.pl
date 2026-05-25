% ============================================================================
% CONSTRAINT STORY: journal_publication_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_journal_publication_gatekeeping, []).

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
 *   constraint_id: journal_publication_gatekeeping
 *   human_readable: Journal Publication Gatekeeping in Academic Scholarship
 *   domain: academic/publishing
 *
 * SUMMARY:
 *   Journal publication gatekeeping represents a hybrid
 *   extraction-coordination mechanism that has grown increasingly extractive
 *   over the past two decades. Nominally, peer review serves the coordination
 *   function of quality filtering and error detection. Structurally, the
 *   gatekeeping mechanism concentrates publishing power in a small number of
 *   high-impact journals, enables rent extraction through subscription fees,
 *   creates artificial scarcity (limited journal slots), and embeds
 *   hierarchical discrimination based on institutional prestige and
 *   researcher demographics. Early-career researchers and scholars from
 *   marginalized institutions face the highest suppression: their career
 *   advancement depends on journal acceptance, they have fewer alternative
 *   pathways, and they face discrimination from reviewers and editors. The
 *   constraint exhibits all six classification types from different
 *   structural positions, with the critical distinction between those who can
 *   exit (beneficiaries with arbitrage, organized communities with
 *   alternative venues) and those who cannot (powerless trapped agents). The
 *   rising theater ratio (0.48 to 0.64 over the interval) reflects that
 *   gatekeeping has become increasingly performative: rejection letters cite
 *   'fit' and 'scope' more than substantive quality issues; revisions address
 *   reviewer preferences rather than genuine errors; prestige metrics
 *   (h-index, impact factor) become targets rather than indicators. The
 *   open-access and preprint movement represents genuine Scaffold logic:
 *   alternative verification pathways (arXiv comments, bioRxiv peer review,
 *   open-access journals) are being constructed as exits from traditional
 *   gatekeeping, with explicit sunset dynamics as these alternatives mature.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victim (powerless/trapped) — career dependent on journal acceptance; no practical exit; bears full cost of gatekeeping.
 *   - Marginalized Institution Researchers: Primary victim (powerless/trapped) — face double discrimination (institutional and credential-based); resource constraints limit alternative publishing; highest suppression and lowest exit capacity.
 *   - Established Publishers: Primary beneficiary (institutional/arbitrage) — capture subscription revenue, prestige, and citation metrics; experience constraint as pure coordination.
 *   - Peer Reviewer Hierarchy: Secondary beneficiary (powerful/arbitrage) — high-prestige reviewers gain power to shape research directions; can arbitrage review status for career benefits.
 *   - Research Communities: Mixed actor (organized/constrained) — benefit from coordination function but bear extraction through publish-or-perish incentives and research distortion.
 *   - Open Science Movement: Organized actors (organized/mobile) — building exit pathways through preprints, open access, alternative metrics; see sunset logic in traditional gatekeeping.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing gatekeeping as immutable quality control necessity rather than contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(journal_publication_gatekeeping, 0.58).
domain_priors:suppression_score(journal_publication_gatekeeping, 0.68).
domain_priors:theater_ratio(journal_publication_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(journal_publication_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(journal_publication_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(journal_publication_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(journal_publication_gatekeeping, tangled_rope).
narrative_ontology:human_readable(journal_publication_gatekeeping, "Journal Publication Gatekeeping in Academic Scholarship").
narrative_ontology:topic_domain(journal_publication_gatekeeping, "academic/publishing").

domain_priors:requires_active_enforcement(journal_publication_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(journal_publication_gatekeeping, established_publishers).
narrative_ontology:constraint_beneficiary(journal_publication_gatekeeping, elite_research_institutions).
narrative_ontology:constraint_beneficiary(journal_publication_gatekeeping, peer_reviewer_hierarchy).
narrative_ontology:constraint_victim(journal_publication_gatekeeping, early_career_researchers).
narrative_ontology:constraint_victim(journal_publication_gatekeeping, scholars_from_marginalized_institutions).
narrative_ontology:constraint_victim(journal_publication_gatekeeping, heterodox_research_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Career advancement (tenure, grants, employment) depends on publication in high-impact journals. Rejection by gatekeepers carries severe costs: lost funding, damaged reputation, delayed promotion. No practical exit: cannot publish elsewhere without career penalty. Cannot opt out: participation is mandatory for career survival. Maximum extraction.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED INSTITUTION RESEARCHER (SNARE) — Faces double gatekeeping: journal editors preferentially accept submissions from elite institutions; reviewers often assume lower-quality work from non-prestigious affiliations. Trapped by institutional resource constraints (cannot afford expensive journals, limited library access) and by credential discrimination. Exit paths blocked: alternative publishing venues carry lower prestige value, perpetuating institutional hierarchy.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH COMMUNITY (TANGLED ROPE) — Communities of researchers benefit from peer review's coordination function: quality filtering, error detection, standardization of methodology. But also bear extraction: publish-or-perish incentives distort research priorities toward fashionable topics, pressure toward positive results suppresses null findings, replication studies discouraged. Organized enough to advocate (open science movement, preprint servers) but constrained by career dependencies on journal prestige.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED PUBLISHERS (ROPE) — Capture value through subscription revenue, citation metrics, brand prestige. Journal prestige enables them to set submission fees, bundle journals into expensive packages, extract rent from institutional subscriptions. Experience the constraint as pure coordination: managing peer review, standardizing formats, enabling discovery. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ACCESS MOVEMENT (SCAFFOLD) — arXiv, bioRxiv, PLoS, and institutional repositories represent temporary scaffolding being constructed to bypass journal gatekeeping. These alternatives reduce suppression by enabling rapid dissemination without subscription paywalls. The movement has explicit sunset logic: as preprints become mainstream, as open-access publishing matures, and as hiring committees devalue traditional impact factor metrics, the traditional gatekeeping constraint loses enforcement power. Sunset estimated at 15-25 years depending on discipline.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW RITUAL (PITON) — The peer review process persists through institutional inertia despite documented limitations: reviewers are often unqualified for specialized work, reviews are inconsistent and sometimes hostile, review quality is unpaid and therefore variable. The ritual is maintained because alternatives haven't completely replaced it, not because it functions well. Theater ratio (0.64) reflects that much review activity is performative: authors revise papers to satisfy reviewer ego rather than to improve quality; editors accept reviews that validate their existing preferences. Degraded mechanism persists through habit.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — The constraint is often naturalized as an immutable property of knowledge creation: 'Quality control requires gatekeepers,' 'Peer review is the gold standard,' 'Not all work merits publication.' This framing makes the gatekeeping appear as a natural law rather than a contingent institutional arrangement. However, the structural data reveals this as a false summit: the gatekeeping mechanism is enforced through career incentives, institutional prestige hierarchies, and publisher rent-extraction, not through laws of nature.
constraint_indexing:constraint_classification(journal_publication_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(journal_publication_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(journal_publication_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(journal_publication_gatekeeping, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(journal_publication_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(journal_publication_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(journal_publication_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value: publishers charge subscription fees (often $4,000+ per article for open access), restrict access to paywalled journals, and extract prestige rent (high-impact journals charge submission fees). Career gatekeeping extracts time and emotional labor from researchers who revise repeatedly to satisfy reviewers. However, extraction is not total (0.66+) because meaningful coordination does occur — peer review catches errors, flags plagiarism, and filters low-quality work, providing genuine service. The gap between nominal (coordination) and structural (extraction) function reflects the Tangled Rope nature. Suppression (0.68): High. Significant barriers to exit include: (1) career dependence on journal prestige, (2) institutional discrimination (reviewers bias against non-elite institutions), (3) subscription access barriers (researchers without institutional affiliation cannot access paywalled articles), (4) time barriers (peer review takes months or years), (5) psychological suppression (rejection, reviewer hostility, perceived illegitimacy of non-journal work). Suppression has risen over the interval as journal consolidation increased and publish-or-perish pressure intensified. Theater ratio (0.64): Moderate-high. Increasingly, the peer review process is performative: author revisions address reviewer ego (stylistic changes, citation padding) rather than substantive errors; editorial decisions appear influenced by prestige bias rather than quality assessment; rejection justifications cite vague 'fit' rather than specific scientific flaws. The process persists because alternatives haven't fully replaced it, not because it functions optimally. The rising theater ratio indicates Piton-like degradation — the mechanism is maintained through institutional inertia while its actual verification capacity declines.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Snare (powerless/trapped) and Rope (institutional/arbitrage) is maximal: the same constraint structure produces opposite classifications because the structural relationship to extraction differs fundamentally. A trapped early-career researcher sees gatekeeping as a pure extraction mechanism with no exit option. An established publisher sees the same mechanism as a coordination solution (quality control) with full arbitrage mobility. Neither perspective is 'wrong' — they accurately describe different positions within the constraint. The analytical observer's mountain classification is analytically distinct: it claims the constraint is immutable, but the structural data shows it is enforced through career incentives and institutional arrangements, not natural laws. This false summit reveals the risk of naturalizing political arrangements: the 'immutability' is contingent on agents continuing to treat journal prestige as a career requirement, which is not inevitable (the open science movement proves this).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (publishers, prestige reviewers) have d ≈ 0.10–0.15 because they experience arbitrage exit: they can choose to leave the traditional gatekeeping system and lose nothing (in fact, high-prestige academics can publish in open-access venues and maintain career status). This low d produces negative f(d) ≈ -0.10, yielding χ = 0.58 × (-0.10) × 1.0 ≈ -0.06: effective negative extraction (the constraint subsidizes this agent). Victims trapped in academia have d ≈ 0.92 because they cannot exit without abandoning their career (trapped exit option). This high d produces f(d) ≈ 1.32, yielding χ = 0.58 × 1.32 × 1.0 ≈ 0.76: high effective extraction. Organized research communities have d ≈ 0.60 because they have constrained exit (can launch preprints, but face prestige penalty). This intermediate d produces f(d) ≈ 0.75, yielding χ ≈ 0.44: moderate effective extraction. The perspectival gap is not a failure of the framework — it is the framework detecting that the same structural mechanism produces opposite experienced realities for different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the six-type taxonomy correctly captures multiple real structural relationships within a single institution. Journal gatekeeping is NOT a pure extraction mechanism (Snare) for beneficiaries — they genuinely benefit from coordination (quality filtering, prestige curation). It is NOT pure coordination (Rope) for victims — they experience severe extraction with no ability to exit. The Tangled Rope classification at the organized community level is the correct synthesis: the constraint provides genuine coordination benefits coupled with asymmetric extraction that depends on the agent's position. The piton observation (degraded ritual persisting through inertia) is empirically sound: peer review is maintained because alternatives haven't fully matured, not because current systems work optimally. The scaffold perspective is not aspirational — open-access and preprint movements represent genuine alternative infrastructure being constructed as exits. The mountain false summit is the critical diagnostic: claiming immutability ('quality control requires gatekeeping') naturalizes a contingent institutional arrangement. The constraint is Tangled Rope, not Mountain, because the coordination function (quality filtering) is structurally distinguishable from the extraction mechanism (career gatekeeping), and both are empirically observable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_verification_sufficiency,
    'Do preprint peer comment communities (arXiv, bioRxiv, open peer review platforms) provide equivalent quality filtering to traditional anonymous peer review?',
    'Longitudinal study comparing error rates, citation counts, and retraction rates between papers published via preprints with open comment vs traditional peer review; analysis of comment quality and correction implementation',
    'If equivalent or superior: the constraint reclassifies from Snare to Scaffold with clearer sunset logic. If inferior: preprints reduce gatekeeping suppression but don''t solve quality control, constraining the scaffold''s exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether open peer comment systems provide equivalent verification to anonymous review').

omega_variable(
    hiring_committee_prestige_dependence,
    'To what degree do tenure and hiring decisions actually require publication in high-impact journals versus accepting preprints, open-access venues, and alternative metrics?',
    'Survey of hiring/tenure committee decisions; analysis of correlation between journal prestige and hiring outcomes; measurement of shift in acceptance of alternative publication venues over time',
    'If prestige-dependence is declining: trapped agents gain mobile/constrained exit options, lowering suppression and repositioning the constraint toward Scaffold. If prestige-dependence is entrenched: trapped agents remain trapped, and Snare classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hiring_committee_prestige_dependence, empirical, 'Degree to which career advancement requires high-impact journal publication').

omega_variable(
    consolidation_versus_decentralization,
    'Is the publishing landscape consolidating (Snare entropy increasing) or decentralizing (Scaffold sunset tightening)?',
    'Market concentration analysis: Herfindahl-Hirschman index for journal market share; measurement of preprint adoption rates and institutional open-access repository growth; tracking of journal cancellation vs launch rates',
    'If consolidating: gatekeeping power concentrates, extraction increases, snare classification strengthens, mountain false-summit risk grows. If decentralizing: constraints fragment, scaffold sunset becomes salient, multiple constraint stories needed to represent discipline-specific variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_versus_decentralization, empirical, 'Whether publishing infrastructure is consolidating or decentralizing').

omega_variable(
    review_quality_versus_speed_tradeoff,
    'Is the high-extraction mechanism intrinsic to peer review quality or a contingent artifact of journal economics (slow process justified by quality, but quality not actually proportional to slowness)?',
    'Comparison of review quality metrics (error detection, constructiveness, consistency) between fast-review venues (medRxiv, rapid-review journals) and slow traditional journals; analysis of whether extended review timelines improve final paper quality',
    'If quality is contingent: suppression is institutional/economic, not necessary, and constraint reclassifies as higher-extraction Snare or toward Tangled Rope with clearer coordination vs extraction boundaries. If quality is intrinsic: suppression is structural to quality control, partially vindicating the mountain false-summit framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_quality_versus_speed_tradeoff, empirical, 'Whether high extraction is intrinsic to review quality or contingent to journal economics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(journal_publication_gatekeeping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jpg_tr_t0, journal_publication_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jpg_tr_t5, journal_publication_gatekeeping, theater_ratio, 5, 0.56).
narrative_ontology:measurement(jpg_tr_t10, journal_publication_gatekeeping, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(jpg_be_t0, journal_publication_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jpg_be_t5, journal_publication_gatekeeping, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(jpg_be_t10, journal_publication_gatekeeping, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(journal_publication_gatekeeping, information_standard).
narrative_ontology:affects_constraint(journal_publication_gatekeeping, research_publish_or_perish_incentive).
narrative_ontology:affects_constraint(journal_publication_gatekeeping, institutional_prestige_hierarchy).
narrative_ontology:affects_constraint(journal_publication_gatekeeping, citation_metric_gaming).

% DUAL FORMULATION NOTE:
% Journal gatekeeping decomposes into three structurally distinct constraints: (1) information standardization (legitimate coordination for quality filtering, ε ≈ 0.15, Rope), (2) prestige hierarchy enforcement (institutional gatekeeping by prestigious institutions, ε ≈ 0.55, Tangled Rope), (3) publisher rent extraction (subscription monopoly, ε ≈ 0.65, Snare). This story represents the hybrid (prestige-based) constraint. The information standard component is upstream and coordination-heavy; the publisher extraction is downstream and asymmetrically extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
