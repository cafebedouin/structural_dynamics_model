% ============================================================================
% CONSTRAINT STORY: open_access_publishing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_access_publishing, []).

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
 *   constraint_id: open_access_publishing
 *   human_readable: Open Access Publishing as Coordination and Extraction Hybrid
 *   domain: academic_publishing/knowledge_distribution
 *
 * SUMMARY:
 *   Open access publishing emerged as a coordination solution to the problem
 *   of knowledge enclosure by subscription publishers. The nominal goal —
 *   making research openly available to researchers and the public — is
 *   genuine. However, the dominant implementation through article processing
 *   charges (APCs) has created a new extraction mechanism that mirrors and in
 *   some cases exceeds the problems it sought to solve. Wealthy institutions
 *   can afford APC subsidies; poor researchers and researchers in low-income
 *   countries cannot. This constraint exhibits simultaneous coordination
 *   (open access genuinely improves knowledge reach and citations) and
 *   extraction (APCs create new barriers to publication). The theater ratio
 *   has increased over time as commercial open access publishers have adopted
 *   prestige narratives ('fast publication,' 'selective acceptance') while
 *   operating on different editorial criteria than traditional journals,
 *   creating performative distinction. The constraint is classified as
 *   Tangled Rope at the analytical level: it solves a real problem while
 *   creating asymmetric extraction. However, the classification varies
 *   dramatically by agent position — powerless early-career researchers in
 *   low-income countries perceive and experience it as Snare, while
 *   well-funded institutions perceive it as Rope. The organized open science
 *   advocates perceive it as Scaffold with a real sunset: nonprofit
 *   infrastructure and normative shifts are building alternative pathways
 *   that will obsolete the APC extraction mechanism within a generation.
 *
 * KEY AGENTS:
 *   - Early-Career Researcher (powerless/trapped): Primary victim — must pay APCs to publish (career survival) but lacks institutional funding. Trapped between publication requirement and financial impossibility.
 *   - Researcher at Well-Funded Institution (moderate/constrained): Secondary beneficiary with extraction costs — institution absorbs APCs; personal extraction minimal but institutional resources diverted.
 *   - Established Research University (institutional/arbitrage): Primary beneficiary — OA increases prestige and citation metrics; can negotiate bulk APC discounts; absorbs costs from research overhead.
 *   - Commercial OA Publisher (institutional/arbitrage): Primary beneficiary — APC revenue model creates reliable income stream; captures market share as OA mandates drive publication volume to OA venues.
 *   - Open Science Advocates (organized/constrained): Organized agents building exit pathways — PLOS, arXiv, institutional repositories, nonprofit publishers creating alternatives with genuine lower extraction.
 *   - Subscription Journal System (institutional/arbitrage): Degraded institutional actor — persists through inertia in hybrid models but core function (exclusive access control) has atrophied.
 *   - Global Knowledge Equity (powerless/trapped): Abstract collective victim — research knowledge increasingly stratified by researcher wealth and institutional affiliation; OA promised equity but delivered conditionally-open access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_access_publishing, 0.52).
domain_priors:suppression_score(open_access_publishing, 0.48).
domain_priors:theater_ratio(open_access_publishing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_access_publishing, extractiveness, 0.52).
narrative_ontology:constraint_metric(open_access_publishing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(open_access_publishing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_access_publishing, tangled_rope).
narrative_ontology:human_readable(open_access_publishing, "Open Access Publishing as Coordination and Extraction Hybrid").
narrative_ontology:topic_domain(open_access_publishing, "academic_publishing/knowledge_distribution").

domain_priors:requires_active_enforcement(open_access_publishing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_access_publishing, established_research_institutions).
narrative_ontology:constraint_beneficiary(open_access_publishing, wealthy_funders).
narrative_ontology:constraint_beneficiary(open_access_publishing, commercial_oa_publishers).
narrative_ontology:constraint_victim(open_access_publishing, early_career_researchers).
narrative_ontology:constraint_victim(open_access_publishing, researchers_in_low_income_countries).
narrative_ontology:constraint_victim(open_access_publishing, small_research_institutions).
narrative_ontology:constraint_victim(open_access_publishing, global_knowledge_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Faces maximal extraction. Must pay article processing charges (APCs) averaging $2,000-5,000 USD to publish open access, with no institutional funding. Suppression is near-total: cannot exit the publication system (career survival requires publication), cannot access funding for APCs (grant overhead restricted), cannot choose traditional subscription journals (pressure toward OA mandates). Trapped in a system designed nominally for equity but operationally excluding the poorest researchers.
constraint_indexing:constraint_classification(open_access_publishing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCHER AT WELL-FUNDED INSTITUTION (TANGLED ROPE) — Experiences genuine coordination benefit (open access increases citations, enables knowledge reach) alongside asymmetric extraction through institutional APC subsidies. Institution absorbs APC costs, but this diverts funds from other research support. Significant autonomy to publish in traditional venues or OA; exit cost is reputational (OA mandates from funders) rather than economic. Mixed extraction and coordination.
constraint_indexing:constraint_classification(open_access_publishing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED RESEARCH UNIVERSITY (ROPE) — Benefits from OA mandates through higher citation metrics and research visibility. Can arbitrage: negotiate institutional membership discounts, absorb APCs from research overhead, use publication productivity as prestige marker. Sees OA as coordination mechanism that increases knowledge reach without reducing institutional advantage (prestige correlates with citation counts). Net beneficiary.
constraint_indexing:constraint_classification(open_access_publishing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL OA PUBLISHER (ROPE) — Benefits substantially from APC-revenue model. OA mandates create guaranteed revenue stream (articles must be published somewhere; OA publishers capture market share). Can arbitrage: offer rapid publication, minimal editorial overhead, predatory acceptance criteria. Sees OA as coordination mechanism (disseminating research) that generates reliable income. Net beneficiary with minimal extraction experience.
constraint_indexing:constraint_classification(open_access_publishing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE ADVOCATES (SCAFFOLD) — Organized groups (PLOS, arXiv, institutional repositories, Plan S coalition) see OA as a transitional coordination problem with real sunset. Building alternative infrastructure (preprint servers, community-reviewed journals, nonprofit publishing models) that bypasses the APC extraction mechanism. Extraction is temporary — as nonprofit alternatives mature and norms shift, commercial OA publishers' rent-seeking power diminishes. Constrained by entrenched publishing culture but building genuine exit paths.
constraint_indexing:constraint_classification(open_access_publishing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SUBSCRIPTION JOURNAL SYSTEM (PITON) — Persists through institutional inertia despite degraded function. OA mandates have reduced subscription revenue, forcing journals to hybrid models (both subscription and OA tiers), which maintain theater without core function. Peer review remains performative rather than genuinely verifying research quality. System persists because alternatives haven't fully replaced it, not because it works. Theater ratio high: journals maintain prestige narratives ('Nature is selective') while accepting lower-quality research if APCs are paid. Degraded constraint maintained by institutional momentum.
constraint_indexing:constraint_classification(open_access_publishing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — OA publishing solves a genuine coordination problem (knowledge should be openly accessible) while simultaneously creating extractive mechanisms (APCs exclude poor researchers; commercial publishers capture rent-seeking opportunities). The coordination function is real but built atop extraction infrastructure. The analytical perspective sees the hybrid character: OA mandates enable research dissemination AND concentrate publishing power among wealthy institutions and commercial publishers. Neither mountain (not a natural law) nor pure rope (not just coordination).
constraint_indexing:constraint_classification(open_access_publishing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_access_publishing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_access_publishing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_access_publishing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_access_publishing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_access_publishing, TR),
    TR >= 0.70.

:- end_tests(open_access_publishing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The APC mechanism extracts from early-career and low-income researchers but is partially offset by genuine coordination benefits (open access does increase knowledge reach and citation impact). The value has increased from 0.28 (early OA era with lower APCs and greater nonprofit dominance) to 0.52 (contemporary era with dominant commercial OA publishers and inflation in APC costs). The trajectory shows accumulation of extraction overhead rather than erosion of coordination function. Suppression (0.48): Moderate. Researchers cannot easily exit the system (career survival requires publication) but have some exit options (traditional subscription journals, preprints, nonprofit venues). Suppression would be higher if all avenues were OA-mandated, lower if nonprofit alternatives were mature. Theater ratio (0.65): Moderate-high and increasing. Commercial OA publishers maintain prestige narratives ('rigorous peer review,' 'selective acceptance') while operating on different (sometimes lower) quality thresholds than traditional journals. The narrative-reality gap has widened as predatory publishers have become more sophisticated in theater maintenance. Claimed type (Tangled Rope): Required by the presence of genuine beneficiaries (universities, publishers), genuine victims (poor researchers, research equity), both coordination function (open access) and asymmetric extraction (APC barriers), and active enforcement (funder and government OA mandates).
 *
 * PERSPECTIVAL GAP:
 *   The dramatic perspectival gap reveals the extraction mechanism. An early-career researcher in Nigeria experiences this as Snare: they face insurmountable APC barriers and cannot exit the system (career survival requires publication). A researcher at Stanford experiences this as Rope: their institution absorbs APCs, open access increases their citations, and they can arbitrage prestige. A commercial OA publisher experiences this as Rope: guaranteed revenue stream from APC mandates. The open science advocate experiences this as Scaffold: they see a temporary extraction phase that nonprofit and normative alternatives are actively sunset-ing. The subscription journal system experiences this as a degraded institutional role (Piton): they persist in hybrid models through inertia, not function. The analytical observer sees the full Tangled Rope: genuine coordination (open access works) embedded in extractive infrastructure (APCs exclude the poor). The perspectival gaps are NOT differences in perspective but differences in structural position. They reveal that the same constraint distributes extraction and benefit asymmetrically across the global research population.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent reflects their structural relationship to the extraction flow. Early-career researchers in low-income countries have d ≈ 0.95 (nearly pure victims: high suppression, no arbitrage options, trapped in system) → high f(d) → experienced extractiveness near maximum. Well-funded institutions have d ≈ 0.25 (partial victims through resource diversion but primary beneficiaries through prestige and citation gains; institutional power provides arbitrage options) → lower f(d) → moderate effective extraction. Commercial publishers have d ≈ 0.10 (near-pure beneficiaries: APC revenue model aligns with their interests; exit options abundant; institutional power) → negative f(d) range → minimal experienced extraction. The Scaffold perspective (organized advocates) with constrained exit options has d ≈ 0.50 (symmetric: they see both extraction and coordination, and have meaningful agency through building alternatives) → moderate f(d) → moderate effective extraction relative to their power. The directionality derivation explains why powerless agents perceive Snare while institutional agents perceive Rope for the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint illustrates how mandatrophy resolution works in practice. The naive mandatrophy confusion asks: 'Is OA a coordination mechanism (Rope) or an extraction mechanism (Snare)?' The answer is that it is both simultaneously — a genuine coordination function (open access) overlaid with extraction infrastructure (APCs). The tangled rope classification resolves this by requiring both coordination function (OA mandates improve knowledge reach) and asymmetric extraction (APCs harm poor researchers). The classification is neither false (it is not just coordination) nor evasive (it names the extraction). The perspectival variation (Snare for powerless, Rope for institutions, Scaffold for advocates) is not classification inconsistency but observation of the constraint's asymmetric structure. The analytical observer's Tangled Rope view is not a compromise between Snare and Rope but a precise structural reading: the constraint solves a real problem and creates a new extractive mechanism simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apc_cost_sustainability,
    'Are current APC levels ($2,000-5,000 USD average) economically sustainable for global research communities, or are they covering hidden publisher margins that should be extracted as excess rent?',
    'Detailed cost-accounting: production costs (peer review, copyediting, hosting) vs publisher revenue; comparison with nonprofit OA publisher cost structures; longitudinal APC inflation tracking',
    'If APCs are near-cost: OA is efficient coordination. If APCs are 2-3x cost: substantial excess extraction occurs, and commercial publishers are capturing rent-seeking opportunities. Classification shifts from Tangled Rope (if prices sustainable) to Snare (if prices extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apc_cost_sustainability, empirical, 'Whether APC levels reflect publication costs or publisher excess').

omega_variable(
    global_research_equity_threshold,
    'At what percentage of researchers globally priced out of OA publication does the system transition from coordination mechanism to extraction mechanism?',
    'Demographic analysis: estimate proportion of researchers unable to pay APCs; correlation with research output gaps by geography/institution type; alternative publication pathway adoption rates',
    'If <10% excluded: OA is substantially inclusive (Rope). If 10-40% excluded: mixed inclusion and extraction (Tangled Rope). If >40% excluded: primarily excludes majority global researchers (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_research_equity_threshold, empirical, 'Proportion of global researchers excluded by APC costs').

omega_variable(
    nonprofit_oa_scaling_feasibility,
    'Can nonprofit open-access publishing infrastructure (PLOS, arXiv, institutional repositories) scale to absorb >50% of global research publication volume while maintaining peer review quality?',
    'Comparative analysis of rejection rates, citation impact, and editorial cost-per-article across nonprofit vs commercial OA publishers; funding sustainability models for nonprofits at scale',
    'If feasible: scaffold sunset is real, and extractive commercial OA is temporary. Classification shifts toward rope (coordination only) as extraction mechanism loses power. If infeasible: commercial OA remains dominant, extraction persists, classification stays Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nonprofit_oa_scaling_feasibility, empirical, 'Whether nonprofit OA infrastructure can scale to majority of research').

omega_variable(
    mandate_enforcement_versus_equity,
    'Do OA mandates (from funders, governments, institutions) improve knowledge equity or entrench advantage by forcing researchers to choose between career survival (mandate compliance) and financial solvency (avoiding APC bankruptcy)?',
    'Longitudinal tracking of publication patterns post-mandate: do early-career and low-income researchers increase OA publication rates or migrate out of academic careers? Do research output gaps widen or narrow?',
    'If equity improves: mandate enforcement is justified (coordination). If equity worsens: mandates are extractive pressure mechanism (snare). Affects overall classification and interpretation of suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_enforcement_versus_equity, conceptual, 'Whether OA mandates improve or entrench research equity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_access_publishing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, open_access_publishing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(open_tr_t5, open_access_publishing, theater_ratio, 5, 0.54).
narrative_ontology:measurement(open_tr_t10, open_access_publishing, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(open_be_t0, open_access_publishing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(open_be_t5, open_access_publishing, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(open_be_t10, open_access_publishing, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_access_publishing, information_standard).
narrative_ontology:affects_constraint(open_access_publishing, research_equity_stratification).
narrative_ontology:affects_constraint(open_access_publishing, academic_publishing_rent_seeking).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_access_publishing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
