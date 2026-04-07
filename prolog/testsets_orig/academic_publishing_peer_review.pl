% ============================================================================
% CONSTRAINT STORY: academic_publishing_peer_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_publishing_peer_review, []).

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
 *   constraint_id: academic_publishing_peer_review
 *   human_readable: Academic Publishing Peer Review System
 *   domain: academic/knowledge_production
 *
 * SUMMARY:
 *   The academic publishing peer review system represents a global
 *   knowledge-production constraint that blends genuine coordination (error
 *   detection, quality filtering, reputation signaling) with substantial
 *   extraction (unpaid labor, gatekeeping, access restriction, publication
 *   bias). Over the past 40 years, the theater ratio has increased from 0.42
 *   to 0.68 as the volume of submitted manuscripts has exploded while
 *   reviewer capacity has remained flat, forcing peer review to rely
 *   increasingly on proxy signals (author reputation, institutional
 *   affiliation, novelty claims) rather than actual manuscript verification.
 *   Simultaneously, journal publishers have captured increasing economic rent
 *   through subscription paywalls and article processing fees, while the
 *   unpaid labor of reviewers has become more extractive as review cycles
 *   lengthen and desk rejections proliferate. The constraint exhibits all six
 *   DR types from different perspectives, revealing deep structural tension
 *   between its coordination function (quality filtering) and its extraction
 *   function (gatekeeping, access control, career surveillance). Early career
 *   researchers experience it as a snare—trapped without alternatives and
 *   bearing extraction costs through unpaid review labor, submission fees,
 *   and gatekeeping. Established researchers experience it as tangled
 *   rope—genuine benefits (reputation, priority signaling) mixed with
 *   extraction (mandatory review service, subscription costs). Publishers
 *   experience it as pure rope—free quality control and distribution.
 *   Open-access advocates see a scaffold with sunset—preprints, registered
 *   reports, and post-publication review are building alternative pathways
 *   that will eventually bypass the traditional paywall system. The peer
 *   review ritual itself has become a piton—maintained through institutional
 *   inertia despite degraded function. The analytical observer risks
 *   naturalizing peer review as an immutable law of knowledge production,
 *   when it is actually a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary victims (powerless/trapped) — forced participation, unpaid labor, gatekeeping bias, career dependence on publication outcomes
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) — extract economic rent through subscriptions, access control, and article processing fees; receive free editorial and review labor
 *   - Established Researchers: Secondary beneficiaries (moderate/constrained) — benefit from reputation filtering and priority signaling; bear costs through mandatory review service and publication delays
 *   - Research Integrity: Victim-collective (powerless/trapped) — publications with flawed peer review contaminate knowledge base; no mechanism for systematic correction
 *   - Knowledge Access: Victim-collective (powerless/trapped) — paywalls restrict access to publicly-funded research; peer review legitimacy justifies access restrictions
 *   - Open Access Movement: Organized actors (organized/mobile) — arXiv, preprint servers, open journals building alternative verification pathways with generational sunset
 *   - Peer Review Ritual: Institutional performance (institutional/arbitrage) — maintains credentialing function while actual error-detection capacity has degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_publishing_peer_review, 0.54).
domain_priors:suppression_score(academic_publishing_peer_review, 0.62).
domain_priors:theater_ratio(academic_publishing_peer_review, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_publishing_peer_review, extractiveness, 0.54).
narrative_ontology:constraint_metric(academic_publishing_peer_review, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(academic_publishing_peer_review, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_publishing_peer_review, tangled_rope).
narrative_ontology:human_readable(academic_publishing_peer_review, "Academic Publishing Peer Review System").
narrative_ontology:topic_domain(academic_publishing_peer_review, "academic/knowledge_production").

domain_priors:requires_active_enforcement(academic_publishing_peer_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_publishing_peer_review, journal_publishers).
narrative_ontology:constraint_beneficiary(academic_publishing_peer_review, established_researchers).
narrative_ontology:constraint_beneficiary(academic_publishing_peer_review, editorial_gatekeepers).
narrative_ontology:constraint_victim(academic_publishing_peer_review, early_career_researchers).
narrative_ontology:constraint_victim(academic_publishing_peer_review, research_integrity).
narrative_ontology:constraint_victim(academic_publishing_peer_review, knowledge_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Trapped within the peer review system as the only recognized pathway to academic legitimacy and employment. Faces extraction through unpaid labor (manuscript review), career gatekeeping (rejection), delayed publication, and reviewer bias. No viable alternative for establishing credibility. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(academic_publishing_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACTIVE RESEARCH COMMUNITY (TANGLED ROPE) — Experiences both genuine coordination (quality filtering, reputation signaling) and asymmetric extraction (unpaid review labor, publication delays, access paywalls). Constrained by career incentives and disciplinary norms. Some agency through preprints and alternative venues, but significant costs to exit dominant system.
constraint_indexing:constraint_classification(academic_publishing_peer_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JOURNAL PUBLISHER (ROPE) — Benefits substantially from the peer review system which provides free labor for editorial and review functions while enabling subscription and access control. Experiences the constraint as pure coordination mechanism: filtering, quality assurance, and distribution. Net beneficiary with arbitrage capacity—can switch business models (open access) while maintaining institutional position.
constraint_indexing:constraint_classification(academic_publishing_peer_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN ACCESS MOVEMENT (SCAFFOLD) — Organized agents (arXiv, preprint servers, open-access mandates, alternative journals) see peer review as a temporary coordination problem with a clear sunset. Distributed review, post-publication scrutiny, and transparent commenting systems are building alternative verification pathways. Mobile exit options and sunset logic—as norms shift and funding mandates require open access, the traditional paywall-dependent peer review loses extraction power.
constraint_indexing:constraint_classification(academic_publishing_peer_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The formal peer review process is largely performative theater: reviewers often cannot verify data, lack domain expertise for novel work, have insufficient time, and face no accountability. The ritual persists through institutional inertia and credentialing tradition rather than demonstrated effectiveness. High theater ratio (0.68) indicates the core function (error detection) has degraded while performative elements (authority assignment) remain.
constraint_indexing:constraint_classification(academic_publishing_peer_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, peer review appears as an immutable natural constraint on knowledge production: some verification mechanism is always necessary, and distributed expert evaluation is inherent to quality assurance. This perspective naturalizes what the structural data reveals as contingent institutional arrangements. The engine's false summit detector will flag this as naturalization rather than genuine natural law.
constraint_indexing:constraint_classification(academic_publishing_peer_review, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_publishing_peer_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_publishing_peer_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_publishing_peer_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_publishing_peer_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_publishing_peer_review, TR),
    TR >= 0.70.

:- end_tests(academic_publishing_peer_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The system extracts through multiple mechanisms: (1) unpaid reviewer labor estimated at billions annually, (2) publisher capture of economic rent ($25B+ annual subscription revenue for non-open-access journals), (3) gatekeeping that concentrates publication in high-prestige venues, (4) publication bias against null results and replication studies. The 0.54 value reflects that genuine quality filtering does occur (coordination function), but the distribution of benefits is highly asymmetric. Over the 40-year interval, extractiveness has increased as manuscript volume grew exponentially while review capacity remained flat, forcing reviews to become more superficial. Suppression (0.62): Moderate-high. Barriers to exit include: (1) publication requirement for academic employment and funding, (2) limited alternative credentialing systems, (3) network effects (researchers cite journal-published work, creating citation advantage for gatekept venues), (4) disciplinary norms enforcing journal hierarchy, (5) asymmetric information about review criteria. Theater ratio (0.68): High and increasing. The formal peer review process relies heavily on proxy signals: reviewer identity and reputation, author institutional affiliation, journal prestige, novelty framing. Actual manuscript verification is limited by reviewer time constraints, disciplinary expertise boundaries, and inability to access raw data. The theater has increased as complexity of research has outpaced reviewer capacity. Open-access preprints bypass much of this theater by using distributed post-publication commentary, revealing that the performative element was not intrinsic to quality filtering.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The early career researcher sees extraction without exit (Snare). The established researcher sees mixed coordination and extraction (Tangled Rope) because they benefit from the filtering while also bearing service costs. The publisher sees pure coordination with no extraction (Rope) because the system is designed for their benefit. The open access movement sees a temporary institutional arrangement with a clear sunset (Scaffold) as preprints and registered reports create alternatives. The peer review ritual itself is recognized as degraded and performative (Piton)—maintained through credentialing tradition rather than functional necessity. The analytical observer risks seeing natural law (Mountain) when the structural data reveals contingent institutional arrangements. The perspectival gap is so pronounced that single-index analysis (treating the system as one type) produces severe classification error.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position within the extraction flow. Publishers as beneficiaries with arbitrage capacity have low d (approximately 0.15), experiencing negative effective extraction χ—the system subsidizes their institutional position. Early career researchers as victims trapped without alternatives have high d (approximately 0.95), experiencing maximum f(d) multiplier and maximum χ—every structural element of peer review extracts from them. Established researchers as moderate-power victims with constrained exit have mid-range d (approximately 0.65), experiencing moderate χ—they extract value through reputation filtering but bear costs through review service. The analytical observer treating peer review as natural law has canonical d for analytical position (approximately 0.73), but the false summit detector flags this as naturalization—the mountain classification is revealed as a perspectival frame rather than structural necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The peer review constraint resolves mandatrophy by decomposing into two structurally distinct claims: (1) COORDINATION CLAIM: Some expert evaluation of research quality is necessary and unavoidable (true, approaches Mountain). (2) INSTITUTIONAL CLAIM: The current journal-gated peer review system is the only or optimal mechanism (false, approaches Snare/Tangled Rope). The system conflates these by naturalizing institutional arrangements as coordination necessities. The scaffold perspective shows that distributed post-publication review (arXiv, bioRxiv, open commenting) provides quality filtering with lower theater ratio, lower extraction, and lower suppression. The piton perspective reveals that the performative component of peer review (authority assignment through anonymous review ritual) can be decoupled from the filtering component (error detection). Reform pathways: (A) Decouple coordination from extraction by changing incentives: require open-science practices, deweight publication metrics in hiring, pay reviewers, open access all research. (B) Replace journal gatekeeping with decentralized review: preprints + transparent post-publication commentary + discipline-specific reputation systems. (C) Partial replacement: use peer review for high-stakes claims only (medical interventions, policy recommendations); use lightweight filtering for exploratory research. Under pathway A, classification shifts toward Rope. Under pathway B, classification shifts toward Scaffold with sunset. Under pathway C, system splits into domain-specific stories with different ε values.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    review_quality_vs_throughput_tradeoff,
    'Is the peer review system extractive because of inherent quality-throughput tradeoff, or because current institutional incentives prioritize speed and output metrics over review depth?',
    'Comparative analysis of review quality in systems with different incentive structures (slow selective journals vs high-volume venues); measurement of actual error detection rates against post-publication corrections and retraction data',
    'If inherent tradeoff: classification stabilizes as Tangled Rope with moderate extractiveness. If incentive-driven: extractiveness could decrease significantly with structural reform, reclassifying as Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_quality_vs_throughput_tradeoff, empirical, 'Whether review quality-throughput tradeoff is inherent or incentive-driven').

omega_variable(
    reviewer_expertise_scarcity,
    'Does reviewer expertise scarcity necessitate the current gatekeeping structure, or could decentralized expertise networks (post-publication review, transparent commenting) detect errors equally well?',
    'Empirical comparison of error detection rates: traditional pre-publication peer review vs arXiv post-publication comment systems; analysis of which error categories each system catches',
    'If scarcity is genuine constraint: peer review is closer to Mountain (unavoidable bottleneck). If decentralized networks prove effective: scaffold classification confirmed, extraction is institutional rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reviewer_expertise_scarcity, empirical, 'Whether specialized reviewer scarcity necessitates centralized gatekeeping').

omega_variable(
    publication_bias_causality,
    'Does peer review''s suppression of negative results stem from the review process itself, or from downstream publication biases in journal selection and researcher incentives?',
    'Tracking manuscript rejection rates for null results in peer review vs acceptance rates when same studies submitted to journals with explicit null-result policies; analysis of whether reviewers themselves reject null results or whether researchers preemptively filter',
    'If peer review-driven: suppression is intrinsic to system, validates snare/tangled rope classification. If downstream-driven: peer review is partial cause, reform of publication incentives could reduce total suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_causality, empirical, 'Whether publication bias stems from peer review process or downstream incentives').

omega_variable(
    alternative_verification_scalability,
    'Can transparent post-publication review and distributed expert commentary scale to verify the entire volume of contemporary research output?',
    'Pilot implementation analysis (arXiv, bioRxiv communities); measurement of review velocity, coverage, and error detection rates as volume increases; identification of failure points in decentralized systems',
    'If not scalable: decentralized approaches are supplement only, peer review remains necessary structural element. If scalable: scaffold perspective is realistic, sunset is achievable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_verification_scalability, empirical, 'Whether distributed review can scale to full research volume').

omega_variable(
    career_incentive_coupling_plasticity,
    'Can the extraction component be decoupled from the coordination component by modifying career incentive structures (hiring, promotion, funding decisions) without dismantling peer review itself?',
    'Analysis of institutions implementing alternative evaluation metrics (publication-free hiring, open-science bonuses); measurement of whether decoupled incentives reduce extraction while maintaining quality filtering',
    'If decoupling possible: system could transform from Tangled Rope to Rope via institutional reform. If tight coupling: the extraction and coordination are structurally inseparable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_incentive_coupling_plasticity, preference, 'Whether extraction can be decoupled from coordination through incentive reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_publishing_peer_review, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acapub_tr_t0, academic_publishing_peer_review, theater_ratio, 0, 0.42).
narrative_ontology:measurement(acapub_tr_t20, academic_publishing_peer_review, theater_ratio, 20, 0.55).
narrative_ontology:measurement(acapub_tr_t40, academic_publishing_peer_review, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(acapub_be_t0, academic_publishing_peer_review, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acapub_be_t20, academic_publishing_peer_review, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(acapub_be_t40, academic_publishing_peer_review, base_extractiveness, 40, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_publishing_peer_review, information_standard).
narrative_ontology:affects_constraint(academic_publishing_peer_review, research_funding_allocation).
narrative_ontology:affects_constraint(academic_publishing_peer_review, academic_hiring_and_promotion).
narrative_ontology:affects_constraint(academic_publishing_peer_review, journal_subscription_economics).

% DUAL FORMULATION NOTE:
% The peer review system is composed of three structurally distinct constraints: (1) verification_quality_filtering (ε≈0.25, approaches Rope), (2) journal_economic_gatekeeping (ε≈0.72, approaches Snare), (3) career_publication_requirement (ε≈0.60, Tangled Rope). The integrated system has ε≈0.54 (Tangled Rope) but can be decomposed for more precise analysis. Publishers benefit primarily from constraint 2; researchers bear costs primarily from constraint 3; knowledge access is damaged by constraint 2; research integrity is damaged by constraint 1 through publication bias.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_publishing_peer_review, institutional, 0.15).
constraint_indexing:directionality_override(academic_publishing_peer_review, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
