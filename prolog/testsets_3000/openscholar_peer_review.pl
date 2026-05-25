% ============================================================================
% CONSTRAINT STORY: openscholar_peer_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openscholar_peer_review, []).

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
 *   constraint_id: openscholar_peer_review
 *   human_readable: Traditional Academic Peer Review
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   Traditional academic peer review operates as a gatekeeper mechanism
 *   between research and publication. While intended as a quality-control
 *   coordination device, the system exhibits significant extractive
 *   properties: it delays publication, concentrates power among established
 *   researchers, creates barriers to entry for outsiders, and extracts
 *   credibility value for journals and institutions. The constraint is a
 *   classic Tangled Rope — it provides genuine coordination benefits (quality
 *   signals, priority establishment, error detection, norm-setting) while
 *   simultaneously extracting rent through gatekeeping, access barriers, and
 *   status consolidation. The theater ratio has increased over 50 years as
 *   complexity and specialization have outpaced reviewer capacity, making the
 *   review process increasingly performative. Early-career researchers and
 *   outsider scholars experience this as a Snare: trapped within a system
 *   they cannot exit without sacrificing institutional legitimacy.
 *   Established researchers experience it as a Rope: they benefit from the
 *   quality signal without bearing suppression costs. The open-science
 *   movement views it as a Scaffold with sunset logic: preprints,
 *   post-publication review, and transparent peer review are building
 *   alternative pathways that will eventually reduce the extraction
 *   mechanism's force.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victim (powerless/trapped) — depend on peer review for career legitimacy but lack network power to influence outcomes
 *   - Outsider Scholars: Primary victim (moderate/constrained) — face systematic barriers to publication due to non-institutional affiliation or methodological unconventionality
 *   - Journal Publishers: Primary beneficiary (institutional/arbitrage) — extract value through subscription fees, submission fees, and prestige without bearing review costs
 *   - Established Researchers: Primary beneficiary (powerful/arbitrage) — leverage network position, editorial connections, and credibility to navigate system favorably
 *   - Research Communities in Niche Fields: Mixed (organized/constrained) — benefit from coordination function but suffer from extraction by senior gatekeepers within the field
 *   - Open Science Coalition: Alternative pathway (organized/mobile) — building preprint servers, post-publication review, registered reports to create exit routes
 *   - Peer Review Institution: Theater performer (institutional/arbitrage) — maintains performative ritual through cultural legitimacy and institutional mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openscholar_peer_review, 0.52).
domain_priors:suppression_score(openscholar_peer_review, 0.58).
domain_priors:theater_ratio(openscholar_peer_review, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openscholar_peer_review, extractiveness, 0.52).
narrative_ontology:constraint_metric(openscholar_peer_review, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(openscholar_peer_review, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openscholar_peer_review, tangled_rope).
narrative_ontology:human_readable(openscholar_peer_review, "Traditional Academic Peer Review").
narrative_ontology:topic_domain(openscholar_peer_review, "technological/institutional").

domain_priors:requires_active_enforcement(openscholar_peer_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openscholar_peer_review, journal_publishers).
narrative_ontology:constraint_beneficiary(openscholar_peer_review, established_researchers).
narrative_ontology:constraint_beneficiary(openscholar_peer_review, gatekeeper_institutions).
narrative_ontology:constraint_victim(openscholar_peer_review, early_career_researchers).
narrative_ontology:constraint_victim(openscholar_peer_review, outsider_scholars).
narrative_ontology:constraint_victim(openscholar_peer_review, niche_specializations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped within the peer review system for career survival. Cannot bypass traditional journals without destroying credibility. Reviewers extract control over publication timing, project direction, and framing. No exit option: bypassing peer review (preprints alone) does not suffice for tenure, grants, or positions. Maximum suppression: peer review gate controls access to institutional legitimacy.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OUTSIDER SCHOLAR (SNARE) — Independent or non-institutional researcher lacking embedded networks. Review process filters systematically against unfamiliar methodologies and unconventional affiliations. Exit is limited: preprints reach peers but lack institutional weight. Extraction occurs through rejection bias, slow review timelines, and ideological gatekeeping. High suppression due to inability to appeal review decisions or leverage institutional standing.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH COMMUNITY IN NICHE FIELD (TANGLED ROPE) — Benefits from peer review's coordination function: quality signals, priority establishment, and shared standards. Simultaneously extracts rent: limited reviewer pool creates bottlenecks; established researchers review competitors' work; insular subfields enforce methodological orthodoxy. Constrained exit: the field depends on the peer review system even while that system concentrates power among senior members.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JOURNAL PUBLISHER (ROPE) — Benefits from peer review's coordination function without bearing suppression costs. Peer review volunteers filter submissions, establish quality standards, and maintain journal reputation. Publishers arbitrage between author submission fees, institutional subscriptions, and advertiser interest. Exit is available: publishers can shift models (megajournals, open-access). Extraction flows toward publishers, not away from them.
constraint_indexing:constraint_classification(openscholar_peer_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTABLISHED RESEARCHER (ROPE) — Benefits from peer review as a quality signal and credibility marker. Has network access to favorable reviewers, editorial connections, and institutional backing. Can arbitrage: publish in high-impact venues, decline to review, or leverage preprint prestige. Experiences review process as coordination mechanism rather than extraction. Near-zero suppression for this agent.
constraint_indexing:constraint_classification(openscholar_peer_review, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW INSTITUTION AS THEATER (PITON) — The peer review system persists as performative ritual despite degradation. Anonymous review masks biases rather than eliminating them; reviewers are unpaid, overworked, and often biased. Alternative verification mechanisms (preprints, post-publication review, open data) exist but peer review remains mandated by tenure and funding committees. The system continues through institutional inertia and cultural legitimacy, not because it optimally solves quality control. Theater ratio is high: reviewing 10 papers per year feels rigorous but catches few errors; publication rituals (acceptance letters, proofs, copyediting) perform quality without ensuring it.
constraint_indexing:constraint_classification(openscholar_peer_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, open-access mandates, post-publication review platforms, transparent peer review) are building alternative or supplementary verification mechanisms. View traditional peer review as temporary coordination structure with a sunset: as preprint scrutiny, open data, and registered reports mature, the extraction mechanism of closed peer review loses force. Coalition has mobile exit: can migrate to alternative platforms. Suppression is declining as norms shift.
constraint_indexing:constraint_classification(openscholar_peer_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Peer review serves genuine coordination functions (quality signal, priority establishment, norm-setting, error detection). Simultaneously, the system extracts rent through publication delays, gatekeeping against methodological diversity, status hierarchy among journals, and capture by established researchers. The constraint exhibits both properties: it is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid where coordination justifies extraction. Base extractiveness (0.52) reflects this hybrid nature.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openscholar_peer_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openscholar_peer_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openscholar_peer_review, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openscholar_peer_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openscholar_peer_review, TR),
    TR >= 0.70.

:- end_tests(openscholar_peer_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The peer review system extracts value through publication delays (6-18 months average), gatekeeping against outsider perspectives, preferential treatment for high-status authors, and concentration of prestige in elite journals. However, extraction is not total — the system does detect some errors, establish legitimate priority, and maintain quality standards. The value increased from 0.38 to 0.52 over 50 years as reviewer overload has increased theater and reduced actual quality control. Suppression (0.58): Moderate-high. Early-career researchers and outsiders face significant barriers: anonymous review masks but does not eliminate bias, limited reviewer pool creates bottlenecks, publication delays impose costs, rejection is difficult to appeal. Established researchers and institutional affiliates face lower suppression. Theater ratio (0.68): High and increasing. The ritual of peer review (anonymous review, formal review rounds, copyediting) performs quality control but often fails to catch errors. Anonymous review obscures rather than eliminates bias. Reviewer overload reduces actual engagement. Claimed type (Tangled Rope): The system provides coordination (quality signals, priority, norms) while extracting rent (delay, gatekeeping, status hierarchy). Both properties are structural; neither can be removed without degrading the other.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence. The early-career researcher sees a Snare (trapped, extracted, no exit). The established researcher sees a Rope (beneficial coordination, arbitrage options). The journal publisher sees a Rope (passive benefit, no suppression). The open science coalition sees a Scaffold (temporary structure, sunset approaching). The niche research community sees a Tangled Rope (coordination benefits offset by extraction costs). The peer review institution itself, viewed from a 50-year civilizational timescale, appears as a Piton (performative ritual maintained through inertia). The analytical observer sees a Tangled Rope — coordination justified by genuine quality control, but extraction justified by captured incentives and network effects. These divergences are not observational disagreements but structural realities: different agents genuinely experience different effective extractiveness because they occupy different structural positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary significantly across agent types due to their structural position relative to the extraction flow. Early-career researchers (powerless/trapped) derive d ≈ 0.95, experiencing near-maximum extraction because they depend on peer review for legitimacy but have no exit option. Outsider scholars (moderate/constrained) derive d ≈ 0.75, experiencing significant extraction but with some exit capacity (preprints, alternative outlets). Established researchers (powerful/arbitrage) derive d ≈ 0.25, experiencing minimal extraction because they can arbitrage between outlets and leverage network power. Journal publishers (institutional/arbitrage) derive d ≈ 0.05, experiencing net benefit (negative d) because they extract value without bearing costs. The niche research community (organized/constrained) derives d ≈ 0.55, experiencing mixed costs and benefits from the coordination/extraction hybrid. Open science advocates (organized/mobile) derive d ≈ 0.40, experiencing moderate extraction but with genuine exit pathways emerging. The analytical observer (analytical/analytical) derives d ≈ 0.60, seeing the constraint as a legitimate hybrid but not as an immutable natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Tangled Rope classification prevents false dichotomy between 'peer review is pure coordination' (Rope, which would ignore gatekeeping) and 'peer review is pure extraction' (Snare, which would ignore quality benefits). The system exhibits BOTH properties simultaneously and structurally. Early-career researchers experience it predominantly as Snare (extraction-biased) because they have no exit. Established researchers experience it predominantly as Rope (coordination-biased) because they can exit. The true constraint is the hybrid itself: peer review is a coordination mechanism that justifies and enables extraction by its beneficiaries. The theater ratio (0.68) indicates that significant portions of the review ritual are performative rather than functional, creating space for extraction to hide. The measurements show theater increasing over 50 years, suggesting that as reviewer overload has increased, the performative portion has grown while error-detection capability has declined — the extraction mechanism remains in place (suppression, gatekeeping) while the coordination function (actual quality improvement) has atrophied. This is the classic Mandatrophy failure: a system that successfully justified extraction through coordination function begins to extract without coordinating. The Scaffold perspective (open science coalition) offers a potential resolution pathway: alternative mechanisms (preprints, post-publication review, open data) promise to recover coordination without extraction, creating a sunset for the traditional system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reviewer_bias_measurability,
    'Can reviewer bias (ideological, methodological, affiliation-based) be quantified and distinguished from legitimate quality assessment?',
    'Meta-analysis of acceptance rates by reviewer identity, blind vs identified review outcomes, and comparison of review quality metrics across reviewer demographics',
    'If bias is dominant: peer review classifies as pure Snare for targets. If bias is minor: peer review''s coordination function is legitimate, supporting Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reviewer_bias_measurability, empirical, 'Measurement of reviewer bias in peer review outcomes').

omega_variable(
    preprint_replacement_sufficiency,
    'Do preprint platforms with post-publication peer review and distributed scrutiny provide equivalent or superior quality control to traditional anonymous peer review?',
    'Comparative analysis of error detection rates, retraction rates, and citation impact for papers published via preprint-first vs traditional journal-first pathways',
    'If preprints are sufficient: scaffold sunset is real and extraction mechanism will decline. If preprints underperform: peer review''s coordination function is more essential than alternatives suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_replacement_sufficiency, empirical, 'Whether preprints with post-publication review replace traditional peer review').

omega_variable(
    access_barrier_quantification,
    'What fraction of rejection decisions are due to epistemic quality issues vs gatekeeping against outsider perspectives, unconventional methods, or non-institutional affiliation?',
    'Statistical analysis of rejection reasoning across reviewer identities, field specializations, and author affiliations; comparison with retraction rates and citation impact of accepted vs rejected papers',
    'If gatekeeping dominates: suppression increases and extraction increases. If quality issues dominate: suppression is justified and extraction is minimized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_quantification, empirical, 'Quantification of access barriers in peer review decisions').

omega_variable(
    publication_delay_necessity,
    'Do publication delays (average 6-18 months from submission to publication) serve quality control or primarily extract value (priority, prestige, rent) from authors?',
    'Analysis of correlation between review duration and error detection; comparison of quality outcomes for fast-track vs traditional review; measurement of career impact of publication delays',
    'If delays improve quality: they are justified suppression. If delays do not improve quality: they are pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_delay_necessity, empirical, 'Whether publication delays serve quality or extraction').

omega_variable(
    institutional_capture_extent,
    'To what degree does peer review system capture by high-status institutions and established researchers reduce diversity of accepted methodologies and perspectives?',
    'Analysis of acceptance rates by institution type and researcher career stage; measurement of methodological diversity in accepted papers; tracking of citation networks to identify closed communities',
    'If capture is extensive: suppression increases and extraction increases for outsiders. If capture is minimal: peer review serves coordination function more than extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_extent, empirical, 'Extent of institutional capture in peer review').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openscholar_peer_review, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peer_review_tr_t0, openscholar_peer_review, theater_ratio, 0, 0.52).
narrative_ontology:measurement(peer_review_tr_t25, openscholar_peer_review, theater_ratio, 25, 0.6).
narrative_ontology:measurement(peer_review_tr_t50, openscholar_peer_review, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(peer_review_be_t0, openscholar_peer_review, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(peer_review_be_t25, openscholar_peer_review, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(peer_review_be_t50, openscholar_peer_review, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openscholar_peer_review, information_standard).
narrative_ontology:affects_constraint(openscholar_peer_review, publication_bias_mechanism).
narrative_ontology:affects_constraint(openscholar_peer_review, institutional_prestige_hierarchy).
narrative_ontology:affects_constraint(openscholar_peer_review, researcher_credibility_signal).

% DUAL FORMULATION NOTE:
% Traditional peer review decomposes into multiple structural constraints. The quality-control coordination function (upstream: publication_bias_mechanism as a *check* on quality) is distinct from the gatekeeping extraction mechanism (downstream: institutional_prestige_hierarchy that uses peer review as a rank-maintaining tool). The extractiveness value (0.52) reflects both. Preprint alternatives address coordination without extraction by enabling distributed post-publication review.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openscholar_peer_review, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
