% ============================================================================
% CONSTRAINT STORY: academic_journal_peer_review_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_journal_peer_review_gatekeeping, []).

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
 *   constraint_id: academic_journal_peer_review_gatekeeping
 *   human_readable: Academic Journal Peer Review Gatekeeping
 *   domain: academic_publishing/institutional_governance
 *
 * SUMMARY:
 *   Academic journal peer review gatekeeping is a constraint that coordinates
 *   quality assurance for scientific publishing while simultaneously
 *   extracting career advantages for established researchers and
 *   institutional prestige benefits for major publishers. The system exhibits
 *   all markers of Tangled Rope: genuine coordination (error-checking,
 *   priority establishment, quality filtering) is mixed with asymmetric
 *   extraction (publication delays, suppression of outsider voices, citation
 *   advantage for established groups). The constraint's extractiveness has
 *   increased over 40 years (0.38 → 0.54) as journal prestige has become more
 *   concentrated, university rankings have become more dependent on
 *   publication metrics, and early-career researchers face increasing
 *   publication pressure for career advancement. Theater ratio has similarly
 *   increased (0.48 → 0.68) as the performative aspects of peer review
 *   (reviewer bias, anonymity that is easily pierced, desk rejections based
 *   on perceived fit) have become more visible, while the coordination
 *   function remains justified. The constraint demonstrates how indexical
 *   classification reveals the same structural system from eight distinct
 *   observational positions: as pure extraction (snare) from powerless
 *   perspectives, as pure coordination (rope) from beneficiary perspectives,
 *   as mixed (tangled rope) from moderate community and organized alternative
 *   perspectives, as performative ritual (piton) from the institution's own
 *   view, and as a false natural law (mountain from analytical) that risks
 *   naturalizing contingent institutional arrangements.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victims (powerless/trapped) — career advancement locked behind journal publication; no viable exit; bear maximum cost of gatekeeping delays and desk rejections
 *   - Outsider Researchers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with prestige publication; cannot exercise exit because their identity is constituted through pursuit of top-tier venue publication
 *   - Research Community (Field-Level): Mixed beneficiary-victim (moderate/constrained) — genuinely benefits from peer review coordination but also constrained by gatekeeping extraction; cannot fully exit without career cost
 *   - Established Research Groups: Primary beneficiaries (institutional/arbitrage) — gatekeeping advantage runs in their direction; can arbitrage multiple publication venues; low rejection rates; network access to editors
 *   - Major Publishers: Primary beneficiaries (institutional/arbitrage) — gatekeeping sustains market position, prestige, subscription revenue; experiences constraint as pure coordination
 *   - Open Science Movement: Organized alternative agents (organized/mobile) — building alternative verification pathways (preprints, overlay journals, open peer review); mobile exit available but faces institutional suppression
 *   - Peer Review Institution: Performative actor (institutional/arbitrage) — maintains itself through inertia; recognizes own degradation (piton); alternatives have not fully replaced traditional review
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summitry by naturalizing gatekeeping as immutable feature of knowledge production rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_journal_peer_review_gatekeeping, 0.54).
domain_priors:suppression_score(academic_journal_peer_review_gatekeeping, 0.65).
domain_priors:theater_ratio(academic_journal_peer_review_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_journal_peer_review_gatekeeping, extractiveness, 0.54).
narrative_ontology:constraint_metric(academic_journal_peer_review_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(academic_journal_peer_review_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_journal_peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(academic_journal_peer_review_gatekeeping, "Academic Journal Peer Review Gatekeeping").
narrative_ontology:topic_domain(academic_journal_peer_review_gatekeeping, "academic_publishing/institutional_governance").

domain_priors:requires_active_enforcement(academic_journal_peer_review_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_journal_peer_review_gatekeeping, established_research_groups).
narrative_ontology:constraint_beneficiary(academic_journal_peer_review_gatekeeping, journal_editors).
narrative_ontology:constraint_beneficiary(academic_journal_peer_review_gatekeeping, major_publishers).
narrative_ontology:constraint_victim(academic_journal_peer_review_gatekeeping, early_career_researchers).
narrative_ontology:constraint_victim(academic_journal_peer_review_gatekeeping, outsider_researchers).
narrative_ontology:constraint_victim(academic_journal_peer_review_gatekeeping, research_quality_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Career advancement is locked behind journal publication in top venues. No viable exit: preprints do not count for tenure, metrics are entirely journal-dependent, and rejection cycles consume years. Trapped by institutional requirements, bears maximum cost of gatekeeping delays and desk rejections. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OUTSIDER RESEARCHER (SNARE) — Structurally mobile (could publish in lower-tier journals or preprints) but identity-locked into aspirational affiliation with top-tier venues. The researcher's identity is constituted through the pursuit of prestige publication — the constraint is binding because it has been internalized as what legitimate research requires. Exit from top-tier pursuit would require abandoning professional identity as a 'serious researcher.' Bears extraction through delayed publication, higher rejection rates, and citation disadvantage from non-prestigious venues.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: FIELD'S RESEARCH COMMUNITY (TANGLED ROPE) — Genuinely coordinates through peer review (error-checking, quality filtering, priority establishment). Also extracts: gatekeeping delays research dissemination, creates citation advantage for established groups, and suppresses methodological heterodoxy. Communities at major universities have constrained exit (can publish elsewhere at career cost); resource-poor communities face higher barriers. Mixed coordination and extraction.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH GROUP (ROPE) — Experiences the constraint as pure coordination. Top venues are calibrated to their standards; their graduate students have networks with editors; their grants support publication costs; rejection rates are low. They can arbitrage: maintain high-prestige publication while also publishing in specialized venues. Net beneficiary — gatekeeping advantage runs in their direction.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MAJOR PUBLISHER (ROPE) — Gatekeeping sustains their market position and subscription revenue. Experiences the constraint as pure coordination: peer review maintains journal prestige, prestige attracts submissions, submissions generate content that justifies subscription fees. No exit needed — the mechanism benefits them directly. Zero experienced extraction.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE MOVEMENT (TANGLED ROPE) — Organized alternative (preprints, overlay journals, open peer review) with mobile exit: researchers can publish via arXiv or decentralized platforms. The movement experiences genuine coordination (distributed peer review) alongside extraction by traditional journals through citation disadvantage and institutional non-recognition. Sees a pathway forward but faces institutional suppression (universities don't count preprints toward tenure). Moderate organized power with mobile exit produces tangled rope classification.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PEER REVIEW INSTITUTION (PITON) — The double-blind review ritual is largely performative. Reviewers are notoriously biased by author reputation, institutional affiliation, and novelty aversion. The anonymity is frequently pierced. The review timeline is theater — decisions often reflect gut reactions, not careful evaluation. The institution persists through inertia: it is the established way things are done, and alternatives have not fully replaced it. High theater ratio (0.68) reflects this performative maintenance.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some quality filtering is inherent to scientific communication: not all claims can be published; not all voices carry equal weight; legitimacy requires consensus-building. This perspective naturalizes the gatekeeping system as a law of knowledge production. However, structural data contradicts the mountain gate — the constraint exhibits active enforcement, clear beneficiaries and victims, and suppression above 0.05. The analytical observer risks false summitry: naturalizing a contingent institutional arrangement as an immutable feature of knowledge itself.
constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_journal_peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_journal_peer_review_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_journal_peer_review_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_journal_peer_review_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(academic_journal_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint extracts career advantages for established researchers during the 2-7 year publication-to-career-credit pipeline, and extract prestige/revenue for major publishers. However, extractiveness is not as extreme as a pure snare (0.66+) because genuine coordination occurs — peer review catches errors, establishes priority, and filters quality. The value reflects that the system does coordinate while also gatekeeping. Suppression (0.65): High. Early-career researchers face significant barriers: publication timelines of 1-3 years per cycle, career penalties for non-prestige venues, metrics that devalue preprints and open review, funding that privileges high-impact publication history, and institutional prestige that depends on high-tier journal publication. Outsider researchers face additional suppression through desk rejections, reviewer bias against non-mainstream approaches, and citation disadvantage. Theater ratio (0.68): High. Peer review contains substantial performative elements: double-blind anonymity is frequently pierced; reviewer assessments are notoriously biased by author reputation, institutional affiliation, and intuition about novelty; desk rejections are based on editorial fit rather than careful review; reviewer turnaround reflects availability rather than quality assessment. The performative content has increased as review workload has increased and reviewer compensation has remained zero. Measurements show both theater_ratio and base_extractiveness increasing over 40 years, indicating Goodhart drift — prestige metrics have become more dominant, creating incentive for performative gatekeeping rather than functional quality control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across the six classification types. Early-career researchers (powerless/trapped) experience it as a Snare — pure extraction with no coordination benefit they can access before their career commitment window closes. Outsider researchers (powerless/identity_locked) also experience Snare, but the binding mechanism is internal identity fusion rather than external barriers — this distinction is diagnostically crucial because it identifies where reframing intervention could shift exit options. The established research group (institutional/arbitrage) experiences it as Rope — they are solving the genuine problem of validating research through peer scrutiny, and gatekeeping advantage accrues in their direction. The field-level community (moderate/constrained) experiences it as Tangled Rope — the constraint both coordinates (necessary quality filtering) and extracts (gatekeeping delay and prestige advantage). The open science movement (organized/mobile) also experiences Tangled Rope but with greater agency and a visible exit pathway. The peer review institution itself (institutional/arbitrage) experiences it as Piton — recognizing that the review ritual is largely performative but unable to fully exit because alternatives have not completely replaced it. The civilizational analytical observer (analytical/analytical) risks Mountain classification — naturalizing gatekeeping as an immutable law of knowledge production — but the structural data (active enforcement, beneficiaries, victims, suppression, increasing extractiveness) contradicts the mountain gate and reveals this as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the constraint. Early-career researchers are victims without exit options (trapped) → high d → high f(d) → high experienced extractiveness (χ). Outsider researchers are victims but with structural mobility that is blocked by identity lock (identity_locked exit) → derived d ≈ 0.89 (between trapped and constrained, reflecting that structural exit exists but identity frame prevents exercising it) → high f(d) ≈ 1.28 → high χ. Established researchers are beneficiaries with arbitrage options → low d ≈ 0.05-0.15 → negative/near-zero f(d) ≈ -0.12 to 0.00 → negative or minimal χ (they experience the constraint as net positive). Publishers are beneficiaries with arbitrage (can adjust journal portfolios, subscription models) → low d → negative f(d) → negative χ. The field community with constrained exit and mixed beneficiary/victim status → moderate d ≈ 0.50-0.55 → moderate f(d) ≈ 0.65-0.75 → moderate χ. The open science movement with mobile exit → higher d (they can exit the traditional system but face institutional suppression) ≈ 0.55 → moderate-high f(d) ≈ 0.75 → moderate χ reflecting both their agency and institutional resistance. The directionality chain produces the perspectival gap: same constraint, same ε (0.54), but radically different experienced χ based on position (d) and time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint resolves the false choice between 'coordination' and 'extraction' by showing that both mechanisms are simultaneously present and structurally independent. The coordination function (peer review catches errors, filters quality, establishes priority) is real and significant. The extraction function (gatekeeping delays publication, suppresses outsider voices, advantages prestige-network members) is also real and significant. Tangled Rope is precisely the category for constraints where both functions coexist and neither is reducible to the other. The extractiveness (0.54) is partially explained by the extraction mechanism (victim-side delay, network advantage) and partially by the coordination function's implementation cost (reviewer time, editorial overhead, revision cycles). The theater ratio (0.68) reveals that much of the formal gatekeeping apparatus is performative rather than functional — the coordination is real, but the mechanism by which it is performed has become decoupled from its function. Mandatrophy is NOT resolved here by claiming 'it's really just coordination' (false Rope classification) or 'it's really just extraction' (false Snare classification). Rather, it is resolved by recognizing that Tangled Rope is the structural reality: a genuine coordination mechanism that has accumulated extractive overlay and performative theater, creating a hybrid that does coordinate but at high cost. The measurement trajectory (extractiveness 0.38 → 0.54, theater 0.48 → 0.68) shows Goodhart drift: as prestige metrics have become more central to institutional evaluation, the extraction and performance components have grown while the coordination component has remained relatively constant. This is the signature of a Tangled Rope degrading toward Snare — the constraint is accumulating extraction without losing its coordination function, making it a prime candidate for structural reform (unbundle coordination from extraction through alternative peer review models) rather than abandonment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of peer review''s extraction is necessary cost of coordination versus unnecessary gatekeeping extraction?',
    'Comparative analysis of pre-publication dissemination timelines across disciplines with different review cultures; citation impact correlation with review rigor metrics; researcher survey data on whether review quality justifies delays',
    'If coordination cost is 60%+ of observed extraction: classification shifts toward Rope for more perspectives. If coordination cost is <30%: classification shifts toward Snare for more perspectives, revealing pure gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Proportion of peer review delay that reflects necessary quality control versus unnecessary gatekeeping').

omega_variable(
    prestige_signal_functionality,
    'Does journal tier actually predict research quality and impact, or is prestige self-perpetuating through citation bias and institutional affiliation loops?',
    'Longitudinal citation analysis controlling for author reputation and institutional affiliation; comparison of citation trajectories for identical research published in high vs low-prestige venues; measurement of prestige decay when same author publishes in non-prestigious venues',
    'If prestige predicts quality: peer review gatekeeping serves genuine coordination function (more Rope classifications). If prestige is self-perpetuating: gatekeeping is pure extraction mechanism (more Snare classifications). If mixed: Tangled Rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_signal_functionality, empirical, 'Whether journal prestige correlates with research quality or represents self-perpetuating bias').

omega_variable(
    identity_lock_mechanism_specificity,
    'For outsider researchers using identity_locked exit option: is the identity lock primarily internalized prestige aspiration, career ecosystem requirement, or epistemic framework capture?',
    'Qualitative research on researcher decision-making around publication venue; correlation between cultural/institutional prestige messaging and individual publication strategy choices; analysis of researchers who have successfully broken identity lock and the triggers for reframing',
    'If primarily aspirational internalization: identity_locked classification is correct, and reframing intervention could shift exit_options to mobile. If primarily career ecosystem requirement: trapped classification is more accurate, and exit requires systemic change. If epistemic capture: constraint persists even after institutional barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, empirical, 'Mechanism binding outsider researchers to prestige publication through identity rather than external barriers').

omega_variable(
    alternative_verification_sufficiency,
    'Do alternative peer review models (arXiv overlay journals, decentralized peer review, post-publication open commentary) achieve equivalent or superior error-detection compared to traditional journals?',
    'Comparison of retraction/correction rates between traditional journals and overlay journals for equivalent papers; measurement of quality assessment accuracy in decentralized vs centralized review; user surveys on trust and citation behavior',
    'If alternatives are equivalent or superior: open science exit is genuine, and the Scaffold perspective''s sunset is real. If alternatives show systematic quality deficits: traditional gatekeeping retains functional justification (Rope shifts upward in strength).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether alternative peer review systems provide equivalent quality filtering to traditional journals').

omega_variable(
    early_career_career_path_malleability,
    'Can early-career researchers successfully exit traditional journal gatekeeping (pursue alternative venues, preprints, non-academic careers) without sacrificing long-term career prospects?',
    'Longitudinal career tracking of researchers who publish primarily in non-prestige venues or preprints; measurement of tenure/hiring success rates; analysis of salary and prestige outcomes after 5-10 years',
    'If exit is genuinely possible without cost: early-career researchers are constrained, not trapped (shift to constrained exit_options, reduce Snare pressure). If exit consistently carries career penalty: trapped classification is accurate, and suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_career_career_path_malleability, empirical, 'Whether early-career researchers can exit traditional publishing without career penalty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_journal_peer_review_gatekeeping, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ajprg_tr_t0, academic_journal_peer_review_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ajprg_tr_t20, academic_journal_peer_review_gatekeeping, theater_ratio, 20, 0.58).
narrative_ontology:measurement(ajprg_tr_t40, academic_journal_peer_review_gatekeeping, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(ajprg_be_t0, academic_journal_peer_review_gatekeeping, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ajprg_be_t20, academic_journal_peer_review_gatekeeping, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(ajprg_be_t40, academic_journal_peer_review_gatekeeping, base_extractiveness, 40, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_journal_peer_review_gatekeeping, information_standard).
narrative_ontology:affects_constraint(academic_journal_peer_review_gatekeeping, research_funding_prestige_concentration).
narrative_ontology:affects_constraint(academic_journal_peer_review_gatekeeping, university_ranking_metric_gaming).
narrative_ontology:affects_constraint(academic_journal_peer_review_gatekeeping, early_career_researcher_precarity).

% DUAL FORMULATION NOTE:
% Academic journal gatekeeping can be decomposed into two structurally distinct constraints: (1) quality_filtering_coordination (ε≈0.15, pure Rope) — the genuine error-checking and priority-establishment function of peer review; and (2) prestige_extraction_mechanism (ε≈0.58, pure Snare) — the gatekeeping delay and citation-advantage extraction that has accumulated over 40 years. The unified 'peer review gatekeeping' story (ε=0.54) represents the empirically observable mixed constraint. Decomposition would require separate stories for pure coordination vs pure extraction, each with different measurement trajectories. The current story models them as an integrated Tangled Rope rather than decomposing, because they operate through the same institutional apparatus (the journal review system) and cannot be cleanly separated without institutional reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_journal_peer_review_gatekeeping, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
