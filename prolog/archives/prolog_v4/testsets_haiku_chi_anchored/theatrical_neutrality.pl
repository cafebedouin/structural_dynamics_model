% ============================================================================
% CONSTRAINT STORY: theatrical_neutrality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theatrical_neutrality, []).

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
 *   constraint_id: theatrical_neutrality
 *   human_readable: Theatrical Neutrality: The View from Nowhere as Epistemic Extraction
 *   domain: epistemology/media/communications
 *
 * SUMMARY:
 *   Theatrical neutrality — the presentation of asymmetric claims as equally
 *   valid 'perspectives' to avoid charges of bias — represents a hybrid
 *   extraction mechanism that uses the language of fairness and non-curation
 *   to systematically degrade epistemic discernment. The constraint operates
 *   at the intersection of platform incentives (liability reduction, audience
 *   expansion, algorithmic simplicity), institutional norms (editorial
 *   neutrality doctrine), and reader vulnerability (cognitive load, trust
 *   erosion). The 'view from nowhere' is performative: all platforms make
 *   editorial choices (through curation, algorithm ranking, or moderation),
 *   but the neutrality frame masks these choices, converting them into
 *   non-decisions. This allows asymmetric claim makers to access audiences
 *   and credibility without bearing the burden of evidence, while readers
 *   lose the ability to discern truth, and truth-tracking communities bear
 *   the cost of continuous correction. The constraint exhibits a perspectival
 *   range from pure snare (reader powerless/trapped) to rope (platform and
 *   beneficiary experiencing pure coordination) to piton (degraded
 *   institutional ritual). The theater_ratio (0.81) reflects that the
 *   'multiple perspectives' framing is substantially performative: the
 *   selection of which perspectives to present, how much prominence to grant,
 *   and what context to provide are all curation acts masked by the
 *   neutrality claim.
 *
 * KEY AGENTS:
 *   - Readers Seeking Truth: Primary victims (powerless/trapped) — cannot exit platform without abandoning information access; cannot distinguish signal from noise under neutrality framing
 *   - Truth-Tracking Communities: Secondary victims (moderate/constrained) — bear correction overhead; also constrained by platform norms that prevent them from editorial judgment without appearing biased
 *   - Publication Platforms: Primary beneficiaries (institutional/arbitrage) — reduce liability, maximize audience reach, avoid editorial decision costs through neutrality frame
 *   - Asymmetric Claim Makers: Secondary beneficiaries (institutional/arbitrage) — gain platform access and credibility without evidence burden through neutrality framing
 *   - Critical Literacy Movement: Organized agents (organized/constrained) — building alternative pathways (media literacy, epistemic auditing, source verification) with generational sunset logic
 *   - Editorial Neutrality Doctrine: Institutional doctrine (institutional/arbitrage) — persists through inertia despite being performative; masks genuine curation as non-curation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent institutional choice ('present all perspectives') as an immutable epistemic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theatrical_neutrality, 0.52).
domain_priors:suppression_score(theatrical_neutrality, 0.68).
domain_priors:theater_ratio(theatrical_neutrality, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theatrical_neutrality, extractiveness, 0.52).
narrative_ontology:constraint_metric(theatrical_neutrality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(theatrical_neutrality, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theatrical_neutrality, tangled_rope).
narrative_ontology:human_readable(theatrical_neutrality, "Theatrical Neutrality: The View from Nowhere as Epistemic Extraction").
narrative_ontology:topic_domain(theatrical_neutrality, "epistemology/media/communications").

domain_priors:requires_active_enforcement(theatrical_neutrality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theatrical_neutrality, asymmetric_claim_makers).
narrative_ontology:constraint_beneficiary(theatrical_neutrality, publication_platforms).
narrative_ontology:constraint_victim(theatrical_neutrality, reader_epistemic_discernment).
narrative_ontology:constraint_victim(theatrical_neutrality, truth_tracking_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE READER (SNARE) — Cannot exit the neutrality frame without abandoning the platform; cannot distinguish signal from noise when asymmetric claims are presented as equipoise. d≈0.96, f(d)≈1.42, σ=1.2 → χ≈0.88. Maximum extraction: epistemic discernment is systematically degraded.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRUTH-TRACKING COMMUNITY (TANGLED ROPE) — Constrained by platform norms and audience reach; also benefits from the neutrality frame as protection against charges of bias when their own claims are presented. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.60. Mixed: coordination (shared norms against censorship) and extraction (coordinated falsehood).
constraint_indexing:constraint_classification(theatrical_neutrality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLICATION PLATFORM (ROPE) — Experiences neutrality as pure coordination: avoiding editorial judgment reduces liability, maximizes audience reach, and solves the collective action problem of 'who decides truth?' d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary through reduced friction.
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASYMMETRIC CLAIM MAKER (ROPE) — Benefits from the neutrality frame, which grants platform access and audience reach without burden of evidence. Experiences the constraint as enabling coordination: their claims receive equal treatment to well-evidenced claims. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CRITICAL LITERACY MOVEMENT (SCAFFOLD) — Organized response (media literacy campaigns, epistemic auditing, source verification tools) creating alternative verification pathways. Sees neutrality as temporary institutional failure with sunset through widening critical capacity. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.18. Low effective extraction because organized agents see an exit path and timeline.
constraint_indexing:constraint_classification(theatrical_neutrality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EDITORIAL NEUTRALITY DOCTRINE (PITON) — The 'view from nowhere' is a theater that masks editorial choice. Even algorithmic curation makes selection. The doctrine persists through institutional inertia (editorial liability concerns, audience expansion incentives) despite the pretense of non-curation being technically false. theater_ratio=0.81 satisfies piton gate. The doctrine sees itself as degraded — maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(theatrical_neutrality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, presenting multiple perspectives is epistemically valuable and inherent to knowledge systems: science works by testing competing claims, journalism covers multiple sides. This perspective risks naturalizing the contingent institutional norm (equal treatment regardless of evidence) as an immutable epistemic law. However, structural data (ε=0.52, suppression=0.68, theater=0.81) contradicts the mountain classification — the engine will detect this as a false summit, revealing that the 'multiple perspectives' framing naturalizes what is actually an extractive institutional choice.
constraint_indexing:constraint_classification(theatrical_neutrality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theatrical_neutrality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theatrical_neutrality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theatrical_neutrality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theatrical_neutrality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(theatrical_neutrality, TR),
    TR >= 0.70.

:- end_tests(theatrical_neutrality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The neutrality frame systematically impairs the reader's ability to identify truth, but the extraction is not total — motivated readers can perform additional research, communities provide counter-narratives, and literacy interventions reduce vulnerability. The value reflects significant but not complete degradation of epistemic capacity. Suppression (0.68): High. Significant barriers include platform algorithmic lock-in, cost of alternative information sources, cognitive load in distinguishing claims, and social pressure to accept platform-provided perspectives. The reader cannot easily exit or access better information without substantial effort. Theater ratio (0.81): Very high. The neutrality frame is substantially performative. Platforms DO make editorial choices (algorithm ranking, which claims to surface, response time to moderation), but these are presented as non-decisions. The performance has intensified as platforms have become central information infrastructure — the stakes of appearing to make editorial judgments (and thus appearing biased) have increased, driving up theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Platforms and asymmetric claim makers see pure coordination (Rope) — they experience neutrality as solving the problem of 'who decides what is true?' without imposing their judgment. Readers see pure extraction (Snare) — their epistemic capacity is systematically impaired with no exit option. Truth-tracking communities see mixed coordination and extraction (Tangled Rope) — they benefit from norms against censorship but bear costs of circulating false claims. The critical literacy movement sees a temporary institutional problem with a sunset (Scaffold) — media literacy and epistemic auditing are building alternative information pathways. The editorial doctrine sees itself as degraded (Piton) — neutrality persists through institutional inertia and liability concerns, not because it works. The analytical observer risks seeing an immutable epistemic law (false Mountain) — 'multiple perspectives are always valuable' — but the structural data reveals this as a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Readers: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. Truth-tracking communities: Victim + constrained → d≈0.72, f(d)≈1.15. High extraction; communities cannot fully exit platform access norms. Publication platforms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; have full exit option (could implement editorial standards) but choose not to. Asymmetric claim makers: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; gain platform access without evidence burden. Critical literacy movement: Organized + constrained → d≈0.42, f(d)≈0.42. Low effective extraction; organized agents building alternative pathways with visible sunset timeline. Editorial doctrine: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater_ratio gate, not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector engages — observer naturalizes contingent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the 'is this coordination or extraction?' question by showing that platforms USE the language of coordination ('fairness,' 'non-curation,' 'multiple perspectives') while implementing extractive mechanisms ('algorithm ranking asymmetric claims higher for engagement,' 'reducing friction for false-claim circulation'). The mandatrophy arises because the coordination framing (avoiding editorial bias) is genuine institutional motivation, but the outcome (degraded epistemic discernment) is extraction. The tangled_rope classification resolves this: YES, there is genuine coordination (solving the 'who decides?' problem) AND YES, there is asymmetric extraction (readers lose epistemic capacity, asymmetric claim makers gain unchecked platform access). The constraint is not a rope pretending to be a snare — it is structurally both. The presence of active enforcement (platform choice to maintain neutrality norms rather than implement evidential curation) and victims (readers, truth-tracking communities) with beneficiaries (platforms, asymmetric claim makers) confirms tangled_rope over rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equipoise_definition_boundary,
    'What empirical threshold distinguishes genuine epistemic equipoise (two well-developed competing claims) from manufactured equipoise (asymmetric evidence presented as balanced)?',
    'Comparative analysis: bibliometric review of citation networks, evidence quality scores, and expert consensus indicators for claimed-equipoise vs non-equipoise domains',
    'If threshold can be precisely defined: platforms can automate detection (Scaffold perspective confirmed). If boundary is inherently contested: equipoise is a power question disguised as an epistemic one (Snare perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equipoise_definition_boundary, empirical, 'Whether genuine vs manufactured epistemic equipoise can be distinguished').

omega_variable(
    reader_capacity_adaptation,
    'Do readers actually develop more robust truth-discernment under continuous exposure to conflicting claims, or does cognitive load cause epistemic collapse (belief in ''all claims equally uncertain'')?',
    'Longitudinal cognitive studies: epistemic confidence, false-claim detection rates, and belief formation under repeated exposure to conflicting narratives; comparison of critical literacy outcomes in different information environments',
    'If adaptation succeeds: critical literacy movement''s scaffold perspective is structural (readers can learn). If capacity degrades: the snare perspective is confirmed — readers cannot escape the trap through education alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_capacity_adaptation, empirical, 'Whether reader cognitive capacity adapts to conflicting claims or collapses').

omega_variable(
    neutrality_as_choice_observability,
    'Can the performative nature of neutrality (that platform curation IS a choice, despite neutrality claims) be made sufficiently visible to readers that the theatrical nature is exposed?',
    'Experimental design: transparency interventions (explicit disclosure of algorithmic curation, evidence quality metadata, expert consensus labeling) and measurement of reader perception change; adoption of transparency standards across platforms',
    'If exposed successfully: theater_ratio declines, constraint may reclassify to Rope or Tangled Rope. If exposure fails: theater persists, Piton classification confirmed, institutional inertia dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_as_choice_observability, empirical, 'Whether curation choice can be made sufficiently visible to undermine theater').

omega_variable(
    asymmetric_claim_cost_absorption,
    'Who absorbs the epistemic and social costs of sustained false-claim circulation — the reader, the community, or the platform?',
    'Cost accounting: public health impacts of health misinformation, erosion of institutional trust, researcher reputation damage, and correction overhead; comparison with platforms that actively filter asymmetric claims',
    'If costs fall on readers/community: pure Snare (victims trapped, no exit). If platforms absorb costs: incentives shift (constraints tighten or relax depending on cost tolerance). If costs are externalized: extraction mechanism is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_claim_cost_absorption, empirical, 'Attribution of epistemic and social costs of false-claim circulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theatrical_neutrality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theat_tr_t0, theatrical_neutrality, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theat_tr_t5, theatrical_neutrality, theater_ratio, 5, 0.72).
narrative_ontology:measurement(theat_tr_t10, theatrical_neutrality, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(theat_be_t0, theatrical_neutrality, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(theat_be_t5, theatrical_neutrality, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(theat_be_t10, theatrical_neutrality, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theatrical_neutrality, information_standard).
narrative_ontology:affects_constraint(theatrical_neutrality, epistemic_trust_erosion).
narrative_ontology:affects_constraint(theatrical_neutrality, asymmetric_information_advantage).
narrative_ontology:affects_constraint(theatrical_neutrality, algorithmic_amplification_bias).

% DUAL FORMULATION NOTE:
% Theatrical neutrality is downstream of platform business model constraints (engagement-driven ranking, liability reduction incentives). It is upstream of epistemic trust erosion and asymmetric information advantage. The three constraints form a causal chain: business model → neutrality frame → trust collapse. Decomposition based on distinct ε values: theatrical_neutrality (ε=0.52) focuses on the performance mechanism; epistemic_trust_erosion (higher ε, more extraction) focuses on reader outcome; asymmetric_information_advantage (ε≈0.65, more extraction) focuses on beneficiary outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(theatrical_neutrality, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
