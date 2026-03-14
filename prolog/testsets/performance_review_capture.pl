% ============================================================================
% CONSTRAINT STORY: performance_review_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_review_capture, []).

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
 *   constraint_id: performance_review_capture
 *   human_readable: Performance Review Capture in Organizational Hierarchies
 *   domain: organizational_economics/human_resources
 *
 * SUMMARY:
 *   Performance review systems in hierarchical organizations serve dual
 *   functions: legitimate coordination of performance expectations and
 *   resource allocation (the rope/tangled rope component), and extraction of
 *   compliance, control, and discretionary authority concentration (the
 *   snare/piton component). The constraint exhibits all six classification
 *   types from different structural positions, making it a diagnostic
 *   exemplar for institutional capture mechanisms. Base-level workers
 *   experience performance reviews as snares — outcomes drive compensation
 *   and career progression with minimal transparency and maximum supervisor
 *   discretion. Supervisors experience reviews as coordination tools that
 *   legitimately enable performance management while amplifying their
 *   authority. HR and organizational leadership maintain review systems that
 *   mix genuine coordination (distributing performance data enables talent
 *   allocation) with theater (documenting fairness procedures protects the
 *   organization from liability despite often producing predetermined
 *   outcomes). The theater ratio of 0.68 reflects that performance review
 *   processes increasingly emphasize procedural compliance and documentation
 *   of 'fair process' over actual merit assessment. The extractiveness has
 *   increased from 0.38 to 0.58 over the measurement interval (0-10 years),
 *   driven by regulatory expansion, litigation risk, and the layering of
 *   compliance requirements onto the core review function. This pattern is
 *   consistent with Goodhart drift: as the review system became subject to
 *   legal scrutiny and fairness audits, the system optimized for appearing
 *   fair rather than being fair, incrementally replacing merit assessment
 *   with theater.
 *
 * KEY AGENTS:
 *   - Rank-and-File Workers: Primary victims (powerless/trapped) — economic dependency, opaque outcomes, supervisor discretion, minimal exit options
 *   - Supervisory Management: Primary beneficiary (institutional/arbitrage) — review authority amplifies managerial power; can exit by changing roles or firms
 *   - Organizational Power Centers (C-Suite/HR Leadership): Institutional actor with dual role (powerful/mobile) — design and maintain system that coordinates performance data while enabling capture; active enforcement sustains both functions
 *   - HR/Compliance Apparatus: Institutional actor (organized/constrained) — maintains review theater through liability aversion and regulatory compliance; sees own process as degraded (piton perspective)
 *   - Merit Allocation Integrity: Abstract victim (powerless/trapped) — collective good that cannot organize; bears cost of capture through outcome variance driven by preference rather than performance
 *   - Performance Transparency Movement: Organized alternative actors (organized/mobile) — building sunset mechanisms through continuous feedback, peer review, algorithmic scoring, and transparent criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_review_capture, 0.58).
domain_priors:suppression_score(performance_review_capture, 0.65).
domain_priors:theater_ratio(performance_review_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_review_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_review_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(performance_review_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_review_capture, tangled_rope).
narrative_ontology:human_readable(performance_review_capture, "Performance Review Capture in Organizational Hierarchies").
narrative_ontology:topic_domain(performance_review_capture, "organizational_economics/human_resources").

domain_priors:requires_active_enforcement(performance_review_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_review_capture, supervisory_management).
narrative_ontology:constraint_beneficiary(performance_review_capture, organizational_power_centers).
narrative_ontology:constraint_victim(performance_review_capture, rank_and_file_workers).
narrative_ontology:constraint_victim(performance_review_capture, merit_allocation_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EVALUATED WORKER (SNARE) — Trapped by economic dependency and lack of exit routes. Performance review outcomes directly determine compensation, advancement, and reference quality. Worker cannot exit the evaluation regime without abandoning career trajectory. Review outcomes are opaque, subject to supervisor discretion, and non-transparent in their derivation. High experienced extraction — the worker's livelihood is hostage to supervisor preference.
constraint_indexing:constraint_classification(performance_review_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUPERVISOR/MIDDLE MANAGEMENT (ROPE) — Experiences performance review as a coordination mechanism. The review system enables communication of expectations, documentation of performance, and alignment of incentives. Supervisors can exit by changing firms or roles without losing the skill of conducting reviews. Net beneficiary position — reviews amplify supervisor authority while enabling legitimate performance management coordination.
constraint_indexing:constraint_classification(performance_review_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZATIONAL POWER CENTER (TANGLED ROPE) — Executive leadership and HR functions design and maintain the review system. They genuinely need performance data for organizational coordination and talent allocation (coordination function). Simultaneously, they extract organizational value through review capture: reviews become theater for documenting compliance, managing liability, and justifying predetermined outcomes rather than measuring merit. Active enforcement of review processes maintains both the coordination function and the extraction layer. Mobile exit options (senior executives move freely between organizations) but choose to maintain the system because it concentrates power.
constraint_indexing:constraint_classification(performance_review_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: HR/COMPLIANCE APPARATUS (PITON) — Performance review systems persist largely as theater for regulatory compliance, liability protection, and procedural documentation. The primary function — identifying and rewarding merit — has atrophied while the compliance ritual (documentation, fairness procedures, appeal channels) has expanded. Theater ratio high (0.68): much activity is performative verification of 'fair process' rather than actual merit assessment. HR departments maintain the ritual through institutional inertia and legal risk aversion, not because the system effectively coordinates performance.
constraint_indexing:constraint_classification(performance_review_capture, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PERFORMANCE TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized actors (management consultants, tech firms piloting radical transparency, open-source governance models) are experimenting with alternative evaluation pathways: continuous feedback, peer review, algorithmic performance tracking, public performance data. These are building sunset mechanisms for traditional annual review capture. Organizations using real-time feedback, 360-degree review, and transparent criteria see lower theater ratios and measurably different distributions of outcomes. This is a genuine temporary support structure — it will sunset as either (a) transparency mechanisms fully replace capture-prone annual reviews, or (b) new forms of capture evolve around the transparency mechanisms themselves.
constraint_indexing:constraint_classification(performance_review_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, some performance assessment is inherent to any large coordinated system: agents must be differentiated by contribution, and those differentiations affect resource allocation. The bottleneck appears as a natural consequence of hierarchical organization itself — if you have hierarchy, you must have evaluation; if you have evaluation, capture is inevitable. This perspective naturalizes capture as an unavoidable organizational law. However, the structural data contradicts the mountain classification: organizations with radical transparency, peer review, and open metrics show measurably lower capture. The 'invariance of capture' is a false summit — it naturalizes contingent institutional choices.
constraint_indexing:constraint_classification(performance_review_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_review_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_review_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_review_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_review_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_review_capture, TR),
    TR >= 0.70.

:- end_tests(performance_review_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The core extraction mechanism is supervisor discretion over outcomes that determine worker compensation and advancement. Unlike snares with extractiveness > 0.70, performance review capture is not pure extraction — legitimate performance differentiation and organizational coordination are genuinely present. The extraction layer (supervisor preference, in-group bias, arbitrary outcome variance) is substantial but sits atop the coordination function. The measurement trajectory (0.38 → 0.58) reflects increasing theater ratio and declining actual merit content as organizations added compliance requirements and procedural documentation. Suppression (0.65): High. Workers face multiple suppression mechanisms: economic dependency (alternative employment scarce or lower-paid), information asymmetry (review criteria opaque and supervisor discretion high), reputational risk (negative reviews damage reference quality), and institutional power concentration (appeal mechanisms exist but decision-makers are organizational insiders). Suppression is not total because some workers can and do exit, and union/collective action provides occasional countervailing power. Theater ratio (0.68): High and increasing. The measurement trajectory shows theater rising from 0.42 to 0.68 as organizational reviews added compliance documentation, fairness procedures, and liability-protection language while retaining core opacity in actual evaluation logic. Modern reviews often include extensive procedural theater — rubrics, training requirements, documentation standards, appeal processes — that create the appearance of rigor while preserving supervisor discretion in outcome determination.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across power positions. The powerless worker sees a snare (outcomes are opaque, extraction is high, exit is impossible). The supervisor sees a rope (reviews are a coordination tool that enables performance communication and legitimate authority). The organizational leadership sees a tangled rope (genuine performance coordination mixed with extraction through discretionary authority concentration). The HR system sees a piton (the review process is increasingly performative theater maintained by legal/compliance inertia). The transparency movement sees a scaffold (alternative mechanisms with sunset logic are emerging). The civilizational observer risks seeing a mountain (hierarchies require evaluation, capture is inevitable) — but the structural data contradicts this: organizations with radical transparency and peer review show measurably lower capture, revealing the mountain as naturalization of contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from structural position relative to the extraction flow. Powerless workers with trapped exit options (d ≈ 0.95) experience near-maximal effective extractiveness. Supervisors as beneficiaries with arbitrage exit options (d ≈ 0.10) experience low effective extraction — they benefit from the system's authority concentration. Organizational leadership with mobile exit options but active system maintenance (d ≈ 0.50) experience moderate extraction weighted toward beneficiary (they designed the system). HR/compliance apparatus with organizational entrenchment (d ≈ 0.45) experience moderate extraction as they maintain theater that serves power centers while providing legal cover. The performance transparency movement with organized power and mobile exit (d ≈ 0.35) experience low-moderate extraction because they are building alternatives. The analytical observer at civilizational scope (d ≈ 0.72) experiences high extraction if using analytical position to naturalize hierarchical capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that classification depends entirely on structural position and measurement scope. No single type is 'correct' — all six are legitimate perspectival readings. The mandatrophy arises from asking 'is performance review capture inherent to large organizations or contingent on hierarchical structures?' The mountain classification (hierarchies require capture, this is natural law) is a false summit because organizations with radical transparency, peer review, and algorithmic scoring show measurably lower capture variance. The constraint is not immutable — it is contingent on specific institutional choices (hierarchical authority, supervisor discretion, opaque criteria). The resolution reveals that what appeared as 'natural organizational law' was actually the naturalization of a contingent power distribution. Organizations can reduce capture by changing structural features: increasing transparency, distributing evaluation authority (peer review), using algorithmic scoring with open criteria, and enabling worker voice in outcome review. These are not just different perspectives on the same immutable constraint — they are different constraints with different ε values. A transparent peer-review system is structurally distinct (lower ε, different beneficiary/victim distributions) from supervisor-driven capture. The framework's job is to force the distinction: either the organization has outcome transparency and distributed authority (a different constraint, lower ε), or it maintains opaque supervisor-discretionary review (this constraint, ε=0.58).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_versus_supervisor_preference_correlation,
    'To what degree does measured performance review outcome variance track actual performance differences versus supervisor preference, personality alignment, and in-group favoritism?',
    'Longitudinal tracking of review outcomes against objective performance metrics (sales, output, quality); correlation analysis of reviews with subsequent employment trajectories and retention; cross-supervisor variance analysis for identical job roles.',
    'If high correlation with objective metrics: extraction is moderate and reviews serve legitimate coordination. If low correlation: reviews are primarily theater for supervisor preference, confirming snare classification from worker perspective. High in-group preference concentration would indicate extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_versus_supervisor_preference_correlation, empirical, 'Correlation between review outcomes and objective performance metrics').

omega_variable(
    transparency_mechanism_capture_risk,
    'Do alternative evaluation systems (continuous feedback, peer review, algorithmic scoring) eliminate capture or merely shift it to new mechanisms (algorithm bias, peer cartel, visibility theater)?',
    'Comparative analysis of outcome distributions in transparent vs opaque review systems; tracking of new forms of gaming in transparent systems; longitudinal monitoring of whether outcome variance decreases or shifts in character.',
    'If transparency genuinely reduces capture: scaffold perspective confirmed and sunset mechanisms are real. If capture merely migrates: organizations face the risk of Goodhart drift where the system optimizes for appearing fair rather than being fair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_mechanism_capture_risk, empirical, 'Whether alternative evaluation systems reduce or relocate capture mechanisms').

omega_variable(
    supervisor_identity_lock_mechanism,
    'To what degree is supervisor participation in performance review capture locked into supervisor identity and career advancement logic, versus subject to genuine exit if the system were redesigned?',
    'Survey and interview data on supervisor awareness of review system''s extractive function; longitudinal tracking of whether supervisors who move to transparent-review organizations continue capture behaviors; analysis of whether supervisors resist alternative systems due to preference for power or due to skill/training dependence.',
    'If identity-locked: supervisors would resist alternatives even if offered. If merely incentive-responsive: alternative systems could reduce capture by changing supervisor incentive structure. This distinguishes between institutional capture and cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supervisor_identity_lock_mechanism, conceptual, 'Extent to which supervisor capture participation is identity-locked versus incentive-responsive').

omega_variable(
    organizational_coordination_necessity,
    'How much of the suppression (0.65) is inherent to coordinating complex organizations versus how much is contingent on hierarchical authority structures?',
    'Comparison of suppression metrics across organizational structures: flat teams, decentralized peer governance, market-based task assignment, algorithmic task routing. Measurement of worker agency, exit options, and voice mechanisms in these alternative structures.',
    'If suppression is inherent: large organizations necessarily involve some worker trapped-ness. If contingent: alternative structures can measurably reduce suppression while maintaining coordination, suggesting the trap is not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_coordination_necessity, empirical, 'Decomposition of suppression into inherent coordination cost versus hierarchy-specific trapping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_review_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_rev_tr_t0, performance_review_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(perf_rev_tr_t5, performance_review_capture, theater_ratio, 5, 0.55).
narrative_ontology:measurement(perf_rev_tr_t10, performance_review_capture, theater_ratio, 10, 0.68).
narrative_ontology:measurement(perf_rev_tr_t3, performance_review_capture, theater_ratio, 3, 0.48).

% Extraction over time
narrative_ontology:measurement(perf_rev_be_t0, performance_review_capture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_rev_be_t5, performance_review_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(perf_rev_be_t10, performance_review_capture, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(perf_rev_be_t3, performance_review_capture, base_extractiveness, 3, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_review_capture, resource_allocation).
narrative_ontology:affects_constraint(performance_review_capture, wage_compression_dynamics).
narrative_ontology:affects_constraint(performance_review_capture, organizational_compliance_theater).
narrative_ontology:affects_constraint(performance_review_capture, supervisor_discretion_authority).

% DUAL FORMULATION NOTE:
% Performance review capture decomposes into at least three structurally distinct constraints: (1) outcome variance measurement (does review output track performance or preference?), (2) compliance theater accumulation (increasing procedural requirements without merit content), (3) authority concentration in supervisory discretion. This story models the integrated system; downstream constraints model specific mechanisms and can have different ε values reflecting domain-specific empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_review_capture, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
