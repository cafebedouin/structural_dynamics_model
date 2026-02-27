% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification, []).

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
 *   constraint_id: epistemic_process_of_verification
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The epistemic process of scientific verification represents the canonical
 *   coordination mechanism by which the research community establishes facts:
 *   novel claims must be independently replicated and corroborated before
 *   they are accepted into the reliable knowledge base. This constraint
 *   exists in tension with discovery incentives (which reward novel claims,
 *   often before verification is complete) and resource constraints
 *   (replication consumes time and funding). The constraint is fundamentally
 *   a rope — it solves the collective action problem of distinguishing signal
 *   from noise in a complex empirical landscape. However, the indexical
 *   analysis reveals that different agents experience this constraint
 *   differently: research institutions and the analytical observer see pure
 *   coordination; individual researchers see mixed coordination and
 *   extraction; specific claims awaiting verification see extraction and
 *   suppression; the open science movement sees a temporary constraint with a
 *   sunset; and the journal gatekeeping system exhibits degraded theater. The
 *   constraint's theater ratio (0.38) reflects the degree to which
 *   traditional peer review rituals have become performative rather than
 *   functional, particularly as experimental complexity has outpaced reviewer
 *   expertise.
 *
 * KEY AGENTS:
 *   - Research Community: Primary beneficiary (institutional/arbitrage) — benefits from verification assurance that prevents false positives from contaminating knowledge base
 *   - Epistemic Reliability: Primary beneficiary (analytical/analytical) — abstract collective good ensuring robust knowledge accumulation
 *   - Individual Researchers: Mixed experience (moderate/constrained) — benefit from verification ecosystem protecting their work; constrained by verification delays and career friction
 *   - Specific Claims: Trapped subjects (powerless/trapped) — awaiting verification with no agency; bear extraction cost through publication delays
 *   - Open Science Movement: Organized alternative (organized/mobile) — building distributed verification pathways that reduce dependence on centralized peer review
 *   - Journal Gatekeeping System: Institutional custodian (institutional/arbitrage) — maintains verification ritual; increasingly theatrical as function degrades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification, 0.28).
domain_priors:suppression_score(epistemic_process_of_verification, 0.25).
domain_priors:theater_ratio(epistemic_process_of_verification, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification, extractiveness, 0.28).
narrative_ontology:constraint_metric(epistemic_process_of_verification, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(epistemic_process_of_verification, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification, rope).
narrative_ontology:human_readable(epistemic_process_of_verification, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification, "scientific/epistemology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, epistemic_reliability).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, research_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / IDEAL EPISTEMOLOGY (ROPE) — From a civilizational, universal perspective, the verification constraint is pure coordination: independent replication ensures robust knowledge accumulation and prevents false positives from contaminating the corpus. This perspective sees minimal extraction or coercion — the constraint solves a genuine collective action problem (many agents need assurance the claim is reproducible). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.18. Low effective extraction because the mechanism aligns all interests.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH INSTITUTION (ROPE) — Scientific institutions benefit from the verification constraint as a coordination mechanism: it provides legitimacy, prevents institutional reputational damage, and enables reliable knowledge accumulation that justifies funding. Institutions can exploit arbitrage (claiming results before full verification completes) but generally benefit more from reliable institutional reputation. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.003. Negligible effective extraction; net beneficiary through coordination.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL RESEARCHER (TANGLED ROPE) — Individual researchers experience the verification constraint as both coordination and extraction. Verification provides protection (their work won't be undermined by fraudulent claims) but also creates friction (their own results must pass replication). Career timescales make the constraint feel extractive: verification delays publication, funding, and career advancement. Yet they also benefit from the verification ecosystem protecting their own future claims. d≈0.58, f(d)≈0.76, σ=1.0 → χ≈0.21. Moderate effective extraction at the biographical timescale.
constraint_indexing:constraint_classification(epistemic_process_of_verification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUBJECT OF VERIFICATION (SNARE) — From the perspective of an individual result, claim, or data point awaiting verification, the constraint appears as a snare: the claim is trapped in an extended review process with no exit, facing suppression through skeptical scrutiny, and bearing extraction through publication delays and career cost to the originating researcher. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.39. High effective extraction because this perspective has zero agency.
constraint_indexing:constraint_classification(epistemic_process_of_verification, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, open data advocates, registered reports) see the traditional verification constraint as temporary institutional scaffolding that is being replaced by more efficient mechanisms: distributed preprint scrutiny, real-time open-source replication, and transparent data sharing. The traditional constraint has a sunset: as open science norms mature, centralized peer review verification becomes supplementary rather than gatekeeping. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.12. Low effective extraction because the movement has agency and sees an exit path.
constraint_indexing:constraint_classification(epistemic_process_of_verification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL GATEKEEPING SYSTEM (PITON) — The traditional peer review apparatus for verification is increasingly theatrical: journals maintain review rituals despite evidence that peer review catches only ~10-20% of errors and is often slow, exclusive, and biased. The institutional mechanism persists through inertia rather than functional necessity. theater_ratio=0.38 is borderline for piton (≥0.70 gate), so this perspective shows the constraint transitioning toward piton status. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.014. Institutional beneficiary despite theatrical mechanism; the extraction is minimal because the function is degraded.
constraint_indexing:constraint_classification(epistemic_process_of_verification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_process_of_verification, TR),
    TR >= 0.70.

:- end_tests(epistemic_process_of_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The verification constraint is primarily a coordination mechanism with moderate overhead. The value reflects genuine costs (time, resources, career friction) but these are largely justified by the coordination function. The trajectory shows increasing extractiveness from 0.18 to 0.28 over the interval, indicating that verification friction is growing as experiments become more complex and reproduction becomes more resource-intensive. Suppression (0.25): Moderate-low. The constraint does suppress alternative pathways (a researcher cannot publish without verification) but suppression is not coercive — it is the mechanism itself. Skeptical scrutiny is built-in but not overwhelming; replication groups can succeed by conducting careful work. Theater ratio (0.38): Moderate-low. Traditional peer review contains performative elements (reviewers assessing plausibility rather than verifying results), but the constraint is not primarily theatrical. The increasing trajectory (0.22→0.38) reflects growing gap between reviewer expertise and experimental complexity, indicating theater is accumulating as the field matures.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between the collective epistemic beneficiary (research community, analytical observer) who sees pure coordination, and the individual or specific claim experiencing verification (powerless perspective) who sees extraction and suppression. The rope classification holds at the community and institutional level because verification solves a genuine problem — preventing false positives that would waste collective resources. But at the biographical/immediate timescale, the same constraint appears as mixed extraction and suppression because the individual bears friction costs and has no exit. The open science perspective represents a genuine disagreement about whether the constraint is permanent (rope) or temporary (scaffold) — this depends on empirical questions about whether distributed verification can replace centralized review.
 *
 * DIRECTIONALITY LOGIC:
 *   Research community/institutions: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary; verification mechanism aligns with their interests. Individual researchers: Both beneficiary and victim + constrained → d≈0.58, f(d)≈0.76. Mixed because they benefit from verification protecting their own work but are constrained by verification friction. Specific claims: Victim + trapped → d≈0.90, f(d)≈1.38. Maximum extraction because claims have zero agency in the verification process. Open science movement: Organized + mobile → d≈0.35, f(d)≈0.32. Low extraction because the coalition has agency and perceives an exit path through alternative mechanisms. Journal system: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Derived as beneficiary despite piton classification (theater ≥ 0.70 is expected for piton, but this constraint is transitioning toward piton rather than fully piton; theater_ratio=0.38 is below the piton gate).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating the difference between a genuine coordination mechanism (rope) viewed from the collective/institutional perspective and the same mechanism viewed from the perspective of agents bearing friction costs. The rope classification is defensible at the epistemic collective level — verification does solve a collective action problem and all agents benefit from the ecosystem's existence. However, the perspectival analysis reveals that the beneficiary status is asymmetric: institutions and abstract epistemic goods benefit; individual researchers and specific claims bear costs. The constraint is NOT a snare because: (1) suppression is not high (≥0.60 gate) — researchers can succeed through careful work; (2) there are genuine alternatives forming (open science) — not trapped; (3) most researchers accept the constraint as legitimate rather than perceiving pure predation. The constraint is NOT a tangled rope because: (1) no clear victim group exists beyond those self-selected into research careers; (2) beneficiary and victim are not structural groups but the same agents experiencing different aspects of the system. The rope classification holds because the constraint's primary function is coordination: preventing false positives that would waste collective resources. Individual friction is an unavoidable cost of achieving that coordination, not extraction in the predatory sense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_replication_threshold,
    'What constitutes sufficient independent replication for robust acceptance of a novel claim?',
    'Longitudinal study of retracted vs confirmed discoveries; correlation between number of successful replications and long-term validity; cross-domain analysis of replication rates by claim type',
    'If threshold is too low (1-2 replications): field accepts false positives at high rates. If threshold is too high (10+ replications): verification delays impede legitimate discovery and deter risk-taking in novel directions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_replication_threshold, empirical, 'Sufficient replication threshold for robust claim acceptance').

omega_variable(
    verification_cost_allocation,
    'Should the burden of verification (time, resources, career risk) fall primarily on the originating researcher, replication groups, or the epistemic collective?',
    'Comparative institutional analysis of different funding models (replicate-first vs author-led vs collective funding); measurement of career outcomes under different allocation schemes',
    'If burden on originator: maximum incentive to verify before claiming (low false positive rate) but suppresses high-risk research. If burden on collective: enables discovery but requires public funding of verification. Allocation determines whether constraint is rope (shared burden) or snare (concentrated burden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_allocation, preference, 'Cost allocation for verification burden across epistemic stakeholders').

omega_variable(
    open_verification_sufficiency,
    'Does distributed open-source verification (preprints, GitHub reviews, replication code) provide equivalent epistemic assurance to centralized peer review?',
    'Head-to-head error detection rates between traditional peer review and open-source scrutiny; false positive/negative rates; longitudinal tracking of claim outcomes by verification pathway',
    'If open verification is sufficient: scaffold perspective confirmed, traditional verification is genuinely sunset. If insufficient: open science represents coordination aspiration rather than functional alternative; rope constraint persists with higher theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_verification_sufficiency, empirical, 'Whether open-source distributed verification provides adequate epistemic assurance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epvf_tr_t0, epistemic_process_of_verification, theater_ratio, 0, 0.22).
narrative_ontology:measurement(epvf_tr_t3, epistemic_process_of_verification, theater_ratio, 3, 0.3).
narrative_ontology:measurement(epvf_tr_t6, epistemic_process_of_verification, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(epvf_be_t0, epistemic_process_of_verification, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(epvf_be_t3, epistemic_process_of_verification, base_extractiveness, 3, 0.23).
narrative_ontology:measurement(epvf_be_t6, epistemic_process_of_verification, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification, verification_bottleneck).
narrative_ontology:affects_constraint(epistemic_process_of_verification, publication_bias_against_negative_results).
narrative_ontology:affects_constraint(epistemic_process_of_verification, replication_crisis_in_social_psychology).

% DUAL FORMULATION NOTE:
% The epistemic process of verification is the upstream constraint governing how claims achieve acceptance. It affects downstream constraints (verification bottleneck, publication bias) by establishing the standard that must be met. The replication crisis represents a failure mode of this constraint when verification standards become too weak or too late to catch systematic errors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
