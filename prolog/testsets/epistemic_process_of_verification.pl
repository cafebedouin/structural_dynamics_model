% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The epistemic process of scientific verification — the requirement that
 *   novel claims be independently replicated and corroborated before
 *   acceptance — is a foundational coordination mechanism in science. It
 *   solves the collective action problem of distinguishing genuine
 *   discoveries from noise, fraud, or luck. From the analytical perspective,
 *   this constraint is pure coordination: all agents benefit from living in a
 *   world where false claims are filtered out. Individual researchers benefit
 *   because their work gains credibility through replication. Institutions
 *   benefit because verification protects their reputation and provides an
 *   allocation mechanism. The research community benefits because shared
 *   verification norms enable collective knowledge-building. The constraint
 *   exhibits low extractiveness (0.32) and low suppression (0.28) because it
 *   genuinely serves all participants — the costs of replication are real but
 *   proportional, and exit options exist but are not attractive (unverified
 *   claims lose legitimacy). The theater ratio (0.35) reflects that
 *   verification has developed some performative elements (citation counts,
 *   publication speed) but remains functionally grounded in actual
 *   replication work. This constraint represents the ideal-type Rope: pure
 *   coordination with minimal coercion.
 *
 * KEY AGENTS:
 *   - Individual Researchers: Constrained participant (powerless/constrained) — must replicate others' work, must submit to verification; benefit from credibility conferred by replication
 *   - Research Institutions: Beneficiary with arbitrage (institutional/arbitrage) — protect reputation through verification standards; can set alternative standards but choose not to
 *   - Research Communities: Organized enforcers (organized/constrained) — collectively enforce verification norms; benefit from collective coordination power
 *   - Epistemic Commons: Collective beneficiary (analytical) — shared knowledge base protected by verification requirement
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — sees verification as structural solution to signal/noise problem in distributed epistemic system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification, 0.32).
domain_priors:suppression_score(epistemic_process_of_verification, 0.28).
domain_priors:theater_ratio(epistemic_process_of_verification, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification, extractiveness, 0.32).
narrative_ontology:constraint_metric(epistemic_process_of_verification, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(epistemic_process_of_verification, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification, rope).
narrative_ontology:human_readable(epistemic_process_of_verification, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification, "scientific/epistemology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, epistemic_commons).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, future_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RESEARCHER (ROPE) — Individual researchers are bound by the verification requirement but also benefit from it: they gain credibility when their work passes replication, and they rely on others' verified results. Constrained by the need to replicate others' work before building on it, but this constraint solves the collective action problem of preventing false cascades. Low experienced extraction because the researcher gains legitimacy through the same process.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC INSTITUTION (ROPE) — Universities, funding agencies, and research institutions benefit from the verification norm: it protects their reputation and provides a filtering mechanism for allocating resources. They have arbitrage options (can establish alternative standards, can fund high-risk research without verification) but the verification norm is their best coordination solution. The constraint is experienced as enabling, not extractive.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, independent replication is the core coordination mechanism that transforms individual claims into shared knowledge. The verification process solves the problem of distinguishing signal from noise in a distributed epistemic system. No external extraction; the constraint is structurally pure coordination. This perspective sees the mechanism functioning as designed.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (ROPE) — Organized research communities (societies, consortia, collaborative networks) enforce and benefit from verification norms. They are constrained by the requirement to maintain standards but also gain coordination power through collective enforcement. The mechanism enables collective action that individuals alone could not achieve.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_tests).
:- end_tests(epistemic_process_of_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The verification requirement does impose costs: researchers must spend time on replication, must risk negative results, must submit to peer scrutiny. However, these costs are proportional to benefits and are incurred symmetrically — all participants bear them and all gain from them. The constraint does not extract from one group to benefit another; it extracts value from everyone in order to preserve the commons. Suppression (0.28): Moderate-low. Alternative epistemic standards exist (preregistration, open data, citizen science) and researchers can pursue them. The suppression reflects primarily the friction of coordination itself — the need to achieve consensus on standards — rather than coercive power. Theater ratio (0.35): Low-moderate. Verification involves some performative elements: citation metrics, publication prestige, priority racing. But the core function (actual replication work) remains substantive. The measurement trajectory shows slight increase over 50 years, reflecting gradual drift toward more performative metrics, but the constraint remains functionally grounded.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal — all agents perceive it as Rope. This is characteristic of successful pure coordination mechanisms. The individual researcher sees coordination, the institution sees coordination, the community sees coordination, and the analytical observer sees coordination. The absence of a large perspectival gap is not a weakness but a diagnostic feature: it indicates that the constraint is genuinely solving a collective action problem rather than masking extraction. If one perspective were Snare while others were Rope, the constraint would be tangled_rope or would be experiencing mandatrophy. The uniform Rope classification across perspectives suggests the constraint is functioning as designed.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d from the same structural principle: the verification requirement benefits and constrains symmetrically. Individual researchers experience d ≈ 0.50 (both constrained and beneficiary). Institutions experience d ≈ 0.15 (primarily beneficiary; they have arbitrage options). The research community experiences d ≈ 0.45 (mixed: constrained by need to maintain standards, beneficiary through coordination power). The analytical observer experiences d ≈ 0.72 (analytical position seeing the epistemic necessity without direct participation). None of these values produce high f(d) because none correspond to asymmetric extraction. The derived chi values are all in the 0.20-0.35 range, consistent with pure coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY. This constraint exhibits no confusion between coordination and extraction. All perspectives converge on Rope because the empirical structure is genuinely coordinative: the costs and benefits are symmetrical, the extraction flows are balanced, and no agent experiences asymmetric burden. The verification process is not a coordination mechanism masking extraction; it is coordination. The slight increase in theater_ratio over time (0.25 → 0.35) is worth monitoring — if this trend continues and exceeds 0.70, the constraint would degrade toward Piton. Currently, the functional core remains intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_sufficiency_threshold,
    'How many successful independent replications constitute sufficient verification for acceptance into canonical knowledge?',
    'Meta-analysis of replication datasets; comparison of retraction rates vs empirical success thresholds across disciplines',
    'If threshold is too low: false positives contaminate knowledge. If threshold is too high: legitimate discoveries are delayed indefinitely. The ambiguity affects classification from snare (if threshold is weaponized to delay) vs rope (if threshold is functionally optimal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_sufficiency_threshold, empirical, 'Empirical threshold for sufficient replication count').

omega_variable(
    replication_fidelity_asymmetry,
    'Does the verification process actually detect systematic errors or primarily confirm procedural conformity?',
    'Analysis of replication failure modes; examination of whether negative replications catch substantive errors or merely procedural variations',
    'If verification detects true errors: rope classification holds. If verification is performative (confirms procedure, misses substance): classification shifts toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(replication_fidelity_asymmetry, empirical, 'Whether replication detects substantive errors or procedural conformity').

omega_variable(
    verification_resource_equity,
    'Are verification requirements enforced equally across research domains, institutions, and researcher demographics, or do resource asymmetries create de facto exemptions?',
    'Comparative analysis of retraction rates, replication requirements, and verification timelines across domains; demographic analysis of verification burden',
    'If enforced equally: rope. If resource asymmetries create extraction (well-funded labs skip verification; under-resourced labs bear full burden): tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_resource_equity, empirical, 'Equity of verification enforcement across institutions and demographics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epist_verif_tr_t0, epistemic_process_of_verification, theater_ratio, 0, 0.25).
narrative_ontology:measurement(epist_verif_tr_t25, epistemic_process_of_verification, theater_ratio, 25, 0.32).
narrative_ontology:measurement(epist_verif_tr_t50, epistemic_process_of_verification, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(epist_verif_be_t0, epistemic_process_of_verification, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(epist_verif_be_t25, epistemic_process_of_verification, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(epist_verif_be_t50, epistemic_process_of_verification, base_extractiveness, 50, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification, verification_bottleneck).
narrative_ontology:affects_constraint(epistemic_process_of_verification, publication_bias_replication).

% DUAL FORMULATION NOTE:
% This constraint is the upstream structural mechanism that enables all discipline-specific verification processes. Verification_bottleneck and publication_bias_replication are downstream manifestations where institutional frictions create extraction on top of the coordinative base. All three constraints share the same base epistemic mechanism but differ in whether institutional layers convert it to Tangled Rope or Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
