% ============================================================================
% CONSTRAINT STORY: attribution_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attribution_opacity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: attribution_opacity
 *   human_readable: Attribution Opacity in AI-Generated Text
 *   domain: philosophy_of_mind/social_epistemics/ai_interaction_design
 *
 * SUMMARY:
 *   Attribution opacity in AI-generated text arises from the
 *   information-theoretic properties of stateless digital artifacts. A text
 *   string presented without metadata (timestamps, session identifiers,
 *   cryptographic signatures) contains only its semantic content — authorship
 *   information is not encoded in the symbol sequence itself. This constraint
 *   appears across all observer positions as an immutable structural limit:
 *   users cannot verify misattributions, providers cannot authenticate
 *   unsigned text, standards bodies cannot retroactively verify unsigned
 *   artifacts, and forensic analysts cannot extract authorship from
 *   information that was never encoded. The constraint exhibits the canonical
 *   mountain signature: emerges naturally from information theory, shows
 *   maximum accessibility collapse (no alternative pathway exists to extract
 *   non-existent information), minimal resistance (no agent can overcome the
 *   constraint through effort), and near-zero extractiveness (the constraint
 *   does not systematically benefit any agent class). The small residual
 *   extractiveness (0.08) reflects minor asymmetries: sophisticated actors
 *   can implement signing infrastructure more easily than individuals, and
 *   platform providers control whether attribution mechanisms are exposed to
 *   users. But these asymmetries are second-order effects — the primary
 *   constraint is the absence of information, not its asymmetric
 *   distribution.
 *
 * KEY AGENTS:
 *   - Misattributed User: Powerless/trapped — confronts text falsely attributed to AI with no verification mechanism; experiences constraint as absolute epistemic barrier
 *   - AI Service Provider: Institutional/arbitrage — cannot verify authorship of unsigned text even with system access; could implement signing but cannot retroactively verify
 *   - Standards Body: Organized/mobile — developing attribution standards (C2PA, content credentials) but cannot solve the fundamental problem for unsigned artifacts
 *   - Information Theorist: Analytical/analytical — recognizes constraint as information-theoretic necessity; authorship data not present in stateless text
 *   - Content Moderator: Moderate/constrained — cannot determine AI authorship from text alone; must rely on external context or probabilistic inference
 *   - Forensic Analyst: Powerful/mobile — has technical capability but cannot extract information that does not exist in the artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attribution_opacity, 0.08).
domain_priors:suppression_score(attribution_opacity, 0.03).
domain_priors:theater_ratio(attribution_opacity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attribution_opacity, extractiveness, 0.08).
narrative_ontology:constraint_metric(attribution_opacity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(attribution_opacity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(attribution_opacity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(attribution_opacity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attribution_opacity, mountain).
narrative_ontology:human_readable(attribution_opacity, "Attribution Opacity in AI-Generated Text").
narrative_ontology:topic_domain(attribution_opacity, "philosophy_of_mind/social_epistemics/ai_interaction_design").

domain_priors:emerges_naturally(attribution_opacity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISATTRIBUTED USER (MOUNTAIN) — User confronted with text falsely attributed to an AI system has no technical mechanism to verify authorship without session continuity or cryptographic signatures. The constraint is structural: stateless text lacks provenance metadata. No amount of effort changes this — the information simply does not exist in the artifact.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: AI SERVICE PROVIDER (MOUNTAIN) — Provider cannot verify authorship of text presented without context. Even with full system access, a stateless text snippet contains no intrinsic signature. Could implement cryptographic signing or session tracking, but the unsigned text itself remains opaque. The constraint is in the artifact's information-theoretic properties, not in institutional barriers.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARDS BODY (MOUNTAIN) — Organizations developing AI attribution standards (C2PA, content credentials, watermarking) recognize the constraint as fundamental: unsigned digital text has no intrinsic provenance. Standards can mandate future signing but cannot retroactively verify unsigned artifacts. The constraint emerges from information theory, not from coordination failure.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From an information-theoretic perspective, attribution opacity is a necessary consequence of stateless text representation. A text string without metadata contains only its semantic content — authorship information is not encoded in the symbol sequence itself. Cryptographic signatures or session continuity add information to the system; they do not extract latent information from the text. This is a structural limit, not a policy choice.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CONTENT MODERATOR (MOUNTAIN) — Moderator evaluating whether text violates platform policy cannot determine if it was AI-generated without external context. Stylometric analysis provides probabilistic signals but not verification. The constraint is epistemic: the text artifact does not contain the answer. Moderation policies can require disclosure, but cannot extract authorship from undisclosed text.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORENSIC ANALYST (MOUNTAIN) — Analyst with full technical capability and resources still cannot verify authorship of isolated text without session logs or signatures. Can develop probabilistic models (stylometry, linguistic fingerprinting) but these provide likelihood ratios, not verification. The constraint is in the artifact's information content, not in the analyst's capability.
constraint_indexing:constraint_classification(attribution_opacity, mountain,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attribution_opacity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(attribution_opacity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attribution_opacity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(attribution_opacity, ExtMetricName, E),
    domain_priors:suppression_score(attribution_opacity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(attribution_opacity),
    narrative_ontology:constraint_metric(attribution_opacity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(attribution_opacity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(attribution_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint does not systematically extract from any agent class. The small residual value reflects minor asymmetries: (1) Platform providers control whether to expose attribution mechanisms, creating information asymmetry between provider and user. (2) Sophisticated actors (institutions, researchers) can implement cryptographic signing more easily than individuals, creating a capability gap. (3) Actors who can afford session-preserving infrastructure (persistent accounts, logged interactions) have verification pathways unavailable to anonymous or ephemeral users. But these are second-order effects — the primary constraint (unsigned text lacks intrinsic provenance) affects all agents equally. Suppression (0.03): Near-zero. No agent is coerced into accepting the constraint, and alternatives exist (implement signing, preserve session context, use cryptographic commitments). The small residual reflects that some users lack technical literacy to implement alternatives, and some platforms deliberately obscure attribution mechanisms. But these are not inherent to the constraint — they are contingent institutional choices layered on top. Theater ratio (0.15): Very low. Minimal performative activity. The small residual reflects: (1) Stylometric analysis presented as 'verification' when it provides only probabilistic inference. (2) Platform 'AI detection' tools that flag probabilistic signals as definitive. (3) Disclosure requirements that cannot be enforced without verification mechanisms. But the constraint itself has no theatrical component — it is a straightforward information-theoretic limit. Accessibility collapse (0.92): Very high. No alternative pathway exists to verify authorship of unsigned text. Stylometry provides probabilistic signals, not verification. Watermarking requires prospective implementation. Session continuity requires preserved context. The constraint is not that verification is difficult — it is that the information does not exist in the artifact. Resistance (0.08): Very low. The constraint does not resist institutional change. Cryptographic signing, session tracking, and watermarking are all feasible. The constraint is in the unsigned artifact, not in the difficulty of implementing signing. The small residual reflects deployment friction (backward compatibility, user experience costs, infrastructure requirements) but these are not resistance to change — they are coordination costs of implementing alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits ZERO perspectival gap — all six perspectives classify as mountain. This is the diagnostic signature of a genuine natural law constraint: the classification is invariant across power levels, time horizons, exit options, and spatial scopes. The misattributed user with no technical capability sees the same structural limit as the forensic analyst with full resources. The immediate time horizon produces the same classification as the civilizational horizon. Local scope and universal scope agree. This invariance is not a failure of the indexical system — it is the system correctly identifying a constraint that emerges from information theory rather than from institutional arrangements. The constraint passes all three natural law gates: (1) Emerges naturally — no institution created the constraint; it follows from stateless text representation. (2) Accessibility collapse ≥ 0.85 — no alternative pathway exists to extract non-existent information. (3) Resistance ≤ 0.15 — the constraint does not resist change; signing mechanisms are feasible. The uniform mountain classification across all perspectives is the expected outcome for a constraint with these properties.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d values near 0.50 (symmetric) because the constraint does not systematically benefit or harm any agent class. The misattributed user (d ≈ 0.52) experiences slightly higher extraction than the provider (d ≈ 0.48) because users bear the epistemic cost of misattribution while providers bear only reputational risk. But both are close to symmetric — neither is a clear beneficiary or victim. The standards body (d ≈ 0.50) experiences the constraint as a coordination problem to solve, not as extraction. The forensic analyst (d ≈ 0.51) experiences slight extraction (cannot do their job without external context) but not severe. All d values cluster near 0.50, producing f(d) values near 0.65, which when multiplied by the low base extractiveness (0.08) yields χ values in the range 0.05-0.07 — well below the rope threshold (0.35). The constraint is not extractive because it does not systematically transfer value from one agent class to another — it is a shared epistemic limit.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates how the mandatrophy is resolved for genuine natural law constraints. The classification is mountain from all perspectives because the constraint emerges from information-theoretic necessity, not from institutional arrangements that could be classified differently by differently-positioned observers. The key distinction: (1) CONTINGENT institutional arrangements (verification bottleneck in quantum materials, peer review theater, regulatory capture) produce perspectival gaps — beneficiaries see rope, victims see snare, analysts see tangled rope. (2) NECESSARY information-theoretic limits (attribution opacity, Gödel incompleteness, halting problem) produce uniform mountain classification — all observers see the same structural limit because it is not observer-dependent. The mandatrophy asks: 'Is this a mountain of physics or a snare of policy?' For attribution opacity, the answer is unambiguous: mountain of information theory. The constraint could be BYPASSED by implementing signing mechanisms (making it a scaffold — temporary until signing is adopted), but the unsigned artifact itself remains opaque regardless of institutional arrangements. The perspectival invariance is not a bug — it is the diagnostic signature that distinguishes natural law from naturalized extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stylometric_threshold,
    'At what confidence threshold does stylometric analysis constitute ''verification'' rather than probabilistic inference?',
    'Philosophical analysis of verification standards; legal precedent for probabilistic evidence; community consensus on acceptable false-positive rates',
    'If threshold is low (e.g., 70% confidence): stylometry ''solves'' attribution, constraint becomes rope (coordination on standards). If threshold is high (e.g., 99.9%): stylometry remains probabilistic, constraint stays mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stylometric_threshold, conceptual, 'Confidence threshold for stylometric verification').

omega_variable(
    watermarking_robustness,
    'Do statistical watermarking techniques (token distribution biasing) survive adversarial paraphrasing and translation?',
    'Empirical testing of watermark persistence under transformation; adversarial robustness benchmarks; real-world deployment data',
    'If robust: watermarking provides verification pathway, constraint becomes scaffold (temporary until adoption). If fragile: watermarking fails, constraint remains mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(watermarking_robustness, empirical, 'Robustness of statistical watermarking under adversarial transformation').

omega_variable(
    session_continuity_requirement,
    'Does requiring session continuity for attribution verification constitute a fundamental architectural constraint or a design choice?',
    'Analysis of alternative architectures (blockchain provenance, distributed ledgers, cryptographic commitments); feasibility and cost-benefit analysis',
    'If architectural: constraint is mountain (session continuity is necessary). If design choice: constraint is rope (coordination on session-preserving interfaces).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(session_continuity_requirement, conceptual, 'Whether session continuity is architecturally necessary for attribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attribution_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attr_opac_tr_t0, attribution_opacity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(attr_opac_tr_t5, attribution_opacity, theater_ratio, 5, 0.15).
narrative_ontology:measurement(attr_opac_tr_t10, attribution_opacity, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(attr_opac_be_t0, attribution_opacity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(attr_opac_be_t5, attribution_opacity, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(attr_opac_be_t10, attribution_opacity, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attribution_opacity, information_standard).

% DUAL FORMULATION NOTE:
% Attribution opacity is a foundational constraint in the AI epistemics domain. Downstream constraints (content moderation, academic integrity, legal liability) all depend on whether attribution can be verified. If signing mechanisms become universal, those downstream constraints shift from mountains to ropes (coordination on standards). The network structure is: attribution_opacity (mountain) → [content_moderation_without_attribution, academic_integrity_verification, legal_liability_assignment] (currently mountains, become ropes if signing is adopted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
