% ============================================================================
% CONSTRAINT STORY: attention_as_bottleneck_resource
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_as_bottleneck_resource, []).

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
 *   constraint_id: attention_as_bottleneck_resource
 *   human_readable: The Finite Cognitive Aperture
 *   domain: social/technological/economic
 *
 * SUMMARY:
 *   This constraint models the socio-technical system built upon the
 *   biological fact of finite human attention. In an information-saturated
 *   world, attention becomes the primary scarce resource, leading to an
 *   'attention economy' where platforms, advertisers, and creators compete to
 *   capture and monetize it. The system is characterized by technologies
 *   (e.g., infinite scroll, notifications, algorithmic feeds) designed to
 *   exploit cognitive biases and maximize engagement, creating a structural
 *   conflict between the platforms' business models and the users' well-being
 *   and autonomy.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary target (powerless/trapped) — their attention is the resource being extracted.
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — design and control the mechanisms of extraction, which they frame as coordination.
 *   - Advertisers: Secondary beneficiary (organized/mobile) — purchase access to the extracted attention.
 *   - Digital Wellness Advocates: Organized opposition (organized/mobile) — seek to reform the system and provide users with tools for exit.
 *   - Legacy Media Institutions: Inertial actors (institutional/constrained) — their former attention-capturing models are now degraded.
 *   - Public Discourse Quality: Abstract victim (powerless/trapped) — suffers from the decline of shared context and the rise of outrage-driven engagement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_as_bottleneck_resource, 0.68).
domain_priors:suppression_score(attention_as_bottleneck_resource, 0.8).
domain_priors:theater_ratio(attention_as_bottleneck_resource, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, extractiveness, 0.68).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_as_bottleneck_resource, tangled_rope).
narrative_ontology:human_readable(attention_as_bottleneck_resource, "The Finite Cognitive Aperture").
narrative_ontology:topic_domain(attention_as_bottleneck_resource, "social/technological/economic").

domain_priors:requires_active_enforcement(attention_as_bottleneck_resource).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, platform_operators).
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, advertisers).
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, engagement_driven_content_creators).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, individual_users).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, public_discourse_quality).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, deep_work_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Trapped within a digital environment designed to maximize engagement. Experiences constant cognitive load, distraction, and manipulation of attention with high social and professional costs to exit. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.16. This extreme effective extraction firmly classifies the system as a Snare.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Experiences the system as a pure coordination mechanism: connecting users to content, advertisers to audiences. Benefits directly from the system's operation and can arbitrage different models of attention capture. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the dual nature of the system. It provides a genuine coordination function (information access, social connection) while simultaneously enabling massive, asymmetric extraction of attention as a resource. This is the canonical Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL WELLNESS ADVOCATE (SCAFFOLD) — Organized groups (e.g., Center for Humane Technology) see the current extractive model as a temporary phase. They are building alternatives (humane design principles, legislative proposals, educational campaigns) with an implicit sunset clause: to make the extractive attention economy obsolete. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.61. While χ is high, the sunset logic and coordination goal classify it as a Scaffold.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEDIA (PITON) — Old methods of capturing attention (e.g., scheduled broadcasts, print editions) are now largely inertial. The function has been usurped by digital platforms, but the rituals and institutions persist. The high theater_ratio (0.75) reflects that their activity is more performative than functional in the modern attention economy.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BIOLOGICAL REDUCTIONIST (MOUNTAIN) — This perspective incorrectly conflates the socio-technical system with the underlying biological limit. It frames the finite cognitive aperture as an immutable natural law, thus classifying the entire system as a Mountain. The engine will flag this as a 'false summit' because the system's high ε (0.68) and suppression (0.80) are properties of a contingent social arrangement, not a natural one.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_as_bottleneck_resource_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_as_bottleneck_resource, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_as_bottleneck_resource, TR),
    TR >= 0.70.

:- end_tests(attention_as_bottleneck_resource_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68): High. The value extracted from users (data, time, subscription fees driven by engagement) is significant and asymmetrically benefits platform operators. Suppression (0.80): Very High. Network effects, social pressure, and the integration of platforms into professional life create formidable barriers to exit. The environment is actively designed to prevent disengagement. Theater Ratio (0.75): High. While platforms provide real utility, a large portion of their activity (e.g., performative community standards enforcement, mission statements about 'connecting the world') serves to obscure the underlying extractive model. This high ratio enables the Piton perspective for legacy actors.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Platform operators experience a benign Rope, coordinating a complex market. Users experience a coercive Snare, their most valuable cognitive resource harvested against their long-term interests. Analysts see the full picture: a Tangled Rope that combines genuine coordination with severe extraction. Advocates see a temporary Scaffold to be dismantled, while legacy media see their own role as a Piton. The most critical gap is with the Biological Reductionist, who misidentifies the contingent, man-made Tangled Rope as an inevitable, natural Mountain, thereby justifying the status quo.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (platforms, advertisers) have arbitrage and mobile exit options, leading to low derived 'd' values and a perception of the system as coordination (Rope). Victims (users) are trapped, leading to a high 'd' value (≈0.95) and a perception of pure extraction (Snare). Organized advocates have some agency and mobility, placing them in the middle, consistent with a Scaffold. The analytical perspective's canonical 'd' value (≈0.72) is high enough to detect the severe extraction, leading to the correct Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary example of resolving the mandatrophy of naturalization. The system's defenders often frame the attention economy's problems as inevitable consequences of a natural law (the 'Mountain' of finite attention). Deferential Realism rejects this by assigning the high extractiveness and suppression values to the *socio-technical system*, not the biological limit. This correctly identifies the constraint as a contingent Tangled Rope—a set of design and policy choices that can be changed—rather than an immutable Mountain that must be accepted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_stability,
    'Is the current high-extraction attention economy a stable, long-term equilibrium or a transitional phase before regulatory or technological shifts?',
    'Longitudinal analysis of user engagement trends, regulatory adoption rates (e.g., GDPR-like laws), and the market share of non-extractive platforms.',
    'If stable, the Snare perspective is dominant. If transitional, the Scaffold perspective is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_stability, empirical, 'Stability of the attention economy equilibrium').

omega_variable(
    technological_mitigation,
    'Can AI-driven filters and personal assistants effectively mitigate cognitive overload, or do they simply create a new layer of extractive dependency?',
    'Comparative studies on the cognitive load and autonomy of users with and without advanced AI filtering tools.',
    'Effective mitigation would lower the system''s base extractiveness (ε). Creating new dependencies would maintain or increase it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_mitigation, empirical, 'Efficacy and side-effects of AI mitigation tools').

omega_variable(
    coercion_threshold,
    'What is the ethical and structural threshold where ''persuasive design'' becomes coercive manipulation?',
    'Establishing formal models of user autonomy and defining quantifiable metrics for coercive design patterns (e.g., dark patterns).',
    'A clear threshold would allow for the reclassification of some Tangled Rope dynamics as pure Snare, sharpening regulatory focus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_threshold, conceptual, 'Defining the threshold between persuasion and coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_as_bottleneck_resource, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atte_tr_t2004, attention_as_bottleneck_resource, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(atte_tr_t2014, attention_as_bottleneck_resource, theater_ratio, 2014, 0.6).
narrative_ontology:measurement(atte_tr_t2024, attention_as_bottleneck_resource, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(atte_be_t2004, attention_as_bottleneck_resource, base_extractiveness, 2004, 0.2).
narrative_ontology:measurement(atte_be_t2014, attention_as_bottleneck_resource, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(atte_be_t2024, attention_as_bottleneck_resource, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_as_bottleneck_resource, global_infrastructure).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, misinformation_dynamics).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, political_polarization).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, gig_economy_precarity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
