% ============================================================================
% CONSTRAINT STORY: ontological_friction_resolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ontological_friction_resolution, []).

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
 *   constraint_id: ontological_friction_resolution
 *   human_readable: The Chaste Fire of Truth — Ontological Friction Resolution
 *   domain: metaphysics/identity_resolution
 *
 * SUMMARY:
 *   Ontological friction arises when constructed identity (the 'vaporous veil
 *   of smiles' — the mask of social performance, inherited narratives, and
 *   defensive self-concept) encounters the pressure toward authenticity and
 *   truth-revealing. The 'chaste fire' is the pain of this friction: the
 *   burning sensation of pretense stripped away, of self-deception exposed,
 *   of the gap between performed self and lived self becoming undeniable.
 *   This constraint operates across all human consciousness-bearing agents
 *   and manifests across psychological, social, philosophical, and spiritual
 *   domains. The constraint exhibits the full range of DR types depending on
 *   structural position: those invested in maintaining comfort experience it
 *   as pure extraction (Snare); those navigating community identity
 *   maintenance experience mixed extraction and coordination (Tangled Rope);
 *   those seeking authenticity experience coordination benefits (Rope); those
 *   managing structured truth-facing experience temporal scaffolding with
 *   sunset (Scaffold); those in the commercialized authenticity industry
 *   perpetuate it as degraded ritual (Piton); logical analysis risks
 *   naturalizing it as inevitable (Mountain, but likely a false summit).
 *
 * KEY AGENTS:
 *   - Comfort Preservers: Primary victims (powerless/trapped) — those whose identity and social position depend on maintaining masks and self-deception; bear full cost of ontological exposure
 *   - Identity Defenders: Secondary targets (moderate/constrained) — communities and institutions defending particular identity frameworks; experience extraction but retain some collective agency
 *   - Truth Seekers: Primary beneficiaries (institutional/arbitrage) — philosophical movements, authenticity frameworks, epistemological schools that benefit from friction and deconstruction
 *   - Transition Architects: Organized agents (organized/mobile) — therapeutic practices, deconstructionist methods, contemplative disciplines providing structured pathways through friction with planned sunset
 *   - Ritualized Authenticity Industry: Institutional degradation (institutional/constrained) — wellness industries, self-help movements, 'authenticity' marketing perpetuating the friction as performative practice
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — philosophical analysis risk naturalizing the constraint as logically inevitable rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ontological_friction_resolution, 0.58).
domain_priors:suppression_score(ontological_friction_resolution, 0.68).
domain_priors:theater_ratio(ontological_friction_resolution, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, 0.58).
narrative_ontology:constraint_metric(ontological_friction_resolution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ontological_friction_resolution, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ontological_friction_resolution, tangled_rope).
narrative_ontology:human_readable(ontological_friction_resolution, "The Chaste Fire of Truth — Ontological Friction Resolution").
narrative_ontology:topic_domain(ontological_friction_resolution, "metaphysics/identity_resolution").

domain_priors:requires_active_enforcement(ontological_friction_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, truth_seekers).
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, authenticity_advocates).
narrative_ontology:constraint_victim(ontological_friction_resolution, identity_defenders).
narrative_ontology:constraint_victim(ontological_friction_resolution, comfort_preservers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMFORT PRESERVERS (SNARE) — Those whose existing identity framework depends on the 'vaporous veil of smiles' experience the friction of truth as pure extraction. Trapped within inherited narratives and social constructs, they bear the full cost of ontological exposure with minimal means to escape or negotiate. The constraint extracts by forcing acknowledgment of self-deception.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: IDENTITY DEFENDERS (TANGLED ROPE) — Communities invested in maintaining particular identity constructs experience both extraction (forced reckoning with contradictions) and coordination benefits (shared frameworks for negotiating change). Constrained by social bonds and institutional dependencies, yet capable of collective resistance or adaptation. The constraint enforces active maintenance of the tension between mask and truth.
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRUTH SEEKERS (ROPE) — Philosophical movements, epistemological frameworks, and authenticity-centered institutions benefit from the friction itself as a coordination mechanism. The resolution of ontological friction enables new forms of shared understanding and collective inquiry. Arbitrage exit: they can shift between different truth frameworks and benefit from exposure.
constraint_indexing:constraint_classification(ontological_friction_resolution, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSITION ARCHITECTS (SCAFFOLD) — Therapeutic practices, deconstructionist methodologies, and contemplative disciplines function as structured pathways for managing ontological friction with planned sunsetting of performative masks. Theater is explicitly part of the protocol (the 'sacred space' for facing truth) with declining necessity as authentic frameworks stabilize. Has sunset clause: the scaffolding dissolves when genuine identity integration occurs.
constraint_indexing:constraint_classification(ontological_friction_resolution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RITUALIZED AUTHENTICITY INDUSTRY (PITON) — Self-help movements, wellness industries, and 'authenticity' marketing perpetuate the performative stripping of masks as itself a theatrical practice. The process degrades from genuine ontological reckoning into a commodified ritual where 'facing truth' becomes another persona to adopt. Theater ratio is high (0.65+): the mechanism persists through institutional inertia despite low functional authenticity.
constraint_indexing:constraint_classification(ontological_friction_resolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LOGICAL STRUCTURE VIEW (MOUNTAIN) — From pure logical analysis, the friction between any constructed identity (mask) and its deconstruction is a structural necessity, not a contingent institutional arrangement. The gap between representation and referent, between self-image and self-awareness, is an irreducible feature of reflexive consciousness. However, the empirical data contradicts this: the constraint has variable suppression (0.68) and measurable theater (0.55), indicating institutional and social contingency rather than logical necessity.
constraint_indexing:constraint_classification(ontological_friction_resolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ontological_friction_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ontological_friction_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ontological_friction_resolution, TR),
    TR >= 0.70.

:- end_tests(ontological_friction_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts benefit primarily toward those who control frameworks of authenticity (Truth Seekers, therapists, deconstructionist academics) while imposing costs on those defending existing identity structures. The extraction is not total because the process has genuine coordination value — the friction itself enables growth, integration, and authentic connection. The measurement trajectory (0.42 → 0.58) reflects increasing institutionalization of 'authenticity' as a marketable commodity. Suppression (0.68): High. Powerful incentives suppress honest confrontation with identity friction: social taboo against vulnerability, institutional penalties for questioning shared narratives, neurobiological avoidance of cognitive dissonance, economic interests in maintaining performative identities. Yet suppression is not absolute — some communities and individuals actively practice truth-facing. Theater ratio (0.55): Moderate-high. The performance has risen over the interval as 'authenticity work' itself becomes a performed role — the very tools designed to strip masks have become new masks in the hands of the commercialized wellness industry.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between victims and beneficiaries is substantial. Comfort Preservers experience the constraint as pure coercion — the chaste fire burns only them. Truth Seekers experience it as enabling coordination — the friction itself is the tool for building authentic frameworks. Identity Defenders see mixed experience: the friction threatens community cohesion but also offers pathways to deeper integration. The Transition Architects deliberately sit in the middle, managing the friction as a structured process. The Ritualized Authenticity Industry experiences collapse of the gap: what began as genuine friction-work has degraded into another persona. The analytical observer risks seeing all of this as inevitable — the Mountain perspective — but the empirical variation in suppression and theater across communities reveals institutional contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the friction-resolution process. Comfort Preservers derive d ≈ 0.95 (nearly full victims) — they bear costs without exit. Identity Defenders derive d ≈ 0.65 (constrained victims with some collective agency). Truth Seekers derive d ≈ 0.15 (beneficiaries with mobile exit) — they can arbitrage between frameworks and benefit from friction. Transition Architects derive d ≈ 0.50 (symmetric experience) — they are both targets of the friction and designers of managed pathways through it. The Ritualized Authenticity Industry derives d ≈ 0.35 (paradoxical: benefits from the friction but trapped within degraded ritual) — institutional leverage constrains them despite their nominal beneficiary status. The analytical observer's d ≈ 0.72 reflects the risk of naturalization: seeing inevitable what is contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the friction is BOTH natural (an inevitable feature of reflexive consciousness encountering its own constructs) AND contingent (the degree of suppression, theater, and extraction is highly variable across cultures, communities, and historical periods). The constraint is NOT a Mountain — the base extraction (0.58) and measurable theater (0.55) exceed the Mountain thresholds. The constraint IS a Tangled Rope from the analytical perspective: it combines genuine coordination (the friction enables authentic connection and growth) with asymmetric extraction (Truth Seekers and authenticity industries benefit disproportionately). The Scaffold perspective is empirically justified IF integration genuinely reduces friction over time; it remains to be tested whether the sunset clause is real or aspirational. The Piton perspective captures the real degradation of authenticity work into commodified ritual. The mandatrophy is resolved by accepting that the constraint operates across multiple simultaneous modalities — it is not reducible to a single type, and the perspectival plurality itself is the analytical result.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_coherence_paradox,
    'Can there be a stable authentic identity, or does the act of identifying authenticity necessarily create a new mask?',
    'Longitudinal studies of identity integration; analysis of whether ''authentic'' frameworks exhibit the same mask/truth friction as the identities they replaced',
    'If all identity states generate friction: the constraint is Mountain (inevitable). If friction diminishes with genuine integration: the constraint is Tangled Rope (manageable extraction embedded in coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_coherence_paradox, conceptual, 'The reflexive paradox of authenticity itself generating new masks').

omega_variable(
    suppression_mechanism_source,
    'What drives suppression of ontological friction — social taboo, cognitive dissonance avoidance, or institutional incentive structures?',
    'Cross-cultural comparison of truth-facing practices; analysis of communities with explicit truth-telling protocols vs suppression-dominant cultures; neurocognitive studies of identity threat response',
    'If primarily cognitive: friction is universal but management techniques reduce extraction. If primarily institutional: suppression is contingent and can be dismantled through structural reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'The source of suppression mechanisms in ontological friction').

omega_variable(
    integration_timeline_existence,
    'Is there a realistic timeline for integrating authentic identity such that the friction resolves and scaffold sunsetting occurs, or is the constraint perpetually renewable?',
    'Case studies of individuals/communities claiming full integration; analysis of whether friction returns in new forms; longitudinal tracking of authenticity practices',
    'If integration is possible: Scaffold perspective is empirically grounded with real sunset. If friction perpetually renews: Scaffold is aspirational; constraint is actually Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_timeline_existence, empirical, 'Whether authentic integration can be achieved with declining friction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ontological_friction_resolution, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ontofric_tr_t0, ontological_friction_resolution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ontofric_tr_t3, ontological_friction_resolution, theater_ratio, 3, 0.45).
narrative_ontology:measurement(ontofric_tr_t6, ontological_friction_resolution, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(ontofric_be_t0, ontological_friction_resolution, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ontofric_be_t3, ontological_friction_resolution, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(ontofric_be_t6, ontological_friction_resolution, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ontological_friction_resolution, information_standard).
narrative_ontology:affects_constraint(ontological_friction_resolution, self_deception_maintenance).
narrative_ontology:affects_constraint(ontological_friction_resolution, authentic_connection_barriers).
narrative_ontology:affects_constraint(ontological_friction_resolution, institutional_authenticity_commodification).

% DUAL FORMULATION NOTE:
% Ontological friction resolution decomposes into three structurally distinct constraints: (1) the maintenance of self-deceptive identity structures (Mountain from individual cognitive perspective, Piton from institutional perspective); (2) barriers to authentic interpersonal connection arising from masks (Tangled Rope); (3) commodification of authenticity work itself (Piton). This story addresses the meta-constraint of how these three interact. The upstream constraint is self-deception maintenance (lower ε); the downstream constraint is authenticity commodification (higher theater).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ontological_friction_resolution, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
