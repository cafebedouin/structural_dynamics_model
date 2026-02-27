% ============================================================================
% CONSTRAINT STORY: narcissistic_ego_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narcissistic_ego_maintenance, []).

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
 *   constraint_id: narcissistic_ego_maintenance
 *   human_readable: The Mirror Trap: Narcissistic Ego Maintenance
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The narcissistic relationship structure creates a relational trap where
 *   one party (The Echo) is compelled to provide constant affirmation and
 *   mirroring to the other party (The Image). This constraint exhibits high
 *   extractiveness (0.68) because the extraction mechanism operates through
 *   psychological dependence, identity subsumption, and reality distortion
 *   rather than explicit force. The Echo loses access to independent
 *   reality-testing, emotional authenticity, and self-directed agency. The
 *   constraint appears differently from each perspective: the Image
 *   experiences it as coordination (necessary mutual arrangement for
 *   psychological equilibrium), the Echo experiences pure extraction (trapped
 *   with no visible alternatives), the Partial Resister experiences mixed
 *   coordination-extraction (some genuine connection mixed with clear
 *   asymmetry), the Intergenerational Pattern perspective sees a degraded
 *   family mechanism maintained by inertia, the Therapeutic view sees a
 *   temporary problem being solved by external intervention, and the
 *   Civilizational Analyst risks naturalizing the extraction as inherent to
 *   human attachment. The theater_ratio (0.65) reflects that much of the
 *   relationship's maintenance work is performative: both parties rehearse
 *   roles ('caretaker' / 'dependent', 'strong' / 'admiring') rather than
 *   engaging in authentic exchange. The constraint's suppression (0.72)
 *   operates through multiple mechanisms: psychological (identity loss fears,
 *   abandonment terror, trauma bonding), social (isolation from outside
 *   validation, narrative reframing of the Echo's identity), and relational
 *   (devaluation cycles, intermittent reinforcement that resembles addiction
 *   patterns).
 *
 * KEY AGENTS:
 *   - The Echo: Primary victim (powerless/trapped) — bears extraction through emotional labor, reality distortion, agency suppression, identity submersion
 *   - The Image: Primary beneficiary (institutional/arbitrage) — captures psychological equilibrium benefit; has escape options (devaluation, supply sourcing, reframing)
 *   - The Partial Resister: Secondary actor (moderate/constrained) — beginning to recognize asymmetry; some agency but high relational cost for resistance
 *   - The Intergenerational Pattern: Structural inheritance (moderate/constrained) — vestigial family mechanism or culturally embedded expectation maintaining the trap
 *   - The Therapeutic Intervention: Organized intervention (organized/mobile) — alternative pathway with sunset logic; creates echo agency and exit capacity
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent relational extraction as inherent to psychology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narcissistic_ego_maintenance, 0.68).
domain_priors:suppression_score(narcissistic_ego_maintenance, 0.72).
domain_priors:theater_ratio(narcissistic_ego_maintenance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, extractiveness, 0.68).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narcissistic_ego_maintenance, snare).
narrative_ontology:human_readable(narcissistic_ego_maintenance, "The Mirror Trap: Narcissistic Ego Maintenance").
narrative_ontology:topic_domain(narcissistic_ego_maintenance, "psychological/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, the_echo).
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, emotional_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ECHO (SNARE) — The subject trapped in narcissistic supply provision. Cannot exit without severe psychological and social cost (identity dissolution, abandonment, retaliation narrative). Extraction is total: emotional labor, reality distortion, agency suppression, identity submersion. No alternatives visible from inside the constraint. Maximum experienced extraction.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE IMAGE (ROPE) — The narcissist experiences the constraint as pure coordination: mirroring is the mechanism that sustains their psychological equilibrium. The Image perceives this as necessary mutual arrangement (though the mutuality is illusory). Net beneficiary with escape options — can devalue and discard the Echo, activate backup supply sources, or reframe the relationship. Effective extraction runs toward this agent; suppression is structural but not phenomenologically experienced as coercive by the beneficiary.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PARTIAL RESISTER (TANGLED ROPE) — Moderately empowered Echo beginning to recognize the extraction (therapy, intervention, peer support). Experiences both genuine coordination (genuine affection mixed with narcissistic supply provision) and clear asymmetric extraction. Constrained exit: can partially resist, establish boundaries, seek outside validation, but at high relational cost. Some agency, some extraction, some benefit-to-beneficiary.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE INTERGENERATIONAL PATTERN (PITON) — From the generational view, narcissistic supply provision appears as a vestigial family pattern or culturally embedded expectation (parental narcissism modeling, gender-role enforcement of emotional caretaking, attachment trauma responses). The original coordination function (parent-child bonding, reciprocal care) has atrophied; the extraction mechanism persists through narrative inertia and institutional invisibility. Theater_ratio high (80%+) because the relationship is maintained through performative roles — 'I'm the caretaker,' 'You're the dependent' — rather than genuine function.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE THERAPEUTIC EXIT (SCAFFOLD) — Organized interventions (trauma-informed therapy, support groups, psychoeducation on narcissistic patterns) create a sunset for the trap. The Echo gains agency through external validation and skill-building, exits the relationship, or restructures it with enforced boundaries. This is genuinely temporary coordination (therapy working) with declining suppression over time. Theater involves performative self-awareness work and role rehearsal, but real functional gains are present. High suppression initially, declining as new patterns solidify.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / PSYCHOLOGICAL NATURALISM (MOUNTAIN) — From a civilizational perspective, narcissistic mirroring appears as an irreducible feature of human attachment and identity formation. All humans to some degree project and seek affirmation; narcissistic structures represent an extreme point on a normal-distribution continuum. The constraint naturalizes the extraction as inherent to how narcissism 'must' function. However, structural data contradicts this: narcissistic supply provision is maintained by specific behavioral patterns, belief systems, and suppression mechanisms — all contingent, not laws of psychology. This is a false summit revealing naturalization of socially constructed extraction.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narcissistic_ego_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narcissistic_ego_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narcissistic_ego_maintenance, TR),
    TR >= 0.70.

:- end_tests(narcissistic_ego_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from the Echo through mandatory emotional labor, reality distortion, and identity submersion. The extraction is not purely economic but psychological — the Echo's authentic self is subordinated to the Image's ego maintenance requirements. The value reflects that psychological extraction is as real as material extraction, operating through dependence mechanisms rather than force. The trajectory (0.45 → 0.68) shows escalation over the biographical interval: initial phase of idealization involves genuine emotional reciprocity (lower extraction), but as the relationship stabilizes, the Image demands increase and the Echo's reality-testing capacity declines (higher extraction). Suppression (0.72): High. Multiple mechanisms prevent exit: psychological (identity loss, abandonment terror, trauma bonding through intermittent reinforcement), social (isolation, narrative reframing, invalidation of outside perspectives), and relational (devaluation cycles that punish boundary-setting). The suppression is not total — some Echoes do escape — but it is severe enough to create a high-friction exit. Theater ratio (0.65): Moderate-high. The relationship is substantially performative: both parties maintain roles ('caretaker'/'dependent', 'strong'/'admiring') through repetition. However, genuine affect and connection are also present (distinguishing this from pure theater), making it tangled rather than pure piton. The trajectory (0.50 → 0.65) reflects increasing performative maintenance: as the relationship matures, authenticity declines and role-playing intensifies to maintain the extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap between the Image and the Echo. The Image (institutional/arbitrage) experiences rope: their narrative frames the relationship as mutually necessary coordination for emotional stability. The Echo (powerless/trapped) experiences snare: they perceive mandatory supply provision with no visible alternatives and severe costs for exit. This gap is not observational but structural: the Image has genuine exit options (devaluation, supply-sourcing from backup sources, narrative reframing), while the Echo's exit options are suppressed through psychological mechanisms (identity loss fears) and social mechanisms (isolation from validating perspectives). The Partial Resister occupies a transition point: as they gain outside support, they begin to perceive the extraction, moving from Snare toward Tangled Rope. The Intergenerational Pattern perspective reveals that the extraction is not a stable equilibrium but a degraded mechanism — the original coordination function (parent-child bonding, reciprocal emotional attunement) has atrophied, leaving only the extraction framework. The Therapeutic perspective identifies a sunset pathway: organized intervention (trauma-informed therapy, support groups, psychoeducation) can create Echo agency and eventually exit or boundary-setting. The Civilizational Analyst perspective risks naturalizing the constraint as inherent to how narcissism 'must' function, but the structural data reveals this as a false summit: narcissistic supply provision is maintained by specific, contingent behavioral patterns and suppression mechanisms, not laws of psychology.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is determined by their power level, exit options, and structural position relative to the extraction flow. The Image (institutional/arbitrage) has low directionality (d ≈ 0.05) because they are a full beneficiary with multiple exit options — the constraint subsidizes their psychological equilibrium. Their derived f(d) produces negative or near-zero effective extraction: they experience the relationship as beneficial coordination, not as extraction. The Echo (powerless/trapped) has maximum directionality (d ≈ 0.95) because they are a full target with no visible exit options. Their derived f(d) produces maximum effective extraction (chi ≈ 0.97 at global scope), reflecting that psychological extraction operates with full force against a powerless agent. The Partial Resister (moderate/constrained) has moderate-high directionality (d ≈ 0.70) because they are partly victimized but have begun to access external resources (therapy, support groups) that provide partial exit options. Their chi value is moderate (perhaps 0.55) reflecting mixed extraction and emerging agency. The intergenerational pattern perspective (moderate/constrained) also derives d ≈ 0.65 because the pattern is a structural inheritance with some agency to interrupt (choosing different parenting models, therapy, conscious modification) but high cultural inertia. The therapeutic perspective (organized/mobile) derives d ≈ 0.35 because organized intervention creates mobile exit options, reducing the constraint's experienced extraction force.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids false classification as pure coordination (Rope) by including explicit victim declarations (the_echo, emotional_authenticity) and high suppression (0.72). The Image's rope experience is their subjective reality, but the structural data reveals it as a rationalization of extraction. The Echo's snare classification is accurate from the primary perspective (powerless/trapped) and represents the constraint's net effect on human flourishing. The Partial Resister and therapeutic perspectives confirm that the extraction is real and remediable — it is not an immutable feature of narcissism but a contingent relational pattern maintained by specific suppression mechanisms. The mandatrophy is resolved by showing that all six perspective types are legitimate readings of a single base structure, but the analytical observer's mountain classification is a false summit: it naturalizes what is contingent. The therapeutic exit (scaffold) confirms that the constraint is remediable, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narcissism_continuum_threshold,
    'Where does normal projection and identity-seeking end and pathological narcissistic extraction begin?',
    'Longitudinal study of projection patterns across clinical and non-clinical populations; measurement of asymmetry in emotional labor and reality negotiation; comparison of relationship satisfaction trajectories',
    'If threshold is continuous and gradual: many relationships misclassified as purely exploitative when they involve mutual but asymmetric projection. If threshold is discrete: extraction classification is clearer but may pathologize relationship styles that are merely frustrating rather than trapping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narcissism_continuum_threshold, empirical, 'Threshold distinguishing normal projection from pathological narcissistic extraction').

omega_variable(
    supply_necessity_for_stability,
    'Is narcissistic supply functionally necessary for the narcissist''s psychological stability, or is it a reinforced preference?',
    'Neurological studies of narcissistic response to supply withdrawal; longitudinal analysis of narcissist stability under chronic supply deprivation; comparison with other addiction or reinforcement patterns',
    'If necessary: narcissist''s experience of the constraint as coordination is structurally accurate — withdrawal is genuinely destabilizing. If reinforced preference: the ''coordination'' is illusory, and the constraint is pure snare from all perspectives except the Image''s own rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_necessity_for_stability, empirical, 'Whether narcissistic supply is functionally necessary or reinforced preference').

omega_variable(
    exit_cost_quantification,
    'What are the actual psychological and social costs of exiting narcissistic relationships versus the perceived costs that create trapping?',
    'Post-exit longitudinal studies of Echo wellbeing, social reintegration, and identity recovery; comparison of predicted versus actual costs; analysis of cost-minimization strategies that enable exits',
    'If actual costs << perceived costs: exit suppression is primarily psychological (trauma bonding, identity loss fears, abandonment terror) rather than material, changing intervention design. If actual costs == perceived costs: the trapping is real and requires structural intervention, not just psychological reframing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_quantification, empirical, 'Gap between perceived exit costs and actual post-exit outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narcissistic_ego_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narc_tr_t0, narcissistic_ego_maintenance, theater_ratio, 0, 0.5).
narrative_ontology:measurement(narc_tr_t5, narcissistic_ego_maintenance, theater_ratio, 5, 0.62).
narrative_ontology:measurement(narc_tr_t10, narcissistic_ego_maintenance, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(narc_be_t0, narcissistic_ego_maintenance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(narc_be_t5, narcissistic_ego_maintenance, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(narc_be_t10, narcissistic_ego_maintenance, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narcissistic_ego_maintenance, enforcement_mechanism).
narrative_ontology:affects_constraint(narcissistic_ego_maintenance, coercive_control_in_intimate_relationships).
narrative_ontology:affects_constraint(narcissistic_ego_maintenance, trauma_bonding_attachment).
narrative_ontology:affects_constraint(narcissistic_ego_maintenance, identity_foreclosure_in_dependent_roles).

% DUAL FORMULATION NOTE:
% Narcissistic ego maintenance decomposition: This story focuses on the relational dyadic structure (narcissist-supply provider). Upstream constraints include attachment trauma that predisposes to narcissistic vulnerability; downstream constraints include broader institutional structures (family narratives, gender role enforcement, therapeutic system responses) that either maintain or interrupt the dyadic trap. Each decomposed story has distinct extractiveness and suppression values reflecting the specific mechanism level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
