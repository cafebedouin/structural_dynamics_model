% ============================================================================
% CONSTRAINT STORY: cinderella_midnight_deadline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cinderella_midnight_deadline, []).

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
 *   constraint_id: cinderella_midnight_deadline
 *   human_readable: The Fairy Godmother's Midnight Deadline
 *   domain: magical/social
 *
 * SUMMARY:
 *   The Fairy Godmother's midnight deadline creates a structural tension
 *   between gift and coercion. On its surface, the constraint grants
 *   Cinderella temporary social elevation and access to the royal court — a
 *   genuine coordination benefit. But the mechanism is enforced by magical
 *   termination: at midnight, the transformation dissolves, forcing
 *   Cinderella to flee and reverting her to servant status. This constraint
 *   exhibits five distinct classification types from different structural
 *   positions, revealing how the same gift can be experienced as coordinate
 *   mechanism (rope), temporary mobility platform (scaffold), pure extraction
 *   (snare), mixed gift-coercion (tangled rope), or degraded social authority
 *   (piton), depending on the observer's relationship to the deadline. The
 *   theater_ratio (0.65) reflects that the ball itself is performative —
 *   Cinderella plays a role, the court performs courtship rituals, the Fairy
 *   Godmother's magic is theatrical spectacle. What appears to be social
 *   transformation is largely role-play with a hard termination rule. The
 *   extractiveness (0.52) is moderate-high: the deadline extracts
 *   Cinderella's agency to negotiate or remain, while the gift coordination
 *   function provides genuine (temporary) benefit.
 *
 * KEY AGENTS:
 *   - Cinderella: Primary victim/beneficiary (powerless/trapped-to-constrained) — receives temporary transformation but is trapped by the midnight deadline; cannot negotiate, cannot stay, cannot claim the elevated identity as her own
 *   - Fairy Godmother: Primary benefactor (institutional/arbitrage) — grants the transformation as a gift; has full exit and sees the interaction as pure coordination without extraction flowing toward her
 *   - Stepfamily: Institutional authority holder (institutional/arbitrage) — their household power is degraded and theatrical; the deadline paradoxically ensures Cinderella's re-subordination by forcing her return
 *   - Royal Court/Prince: Secondary beneficiary (powerful/mobile) — enables Cinderella's participation in court society; benefits from the narrative outcome (marriage) which validates the temporary mobility system
 *   - Cinderella's Authentic Identity: Abstract victim (powerless/trapped) — the transformation extracts and suppresses her true identity, replacing it with a borrowed persona that must dissolve at midnight
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the deadline as a law of magic when it is actually a contractual term set by the Fairy Godmother
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cinderella_midnight_deadline, 0.52).
domain_priors:suppression_score(cinderella_midnight_deadline, 0.68).
domain_priors:theater_ratio(cinderella_midnight_deadline, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cinderella_midnight_deadline, extractiveness, 0.52).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cinderella_midnight_deadline, tangled_rope).
narrative_ontology:human_readable(cinderella_midnight_deadline, "The Fairy Godmother's Midnight Deadline").
narrative_ontology:topic_domain(cinderella_midnight_deadline, "magical/social").

domain_priors:requires_active_enforcement(cinderella_midnight_deadline).
narrative_ontology:has_sunset_clause(cinderella_midnight_deadline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, cinderella).
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, royal_court).
narrative_ontology:constraint_victim(cinderella_midnight_deadline, cinderella_authentic_identity).
narrative_ontology:constraint_victim(cinderella_midnight_deadline, stepfamily_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CINDERELLA AT MIDNIGHT (SNARE) — The transformation is temporary and enforced by external magic. Cinderella cannot negotiate, extend, or escape the deadline. The constraint extracts her authentic identity and replaces it with a borrowed persona that dissolves at midnight. She is trapped by the magic's termination rule — no alternatives, no escape options, maximum suppression of her agency.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CINDERELLA NEGOTIATED (TANGLED ROPE) — The Fairy Godmother grants the transformation voluntarily as a gift. Cinderella benefits from access to the ball, social elevation, and romantic possibility. However, the benefit is conditional on the deadline — she cannot stay, cannot claim the transformation as her own, cannot make it permanent. She is both coordinated (given agency to attend) and extracted (stripped of the ability to sustain that agency beyond midnight). Active enforcement via magical termination.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FAIRY GODMOTHER (ROPE) — Grants a temporary coordination gift that enables Cinderella's participation in court society. The Fairy Godmother is a pure benefactor experiencing this as a coordination mechanism: she solves the access problem (how Cinderella can attend) without extraction. She has full exit (can grant or refuse the magic) and sees no extraction flowing toward her. This is genuine gift coordination.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ROYAL COURT SOCIAL MOBILITY (SCAFFOLD) — The magical intervention temporarily suspends the status system that normally excludes servant-class individuals from court. The deadline is explicitly a sunset clause — midnight reverts the social structure to its original form. The constraint enables temporary upward mobility with a built-in sunset, functioning as a transitional coordination mechanism. Court norms are mobile enough to accommodate temporary exceptions; the real marriage at the end would represent permanent renegotiation rather than scaffold collapse.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: STEPFAMILY AUTHORITY (PITON) — The household authority structure that makes Cinderella a servant persists through theatrical performance of household order and tradition. The Fairy Godmother's intervention bypasses this system entirely by taking Cinderella outside the house. The stepfamily's authority is degraded/inertial — it has no enforcement mechanism beyond physical proximity and social convention. When Cinderella leaves, the authority dissolves. It is maintained through habit and theater, not genuine power.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational view, the magical transformation embodies an immutable constraint: all borrowed power has an expiration date. The deadline is not a convention but a law of magic itself — transformation cannot be sustained indefinitely without deeper change. The stroke of midnight represents the fundamental impossibility of maintaining a false identity. However, the engine will identify this as a false summit: the 'law' is actually a contract term set by the Fairy Godmother, not a natural limit.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cinderella_midnight_deadline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cinderella_midnight_deadline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cinderella_midnight_deadline, TR),
    TR >= 0.70.

:- end_tests(cinderella_midnight_deadline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The deadline mechanism extracts Cinderella's ability to sustain the transformation or negotiate its terms. She receives a gift (coordination benefit) but at the cost of temporal termination — the magical enforcement ensures she cannot keep the elevation. The extractiveness increased from 0.25 (initial grant) to 0.52 (sustained until midnight) because the forced return compounds the extraction. Suppression (0.68): High. Cinderella has no alternatives to midnight termination. She cannot negotiate the deadline, extend it, or refuse the magic's return. The stepfamily holds her under physical and social control; the Fairy Godmother's magic is absolute. The suppress value reflects both the lack of exit options and the external magical enforcement. Theater ratio (0.65): Moderate-high. The ball is performative spectacle. Cinderella performs a role, the court performs courtship, the Fairy Godmother's magic is theatrical transformation. But the theater is functional — it enables social participation that would otherwise be impossible. The high theater reflects that the 'real' work (romance, social recognition) happens within the performed interaction. Theater increased from 0.40 (before the ball) to 0.65 (during and after, as the magic becomes central to the narrative).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival disagreement across the six types. Cinderella experiences it as a snare (pure extraction via forced termination). The Fairy Godmother experiences it as rope (pure coordination gift). The royal court experiences it as scaffold (temporary mobility with an expected sunset). The stepfamily experiences it as piton (their degraded authority is maintained only by Cinderella's forced return). The analytical observer risks seeing mountain (the deadline as immutable magic law) but the structural data reveals this as a false summit. The perspectival gap is not due to measurement ambiguity but to genuine structural differences in exit options and relationship to the deadline. Each observer's classification is locally correct for their position — the global picture requires all six types simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Cinderella's directionality (d) is high: she is the primary victim of the deadline enforcement, has trapped exit options, and bears the cost of the termination rule. The Fairy Godmother's d is low: she is a benefactor with arbitrage exit, sees no extraction flowing toward herself, and experiences the constraint as pure coordination. The stepfamily's d is paradoxical: they appear to be targets of disruption (Cinderella leaves their authority), but the deadline ensures her return, so they benefit from the constraint's termination rule enforcing re-subordination. The royal court's d is moderate: they benefit from the social opening (temporary mobility) but are not directly coerced by the deadline. The engine derives Cinderella's high d from victim status + trapped exit options, producing high f(d) and high effective extraction. The Fairy Godmother's low d (beneficiary + arbitrage) produces low or negative f(d), appearing as no extraction to her. The stepfamily's paradoxical d requires an override if the structural data is to capture their benefit from the deadline's return mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between the gift's coordination function and the deadline's extraction mechanism. The Fairy Godmother's grant is genuine coordination (Rope from her perspective) — it solves the access problem without extraction. But the deadline is genuine extraction (Snare from Cinderella's perspective) — it forces termination without negotiation. The Tangled Rope classification at moderate power and constrained exit captures the hybrid: Cinderella benefits from access and elevation (coordination) but is extracted from via forced return (coercion). The scaffold perspective shows how the deadline could be beneficial if it were genuinely temporary (e.g., a test or trial period before permanent elevation) — but without commitment to permanent integration, it remains extraction of her agency. The false summit (mountain view) naturalizes the deadline as magic law rather than the Fairy Godmother's contractual choice. Mandatrophy is resolved by showing that the gift and deadline are separable: the first is coordination, the second is extraction. Their combination is neither pure rope nor pure snare, but the mixed mechanism that Tangled Rope captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magic_sustainability_threshold,
    'Is the midnight deadline a hard magical law or a negotiable contract term set by the Fairy Godmother?',
    'Counterfactual analysis: Can the deadline be extended if Cinderella and the Fairy Godmother agree? Does the magic enforce the deadline or does the Godmother?',
    'If hard law: constraint is Mountain (transformation decay is inherent to magic). If negotiable: constraint is Snare (the deadline is a form of coercion enforced by the Godmother''s authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magic_sustainability_threshold, conceptual, 'Whether midnight deadline is immutable magic law or negotiable contract').

omega_variable(
    royal_recognition_binding_power,
    'Does the Prince''s recognition of Cinderella at the ball constitute a permanent social bond that could override the magical deadline?',
    'Narrative analysis: Can the Prince''s subsequent search for the slipper owner override the Fairy Godmother''s termination? Is royal recognition sufficient to make the transformation permanent?',
    'If recognition is binding: the midnight deadline is merely a test (Scaffold). If recognition has no power: the deadline is truly separating (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_recognition_binding_power, conceptual, 'Whether royal recognition can override the magical deadline').

omega_variable(
    stepfamily_authority_reinforcement,
    'Does the magical intervention reveal the stepfamily''s authority as fundamentally weak, or does the Fairy Godmother''s deadline itself reinforce their control by ensuring Cinderella must return?',
    'Structural analysis: Is the deadline a tool of liberation or a mechanism that forces re-subordination? Does it strengthen the stepfamily''s hold by making escape temporary?',
    'If liberation: Fairy Godmother opposes the stepfamily. If reinforcement: the Fairy Godmother''s deadline inadvertently serves the stepfamily''s interest in keeping Cinderella subordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stepfamily_authority_reinforcement, conceptual, 'Whether the deadline reinforces or undermines stepfamily authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cinderella_midnight_deadline, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cind_theater_0, cinderella_midnight_deadline, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cind_theater_4, cinderella_midnight_deadline, theater_ratio, 4, 0.65).
narrative_ontology:measurement(cind_theater_8, cinderella_midnight_deadline, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(cind_extract_0, cinderella_midnight_deadline, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cind_extract_4, cinderella_midnight_deadline, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(cind_extract_8, cinderella_midnight_deadline, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cinderella_midnight_deadline, resource_allocation).
narrative_ontology:affects_constraint(cinderella_midnight_deadline, stepfamily_servitude_authority).
narrative_ontology:affects_constraint(cinderella_midnight_deadline, royal_status_hierarchy).

% DUAL FORMULATION NOTE:
% The Fairy Godmother's midnight deadline decomposes into two structurally distinct constraints: (1) the gift coordination mechanism (Rope, ε≈0.08) which enables Cinderella's temporary access to the court, and (2) the deadline enforcement mechanism (Tangled Rope/Snare, ε≈0.52) which forces her return and maintains her subordination. The gift alone would be pure coordination; the deadline mechanism converts it into extraction. These are linked via network dependency — the deadline's extractive force depends on the stepfamily's pre-existing authority structure, and the stepfamily's authority is revealed as theatrical (piton) by the gift's demonstration that escape is possible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cinderella_midnight_deadline, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
