% ============================================================================
% CONSTRAINT STORY: force_doctrine_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_force_doctrine_erosion, []).

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
 *   constraint_id: force_doctrine_erosion
 *   human_readable: Force Doctrine Erosion in Democratic Governance
 *   domain: political/institutional/governance
 *
 * SUMMARY:
 *   Force doctrine erosion describes the gradual decoupling of constitutional
 *   force constraints from their actual enforcement capacity. Democracies
 *   establish legal and procedural requirements for deploying state force —
 *   declarations of war, legislative authorization, separation of command
 *   authority — to distribute the decision to use force across multiple
 *   institutions and prevent unilateral executive deployment. Force doctrine
 *   erosion occurs when these constraints persist in form while degrading in
 *   function: executives deploy force unilaterally then request retroactive
 *   authorization (or ignore denial); legislatures maintain formal war powers
 *   but lack information to exercise meaningful oversight; security
 *   establishments classify information that would enable constraint
 *   enforcement; crisis narratives privilege speed of action over
 *   deliberation; precedent of unconstitutional deployment normalizes future
 *   deployment. The constraint exhibits high theater_ratio (0.68) because
 *   formal procedures (authorization votes, briefings, legal memos) continue
 *   while operative constraints vanish. The extractiveness value (0.58)
 *   reflects significant asymmetric control by executive and security
 *   institutions over the deployment decision, but not total — legislative
 *   funding authority and constitutional framework still function nominally.
 *   This makes force doctrine erosion a canonical tangled_rope: genuine
 *   coordination function (organizing force deployment) embedded within
 *   asymmetric extraction (executive unilateral control).
 *
 * KEY AGENTS:
 *   - Executive Power Center: Primary beneficiary (institutional/arbitrage) — expands operational scope through unilateral interpretation of constitutional war powers; can circumvent formal authorization through emergency declarations or retroactive legislative requests
 *   - Security Establishment: Primary beneficiary (institutional/constrained) — controls threat assessment and operational scope definition; benefits from expanded deployment and erosion that enables it
 *   - Legislative Body: Secondary beneficiary, primary victim (institutional/constrained) — nominally coordinates force through war powers authority and budget control; actually constrained by information asymmetries, crisis narratives, and executive fait accompli deployments
 *   - Constitutional Constraint Capacity: Primary victim (powerless/trapped) — abstract institutional structure that cannot organize or advocate; bears cost of erosion through degradation of its own function
 *   - Citizens Under Force: Primary victim (powerless/trapped) — subject to force deployment decisions made in institutions where they have no direct participation; suppressed through monopoly on legitimate violence and information asymmetries
 *   - Analytical Observer: Sees false mountain of 'inevitable state violence expansion' — risks naturalizing contingent institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(force_doctrine_erosion, 0.58).
domain_priors:suppression_score(force_doctrine_erosion, 0.65).
domain_priors:theater_ratio(force_doctrine_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(force_doctrine_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(force_doctrine_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(force_doctrine_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(force_doctrine_erosion, tangled_rope).
narrative_ontology:human_readable(force_doctrine_erosion, "Force Doctrine Erosion in Democratic Governance").
narrative_ontology:topic_domain(force_doctrine_erosion, "political/institutional/governance").

domain_priors:requires_active_enforcement(force_doctrine_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(force_doctrine_erosion, executive_branch_power_centers).
narrative_ontology:constraint_beneficiary(force_doctrine_erosion, security_establishment).
narrative_ontology:constraint_victim(force_doctrine_erosion, constitutional_constraint_capacity).
narrative_ontology:constraint_victim(force_doctrine_erosion, legislative_oversight_authority).
narrative_ontology:constraint_victim(force_doctrine_erosion, citizen_political_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN UNDER MILITARIZED CONTROL (SNARE) — Trapped by monopoly on legitimate violence; no exit from jurisdiction without loss of citizenship and property rights. Suppressed through asymmetric force capacity and information control. Maximum experienced extraction — coercion is both the mechanism and the product.
constraint_indexing:constraint_classification(force_doctrine_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BODY (TANGLED ROPE) — Nominally coordinates force deployment through war powers declarations and budget authority. Actual mechanism is mixed: genuine coordination function (formal authorization structures) embedded within asymmetric extraction (executive can deploy force unilaterally, then request authorization retroactively or ignore denial). Constrained by information asymmetries and crisis narratives that privilege executive action over deliberation.
constraint_indexing:constraint_classification(force_doctrine_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE POWER CENTER (ROPE) — Experiences force doctrine as pure coordination: mobilizing force capacity solves collective action problems (border security, deterrence, crisis response). From the executive perspective, the constraint enables function rather than blocking it. Arbitrage exit options (can circumvent constraints through executive privilege, emergency powers, reinterpretation of scope). Net beneficiary from the constraint structure.
constraint_indexing:constraint_classification(force_doctrine_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECURITY ESTABLISHMENT (TANGLED ROPE) — Coordinates threat assessment and deployment logistics (genuine function). Also extracts: controls operational scope, classifies information, defines threat thresholds unilaterally. Constrained by budget cycles and legal accountability structures that are increasingly nominal. Benefits from expanded force deployment and the erosion that enables it.
constraint_indexing:constraint_classification(force_doctrine_erosion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL CONSTRAINT SYSTEM (PITON) — War powers clauses, require declarations of war, presidential role as commander-in-chief — these institutional structures persist but function increasingly through theater and inertia. Formal procedures remain (AUMF voting, briefings, authorization requests) but lack enforcement: executive can act before authorization, deny legislators access to operational facts, or reinterpret scope unilaterally. The theater (formal procedures) decouples from function (actual constraint on force deployment). High theater_ratio indicates degradation.
constraint_indexing:constraint_classification(force_doctrine_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal level, force doctrine erosion might appear as an irreversible structural feature of modern governance: states with monopoly on violence will inevitably expand its use; information asymmetries make oversight impossible; crisis creates window for unilateral action that becomes precedent. This perspective risks naturalizing what is actually a contingent institutional arrangement. Engine will compute this as false summit.
constraint_indexing:constraint_classification(force_doctrine_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(force_doctrine_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(force_doctrine_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(force_doctrine_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(force_doctrine_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(force_doctrine_erosion, TR),
    TR >= 0.70.

:- end_tests(force_doctrine_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Measured as asymmetric control over force deployment decisions. Executive can deploy force unilaterally; legislature cannot block deployment unilaterally; citizens cannot participate in decision. The 0.58 value reflects that formal procedures remain operative (authorization votes, briefings, legal reviews) even though they lack enforcement capacity. If force deployment were completely unilateral with zero procedural constraint, extractiveness would approach 0.80+. The constraint has degraded but not collapsed. Suppression (0.65): Significant structural barriers to challenging force deployment include: monopoly on legitimate violence (armed challenge is impossible), information control (operational details are classified), institutional structure (citizens have no formal role in war powers), normalization through precedent (unilateral deployments become expected). Citizens face suppression approaching 0.85; legislators face suppression around 0.50 (they have formal authority but lack information and face crisis pressure). The population-level average is 0.65. Theater ratio (0.68): Formal procedures (congressional briefings, legal authorizations, AUMF votes) continue but increasingly decouple from actual constraint enforcement. The theater has increased over the measurement interval (0.42 → 0.68) as executive powers have expanded while formal procedures persist unchanged. This is Goodhart drift: the procedure (theater) is now gamed rather than functioning as constraint.
 *
 * PERSPECTIVAL GAP:
 *   The executive and security establishment see force doctrine as coordination mechanism (Rope) — they are solving collective action problems in threat response and deterrence. Their exit options are arbitrage: they can work around constraints through reinterpretation, emergency declaration, or retroactive authorization. They experience the constraint as enabling rather than blocking. The legislative body sees mixed coordination and extraction (Tangled Rope) — they have nominal authority but lack information and face crisis pressure that privileges executive action. Their constrained exit options (they can deny budget or authorization, but the political cost is high and the executive can deploy anyway) mean they experience asymmetric extraction. Citizens see pure extraction (Snare) — they are trapped by monopoly on violence and have no institutional role in force decisions. The constitutional constraint system itself (from the civilizational perspective) sees its own degradation (Piton) — formal procedures persist through institutional inertia while their enforcement capacity erodes. The analytical observer risks seeing an immutable mountain (states inevitably expand force use) but this is a false summit naturalizing contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural relationship to force deployment decision-making. Executive power centers are beneficiaries with arbitrage options: they gain control over deployment, they can exit formal constraints through unilateral action or reinterpretation. Security establishment are beneficiaries with constrained options: they gain expanded deployment authority, but their organizational survival depends on continued funding and political support, so exit is only partial. Legislative body occupies the critical position: nominally they are beneficiaries (they have formal war powers authority) but structurally they are victims (they cannot enforce constraints without information they don't have). Their exit options are constrained — they can deny budget or authorization, but this is politically costly and executive can deploy anyway. Citizens are pure victims with trapped exit: they bear the cost of force deployment, they have no formal role in authorization, they face suppression through monopoly on violence and information control. The constitutional constraint system is a victim: its function is erosion, its exit would require executive to voluntarily accept constraint, which executive avoids through unilateral interpretation. The mapping from beneficiary/victim status and exit options to d values produces the perspectival gap: executive sees low d (beneficiary + arbitrage), legislative sees high d (victim + constrained), citizens see maximum d (victim + trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   Force doctrine erosion resolves the mandatrophy by showing that the constraint is genuinely tangled: it coordinates force deployment (preventing chaotic or fragmented force use) while enabling asymmetric extraction (executive and security establishment control deployment against legislative and citizen preferences). The tension is not resolvable by classifying it as pure coordination (Rope) — the asymmetric extraction is real and structural. It is not resolvable by classifying it as pure extraction (Snare) — the coordination function is real; formal procedures do work at the margin; citizens are not subject to chaotic private violence but to organized state violence. The tangled_rope classification captures both: genuine coordination function embedded within asymmetric extraction. The theater_ratio (0.68) indicates that the coordination function is increasingly performative — the theater of authorization votes masks the operative extraction. The measurement trajectory (extractiveness rising 0.32 → 0.58, theater rising 0.42 → 0.68) shows degradation over time: the constraint is becoming more extractive and more performative, a classic Goodhart drift pattern where the measured procedure (theater) diverges from the actual constraint. The mandatrophy is resolved by recognizing that force doctrine erosion is a tangled_rope that is sliding toward snare as the coordination function degrades and the extraction mechanism becomes dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constraint_recovery_feasibility,
    'Can force doctrine constraints be reinstated after erosion, or does erosion create path dependence that makes restoration structurally impossible?',
    'Historical analysis of constitutional constraint recovery post-erosion (War Powers Act aftermath, post-Emergency Powers study); examination of whether precedent-setting unilateral deployments can be reversed through institutional reform or only through political crisis.',
    'If reversible: erosion is temporary (Scaffold). If irreversible: erosion is permanent degradation (Piton or Snare). If partial: ongoing oscillation between reform and capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_recovery_feasibility, empirical, 'Whether force doctrine constraints can be recovered after institutional erosion').

omega_variable(
    information_asymmetry_closure,
    'Do transparency mechanisms (classified document releases, inspector general oversight, intelligence committee briefings) actually reduce executive information monopoly or merely create performative access that preserves secrecy?',
    'Comparative analysis of actual information flow: percentage of operational details legislators can verify independently vs percentage dependent on executive branch disclosure; correlation between oversight committee briefings and subsequent legislative action (reversals, constraints, etc.)',
    'If asymmetry closed: suppression decreases, constraint becomes more viable (rope or tangled rope). If performative: suppression persists despite institutional mechanisms, sustaining snare classification for powerless agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_closure, empirical, 'Whether transparency mechanisms reduce executive information monopoly').

omega_variable(
    institutional_identity_capture,
    'Has the security establishment''s institutional identity become fused with force expansion such that constraint restoration would require the organization to redefine its core purpose?',
    'Institutional analysis: can the security apparatus articulate a positive identity focused on constraint and restraint, or does organizational culture, personnel incentives, and self-conception require continuous expansion? Survey of organizational statements, budget requests, personnel testimony.',
    'If identity-fused: security establishment operates under identity_locked exit, making collaborative constraint restoration impossible without organizational crisis. If separable: reform is viable through incentive restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_capture, conceptual, 'Whether security establishment identity is fused with force expansion').

omega_variable(
    citizen_participation_threshold,
    'At what level of force deployment does democratic participation collapse completely, or is suppression asymptotic (participation declines continuously without sharp threshold)?',
    'Longitudinal measurement of participation metrics (voting, protest participation, freedom of assembly exercise) correlated with force deployment scale and militarization intensity. Identification of breaking points or continuous decline pattern.',
    'If threshold exists: trapped citizens can in principle exit through crisis escape (Snare with collapse risk). If asymptotic: suppression is structural and permanent, supporting mountain classification from powerless perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(citizen_participation_threshold, empirical, 'Whether citizen participation has a discrete threshold or continuous decline under militarization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(force_doctrine_erosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(forc_tr_t0, force_doctrine_erosion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(forc_tr_t10, force_doctrine_erosion, theater_ratio, 10, 0.55).
narrative_ontology:measurement(forc_tr_t20, force_doctrine_erosion, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(forc_be_t0, force_doctrine_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(forc_be_t10, force_doctrine_erosion, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(forc_be_t20, force_doctrine_erosion, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(force_doctrine_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(force_doctrine_erosion, legislative_information_asymmetry).
narrative_ontology:affects_constraint(force_doctrine_erosion, emergency_powers_precedent).
narrative_ontology:affects_constraint(force_doctrine_erosion, classified_information_control).

% DUAL FORMULATION NOTE:
% Force doctrine erosion is downstream of specific institutional mechanisms: information asymmetry (executives can classify threat assessments), precedent normalization (unilateral deployments become expected), and emergency powers (executive can invoke crisis to bypass authorization). Each of these constraints has its own ε value and contributes to the overall erosion pattern. The network links show that recovery of force doctrine constraints requires addressing all three mechanism constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(force_doctrine_erosion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
