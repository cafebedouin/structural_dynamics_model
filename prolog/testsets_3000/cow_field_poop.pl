% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cow_field_poop, []).

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
 *   constraint_id: cow_field_poop
 *   human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Cow Field Hazard models a fundamental social and psychological
 *   constraint: the presence of negative events, social friction, and
 *   nonsense as an inherent feature of any navigable environment. This is not
 *   a pathology — it is a structural reality that creates coordination
 *   problems and opportunities. Agents who share awareness of hazards can
 *   coordinate to avoid them; agents isolated from collective knowledge
 *   experience pure extraction. The constraint exhibits all six
 *   classification types depending on the observer's structural position:
 *   natural inevitability (mountain), coordination mechanism (rope),
 *   information asymmetry with mixed benefits (tangled rope), information
 *   isolation with pure costs (snare), and temporary coordination problems
 *   with sunset clauses (scaffold). The theater_ratio increases over time as
 *   hazard-warning institutions persist through inertia rather than function,
 *   a classic piton pattern.
 *
 * KEY AGENTS:
 *   - Hazard-Aware Agents: Primary beneficiaries (organized/mobile) — benefit from collective knowledge and reduced stepping costs
 *   - Field Epistemic Commons: Secondary beneficiary (analytical) — collective knowledge base that improves over time
 *   - Unaware Newcomers: Secondary victims (powerless/constrained) — experience both coordination benefits and information asymmetry extraction
 *   - Isolated Agents: Primary victims (powerless/trapped) — experience pure hazard costs with no coordination access
 *   - Hazard-Mapping Initiative: Organized coordinator (organized/mobile) — systematic awareness and navigation training with sunset horizon
 *   - Warning Institutions: Inertial actors (institutional/arbitrage) — maintain warning rituals despite declining functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cow_field_poop, 0.35).
domain_priors:suppression_score(cow_field_poop, 0.4).
domain_priors:theater_ratio(cow_field_poop, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cow_field_poop, extractiveness, 0.35).
narrative_ontology:constraint_metric(cow_field_poop, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cow_field_poop, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cow_field_poop, rope).
narrative_ontology:human_readable(cow_field_poop, "The Cow Field Hazard (Pragmatic Avoidance)").
narrative_ontology:topic_domain(cow_field_poop, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cow_field_poop, hazard_aware_agents).
narrative_ontology:constraint_beneficiary(cow_field_poop, coordination_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Negative events, social friction, and nonsense are inherent features of any complex environment. No agent can eliminate hazards from existence — they can only navigate them. This is an irreducible structural fact, not a policy choice. The constraint emerges naturally from the combinatorial explosion of possible failure modes in any social system.
constraint_indexing:constraint_classification(cow_field_poop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PRAGMATIC COORDINATOR (ROPE) — Agents who acknowledge the inevitability of hazards benefit from shared awareness and collective avoidance protocols. The constraint functions as pure coordination: 'yes, there is poop in the field' is information that enables cooperative navigation. Suppression is moderate — groups can share maps and strategies without coercion. Extraction is minimal — the information system itself creates no winners and losers, only shared immunity.
constraint_indexing:constraint_classification(cow_field_poop, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: UNAWARE NEWCOMER (TANGLED ROPE) — Those unfamiliar with the hazards face both coordination benefits (can learn from others' experience) and extraction (more exposed to costs of stepping in poop, figuratively and literally). Others' knowledge of hazard locations benefits them asymmetrically — the experienced agents coordinate successfully while newcomers still stumble. High suppression due to information asymmetry and social shame around admitting inexperience.
constraint_indexing:constraint_classification(cow_field_poop, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ISOLATED AGENT (SNARE) — An agent with no access to others' warnings, no communication channels, and no ability to exit the field experiences pure extraction: all hazards, no coordination. Maximum suppression (no alternatives) and high experienced extractiveness due to complete information isolation. The constraint appears as pure coercion — stepped in poop and has no one to blame but the universe.
constraint_indexing:constraint_classification(cow_field_poop, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: HAZARD-MAPPING INITIATIVE (SCAFFOLD) — Organized groups implementing systematic hazard awareness and navigation training see this as a temporary coordination problem with a sunset clause. As awareness improves and practice solidifies into habit, the constraint's suppression and theater decline — the need for external coordination mechanisms decreases. Exit paths emerge: better shoes, elevated walkways, alternative fields. Theater_ratio is declining as practical solutions replace performative cautionary tales.
constraint_indexing:constraint_classification(cow_field_poop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: WARNING INSTITUTION (PITON) — Organizations that maintain hazard warnings (signs, training protocols, designated 'poop spotters') persist through institutional inertia even when actual hazard awareness has improved and agents no longer need the warnings. The ritual of warnings persists long after the functional information transfer has degraded. Theater_ratio is high (0.65+) — much activity is performative compliance with hazard-warning norms rather than substantive new information.
constraint_indexing:constraint_classification(cow_field_poop, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cow_field_poop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cow_field_poop, TR),
    TR >= 0.70.

:- end_tests(cow_field_poop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate-low. The constraint's primary function is coordination — sharing hazard awareness enables collective navigation. While information asymmetry creates some extraction (experienced agents have advantages over newcomers), the asymmetry decays as information spreads. The value reflects that this is primarily a coordination problem, not an extraction mechanism. Suppression (0.40): Moderate. Significant barriers include information access gaps, social shame around admitting inexperience, tacit knowledge in hazard recognition, and local variation in hazard types. But suppression is not total — information sharing is possible and agents can learn. Theater ratio (0.55): Moderate-high, rising over time. Early in the constraint's operation, warnings and coordination protocols serve genuine functions. Over time (measured at time_point 10), warning institutions persist through inertia — the theater ratio rises to 0.65 as performative compliance (posting signs, conducting trainings) outlasts functional information transfer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as natural law (inevitable hazards exist), coordination mechanism (shared awareness helps), mixed extraction (information asymmetry benefits some agents more than others), pure hazard (isolation), temporary coordination failure (fixable through better systems), and inertial ritual (outdated warning practices). The gap between analytical/civilizational (mountain) and powerless/immediate (snare) perspectives is maximal — one sees inevitable natural structure, the other sees pure coercion. The organized/mobile perspective (rope and scaffold) sees genuinely solvable coordination problems. The institutional/arbitrage perspective (piton) sees its own process as degraded but persisting.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's relationship to the hazard information system. Agents with arbitrage options (experienced, well-connected) derive low d — they benefit from the coordination function. Trapped agents with no information access derive high d — they bear all costs. Organized agents with mobile exit options derive moderate d — they experience both coordination benefits and some information advantage. The sigmoid f(d) produces experienced extractiveness values that reflect actual felt costs and benefits: beneficiaries see rope (low chi), isolated agents see snare (high chi), mixed agents see tangled rope or scaffold depending on time horizon and whether exit paths exist. The constraint's effectiveness depends entirely on whether d values map to real structural access — agents with actual communication channels derive lower d than agents with communication barriers, independent of nominal power level.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hazard_inevitability_threshold,
    'At what ratio of hazards-to-safe-zones does pragmatic avoidance become impossible and the constraint transitions from rope to snare?',
    'Empirical mapping of hazard density in real navigational environments; correlation with coordination system breakdown rates',
    'If threshold is high (>70% hazards): most human environments are snares, not coordination systems. If threshold is low (<30% hazards): rope classification is robust across most real scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_inevitability_threshold, empirical, 'Threshold for hazard density causing coordination collapse').

omega_variable(
    social_vs_physical_hazards,
    'Do social hazards (betrayal, shame, rejection) follow the same navigational logic as physical hazards (obstacles, danger), or do they require distinct constraint models?',
    'Comparative analysis of avoidance behaviors across physical and social domains; identification of common structural features or domain-specific extraction mechanisms',
    'If equivalent: single rope model explains both. If distinct: social hazards may be snares while physical hazards remain rope (different base_properties for different domains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_physical_hazards, conceptual, 'Whether social and physical hazards share constraint structure').

omega_variable(
    information_asymmetry_extraction,
    'Does the asymmetry between hazard-aware and hazard-unaware agents constitute extractive exploitation or legitimate first-mover advantage?',
    'Behavioral analysis of experienced agents'' willingness to share hazard information; correlation with reputational benefits, social obligation norms, and group identity',
    'If extractive: unaware agents face snare conditions. If legitimate: coordination is rope with natural learning gradient. Classification of tangled_rope vs rope depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_extraction, preference, 'Whether information asymmetry in hazard awareness constitutes extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cow_field_poop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cowfield_tr_t0, cow_field_poop, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cowfield_tr_t5, cow_field_poop, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cowfield_tr_t10, cow_field_poop, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(cowfield_be_t0, cow_field_poop, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cowfield_be_t5, cow_field_poop, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cowfield_be_t10, cow_field_poop, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cow_field_poop, information_standard).
narrative_ontology:affects_constraint(cow_field_poop, information_asymmetry_extraction).
narrative_ontology:affects_constraint(cow_field_poop, social_stigma_suppression).

% DUAL FORMULATION NOTE:
% The Cow Field Hazard represents two structurally distinct claims: (1) hazards exist and are navigable through collective awareness (coordination problem, ε ≈ 0.25), and (2) information asymmetry about hazards creates extraction opportunities for aware agents over unaware ones (extraction mechanism, ε ≈ 0.45 in isolated contexts). This story emphasizes the coordination reading (rope, tangled_rope, scaffold) but acknowledges the extraction reading in the snare and piton perspectives. If the extraction mechanism becomes primary (hazard information is actively suppressed or weaponized), a separate constraint story should be authored with higher extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
