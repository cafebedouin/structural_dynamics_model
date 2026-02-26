% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

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
 *   constraint_id: bip_narrative_illusion
 *   human_readable: The Black Iron Prison (BIP) and Sensory Optimization
 *   domain: philosophical/social/technological
 *
 * SUMMARY:
 *   This constraint models the 'Black Iron Prison' (BIP), a concept from
 *   Philip K. Dick's gnostic philosophy, as a system of social control. The
 *   BIP is a perceived false reality constructed by institutional powers
 *   ('The Empire') to keep individuals subordinate. In this model, modern
 *   sensory optimization—through targeted advertising, algorithmic media
 *   feeds, and curated consumer experiences—serves as the primary mechanism
 *   for building and maintaining the prison walls. The constraint is not a
 *   physical prison but a psychic and social one that operates by shaping
 *   perception to foreclose alternatives to the dominant system.
 *
 * KEY AGENTS:
 *   - The Empire's Architects: Primary beneficiary (institutional/arbitrage) — Corporate and state powers that design and profit from the system of control.
 *   - Unawakened Individuals: Primary victim (powerless/trapped) — The general populace, whose perception is managed and whose autonomy is extracted.
 *   - The Gnostic/Resistor: Secondary victim (moderate/constrained) — Individuals aware of the prison who struggle against its influence.
 *   - The Utopian Technologist: Organized agent (organized/mobile) — Believes technology is a temporary means to a better end, viewing the control system as a scaffold.
 *   - The Analytical Observer: Observer (analytical/analytical) — Describes the system's structure, risking either accurate classification (Tangled Rope) or naturalizing it (Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bip_narrative_illusion, 0.65).
domain_priors:suppression_score(bip_narrative_illusion, 0.75).
domain_priors:theater_ratio(bip_narrative_illusion, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.65).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bip_narrative_illusion, theater_ratio, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bip_narrative_illusion, tangled_rope).
narrative_ontology:human_readable(bip_narrative_illusion, "The Black Iron Prison (BIP) and Sensory Optimization").
narrative_ontology:topic_domain(bip_narrative_illusion, "philosophical/social/technological").

domain_priors:requires_active_enforcement(bip_narrative_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, the_empire_institutional_power).
narrative_ontology:constraint_victim(bip_narrative_illusion, unawakened_individuals).
narrative_ontology:constraint_victim(bip_narrative_illusion, human_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNWAKENED INDIVIDUAL (SNARE) — Trapped within a reality constructed by sensory optimization and media narratives. Lacks awareness of the prison's existence, making exit impossible. Their attention, economic output, and compliance are extracted to power the system. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11. This high χ value firmly classifies the experience as a Snare.
constraint_indexing:constraint_classification(bip_narrative_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE EMPIRE'S ARCHITECTS (ROPE) — The institutional powers that benefit from the BIP see it as a necessary system for social coordination and economic stability. Sensory optimization is merely a tool for managing consumer demand and ensuring social cohesion. From this vantage point, extraction is invisible, perceived only as efficient system management. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. A net beneficiary.
constraint_indexing:constraint_classification(bip_narrative_illusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — This observer (e.g., a philosopher like Philip K. Dick) recognizes both the coordination function (society requires structure) and the severe, asymmetric extraction. The system is a hybrid: it provides order but at the cost of authentic reality and individual sovereignty. This is the default analytical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE UTOPIAN TECHNOLOGIST (SCAFFOLD) — Believes that current control systems, including sensory optimization, are temporary scaffolds necessary to guide humanity towards a more advanced, liberated state. They see a sunset clause where technology eventually transcends its control function to enable true human flourishing. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.49. The classification is Scaffold due to the perceived sunset clause, despite the high chi.
constraint_indexing:constraint_classification(bip_narrative_illusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE MID-LEVEL BUREAUCRAT (PITON) — An agent within the system who perpetuates its rules not out of malice, but inertia. They experience the system's functions as largely performative rituals (e.g., quarterly reports, compliance checks) detached from any real purpose, yet the rules must be followed. The high theater_ratio (0.72) triggers the Piton classification, reflecting a system running on degraded, inertial logic.
constraint_indexing:constraint_classification(bip_narrative_illusion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE SOCIAL DETERMINIST (MOUNTAIN) — Argues that a 'Black Iron Prison' is an inevitable, emergent property of any large-scale technological society. It is a natural law of social physics, not a contingent creation. This perspective naturalizes the constraint, rendering it immutable. The engine will flag this as a false summit, as the high ε and suppression values are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(bip_narrative_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bip_narrative_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bip_narrative_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bip_narrative_illusion, TR),
    TR >= 0.70.

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high, representing the immense value (attention, data, economic activity, political compliance) extracted from the populace. Suppression (0.75) is also high, as the system actively marginalizes or co-opts dissenting narratives and alternative lifestyles, making escape or rebellion seem impossible or irrational. Theater Ratio (0.72) is high because the system's primary mechanism of control is the performance of freedom and choice (e.g., consumer choice, electoral politics) that masks an underlying lack of structural agency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar, demonstrating how a single set of structural properties can generate all six classifications. For the trapped victim, it is a Snare. For the beneficiary, it is a Rope for social coordination. For the aware resistor, it is a Tangled Rope of control and function. For the optimistic builder, it is a temporary Scaffold. For the inertial bureaucrat, it is a degraded Piton. For the determinist philosopher, it is an inescapable Mountain. The perspectival gap is total, spanning the entire classification system.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries ('The Empire') have arbitrage exit options and a low derived directionality (d), resulting in a negative effective extraction (χ) and a Rope classification. Victims ('Unawakened Individuals') are trapped with no exit, leading to a high d, a high f(d) multiplier, and a χ value that crosses the Snare threshold. Other agents fall in between, with their power, exit options, and structural relationship determining their d value and thus their perspectival classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by showing that the question 'Which type is it?' is ill-posed. The constraint's identity is the complete set of all perspectival classifications derived from its fixed base properties. The system is simultaneously a Rope to its creators and a Snare to its captives. Deferential Realism's power lies in holding these contradictory but structurally valid truths within a single analytical framework, preventing the collapse into a single, privileged description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_emergence,
    'Is the Black Iron Prison a consciously designed system by ''The Empire'' or an agentless, emergent phenomenon of complex systems?',
    'Tracing decision-making chains within key institutions (corporate, state) to identify intent versus path-dependency and unintended consequences.',
    'If consciously designed, it reinforces the Snare classification for victims. If emergent, it strengthens the Piton and Tangled Rope classifications, pointing to systemic dysfunction rather than directed malice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agency_vs_emergence, conceptual, 'Distinguishing between conscious design and emergent properties of the BIP.').

omega_variable(
    technological_determinism,
    'Will advanced technologies like AI and neuro-interfaces inevitably reinforce the prison, or can they be used as tools for ''gnosis'' and liberation?',
    'Empirical analysis of the centralizing vs. decentralizing effects of new technologies as they are deployed.',
    'If they reinforce control, the Scaffold perspective is invalidated. If they enable liberation, the system''s suppression score would decrease over time, potentially transforming it into a true Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism, empirical, 'Whether technology is a tool for liberation or control.').

omega_variable(
    existence_of_exterior,
    'Is there a coherent, accessible ''true reality'' outside the constructed reality of the BIP?',
    'This is a metaphysical question, unresolvable by empirical means. It can only be explored via philosophical argument and phenomenological reports.',
    'If no exterior exists, the BIP is functionally a Mountain—an inescapable condition of existence. If an exterior exists, then classifications like Snare and Tangled Rope, which imply a loss of something real, are valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existence_of_exterior, conceptual, 'The metaphysical status of a ''true reality'' outside the BIP.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bip_narrative_illusion, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bip__tr_t1980, bip_narrative_illusion, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(bip__tr_t2002, bip_narrative_illusion, theater_ratio, 2002, 0.58).
narrative_ontology:measurement(bip__tr_t2024, bip_narrative_illusion, theater_ratio, 2024, 0.72).

% Extraction over time
narrative_ontology:measurement(bip__be_t1980, bip_narrative_illusion, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(bip__be_t2002, bip_narrative_illusion, base_extractiveness, 2002, 0.55).
narrative_ontology:measurement(bip__be_t2024, bip_narrative_illusion, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bip_narrative_illusion, enforcement_mechanism).
narrative_ontology:affects_constraint(bip_narrative_illusion, social_media_addiction).
narrative_ontology:affects_constraint(bip_narrative_illusion, consumer_debt_trap).
narrative_ontology:affects_constraint(bip_narrative_illusion, narrative_collapse_disorder).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
