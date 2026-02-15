% ============================================================================
% CONSTRAINT STORY: biological_curiosity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_biological_curiosity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biological_curiosity
 *   human_readable: Curiosity (The Information-Seeking Drive)
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   Curiosity is the innate biological drive to seek out new information and reduce
 *   uncertainty in the environment. It acts as the "Intrinsic Motivation" that
 *   offsets the high costs of exploration, ensuring an agent does not settle
 *   for a suboptimal local peak. From an analytical perspective, it is a
 *   fundamental, unchangeable feature of mammalian neurobiology.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Explorer (moderate/mobile): Primary beneficiary — uses curiosity to discover new resources.
 *   - The Trapped Subject (powerless/trapped): Primary victim — curiosity leads to discovering unattainable alternatives, causing psychological distress.
 *   - The Institution (institutional/arbitrage): Secondary beneficiary — harnesses curiosity for education and innovation.
 *   - The Neuroscientist (analytical/analytical): Analytical observer — views the drive as a fixed biological law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_curiosity, 0.15).
domain_priors:suppression_score(biological_curiosity, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(biological_curiosity, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_curiosity, extractiveness, 0.15).
narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(biological_curiosity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(biological_curiosity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(biological_curiosity, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(biological_curiosity, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(biological_curiosity).
% domain_priors:requires_active_enforcement(biological_curiosity).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(biological_curiosity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(biological_curiosity, explorers).
narrative_ontology:constraint_beneficiary(biological_curiosity, scientific_endeavors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(biological_curiosity, subjects_in_total_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED SUBJECT (SNARE)
% In a highly restrictive environment (like a prison or a rigid bureaucracy),
% curiosity is a "Snare." Seeking out information leads to punishment or
% psychological despair, as the agent discovers alternatives they are legally
% or physically unable to reach.
constraint_indexing:constraint_classification(biological_curiosity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EDUCATIONAL SYSTEM (ROPE)
% For an educational system, curiosity is a "Rope." It is a fundamental
% biological drive that, when properly channeled, can be used as a powerful
% coordination mechanism for learning, exploration, and the transmission of
% knowledge.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NEUROSCIENTIST (MOUNTAIN)
% The observer sees the "Dopaminergic System" as a "Mountain." The reward for
% information-seeking is a hard-coded feature of the mammalian brain. It is an
% immutable law of behavioral biology designed to solve the
% Exploration/Exploitation trade-off.
constraint_indexing:constraint_classification(biological_curiosity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DISCOVERER (ROPE)
% For the agent with surplus energy, curiosity is a "Rope." It coordinates
% their internal resources to engage with the unknown. It allows them
% to climb out of stagnant environments and discover new opportunities.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_curiosity_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(biological_curiosity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_curiosity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(biological_curiosity, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeTarget = snare,
    TypeBeneficiary = rope,
    TypeAnalyst = mountain,
    TypeTarget \= TypeBeneficiary,
    TypeBeneficiary \= TypeAnalyst.

test(threshold_validation) :-
    % The analytical view is Mountain, so metrics must comply.
    narrative_ontology:constraint_metric(biological_curiosity, extractiveness, E),
    narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(biological_curiosity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core idea is that curiosity is a fundamental, non-extractive biological
 *   drive (ε=0.15, S=0.05), making it a Mountain from an analytical view.
 *   However, its *effect* is highly context-dependent. For those with agency
 *   (explorers, institutions), it's a Rope for coordination and discovery.
 *   For those without agency (trapped subjects), the drive persists but only
 *   reveals painful, inaccessible truths, making it function as a Snare of
 *   psychological extraction. The low base metrics reflect the drive itself,
 *   while the perspectival classifications reflect its experienced outcome.
 *
 * PERSPECTIVAL GAP:
 *   - The Trapped Subject (powerless) sees a Snare because the drive to know,
 *     combined with an inability to act, creates suffering.
 *   - The Institution (institutional) sees a Rope, a natural resource to be
 *     harnessed for coordination (education, R&D).
 *   - The Neuroscientist (analytical) sees a Mountain, an immutable feature
 *     of neurobiology that cannot be altered, only understood or channeled.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries are agents who can act on the information curiosity provides
 *     (explorers, scientists). They benefit from the coordination function.
 *   - Victims are those for whom the information provides no agency and only
 *     highlights their constraints, leading to psychological cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This story shows how a single, low-extraction phenomenon can be perceived
 *   as a Snare. The classification system correctly identifies the base
 *   constraint as non-extractive (Mountain/Rope) while still capturing the
 *   Snare experience of the powerless agent. This prevents mislabeling the
 *   fundamental drive as extractive, while acknowledging its harmful effects
 *   in coercive environments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_biological_curiosity,
    "At what point does the search for information (Curiosity) stop being a Rope and start being a 'Snare of Distraction' due to information overload?",
    "Long-term studies on focus-depletion in high-entropy digital environments",
    "If overload threshold is low, the Rope of Curiosity snaps easily, and agents fall into a Snare of paralysis. If high, it remains a robust coordination tool.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_biological_curiosity, empirical, "Determining the cognitive threshold where information-seeking yields negative returns.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(biological_curiosity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Not required for this constraint as
% base_extractiveness (0.15) is below the 0.46 threshold. The biological
% drive is assumed to be stable over the interval.

/*
narrative_ontology:measurement(biological_curiosity_tr_t0, biological_curiosity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(biological_curiosity_tr_t5, biological_curiosity, theater_ratio, 5, 0.05).
narrative_ontology:measurement(biological_curiosity_tr_t10, biological_curiosity, theater_ratio, 10, 0.05).

narrative_ontology:measurement(biological_curiosity_ex_t0, biological_curiosity, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(biological_curiosity_ex_t5, biological_curiosity, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(biological_curiosity_ex_t10, biological_curiosity, base_extractiveness, 10, 0.15).
*/

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No specific coordination type or network relationships declared for this fundamental drive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% declarations and exit options is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */