% ============================================================================
% CONSTRAINT STORY: lethal_targeting_of_journalists
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_lethal_targeting_of_journalists, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lethal_targeting_of_journalists
 *   human_readable: De Facto Policy of Lethal Targeting of Journalists in Conflict Zones
 *   domain: political/military
 *
 * SUMMARY:
 *   Based on reports of disproportionate journalist killings by a state actor
 *   (e.g., Israel, as per the source article), this constraint models the
 *   emergent policy of lethally targeting journalists. This practice serves
 *   to control the information environment by creating a high-risk zone for
 *   independent reporting, thereby suppressing narratives that challenge the
 *   state's official account of its military operations.
 *
 * KEY AGENTS (by structural relationship):
 *   - Journalists in Conflict Zones: Primary target (powerless/trapped) — bear the ultimate extraction: risk of death and suppression of their work.
 *   - State Military/Political Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from a controlled information space and reduced scrutiny.
 *   - International Watchdog Bodies (CPJ, RSF, UN): Secondary actor (institutional/constrained) — oppose the constraint but lack direct enforcement power.
 *   - Analytical Observer: Sees the full structure of coercive coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lethal_targeting_of_journalists, 0.55).
domain_priors:suppression_score(lethal_targeting_of_journalists, 0.90).   % Structural property (raw, unscaled). High due to lethal threat.
domain_priors:theater_ratio(lethal_targeting_of_journalists, 0.15).       % The actions are functional, not primarily theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, extractiveness, 0.55).
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lethal_targeting_of_journalists, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(lethal_targeting_of_journalists). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lethal_targeting_of_journalists, israeli_state_apparatus).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lethal_targeting_of_journalists, journalists_in_conflict_zones).
narrative_ontology:constraint_victim(lethal_targeting_of_journalists, global_news_consumers).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Journalists on the ground experience this as a pure extraction of their safety
% and ability to work. Engine derives d from victim + trapped exit -> high d -> high chi.
% χ = 0.55 * f(d=0.95) * σ(regional=0.9) = 0.55 * 1.42 * 0.9 ≈ 0.70. This meets the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The state actor sees this as an effective coordination mechanism for managing the
% information environment. Engine derives d from beneficiary + arbitrage exit -> low d -> negative chi.
% χ = 0.55 * f(d=0.05) * σ(national=1.0) = 0.55 * -0.12 * 1.0 ≈ -0.07. Negative chi is Rope.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the coercive coordination function (benefiting the state) and the
% severe, asymmetric extraction (costing journalists their lives).
% χ = 0.55 * f(d=0.73) * σ(global=1.2) = 0.55 * 1.15 * 1.2 ≈ 0.76. This is in the Tangled Rope range [0.40, 0.90].
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% International watchdog bodies (e.g., CPJ, RSF, UN) are institutional actors,
% but they are constrained and structurally opposed to the constraint. Their
% derived d would be ambiguous. An override clarifies their position as aligned
% with the victims, experiencing high extraction.
% χ = 0.55 * f(d=0.65) * σ(global=1.2) = 0.55 * 1.00 * 1.2 ≈ 0.66. This borders Snare/Tangled Rope,
% reflecting their view of the constraint's extreme severity.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lethal_targeting_of_journalists_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(lethal_targeting_of_journalists, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: Target (Snare) vs Beneficiary (Rope)').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(lethal_targeting_of_journalists, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(lethal_targeting_of_journalists, _),
    narrative_ontology:constraint_victim(lethal_targeting_of_journalists, _),
    domain_priors:requires_active_enforcement(lethal_targeting_of_journalists).

:- end_tests(lethal_targeting_of_journalists_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, reflecting the severe cost imposed on journalists—the extraction of their ability to report, and potentially their lives.
 *   - Suppression (0.90): Extremely high. The threat of lethal force is a near-absolute method of suppressing alternative information sources and deterring journalistic presence.
 *   - Theater Ratio (0.15): Low. The actions are brutally functional, aimed at producing a material outcome (fewer reporters, controlled narrative) rather than a symbolic one.
 *   The combination of a coordination function (for the beneficiary) and high asymmetric extraction (for the victim) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - Journalists (Powerless/Trapped) see a Snare. From their viewpoint, there is no coordination benefit, only lethal risk and suppression. The effective extraction is maximal.
 *   - The State Actor (Institutional/Arbitrage) sees a Rope. The constraint is a tool for coordinating the information environment to align with strategic objectives. The negative costs are externalized onto others, so from their indexical position, effective extraction is negative (a net benefit).
 *   - The Analytical observer sees a Tangled Rope, recognizing the validity of both perspectives: a system that provides coordination for one group by means of severe extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `israeli_state_apparatus`. They directly benefit from a less scrutinized operational environment. Their `arbitrage` exit options grant them a low directionality `d`.
 *   - Victim: `journalists_in_conflict_zones`. They are the direct targets who bear the costs. Their `trapped` status (they must be in the zone to do their job) gives them a very high `d`. This structural data is what drives the perspectival gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The dynamic between the state actor and international watchdogs is captured. Both are `institutional` but have opposing relationships to the constraint. The watchdog's `constrained` exit options and structural opposition mean they perceive high extraction. We use a directionality override to model their position more accurately than the standard derivation, reflecting that despite being an institution, they are functionally aligned with the constraint's targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. 1) It does not mislabel the constraint as a pure Snare, because it acknowledges the genuine (if coercive) coordination function it serves for the beneficiary. 2) It does not mislabel it as a simple Rope, because it quantifies the massive, asymmetric cost imposed on the target group. The Tangled Rope classification captures this dual nature, which is essential for understanding how such constraints persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lethal_targeting_of_journalists,
    'Is the targeting of journalists a deliberate, top-down state policy or an emergent battlefield norm tolerated by commanders?',
    'Leaked internal military directives, whistleblower testimony from military units, or statistical analysis of targeting patterns vs. accidental crossfire incidents.',
    'If deliberate policy -> higher ε, closer to pure Snare. If emergent norm -> lower ε, but still a Tangled Rope due to institutional tolerance and benefit.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lethal_targeting_of_journalists, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has likely intensified over time. The data models an
% increase in brazenness (falling theater) and extractive effect (rising ε).
% Base extractiveness is > 0.46, so this data is required.

% Theater ratio over time (metric substitution in reverse: less performance, more direct action)
narrative_ontology:measurement(ltj_tr_t0, lethal_targeting_of_journalists, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ltj_tr_t5, lethal_targeting_of_journalists, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ltj_tr_t10, lethal_targeting_of_journalists, theater_ratio, 10, 0.15).

% Extraction over time (extraction accumulation)
narrative_ontology:measurement(ltj_ex_t0, lethal_targeting_of_journalists, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ltj_ex_t5, lethal_targeting_of_journalists, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ltj_ex_t10, lethal_targeting_of_journalists, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The coordination type is enforcement of an information regime.
narrative_ontology:coordination_type(lethal_targeting_of_journalists, enforcement_mechanism).

% This constraint is coupled with the broader system of international law.
% Its existence degrades the latter.
narrative_ontology:affects_constraint(lethal_targeting_of_journalists, international_humanitarian_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is used for the institutional watchdog perspective. The standard
% derivation for an institutional actor might be ambiguous. This override
% clarifies that their structural position is one of opposition, aligned with
% the constraint's victims, leading them to perceive high extraction. We set
% their d to 0.65, equivalent to a 'moderate' power actor.
constraint_indexing:directionality_override(lethal_targeting_of_journalists, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */