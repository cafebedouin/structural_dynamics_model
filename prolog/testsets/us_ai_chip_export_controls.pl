% ============================================================================
% CONSTRAINT STORY: us_ai_chip_export_controls
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_ai_chip_export_controls, []).

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
 *   constraint_id: us_ai_chip_export_controls
 *   human_readable: "US Export Controls on Advanced AI Chips to China"
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The US Department of Commerce has implemented a series of escalating
 *   export controls designed to prevent China from acquiring or producing
 *   the most advanced semiconductor chips, particularly those used for
 *   training large-scale AI models. The stated goal is to slow China's
 *   military modernization and prevent breakthroughs in military AI. This
 *   constraint forces US companies like Nvidia to create less powerful,
 *   export-compliant chips and denies Chinese tech firms access to
 *   state-of-the-art hardware, creating significant friction in the global
 *   tech ecosystem.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chinese AI Firms: Primary target (organized/constrained) — bears extraction by being denied critical technology.
 *   - Chinese AI Researchers/Startups: Powerless target (powerless/trapped) — completely blocked from accessing critical technology.
 *   - US Chip Designers (e.g., Nvidia): Secondary victim (powerful/constrained) — bears extraction through lost revenue and R&D costs for compliance.
 *   - US National Security Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from perceived strategic advantage and control.
 *   - Analytical Observer: Sees the full structure, including the coordination goal and the asymmetric costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_ai_chip_export_controls, 0.55).
domain_priors:suppression_score(us_ai_chip_export_controls, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_ai_chip_export_controls, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_ai_chip_export_controls, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, theater_ratio, 0.15).

% --- NL Profile Metrics ---
% Not applicable. This is a human-constructed, actively enforced policy, not a natural law.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_ai_chip_export_controls, tangled_rope).

% --- Binary flags ---
% This constraint requires a massive, active bureaucracy (Dept. of Commerce, etc.)
% to define performance thresholds, review licenses, and investigate violations.
domain_priors:requires_active_enforcement(us_ai_chip_export_controls). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_national_security_apparatus).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chinese_ai_firms).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, us_chip_designers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (CHINESE AI FIRMS)
% As a member of the 'victim' group with constrained exit, the engine derives
% a high directionality (d), leading to a high effective extraction (χ).
% From their perspective, this is a pure tool of coercion to block their
% technological development.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US NATIONAL SECURITY APPARATUS)
% As the designated 'beneficiary' with arbitrage exit (they set and can change
% the rules), the engine derives a very low, likely negative 'd'. The effective
% extraction (χ) is minimal or negative. From their view, it is a pure
% coordination tool to align industrial policy with strategic objectives.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view considers both the coordination function (beneficiary exists)
% and the high asymmetric extraction (victim exists), alongside the requirement for
% active enforcement. This combination is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% This captures the complex position of US chip designers like Nvidia.

% PERSPECTIVE 4: US CHIP DESIGNERS
% They are a 'victim' in terms of lost revenue, but they are also a 'powerful' US actor.
% Their exit is 'constrained'—they cannot simply ignore US law. They must spend
% resources to comply and design workarounds. The policy is not a pure snare to
% them, but it has a significant extractive component layered onto a national
% coordination goal they are compelled to follow. This is a classic Tangled Rope.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE POWERLESS TARGET (CHINESE AI RESEARCHERS/STARTUPS)
% This perspective represents smaller entities or individuals within the target
% nation who are completely trapped by the constraint. They lack the resources
% of large corporations to develop workarounds. For them, the policy is an
% insurmountable barrier to participation in cutting-edge AI development.
% As a 'victim' with 'trapped' exit, the engine derives the highest possible
% directionality, classifying this as a pure Snare.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_ai_chip_export_controls_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the primary target and beneficiary.
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_view_is_tangled_rope) :-
    % The system's objective classification must be Tangled Rope.
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_view_is_tangled_rope) :-
    % The constrained US industry also views it as a Tangled Rope, not a simple Rope.
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope, context(agent_power(powerful), _, exit_options(constrained), _)).

:- end_tests(us_ai_chip_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This is high, reflecting the massive economic friction. Billions in lost sales for US firms, billions in duplicative R&D for Chinese firms, and significant administrative costs for the US government.
 *   - Suppression (0.75): The rules are very effective at blocking access to the *highest-performance* chips. While workarounds exist (using many slower chips, developing indigenous tech), the primary path is strongly suppressed.
 *   - Tangled Rope Classification: The constraint possesses the two defining features of a Tangled Rope: a genuine coordination function (aligning US industry with national security goals) and a highly asymmetric extractive function (imposing severe costs on specific targeted entities).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the US national security apparatus (beneficiary), the constraint is a low-cost 'Rope' to coordinate foreign policy. For Chinese AI firms and researchers (targets), it is a 'Snare' designed to trap their technological development. The classification depends entirely on whether one is the author or the subject of the coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the clear beneficiary/victim declarations. The `us_national_security_apparatus` benefits from strategic denial. `chinese_ai_firms` are the direct targets of this denial. Crucially, `us_chip_designers` are also listed as victims because they bear direct, quantifiable economic costs (lost revenue), making their relationship to the constraint extractive despite being a domestic entity.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The case of US chip designers is a classic inter-institutional problem. They are a powerful US institution, but their relationship to this specific constraint is not one of a pure beneficiary. Their `exit_options(constrained)` reflects their legal obligation to comply, contrasting with the `exit_options(arbitrage)` of the government, which can alter the rules at will. This difference in exit optionality, combined with their victim status, correctly yields a `tangled_rope` classification from their perspective, capturing the coercive aspect of domestic industrial policy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two key errors. It does not mislabel the policy as a pure 'Snare', which would ignore the legitimate (from the US perspective) coordination goal. It also avoids calling it a 'Rope', which would erase the immense, targeted costs imposed on both foreign and domestic companies. The `tangled_rope` classification correctly identifies that it is both a coordination tool and an extractive instrument simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_ai_chip_export_controls,
    'Will the export controls durably slow China''s military AI, or will they trigger a ''Sputnik moment'' that accelerates Chinese self-sufficiency, ultimately backfiring?',
    'Observation of China''s domestic semiconductor production capabilities and military AI deployments over a 5-10 year horizon.',
    'If effective, it remains a successful Tangled Rope. If it backfires, its function degrades and it may become a high-maintenance Piton, imposing costs for no strategic gain.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_us_ai_chip_export_controls, empirical, 'Will export controls durably slow target nation''s AI progress or accelerate its technological self-sufficiency?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_ai_chip_export_controls, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness (0.55) > 0.46.
% The controls have tightened over time, increasing the friction and economic cost.
% The initial rules were less restrictive than the current ones.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(us_ai_chip_export_controls_tr_t0, us_ai_chip_export_controls, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_ai_chip_export_controls_tr_t5, us_ai_chip_export_controls, theater_ratio, 5, 0.12).
narrative_ontology:measurement(us_ai_chip_export_controls_tr_t10, us_ai_chip_export_controls, theater_ratio, 10, 0.15).

% Extraction over time (shows intensification of the policy):
narrative_ontology:measurement(us_ai_chip_export_controls_ex_t0, us_ai_chip_export_controls, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_ai_chip_export_controls_ex_t5, us_ai_chip_export_controls, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(us_ai_chip_export_controls_ex_t10, us_ai_chip_export_controls, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is a set of rules enforced by a state actor.
narrative_ontology:coordination_type(us_ai_chip_export_controls, enforcement_mechanism).

% Network relationships (structural influence edges)
% This policy is deeply intertwined with efforts to onshore and secure
% semiconductor supply chains.
narrative_ontology:affects_constraint(us_ai_chip_export_controls, semiconductor_supply_chain_resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The combination of
% beneficiary/victim declarations and distinct exit_options (arbitrage vs.
% constrained) allows the derivation engine to correctly compute the
% directionality (d) for each agent, accurately reflecting their
% different structural relationships to the policy.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */