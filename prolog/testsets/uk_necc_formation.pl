% ============================================================================
% CONSTRAINT STORY: uk_necc_formation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-31
% ============================================================================

:- module(constraint_uk_necc_formation, []).

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
 *   constraint_id: uk_necc_formation
 *   human_readable: UK National Economic Crime Centre (NECC) Formation
 *   domain: political/economic
 *
 * SUMMARY:
 *   The UK government is establishing a new law enforcement agency, the
 *   National Economic Crime Centre (NECC), modelled on the American FBI, to
 *   combat economic crimes such as fraud, money laundering, and kleptocracy.
 *   This constraint represents the new regulatory and enforcement powers of this
 *   state body, which simultaneously provides a coordination function (a stable
 *   economic environment) while asymmetrically extracting resources and liberty
 *   from its targets.
 *
 * KEY AGENTS (by structural relationship):
 *   - Economic Crime Syndicates: Primary target (powerless/trapped) — bears the full coercive force of the new agency.
 *   - UK Government & Public: Primary beneficiary (institutional/arbitrage) — benefits from reduced crime, increased tax revenue, and geopolitical stability.
 *   - UK Financial Sector: Secondary actor (organized/constrained) — benefits from a level playing field but bears new compliance costs.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric coercion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_necc_formation, 0.48).
domain_priors:suppression_score(uk_necc_formation, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_necc_formation, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_necc_formation, extractiveness, 0.48).
narrative_ontology:constraint_metric(uk_necc_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_necc_formation, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_necc_formation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_necc_formation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_necc_formation, uk_government_and_public).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_necc_formation, economic_crime_syndicates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For an economic criminal, the NECC is a pure instrument of coercion.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(uk_necc_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the UK government, the NECC is a tool for economic stability and order.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(uk_necc_formation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function and the asymmetric extraction.
% This is the basis for the constraint's formal claim.
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATED INDUSTRY (TANGLED ROPE)
% A legitimate financial firm experiences this as a coordination mechanism that
% also imposes compliance costs (a form of extraction).
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_necc_formation_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the primary target and beneficiary.
    constraint_indexing:constraint_classification(uk_necc_formation, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_necc_formation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint can only be a Tangled Rope if it has beneficiaries, victims, AND requires enforcement.
    narrative_ontology:constraint_beneficiary(uk_necc_formation, _),
    narrative_ontology:constraint_victim(uk_necc_formation, _),
    domain_priors:requires_active_enforcement(uk_necc_formation).

:- end_tests(uk_necc_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects a powerful state institution with the authority to seize assets, impose massive fines, and imprison individuals. The extraction is significant but is not its sole purpose, distinguishing it from a pure wealth-transfer mechanism (which would have ε > 0.6).
 *   - Suppression Score (0.65): The agency's explicit goal is to suppress illegal economic activity. Its success is contingent on its ability to make alternatives to legal commerce prohibitively risky and costly, justifying a high suppression score.
 *   - Theater Ratio (0.10): As a new institution, its activities are almost entirely functional. Bureaucratic drift and performative action (theater) are currently minimal.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an 'economic_crime_syndicate' (the victim), the NECC is a pure Snare designed to trap and extract. There is no coordination benefit from their perspective. For the 'uk_government_and_public' (the beneficiary), it's a Rope that coordinates economic activity towards legal channels, ensuring stability and fairness. The institution's function is perceived entirely differently depending on one's structural relationship to its enforcement actions.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the declared beneficiaries and victims.
 *   - Beneficiary: `uk_government_and_public`. This group gains a more stable, predictable, and fair economic system. The engine assigns a low directionality (`d`) value, leading to a low or negative effective extraction (`χ`), classifying the constraint as a Rope from their view.
 *   - Victim: `economic_crime_syndicates`. This group is the direct target of coercive enforcement. The engine assigns a high `d` value, leading to a high `χ`, classifying it as a Snare from their view.
 *   This mirrors the reality that state enforcement is simultaneously a public good for one group and a targeted liability for another.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of state power. A naive analysis might label the NECC a pure Rope, ignoring the coercive extraction imposed on its targets. Conversely, a cynical analysis might label it a pure Snare, ignoring its genuine and necessary function in coordinating a complex economy. The Tangled Rope classification avoids both errors by requiring evidence of both a coordination function (`constraint_beneficiary`) and asymmetric extraction (`constraint_victim` + `requires_active_enforcement`), providing a more complete and accurate model of the institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_necc_formation,
    'Will the NECC maintain its functional mandate, or will it succumb to mission creep, political capture, or bureaucratic inertia?',
    'Long-term (10-20 year) audit of prosecution targets, success rates, and comparison against its stated goals. Evidence of use against political opponents or devolving into theatrical enforcement would resolve this.',
    'If it maintains function, it remains a Tangled Rope. If it becomes a tool for political suppression, it becomes a Snare for unintended victims. If it becomes ineffective but maintains its budget, it degrades into a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_necc_formation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the NECC's projected first decade.
% Required because base_extractiveness (0.48) > 0.46.
% The model shows a slight increase in bureaucratic theater and a stabilization
% of extractive capacity as the institution matures.

% Theater ratio over time (projected):
narrative_ontology:measurement(uk_necc_formation_tr_t0, uk_necc_formation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(uk_necc_formation_tr_t5, uk_necc_formation, theater_ratio, 5, 0.08).
narrative_ontology:measurement(uk_necc_formation_tr_t10, uk_necc_formation, theater_ratio, 10, 0.10).

% Extraction over time (projected):
narrative_ontology:measurement(uk_necc_formation_ex_t0, uk_necc_formation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uk_necc_formation_ex_t5, uk_necc_formation, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(uk_necc_formation_ex_t10, uk_necc_formation, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(uk_necc_formation, enforcement_mechanism).

% Network relationships (structural influence edges)
% The formation of the NECC directly impacts and is impacted by rules
% governing financial transparency and international sanction regimes.
narrative_ontology:affects_constraint(uk_necc_formation, uk_financial_transparency_rules).
narrative_ontology:affects_constraint(uk_sanctions_regime, uk_necc_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain (beneficiary/victim + exit options -> d) accurately models the
% structural relationships between the agents and the new enforcement agency.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */