% ============================================================================
% CONSTRAINT STORY: bwb_adeg_rewesale_conditions
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% ============================================================================

:- module(constraint_bwb_adeg_rewesale_conditions, []).

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
 *   constraint_id: bwb_adeg_rewesale_conditions
 *   human_readable: BWB Conditions on Rewe's Adeg Store Divestment
 *   domain: economic
 *
 * SUMMARY:
 *   The Austrian Federal Competition Authority (BWB) imposed conditions on the
 *   transfer of 75 Adeg grocery stores from the Rewe Group to independent
 *   merchants. These conditions are designed to ensure the merchants'
 *   independence, particularly by preventing Rewe from forcing them into
 *   exclusive purchasing agreements, thereby preserving competition among
 *   suppliers and protecting smaller market participants.
 *
 * KEY AGENTS (by structural relationship):
 *   - rewe_group: Primary target (powerful/constrained) — bears extraction by having its market power and deal-making ability constrained.
 *   - independent_merchants: Primary beneficiary (organized/mobile) — benefits from enhanced autonomy and protection from coercive contracts.
 *   - food_and_beverage_suppliers: Secondary beneficiary (moderate/mobile) - benefits from a more diverse and competitive purchasing landscape.
 *   - bwb_austria: Inter-institutional architect (institutional/arbitrage) — designs and enforces the constraint to fulfill its mandate.
 *   - end_consumers: Powerless observers (powerless/trapped) - experience the market structure as an unchangeable fact.
 *   - analytical_observer: Analytical observer — sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, 0.48).
domain_priors:suppression_score(bwb_adeg_rewesale_conditions, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(bwb_adeg_rewesale_conditions, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, extractiveness, 0.48).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(bwb_adeg_rewesale_conditions, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(bwb_adeg_rewesale_conditions). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, independent_merchants).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, food_and_beverage_suppliers).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, bwb_austria). % Fulfills its mandate

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, rewe_group).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (REWE GROUP)
% Agent whose market power is constrained. Engine derives d from:
%   victim membership + constrained exit -> d approx 0.9 -> f(d) approx 1.35 -> high chi
% The high suppression and extraction makes this feel like a snare, trapping
% them in a sub-optimal deal structure.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (INDEPENDENT MERCHANTS)
% Agent who benefits from the protection. Engine derives d from:
%   beneficiary membership + mobile exit -> d approx 0.15 -> f(d) approx -0.01 -> low/negative chi
% From their view, it's a pure coordination mechanism that enables fair competition.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Recognizes both the coordination function and
% the asymmetric extraction, hence classifying it as a Tangled Rope.
% This classification serves as the ground truth for the constraint_claim.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The regulator (BWB) and the regulated (Rewe) are both powerful actors
% with different structural relationships to the constraint.

% PERSPECTIVE 4A: THE REGULATOR (BWB)
% As the architect, the BWB sees the constraint as a tool to fulfill its
% mandate of ensuring market competition. It is a pure coordination mechanism.
% Engine derives d from: beneficiary membership + arbitrage exit -> d approx 0.05 -> f(d) approx -0.12
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE END CONSUMER (MOUNTAIN)
% A single consumer in a town with one of the affected stores. They are
% powerless to change the regulatory conditions and trapped by their local
% geography. From this viewpoint, the constraint is an unchangeable feature
% of their economic landscape, appearing as a Mountain.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bwb_adeg_rewesale_conditions_tests).

test(perspectival_gap) :-
    % Verify the gap between the target (Rewe) and a beneficiary (BWB).
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypeTarget, context(agent_power(powerful), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_conditions_met) :-
    % Verify that all structural preconditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, _),
    narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, _),
    domain_priors:requires_active_enforcement(bwb_adeg_rewesale_conditions).

test(analytical_view_is_tangled_rope) :-
    narrative_ontology:constraint_claim(bwb_adeg_rewesale_conditions, tangled_rope),
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(bwb_adeg_rewesale_conditions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Set moderately high. The constraint doesn't seize assets, but it significantly extracts potential monopoly profits and market control from Rewe by severing tied purchasing agreements. This represents a substantial transfer of economic power.
 *   - Suppression Score (s=0.65): High. As the national competition authority, the BWB's decision is binding. Rewe's alternative is to cancel the entire divestment, which may be commercially untenable. This gives the BWB strong coercive power in this context.
 *   - Theater Ratio (τ=0.10): Low. This is a concrete, legally enforceable set of conditions with clear economic impact, not a performative gesture.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The target, Rewe Group, experiences the constraint as a Snare. From its viewpoint, the rules are coercive (high suppression) and extract significant value (high ε), trapping it in a less favorable transaction. The beneficiaries—independent merchants and the BWB itself—see a Rope. For them, it is a pure coordination device that corrects a market failure, levels the playing field, and enables fair competition. The disagreement is not about the facts, but about the structural position of each agent relative to the flow of costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived automatically from the declared structural relationships.
 *   - Beneficiaries: `independent_merchants`, `food_and_beverage_suppliers`, and the `bwb_austria` (which benefits by fulfilling its mandate). These agents receive a low `d`, resulting in a low or negative effective extraction (χ).
 *   - Victim: `rewe_group` is the sole victim, bearing the costs of the constraint. This assigns it a high `d`, leading to a high χ from its perspective.
 *   This mapping directly reflects the economic reality of the regulatory intervention.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This case perfectly illustrates an inter-institutional dynamic. Both the BWB (`institutional`) and Rewe (`powerful`) are significant actors. However, their relationship to the constraint is oppositional. The BWB, with `exit_options(arbitrage)` (it designed the rule), sees a Rope. Rewe, with `exit_options(constrained)` (it must accept the rule to proceed), sees a Snare. The model correctly captures that even powerful actors can be targets of extraction depending on the regulatory context.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. A simplistic analysis might label this regulation as either purely beneficial "pro-competition policy" (Rope) or purely harmful "government overreach" (Snare). The Tangled Rope designation correctly identifies that it is BOTH: it has a genuine coordination function that benefits some market actors (`constraint_beneficiary` is declared) while simultaneously imposing asymmetric costs on another (`constraint_victim` is declared). This dual nature is the hallmark of effective, but non-neutral, regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_bwb_adeg,
    'Will Rewe Group find structural workarounds (e.g., through complex wholesale incentives or loyalty programs) to re-establish de facto purchasing control over the theoretically independent merchants?',
    'Long-term audit of supplier diversity and purchasing prices for the divested stores over a 5-10 year period.',
    'If True, the constraint degrades into a Piton (high theater, low function), and the initial extraction was temporary. If False, the constraint remains an effective Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_bwb_adeg, empirical, 'Whether Rewe can structurally bypass the conditions to re-establish de facto purchasing control.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bwb_adeg_rewesale_conditions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this new constraint models its intended stability.
% A future audit could detect drift (e.g., rising theater) if enforcement wanes.
% Required because base_extractiveness > 0.46.

% Theater ratio over time (stable):
narrative_ontology:measurement(bwb_adeg_tr_t0, bwb_adeg_rewesale_conditions, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bwb_adeg_tr_t5, bwb_adeg_rewesale_conditions, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bwb_adeg_tr_t10, bwb_adeg_rewesale_conditions, theater_ratio, 10, 0.10).

% Extraction over time (stable):
narrative_ontology:measurement(bwb_adeg_ex_t0, bwb_adeg_rewesale_conditions, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bwb_adeg_ex_t5, bwb_adeg_rewesale_conditions, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bwb_adeg_ex_t10, bwb_adeg_rewesale_conditions, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic regulatory enforcement mechanism for market structure.
narrative_ontology:coordination_type(bwb_adeg_rewesale_conditions, enforcement_mechanism).

% Network relationships (structural influence edges)
% This constraint influences the general balance of power in the Austrian grocery supply chain.
narrative_ontology:affects_constraint(bwb_adeg_rewesale_conditions, austrian_grocery_supplier_leverage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and the agents' exit
% options accurately captures the structural dynamics of the situation.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */