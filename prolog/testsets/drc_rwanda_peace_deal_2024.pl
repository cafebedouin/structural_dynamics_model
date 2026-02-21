% ============================================================================
% CONSTRAINT STORY: drc_rwanda_peace_deal_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_drc_rwanda_peace_deal_2024, []).

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
 *   constraint_id: drc_rwanda_peace_deal_2024
 *   human_readable: US-Brokered DRC-Rwanda De-escalation Framework
 *   domain: geopolitical
 *
 * SUMMARY:
 *   A diplomatic framework brokered by the United States intended to de-escalate
 *   conflict in the eastern Democratic Republic of Congo (DRC). The deal
 *   pressures the DRC to cease support for FDLR rebels in exchange for Rwanda
 *   ceasing its support for M23 rebels. However, critics and the DRC government
 *   allege the framework is asymmetric, failing to hold Rwanda accountable while
 *   placing the primary burden of compliance on the DRC, thus failing to
' *   address the root cause of the immediate violence.
 *
 * KEY AGENTS (by structural relationship):
 *   - Civilians in Eastern DRC: Primary target (powerless/trapped) — bear the cost of continued violence.
 *   - DRC Government: Secondary target (powerful/constrained) — bears political extraction, forced to make concessions with limited recourse.
 *   - Rwanda Government: Primary beneficiary (powerful/mobile) — gains diplomatic legitimacy and leverage without significant reciprocal action.
 *   - US State Department: Architect/Beneficiary (institutional/arbitrage) — benefits from the appearance of diplomatic action and regional management.
 *   - Analytical Observer: Sees the full structure, including the gap between stated intent and actual outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drc_rwanda_peace_deal_2024, 0.55). % Political concessions, diplomatic capital, and indirect economic costs extracted from DRC.
domain_priors:suppression_score(drc_rwanda_peace_deal_2024, 0.65).   % High; rejecting a US-backed deal carries significant diplomatic risk, suppressing alternative frameworks.
domain_priors:theater_ratio(drc_rwanda_peace_deal_2024, 0.40).       % Significant performance of peacemaking without corresponding de-escalation on the ground.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, extractiveness, 0.55).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(drc_rwanda_peace_deal_2024, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(drc_rwanda_peace_deal_2024). % Required for Tangled Rope. The deal relies on US diplomatic pressure.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, rwanda_government).
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, us_state_department).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, civilians_eastern_drc).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, drc_government).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (CIVILIANS IN EASTERN DRC)
% The deal fails to protect them, trapping them in conflict. They experience
% the framework's failure as a pure cost with no coordination benefit.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → high χ.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US STATE DEPARTMENT)
% Views the deal as a low-cost coordination tool for regional management.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → low/negative χ.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the stated coordination function and the actual asymmetric extraction.
% The high base extraction (ε=0.55) and suppression (0.65) lead to a
% Tangled Rope classification, capturing the duality of the constraint.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The two primary state actors experience the same deal very differently due
% to their structural positions and exit options.

% PERSPECTIVE 4A: DRC GOVERNMENT (CONSTRAINED VICTIM)
% As a sovereign state, it is 'powerful' but has 'constrained' exit options
% due to diplomatic pressure. It experiences the deal as highly extractive.
% Engine derives d from: victim membership + powerful + constrained exit -> high d.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: RWANDA GOVERNMENT (MOBILE BENEFICIARY)
% As a sovereign state, it is 'powerful' and has 'mobile' exit options, able to
% leverage the deal for diplomatic gain while facing few consequences for non-compliance.
% Engine derives d from: beneficiary + powerful + mobile exit -> low d.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drc_rwanda_peace_deal_2024_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify gap between the civilian victims and the institutional beneficiary.
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the deal is classified differently by the two primary state actors.
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, SnareType, context(agent_power(powerful), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, RopeType, context(agent_power(powerful), _, exit_options(mobile), _)),
    SnareType = snare,
    RopeType = rope.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, _),
    narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, _),
    domain_priors:requires_active_enforcement(drc_rwanda_peace_deal_2024).

:- end_tests(drc_rwanda_peace_deal_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This score reflects the significant political and diplomatic capital extracted from the DRC. It is forced to accept a narrative that equates its alleged support for the FDLR with Rwanda's active, documented support for the M23, a false equivalence that undermines the DRC's sovereignty and strategic position.
 *   - Suppression (0.65): The high suppression score represents the lack of viable alternatives for the DRC. Rejecting a framework endorsed by the United States would risk diplomatic isolation and the loss of a powerful ally, effectively coercing compliance.
 *   - The combination of a coordination function (the stated goal of de-escalation) and high asymmetric extraction, backed by enforcement, is the classic signature of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the US State Department (institutional/arbitrage), the deal is a Rope—a successful diplomatic instrument for managing a crisis at low cost, regardless of its efficacy on the ground. For the DRC Government and civilians (powerful/constrained and powerless/trapped), it is a Snare. It extracts concessions and perpetuates violence while being dressed in the language of peace. The deal's structure benefits the architect and one party at the direct expense of the other.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `rwanda_government` benefits by having its security narrative legitimized internationally and by deflecting scrutiny from its role in sponsoring the M23. The `us_state_department` benefits by creating the appearance of diplomatic progress. Their mobile/arbitrage exit options give them low directionality scores (d).
 *   - Victims: The `drc_government` is the primary victim of the extraction, forced into a strategically disadvantageous position. The `civilians_eastern_drc` are the ultimate victims, bearing the physical costs of the deal's failure to stop the violence. Their constrained/trapped exit options give them high directionality scores (d), leading to high effective extraction (χ).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a prime example of an inter-institutional constraint. The DRC and Rwanda are both 'powerful' state actors, but they experience the deal entirely differently. The DRC's `exit_options(constrained)` reflects its dependency on Western allies and the high cost of defiance. Rwanda's `exit_options(mobile)` reflects its greater leverage and ability to operate with fewer consequences. The directionality engine correctly interprets this asymmetry, classifying the deal as a Snare for the constrained party and a Rope for the mobile one, even though both are institutionally 'powerful'.
 *
 * MANDATROPHY ANALYSIS:
 *   This model correctly avoids two potential errors. First, it doesn't mistake the deal for a pure Rope, which would ignore the severe, one-sided extraction experienced by the DRC. Second, it doesn't classify it as a pure Snare from an analytical view, which would miss the genuine (if failed) coordination function that gives the constraint its diplomatic cover. The Tangled Rope classification captures the essential duality: it is a tool of coordination that has been co-opted for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_drc_rwanda_peace_deal_2024,
    'Is the deal''s asymmetry a result of flawed intelligence/diplomacy (a failed Rope) or a deliberate strategic choice by the US to favor a regional ally (a purposeful Tangled Rope)?',
    'Declassification of State Department cables and intelligence assessments regarding Rwandan support for M23 prior to the deal''s formation.',
    'If flawed, it suggests incompetence and a potential for reform. If deliberate, it reveals the deal is a tool of realpolitik, functioning exactly as intended from the US perspective.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(drc_rwanda_peace_deal_2024, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint's extractive nature became more apparent over time as its
% failure to produce peace became clear, while the diplomatic pressure remained.
% The initial state reflects higher hopes for coordination.
% base_extractiveness > 0.46, so this section is required.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(drc_deal_tr_t0, drc_rwanda_peace_deal_2024, theater_ratio, 0, 0.20).
narrative_ontology:measurement(drc_deal_tr_t5, drc_rwanda_peace_deal_2024, theater_ratio, 5, 0.35).
narrative_ontology:measurement(drc_deal_tr_t10, drc_rwanda_peace_deal_2024, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(drc_deal_ex_t0, drc_rwanda_peace_deal_2024, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(drc_deal_ex_t5, drc_rwanda_peace_deal_2024, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(drc_deal_ex_t10, drc_rwanda_peace_deal_2024, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(drc_rwanda_peace_deal_2024, enforcement_mechanism).

% Network relationships: This peace deal does not exist in a vacuum. It is
% structurally influenced by the global demand for minerals concentrated in the DRC.
narrative_ontology:affects_constraint(global_competition_for_critical_minerals, drc_rwanda_peace_deal_2024).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the beneficiary/victim declarations and the distinct exit options
% for the institutional actors (constrained vs. mobile) accurately captures
% the structural dynamics and produces the correct directionality (d) values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */