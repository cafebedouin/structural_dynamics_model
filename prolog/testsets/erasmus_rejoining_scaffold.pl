% ============================================================================
% CONSTRAINT STORY: erasmus_rejoining_scaffold
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-07
% ============================================================================

:- module(constraint_erasmus_rejoining_scaffold, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: erasmus_rejoining_scaffold
 *   human_readable: UK's potential re-entry into the EU Erasmus+ student exchange program
 *   domain: political
 *
 * SUMMARY:
 *   Following its exit from the EU and the Erasmus+ program, the UK is considering
 *   rejoining. This constraint models the structure of the re-entry agreement itself.
 *   It is framed as a temporary support (Scaffold) to rebuild a coordination
 *   mechanism (student and academic mobility) that was dismantled, with clear
 *   beneficiaries and a quantifiable cost borne by the state.
 *
 * KEY AGENTS (by structural relationship):
 *   - UK Students & Universities: Primary beneficiary (organized/constrained) — regain access to EU mobility.
 *   - UK Taxpayers & Fiscal Conservatives: Primary cost-bearer (powerless/trapped) — bear the financial contribution to the EU budget.
 *   - UK Government: Architect & Institutional Beneficiary (institutional/constrained) — gains soft power and satisfies a key sector, but constrained by political pressure.
 *   - EU Commission: Institutional Beneficiary (institutional/arbitrage) — regains a major partner, strengthening the program, with many other partners as alternatives.
 *   - Analytical Observer: Sees the full structure of costs, benefits, and temporary nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(erasmus_rejoining_scaffold, 0.28).
domain_priors:suppression_score(erasmus_rejoining_scaffold, 0.20).   % Structural property (raw, unscaled). The UK's 'Turing Scheme' exists as a (weaker) alternative.
domain_priors:theater_ratio(erasmus_rejoining_scaffold, 0.25).       % Political signaling is present, but the core function is real.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, theater_ratio, 0.25).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(erasmus_rejoining_scaffold, scaffold).
narrative_ontology:human_readable(erasmus_rejoining_scaffold, "UK's potential re-entry into the EU Erasmus+ student exchange program").

% --- Binary flags ---
narrative_ontology:has_sunset_clause(erasmus_rejoining_scaffold).      % Mandatory for Scaffold. A future government could withdraw.
domain_priors:requires_active_enforcement(erasmus_rejoining_scaffold). % Required for Tangled Rope. The agreement is legally binding and requires financial transfers.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_students_and_universities).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, eu_students_and_universities).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(erasmus_rejoining_scaffold, uk_taxpayers_and_fiscal_conservatives).
%
% Gate requirements:
%   Scaffold: beneficiary + has_sunset_clause (both present).
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all present).

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

% PERSPECTIVE 1: THE TAXPAYER (TANGLED ROPE)
% Bears the cost of the program without direct participation. From this view,
% it's a mix of a genuine public good (coordination) and an extractive
% transfer payment (extraction).
% victim + trapped -> d≈0.95, f(d)≈1.42 -> χ ≈ 0.28 * 1.42 * 1.1 = 0.437. This χ is in Tangled Rope range.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: UK STUDENTS & UNIVERSITIES (ROPE)
% As direct beneficiaries, they see a pure coordination mechanism that opens
% opportunities. The costs are externalized to the government/taxpayer.
% beneficiary + constrained -> d≈0.25, f(d)≈0.15 -> χ ≈ 0.28 * 0.15 * 1.1 = 0.046. This χ is well within Rope range.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Sees the full picture: a coordination function with a cost, but critically,
% a temporary and reversible structure designed to rebuild capacity. The
% 'has_sunset_clause' flag is key to this classification.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The UK Government and EU Commission are both institutional actors but have
% different structural positions and exit options, leading to different views.

% Perspective 4A: UK Government (Architect)
% Views the program as a Scaffold: a tool to achieve policy goals (soft power,
% research strength) that is politically necessary but also reversible.
% Exit is 'constrained' by domestic political pressure from the academic sector.
% beneficiary + constrained -> d≈0.25, f(d)≈0.15.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Perspective 4B: EU Commission (Partner)
% Views the program as a stable Rope. The re-entry of a large partner
% strengthens the overall coordination network. Their exit option is 'arbitrage'
% as the program functions well with or without the UK.
% beneficiary + arbitrage -> d≈0.05, f(d)≈-0.12.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erasmus_rejoining_scaffold_tests).

test(perspectival_gap_taxpayer_vs_student) :-
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypeBeneficiary, context(agent_power(organized), _, exit_options(constrained), _)),
    assertion(TypeTarget == tangled_rope),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(inter_institutional_gap_uk_vs_eu) :-
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypeUK, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, TypeEU, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeUK == scaffold),
    assertion(TypeEU == rope),
    TypeUK \= TypeEU.

test(scaffold_properties_adherence) :-
    narrative_ontology:has_sunset_clause(erasmus_rejoining_scaffold),
    domain_priors:theater_ratio(erasmus_rejoining_scaffold, TR), TR =< 0.70.

test(analytical_claim_matches) :-
    narrative_ontology:constraint_claim(erasmus_rejoining_scaffold, Claim),
    constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, Claim, context(agent_power(analytical), _, _, _)).

:- end_tests(erasmus_rejoining_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.28): Represents the significant financial contribution the UK must make to the EU budget to participate. It's a real cost, but not high enough to be purely extractive, as it funds a genuine coordination function.
 *   - Suppression (0.20): Low, because the UK created its own alternative, the Turing Scheme. While widely seen as inferior, it means non-participation in Erasmus+ does not completely foreclose international study options.
 *   - Classification (Scaffold): The key structural feature is its reversibility. A future government can withdraw, making it a temporary support structure for rebuilding academic ties, not a permanent commitment. This is captured by the `has_sunset_clause/1` flag, which is mandatory for the Scaffold classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. For students and universities (beneficiaries), it's a pure Rope that provides immense value while the costs are paid by someone else. For taxpayers (victims), it's a Tangled Rope where their funds are extracted for a public good they may not directly benefit from. This highlights the core tension in publicly funded programs.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `uk_students_and_universities` and the `uk_government` are declared. This signals a clear coordination function that benefits key domestic groups.
 *   - Victim: `uk_taxpayers_and_fiscal_conservatives` captures the group bearing the direct financial burden. The engine uses this to derive a high directionality `d` for the powerless/trapped perspective, correctly identifying the extractive component felt by that group.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the UK Government and the EU Commission. Both are `institutional` beneficiaries. However, the UK's `exit_options(constrained)` reflects the strong domestic pressure to rejoin, limiting its negotiating freedom. The EU's `exit_options(arbitrage)` reflects its much stronger position; the program thrives with or without the UK. This difference in exit optionality correctly generates different classifications (Scaffold vs. Rope) from their respective viewpoints, capturing the asymmetry of the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Scaffold correctly identifies its purpose as a transitional, supportive measure. A simpler analysis might label it a Rope (ignoring the costs and political fragility) or a Tangled Rope (over-focusing on the financial extraction). The Scaffold classification, contingent on the sunset clause, captures the nuanced reality: it's a coordination tool being rebuilt, not a permanent fixture, and its value is weighed against its cost and political viability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_erasmus_rejoining,
    'Will the intangible "soft power" and long-term economic benefits from student mobility demonstrably outweigh the direct UK budgetary contributions?',
    'Longitudinal studies over 10+ years, tracking career paths of participants, international research collaboration metrics, and UK standing in global academic/innovation rankings.',
    'If benefits are demonstrably high, the Scaffold is a success and might transition to a stable Rope. If benefits are low or unquantifiable, it will be perpetually seen as an extractive Tangled Rope by fiscal conservatives, risking future withdrawal.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(erasmus_rejoining_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is not highly extractive (ε < 0.46), so temporal data is not
% strictly required. However, modeling its stabilization is useful.
% T=0 is the point of agreement, T=10 is a mature, operational state.

% Theater ratio decreases as political negotiation gives way to routine administration.
narrative_ontology:measurement(erasmus_tr_t0, erasmus_rejoining_scaffold, theater_ratio, 0, 0.40).
narrative_ontology:measurement(erasmus_tr_t5, erasmus_rejoining_scaffold, theater_ratio, 5, 0.30).
narrative_ontology:measurement(erasmus_tr_t10, erasmus_rejoining_scaffold, theater_ratio, 10, 0.25).

% Extraction stabilizes as contributions become regularized.
narrative_ontology:measurement(erasmus_ex_t0, erasmus_rejoining_scaffold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(erasmus_ex_t5, erasmus_rejoining_scaffold, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(erasmus_ex_t10, erasmus_rejoining_scaffold, base_extractiveness, 10, 0.28).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(erasmus_rejoining_scaffold, resource_allocation).

% Network relationships: This is a direct consequence of the UK's exit from the EU.
narrative_ontology:affects_constraint(brexit_withdrawal_agreement, erasmus_rejoining_scaffold).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, uk_eu_trade_agreement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim groups
% and exit options accurately models the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */