% ============================================================================
% CONSTRAINT STORY: asce_7_22_seismic_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asce_7_22_seismic_design, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: asce_7_22_seismic_design
 *   human_readable: ASCE 7-22 Seismic Design Requirements
 *   domain: technological/legal
 *
 * SUMMARY:
 *   ASCE 7-22 is a building code standard from the American Society of Civil
 *   Engineers that dictates the minimum loads, including seismic forces, that
 *   structures in the United States must be designed to withstand. Its
 *   primary purpose is to ensure public safety by preventing structural
 *   collapse during earthquakes. While it serves a critical coordination
 *   function for engineers, architects, and regulators, it also imposes
 *   significant financial costs on construction, which are asymmetrically
 *   distributed throughout society. This dual nature makes it a classic
 *   example of a Tangled Rope.
 *
 * KEY AGENTS:
 *   - General Public Safety: The abstract primary beneficiary, whose physical safety is the goal of the standard.
 *   - ASCE/SEI Standard Setters: Institutional beneficiary (institutional/arbitrage) — controls the standard, gaining prestige and professional authority.
 *   - Building Developers/Owners: Primary cost-bearers (powerful/mobile) — must fund the expensive compliance measures.
 *   - Low-Income Communities: Primary victims (powerless/trapped) — bear passed-down costs via higher housing prices and are often concentrated in older, non-compliant buildings.
 *   - Structural Engineers: Users of the standard (organized/constrained) — rely on it as a coordination tool and liability shield.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asce_7_22_seismic_design, 0.35).
domain_priors:suppression_score(asce_7_22_seismic_design, 0.85).
domain_priors:theater_ratio(asce_7_22_seismic_design, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asce_7_22_seismic_design, extractiveness, 0.35).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asce_7_22_seismic_design, tangled_rope).
narrative_ontology:human_readable(asce_7_22_seismic_design, "ASCE 7-22 Seismic Design Requirements").
narrative_ontology:topic_domain(asce_7_22_seismic_design, "technological/legal").

domain_priors:requires_active_enforcement(asce_7_22_seismic_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, general_public_safety).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, asce_sei_standard_setters).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, structural_engineering_profession).
narrative_ontology:constraint_victim(asce_7_22_seismic_design, building_developers_and_owners).
narrative_ontology:constraint_victim(asce_7_22_seismic_design, low_income_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RESIDENT (SNARE) — Experiences the downstream effects as pure extraction. Higher construction costs for new buildings increase overall housing prices, while older, non-compliant housing in their communities remains un-retrofitted, leaving them bearing both the costs and the risks. They are trapped by economic circumstance. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.40.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BUILDING DEVELOPER (TANGLED ROPE) — Experiences the standard as a significant cost driver that constrains projects, but also as a coordination mechanism that standardizes liability and ensures a level playing field. They have mobility to pass costs to consumers or choose different projects. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.36.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: STRUCTURAL ENGINEER (ROPE) — For the practicing engineer, the standard is a pure coordination tool. It provides a common language, a defensible methodology, and a liability shield. While it constrains design freedom, its primary function from this perspective is to solve a complex coordination problem. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.23.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASCE STANDARD-SETTING BODY (ROPE) — As the author of the constraint, the institution sees it as a pure public good and coordination mechanism. They benefit from the institutional prestige and control over the profession, experiencing no extraction. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04. Negative effective extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a necessary coordination function for public safety that is coupled with high compliance costs borne asymmetrically by developers and low-income groups, all enforced by law (high suppression). The combination of a genuine coordination function and asymmetric extraction defines a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asce_7_22_seismic_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(asce_7_22_seismic_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.35) is moderate. The value represents the mandatory 'over-design' cost imposed for safety, which, while providing a public good, is extracted from project budgets. It is not pure rent-seeking, but a real cost with asymmetric beneficiaries. Suppression (0.85) is high because the standard is legally mandated via adoption into building codes; there is no legal way to build a major structure in the US without complying. Theater Ratio (0.10) is very low, as the standard is highly functional, based on extensive seismic science and engineering practice, and directly impacts structural integrity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The standard's authors (ASCE) and users (engineers) perceive a pure coordination mechanism (Rope) that solves a complex safety and liability problem. However, those who bear the costs see extraction. For a developer with agency, it's a manageable but costly regulation (Tangled Rope). For a low-income resident trapped in a high-cost housing market and potentially unsafe older building, the system is purely extractive (Snare), raising costs without delivering proportional benefits to them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the abstract concept of public safety and the institutions (ASCE) that create and maintain the standard. Victims are those who pay the direct costs (developers) and the indirect, passed-down costs (low-income communities). The structural engineer is modeled as a neutral user who sees it as a coordination tool. This distribution of costs and benefits, combined with varying exit options (arbitrage for ASCE, mobile for developers, trapped for residents), drives the wide perspectival gap from Rope to Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy where a public safety standard might be uncritically labeled a 'Rope'. The framework demonstrates that even a well-intentioned, scientifically grounded constraint can function as a Snare from the perspective of the powerless and trapped. It correctly identifies the structure as a Tangled Rope from an analytical view, acknowledging both its vital coordination role and its extractive consequences, preventing a simplistic 'good' or 'bad' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asce_7_22_seismic_design, 1962, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asce_7_22_seismic_design, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
