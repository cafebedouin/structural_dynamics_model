% ============================================================================
% CONSTRAINT STORY: astm_d638_tensile_testing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_astm_d638_tensile_testing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: astm_d638_tensile_testing
 *   human_readable: ASTM D638 Tensile Property Standard for Plastics
 *   domain: technological/materials_science
 *
 * SUMMARY:
 *   ASTM D638 is the dominant North American standard for determining the
 *   tensile properties of plastics. It provides a common language and
 *   methodology, enabling engineers to compare materials and design reliable
 *   products. This function is a classic coordination good (Rope). However,
 *   the standard is not without costs. Compliance requires expensive testing
 *   equipment, purchase of the standard itself, and often third-party lab
 *   certification. This creates a financial barrier to entry for smaller
 *   firms and generates revenue for the standards body and equipment
 *   manufacturers, introducing an extractive element. Furthermore, its
 *   incompatibility with the globally prevalent ISO 527 standard creates
 *   friction in international supply chains.
 *
 * KEY AGENTS:
 *   - ASTM International: Primary beneficiary (institutional/arbitrage) - Develops, maintains, and sells the standard.
 *   - Testing Equipment Manufacturers: Primary beneficiary (institutional/arbitrage) - Sell the specialized machines required for compliance.
 *   - Small-Scale Producers: Primary victim (moderate/constrained) - Must bear the high costs of compliance to access major markets.
 *   - Design Engineers: Secondary beneficiary (moderate/mobile) - Use the standard as a coordination tool to simplify their work.
 *   - Global Interoperability: Abstract victim (powerless/trapped) - The ideal of a single global standard is undermined by regional fragmentation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(astm_d638_tensile_testing, 0.35).
domain_priors:suppression_score(astm_d638_tensile_testing, 0.5).
domain_priors:theater_ratio(astm_d638_tensile_testing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(astm_d638_tensile_testing, extractiveness, 0.35).
narrative_ontology:constraint_metric(astm_d638_tensile_testing, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(astm_d638_tensile_testing, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(astm_d638_tensile_testing, tangled_rope).
narrative_ontology:human_readable(astm_d638_tensile_testing, "ASTM D638 Tensile Property Standard for Plastics").
narrative_ontology:topic_domain(astm_d638_tensile_testing, "technological/materials_science").

domain_priors:requires_active_enforcement(astm_d638_tensile_testing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, astm_international).
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, testing_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, large_material_producers).
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, certified_testing_labs).
narrative_ontology:constraint_victim(astm_d638_tensile_testing, small_scale_producers).
narrative_ontology:constraint_victim(astm_d638_tensile_testing, engineers_in_iso_regions).
narrative_ontology:constraint_victim(astm_d638_tensile_testing, academic_research_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STANDARDS BODY (ROPE) — From the perspective of the creating institution, the standard is a pure coordination mechanism. It solves the problem of inconsistent material data, enabling global trade and reliable engineering. They benefit from its adoption but see this as a fair reward for providing a critical public good. d≈0.05, f(d)≈-0.12, χ is negative.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: DESIGN ENGINEER (ROPE) — For a practicing engineer, the standard is a tool that simplifies material selection and ensures product reliability. It's a pure coordination good that reduces uncertainty. The costs are borne by their employer and are seen as a normal cost of doing business. d≈0.50, f(d)≈0.65, χ≈0.18.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL-SCALE PRODUCER (TANGLED ROPE) — This agent experiences both the coordination benefits (access to markets) and the extractive costs (equipment, certification, membership fees). To sell to major clients, compliance is non-negotiable, making their exit options constrained. The costs are a significant barrier to entry. d≈0.80, f(d)≈1.25, χ≈0.35.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL INTEROPERABILITY (SNARE) — The abstract ideal of a single, frictionless global standard is a victim. The existence of competing standards (ASTM vs. ISO) creates fragmentation. From this viewpoint, any dominant regional standard is a snare that traps its ecosystem and prevents seamless global coordination. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.60.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes the essential coordination function while also accounting for the extractive financial flows to the standards body and equipment makers, and the barriers to entry it creates. The classification is Tangled Rope, acknowledging both functions. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(astm_d638_tensile_testing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(astm_d638_tensile_testing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.35): Moderate. Represents the significant but not predatory costs of equipment, certification, and the standard documentation itself. This creates a real financial barrier and revenue stream. Suppression (0.50): Moderate. In the North American market, it is extremely difficult to operate without adhering to this standard. While alternatives like ISO 527 exist, they are not interchangeable, effectively suppressing their use within the ASTM-dominated ecosystem. Theater Ratio (0.20): Low. The test is highly functional and provides valuable data. The small theater component acknowledges the well-known gap between idealized lab conditions and real-world material performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. For the standards body and the end-user engineer, the constraint is a pure Rope, a tool for coordination. For the small producer who must pay to play, it is a Tangled Rope, a necessary system that both enables and extracts. For the abstract ideal of a perfectly unified global system, the standard's regional dominance makes it a Snare, fragmenting the world and trapping participants in a specific ecosystem. The analytical view must be Tangled Rope to capture both the undeniable coordination benefit and the structural extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like ASTM International and equipment manufacturers have arbitrage exit options and a structural position that benefits from the standard's widespread adoption, leading to a low 'd' value and a Rope classification. Victims like small-scale producers are constrained by market requirements, forcing them to participate in the system and bear its costs, leading to a higher 'd' value. The analytical observer's 'd' is derived from the canonical value for the 'analytical' power atom, reflecting a position that sees the costs borne by others.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates the necessity of the Tangled Rope classification. Labeling ASTM D638 as a pure Rope would ignore the very real financial barriers and extractive flows it creates. Labeling it a Snare would be an overstatement, as it provides a genuine and indispensable coordination function. The Tangled Rope classification correctly identifies that the constraint has both a legitimate coordination purpose and an asymmetric distribution of costs and benefits that is maintained through active enforcement (customer requirements, quality control systems).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(astm_d638_tensile_testing, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(astm_d638_tensile_testing, information_standard).
narrative_ontology:affects_constraint(astm_d638_tensile_testing, automotive_component_sourcing).
narrative_ontology:affects_constraint(astm_d638_tensile_testing, medical_device_manufacturing_fda).
narrative_ontology:affects_constraint(astm_d638_tensile_testing, aerospace_materials_certification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
