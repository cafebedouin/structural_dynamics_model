% ============================================================================
% CONSTRAINT STORY: indo_german_defense_pact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_german_defense_pact, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indo_german_defense_pact
 *   human_readable: India-Germany Defense Industrial Partnership
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The India-Germany Defense Industrial Partnership represents a
 *   geopolitical constraint combining genuine coordination interests with
 *   structural extraction mechanisms. Both nations benefit from technology
 *   cooperation and industrial capacity development, but the partnership
 *   operates within legacy export control architectures and asymmetric power
 *   relationships that favor established German defense industrial bases.
 *   India seeks technology sovereignty and strategic autonomy; Germany seeks
 *   market access and industrial revenue. The constraint exhibits hybrid
 *   characteristics: coordination function (solving mutual strategic
 *   interests) combined with extraction mechanism (technology access
 *   asymmetry, licensing dependencies, supply chain lock-in). The theater
 *   ratio (0.62) reflects that significant portions of the compliance and
 *   certification frameworks are performative institutional maintenance
 *   rather than functional security mechanisms. Extractiveness has increased
 *   over time as practical co-production reveals hidden dependency
 *   relationships not apparent in initial bilateral rhetoric.
 *
 * KEY AGENTS:
 *   - German Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — gains market access, licensing fees, co-production contracts without major domestic reorganization
 *   - Indian Strategic Autonomy Coalition: Primary victim and secondary beneficiary (organized/constrained) — seeks technology and capability development while constrained by partnership dependency and export controls
 *   - Indian Procurement Officers: Trapped agent (powerless/trapped) — operationally locked into German-certified supply chains and technical oversight requirements
 *   - Indian Domestic Defense Innovation Initiative: Exit pathway agent (organized/mobile) — Make in India programs create sunset logic for co-production dependency
 *   - Export Control Architecture: Institutional inertia (institutional/constrained) — Cold War certification frameworks applied to new partnerships through ritual rather than functional necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as hybrid coordination-extraction operating within legacy geopolitical structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_german_defense_pact, 0.52).
domain_priors:suppression_score(indo_german_defense_pact, 0.58).
domain_priors:theater_ratio(indo_german_defense_pact, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_german_defense_pact, extractiveness, 0.52).
narrative_ontology:constraint_metric(indo_german_defense_pact, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(indo_german_defense_pact, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_german_defense_pact, tangled_rope).
narrative_ontology:human_readable(indo_german_defense_pact, "India-Germany Defense Industrial Partnership").
narrative_ontology:topic_domain(indo_german_defense_pact, "geopolitical/economic").

domain_priors:requires_active_enforcement(indo_german_defense_pact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, german_defense_industrial_base).
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, indian_strategic_autonomy_aspirations).
narrative_ontology:constraint_victim(indo_german_defense_pact, indian_technology_sovereignty).
narrative_ontology:constraint_victim(indo_german_defense_pact, german_export_control_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN PROCUREMENT OFFICER (SNARE) — Trapped within dependency relationships. Co-production agreements require German technical oversight, certification, and component supply. Exit options are minimal: domestic production timelines are longer, and strategic urgency creates pressure to accept unfavorable terms. Extraction flows from India to Germany through locked-in supply relationships and technology access asymmetries.
constraint_indexing:constraint_classification(indo_german_defense_pact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GERMAN DEFENSE INDUSTRIAL BASE (ROPE) — Institutional beneficiary with arbitrage options. Partnership expands market access to Indian defense procurement without requiring major domestic reorganization. German firms gain co-production contracts, technology licensing fees, and supply chain positioning. From German industry perspective, the constraint operates as coordination mechanism — partners share development costs and gain market expansion.
constraint_indexing:constraint_classification(indo_german_defense_pact, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIAN STRATEGIC AUTONOMY COALITION (TANGLED ROPE) — Organized Indian defense establishment sees partnership as both essential coordination (technology acquisition, industrial development) and extractive constraint (dependency on German certification, export controls, component supply). Strategic autonomy requires technology transfer, but partnership structure limits it. Constrained exit: cannot simply walk away from strategic partnerships in multipolar environment.
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: DOMESTIC DEFENSE INNOVATION (SCAFFOLD) — Emerging indigenous development programs (Make in India defense initiatives, DRDO autonomy) create exit pathways. Co-production with Germany is viewed as transitional: build capability and capacity through partnership, then migrate to independent production. Partnership has implicit sunset as Indian technical capacity matures. Low experienced extraction because structured transition exists.
constraint_indexing:constraint_classification(indo_german_defense_pact, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EXPORT CONTROL ARCHITECTURE (PITON) — German export controls (ITAR-adjacent restrictions, technology classification schemes) persist largely through institutional inertia. Originally designed for NATO alliance management, now applied to India partnership through ritualized compliance frameworks. Functional purpose (preventing proliferation) has degraded relative to theater (certification procedures, documentation requirements). High theater ratio reflects that much of the process is performative institutional maintenance rather than effective security mechanism.
constraint_indexing:constraint_classification(indo_german_defense_pact, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, the partnership represents coordination (technology cooperation toward shared strategic stability) and extraction (structural dependency relationships favoring established defense industrial powers). Neither pure coordination nor pure extraction. The constraint combines genuine coordination function with asymmetric power flows.
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_german_defense_pact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_german_defense_pact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_german_defense_pact, TR),
    TR >= 0.70.

:- end_tests(indo_german_defense_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The partnership combines legitimate technology access benefits with structural lock-in through certification dependencies, licensing fee structures, and component supply asymmetries. Indian firms cannot easily substitute German components or bypass German technical oversight without losing partnership access. The value reflects both genuine coordination value (~0.30) and extraction overhead (~0.22) embedded in the partnership structure. Suppression (0.58): Moderate-high. Barriers include export control frameworks limiting technology depth, German IP protections restricting independent modifications, supply chain dependencies creating switching costs, and geopolitical pressure to maintain partnership despite unfavorable terms. However, suppression is not total — Indian domestic programs create exit pathways and reduce suppression over time. Theater ratio (0.62): Moderate-high. Significant portions of the compliance framework are performative: export control certification procedures reflect Cold War institutional patterns rather than contemporary security logic; documentation requirements consume resources without proportional security benefit; German oversight maintains institutional control beyond functional necessity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_threshold,
    'Does co-production enable genuine Indian technology sovereignty or create permanent dependency on German technical oversight and component supply?',
    'Longitudinal analysis of Indian independent production capacity; tracking of domestic design capability growth; assessment of German component substitutability over 10-year horizon',
    'If transfer is genuine: Scaffold perspective confirmed, constraint has real sunset. If transfer is constrained by design: Snare perspective dominates, dependency is structural not temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_threshold, empirical, 'Whether technology transfer enables Indian sovereignty or creates permanent dependency').

omega_variable(
    export_control_enforcement_level,
    'Are German export controls applied as functional security mechanisms or as institutional theater maintaining legacy architectures?',
    'Analysis of denied technologies vs approved transfers; comparison of Indian vs NATO partner treatment; assessment of security rationale vs market protection rationale in control decisions',
    'If functional: controls represent real coordinated security framework. If theatrical: controls are Piton inertia constraining legitimate partnership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_control_enforcement_level, empirical, 'Whether export controls function as security or as institutional theater').

omega_variable(
    strategic_multipolarity_lock,
    'Does geopolitical multipolarity lock India into accepting unfavorable partnership terms because alternatives are limited or unreliable?',
    'Assessment of Indian options with Russia, Israel, Japan, or indigenous development; analysis of cost-benefit vs alternatives; geopolitical risk evaluation of each alternative',
    'If lock-in is structural: Snare extraction from trapped agent is dominant read. If alternatives exist: Tangled Rope classification more accurate, agency is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_multipolarity_lock, empirical, 'Whether geopolitical constraints lock India into accepting unfavorable terms').

omega_variable(
    symmetry_of_benefit,
    'Does partnership provide symmetrical strategic benefit (German market access, Indian capability development) or asymmetrical benefit (German rent extraction, Indian dependency)?',
    'Comparative analysis of contract terms, licensing fee structures, IP ownership, export margin capture; long-term cost-benefit accounting for both parties; strategic autonomy gains for India vs market access for Germany',
    'If asymmetrical: extraction interpretation (Snare/Tangled Rope lower perspectives) dominates. If symmetrical: coordination interpretation (Rope/Scaffold) more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_of_benefit, empirical, 'Whether partnership benefits are symmetrical or asymmetrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_german_defense_pact, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igdp_tr_t0, indo_german_defense_pact, theater_ratio, 0, 0.45).
narrative_ontology:measurement(igdp_tr_t7, indo_german_defense_pact, theater_ratio, 7, 0.55).
narrative_ontology:measurement(igdp_tr_t15, indo_german_defense_pact, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(igdp_be_t0, indo_german_defense_pact, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(igdp_be_t7, indo_german_defense_pact, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(igdp_be_t15, indo_german_defense_pact, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_german_defense_pact, resource_allocation).
narrative_ontology:affects_constraint(indo_german_defense_pact, indian_domestic_defense_autonomy).
narrative_ontology:affects_constraint(indo_german_defense_pact, european_export_control_regime).
narrative_ontology:affects_constraint(indo_german_defense_pact, us_strategic_pivot_to_india).

% DUAL FORMULATION NOTE:
% The partnership decomposes into coordination value (technology transfer for strategic stability) and extraction mechanism (dependency lock-in). Initial partnership descriptions emphasize coordination; operational implementation reveals extraction. These are distinct constraint families: the idealized partnership (coordination focus) vs the operational partnership (extraction dominant). Theater ratio and extractiveness trends show migration from coordination rhetoric to extraction practice over the 15-year interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_german_defense_pact, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
