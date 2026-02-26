% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_critical_mineral_chokepoint, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: china_critical_mineral_chokepoint
 *   human_readable: China's Strategic Chokepoint in Critical Mineral Processing
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   China has established a strategic dominance over the mid-stream
 *   processing of critical minerals essential for high-tech and green energy
 *   applications, such as rare earth elements, gallium, and germanium. This
 *   is not a monopoly on raw ore, but on the complex, capital-intensive, and
 *   often polluting refining stage. This control functions as a global
 *   chokepoint, creating a structural dependency for nearly every other
 *   industrialized nation.
 *
 * KEY AGENTS:
 *   - Chinese State and SOEs: Primary beneficiary (institutional/arbitrage) - Wields the chokepoint for economic and geopolitical gain.
 *   - Non-Chinese High-Tech Manufacturers: Primary victim (powerless/trapped) - Dependent on a potentially unreliable supply chain controlled by a strategic rival.
 *   - Western Governments (US/EU): Organized victim (organized/constrained) - Views the dependency as a national security threat and is actively trying to build alternatives.
 *   - Chinese Downstream Manufacturers: Secondary beneficiary (powerful/mobile) - Benefits from a secure and subsidized domestic supply chain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.65).
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.8).
domain_priors:theater_ratio(china_critical_mineral_chokepoint, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, extractiveness, 0.65).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_critical_mineral_chokepoint, tangled_rope).
narrative_ontology:human_readable(china_critical_mineral_chokepoint, "China's Strategic Chokepoint in Critical Mineral Processing").
narrative_ontology:topic_domain(china_critical_mineral_chokepoint, "economic/political/technological").

domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, chinese_state_and_soes).
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, chinese_downstream_manufacturers).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, non_chinese_high_tech_manufacturers).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, western_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CHINESE MANUFACTURER (SNARE) — Trapped by the lack of viable alternative suppliers in the short-to-medium term. Experiences the constraint as pure coercive extraction, subject to politically motivated supply disruptions and price manipulation. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11. The high chi value firmly places this in the Snare category.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHINESE STATE (ROPE) — Views the chokepoint as a legitimate industrial policy outcome and a global coordination mechanism it provides. It benefits from economic rents and strategic leverage, experiencing the constraint as a net subsidy. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative effective extraction signifies a clear beneficiary.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (providing the world with efficiently processed minerals) and the immense, asymmetric extractive power. The high base extraction and suppression, combined with the clear coordination role, define it as a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN GOVERNMENTS (SCAFFOLD) — Perceives the chokepoint as a temporary, unacceptable vulnerability. Actively building alternative supply chains (the scaffold) via subsidies and alliances. The constraint's power is seen as having a sunset clause, expiring when domestic processing capacity comes online. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.54. While chi is high, the sunset clause logic drives the Scaffold classification.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CHINESE DOMESTIC INDUSTRY (ROPE) — A powerful domestic actor that benefits from privileged access to a secure, low-cost supply of critical inputs. Experiences the state's control as a pure coordination benefit that enhances its global competitiveness. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_critical_mineral_chokepoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_critical_mineral_chokepoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_critical_mineral_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): High. China can extract significant economic rent through price controls and gain major political concessions by threatening or enacting export controls. Suppression (0.80): Very High. The barriers to creating alternative processing supply chains are immense, including massive capital costs, long lead times, complex engineering challenges, and stringent environmental regulations in Western countries that China historically bypassed to gain its lead. Theater Ratio (0.15): Low. The chokepoint is a functional, structural reality of the global industrial base. Its power is real, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. From Beijing's perspective, this is a successful industrial policy that provides a global public good (efficiently processed minerals), classifying as a Rope. For a dependent company like an American defense contractor or a European EV manufacturer, it is a Snare, as they have no immediate alternative and are subject to the whims of a geopolitical competitor. For Western governments, it's a temporary problem they are building a Scaffold to overcome. The analytical view must hold both truths at once: it is a system of both coordination and extraction, hence a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (Chinese state, domestic firms) have arbitrage power and see the system as a subsidy, leading to low or negative effective extraction (χ). The victims (foreign firms, Western autonomy) are trapped or constrained, leading to a high directionality (d) and thus a very high effective extraction (χ), pushing their classification to Snare. The organized efforts of Western governments to build alternatives give them a different structural position, leading to the Scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a clear refutation of mandatrophy. A simplistic analysis might label the chokepoint as 'bad' (Snare) or 'efficient' (Rope). The DR framework shows these are not mutually exclusive but are different indexed perspectives on the same structural reality. The system is simultaneously a Rope for beneficiaries, a Snare for the trapped, and a Scaffold for those actively building an exit. The core truth is the Tangled Rope classification from the analytical perspective, which acknowledges the duality of its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversification_velocity,
    'How quickly can non-Chinese entities build and scale alternative processing facilities, considering capital, environmental, and technical hurdles?',
    'Tracking capital investment, plant construction timelines, and output volumes of new non-Chinese processing facilities over a 5-10 year period.',
    'Rapid diversification confirms the ''Scaffold'' perspective and reduces the constraint''s suppression score. Slow or failed diversification entrenches the ''Snare'' perspective for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversification_velocity, empirical, 'The rate at which alternative supply chains can be established.').

omega_variable(
    strategic_intent_vs_economic_logic,
    'Is China''s primary motivation for maintaining the chokepoint economic efficiency and profit, or is it geopolitical leverage?',
    'Analysis of export control decisions: are they triggered by market price fluctuations (economic) or geopolitical tensions (strategic)?',
    'Primarily economic logic would lower the perceived suppression and base extractiveness. Primarily strategic logic confirms the high scores and the Snare/Tangled Rope classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_intent_vs_economic_logic, conceptual, 'Distinguishing between economic and geopolitical drivers of the constraint.').

omega_variable(
    technological_substitution_risk,
    'Will new technologies (e.g., sodium-ion batteries, new magnet compositions) reduce or eliminate the demand for the specific minerals China controls?',
    'Monitoring R&D progress and commercial adoption rates of technologies that substitute away from rare earths, gallium, etc.',
    'Successful substitution would render the constraint a Piton, as the vast processing infrastructure would lose its primary function but remain in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_risk, empirical, 'The potential for new technology to obsolete the controlled minerals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_critical_mineral_chokepoint, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t2004, china_critical_mineral_chokepoint, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(chin_tr_t2014, china_critical_mineral_chokepoint, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(chin_tr_t2024, china_critical_mineral_chokepoint, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(chin_be_t2004, china_critical_mineral_chokepoint, base_extractiveness, 2004, 0.3).
narrative_ontology:measurement(chin_be_t2014, china_critical_mineral_chokepoint, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(chin_be_t2024, china_critical_mineral_chokepoint, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_critical_mineral_chokepoint, resource_allocation).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, green_energy_transition).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, defense_technology_production).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
