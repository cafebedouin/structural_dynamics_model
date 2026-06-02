% ============================================================================
% CONSTRAINT STORY: crustal_hydrogen_availability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crustal_hydrogen_availability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: crustal_hydrogen_availability
 *   human_readable: Crustal Hydrogen Availability and Distribution
 *   domain: geochemistry/planetary_science
 *
 * SUMMARY:
 *   Crustal hydrogen availability represents a planetary-scale constraint on
 *   humanity's energy options. Earth's hydrogen is distributed among water
 *   (the dominant reservoir, ~1.4e21 kg in oceans plus continental water),
 *   hydrous silicates (bound in pyroxenes, amphiboles, micas), carbonates
 *   (hydroxyapatite, hydrated carbonates in sediments), and organic compounds
 *   (kerogen, methane hydrate). This hydrogen inventory is finite and fixed
 *   on human timescales (human timescales = years to millennia; planetary
 *   regeneration timescales = millions of years). The constraint emerges
 *   directly from stellar nucleosynthesis in the pre-solar nebula — Earth
 *   captured a fixed hydrogen budget during planetary accretion ~4.5 billion
 *   years ago. No technology, political arrangement, economic incentive, or
 *   institutional innovation can expand the total inventory. This is a
 *   Mountain constraint: an unchangeable physical/chemical limit. However,
 *   the framing of 'what counts as accessible hydrogen' and 'how is the
 *   distribution governed' creates secondary layers where political economy
 *   and distribution mechanics may introduce extraction patterns. The core
 *   constraint is natural law; the allocation of extracted hydrogen is
 *   institutional.
 *
 * KEY AGENTS:
 *   - Geological processes and time: No agent; the constraint emerges from stellar nucleosynthesis and planetary chemistry
 *   - Energy demand: Collective of civilizations seeking hydrogen as energy carrier; powerless before the absolute ceiling
 *   - Extraction technology: Represents human capacity to access hydrogen from various reservoirs; cannot change the total inventory
 *   - Geochemical system: The coupled water-rock-microbe system that governs hydrogen distribution and regeneration rates; no volition but exhibits emergent behaviors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crustal_hydrogen_availability, 0.18).
domain_priors:suppression_score(crustal_hydrogen_availability, 0.03).
domain_priors:theater_ratio(crustal_hydrogen_availability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crustal_hydrogen_availability, extractiveness, 0.18).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(crustal_hydrogen_availability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crustal_hydrogen_availability, mountain).
narrative_ontology:human_readable(crustal_hydrogen_availability, "Crustal Hydrogen Availability and Distribution").
narrative_ontology:topic_domain(crustal_hydrogen_availability, "geochemistry/planetary_science").

domain_priors:emerges_naturally(crustal_hydrogen_availability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY DEMAND AT SCALE (MOUNTAIN) — A civilization pursuing mass hydrogen extraction faces an absolute, unmalleable planetary constraint. The total hydrogen inventory in Earth's accessible crust cannot be exceeded, regardless of technology, political will, or capital investment. This is not a barrier that can be overcome through innovation or reorganization — it is a ceiling imposed by stellar nucleosynthesis and planetary chemistry.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a geochemical and cosmological view, the hydrogen constraint is a consequence of stellar nucleosynthesis in the pre-solar nebula and differential planetary accretion. Earth captured a fixed inventory of hydrogen at ~4.5 Ga. No physical process can expand this inventory on human timescales. This is a hard ceiling, not a soft limit.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENERGY INFRASTRUCTURE (MOUNTAIN) — Even agents with maximal institutional and technological capability (national governments, multinational energy corporations) experience this as an immutable boundary. The constraint cannot be arbitraged, renegotiated, or circumvented. It determines the upper bound of any energy infrastructure strategy based on hydrogen.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crustal_hydrogen_availability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(crustal_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crustal_hydrogen_availability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, ExtMetricName, E),
    domain_priors:suppression_score(crustal_hydrogen_availability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(crustal_hydrogen_availability),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(crustal_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. This is the critical metric for Mountain classification. Extractiveness measures how much of the constraint's burden falls on specific agents. Crustal hydrogen availability is a natural law with no beneficiary/victim structure — it is not extractive in the institutional sense. The 0.18 value reflects a minor administrative/measurement cost (characterizing hydrogen distribution requires geochemical surveys and modeling) but no systematic extraction. Suppression (0.03): Near-zero. The constraint imposes no coercive apparatus, no punishment for non-compliance, no enforcement mechanism. It is simply a boundary. Geological processes do not suppress alternatives; alternatives do not exist. Theater ratio (0.05): Near-zero. The constraint requires minimal performative content. Accessing crustal hydrogen requires genuine technical capability, not ritual compliance. There is no theater — either the hydrogen is there or it is not. Accessibility collapse (0.92): Very high. The constraint collapses alternative accessible sources almost completely — alternatives (extraterrestrial hydrogen, fusion-generated hydrogen, hypothetical other energy carriers) lie far outside practical reach. Humans are trapped in the terrestrial hydrogen inventory. Resistance (0.08): Very low. There is no resistance to the constraint's logic or legitimacy — no credible scientific disagreement with the planetary formation model or the inventory estimate. Emerges naturally: TRUE. The constraint emerges from stellar nucleosynthesis and planetary chemistry without human intervention.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most DR constraints, this one shows NO perspectival gap — all observers classify it as Mountain. This uniformity is the signature of a genuine natural law. The powerless agent (energy demand), the institutional actor (energy infrastructure), and the analytical observer all perceive the same absolute, unchangeable boundary. This is the gold standard for Mountain classification: invariant across all observables and observer positions. The absence of a perspectival gap is itself the evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation (beneficiary/victim → d → f(d) → χ) is not applicable to genuine natural law Mountains. There are no beneficiaries or victims of crustal hydrogen availability — it is not an institutional arrangement that benefits some agents at the expense of others. The constraint is pre-institutional. However, the ALLOCATION of extracted hydrogen and the DISTRIBUTION of extraction capacity ARE subject to institutional politics and potential extraction patterns. Those are secondary layers — they sit on top of the natural law constraint. This constraint story addresses the natural law; secondary stories about hydrogen extraction rights, technology access, and geopolitical leverage would be separate constraints in the same domain.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy arises here. The constraint is not mislabeled and does not hide multiple types. It is simply and unambiguously Mountain — a natural law. The classification is stable across all perspectives, all time horizons, all power levels, all exit options. This is what a genuine natural law looks like in the DR framework. The mandatrophy resolution is: this constraint has no mandatrophy. It is not a case where different perspectives disagree on the type or where institutional layers create ambiguity. It is a pure boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accessible_inventory_demarcation,
    'What fraction of Earth''s crustal hydrogen is genuinely accessible to human extraction given drilling depth, pressure/temperature limits, and economic viability?',
    'Geochemical surveys of hydrogen distribution by depth; cost curves for deep drilling and recovery; thermodynamic modeling of equilibrium states at various extraction depths',
    'If accessible_fraction = 0.30, the effective constraint is tighter than the total inventory. If accessible_fraction = 0.70+, technology improvement could expand practical access. Classification remains Mountain either way (ε ≤ 0.25 is maintained), but the severity of the constraint shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessible_inventory_demarcation, empirical, 'Proportion of total crustal hydrogen accessible to human extraction').

omega_variable(
    extraction_thermodynamic_coupling,
    'Does hydrogen extraction from hydrogeological systems trigger secondary geochemical responses (mineral dissolution/precipitation, microbial blooms, pressure cascades) that alter extraction efficiency or make the resource effectively less available?',
    'Long-term field experiments in aquifer systems; reactive transport modeling of water-rock-microbe interactions during sustained hydrogen withdrawal; comparison of theoretical vs actual recovery rates',
    'If coupling is weak: hydrogen availability is roughly as calculated from static inventory. If coupling is strong: effective availability may degrade by 20-40% over decadal timescales due to induced chemical and biological responses. Does not change Mountain classification but informs practical deployment constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_thermodynamic_coupling, empirical, 'Whether extraction induces secondary geochemical responses reducing effective availability').

omega_variable(
    regeneration_rate_credibility,
    'Is the ''abiotic hydrogen generation'' pathway (serpentinization, radiolytic water splitting) capable of replenishing hydrogen fast enough to matter for energy infrastructure timescales?',
    'Direct measurement of H2 generation in serpentinization zones; isotope tracing of abiotic vs biotic hydrogen; flux estimates from natural analogs (Lost City hydrothermal field, etc.); comparison of theoretical regeneration rates to consumption scenarios',
    'If regeneration_rate > 1% of current geothermal flux: there may be a small renewable component to crustal hydrogen, shifting the constraint from a static inventory ceiling to a renewable resource with very low flux. If regeneration_rate < 0.1%: regeneration is negligible and the constraint remains a hard inventory ceiling. Classification remains Mountain regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regeneration_rate_credibility, empirical, 'Rate at which abiotic hydrogen regeneration could replenish crustal stocks').

omega_variable(
    alternative_nucleosynthesis_framing,
    'Does framing this constraint as ''the hydrogen legacy of stellar nucleosynthesis'' vs ''the hydrogen allocation problem within Earth''s crust'' change the perspectival gap or classification landscape?',
    'Conceptual: whether shifting the kernel from stellar chemistry to planetary distribution mechanics creates new agent perspectives or uncovers different beneficiary/victim structures. Analytical exercise, not empirical.',
    'If nucleosynthesis framing dominates: the constraint appears as a pure natural law with no beneficiary/victim distinction. If planetary distribution framing dominates: different regions, nations, and technologies have asymmetric access — potentially revealing an extractive layer on top of the natural law. This could trigger FSM evaluation if distribution is not treated as natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_nucleosynthesis_framing, conceptual, 'Whether nucleosynthesis or planetary distribution framing changes perspectival structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crustal_hydrogen_availability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crust_h_tr_t0, crustal_hydrogen_availability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(crust_h_tr_t30, crustal_hydrogen_availability, theater_ratio, 30, 0.05).
narrative_ontology:measurement(crust_h_tr_t100, crustal_hydrogen_availability, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(crust_h_be_t0, crustal_hydrogen_availability, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(crust_h_be_t30, crustal_hydrogen_availability, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(crust_h_be_t100, crustal_hydrogen_availability, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crustal_hydrogen_availability, global_infrastructure).
narrative_ontology:affects_constraint(crustal_hydrogen_availability, hydrogen_economy_transition).
narrative_ontology:affects_constraint(crustal_hydrogen_availability, energy_system_decarbonization).
narrative_ontology:affects_constraint(crustal_hydrogen_availability, synthetic_fuel_viability).

% DUAL FORMULATION NOTE:
% Crustal hydrogen availability is a boundary constraint on several downstream constraints about energy infrastructure. The three affected constraints represent different strategic responses to the hydrogen availability ceiling: whether a hydrogen economy is economically viable (downstream), what decarbonization paths are feasible without relying on abundant hydrogen (downstream), and whether synthetic fuels can substitute (downstream). Each downstream constraint has its own ε value and type; this constraint anchors all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
