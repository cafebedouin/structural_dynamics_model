% ============================================================================
% CONSTRAINT STORY: ancestral_pueblo_hydrology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancestral_pueblo_hydrology, []).

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
 *   constraint_id: ancestral_pueblo_hydrology
 *   human_readable: Ancestral Puebloan Hydrological Debt
 *   domain: environmental/social
 *
 * SUMMARY:
 *   The Ancestral Puebloan hydrological constraint models the structural
 *   entrapment created by water-dependent agricultural societies in semi-arid
 *   environments when elite institutions capture coordination mechanisms for
 *   extractive purposes. Chaco Canyon (c. 900-1150 CE) and Mesa Verde (c.
 *   1190-1300 CE) developed sophisticated hydrological infrastructure
 *   (terraces, check dams, reservoirs) that genuinely solved collective
 *   action problems: flood management, seasonal distribution, drought
 *   buffering. However, elite households and kiva societies used control of
 *   water distribution to extract labor, agricultural surplus, and status
 *   goods from subsistence farmers. The constraint exhibits the full
 *   mandatrophy: from the subsistence farmer's perspective, the system
 *   appears as a pure snare (trapped in extraction, no exit); from the elite
 *   perspective, it appears as rope (solving coordination problems); from the
 *   paleoclimatic view, it appears as mountain (immutable regional
 *   precipitation ceiling); from the deep temporal view, it appears as piton
 *   (theatrical ceremonial redistribution persisting despite declining
 *   hydrological function). The extractiveness rose from 0.35 to 0.68 over
 *   the interval as drought intensified (megadrought 1130-1180 CE and
 *   1240-1300 CE), forcing elite institutions to intensify appropriation.
 *   Theater ratio rose from 0.25 to 0.55 as ceremonial narrative expansion
 *   compensated for declining material redistributive capacity. The collapse
 *   (settlement abandonment by 1320 CE) followed the intersection of
 *   prolonged drought and institutional failure — the constraint became
 *   unsustainable when hydrological stress exceeded both elite extraction
 *   capacity and commoner subsistence margins.
 *
 * KEY AGENTS:
 *   - Subsistence Farmers: Primary victims (powerless/trapped) — dependent on seasonal water for agricultural survival; bear full cost of drought intensification and elite extraction
 *   - Regional Elite Households: Primary beneficiaries (institutional/arbitrage) — control water distribution, extract labor and surplus; experience constraint as coordination mechanism
 *   - Kiva Societies: Secondary institutional actors (organized/constrained) — ceremonial and defense function; serve as extraction intermediaries for elite households
 *   - Downstream Communities: Secondary victims (moderate/constrained) — experience water shortage as upstream water-harvesting systems intensify; reduced access to seasonal flooding
 *   - Future Generations: Abstract victim (powerless/trapped) — experience the legacy of environmental degradation and settlement collapse; no contemporary exit options
 *   - Paleoclimatic System: Analytical observer (analytical/analytical) — deep temporal view reveals the constraint as both natural law (precipitation ceiling) and contingent institutional failure (elite extraction intensity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancestral_pueblo_hydrology, 0.68).
domain_priors:suppression_score(ancestral_pueblo_hydrology, 0.72).
domain_priors:theater_ratio(ancestral_pueblo_hydrology, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, extractiveness, 0.68).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancestral_pueblo_hydrology, snare).
narrative_ontology:human_readable(ancestral_pueblo_hydrology, "Ancestral Puebloan Hydrological Debt").
narrative_ontology:topic_domain(ancestral_pueblo_hydrology, "environmental/social").

domain_priors:requires_active_enforcement(ancestral_pueblo_hydrology).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancestral_pueblo_hydrology, regional_elite_households).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, subsistence_farmers).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, downstream_communities).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Subsistence farmers operating in Chaco and Mesa Verde regions experience the hydrological constraint as a snare. Dependent on seasonal water availability for corn, beans, and squash; no alternative livelihood options; cannot migrate without abandoning ancestral lands. As water stress increases, suppression becomes total — agricultural failure equals starvation. No exit. Maximum extraction experienced as pure resource deprivation.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Pueblo nation-states and inter-settlement confederations experience the constraint as tangled rope. Coordinating defense, trade, and ceremonial exchange requires shared water infrastructure and astronomical observation — cooperation provides genuine benefits. But this coordination mechanism is asymmetrically captured by elite households controlling water redistribution. Chiefs and kivas extract disproportionate surpluses under the guise of ceremonial obligation. Exit is constrained: abandoning the coalition means losing shared defense and trade networks.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Chacoan elites and kiva societies experience the constraint as rope — pure coordination. Water-management systems (terracing, check dams, reservoirs) solve genuine collective action problems: flood protection, drought buffering, seasonal distribution. The elite households that control water allocation see themselves as solving coordination problems through ritual and ceremonial redistribution. They experience the constraint as solving, not exploiting — institutional arbitrage with low experienced extraction.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% From the deep temporal perspective of paleoclimate and dendrochronology, the Ancestral Puebloan system appears as piton — a degraded water-management institution persisting through theatrical maintenance. The civilization's water-control systems, while functionally sophisticated, became increasingly performative as climatic conditions deteriorated. Monumental construction continued despite declining yields; ceremonial redistribution theater expanded as material redistributive capacity contracted. The institutions persisted through narrative (the Corn Mother, emergence myths) long after real hydrological function declined. Theater ratio rises as drought persists.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational analytical perspective, the hydrological constraint appears as mountain — an immutable natural law. Chaco Canyon received 8-10 inches annual precipitation (semi-arid limit); mesa-top communities relied on episodic flooding. Sustained population growth always faced a ceiling: no technology or social organization can overcome the thermodynamic ceiling of regional precipitation. The mountain classification reflects that all alternative trajectories (irrigation intensification, technological innovation, social reorganization) hit the same hydrological boundary. However, this classification risks naturalizing what is partly contingent: the civilization's extractive intensification (elite appropriation of surpluses, monumental construction, population centralization) made the eventual collapse more severe, not inevitable.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancestral_pueblo_hydrology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancestral_pueblo_hydrology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancestral_pueblo_hydrology, TR),
    TR >= 0.70.

:- end_tests(ancestral_pueblo_hydrology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. Elite households extract significant labor surpluses and agricultural output through control of water redistribution, but the extraction is partial and mediated through ceremonial obligation rather than total coercion. Subsistence farmers retain some autonomous production capacity and ceremonial participation generates psychological and status benefits. The value reflects moderate-high extraction constrained by legitimacy requirements of ceremonial redistribution. Suppression (0.72): High. Subsistence farmers face complete dependence on elite-controlled water systems with no technological alternative, no exit option, and no organizational capacity for collective resistance. Migrating meant abandoning ancestral lands and losing access to regional trade networks. Suppression is total by the end of the interval. Theater ratio (0.55): Moderate-rising. Early period (900-1050 CE) water management was functionally sophisticated — ceremonial redistribution legitimately coordinated collective action. As drought intensified, the same ceremonial institutions persisted but became increasingly performative: kiva construction continued despite declining yields; redistribution theater expanded as material capacity contracted. The theater ratio rise indicates institutional degradation (piton dynamics) where ceremonial maintenance replaced functional hydrological management.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (elite control of water in a semi-arid region) generates completely different classifications from different perspectives. The subsistence farmer sees a snare: they are trapped, suppressed, exploited with no exit. The elite see rope: they are solving the genuine coordination problem of managing scarce water and have substantial agency. The kiva societies see tangled rope: they coordinate defense and ceremonial exchange, but their position is constrained by elite capture of their redistribution function. The paleoclimatic observer sees mountain: no region-specific technology can overcome the 8-10 inch annual precipitation ceiling. The deep temporal observer sees piton: the ceremonial institutions persisted through narrative and ritual long after their hydrological function declined. The perspectival gap is fundamental — no single classification captures the full structural reality. The snare classification for the subsistence farmer is not wrong; nor is the rope classification for the elite; nor is the mountain classification for the paleoclimatic view. The constraint is a presheaf over observation positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Subsistence farmers occupy d ≈ 0.95 (trapped victims with no exit) → f(d) ≈ 1.42 → high experienced extractiveness. Elite households occupy d ≈ 0.10 (beneficiaries with arbitrage options) → f(d) ≈ -0.01 → negative experienced extraction (they experience net benefit). Kiva societies occupy d ≈ 0.50 (symmetric: they coordinate but are partly captured) → f(d) ≈ 0.65 → moderate experienced extraction. The scope modifier σ(regional) = 0.9 applies because verification of elite extraction requires local knowledge of distribution patterns and ceremonial obligations. The computed χ values reflect how different agents experience the same constraint: χ_farmer ≈ 0.68 × 1.42 × 0.9 ≈ 0.87 (high effective extraction); χ_elite ≈ 0.68 × (-0.01) × 0.9 ≈ -0.006 (negative/benefit); χ_kiva ≈ 0.68 × 0.65 × 0.9 ≈ 0.40 (moderate extraction). The directionality derivation is straightforward structural causation — no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING THE MANDATROPHY: The constraint resolves the apparent paradox of simultaneous coordination and extraction by showing that mandatrophy is not 'which type is correct?' but 'who is measuring?' The Ancestral Puebloan hydrological system WAS a genuine coordination mechanism that solved real collective action problems (flood protection, seasonal distribution, drought buffering). It also WAS an extraction mechanism that concentrated labor, surplus, and status in elite households. These are not contradictory — they are simultaneous structural facts. The mandatrophy resolves by recognizing that legitimacy (the psychological and social acceptance of the system) creates coordination benefits even within extraction relationships. Subsistence farmers participated in kiva ceremonies, accepted elite redistribution as legitimate, and genuinely benefited from collective flood protection. They were simultaneously trapped in extraction with no exit. Both truths hold. The mountain classification (from the paleoclimatic view) risks naturalizing the constraint by treating the 8-10 inch precipitation ceiling as the cause of collapse. But alternative trajectories existed: other semi-arid societies (Nabataeans, Hohokam in their early phases) maintained lower-density settlement patterns and avoided elite extraction intensification. The Ancestral Puebloan collapse was not inevitable from climate alone — it resulted from institutional choices (elite appropriation, population centralization, ceremonial intensification) that made the inevitable hydrological stress more severe. The snare classification is the most honest frame: commoners were trapped by elite institutions into a system that appeared coordinated but functioned as extraction, and when drought stress exceeded carrying capacity, the legitimacy failed catastrophically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_extraction_vs_coordination,
    'To what degree did elite water control represent genuine coordination against drought versus extraction of surplus labor and agricultural output?',
    'Archaeological analysis of labor allocation (monument construction vs subsistence activity); isotopic evidence of elite diet quality vs commoner diet; settlement dispersal patterns after elite collapse',
    'If coordination-dominant: constraint reclassifies toward Rope. If extraction-dominant: constraint reclassifies toward Snare. Current estimate (0.68 extractiveness) assumes mixed model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_extraction_vs_coordination, empirical, 'Proportion of elite water control attributable to genuine coordination versus extraction').

omega_variable(
    climate_determinism_boundary,
    'What fraction of the Ancestral Puebloan collapse was driven by megadrought (hydroclimate) versus institutional failure (elite extraction, population density, maladaptive investment)?',
    'Climate reconstruction from tree-rings, stalagmites, lake cores; modeling of population-carrying capacity under reconstructed rainfall; comparison with alternative societies facing similar climatic stress',
    'If >70% climate-determined: mountain classification is justified. If <50%: classification should reflect human agency and institutional failure, shifting toward Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_determinism_boundary, empirical, 'Attribution of collapse to paleoclimate versus institutional factors').

omega_variable(
    ceremonial_redistribution_function,
    'Did kiva-based ceremonial redistribution actually equalize resource distribution or primarily redistribute upward to elite households?',
    'Archaeological distribution of exotic goods, cache contents, and ritual deposits; bone chemistry (isotopic) evidence of diet access; spatial analysis of storage facilities relative to settlement hierarchy',
    'If downward-redistributing: Rope classification strengthens. If upward-concentrating: Snare classification strengthens. Current mixed classification reflects uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_redistribution_function, empirical, 'Net direction of ceremonial redistribution flows').

omega_variable(
    alternative_trajectory_availability,
    'Were there viable alternative trajectories (reduced population density, settlement dispersal, intensified trade) that the Ancestral Puebloans could have pursued to avoid collapse?',
    'Comparative analysis with other semi-arid civilizations (Nabataeans, Pre-Columbian Sonoran Desert societies); modeling of population sustainability under dispersed settlement patterns; trade network analysis',
    'If alternatives existed: extraction and institutional failure are more culpable, constraint reclassifies toward Snare. If alternatives unavailable: mountain/parity classification more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_trajectory_availability, conceptual, 'Whether viable alternatives to the collapse trajectory existed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancestral_pueblo_hydrology, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apueblo_tr_t0, ancestral_pueblo_hydrology, theater_ratio, 0, 0.25).
narrative_ontology:measurement(apueblo_tr_t150, ancestral_pueblo_hydrology, theater_ratio, 150, 0.42).
narrative_ontology:measurement(apueblo_tr_t300, ancestral_pueblo_hydrology, theater_ratio, 300, 0.55).

% Extraction over time
narrative_ontology:measurement(apueblo_be_t0, ancestral_pueblo_hydrology, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(apueblo_be_t150, ancestral_pueblo_hydrology, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(apueblo_be_t300, ancestral_pueblo_hydrology, base_extractiveness, 300, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancestral_pueblo_hydrology, resource_allocation).
narrative_ontology:affects_constraint(ancestral_pueblo_hydrology, southwestern_megadrought_1130_1180).
narrative_ontology:affects_constraint(ancestral_pueblo_hydrology, chaco_road_network_hierarchy).
narrative_ontology:affects_constraint(ancestral_pueblo_hydrology, puebloan_kiva_ceremonialism).

% DUAL FORMULATION NOTE:
% The hydrological constraint is downstream of paleoclimatic forcing (megadroughts) and upstream of institutional collapse dynamics. Separate constraint stories exist for the megadrought as natural law (mountain) and for the elite extraction institutions as social snare. The hydrological debt story bridges them: it models how elite institutions intensified extraction in response to climatic stress, amplifying the eventual collapse beyond what climate alone would produce.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
