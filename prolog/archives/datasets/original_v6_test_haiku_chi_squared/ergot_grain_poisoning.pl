% ============================================================================
% CONSTRAINT STORY: ergot_grain_poisoning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergot_grain_poisoning, []).

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
 *   constraint_id: ergot_grain_poisoning
 *   human_readable: The Ergot/Famine Dilemma
 *   domain: social/technological/biological
 *
 * SUMMARY:
 *   The ergot/famine dilemma represents a constraint at the intersection of
 *   biology, agriculture, and social power that persisted across 600 years of
 *   European history until modern grain handling eliminated it. Ergot
 *   (Claviceps purpurea) is a fungal parasite that produces ergotamine
 *   alkaloids—the chemical parent of LSD—causing convulsions, hallucinations,
 *   gangrene, and death when consumed in contaminated grain. From the
 *   subsistence population's perspective, the constraint is a snare: eat
 *   poisoned grain or starve. From the grain producer's perspective with
 *   ergot knowledge, it is coordination problem: disseminating prevention
 *   techniques would reduce contamination. From the church's perspective,
 *   ergot epidemics are divine punishment that reinforce ecclesiastical
 *   authority and generate pilgrimage revenue—a tangled rope mixing genuine
 *   sanctuary provision with extraction of fear-driven donations. The
 *   constraint exhibits theater: medieval grain storage systems are largely
 *   performative (granaries exist but lack effective drying and inspection),
 *   yet persist through institutional obligation. The extractiveness has
 *   increased over the interval (0.28 to 0.52) as population pressure
 *   intensified competition for grain and as religious interpretations of
 *   ergot became institutionalized, directing resources toward pilgrimage
 *   sites rather than agricultural reform. The constraint has no single
 *   type—it is all six, depending on observational position.
 *
 * KEY AGENTS:
 *   - Subsistence populations: Primary victims (powerless/trapped) — forced to consume contaminated grain or face starvation; zero exit options
 *   - Marginal grain consumers: Secondary victims (moderate/constrained) — poorer populations consume higher-contamination lots; cannot afford alternative foods
 *   - Grain producers with ergot knowledge: Primary beneficiaries (institutional/arbitrage) — understand sclerotia identification, winnowing, drying; can reduce contamination through selective practices
 *   - Religious authorities: Institutional beneficiaries (powerful/arbitrage) — interpret ergot as divine punishment; capture authority and resources through pilgrimage; provide genuine sanctuary but extract wealth and social control
 *   - Agricultural reformers: Organized agents (organized/constrained) — develop grain inspection mandates and crop rotation rules; see ergot as temporary problem with sunset through agricultural science
 *   - Medieval feudal grain systems: Institutional actor (institutional/constrained) — tithe collection, granary maintenance; largely performative by ergot era, persisting through obligation rather than effectiveness
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent agricultural practices as immutable biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergot_grain_poisoning, 0.52).
domain_priors:suppression_score(ergot_grain_poisoning, 0.68).
domain_priors:theater_ratio(ergot_grain_poisoning, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergot_grain_poisoning, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergot_grain_poisoning, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ergot_grain_poisoning, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergot_grain_poisoning, tangled_rope).
narrative_ontology:human_readable(ergot_grain_poisoning, "The Ergot/Famine Dilemma").
narrative_ontology:topic_domain(ergot_grain_poisoning, "social/technological/biological").

domain_priors:requires_active_enforcement(ergot_grain_poisoning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, grain_producers_with_selection_knowledge).
narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, religious_authorities_interpreting_plagues).
narrative_ontology:constraint_victim(ergot_grain_poisoning, subsistence_populations).
narrative_ontology:constraint_victim(ergot_grain_poisoning, marginal_grain_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE POPULATION (SNARE) — Trapped between consuming contaminated grain (ergot poisoning) or facing starvation. No exit: cannot afford to reject grain, cannot relocate to unaffected regions, cannot access uncontaminated food supplies. Extraction is severe: the constraint forces consumption of a poisoned commodity without alternative. d≈0.94, f(d)≈1.40, σ=0.9 → χ≈0.67.
constraint_indexing:constraint_classification(ergot_grain_poisoning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MARGINAL GRAIN CONSUMER (TANGLED ROPE) — Constrained by poverty and harvest vulnerability, but also benefits from grain availability itself (coordination function: grain production enables survival). During ergot years, bears disproportionate cost because wealthy can afford clean grain or alternative foods. d≈0.72, f(d)≈1.13, σ=0.9 → χ≈0.53.
constraint_indexing:constraint_classification(ergot_grain_poisoning, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRAIN PRODUCERS WITH ERGOT KNOWLEDGE (ROPE) — Institutional coordination: those who understand ergot (can identify sclerotia, know winnowing techniques, understand wet/damp conditions favor infection) benefit from both grain production AND knowledge selection. They can reduce ergot risk through crop rotation, drying practices, and visual inspection. They experience the constraint as a coordination problem: disseminating knowledge would stabilize grain supplies. d≈0.15, f(d)≈0.02, σ=0.9 → χ≈0.01.
constraint_indexing:constraint_classification(ergot_grain_poisoning, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RELIGIOUS AUTHORITIES (TANGLED ROPE) — Interpret ergot poisoning epidemics as divine punishment (St. Anthony's Fire), which reinforces their institutional authority and generates pilgrimage revenue and donations. Coordination function: church offers sanctuary and collective prayer during crisis. Extraction: church captures authority to define the plague's meaning and solution, directing fear-driven resources to ecclesiastical institutions. They have arbitrage exit (can reframe causation, redirect blame). d≈0.25, f(d)≈0.08, σ=0.9 → χ≈0.04.
constraint_indexing:constraint_classification(ergot_grain_poisoning, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: AGRICULTURAL REFORMERS (SCAFFOLD) — Organized agents (agronomists, clergy promoting hygiene, local authorities mandating grain inspection) see ergot as a problem WITH a sunset: systematic grain inspection, crop rotation, and improved drying practices can reduce contamination. This is temporary coordination to build toward a permanent solution (fungicide-resistant varieties, industrial grain cleaning). d≈0.35, f(d)≈0.28, σ=0.9 → χ≈0.13. Sunset clause: by 18th-19th centuries, these reforms reduce ergot outbreaks to near-zero in regions with centralized grain control.
constraint_indexing:constraint_classification(ergot_grain_poisoning, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MEDIEVAL GRAIN STORAGE SYSTEMS (PITON) — The feudal tithe and grain storage system is largely performative by the ergot era (13th-16th centuries): granaries exist but are poorly maintained, drying is inconsistent, and inspection is minimal. Theater ratio (0.48) is moderate here—some functional storage exists but much is degraded ritual. The system persists through institutional inertia (lord's duty, tithe obligation) rather than effectiveness. d≈0.40, f(d)≈0.40, σ=0.9 → χ≈0.18.
constraint_indexing:constraint_classification(ergot_grain_poisoning, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL INEVITABILITY (MOUNTAIN?) — From a civilizational perspective, ergot is a fungal parasite that will infect damp grain wherever human agriculture exists. This appears as an immutable biological law: grain cultivation creates the ecological conditions for Claviceps; prevention requires knowledge or technology not available before the 19th century. However, structural data (ε=0.52, suppression=0.68, theater=0.48) contradicts mountain criteria (ε≤0.25). The engine will classify this as a false summit: ergot is contingent on grain handling practices, not an inherent biological necessity.
constraint_indexing:constraint_classification(ergot_grain_poisoning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergot_grain_poisoning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergot_grain_poisoning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergot_grain_poisoning, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergot_grain_poisoning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergot_grain_poisoning, TR),
    TR >= 0.70.

:- end_tests(ergot_grain_poisoning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, final): Moderate-high and rising. The constraint extracts through multiple mechanisms: (1) Population growth and marginal land cultivation increase ergot exposure (wetter soils favor fungus); (2) Feudal tithe systems prioritize tax grain collection over quality control, concentrating contamination in poorer households; (3) Religious interpretations direct crisis response toward pilgrimage rather than prevention. The trajectory from 0.28 to 0.52 over the interval reflects intensifying extraction as population density increased and religious institutions monopolized interpretation of the plague. By 1500s, ergot epidemics were endemic in some regions, extracting immense cost in mortality, disability, and institutional loyalty. Suppression (0.68, high): Substantial barriers prevent escape. Subsistence farmers cannot refuse grain. Alternative proteins (legumes, meat) are expensive or unavailable. Geographic mobility is restricted (feudalism). Knowledge of prevention is not accessible (guild secrets, clerical monopoly on written knowledge). The only escape is wealth—the wealthy buy clean grain or alternative foods. Suppression is structural and deliberate: feudal systems did not invest in grain quality control because contaminated grain still collected taxes and kept populations dependent. Theater ratio (0.48, moderate): Grain storage systems are partly functional and partly performative. Granaries do exist and serve real coordination purposes (stabilizing supply across seasons). But inspection and drying are inconsistent. The performative element increases over time: religious processions and pilgrimages to St. Anthony's shrines are ritual responses that displace actual prevention. By 1600s, the theater dominates institutional response—pilgrimage revenue grows as ergot episodes continue.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The subsistence population sees an unavoidable poisoning mechanism (snare). The grain producer with knowledge sees a solvable problem requiring information dissemination (rope). The religious authority sees an opportunity to reinforce institutional power through interpretation (tangled rope, mixing genuine sanctuary with extraction). The agricultural reformer sees a temporary problem yielding to systematic reform (scaffold). The feudal grain system sees itself as functional coordination, but the analytical observer sees it as largely performative (piton). The civilizational observer risks seeing immutable biological fate (mountain), but the structural data reveals contingency: ergot is not inevitable—it results from damp grain handling, population pressure on marginal lands, and institutional failure to invest in quality control. The perspectival gap measures institutional capture: the church monopolizes interpretation, preventing the knowledge-holder's (grain producer's) rope perspective from becoming dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Subsistence population: Victim + trapped → d≈0.94, f(d)≈1.40. Maximum extraction. Marginal grain consumer: Victim + constrained → d≈0.72, f(d)≈1.13. High extraction; has minimal alternatives. Grain producers with knowledge: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary; can exit through knowledge transfer or relocation. Religious authorities: Beneficiary + arbitrage (but partly captured by their own institutional narrative) → d≈0.25, f(d)≈0.08. Institutional beneficiary; they have exit (can reframe causation) but are somewhat locked into the pilgrimage-interpretation narrative. Agricultural reformers: Organized + constrained → d≈0.35, f(d)≈0.28. Low effective extraction; they see the constraint as temporary and solvable. Feudal grain systems: Institutional + constrained (not true arbitrage because the system cannot easily modernize) → d≈0.40, f(d)≈0.40. Moderate extraction as a piton.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled_rope is the correct canonical classification at the civilizational analytical level, not mountain (false summit via naturalization). The extractiveness (0.52) and suppression (0.68) clearly exceed mountain thresholds (ε≤0.25, suppression≤0.05). The constraint exhibits both coordination (grain production enables survival) and asymmetric extraction (the constraint forces poisoned consumption on the powerless while benefiting authorities). Beneficiaries include grain producers with knowledge and religious institutions that profit from epidemic interpretation. Victims include subsistence populations and marginal consumers. The constraint requires active institutional enforcement: feudal tithe systems, clerical monopoly on knowledge, lack of grain quality regulation. These three features (beneficiaries, victims, active enforcement) satisfy the tangled_rope gate. The rope perspective (beneficiary grain producer) is empirically real but minority—confined to elites with agronomic knowledge. The snare perspective (subsistence population) is the majority experience. The mandatrophy is resolved by recognizing that the constraint IS a tangled rope (mixing genuine coordination with extraction) across all temporal and spatial scales, but the extraction component dominates at the subsistence level because knowledge is monopolized and institutions fail to disseminate prevention. The scaffold and piton perspectives show that this could be resolved—agricultural reform reduces ergot to negligible levels by 1850—but resolution was delayed by institutional capture (church monopolizing interpretation) and economic structures (feudal systems not incentivized to improve grain quality).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ergot_knowledge_distribution_threshold,
    'What fraction of a grain-producing region''s population needs to understand ergot identification and prevention for transmission of contaminated grain to cease?',
    'Historical analysis of regional grain contamination rates correlated with documented spread of agronomic knowledge; comparison of regions with organized guild knowledge (bakers, millers) vs decentralized peasant farming',
    'If threshold < 15%: constraint becomes rope-dominant even at regional scale. If threshold > 50%: ergot persists as snare until industrial grain cleaning (19th century).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergot_knowledge_distribution_threshold, empirical, 'Knowledge distribution threshold for ergot control').

omega_variable(
    religious_authority_capture_mechanism,
    'Does the church''s interpretation of ergot as divine punishment (St. Anthony''s Fire) reduce or increase contamination by directing resources toward actual prevention?',
    'Comparison of ergot outbreak rates in regions with strong ecclesiastical authority over grain distribution vs secular-managed regions; analysis of church-sponsored grain inspection vs pilgrimage-driven wealth extraction',
    'If prevention-dominant: religious interpretation functions as coordination (constraint becomes Rope from church perspective). If extraction-dominant: religious interpretation is purely extractive narrative (remains Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_authority_capture_mechanism, empirical, 'Whether religious authority reduces or exploits ergot epidemics').

omega_variable(
    famine_severity_feedback_loop,
    'Does rejection of ergot-contaminated grain during scarcity increase starvation-driven mortality more severely than ergotism mortality, creating a perverse incentive to consume poison?',
    'Historical mortality data comparing ergotism deaths vs starvation deaths in same regions during same years; economic analysis of grain prices and availability during outbreak years',
    'If starvation > ergotism: constraint becomes unavoidable (suppression≥0.80). If ergotism > starvation: populations could theoretically refuse contaminated grain (suppression closer to 0.50).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(famine_severity_feedback_loop, empirical, 'Starvation vs ergotism mortality trade-off').

omega_variable(
    winnowing_sufficiency_for_prevention,
    'Does mechanical winnowing and visual inspection of rye actually remove ergot sclerotia at rates sufficient to reduce contamination below poisoning thresholds?',
    'Technical analysis of sclerotia size and density vs winnowing effectiveness; historical accounts of miller and baker practices in regions with low ergot rates',
    'If sufficient: ergot becomes manageable through coordination of grain handling (constraint becomes rope). If insufficient: constraint remains snare until fungicide or modern grain cleaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(winnowing_sufficiency_for_prevention, empirical, 'Mechanical removal effectiveness for ergot prevention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergot_grain_poisoning, 1200, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergot_tr_t0, ergot_grain_poisoning, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ergot_tr_t4, ergot_grain_poisoning, theater_ratio, 4, 0.42).
narrative_ontology:measurement(ergot_tr_t8, ergot_grain_poisoning, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(ergot_be_t0, ergot_grain_poisoning, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ergot_be_t4, ergot_grain_poisoning, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ergot_be_t8, ergot_grain_poisoning, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergot_grain_poisoning, resource_allocation).
narrative_ontology:affects_constraint(ergot_grain_poisoning, feudal_grain_monopoly).
narrative_ontology:affects_constraint(ergot_grain_poisoning, ecclesiastical_knowledge_monopoly).
narrative_ontology:affects_constraint(ergot_grain_poisoning, agricultural_innovation_diffusion).

% DUAL FORMULATION NOTE:
% The ergot dilemma is downstream of feudal grain monopolies (constraints on who can own/trade grain) and ecclesiastical monopolies on written knowledge (constraints on who can disseminate agricultural know-how). These upstream constraints have their own ε values (likely higher, as they are purely extractive). The ergot constraint (ε=0.52) represents the intersection of biological contingency with institutional capture. Decomposition: ergot_as_biological_hazard (ε≈0.15, rope—pure coordination problem) vs ergot_as_extracted_famine_mechanism (ε≈0.52, tangled_rope—institutional capture of response).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergot_grain_poisoning, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
