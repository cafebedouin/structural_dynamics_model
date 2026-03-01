% ============================================================================
% CONSTRAINT STORY: guano_wealth_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guano_wealth_extraction, []).

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
 *   constraint_id: guano_wealth_extraction
 *   human_readable: Guano-based Wealth Extraction in the Chincha Islands
 *   domain: economic/resource_extraction
 *
 * SUMMARY:
 *   The Chincha Islands guano deposits (800 AD – 1400 AD) generated wealth
 *   concentration and labor coercion at unprecedented scale. The structural
 *   constraint involves a resource monopoly (elite control of guano harvest
 *   rights), coercive labor enforcement (tributary obligation and harvester
 *   servitude), and asymmetric wealth distribution (guano export value
 *   captured by elite, labor costs borne by commoners). The constraint's
 *   extractiveness increased over the 600-year cycle as elite demand for
 *   luxury goods and monumental architecture expanded, requiring higher guano
 *   export volumes and correspondingly higher labor extraction. Theater ratio
 *   remained low because the extractive mechanism was direct and undisguised:
 *   elite wealth derived visibly from guano trade, labor obligation was
 *   explicit, and the constraint required continuous enforcement rather than
 *   performative legitimation. The ceremonial redistribution system provided
 *   some legitimation (piton perspective) but was insufficient to mask the
 *   fundamental asymmetry. Collapse around 1400 AD involved both ecological
 *   pressures (guano regeneration limits) and political fragmentation (loss
 *   of enforcement capacity as regional rivals organized), though the
 *   relative weighting remains contested. The constraint exemplifies how
 *   resource monopolies in pre-industrial economies generate pure extraction
 *   (snare) unless offsetting coordination functions (reciprocal obligation,
 *   genuine risk-sharing) are present.
 *
 * KEY AGENTS:
 *   - Chincha Elite: Primary beneficiary (institutional/arbitrage) — controls guano harvest monopoly, captures trade surplus, directs labor through tributary obligations
 *   - Guano Harvesters: Primary victims (powerless/trapped) — coerced labor, no exit option, direct exposure to dangerous extraction conditions
 *   - Tributary Populations: Secondary victims (powerless/trapped) — obligated labor service to support elite and extraction infrastructure, multigenenerational extraction
 *   - Merchant Traders: Secondary actors (moderate/constrained) — access guano trade but subject to elite price monopoly and supply restrictions
 *   - Ceremonial Redistribution System: Institutional performance (institutional/arbitrage) — legitimizes extraction through ritualized wealth distribution that is increasingly theatrical
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing monopoly as ecological necessity rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guano_wealth_extraction, 0.72).
domain_priors:suppression_score(guano_wealth_extraction, 0.68).
domain_priors:theater_ratio(guano_wealth_extraction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guano_wealth_extraction, extractiveness, 0.72).
narrative_ontology:constraint_metric(guano_wealth_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(guano_wealth_extraction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guano_wealth_extraction, snare).
narrative_ontology:human_readable(guano_wealth_extraction, "Guano-based Wealth Extraction in the Chincha Islands").
narrative_ontology:topic_domain(guano_wealth_extraction, "economic/resource_extraction").

domain_priors:requires_active_enforcement(guano_wealth_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guano_wealth_extraction, chincha_elite).
narrative_ontology:constraint_victim(guano_wealth_extraction, guano_harvesters).
narrative_ontology:constraint_victim(guano_wealth_extraction, tributary_populations).
narrative_ontology:constraint_victim(guano_wealth_extraction, ecological_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUANO HARVESTER (SNARE) — Trapped in labor obligation to extract guano under elite control. No exit option: flight incurs severe punishment. Bearing full extraction cost through labor coercion. Suppression is high: alternatives (migration, refusal) are blocked by state enforcement and social sanctions.
constraint_indexing:constraint_classification(guano_wealth_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRIBUTARY POPULATION (SNARE) — Obligated to supply labor and resources to support guano extraction infrastructure. Trapped by tributary obligation encoded in social hierarchy. Suppression: refusal means loss of status, resource access, and community inclusion. Multigenational extraction burden.
constraint_indexing:constraint_classification(guano_wealth_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINCHA ELITE (ROPE) — Organizes labor and guano trade. Experiences constraint as coordination mechanism: managing harvest cycles, maintaining trade networks, distributing extraction benefits. Has arbitrage options (trade partnerships, ceremonial exchange). Net beneficiary experiencing the constraint as coordination.
constraint_indexing:constraint_classification(guano_wealth_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT TRADER (TANGLED ROPE) — Benefits from guano trade access but constrained by Chincha elite monopoly on harvest. Can arbitrage within constraints but cannot fully exit. Experiences both coordination (access to unique commodity) and extraction (restricted pricing power). Moderate agency but significant overhead costs.
constraint_indexing:constraint_classification(guano_wealth_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CEREMONIAL REDISTRIBUTION SYSTEM (PITON) — Formal mechanism for distributing guano wealth appears functional but increasingly theatrical. Ceremonial feasts and redistribution rituals legitimize extraction as reciprocal obligation. Theater ratio: redistribution ceremonies are performative; actual wealth concentration in elite hands continues. Coordination function (legitimation) persists through ritual maintenance despite diminishing actual wealth sharing.
constraint_indexing:constraint_classification(guano_wealth_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECOLOGICAL CONSTRAINT VIEW (MOUNTAIN) — From universal analytical perspective, the guano island ecosystem's finite regeneration capacity is an immutable natural law. Guano accumulates at fixed biological rate; harvest cannot exceed regeneration without collapse. This creates absolute scarcity regardless of social organization. However, the structural data reveals this is a false summit: the Chincha managed sustainable harvest for 600 years, proving the ecological limit is not what drives extraction severity — the institutionalized monopoly and coercive labor system are contingent, not natural.
constraint_indexing:constraint_classification(guano_wealth_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guano_wealth_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guano_wealth_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(guano_wealth_extraction, TR),
    TR >= 0.70.

:- end_tests(guano_wealth_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High. The Chincha elite captured guano export surplus (commodity value minus extraction costs) while transferring all labor and risk to coerced populations. The progression from 0.50 to 0.72 reflects expansion of extraction intensity as elite demand outpaced guano availability, requiring intensification of labor coercion. Suppression (0.68): High. Harvesters face coercive labor obligation with capital punishment for flight or refusal. Tributaries face loss of status, resource access, and social inclusion for non-compliance. Alternative livelihoods are blocked. Theater ratio (0.35): Low. The extractive mechanism is direct and undisguised: guano trade wealth flows to elite visibly, labor obligation is encoded in explicit tributary status, enforcement is physical rather than performative. This low theater distinguishes it from piton constraints, which use ritual to mask declining function. Mandatrophy resolved: Yes. The constraint is classified as snare (pure extraction) rather than tangled_rope because the coordination benefits to exploited populations are minimal. The tributary system provides some reciprocal redistribution (feast participation, elite protection), but this is insufficient to offset the coercive labor surplus. The analysis confirms snare: suppression ≥ 0.60, extractiveness ≥ 0.46, χ ≥ 0.66 (estimated 0.78 at powerless perspective).
 *
 * PERSPECTIVAL GAP:
 *   The Chincha elite experience the constraint as coordination (rope) — managing trade networks, organizing harvest cycles, maintaining tributary relationships. The harvesters and tributaries experience it as pure extraction (snare) — uncompensated labor obligation, resource seizure, suppressed alternatives. The merchant traders experience mixed coordination and extraction (tangled_rope) — access to valuable commodity but restricted by monopoly pricing. The ceremonial system preserves a fiction of reciprocity (piton) — redistribution rituals persist while wealth concentration increases. The analytical observer risks naturalizing this as ecological necessity (mountain) — 'finite guano requires elite coordination to prevent overharvest' — but the structural evidence reveals the ecological constraint as stable and the social extraction as expanding. The perspectival gap is sharp: from the elite view, the system coordinates sustainable resource use; from the victim view, the system enforces coercive labor extraction that happens to be sustainable as a side effect of elite wealth maximization (not by design).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality of each perspective derives from structural position in the extraction flow. Harvesters have d ≈ 0.95 (full targets): powerless + trapped + victim → high f(d) → high χ. Tributaries have d ≈ 0.92 (near-full targets): powerless + trapped + victim + multigenenerational → high χ. Chincha elite have d ≈ 0.05 (full beneficiaries): institutional + arbitrage + beneficiary → negative f(d) → negative χ (experienced as coordination benefit). Merchant traders have d ≈ 0.50 (symmetric): moderate power + constrained exit + mixed beneficiary/victim → χ moderate. The ceremonial system has d ≈ 0.15 (beneficiary with institutional inertia): performs legitimation function for elite. Analytical observer has d ≈ 0.73 (observer position): sees structural pattern from outside, risks false summit (naturalizing contingency as necessity).
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecological_carrying_capacity_knowledge,
    'Did the Chincha elite possess explicit knowledge of guano regeneration rates and intentionally limit harvest to sustainable levels, or did sustainable practice emerge from cultural tradition without explicit ecological science?',
    'Archaeological analysis of harvest records, guano deposit stratigraphy, isotope signatures in midden deposits, ethnohistorical accounts of harvest management protocols',
    'If intentional knowledge: constraint includes a genuine coordination function (sustainable resource management) alongside extraction. If traditional practice: coordination emerges from cultural ratcheting rather than explicit constraint design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecological_carrying_capacity_knowledge, empirical, 'Whether ecological carrying capacity was explicitly known and managed').

omega_variable(
    harvester_surplus_extraction_boundary,
    'What portion of harvesters'' labor surplus was extracted as pure rent vs. necessary cost for guano extraction infrastructure (boats, processing, storage)?',
    'Comparative analysis of labor time required for physical extraction vs. labor time demanded; calculation of elite consumption rates vs. infrastructure maintenance costs',
    'If infrastructure overhead is high: extractiveness should be lower (0.55-0.60). If pure rent dominates: extractiveness confirmed at 0.72. Classification sensitivity: impacts whether this is snare or tangled_rope from harvester perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harvester_surplus_extraction_boundary, empirical, 'Proportion of labor extraction that is pure rent vs. infrastructure overhead').

omega_variable(
    tributary_reciprocal_benefit_measurement,
    'What proportion of tributary populations perceived genuine reciprocal benefit from elite protection, ceremonial redistribution, and social stability vs. perceiving pure extraction?',
    'Ethnohistorical analysis of tribute narratives; oral tradition preservation; settlement pattern analysis showing elite and tributary settlement synchrony/conflict periods',
    'If beneficiaries experienced reciprocity: snare classification for tributaries is overstated; should be tangled_rope. If pure extraction: snare classification confirmed from tributary perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tributary_reciprocal_benefit_measurement, conceptual, 'Whether tributaries perceived reciprocal benefit or pure extraction').

omega_variable(
    collapse_causation_extractive_vs_ecological,
    'Did the Chincha system collapse around 1400 AD primarily due to ecological exhaustion (guano regeneration could not support expanding extraction demand) or due to political fragmentation and loss of labor enforcement capacity?',
    'Guano deposit analysis for signs of post-1350 overharvesting; settlement archaeology showing elite residence abandonment vs. harvester settlement persistence; proxy records of climate change affecting marine bird productivity',
    'If ecological: validates mountain perspective (ecological limit is binding constraint). If political: reveals extraction dynamics were contingent institutional arrangements, not natural limits. Mandatrophy resolution hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_causation_extractive_vs_ecological, empirical, 'Whether collapse was driven by ecological exhaustion or political fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guano_wealth_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(guano_tr_t0, guano_wealth_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(guano_tr_t3, guano_wealth_extraction, theater_ratio, 3, 0.3).
narrative_ontology:measurement(guano_tr_t6, guano_wealth_extraction, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(guano_be_t0, guano_wealth_extraction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(guano_be_t3, guano_wealth_extraction, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(guano_be_t6, guano_wealth_extraction, base_extractiveness, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guano_wealth_extraction, resource_allocation).
narrative_ontology:affects_constraint(guano_wealth_extraction, tributary_labor_obligation_system).
narrative_ontology:affects_constraint(guano_wealth_extraction, maritime_trade_monopoly).

% DUAL FORMULATION NOTE:
% The guano extraction system decomposes into three structurally distinct constraints: (1) ecological regeneration limit (mountain), (2) tributary labor mobilization (snare), (3) merchant trader monopoly (tangled_rope). This story focuses on the labor extraction mechanism. The ecological constraint is upstream (affects all three). The merchant monopoly is downstream (enabled by labor extraction infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guano_wealth_extraction, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
