% ============================================================================
% CONSTRAINT STORY: trajans_bridge_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trajans_bridge_lifecycle, []).

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
 *   constraint_id: trajans_bridge_lifecycle
 *   human_readable: Trajan's Bridge over the Danube (Lifecycle)
 *   domain: technological/military/geopolitical
 *
 * SUMMARY:
 *   Trajan's Bridge over the Danube (completed circa 105 AD) was a monumental
 *   Roman engineering achievement: a stone and masonry bridge spanning 1,135
 *   meters, standing 15 meters above water, designed to facilitate the
 *   conquest and administration of Dacia. The bridge represents a constraint
 *   with deeply asymmetric structural relationships. From the perspective of
 *   enslaved construction laborers and conquered Dacian populations, it was
 *   pure extraction — a mechanism of coercive dominion with no exit option
 *   and high human cost. From the perspective of the Roman military command
 *   and treasury, it was a coordination solution enabling supply line
 *   optimization and strategic advantage. From the perspective of frontier
 *   administrators, particularly after Hadrian's withdrawal from Dacia (271
 *   AD), it became increasingly performative — a monument to Roman reach
 *   maintained through institutional inertia long after its strategic
 *   function had degraded. The constraint's lifecycle shows extraction
 *   intensity declining over time (as the military objective was achieved and
 *   the technological advantage was superseded) while theater ratio rose (as
 *   the bridge shifted from functional to symbolic). This progression traces
 *   a classic snare-to-piton degradation: from high-extraction tool of
 *   conquest to dead institutional weight.
 *
 * KEY AGENTS:
 *   - Enslaved Bridge Laborers: Primary victims (powerless/trapped) — compelled construction workers with 40-60% mortality rate; no exit option
 *   - Dacian Population: Secondary victims (moderate/trapped) — conquered via the bridge's logistical advantage; confined to subjugation for generations
 *   - Roman Military Command: Primary beneficiary (institutional/arbitrage) — gains conquest capability and long-term strategic dominion over Dacia
 *   - Roman Treasury: Secondary beneficiary (institutional/arbitrage) — captures tax revenue and tribute from Dacia after conquest
 *   - Roman Military-Engineering Complex: Organized coordinator (organized/constrained) — solves genuine logistical problem of river crossing during active campaign
 *   - Frontier Administrative Apparatus: Institutional actor (institutional/analytical) — maintains bridge infrastructure post-conquest; transitions to performative role after Hadrian's withdrawal
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the bridge as an immutable engineering constraint rather than a political and extractive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trajans_bridge_lifecycle, 0.58).
domain_priors:suppression_score(trajans_bridge_lifecycle, 0.72).
domain_priors:theater_ratio(trajans_bridge_lifecycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trajans_bridge_lifecycle, snare).
narrative_ontology:human_readable(trajans_bridge_lifecycle, "Trajan's Bridge over the Danube (Lifecycle)").
narrative_ontology:topic_domain(trajans_bridge_lifecycle, "technological/military/geopolitical").

domain_priors:requires_active_enforcement(trajans_bridge_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, roman_military_command).
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, roman_treasury).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, enslaved_construction_labor).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, dacian_population).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, frontier_stability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED BRIDGE LABORERS (SNARE) — Compelled to construct the bridge under military command with no exit option. High mortality (estimated 40-60% during construction), no choice of participation, no negotiation capacity. Trapped extraction. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.74.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DACIAN POPULATION POST-CONQUEST (SNARE) — The bridge enables Roman military supply lines that enforce Dacian subjugation. Exit option is death or deportation. The constraint persists across generations: the bridge remains a symbol of Roman dominion. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROMAN MILITARY-ENGINEERING COMPLEX (TANGLED ROPE) — The bridge solves a genuine coordination problem: how to supply military forces across a strategic river during active conquest. The engineering coordination (across 4,000+ workers, 50,000 soldiers) is real; the bridge also enforces extraction from subjugated populations. Mixed coordination + asymmetric extraction. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: ROMAN STATE / MILITARY OBJECTIVE (ROPE) — The bridge is a coordination solution for the state's conquest and governance goals. Massive logistical value: reduces crossing time from days to hours, enables supply line depth, supports permanent garrison deployment. The state experiences this as pure coordination with minimal coercion overhead — the military structure itself is consensual for Roman citizens. d≈0.15, f(d)≈-0.01, σ=1.1 → χ≈-0.01. Net beneficiary; constraint experienced as coordination.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: FRONTIER ADMINISTRATIVE APPARATUS (PITON) — After Hadrian's withdrawal from Dacia (271 AD), the bridge's primary function (conquest supply) becomes inert. However, the administrative structure supporting the bridge persists through institutional momentum: garrison costs, maintenance rituals, symbolic reporting — all maintained despite the loss of the original extractive purpose. theater_ratio≈0.65 reflects that post-Hadrian, most bridge-related activity is performative. The bridge becomes a symbol of Roman reach rather than a functional supply mechanism.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ENGINEERING LIMITS (MOUNTAIN) — From a purely technical standpoint, the bridge represented an immutable engineering constraint: no earlier Roman bridge crossed a river of comparable width (1,135 meters) under such military pressure. The stone-and-masonry technology had hard limits on span, foundation depth, and construction speed. However, the structural data (ε=0.58, suppression=0.72, theater=0.65) contradicts pure mountain classification. The 'engineering limit' framing naturalizes what was actually a political and extractive choice — the bridge could have been smaller, temporary, or never built. The analytical observer risks a false summit.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trajans_bridge_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trajans_bridge_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trajans_bridge_lifecycle, TR),
    TR >= 0.70.

:- end_tests(trajans_bridge_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, declining over time. At the bridge's inception (105 AD), extractiveness was very high (≈0.78) because the primary function was enabling conquest and subjugation — pure military extraction from Dacian populations. By 170 AD, after Hadrian's withdrawal (131 AD), the extractiveness declined to 0.58 because the bridge's primary extraction function (facilitating conquest) was no longer active. Residual extraction from maintenance burden and symbolic dominion persisted but was less intense. Suppression (0.72): High and sustained. The bridge's construction and maintenance relied on coercive labor supply (enslaved workers initially, then provincial levy) with no legitimate market alternative available at the required scale and speed. Frontier communities bore suppressed exit options from the administrative apparatus the bridge enabled. Theater ratio (0.65): High and rising. Initially (≈0.25), the bridge was functionally intensive — actual logistical value was high relative to performative value. By 170 AD, after the bridge's strategic role had been superseded by other frontier controls and Hadrian's retreat, the theater ratio rose to 0.65 as maintenance and garrison administration became largely performative. The bridge persisted through institutional inertia and symbolic significance, not because it was functionally necessary.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence reflects the fundamental asymmetry of the constraint. Enslaved laborers and Dacian populations perceive the bridge as pure extraction (Snare) with no coordination benefit — they bear costs and gain nothing. The Roman military command perceives the bridge as pure coordination (Rope) — a solution to the logistical problem of river crossing with minimal coercion overhead from their perspective (the military structure was consensual for Roman citizens). The organized frontier administration perceives a mixed system (Tangled Rope) that both enables supply chain coordination and enforces extraction. Post-Hadrian withdrawal, the frontier apparatus perceives the bridge as a degraded ritual (Piton) — persisting through momentum even though its functional purpose is obsolete. The analytical observer risks naturalizing the constraint as an engineering necessity (Mountain) when it was actually a political and extractive choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Enslaved laborers: Victims + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. No benefits, maximum coercion. Dacian population: Victims + trapped → d≈0.92, f(d)≈1.40. High extraction; the bridge enables dominion they cannot escape. Roman military command: Beneficiaries + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiaries; they experience the constraint as coordination, not extraction. Frontier apparatus: Institutional + constrained → d≈0.50, f(d)≈0.65 (piton classification comes from theater gate, not chi derivation). Mixed actor, mixed exit capacity. The organizational structure around the bridge constrains innovation (cannot easily replace or remove the bridge) while providing career/status benefits to administrators.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA LIFECYCLE DECOMPOSITION: The constraint resolves mandatrophy by distinguishing phases. The early phase (105-131 AD) is unambiguously a Snare: high extractiveness (0.78), high suppression (0.72), driven by active conquest and coercive labor. The later phase (after 131 AD withdrawal) shows degradation into Piton: extractiveness drops to 0.58, theater rises to 0.65, the bridge's functional purpose has atrophied. The apparent contradiction between 'pure extraction' (snare interpretation) and 'engineering necessity' (mountain interpretation) resolves by noting that extractiveness itself declined as the extraction function was accomplished. The bridge was a snare during its operational phase (conquest) and a piton after its function was superseded. Neither 'the bridge is an eternal snare' nor 'the bridge is an immutable law' is correct — both are phase-specific readings. The mandatrophy is resolved by time-indexing the classification: same structure, different extractiveness at different lifecycle stages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_alternative,
    'Could the bridge have been constructed with voluntary labor at market rates rather than enslavement and coercion?',
    'Historical wage data for Roman construction projects; comparative labor costs for bridges built with free vs enslaved labor; analysis of whether the timeline and budget required coercion',
    'If market labor was economically viable: the snare classification for laborers becomes a choice (extraction intensified beyond necessity), reducing the constraint''s natural-law framing. If market labor was economically infeasible: the snare classification reflects genuine scarcity constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_alternative, empirical, 'Whether voluntary labor could have replaced coerced labor').

omega_variable(
    dacian_exit_capacity,
    'Did Dacian populations have genuine alternatives to subjugation after the bridge was built, or was subjugation inevitable regardless?',
    'Analysis of Dacian military capacity pre- and post-bridge; counterfactual: would Roman forces have conquered Dacia without the bridge''s logistical advantage?',
    'If Dacian exit was viable: the bridge is a snare that eliminated exit (high d). If conquest was inevitable: the bridge merely accelerated subjugation already determined by military imbalance (lower d, constraint becomes more rope-like).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dacian_exit_capacity, empirical, 'Whether Dacian military alternatives existed post-bridge').

omega_variable(
    hadrian_withdrawal_causality,
    'Did Hadrian withdraw from Dacia primarily due to the bridge becoming unsustainable, or for other strategic reasons (Parthian conflict, frontier consolidation)?',
    'Historical sources on Hadrian''s motivation; analysis of whether the bridge maintenance costs and supply requirements drove the decision',
    'If bridge costs drove withdrawal: the piton classification (degraded institutional persistence) is temporary — the constraint''s extinction reflects functional failure. If other factors: the piton classification is more accurate — the bridge persists as dead institutional weight despite functional redundancy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hadrian_withdrawal_causality, empirical, 'Causality of Hadrian''s withdrawal from Dacia').

omega_variable(
    supply_line_necessity,
    'How much of the bridge''s extractive force depended on its actual supply function versus its symbolic dominion function?',
    'Analysis of supply logistics: tonnage capacity, actual supplies delivered, garrison size supported. Comparison with alternative supply routes (river transport, land routes); assessment of redundancy and necessity.',
    'If supply function was dominant: the tangled rope classification (coordination + extraction) is accurate. If symbolic function dominated: the constraint is primarily extraction (snare) with a coordination veneer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_line_necessity, empirical, 'Balance between supply logistics and symbolic dominion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trajans_bridge_lifecycle, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trajan_tr_t0, trajans_bridge_lifecycle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trajan_tr_t50, trajans_bridge_lifecycle, theater_ratio, 50, 0.48).
narrative_ontology:measurement(trajan_tr_t170, trajans_bridge_lifecycle, theater_ratio, 170, 0.65).

% Extraction over time
narrative_ontology:measurement(trajan_be_t0, trajans_bridge_lifecycle, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(trajan_be_t50, trajans_bridge_lifecycle, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(trajan_be_t170, trajans_bridge_lifecycle, base_extractiveness, 170, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trajans_bridge_lifecycle, enforcement_mechanism).
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, dacian_military_capacity).
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, roman_frontier_infrastructure).
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, enslaved_labor_supply_chain).

% DUAL FORMULATION NOTE:
% The bridge constraint decomposes into two analytically distinct claims: (1) the engineering challenge (immutable physical/mathematical limits on span and foundation), which approaches mountain classification but is orthogonal to the human constraint; (2) the military-extractive system the bridge enabled, which is the actual constraint in the Deferential Realism sense. The latter is what the JSON captures (ε=0.58, snare/piton). The former is not modeled here because it is not a constraint in the DR sense — it has no beneficiaries or victims, only technical parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
