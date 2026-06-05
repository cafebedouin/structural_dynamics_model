% ============================================================================
% CONSTRAINT STORY: french_electoral_bifurcation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_electoral_bifurcation, []).

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
 *   constraint_id: french_electoral_bifurcation
 *   human_readable: French Electoral Bifurcation: Far-Right Polarization and Institutional Extraction
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   The French electoral bifurcation describes the structural polarization of
 *   the electorate into two competing coalitions (centrist-left vs
 *   centrist-right) under the fifth republic's two-round runoff system. This
 *   arrangement exhibits genuine coordination benefits — it prevents
 *   fragmentation into six or more non-viable parties and forces
 *   coalition-building during campaigns — while simultaneously extracting
 *   disproportionate costs from the moderate left and progressive
 *   constituencies. The constraint intensified as far-right electoral
 *   performance improved, forcing left voters into increasingly defensive
 *   voting positions and granting centrist gatekeepers maximum kingmaker
 *   leverage. The theater ratio has risen as round-two contests have become
 *   increasingly predictable binary choices with minimal strategic variation,
 *   reducing the functional verification work of the electoral process
 *   itself. The bifurcation is a tangled rope from the analytical view: it
 *   coordinates coalition formation (genuine benefit) while systematically
 *   advantaging centrist and far-right positions at the expense of
 *   progressive alternatives (asymmetric extraction).
 *
 * KEY AGENTS:
 *   - Moderate Left Constituency: Primary victim (powerless/trapped) — forced into defensive coalition voting; no exit from electoral system without political voice abandonment
 *   - Progressive Political Movements: Secondary victim (moderate/constrained) — structurally dependent on centrist coalition partners; cannot achieve independent majorities
 *   - Centrist Establishment: Primary beneficiary (institutional/arbitrage) — kingmakers in round two; can negotiate from maximum leverage position
 *   - Far-Right Political Movement: Secondary beneficiary (organized/mobile) — polarization benefits from structural positioning; converts high vote share into structural leverage against fragmented left
 *   - Fifth Republic Electoral System: Institutional actor (institutional/arbitrage) — two-round runoff structure; maintains coordination function while creating extraction asymmetry
 *   - Electoral System Legitimacy: Victim (powerless/trapped) — abstract collective good; loses legitimacy as bifurcation increases voter frustration with binary choice constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_electoral_bifurcation, 0.58).
domain_priors:suppression_score(french_electoral_bifurcation, 0.62).
domain_priors:theater_ratio(french_electoral_bifurcation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_electoral_bifurcation, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_electoral_bifurcation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(french_electoral_bifurcation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_electoral_bifurcation, tangled_rope).
narrative_ontology:human_readable(french_electoral_bifurcation, "French Electoral Bifurcation: Far-Right Polarization and Institutional Extraction").
narrative_ontology:topic_domain(french_electoral_bifurcation, "political/electoral_systems").

domain_priors:requires_active_enforcement(french_electoral_bifurcation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_electoral_bifurcation, far_right_political_movement).
narrative_ontology:constraint_beneficiary(french_electoral_bifurcation, centrist_establishment_gatekeepers).
narrative_ontology:constraint_victim(french_electoral_bifurcation, moderate_left_constituency).
narrative_ontology:constraint_victim(french_electoral_bifurcation, progressive_political_space).
narrative_ontology:constraint_victim(french_electoral_bifurcation, electoral_system_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MODERATE LEFT VOTER (SNARE) — Trapped in a two-round runoff system where voting for preferred left candidate in round one guarantees right-wing victory in round two if centrist establishment refuses coalition. Cannot exit electoral participation without abandoning political voice. Bears full extraction cost: forced to vote against preference (defensive voting) or accept defeat.
constraint_indexing:constraint_classification(french_electoral_bifurcation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE COALITION (TANGLED ROPE) — Constrained by institutional rules (two-round system, constituency geometry) and electoral arithmetic (need centrist votes in round two). Also benefits from coordination mechanism: the runoff system enables coalition-building and prevents pure plurality fragmentation. Mixed experience of extraction and coordination benefit.
constraint_indexing:constraint_classification(french_electoral_bifurcation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRIST ESTABLISHMENT (ROPE) — Experiences the constraint as pure coordination: the bifurcation gives centrists maximum leverage as kingmakers in round two. Can arbitrage their position — negotiate concessions from either left or right. Net beneficiary with maximum mobility. Coordination serves their interests perfectly.
constraint_indexing:constraint_classification(french_electoral_bifurcation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FAR-RIGHT MOVEMENT (SNARE) — Despite organizational capacity and voter base, sees the bifurcation as a snare for their opponents, not themselves. The polarization traps centrists and progressives in reactive positions. Far-right experiences the constraint as maximizing their structural leverage — not extraction, but extraction from others. Organized/mobile actors see snare dynamics differently: they benefit from the polarization trap.
constraint_indexing:constraint_classification(french_electoral_bifurcation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FIFTH REPUBLIC ELECTORAL SYSTEM (PITON) — The two-round runoff system has become increasingly performative. Designed to prevent fragmentation and enable consensus-building, it now functions primarily as a theater of inevitability: round two becomes a binary choice with minimal strategic variance. The system persists through institutional inertia despite critics arguing for proportional representation or alternative runoff designs.
constraint_indexing:constraint_classification(french_electoral_bifurcation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The bifurcation is a genuine structural feature with both coordination benefits (prevents fragmentation into 7+ parties with no majority) and extraction costs (traps centrist-left coalitions, empowers kingmakers). The constraint exhibits mandatory features: beneficiaries (centrist, far-right), victims (left), active enforcement (round-two runoff ballot structure), asymmetric extraction (left concedes more than gains). This is the engine's computed constraint claim.
constraint_indexing:constraint_classification(french_electoral_bifurcation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_electoral_bifurcation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_electoral_bifurcation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_electoral_bifurcation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_electoral_bifurcation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_electoral_bifurcation, TR),
    TR >= 0.70.

:- end_tests(french_electoral_bifurcation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The system extracts significant political concessions from the moderate left during coalition negotiations while providing centrist actors with maximum leverage. The rise from 0.35 to 0.58 over the interval reflects intensifying bifurcation as far-right gains, forcing left actors into more defensive positions. This is not maximum extraction — left still influences policy outcomes through coalition membership — but substantial enough to warrant tangled_rope rather than rope classification. Suppression (0.62): Moderate-high. Structural barriers include the round-one vs round-two arithmetic (vote splitting in round one guarantees defeat in round two), electoral threshold effects from geographic constituency design, and the cultural norm (increasingly entrenched) that coalition formation occurs after round-one results are known. Theater ratio (0.68): High. The second round increasingly functions as a predetermined binary choice. Geographic maps show runoff patterns months in advance; strategic voting calculations dominate over genuine preference expression; the round-two campaign often consists of repetitive inevitability messaging rather than substantive alternative presentation.
 *
 * PERSPECTIVAL GAP:
 *   The bifurcation produces maximum perspectival divergence from identical structural data. Centrists see coordination mechanism that works in their favor (rope). Progressives see coordination coupled with unfair cost distribution (tangled rope). Powerless left voters see constraint with no exit (snare). The far-right sees structural advantage from polarization (snare for opponents, leverage for themselves). The electoral system sees its own function degrading into performative ritual (piton). The analytical observer sees the entire structure: genuine coordination benefit with asymmetric extraction cost distribution (tangled rope, the computed constraint claim). This perspectival range indicates the classification is capturing real structural data rather than imposing a predetermined framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position in the extraction flow. Moderate left voters are victims with trapped exit (d ≈ 0.95), experiencing maximum f(d) amplification. Centrist gatekeepers are beneficiaries with arbitrage exit (d ≈ 0.05), experiencing minimal or negative f(d). Progressive coalitions are mixed — they benefit from coordination but face extraction (d ≈ 0.55), generating moderate f(d). The far-right organized movement benefits from bifurcation without bearing suppression costs (d ≈ 0.35), experiencing moderate benefit rather than extraction. The constraint exhibits the hallmark of tangled rope: beneficiaries would collapse the system absent suppression mechanisms (high suppression at 0.62 maintains the extraction flow), yet the coordination function is genuine (round-two runoff does prevent splintering into dysfunctional multi-party competition).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled_rope classification is confirmed by the presence of all three mandatory features. (1) BENEFICIARIES: centrist establishment gains maximum kingmaker leverage through round-two positioning; far-right gains structural advantage from polarization. (2) VICTIMS: moderate-left constituency forced into defensive voting; progressive political space constrained by coalition dependence. (3) ACTIVE ENFORCEMENT: the two-round ballot structure actively enforces the bifurcation — round-one prevents coalition pre-commitment; round-two creates binary choice. The coordination function is genuine: absent the runoff system, French elections would fragment into 6+ viable parties competing under pure plurality rules, preventing stable coalition formation and clear governance mandates. The extraction cost is real: left constituencies pay that cost through concessions to centrist coalition partners and through strategic voting that suppresses preference expression. Both dimensions are structural, not observational. The mandatrophy resolves: this is not confusion between coordination and extraction — it is a constraint that does both simultaneously, and the extraction asymmetry is not incidental but structural to how the coordination function allocates power. The analytical observer should not be tempted to call it 'just coordination' (rope) because the distribution is fair, nor 'just extraction' (snare) because the coordination benefit is real. It is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    runoff_coalition_formation_inevitability,
    'Is the centrist refusal to pre-commit to coalition in round two structural (rational game-theoretic positioning) or institutional (cultural normalization of ''no alliances before votes cast'')?',
    'Comparative analysis of electoral systems: countries with mandatory pre-electoral coalitions vs optional ones; behavioral game theory experiments on voting strategy under uncertainty',
    'If structural: the bifurcation is an unavoidable feature of two-round systems. If institutional: alternative coordination rules could reduce extraction without changing ballot structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(runoff_coalition_formation_inevitability, conceptual, 'Whether coalition refusal is structural or institutional norm').

omega_variable(
    far_right_floor_ceiling_asymmetry,
    'Does the far-right movement experience an organizational ceiling (inability to convert voter support into coalition leverage) that stabilizes the bifurcation, or is the bifurcation itself what prevents the ceiling?',
    'Counterfactual modeling: proportional representation scenarios with identical voter distribution; analysis of historical shifts in far-right structural position under different electoral rules',
    'If ceiling is independent: bifurcation is contingent institutional feature. If ceiling is created by bifurcation: the electoral system actively constrains far-right consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(far_right_floor_ceiling_asymmetry, empirical, 'Whether far-right experiences independent ceiling or system-created constraint').

omega_variable(
    progressive_fragmentation_counterfactual,
    'Would a proportional representation system reduce left fragmentation or increase it by lowering the coordination cost of splinter movements?',
    'Analysis of multi-party left spaces under PR in Europe (Germany, Italy, Spain); simulation of French party competition under 3-5% PR thresholds; organizational structure analysis of French left parties under different rule sets',
    'If fragmentation increases: the two-round system''s coordination benefit is real (snare is the cost of preventing worse outcomes). If fragmentation decreases: the system artificially sustains left coalition dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_fragmentation_counterfactual, empirical, 'Whether proportional representation would increase or decrease left fragmentation').

omega_variable(
    suppression_mechanism_internalization,
    'Do left voters'' strategic voting patterns reflect genuine external suppression (the runoff arithmetic forces the choice) or internalized suppression (left voters have absorbed the inevitability narrative)?',
    'Post-election surveys on counterfactual preferences and perceived choice constraint; comparison of suppression levels in populations before vs after runoff rule changes; measurement of tactical voting elasticity with respect to rule uncertainty',
    'If external: suppression is structural and would persist under alternative systems. If internalized: suppression diminishes rapidly if rules change and expectations reset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether voting suppression is structural or internalized').

omega_variable(
    mandatrophy_false_summit_danger,
    'Does the analytical observer''s tangled_rope classification risk being a false summit that naturalizes contingent institutional arrangements (fifth republic rules, geographic constituency design, party financing structures)?',
    'Historical analysis of French electoral design choices: which elements were explicitly chosen for partisan advantage vs which emerged accidentally? Comparative legal analysis: how many democracies have adopted identical two-round runoff structures?',
    'If design was contingent: the tangled_rope classification captures real structural data but risks treating artificial arrangements as natural. If design was inevitable: the classification appropriately reflects structural inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_false_summit_danger, conceptual, 'Risk of false summit in analytical classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_electoral_bifurcation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fren_tr_t0, french_electoral_bifurcation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fren_tr_t10, french_electoral_bifurcation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(fren_tr_t20, french_electoral_bifurcation, theater_ratio, 20, 0.68).
narrative_ontology:measurement(fren_tr_t5, french_electoral_bifurcation, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(fren_be_t0, french_electoral_bifurcation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fren_be_t10, french_electoral_bifurcation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fren_be_t20, french_electoral_bifurcation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fren_be_t15, french_electoral_bifurcation, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_electoral_bifurcation, enforcement_mechanism).
narrative_ontology:affects_constraint(french_electoral_bifurcation, french_coalition_formation_norms).
narrative_ontology:affects_constraint(french_electoral_bifurcation, progressive_party_fragmentation).
narrative_ontology:affects_constraint(french_electoral_bifurcation, centrist_institutional_lock).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
