% ============================================================================
% CONSTRAINT STORY: sotu_1945_truman_unconditional_surrender_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1945_truman_unconditional_surrender_doctrine, []).

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
 *   constraint_id: sotu_1945_truman_unconditional_surrender_doctrine
 *   human_readable: Unconditional Surrender Doctrine (1945 Truman Administration)
 *   domain: military/geopolitical
 *
 * SUMMARY:
 *   The unconditional surrender doctrine declared by the Truman
 *   administration in early 1945 establishes that the United States will
 *   accept nothing less than complete capitulation from Germany and Japan,
 *   rejecting any negotiated peace settlement or partial victory conditions.
 *   This constraint binds American military strategy, eliminates diplomatic
 *   negotiation as a war-termination pathway, and commits the nation to total
 *   resource expenditure until enemy forces are completely destroyed or
 *   surrender unconditionally. The constraint operates across multiple
 *   structural dimensions simultaneously: it solves the alliance coordination
 *   problem (preventing separate peace agreements that would fracture the
 *   anti-fascist coalition), it enables the Soviet Union's strategic
 *   expansion by eliminating intermediate settlement options, it extends
 *   warfare and increases casualty rates for civilians and soldiers, and it
 *   degrades the State Department's traditional diplomatic function. The
 *   extractiveness trajectory (0.35 → 0.62) reflects increasing human costs
 *   as the doctrine forces continuation of total war through 1945, while
 *   theater ratio remains low (0.40-0.45) because the constraint is
 *   operationally enforced rather than performative — it directly drives
 *   military strategy and resource allocation rather than masking
 *   non-function.
 *
 * KEY AGENTS:
 *   - Truman Administration: Primary enforcer (institutional/arbitrage) — holds discretionary power to declare and maintain the doctrine; benefits from alliance cohesion and military-industrial demand
 *   - Anti-Fascist Alliance (US/UK/USSR): Primary beneficiary (institutional/arbitrage) — doctrine prevents separate peace agreements and ensures coordinated prosecution of total war
 *   - Occupied Civilian Populations: Primary victim (powerless/trapped) — civilians in Germany and Japan have no exit from prolonged warfare; subject to bombing, resource collapse, occupation violence
 *   - Axis Military Personnel: Secondary victim (powerless/trapped) — enlisted soldiers face no exit short of death or unconditional surrender; no negotiation pathway available
 *   - American Military Personnel: Moderate victim (moderate/constrained) — constrained by military discipline; benefits from clear alliance commitment but bears costs of extended combat
 *   - State Department: Institutional casualty (institutional/arbitrage) — traditional negotiation function stripped of relevance; becomes theater institution with no actual authority
 *   - Soviet Union: Strategic beneficiary (institutional/constrained) — benefits from doctrine preventing Western-German separate peace; enabler of Soviet territorial expansion in Eastern Europe
 *   - Anti-War Movements: Organized resistance (organized/constrained) — constrained by state suppression but organized around clear policy target
 *   - Post-War Institution Builders: Transitional actors (organized/constrained) — developing Bretton Woods, UN, occupation frameworks with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1945_truman_unconditional_surrender_doctrine, 0.62).
domain_priors:suppression_score(sotu_1945_truman_unconditional_surrender_doctrine, 0.75).
domain_priors:theater_ratio(sotu_1945_truman_unconditional_surrender_doctrine, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1945_truman_unconditional_surrender_doctrine, extractiveness, 0.62).
narrative_ontology:constraint_metric(sotu_1945_truman_unconditional_surrender_doctrine, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sotu_1945_truman_unconditional_surrender_doctrine, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1945_truman_unconditional_surrender_doctrine, tangled_rope).
narrative_ontology:human_readable(sotu_1945_truman_unconditional_surrender_doctrine, "Unconditional Surrender Doctrine (1945 Truman Administration)").
narrative_ontology:topic_domain(sotu_1945_truman_unconditional_surrender_doctrine, "military/geopolitical").

domain_priors:requires_active_enforcement(sotu_1945_truman_unconditional_surrender_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1945_truman_unconditional_surrender_doctrine, anti_fascist_alliance_cohesion).
narrative_ontology:constraint_beneficiary(sotu_1945_truman_unconditional_surrender_doctrine, american_military_industrial_complex).
narrative_ontology:constraint_beneficiary(sotu_1945_truman_unconditional_surrender_doctrine, soviet_strategic_position).
narrative_ontology:constraint_victim(sotu_1945_truman_unconditional_surrender_doctrine, civilian_populations_occupied_territories).
narrative_ontology:constraint_victim(sotu_1945_truman_unconditional_surrender_doctrine, german_japanese_soldiers).
narrative_ontology:constraint_victim(sotu_1945_truman_unconditional_surrender_doctrine, american_soldiers).
narrative_ontology:constraint_victim(sotu_1945_truman_unconditional_surrender_doctrine, war_termination_negotiations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OCCUPIED CIVILIAN POPULATIONS (SNARE) — No exit available. The unconditional surrender doctrine forces continuation of warfare until total capitulation, eliminating any negotiated settlement that might reduce civilian casualties. Trapped in occupied territories with no voice in surrender negotiations. Maximum extraction: prolonged warfare, resource depletion, displaced populations, bombing campaigns continue until unconditional capitulation achieved. This agent cannot organize, cannot exit, bears maximal costs.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AXIS ENLISTED SOLDIERS (SNARE) — Trapped soldiers face no exit from warfare short of death or unconditional surrender of their nation-state. The doctrine eliminates negotiated surrender or partial peace as options. Career officers might arrange advantageous surrender terms; enlisted personnel have no agency. Suppression is extreme: military discipline, threat of court-martial, ideology, geographic isolation from information about peace negotiations. Bears extraction through extended combat, resource deprivation, high casualty rates.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: AMERICAN MILITARY PERSONNEL (TANGLED ROPE) — Constrained by military discipline and chain of command but not trapped. The doctrine extracts continued combat commitment, extended deployment, high casualty exposure, and emotional costs of total war. However, coordination benefit exists: the clear commitment to unconditional surrender (no negotiated peace threatens morale) provides psychological certainty that allies share commitment. Moderate extraction with genuine coordination function — the constraint both demands sacrifice and ensures all allied forces remain committed.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-FASCIST ALLIANCE COHESION (ROPE) — Primary beneficiary. The unconditional surrender doctrine prevents separate peace agreements that would fracture the alliance. Each member knows no other member can defect through negotiated settlement. This is pure coordination: the constraint solves the collective action problem of preventing bilateral peace agreements that weaken the whole alliance. Benefits flow to all members through increased credibility of alliance commitment. Arbitrage exit: any nation could theoretically pursue separate peace, but the doctrine's enforcement makes this visible betrayal of the alliance. Net beneficiary with high arbitrage capacity.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SOVIET STRATEGIC POSITION (TANGLED ROPE) — The doctrine benefits Stalin by preventing American or British separate peace with Germany (which would leave USSR facing alone). Coordination function: alliance cohesion. But also extraction: unconditional surrender doctrine commits Western allies to total destruction of Germany, eliminating buffer state options, enabling Soviet territorial expansion in Eastern Europe without Western restraint. Mixed: genuine coordination benefit (no Western defection) with asymmetric strategic gain (enables Soviet territorial objectives). Constrained because USSR cannot exit alliance without becoming target.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: AMERICAN MILITARY-INDUSTRIAL COMPLEX (ROPE) — Beneficiary through extended warfare commitment. The unconditional surrender doctrine guarantees continuation of total war economy, sustained defense spending, full mobilization of industrial capacity. The constraint enables coordination of war production across all manufacturing sectors. Arbitrage capacity: defense contractors could theoretically lobby for negotiated peace to end wartime price controls and rationing; instead they benefit from the doctrine's guarantee of continued demand. Net beneficiary.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DIPLOMATIC INSTITUTION (PITON) — The State Department's traditional function (negotiating settlements, crafting compromise peace terms) is stripped of relevance by the unconditional surrender doctrine. Diplomacy becomes theater: military victory conditions are set unilaterally, negotiation space is eliminated. State Department persists through inertia and residual legitimacy but its core coordination function is degraded. Theater ratio reflects performative diplomatic corps with no actual negotiating authority. Sees its own institutional role as atrophied.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANTI-WAR MOVEMENTS (TANGLED ROPE) — Organized resistance groups see both coordination and extraction. Constraint prevents negotiated early peace, extending suffering. But constraint also enables organization: the clarity of the doctrine creates a focal point for anti-war organizing, and generational time horizons allow movements to emerge (conscription resistance, pacifist organizing). Constrained by state suppression of dissent but not trapped. Experience asymmetric extraction (denied negotiation pathway) with some coordination benefit (clear enemy target for advocacy).
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: POST-WAR INSTITUTION BUILDERS (SCAFFOLD) — Organized actors developing post-war frameworks (Bretton Woods, United Nations, occupation governance) see the unconditional surrender doctrine as temporary: the doctrine is necessary to ensure alliance cohesion during active warfare, but post-war institutions will replace coercive enforcement with institutional coordination. Sunset logic: once unconditional surrender is achieved and occupation begins, the harsh doctrine can transition to negotiated governance frameworks. Constrained (cannot exit wartime alliance) but with visibility to sunset path through institution-building. Theater ratio moderate: planning appears performative during active warfare but becomes operative during occupation phase.
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, total war against fascism could be framed as immutable: the existential threat of fascist ideology makes compromise impossible, total victory is the only logical outcome, negotiated settlement is structurally impossible. This perspective risks naturalizing a contingent policy choice as inherent to the conflict. The engine identifies this as a false summit: the unconditional surrender doctrine is an institutional choice made by Truman's administration, not a law of nature. Alternative paths existed (negotiated territorial settlement, conditional surrender with reparations, limited occupation).
constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1945_truman_unconditional_surrender_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1945_truman_unconditional_surrender_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1945_truman_unconditional_surrender_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1945_truman_unconditional_surrender_doctrine, TR),
    TR >= 0.70.

:- end_tests(sotu_1945_truman_unconditional_surrender_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The doctrine forces continuation of warfare until unconditional capitulation, eliminating negotiation as exit route. This creates sustained extraction from multiple victim groups. However, extractiveness is not maximal (0.70+) because the constraint has genuine coordination function — it prevents alliance fracture, which would actually make the war longer and deadlier. The constraint extracts but also delivers coordination value. Suppression (0.75): High. Suppression mechanisms include military discipline, prohibition of negotiation discussions, state control of information about potential peace terms, threat of court-martial for defection, and geographic/institutional barriers to exit from wartime mobilization. Suppression is not absolute (trapped agents might still attempt desertion or communicate with enemy) but substantial. Theater ratio (0.45): Moderate-low. The constraint is operationally enforced through actual military strategy, not theatrical performance. The doctrine directly determines war-termination conditions and resource allocation. Theater is not minimal because diplomatic theater (peace proposals, negotiations) persists as performative background even while decision-making is unilateral, and post-war planning may appear performative during active warfare. Claimed type (Tangled Rope): The constraint exhibits both coordination (preventing alliance fracture) and asymmetric extraction (extended warfare, civilian casualties, suppressed negotiation). The coordination function is genuine — alliance cohesion requires preventing separate peace agreements. The extraction is real — the doctrine forces casualties and suppresses exit routes. Both dimensions are present and structurally necessary, meeting tangled rope requirements.
 *
 * PERSPECTIVAL GAP:
 *   The unconditional surrender doctrine demonstrates maximum perspectival divergence from the same base properties. The anti-fascist alliance sees coordination (Rope) — the doctrine ensures no member can defect through separate peace. Occupied civilians see pure extraction (Snare) — no agency, no exit, prolonged warfare. American soldiers see mixed coordination and extraction (Tangled Rope) — constrained by military discipline but also reassured by alliance commitment to total victory. The Soviet Union sees strategic gain wrapped in coordination (Tangled Rope) — the doctrine benefits USSR geopolitically while providing alliance cohesion. The State Department sees degraded institutional function (Piton) — diplomacy persists as theater with no actual negotiating authority. The military-industrial complex sees pure coordination (Rope) — guaranteed sustained demand and production. Post-war planners see sunset logic (Scaffold) — the harsh doctrine is temporary wartime necessity that will transition to institutional governance once victory is achieved. The analytical observer risks seeing natural law (Mountain) — total war against fascism as inherently requiring unconditional surrender — but the constraint is contingent institutional choice, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to extraction flow. Occupied civilians (d=0.95): trapped victims with no exit, bearing maximum costs — high f(d) → high chi. Axis soldiers (d=0.90): powerless trapped agents with no negotiation pathway, sustaining extraction through military discipline — very high f(d) → very high chi. American soldiers (d=0.60): moderate agents with constrained exit (military discipline, career) but coordination benefit from alliance commitment — moderate f(d) → moderate chi. Anti-fascist alliance members (d=0.15): institutional beneficiaries with arbitrage capacity (could theoretically pursue separate peace but constraint prevents this strategically) — low f(d) → low/negative chi (coordination benefit). State Department (d=0.40): institutional actor with degraded function but still maintaining arbitrage capacity through advisory role — moderate f(d). Soviet Union (d=0.35): institutional beneficiary with constrained exit (cannot abandon alliance without becoming target) but strategic gain from doctrine preventing Western-German separate peace — moderately beneficial but constrained. Directionality overrides are not necessary because the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE TYRRANNY RESOLUTION: The unconditional surrender doctrine's extractiveness (0.62) exceeds the snare threshold (0.46) because the constraint combines asymmetric extraction (civilian casualties, suppressed negotiation) with genuine coordination function (preventing alliance fracture). The mandate tyranny — the tension between 'this constraint extracts from victims' and 'this constraint coordinates alliance' — is resolved by classifying as TANGLED ROPE rather than SNARE. A snare classification would foreclose the alliance-cohesion analysis; a tangled rope classification honors both dimensions. The false summit risk is real: the analytical observer could naturalize the doctrine as inherent to total war against fascism, masking the contingent institutional choice. However, the constraint's extractiveness (high civilian costs) and the existence of identifiable beneficiaries (alliance members, military-industrial complex) prevent the false summit from standing — the engine detects the naturalization as artificial. The constraint is not a law of nature; it is a policy choice with real beneficiaries and real victims. The mandatrophy is resolved by refusing the false choice between 'pure extraction' and 'pure coordination' and accepting the constraint as genuinely hybrid — it solves a coordination problem AND it creates asymmetric suffering. Both realities hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_termination_counterfactual,
    'Would negotiated settlement have been possible if the unconditional surrender doctrine had not been declared?',
    'Historical counterfactual analysis comparing actual German/Japanese surrender conditions with hypothetical terms available through negotiation; examination of German and Japanese internal documents indicating surrender thresholds; assessment of whether conditional surrender terms would have been accepted by any Axis faction',
    'If negotiated settlement was possible: doctrine is extractive choice that extended war. If genuine no-negotiation barriers existed: doctrine is coordination mechanism expressing pre-existing reality rather than creating new extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_termination_counterfactual, conceptual, 'Whether negotiated settlement was feasible absent the unconditional surrender doctrine').

omega_variable(
    civilian_casualty_causation,
    'What proportion of civilian casualties in final war phase resulted from continuation of warfare under unconditional surrender doctrine versus other factors (strategic bombing, occupation, resource collapse)?',
    'Demographic analysis of civilian deaths in 1944-1945 period; comparison with projected casualties under hypothetical early-negotiated settlement scenarios; accounting for bombing, starvation, occupation violence, and Nazi genocide continuation',
    'If high proportion attributable to doctrine: suppression metric should be higher (0.85+). If low proportion: doctrine''s human cost is lower than base assessment (0.75 suppression may be overstated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_casualty_causation, empirical, 'Causal attribution of civilian casualties to unconditional surrender doctrine').

omega_variable(
    alliance_cohesion_mechanism,
    'Did the unconditional surrender doctrine meaningfully increase alliance cohesion, or would the alliance have remained unified without this explicit commitment?',
    'Historical analysis of alliance tensions with and without the doctrine; examination of separate peace negotiations or defection risks in 1943-1945; comparison with WWI coalition breakdown mechanisms',
    'If doctrine was necessary for cohesion: primary coordination function is confirmed. If alliance was stable without doctrine: constraint is extractive theater rather than genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_cohesion_mechanism, empirical, 'Whether unconditional surrender doctrine was necessary for anti-fascist alliance cohesion').

omega_variable(
    axis_surrender_incentive_structure,
    'Did the unconditional surrender doctrine create perverse incentives for Axis forces to resist longer, or would resistance have been equivalent under conditional surrender terms?',
    'Analysis of German and Japanese military planning documents; comparison of actual surrender timing with projections under hypothetical conditional surrender scenarios; assessment of officer corps calculation of personal fate under unconditional versus conditional terms',
    'If doctrine increased resistance incentives: it prolonged war through strategic miscalculation (increases extraction costs on all parties). If resistance would have been equivalent: doctrine did not change incentive structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axis_surrender_incentive_structure, empirical, 'Whether unconditional surrender doctrine affected Axis resistance duration').

omega_variable(
    false_summit_institutional_choice,
    'Is the unconditional surrender doctrine a natural law of total war against existential threat, or a contingent institutional choice by Truman administration?',
    'Historical documentation of decision-making process; identification of alternative paths that were considered and rejected; examination of precedents in previous wars; assessment of whether choice was determined by prior conditions or constituted genuine discretionary choice',
    'If natural law: mountain classification is appropriate and constraint is unavoidable. If institutional choice: false summit detection applies — the constraint is contingent and the naturalizing framing masks extractive dimensions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_institutional_choice, conceptual, 'Whether unconditional surrender doctrine is natural law or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1945_truman_unconditional_surrender_doctrine, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1945_truman_unconditional_surrender_doctrine, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sotu_tr_t6, sotu_1945_truman_unconditional_surrender_doctrine, theater_ratio, 6, 0.42).
narrative_ontology:measurement(sotu_tr_t12, sotu_1945_truman_unconditional_surrender_doctrine, theater_ratio, 12, 0.45).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1945_truman_unconditional_surrender_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t6, sotu_1945_truman_unconditional_surrender_doctrine, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(sotu_be_t12, sotu_1945_truman_unconditional_surrender_doctrine, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1945_truman_unconditional_surrender_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1945_truman_unconditional_surrender_doctrine, total_war_commitment).
narrative_ontology:affects_constraint(sotu_1945_truman_unconditional_surrender_doctrine, allied_coalition_fragmentation_risk).
narrative_ontology:affects_constraint(sotu_1945_truman_unconditional_surrender_doctrine, strategic_bombing_escalation).
narrative_ontology:affects_constraint(sotu_1945_truman_unconditional_surrender_doctrine, soviet_territorial_expansion).

% DUAL FORMULATION NOTE:
% The unconditional surrender doctrine constrains multiple downstream constraints. The commitment to total war enforcement (affects total_war_commitment), prevents alliance fracture by eliminating separate peace options (affects allied_coalition_fragmentation_risk), enables escalation of strategic bombing by removing negotiation ceiling (affects strategic_bombing_escalation), and enables Soviet territorial expansion in Eastern Europe by preventing intermediate German settlement (affects soviet_territorial_expansion). Each downstream constraint has its own extractiveness and classifications but shares structural dependency on the unconditional surrender doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
