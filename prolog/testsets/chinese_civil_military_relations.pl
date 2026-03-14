% ============================================================================
% CONSTRAINT STORY: chinese_civil_military_relations
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chinese_civil_military_relations, []).

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
 *   constraint_id: chinese_civil_military_relations
 *   human_readable: Chinese Civil-Military Relations: Coordination and Control
 *   domain: political/military/institutional
 *
 * SUMMARY:
 *   Chinese civil-military relations represent the institutional integration
 *   of party political control, state civilian administration, and military
 *   command authority into a unified hierarchy. This constraint exhibits
 *   tangled rope structure—genuine coordination functions coexist with
 *   asymmetric extraction mechanisms. The People's Liberation Army's role is
 *   formally subordinate to Communist Party leadership, yet the military
 *   simultaneously coordinates national defense strategy with civilian
 *   sectors and extracts resources through budgetary priority, dual-use
 *   technology mandates, and personnel control. The constraint's
 *   extractiveness (0.58) reflects that coordination benefits (unified
 *   strategic planning, rapid policy implementation, technological
 *   integration) coexist with extraction mechanisms (resource diversion from
 *   civilian sectors, suppression of military autonomy, conscription labor).
 *   Theater ratio (0.68) indicates significant performative content: the
 *   narrative of civil-military coordination for national development masks
 *   underlying command authority exercised by party leadership over both
 *   civilian and military structures. Over the 20-year interval, both
 *   extractiveness and theater have increased, suggesting that coordination
 *   benefits may be plateauing while performance requirements escalate—a
 *   potential degradation signal toward snare dynamics.
 *
 * KEY AGENTS:
 *   - Communist Party Leadership: Primary beneficiary (institutional/arbitrage) — exercises centralized control, consolidates power, benefits from unified command structure with minimal constraint
 *   - Military High Command: Secondary beneficiary and victim (powerful/constrained) — coordinates on defense strategy and gains resource priority, but experiences party oversight suppression and constrained autonomy
 *   - Conscripted Soldiers: Primary victim (powerless/trapped) — bears extraction through mandatory service, discipline suppression, and ideological indoctrination with zero exit capacity
 *   - Civilian Government Sector: Secondary victim (moderate/constrained) — coordinates on resource allocation but experiences military priority claims and diverted civilian resources
 *   - State-Owned Enterprises: Secondary victim (organized/constrained) — coordinate on dual-use technology but face mandatory military orders and R&D diversion
 *   - Civilian Economy: Tertiary victim (powerless/trapped) — bears resource extraction costs through defense spending priority and military-controlled sectors
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing party-military fusion as immutable feature of authoritarian governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chinese_civil_military_relations, 0.58).
domain_priors:suppression_score(chinese_civil_military_relations, 0.72).
domain_priors:theater_ratio(chinese_civil_military_relations, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chinese_civil_military_relations, extractiveness, 0.58).
narrative_ontology:constraint_metric(chinese_civil_military_relations, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(chinese_civil_military_relations, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chinese_civil_military_relations, tangled_rope).
narrative_ontology:human_readable(chinese_civil_military_relations, "Chinese Civil-Military Relations: Coordination and Control").
narrative_ontology:topic_domain(chinese_civil_military_relations, "political/military/institutional").

domain_priors:requires_active_enforcement(chinese_civil_military_relations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chinese_civil_military_relations, communist_party_leadership).
narrative_ontology:constraint_beneficiary(chinese_civil_military_relations, military_high_command).
narrative_ontology:constraint_victim(chinese_civil_military_relations, civilian_economy).
narrative_ontology:constraint_victim(chinese_civil_military_relations, civil_administration).
narrative_ontology:constraint_victim(chinese_civil_military_relations, military_personnel_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED SOLDIER (SNARE) — Trapped by mandatory service; faces suppression through military discipline hierarchy, party indoctrination, and legal prohibition on dissent. Bears extraction through labor conscription with minimal compensation and no exit mechanism. Maximum experienced coercion.
constraint_indexing:constraint_classification(chinese_civil_military_relations, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVILIAN GOVERNMENT SECTOR (TANGLED ROPE) — State-owned enterprises and civilian bureaucracies coordinate with military on strategic planning and resource allocation (genuine coordination function), but military extraction occurs through command authority over civilian resources, dual-use technology mandates, and prioritization in capital allocation. High suppression through party discipline structures; constrained exit from the state apparatus.
constraint_indexing:constraint_classification(chinese_civil_military_relations, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMUNIST PARTY LEADERSHIP (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: civil-military unification strengthens party control, enables rapid policy implementation, and consolidates power concentration. Party has arbitrage options (can reshape military structure unilaterally), so extraction runs toward the party, not away. Low experienced suppression—the constraint is designed to benefit party interests.
constraint_indexing:constraint_classification(chinese_civil_military_relations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY HIGH COMMAND (TANGLED ROPE) — Coordinates with party on national defense, strategic planning, and power projection (genuine coordination). Simultaneously extracts through autonomy in command decisions, resource control, preferential budget allocation, and personnel advancement independent of civilian oversight. Party oversight creates suppression that constrains autonomous decision-making; exit is constrained by party-military fusion at institutional levels.
constraint_indexing:constraint_classification(chinese_civil_military_relations, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARTY-MILITARY INSTITUTIONAL FRAMEWORK (PITON) — From the longest time horizon, the civil-military integration model appears largely performative in its stated coordination goals. The 'People's Army serves the people' rhetoric masks extraction mechanisms (resource diversion, personnel control, suppression of civilian autonomy). Theater ratio (0.68) reflects that institutional integration maintains legitimacy narratives while the actual function has shifted toward party-military consolidation. The framework persists through institutional inertia—alternatives haven't replaced it—rather than through functional effectiveness.
constraint_indexing:constraint_classification(chinese_civil_military_relations, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE-OWNED ENTERPRISE SECTOR (TANGLED ROPE) — Organized actors (industrial ministries, major SOEs) coordinate with military on defense production and dual-use technology (genuine coordination function with efficiency gains). Simultaneously bear extraction through mandatory military orders, price controls on defense goods, and diverted R&D resources from commercial sectors. Suppression through party-military oversight; constrained exit because SOEs cannot refuse military contracts.
constraint_indexing:constraint_classification(chinese_civil_military_relations, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, centralized control of military by a single-party state could appear as a structural necessity for regime survival—an immutable feature of how authoritarian systems organize. However, this perspective risks naturalizing what is actually a contingent institutional choice. The structural data (measured suppression, directionality toward party leadership) reveals extraction mechanisms rather than laws of nature.
constraint_indexing:constraint_classification(chinese_civil_military_relations, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chinese_civil_military_relations_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chinese_civil_military_relations, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chinese_civil_military_relations, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chinese_civil_military_relations, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chinese_civil_military_relations, TR),
    TR >= 0.70.

:- end_tests(chinese_civil_military_relations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts resources from civilian sectors (state-owned enterprises, conscripted labor, economic diversion to defense spending) while providing coordination benefits for national strategy and rapid policy implementation. The 0.58 value reflects genuine mixed function—the coordination of defense with civilian sectors is real and efficient, but the asymmetric resource flow toward military and party leadership constitutes measurable extraction. The upward trajectory from 0.42 to 0.58 over 20 years suggests increasing extraction relative to coordination value, which is a mandatrophy warning signal. Suppression (0.72): High. Military hierarchy, party discipline systems, conscription prohibition on exit, ideological indoctrination requirements, and legal suppression of autonomous military decision-making create substantial barriers. Suppression is both structural (legal/hierarchical) and internalized (identity fusion with party discipline). Theater ratio (0.68): Moderate-high. The performative content includes 'civil-military integration for development' rhetoric that obscures command authority asymmetries. Party-military coordination narratives mask suppression and resource extraction. Theater has increased from 0.55 to 0.68, suggesting that legitimacy maintenance requires increasing performative content as extraction grows—a piton warning signal (inertial institutional maintenance).
 *
 * PERSPECTIVAL GAP:
 *   Detailed gap analysis: Party leadership (Rope, d≈0.05) vs conscripted soldiers (Snare, d≈0.95) create a 1.9x gap in experienced χ magnitude. This gap reflects the constraint's core extractive asymmetry—coordinated national strategy benefits flow concentrated toward party leadership while labor extraction and suppression costs concentrate on powerless conscripted agents. Military high command (Tangled Rope, d≈0.50) occupy the middle—they are both beneficiary (resources, authority) and victim (party oversight, constrained autonomy), making the mixed classification appropriate. The gap between military perspective (Tangled Rope) and party perspective (Rope) reveals the constraint's asymmetric distribution: the party coordinates with the military and benefits from the arrangement, while the military coordinates with the party but also experiences suppression. This asymmetry is precisely what the tangled rope classification captures for military actors while the party experiences pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position within the extraction flow. Communist Party leadership: d ≈ 0.05 (full beneficiary + arbitrage exit = maximum benefit, minimal extraction cost). Military high command: d ≈ 0.50 (symmetric position—benefits from defense coordination and resource priority, but suppressed by party oversight; constrained exit prevents arbitrage). State-owned enterprises: d ≈ 0.65 (victims of mandatory orders but organized—can negotiate within constraints). Conscripted soldiers: d ≈ 0.95 (trapped victims bearing maximum extraction with zero exit). Civilian government sectors: d ≈ 0.72 (moderate victims of resource diversion, constrained but not trapped). The sigmoid function f(d) converts these d values into effective extraction modifiers—agents with d near 1.0 experience maximum χ, while beneficiaries with d near 0.0 experience negative χ. Scope modifier σ(national) = 1.0 applies uniformly. The resulting χ values show party leadership with minimal extraction cost (benefit flow), conscripted soldiers with maximum extraction (burden concentration), and intermediate actors with moderate extraction reflecting their mixed positions in the resource flow hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STATUS: Mandatrophy is not fully resolved but analytically tractable. The constraint shows genuine mixed function (coordination + extraction) across multiple institutional perspectives, preventing pure snare or pure extraction classification. However, the upward trajectory of both extractiveness (0.42→0.58) and theater (0.55→0.68) over 20 years creates a mandatrophy risk: if extractiveness continues rising while coordination benefits plateau, the tangled rope will degrade toward snare. The increasing theater ratio suggests that legitimacy maintenance is becoming increasingly performative relative to functional coordination—a classic pattern of institutional degradation (Piton trajectory). The constraint currently classifies as Tangled Rope from multiple institutional perspectives, which is correct—genuine coordination (defense strategy unification, rapid policy implementation, technological integration) coexists with measurable extraction (resource diversion, conscription labor, suppression). However, the omega variables around coordination-extraction boundary and civil-military fusion cascade risk should guide future remeasurement. If empirical analysis resolves these omegas toward 'extraction dominates,' the classification should shift toward Snare with accelerating degradation patterns. If coordination sustains relative to theater, the Tangled Rope is stable and the performance increase may reflect genuine complexity growth rather than institutional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'What portion of observed civil-military integration serves genuine coordination versus consolidating party-military control over civilian resources and autonomy?',
    'Comparative institutional analysis: measure efficiency gains from unified command versus capability losses from civilian sector diversion. Track resource allocation patterns across defense versus commercial sectors.',
    'If coordination dominates (>60% genuine coordination): reclassify toward higher Rope proportion. If extraction dominates (>70% extraction): reclassify toward higher Snare proportion. Current 58% extractiveness suggests rough parity, but measurement methodology is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Boundary between coordination and extraction in civil-military integration').

omega_variable(
    party_leadership_unilateral_authority,
    'Does party leadership maintain genuine arbitrage optionality over the military, or has military institutional entrenchment created reciprocal constraint on party decisions?',
    'Historical analysis of party decisions overridden by military institutional resistance; structural dependency of party control on military enforcement capacity. Test through studying cases where party directives faced military implementation barriers.',
    'If party maintains unilateral authority: party classification as institutional/arbitrage is correct—party can reshape military unilaterally. If military has entrenchment: party classification should shift to constrained exit, raising party''s experienced extractiveness and potentially reclassifying party perspective from Rope to Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(party_leadership_unilateral_authority, empirical, 'Whether party leadership maintains unilateral authority over military or has become institutionally reciprocally constrained').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is measured suppression (0.72) primarily structural (legal prohibition, institutional barriers, command hierarchy) or internalized (ideological commitment, identity fusion with party discipline, cognitive capture)?',
    'Post-exit analysis: track behavior and attitudes of military personnel after service or defection. Measure cognitive openness to alternative authority structures. Compare suppression persistence in diaspora communities versus within China.',
    'If predominantly structural: suppression should decline as barriers are removed (post-retirement, post-exit). If predominantly internalized: suppression persists even after structural barriers are removed, suggesting identity-lock mechanism. This would justify higher omega confidence in suppression durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    military_personnel_identity_fusion,
    'Do military personnel experience identity-locked exit options (self-concept fused with military role) or constrained exit (high-cost external barriers)?',
    'Qualitative analysis of military culture narratives; post-service career transitions and identity reformulation; psychological profiles of military-to-civilian role transitions. Compare with conscription systems where exit is primarily structural.',
    'If identity-locked: reclassify conscripted soldier perspective from trapped to identity_locked, which changes the immutability profile—the binding mechanism is internal rather than external. If constrained: maintain trapped classification, suggesting pure structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_personnel_identity_fusion, empirical, 'Whether military personnel face identity-locked or constrained exit').

omega_variable(
    civil_military_fusion_cascade_risk,
    'As civil-military integration deepens (observed theater_ratio increase from 0.55 to 0.68 over two decades), does performative integration mask underlying institutional decoupling, or does it reflect genuine consolidation with increasing extraction capacity?',
    'Track coordination efficiency metrics (decision speed, resource allocation optimization) versus extraction metrics (civilian sector resource diversion, suppression indicators) over time. Measure gap between stated civil-military coordination goals and actual implementation.',
    'If theater increases while coordination efficiency plateaus: tangled rope is degrading toward snare—extraction mechanisms are escalating while coordination justifications remain constant (classic piton pattern). If theater and coordination both increase: tangled rope is stable or becoming more effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_military_fusion_cascade_risk, empirical, 'Whether civil-military fusion theater masks degradation or reflects consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chinese_civil_military_relations, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, chinese_civil_military_relations, theater_ratio, 0, 0.55).
narrative_ontology:measurement(chin_tr_t10, chinese_civil_military_relations, theater_ratio, 10, 0.62).
narrative_ontology:measurement(chin_tr_t20, chinese_civil_military_relations, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, chinese_civil_military_relations, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(chin_be_t10, chinese_civil_military_relations, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(chin_be_t20, chinese_civil_military_relations, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chinese_civil_military_relations, enforcement_mechanism).
narrative_ontology:affects_constraint(chinese_civil_military_relations, chinese_defense_spending_prioritization).
narrative_ontology:affects_constraint(chinese_civil_military_relations, military_personnel_autonomy_suppression).
narrative_ontology:affects_constraint(chinese_civil_military_relations, dual_use_technology_mandates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chinese_civil_military_relations, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
