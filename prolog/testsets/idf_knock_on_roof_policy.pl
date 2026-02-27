% ============================================================================
% CONSTRAINT STORY: idf_knock_on_roof_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_idf_knock_on_roof_policy, []).

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
 *   constraint_id: idf_knock_on_roof_policy
 *   human_readable: IDF 'Knock on the Roof' Warning Policy
 *   domain: political/military/humanitarian
 *
 * SUMMARY:
 *   The 'Knock on the Roof' policy represents a structural constraint between
 *   military operational objectives, international humanitarian law
 *   requirements, and civilian survival in armed conflict. The IDF conducts a
 *   small strike (or loud warning signal) on a building's roof prior to
 *   larger airstrikes, nominally to provide occupants time to evacuate. The
 *   policy creates a complex extraction mechanism: it functions
 *   simultaneously as a coordination attempt (alerting occupants), a legal
 *   justification (demonstrating proportionality compliance), and an
 *   enforcement tool (suppressing alternatives by making the strike
 *   inevitable if evacuation is not completed). The constraint exhibits
 *   tangled coordination-extraction dynamics because the warning requirement
 *   is genuine (derived from IHL), but the operational context systematically
 *   prevents the coordination function from working: evacuation routes are
 *   blocked, safe destinations are unavailable, and the warning duration is
 *   insufficient for meaningful escape. This creates the essential tangled
 *   rope signature: active enforcement of the warning requirement
 *   (coordination) combined with systematic suppression of evacuation
 *   capacity (extraction).
 *
 * KEY AGENTS:
 *   - Building Occupants: Primary victims (powerless/trapped) — receive warnings but lack exit capacity; bear full extraction cost
 *   - Palestinian Civilians in Gaza: Primary victims (moderate/trapped) — nominally offered safe passage but encounter systematic blockades; high suppression of alternatives
 *   - IDF Operational Command: Primary beneficiary (institutional/arbitrage) — benefits from policy as operational cover and legal justification; experiences constraint as enabling coordination
 *   - Israeli Government Legal Apparatus: Secondary beneficiary (institutional/constrained) — benefits from proportionality justification but constrained by IHL and accountability mechanisms
 *   - International Humanitarian Law Framework: Institutional observer (analytical/analytical) — warning requirement is maintained performatively despite systemic failure of functional purpose
 *   - International Monitoring Organizations: Organized oversight (organized/constrained) — view policy as temporary institutional arrangement with potential accountability sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(idf_knock_on_roof_policy, 0.58).
domain_priors:suppression_score(idf_knock_on_roof_policy, 0.72).
domain_priors:theater_ratio(idf_knock_on_roof_policy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(idf_knock_on_roof_policy, tangled_rope).
narrative_ontology:human_readable(idf_knock_on_roof_policy, "IDF 'Knock on the Roof' Warning Policy").
narrative_ontology:topic_domain(idf_knock_on_roof_policy, "political/military/humanitarian").

domain_priors:requires_active_enforcement(idf_knock_on_roof_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(idf_knock_on_roof_policy, idf_operational_command).
narrative_ontology:constraint_beneficiary(idf_knock_on_roof_policy, israeli_government_legal_justification).
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, palestinian_civilians_in_gaza).
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, building_occupants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BUILDING OCCUPANTS (SNARE) — Receive minimal warning (10-15 minutes) before airstrike. Unable to evacuate safely due to blocked roads, disabled vehicles, lack of transportation, or inability to identify safe destinations. Maximum extraction: survival depends on IDF discretion. No genuine exit option exists despite nominal warning.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PALESTINIAN CIVILIANS (SNARE) — Warning system creates illusion of consent to evacuation ('safe passage') while actual exit routes are blocked, incomplete, or lead to equally dangerous areas. Suppression is nearly total: no genuine alternative to remaining in Gaza, no safe destination, no protection beyond the knock-and-wait mechanism. High extractiveness from suppression of alternatives.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: IDF OPERATIONAL COMMAND (ROPE) — Experiences the policy as a coordination mechanism solving the military objective of target elimination while (nominally) minimizing civilian harm. Benefits from the policy: provides legal/humanitarian cover for operations, enables operational tempo, demonstrates 'proportionality' claim in international law. Low experienced extraction because the constraint subsidizes military objectives.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ISRAELI GOVERNMENT LEGAL APPARATUS (TANGLED ROPE) — Benefits from policy as legal justification for strikes (demonstrates 'warning' as proportionality defense). Also constrained by international humanitarian law requirements to minimize civilian harm. Experiences mixed coordination (legal requirement to warn) and extraction (ability to conduct strikes despite incomplete evacuation). Active enforcement required to maintain the legal framing.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW FRAMEWORK (PITON) — The warning requirement (per IHL) is maintained as a performative gesture despite systemic inability of civilians to evacuate. Theater ratio is high: the warning ritual persists as institutional practice even though its functional purpose (enabling safe exit) is systematically frustrated by suppression of evacuation routes. The mechanism degrades over time as the framework's core assumption (warnings enable escape) is contradicted by operational reality.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MONITORING AND ACCOUNTABILITY MECHANISMS (SCAFFOLD) — Organized actors (UN, ICC, human rights organizations) view the policy as a temporary institutional arrangement that could be replaced by alternative protective mechanisms (certified safe corridors, pre-evacuation agreements, targeted operations without area strikes). The policy has a potential sunset as accountability pressure and international legal proceedings mature. Suppression is high but constrained by organized scrutiny.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(idf_knock_on_roof_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(idf_knock_on_roof_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(idf_knock_on_roof_policy, TR),
    TR >= 0.70.

:- end_tests(idf_knock_on_roof_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The policy extracts civilian cooperation with their own evacuation or death — the warning creates a binary choice (flee or be struck) with the evacuation option systematically constrained. This is more extractive than pure coordination (which would enable meaningful choice) but somewhat less than pure snare (which would eliminate the choice entirely). The extraction increases as evacuation capacity is suppressed. Suppression (0.72): Very high. The operational context provides almost no genuine alternative to either evacuation or casualty: blocked roads, absent safe destinations, insufficient warning duration, lack of transportation, and the subsequent inevitable strike create a near-total suppression landscape. Theater ratio (0.68): High. The warning ritual is substantially performative — it satisfies the formal requirement of IHL notification while the functional purpose (enabling escape) is systematically frustrated. The theater has increased over the operational period as the gap between nominal warning and actual evacuation capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The most significant gap lies between the IDF's perspective (the policy enables proportional operations while respecting IHL) and the building occupants' perspective (the warning is theater masking inevitable casualty). The Israeli legal apparatus attempts to bridge this gap by claiming the warning satisfies proportionality requirements — but this argument presupposes that evacuation is genuinely feasible. The international humanitarian law framework maintains the warning requirement on paper while operational reality demonstrates its dysfunction (piton perspective). The scaffold perspective (international accountability creating eventual change) conflicts with the piton perspective (institutional persistence despite degradation). These gaps reveal that the constraint's classification depends entirely on whether evacuation capacity can be meaningfully achieved — a structural question masked by the nominal warning ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   The IDF operational command and Israeli legal apparatus occupy beneficiary positions with arbitrage exit options (they can choose when and where to apply the policy, can modify protocols, have institutional flexibility). They experience low or subsidizing chi because the constraint enables their objectives. Building occupants and Palestinian civilians occupy victim positions with trapped exit options (cannot leave Gaza, cannot access roads, cannot refuse to be in buildings). They experience high chi because all alternatives are suppressed. International frameworks and accountability mechanisms occupy moderate positions with constrained exit options (they can investigate and document but cannot unilaterally enforce compliance). The perspectival gap reflects the structural asymmetry: beneficiaries see a coordination mechanism that respects humanitarian requirements; victims see an extraction mechanism that creates the appearance of choice while eliminating actual alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy resolves potential mandatrophy by distinguishing between the coordination component (warning requirement per IHL) and the extraction component (systematic suppression of evacuation capacity). The coordination requirement is genuine and enforced; the extraction emerges from operational context that prevents the coordination from functioning. This is the canonical tangled rope signature: both coordination and extraction are structurally real, not competing frames. The piton perspective reveals the degradation mechanism: as the gap between nominal warning (coordination ritual) and actual evacuation capacity (extraction reality) widened, the institutional justification persists through theater — the warning ritual continues not because it works but because it satisfies legal/political requirements. The scaffold perspective identifies the potential sunset: alternative mechanisms (certified safe corridors, pre-coordinated evacuations, accountability enforcement) could replace the current tangled arrangement with either pure coordination or explicit extraction, but would require institutional changes with generational timescales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evacuation_feasibility_threshold,
    'What warning duration and available evacuation capacity would constitute genuinely enabling escape versus performative warning theater?',
    'Comparative analysis of successful vs failed evacuations; mapping of road access, vehicle availability, and destination safety against warning duration; longitudinal tracking of evacuation times vs strike delays',
    'If threshold < 30 minutes with confirmed routes: current policy approaches functional coordination. If threshold > 2 hours or requires external assistance: policy is pure extraction theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evacuation_feasibility_threshold, empirical, 'Feasibility threshold for warning-enabled evacuation').

omega_variable(
    suppression_mechanism_intent,
    'Is the blocking of evacuation routes an independent operational constraint or a deliberate suppression mechanism to amplify extraction?',
    'Analysis of documented blockades; comparison of strike timing with road closure timing; examination of alternative strike methodologies that would permit evacuation',
    'If independent constraint: classification shifts toward tangled rope (unavoidable coordination problem). If deliberate: classification reinforces snare (designed extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_intent, empirical, 'Whether evacuation blocking is independent constraint or deliberate suppression').

omega_variable(
    humanitarian_law_enforcement_viability,
    'Can international accountability mechanisms effectively enforce IHL warning requirements in ways that force genuine evacuation capability rather than nominal notification?',
    'Tracking of ICC investigations, national court proceedings, and documented changes to operational protocols; analysis of whether accountability pressure alters warning-evacuation feasibility',
    'If enforcement effective: scaffold sunset is credible (10-15 years toward genuine protective mechanisms). If ineffective: the policy persists as institutionalized theater (piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_law_enforcement_viability, conceptual, 'Whether international law enforcement can viabilize genuine evacuation mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(idf_knock_on_roof_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(knor_tr_t0, idf_knock_on_roof_policy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(knor_tr_t5, idf_knock_on_roof_policy, theater_ratio, 5, 0.62).
narrative_ontology:measurement(knor_tr_t10, idf_knock_on_roof_policy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(knor_be_t0, idf_knock_on_roof_policy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(knor_be_t5, idf_knock_on_roof_policy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(knor_be_t10, idf_knock_on_roof_policy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(idf_knock_on_roof_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, gaza_evacuation_route_accessibility).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, ihl_proportionality_justification).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, civilian_targeting_accountability).

% DUAL FORMULATION NOTE:
% The knock-on-roof policy decomposes into three related constraints: (1) the formal warning requirement (coordination), (2) the operational suppression of evacuation routes (extraction), and (3) the legal proportionality justification (institutional performance). The policy as a unified phenomenon exhibits tangled rope structure; its components could be analyzed separately with different extractiveness values. The warning requirement alone would be rope (pure coordination); the evacuation blockade alone would be snare (pure extraction); the legal framing alone would be piton (theater). The integrated policy is tangled because all three components operate simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(idf_knock_on_roof_policy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
