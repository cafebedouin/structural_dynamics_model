% ============================================================================
% CONSTRAINT STORY: jp_eez_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jp_eez_enforcement, []).

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
 *   constraint_id: jp_eez_enforcement
 *   human_readable: Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)
 *   domain: geopolitical/maritime_sovereignty
 *
 * SUMMARY:
 *   Japan's claimed Exclusive Economic Zone (EEZ) around the Senkaku/Diaoyu
 *   Islands represents a multi-layered constraint spanning maritime
 *   sovereignty assertion, resource competition, and great-power rivalry. The
 *   constraint exhibits characteristics of tangled coordination-extraction
 *   hybrid: Japan (and its US ally) benefit from asserting sovereign control
 *   over resources and maritime boundaries; China's fishing fleets and
 *   central government bear extraction costs through restricted access and
 *   enforcement pressure; international maritime law institutions attempt
 *   coordination through UNCLOS frameworks; and the underlying post-war
 *   alliance structure persists through institutional theater despite
 *   atrophying original function. The constraint has intensified over the
 *   measurement interval as enforcement capability improved and Chinese
 *   fishing pressure increased, driving extractiveness from 0.35 to 0.58
 *   while theater ratio rose from 0.48 to 0.61, indicating increasing
 *   performative signaling (military exercises, diplomatic statements)
 *   relative to actual resource extraction.
 *
 * KEY AGENTS:
 *   - Japanese State: Primary beneficiary (institutional/arbitrage) — asserts EEZ sovereignty, controls resource access, enhances alliance credibility
 *   - Japanese Fishing Industry: Secondary beneficiary (powerful/arbitrage) — gains exclusive access to contested fishing grounds, reduced competition from Chinese fleets
 *   - Chinese Fishing Fleet: Primary victim (powerless/trapped) — faces vessel seizures, fines, restrictions on access to traditional grounds; no viable alternative
 *   - Chinese State: Secondary victim and strategic competitor (organized/constrained) — incurs diplomatic costs, escalation risk, must coordinate domestic constraint without losing nationalist credibility
 *   - United States: Alliance beneficiary (powerful/mobile) — maintains strategic positioning, alliance credibility, regional containment, but expends resources and accepts escalation risk
 *   - International Maritime Law: Institutional framework (organized/mobile) — UNCLOS and arbitration mechanisms represent coordination pathway with sunset logic if disputes resolved
 *   - Post-WWII Alliance Order: Structural foundation (institutional/arbitrage) — undergirds Japanese EEZ claims but persists through degraded function (piton status)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_eez_enforcement, 0.58).
domain_priors:suppression_score(jp_eez_enforcement, 0.72).
domain_priors:theater_ratio(jp_eez_enforcement, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_eez_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(jp_eez_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jp_eez_enforcement, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jp_eez_enforcement, tangled_rope).
narrative_ontology:human_readable(jp_eez_enforcement, "Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)").
narrative_ontology:topic_domain(jp_eez_enforcement, "geopolitical/maritime_sovereignty").

domain_priors:requires_active_enforcement(jp_eez_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japanese_state).
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japanese_fishing_industry).
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_state).
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_fishing_fleet).
narrative_ontology:constraint_victim(jp_eez_enforcement, international_maritime_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE FISHING FLEET (SNARE) — Small-scale fishers operating in contested waters face maximum extraction and suppression. Exit options are trapped: abandoning traditional fishing grounds means economic collapse; continuing means risk of detention, vessel seizure, and fines. Enforcement creates a coercive extraction mechanism with minimal coordination benefit for this actor.
constraint_indexing:constraint_classification(jp_eez_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CHINESE STATE (TANGLED ROPE) — China's central government benefits from asserting sovereignty claims (domestic legitimacy, resource access) while bearing costs of escalation risk and international friction. Exit is constrained: retreating from the dispute damages nationalist credibility; aggressive pushback risks military confrontation. Both coordination and extraction present — must coordinate with fishing fleets while extracting concessions (abandoning grounds, accepting maritime restrictions) to avoid escalation.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: JAPANESE STATE (ROPE) — Japan's central government benefits from enforcing EEZ claims (resource control, territorial assertion, alliance credibility with US). Experiences the constraint primarily as coordination: organizing coast guard patrols, communicating sovereignty signals, coordinating with fishing industry. Effective exit via arbitrage — can modulate enforcement intensity without fundamental loss.
constraint_indexing:constraint_classification(jp_eez_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL MARITIME LAW (SCAFFOLD) — UNCLOS and international arbitration mechanisms represent a temporary coordination framework designed to resolve maritime disputes through adjudication rather than coercion. This perspective sees the EEZ enforcement bottleneck as solvable via dispute resolution with sunset logic: as bilateral or arbitrated agreements clarify boundaries, the enforcement intensity should decline. Theater ratio reflects that much EEZ enforcement is performative signaling rather than resource extraction.
constraint_indexing:constraint_classification(jp_eez_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-WWII ALLIED ORDER (PITON) — The underlying constraint is maintenance of the San Francisco System: Japan's territorial integrity and EEZ rights rest on the US-Japan alliance and post-war settlement. This framework persists through institutional inertia despite rising challenges from China. The constraint is degraded — the original function (preventing Japanese military resurgence, integrating Japan into liberal order) has atrophied, but the alliance structure remains maintained through ritual and theater (military exercises, base agreements, coordination statements) rather than core function.
constraint_indexing:constraint_classification(jp_eez_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UNITED STATES (TANGLED ROPE) — The US benefits from EEZ enforcement (maintaining alliance credibility, containing Chinese expansion, strategic positioning in Indo-Pacific). Experiences both coordination (joint patrols, intelligence sharing, alliance theater) and extraction (must expend resources, faces Chinese escalation risk, constrained diplomatic flexibility). Exit is mobile but costly — could theoretically realign but would lose strategic positioning.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, maritime resource competition and territorial assertion are inherent to state behavior. Great powers competing for EEZ control is a natural feature of international anarchy. However, this naturalizes what is actually a contingent post-war legal framework (UNCLOS) layered onto pre-existing claims. The engine's false summit detector will identify this perspective as naturalization rather than true mountain.
constraint_indexing:constraint_classification(jp_eez_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_eez_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jp_eez_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jp_eez_enforcement, TR),
    TR >= 0.70.

:- end_tests(jp_eez_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): The constraint exhibits substantial extraction of Chinese fishing fleet access combined with constraints on Chinese state behavior. The value reflects the asymmetric benefit structure: Japan gains exclusive resource access while China loses both fishing grounds and diplomatic flexibility. However, extractiveness is not higher because (a) enforcement is not total — some Chinese fishing continues despite restrictions — (b) China retains exit options through diplomatic escalation or international arbitration — (c) the US ally relationship partially limits Japan's unilateral extraction capacity. Suppression (0.72): High. Chinese actors face significant coercive pressure with limited alternatives: fleet restrictions via coast guard enforcement; diplomatic costs for state assertion; domestic legitimacy pressure against concession. Yet suppression is not total because China can escalate and international mechanisms exist. Theater ratio (0.61): Moderate-high. Enforcement involves substantial performative elements: military exercises, diplomatic statements, sovereignty signaling. But significant extraction is also real: actual vessel seizures, fines, ground restrictions. The ratio has increased from 0.48 because more enforcement occurs through political theater (statements, exercises) rather than direct resource extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: Japanese state sees Rope (legitimate sovereignty coordination), Chinese fleet sees Snare (pure coercive extraction), US sees Tangled Rope (both alliance coordination and resource competition), Chinese state sees Tangled Rope (coordination with fleets vs extraction by Japan/US), international law sees Scaffold (solvable via arbitration with sunset logic), post-war order sees Piton (degraded function maintained through theater), analytical observer risks Mountain (naturalizing post-war law as natural law). The gap reflects fundamentally incompatible structural positions: what Japan experiences as rightful sovereignty enforcement, China experiences as unjust maritime restriction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from power position and exit options relative to this constraint. Chinese fishing fleets (powerless/trapped) experience maximum extraction: high d → high f(d) → high χ. Japanese state (institutional/arbitrage) experiences low extraction: low d → negative f(d) → negative χ (benefits from the constraint). Chinese state (organized/constrained) experiences moderate extraction: d ≈ 0.55 → moderate χ. US ally (powerful/mobile) experiences moderate extraction with mixed signals: d ≈ 0.45 → mixed f(d). International law institutions (organized/mobile) experience low extraction: d ≈ 0.35 → low χ (benefit from coordination function). Analytical observer (analytical/analytical) risks false summit by naturalizing post-war legal framework as natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between legitimate coordination (maritime boundaries clarification via UNCLOS) and asymmetric extraction (resource access restriction + diplomatic pressure on China). The Tangled Rope classification captures both: the EEZ framework does provide genuine coordination function (reducing collision risk, clarifying boundaries), but it is layered with extraction (benefiting Japan/US while constraining China). The Snare from the Chinese fishing fleet perspective and Rope from the Japanese perspective are both valid but incompletely describe the structure — the Tangled Rope from the Chinese state and US perspectives is the more complete structural picture. The piton classification of the post-war order reveals that the underlying constraint is institutional inertia: the alliance persists through alliance theater and military signaling even though the original deterrent function (preventing Japanese militarization) has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senkaku_sovereignty_status,
    'Are the Senkaku/Diaoyu Islands factually under Japanese administrative control, or do they represent a contested claim subject to reversion?',
    'Historical review of 1972 Okinawa reversion agreement language; examination of whether disputed islands were explicitly included in repatriated territory vs retained by US; legal analysis of Chinese government statements and their relationship to underlying territorial claims',
    'If islands are under unambiguous Japanese control: EEZ enforcement is coordination mechanism (Rope from Japan perspective). If sovereignty is genuinely contested and reversible: enforcement is extraction mechanism (Snare from Chinese perspective, Tangled Rope from Chinese state perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(senkaku_sovereignty_status, empirical, 'Factual status of Senkaku/Diaoyu islands under international law').

omega_variable(
    enforcement_proportionality_threshold,
    'What enforcement intensity threshold distinguishes legitimate maritime sovereignty assertion from militarized coercion?',
    'Comparative analysis of coast guard tactics across EEZ disputes; measurement of escalation rate over time; correlation between enforcement intensity and actual resource extraction or territorial consolidation',
    'If threshold is low (current Japanese enforcement is already excessive): constraint classifies as Snare from more perspectives. If threshold is high (enforcement remains within coast guard norms): constraint is Rope/Tangled Rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_proportionality_threshold, empirical, 'Threshold distinguishing legitimate sovereignty from militarized coercion').

omega_variable(
    fishing_fleet_alternative_grounds,
    'Do realistic alternative fishing grounds exist for Chinese fleets, or are the Senkaku waters irreplaceable for economic survival of fishing communities?',
    'Economic analysis of Chinese fishing fleet catch composition; assessment of resource availability in EEZs of other nations accessible to Chinese vessels; survey of fishing community dependence on Senkaku grounds',
    'If alternatives exist: Chinese fishing fleets have constrained exit (not trapped), affecting directionality upward and reducing Snare classification strength. If Senkaku grounds are unique: fishing fleets are trapped, strengthening Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fishing_fleet_alternative_grounds, empirical, 'Availability of alternative fishing grounds for Chinese fleets').

omega_variable(
    us_alliance_credibility_mechanics,
    'Does US military presence in the region actually constrain Chinese escalation, or does it primarily signal alliance commitment while Chinese escalation proceeds through non-military coercion?',
    'Analysis of Chinese behavior patterns in contested waters during high vs low US military presence; assessment of whether military signaling prevents actual incidents or merely prevents escalation to military engagement',
    'If US presence constrains escalation: Japan''s exit options expand (arbitrage becomes more viable), reducing Snare classification strength. If signaling is decoupled from actual constraint: Snare classification from Chinese perspective strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_alliance_credibility_mechanics, empirical, 'Whether US military presence constrains Chinese escalation or merely signals commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jp_eez_enforcement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jp_eez_tr_t0, jp_eez_enforcement, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jp_eez_tr_t10, jp_eez_enforcement, theater_ratio, 10, 0.55).
narrative_ontology:measurement(jp_eez_tr_t20, jp_eez_enforcement, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(jp_eez_be_t0, jp_eez_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jp_eez_be_t10, jp_eez_enforcement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jp_eez_be_t20, jp_eez_enforcement, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jp_eez_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(jp_eez_enforcement, south_china_sea_maritime_claims).
narrative_ontology:affects_constraint(jp_eez_enforcement, us_japan_alliance_security_commitment).
narrative_ontology:affects_constraint(jp_eez_enforcement, chinese_fishing_fleet_viability).
narrative_ontology:affects_constraint(jp_eez_enforcement, unclos_dispute_resolution).

% DUAL FORMULATION NOTE:
% Japan's EEZ enforcement can be decomposed into two structurally distinct constraints: (1) legitimate maritime boundary clarification (Rope) — UNCLOS coordination function; (2) asymmetric resource extraction from Chinese actors (Snare/Tangled Rope) — great power competition. The current story treats them as unified Tangled Rope, but separate constraint families could distinguish the coordination from the extraction components with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jp_eez_enforcement, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
