% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary Freedom-of-Navigation Regime Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   A set of naval powers — most prominently the leading non-ratifier of the
 *   1982 convention — treat freedom-of-navigation principles as customary
 *   international law binding on all states regardless of treaty membership,
 *   and maintain them through scheduled presence operations through waters
 *   whose controlling governments restrict foreign warship movement. The
 *   enforcing powers invoke the convention's passage provisions while
 *   declining its seabed-mining obligations and dispute-settlement
 *   commitments; coastal states pursuing exclusive economic zone control or
 *   warship-authorization requirements see their claims publicly catalogued
 *   as excessive and their waters entered without consent. This file is ONE
 *   READING of the unclos_sovereignty_boundary kernel and authors epsilon
 *   only for this enforcement-centered arrangement. The siblings differ
 *   structurally: strict_eez_reading would author epsilon for a
 *   codified-exclusivity arrangement whose extraction falls on navies and
 *   shippers denied access, and historical_rights_reading would author
 *   epsilon for a usage-priority arrangement whose costs fall on the
 *   neighbors of historic claimants. Same label, three constraints, three
 *   epsilon referents; they form one linked family. KEY AGENTS (by structural
 *   relationship): - blue_water_naval_powers: Agenda-setting enforcer and
 *   principal collector ([institutional]/[arbitrage]) — runs the presence
 *   operations, collects mobility and obligation-avoidance -
 *   eez_exclusivity_coastal_states: Primary target ([powerful]/[trapped]) —
 *   bears the challenged claims - warship_authorization_requirement_states:
 *   Secondary target ([moderate]/[trapped]) — experiences unconsented entries
 *   - global_commercial_shipping: Diffuse beneficiary
 *   ([organized]/[constrained]) — rides the open lanes without funding them -
 *   ratifying_maritime_states: Dual-positioned party
 *   ([organized]/[constrained]) — carries treaty costs the enforcer sheds -
 *   small_island_development_states: Absent voice ([powerless]/[trapped]) -
 *   law_of_the_sea_doctrine_community: Analytical observer
 *   ([analytical]/[analytical]) — sees the full structure
 *
 * KEY AGENTS:
 *   - blue_water_naval_powers: agenda-setting enforcer and principal collector (institutional power, arbitrage-grade exit) — conducts and schedules the presence operations that constitute the enforcement
 *   - eez_exclusivity_coastal_states: primary target (powerful, trapped) — extended-jurisdiction claims challenged and catalogued as excessive
 *   - warship_authorization_requirement_states: secondary target (moderate, trapped) — domestic authorization requirements overridden by unnotified passages
 *   - global_commercial_shipping: diffuse beneficiary (organized, constrained) — receives predictable transit without contributing to its maintenance
 *   - ratifying_maritime_states: dual-positioned beneficiary/payer (organized, constrained) — accepts treaty obligations the enforcing non-parties avoided
 *   - small_island_development_states: excluded voice (powerless, trapped) — dependent on both open lanes and intact zone revenues, seated nowhere in the contest
 *   - law_of_the_sea_doctrine_community: analytical observer (analytical, analytical) — adjudicates and publishes on which claims are lawful
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.55).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom-of-Navigation Regime Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '32f258a5-0877-4890-9155-863ef380fe6a').
narrative_ontology:cs_kernel_codification('32f258a5-0877-4890-9155-863ef380fe6a', fixed_text).
narrative_ontology:cs_authority_grounding('32f258a5-0877-4890-9155-863ef380fe6a', practice).
narrative_ontology:cs_interpretation_layer_present('32f258a5-0877-4890-9155-863ef380fe6a').
narrative_ontology:cs_reading_relation('32f258a5-0877-4890-9155-863ef380fe6a', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('32f258a5-0877-4890-9155-863ef380fe6a', unclos_sovereignty_boundary__historical_rights_reading, influences).
narrative_ontology:cs_axiom('32f258a5-0877-4890-9155-863ef380fe6a', foundational, navigation_custom_binds_without_ratification).
narrative_ontology:cs_axiom_status(navigation_custom_binds_without_ratification, holdable).
narrative_ontology:cs_axiom_grounding('32f258a5-0877-4890-9155-863ef380fe6a', navigation_custom_binds_without_ratification, conventional).
narrative_ontology:cs_axiom('32f258a5-0877-4890-9155-863ef380fe6a', secondary, presence_exercise_validates_enforcement_title).
narrative_ontology:cs_axiom_status(presence_exercise_validates_enforcement_title, holdable).
narrative_ontology:cs_axiom_grounding('32f258a5-0877-4890-9155-863ef380fe6a', presence_exercise_validates_enforcement_title, instrumental).
narrative_ontology:cs_reference_frame('32f258a5-0877-4890-9155-863ef380fe6a', customary_open_oceans_order).
narrative_ontology:cs_drift_state('32f258a5-0877-4890-9155-863ef380fe6a', post_artificial_island_militarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32f258a5-0877-4890-9155-863ef380fe6a', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_commercial_shipping).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, eez_exclusivity_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, warship_authorization_requirement_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, ratifying_maritime_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, ratifying_maritime_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate fleets worldwide under a doctrine that treats navigation freedoms as long-established practice binding on all states. Publish an annual catalogue of which coastal claims were judged excessive, schedule transits to contest them, and log each operation. Have declined to join the 1982 convention whose passage provisions they invoke, thereby avoiding its seabed-mining obligations and compulsory dispute-settlement commitments while exercising its navigation rights. Their exit would mean accepting the full treaty package or withdrawing from distant operations; neither is contemplated, and they retain discretion over which claims to challenge and when.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers, beneficiary).

% Carries the bulk of world trade along sea lanes whose openness the naval presence underwrites. Pays nothing into the enforcement effort directly, gains predictable transit and stable insurance pricing on open corridors, and absorbs higher costs where disputes heat specific routes. Individual carriers can reflag or reroute, but the industry as a whole has no alternative to the lanes themselves and lobbies quietly for their preservation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_commercial_shipping, beneficiary,
    organized, biographical, constrained, global).

% Large coastal states asserting extended jurisdiction over adjacent waters for security and resource control, including notification or permission requirements for foreign warships. See their claimed boundaries crossed without consent and their legal positions published in foreign challenge catalogues as excessive. Their coastlines and claimed waters are fixed; they cannot relocate away from the operations, and retrenchment reads domestically as ceding sovereign space, so their responses escalate toward construction, garrisoning, and coast-guard confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, eez_exclusivity_coastal_states, payer,
    powerful, generational, trapped, regional).

% States whose domestic law requires prior authorization for foreign warships entering waters they administer, typically citing security sensitivities around straits, anchorages, or sensitive facilities. Experience unnotified passages as intrusions regardless of the entering powers' legal theory, responding with protest notes, shadowing, and occasionally interception maneuvers. Their leverage stops at escalation risk, since they cannot physically exclude larger navies from their own approaches.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, warship_authorization_requirement_states, payer,
    moderate, biographical, trapped, national).

% Joined the 1982 convention and accept its seabed-mining obligations, dispute-settlement procedures, and boundary disciplines — costs the chief enforcing non-party avoids. They benefit from the same open-lane order, frequently second the enforcement position diplomatically, and fund hydrographic surveys, coast guards, and tribunals that support the regime they ratified. Leaving the convention would forfeit legal protections they rely on in their own boundary disputes, so exit is effectively closed.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, ratifying_maritime_states, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, ratifying_maritime_states, payer).

% Depend on open shipping lanes for food, fuel, and export revenue, and simultaneously on fisheries and seabed income inside their own zones. Possess no fleet capable of influencing either side of the contest and are rarely seated in the bilateral encounters that decide facts on the water; their preferred outcome — stable rules honoring both passage and zone rights — reaches the table mainly through coalition statements in UN fora.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_island_development_states, excluded,
    powerless, generational, trapped, regional).

% Legal scholars, tribunal members, and foreign-ministry advisers who adjudicate and publish on which maritime claims are lawful. Trace where custom ends and codified text begins, evaluate historic-title assertions, and supply the interpretive vocabulary every party invokes. Neither collects nor pays under the arrangement, but their determinations shape which claims survive and which are catalogued as excessive.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, law_of_the_sea_doctrine_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, blue_water_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains open, predictable transit through international straits, archipelagic waters, and exclusive economic zones for warships and merchant vessels alike. Solves a real collective-action problem: without a standing open-passage practice, each coastal state's incremental restrictions would accumulate until global mobility fragmented into corridor-by-corridor negotiated access, raising costs for every trading state including the restrictors themselves.
% TRANSFER_FUNCTION: Moves unimpeded mobility and strategic access from coastal-state jurisdictional claims to enforcing naval powers and their commercial sectors. Moves the burden of contested sovereignty — protested passages, militarized standoffs, published excessive-claims catalogues — onto the coastal states asserting control. Leaves the enforcing non-parties free of the seabed-mining contributions and dispute-settlement exposures that ratifying states carry under the same regime they invoke.
% ABSENT_VOICES: Small island development states, dependent on both open lanes and intact zone revenues, appear only through occasional coalition statements and are absent from the bilateral encounters that produce facts on the water. Coastal fishing communities living beside the contested waters are unseated entirely — militarized standoff affects their grounds and safety with no consultative channel. The strict-exclusivity constituency speaks chiefly through the protests of the states comprising it rather than through any neutral venue, so its objections arrive pre-filtered through the disputants' own voices.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, transit through contested straits and zones would revert to case-by-case negotiation, insurance and freight costs on affected corridors would jump immediately, artificial-island and baseline claims currently held in check by recurring challenges would consolidate within months, and the two sibling readings of the boundary kernel would compete openly to fill the vacated enforcement role. The maritime order would reorganize around whichever reading seized the enforcement position — the world does not merely continue without this arrangement.
% FOUNDING_PROBLEM: Post-war expansion of coastal jurisdiction — proclamations extending resource control seaward, creeping two-hundred-mile claims, straight-baseline systems closing formerly international waters — threatened to enclose the straits and high-sea corridors on which global naval and commercial movement depended. The 1982 convention attempted a package trade of guaranteed passage against seabed-mining terms its leading naval power judged unacceptable; this reading arose to hold the passage half through custom and presence without accepting the package.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the UN Division for Ocean Affairs continues logging new baseline and zone-claim notifications each year; annex VII arbitral and ITLOS proceedings document live boundary and passage contests initiated by third parties; and the protesting coastal states themselves — adversaries of this reading — attest by their continuing objections that the anti-enclosure tension remains unresolved. Adversarial confirmation of this kind is stronger than beneficiary attestation, and it exists in volume.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-high (0.58) because the arrangement delivers asymmetric collection: enforcing non-parties gather passage rights and strategic access while shedding the seabed-mining and dispute-settlement burdens ratifiers carry, and coastal states absorb sovereignty friction their legal theories do not concede. Suppression (0.55) is real but incomplete: persistence depends on active naval coercion and on making exclusivity attempts costly, yet the rival arrangements remain fully expressible in diplomacy and tribunals — hence accessibility_collapse is low (0.35) rather than mountain-grade. Theater (0.42) reflects that a growing share of presence activity is assertive signaling — transits that change little materially but contest claim consolidation — layered over genuinely functional patrol, survey, and familiarization activity. Resistance is high (0.70): island-building, coast-guard shadowing, interception maneuvers, and diplomatic protest are continuous, organized, and escalating. The claimed type is tangled_rope on structure alone: a genuine commons-coordination function (open lanes benefit all trading states including the objectors) combined with asymmetric extraction (obligation-free enforcement) held together by active enforcement — all three canonical gates are declared. The measurement series run on one shared time grid (points 0, 10, 20, 30, 40, 46) so every tracked metric is authored at every examined time point; the trajectory is monotonically rising rather than cyclical, driven by accumulating jurisdictional pressure meeting standing enforcement rather than by oscillating crisis-reform phases.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the naval-power seat the arrangement is the maintenance of a long-standing open-oceans practice — the effective extraction experienced there sits near the subsidy end, since the same actors write the challenge lists and collect the mobility. From the exclusivity-asserting coastal-state seat the identical structure operates as unconsented imposition: codified-style entitlements overridden by a power that declined the codification, with trapped exit (coastlines do not move), so effective extraction amplifies toward the target end. Ratifying maritime states occupy the middle — they receive the enforcement umbrella and open lanes but carry the treaty obligations the enforcer sheds, so their position is near-symmetric. The engine derives this divergence from the declared roles, power levels, and exit options; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly onto structural relationships. blue_water_naval_powers appear in the beneficiary set and administer the enforcement schedule, giving them a directionality near the beneficiary pole — amplified further by arbitrage-grade exit, since they can tune whom they challenge and when. eez_exclusivity_coastal_states and warship_authorization_requirement_states appear in the victim set with trapped exit (fixed geography, sovereignty-framed claims that cannot be abandoned without domestic cost), putting them near the target pole. global_commercial_shipping benefits incidentally and diffusely with no enforcement role — near-zero directionality. ratifying_maritime_states are genuinely dual-positioned (declared beneficiary with secondary payer role), landing mid-scale. Note on scaling: suppression enters the computation as a raw structural property and is not scaled by power or scope; only extraction is scaled, by directionality and spatial scope — the global scope of the enforcement surface modestly amplifies effective extraction on the target-side seats because verification and redress at that scope are hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two symmetrical mislabelings. Calling the arrangement pure coordination would bury the obligation asymmetry — non-parties collecting the benefits of a bargain they declined — which is exactly the extraction a rope reading would normalize. Calling it pure extraction would erase the open-lane function that even the protesting coastal states' own export trade depends on, converting a real commons service into villainy and mispredicting coalition behavior. The tangled-rope reading holds both faces in view. On mandate obsolescence: the founding problem — countering the progressive enclosure of straits and sea corridors — is demonstrably live (new baseline systems, artificial-island construction, security-zone claims continue to accumulate), so this is not a resolved mandate kept alive by inertia; what has degraded is the functional-to-performative ratio of the enforcement activity itself, which the theater_ratio series tracks rising from 0.22 to 0.42 across the interval. The constraint is therefore not drifting toward piton; it is drifting toward harder-edged contest, with extraction and suppression both rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    custom_status_empirical_grounding,
    'Do freedom-of-navigation rules actually constitute customary international law binding on states that never ratified UNCLOS — sufficient state practice plus opinio juris — or is the custom claim a doctrinal assertion sustained chiefly by the enforcing powers themselves?',
    'Systematic state-practice and opinio-juris survey across ratifying and non-ratifying states; examination of how international tribunals treat a non-party invoking and enforcing treaty provisions as custom; ICJ or ITLOS clarification of non-party customary obligation.',
    'If the custom grounding fails, the arrangement loses its legal warrant and computes as raw power enforcement leaning snare; if it holds, the coordination legitimacy of the tangled-rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_status_empirical_grounding, empirical, 'Whether the customary-law warrant invoked by non-ratifier enforcement reflects genuine accumulated consent or enforcement-manufactured doctrine.').

omega_variable(
    sibling_reading_counterfactual_structure,
    'How would this constraint''s beneficiary/victim structure reorganize under the sibling readings of the unclos_sovereignty_boundary kernel?',
    'Instantiate the sibling stories: under strict_eez_reading the naval powers become targets and coastal states beneficiaries; under historical_rights_reading the victim set shifts toward states neighboring historic-claim holders and naval mobility contracts inside claimed waters. Compare computed per-seat classifications across the family.',
    'This story''s tangled-rope verdict is indexical to the enforcement-centered framing; sibling framings redistribute who extracts and who pays, so cross-family comparison rather than any single story settles the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_counterfactual_structure, conceptual, 'Kernel-relative framing dependence: the structural delta declared for each sibling reading.').

omega_variable(
    self_validating_enforcement_loop,
    'Does recurrent naval presence manufacture the very practice that constitutes the custom it invokes — is the evidence for the custom partly produced by the enforcement that claims to discover it?',
    'Compare custom-formation dynamics in maritime domains lacking an enforcing hegemon against freedom-of-navigation practice; test whether non-enforcing states'' independent practice evidences opinio juris without the presence operations.',
    'If the loop is self-validating, the arrangement''s legal legitimacy is circular and the coercion component of any extraction assessment should be weighted upward; if independent corroboration exists, the custom grounding is firmer than the enforcement story suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_validating_enforcement_loop, conceptual, 'Circularity risk between the enforcement practice and the customary status it presupposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(uncl_tr_t46, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 46, 0.42).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(uncl_be_t46, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 46, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(uncl_su_t46, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 46, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_infrastructure).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'freedom of navigation under UNCLOS' decomposes into three structurally distinct constraint stories sharing the unclos_sovereignty_boundary kernel: strict_eez_reading (codified exclusivity; epsilon borne by denied navies and shippers), historical_rights_reading (usage priority; epsilon borne by neighbors of historic claimants), and this file (customary passage enforced by non-ratifier presence; epsilon borne by exclusivity-asserting coastal states). Each story has a single stable epsilon over its own referent arrangement, its own beneficiary/victim sets, and its own enforcement logic. Measuring the boundary through different observables yields different extraction profiles — that is the signal that these are different constraints, not one constraint viewed from angles. Family members link via affects_constraints; this reading sits downstream of neither sibling textually but exerts structural pressure on the historical-rights sibling (see reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
