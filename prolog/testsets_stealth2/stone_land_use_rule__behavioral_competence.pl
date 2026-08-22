% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Inundation-Line Stone as Live Land-Use Prohibition (Behavioral-Competence Reading)
 *   domain: disaster anthropology / institutional memory / land-use governance
 *
 * SUMMARY:
 *   On the Sanriku coast, villages that lost hundreds to the 1896 and 1933
 *   tsunamis erected inscribed stones at the waves' high-water marks,
 *   commanding later generations not to build below. This story authors the
 *   behavioral_competence reading of that artifact: the stone as a live
 *   land-use prohibition, renewed by daily spatial practice. In the paradigm
 *   case the village rebuilt uphill of the marker and for 78 years
 *   (1933–2011) kept every home above the line, at the accepted cost of a
 *   steep climb and capped lowland value; in 2011 the Tōhoku tsunami stopped
 *   just short of the houses. The constraint is constructed, not natural — a
 *   stone was erected and a practice built around it — yet compliance is
 *   sustained without coercion: no patrol enforces the line, and the
 *   alternative of building below remains physically available but is
 *   rendered unthinkable by transmitted understanding. Claim and metrics are
 *   authored independently: the constraint is claimed as a rope (a genuine
 *   collective-action solution with symmetric accepted costs and no
 *   collecting party), and the metrics describe that same low-extraction,
 *   low-suppression, low-theater operation as this reading sees it. The
 *   sibling commemorative_husk reading — the stone as memorial without
 *   behavioral force — is a different constraint with its own file; the
 *   contest between readings is carried in omega variables, not inside this
 *   one. KEY AGENTS (by structural relationship):
 *   village_households_above_stone — primary beneficiary
 *   (organized/identity_locked), bears the climb, receives survival of homes
 *   and graves; stone_keeper_association — agenda-setter
 *   (organized/identity_locked), maintains and teaches the line, collects no
 *   rent; shore_dependent_fishing_households — beneficiary with elevated
 *   cost-bearing (moderate/constrained), dual position declared;
 *   lowland_parcel_owners — excluded voice (moderate/constrained),
 *   value-capped land, no seat in the practice; municipal_planning_office —
 *   observer (institutional/analytical), hazard maps and relocation schemes
 *   that could reinforce or supersede the line.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.13).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.08).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.13).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Inundation-Line Stone as Live Land-Use Prohibition (Behavioral-Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster anthropology / institutional memory / land-use governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'c2405a74-449b-4af7-9818-2a66cd0a87b5').
narrative_ontology:cs_kernel_codification('c2405a74-449b-4af7-9818-2a66cd0a87b5', fixed_text).
narrative_ontology:cs_authority_grounding('c2405a74-449b-4af7-9818-2a66cd0a87b5', practice).
narrative_ontology:cs_interpretation_layer_present('c2405a74-449b-4af7-9818-2a66cd0a87b5').
narrative_ontology:cs_reading_relation('c2405a74-449b-4af7-9818-2a66cd0a87b5', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('c2405a74-449b-4af7-9818-2a66cd0a87b5', foundational, stone_command_has_behavioral_force).
narrative_ontology:cs_axiom_status(stone_command_has_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('c2405a74-449b-4af7-9818-2a66cd0a87b5', stone_command_has_behavioral_force, instrumental).
narrative_ontology:cs_axiom('c2405a74-449b-4af7-9818-2a66cd0a87b5', secondary, generational_transmission_obligation).
narrative_ontology:cs_axiom_status(generational_transmission_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c2405a74-449b-4af7-9818-2a66cd0a87b5', generational_transmission_obligation, deontological).
narrative_ontology:cs_reference_frame('c2405a74-449b-4af7-9818-2a66cd0a87b5', binding_inundation_boundary).
narrative_ontology:cs_drift_state('c2405a74-449b-4af7-9818-2a66cd0a87b5', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c2405a74-449b-4af7-9818-2a66cd0a87b5', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, village_households_above_stone).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, shore_dependent_fishing_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, stone_keeper_association).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, shore_dependent_fishing_households).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, embodied_hazard_memory_transmission).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, inundation_line_adequacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live uphill of the inscribed stone marking the highest reach of the 1933 tsunami. Their houses, gardens, and family graves sit above every inundation since. Daily life routes past the marker; parents walk children along the line and retell the waves. Building lower would be cheaper and more convenient, and the grade makes construction, water-hauling, and daily movement harder. Leaving would mean leaving fishing grounds, graves, and the community that carries the practice.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, village_households_above_stone, beneficiary,
    organized, generational, identity_locked, local).

% Village volunteers who maintain the marker — repainting the carved command, clearing brush, repairing after storms — and organize the walk-throughs and retellings that teach each new child where the line runs and why. They decide how the boundary is marked and explained. They collect no payment; their return is the village's continuance, and most are drawn from the households above the line.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, stone_keeper_association, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, stone_keeper_association, beneficiary).

% Work the water below the line: boats, nets, and landing points sit downhill of the marker, so every working day begins with a steep descent and ends with a climb carrying gear and catch. They may not build stores or homes near the shore where the work happens. They accept the grade as the price of houses that have survived every wave since 1933; some elders now make the climb with increasing difficulty.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, shore_dependent_fishing_households, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, shore_dependent_fishing_households, payer).

% Hold land below the marker — river flats and shore lots that cannot be built out while the rule holds. The line caps what their parcels are worth and what can be done with them. They have no seat in the village practice that maintains the marker; their objection — that the land is dry and usable in most years — is voiced, if at all, from outside the rooms where the line is taught and kept.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, lowland_parcel_owners, excluded,
    moderate, biographical, constrained, local).

% Produces the official hazard maps and relocation schemes for the coast. Its modeled lines do not always coincide with the stone's line, and its subsidies can move households further uphill or, if redrawn, invite them back down. It does not maintain the marker and does not enforce it, but its planning decisions can reinforce the practice or supersede it.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, municipal_planning_office, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of transmitting a spatially precise hazard boundary to generations who will never witness the hazard: the stone fixes the maximum inundation line in the landscape so every household, without instruments or expertise, knows which land may be occupied. A secondary coordination function maintains the knowledge itself — walk-throughs, retellings, repainting — so the line stays legible across the memory horizon.
% TRANSFER_FUNCTION: Moves physical effort and convenience from every household — uphill building, daily climbs, capped lowland parcel value — and converts it into a survival margin held collectively by the same households and their descendants. Attention is also moved: each generation's attention is captured by the marker and redirected to a hazard none of them has seen. No third party receives anything.
% ABSENT_VOICES: Lowland parcel owners and would-be shorefront developers would object that the land below the line is dry and buildable in most years and that the rule caps its value; they hold no seat in the practice that keeps the line, and their objection surfaces only indirectly through municipal planning pressure. Within the village, the voices closest to exit — younger households weighing convenience — are present but historically outvoted by the practice itself; after 2011 the internal objection has gone quiet.
% DISAPPEARANCE_RATIONALE: Within two or three generations — the observed memory horizon — building would creep below the line as lowland convenience and parcel value reassert themselves, exactly as happened in nearby villages whose markers lost force; the next major tsunami would then find occupied homes below it. The village's uphill arrangement, its land-use pattern, and its survival margin are all held in place by the stone and the practice that keeps its command legible.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis destroyed coastal villages and killed thousands, survivors needed descendants — who would never personally witness an inundation — to know and heed the hazard's maximum reach. Written records decay and migrate; the founders fixed the line in the landscape itself, as a command addressed to anyone who would build.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the 2011 Tōhoku tsunami itself (physical evidence that the hazard and the line still matter), paleotsunami sediment research and prefectural hazard mapping establishing recurring large inundations on the Sanriku coast, and the comparative record of nearby villages whose residents built below their stones and were killed in 2011. The founding problem's liveness is attested by the hazard record, not only by the village that benefits from the rule.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.13, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.13 at interval end) because the rule's costs — the grade under every construction and daily trip, capped value on lowland parcels — are borne by the same households who hold the benefit, and no seat collects them; the receipt surface is affirmatively diffuse after checking every seat. Suppression is authored as a raw structural property, unscaled by power or scope (the engine scales only extractiveness): 0.08 because there is no coercive machinery at all — no patrol, no permit regime, no sanction beyond ordinary social regard; the alternative stays physically open and is closed by understanding, not force. Theater is 0.04 because the command is acted on daily — the inverse of performative maintenance. Accessibility_collapse is 0.55: for households inside the practice, building below collapses as a serious option once the recurrence is understood, but the understanding must be re-transmitted each generation and is not shared by outsiders, so the alternative revives at the memory horizon rather than staying closed. Resistance is 0.12: recurring grumbling at the grade, strongest among shore-dependent and elderly households, never organized, silent after 2011. The measurement series run on one shared time grid (t = 0–78 years after erection; both tracked metrics authored at every point). The theater series oscillates rather than drifts: salience decays between events and is refreshed by hazard recurrence (1960 and 2010 Chile-origin tsunamis at t=27 and t=77; the 2011 Tōhoku tsunami at t=78) — the oscillation is driven by exogenous geophysics, not by intermittent-reinforcement extraction. base_properties values are measured at interval end (post-2011), the point of maximum salience. The suppression_requirement series is deliberately absent: no enforcement machinery was ever built up or dismantled, so the enforcement picture is static and is captured by the scalar. Identity-lock note: what binds the households is relational and institutional — the village IS the practice of living above the line and teaching it; were that frame to break (a generation treating the marker as superstition), compliance would decay and this reading would drift toward its sibling. fixing_cost is authored 'prohibitive': the only agent who could remove the rule is the village itself, and the cost of removal — re-exposure of rebuilt homes to a recurring catastrophic hazard — dwarfs the benefit of fixing (marginal lowland convenience); this is benefit-negativity, not fixing-machinery cost, and is distinguished from the inertial signature by the live founding problem and near-zero theater.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From the household seat the rule is a bargain they would re-choose: a real but accepted cost for survival of homes and graves. From the fishing-household seat the same rule carries a recurring physical levy — two steep climbs a day — against the same benefit, which is why the dual position is declared rather than flattened. From the excluded lowland-owner seat the stone reads as a pure cap on value imposed by a conversation they are not in; nothing in their situation shows them the survival benefit, so the same artifact presents as confiscatory. From the municipal observer seat the stone is informal regulation competing with modeled hazard lines — sometimes stricter, sometimes looser. The engine derives each seat's classification from power, exit, and role; this story only declares the situations.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: village households hold directionality near zero (benefit without extraction; identity-locked exit deepens the subsidy reading because leaving forfeits the benefit along with the community), and the keeper association administers without collecting. Shore-dependent fishing households are declared with a dual position — beneficiary with elevated cost-bearing — so their derived directionality sits mid-low rather than at the floor: they hold the survival benefit and pay a daily convenience levy. No stakeholder is declared a victim and no seat receives the extraction: climb-effort and capped parcel value accrue to no one — gain_flow is authored 'diffuse' as an affirmative claim after re-reading every stakeholder situation. The excluded lowland owners bear a real cost (the value cap) but sit outside the arrangement's conversation; per the R3 ruling that authored absence is commentary-grade, their position is recorded as a stakeholder situation and an absent voice, not as a victim declaration that would drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Reading the rule as pure extraction (a prohibition confiscating lowland value) would miss that every declared cost-bearer is inside the beneficiary set and that no seat collects; the coordination function — transmitting a survivable building line to people who will never see the hazard — is real and load-bearing. Reading it as a natural limit would miss that it is constructed and must be actively re-transmitted; its 'naturality' is precisely what the sibling reading denies. The founding problem is live (the hazard recurred in 1960, 2010, and 2011), so there is no mandatrophy to resolve: the arrangement has not outlived its function. The characteristic decay path for this constraint is not function-atrophy into a husk of routine but transmission-decay into the sibling commemorative_husk reading — the omega variables carry that drift path, and the theater series' t=70 local maximum (0.12) is its early signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this stone''s rule a live prohibition sustained by practice (the behavioral_competence reading instantiated here) or a memorial husk whose warning no longer binds conduct (the sibling commemorative_husk reading)?',
    'Behavioral evidence at the site: land-use records below the line across generations, whether transmission practices continue, whether any household builds below after each memory-fading generation; set against the comparative record of Sanriku villages with similar stones whose residents built below and were inundated in 2011.',
    'If the husk reading is correct for this site, the constraint is inert — extraction near zero but no coordination function — and the operative harm shifts to unwarned descendants building below the line; classification moves from live rope to the husk form and the victim set changes. The two readings are connected by a drift path (transmission decay), not by logical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the stone_land_use_rule kernel holds at this site: binding conduct or memorial symbol.').

omega_variable(
    cost_distribution_symmetry,
    'Is the cost of the steep climb borne symmetrically across households, or do elderly, disabled, and shore-dependent households bear disproportionate costs without commensurate offsetting benefit?',
    'Household-level cost accounting: daily descent burden and access times, construction cost differentials by parcel elevation, out-migration attributable to the grade, set against the uniform survival benefit of sitting above the line.',
    'A distinct cost-bearing subgroup with no offsetting benefit would give the constraint a victim-like seat inside the beneficiary set and raise effective extraction for that seat, pulling the classification toward a hybrid coordination/extraction form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_distribution_symmetry, empirical, 'Whether compliance costs are symmetric across the village or concentrated on shore-dependent and mobility-limited households.').

omega_variable(
    transmission_persistence,
    'Will the practice that enforces the rule survive demographic decline — aging, out-migration, in-marrying households — or does the enforcement mechanism decay toward the sibling commemorative_husk reading?',
    'Longitudinal observation of transmission: whether children are still walked along the line, whether the marker is maintained after each storm season, whether newcomers adopt the practice or treat it as folklore.',
    'Decay of transmission would not change the rule''s text but would change its operation: theater rises, compliance erodes, and the constraint drifts from this reading into the sibling husk reading — the two readings are joined by this drift path rather than separated by framework logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_persistence, empirical, 'Durability of the practice-based enforcement mechanism under demographic decline.').

omega_variable(
    boundary_adequacy,
    'Does the stone''s line remain an adequate hazard boundary under inundation scenarios exceeding the 1933 event it memorializes?',
    'Paleotsunami sediment records and numerical inundation modeling for the specific ravine, compared against the marker''s elevation and the 2011 run-up just below the village.',
    'If the line is under-conservative, the constraint''s low extraction reflects compliance rather than safety — a coordination mechanism organized around a false boundary whose failure mode is catastrophic and sudden rather than gradual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_adequacy, empirical, 'Whether the memorialized line matches the true worst-case inundation boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__behavioral_competence, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__behavioral_competence, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t27, stone_land_use_rule__behavioral_competence, theater_ratio, 27, 0.05).
narrative_ontology:measurement_basis(ston_tr_t27, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__behavioral_competence, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__behavioral_competence, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(ston_tr_t50, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.09).
narrative_ontology:measurement_basis(ston_tr_t60, observed).
narrative_ontology:measurement(ston_tr_t70, stone_land_use_rule__behavioral_competence, theater_ratio, 70, 0.12).
narrative_ontology:measurement_basis(ston_tr_t70, observed).
narrative_ontology:measurement(ston_tr_t77, stone_land_use_rule__behavioral_competence, theater_ratio, 77, 0.06).
narrative_ontology:measurement_basis(ston_tr_t77, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.04).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__behavioral_competence, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__behavioral_competence, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t27, stone_land_use_rule__behavioral_competence, base_extractiveness, 27, 0.12).
narrative_ontology:measurement_basis(ston_be_t27, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__behavioral_competence, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__behavioral_competence, base_extractiveness, 50, 0.13).
narrative_ontology:measurement_basis(ston_be_t50, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(ston_be_t60, observed).
narrative_ontology:measurement(ston_be_t70, stone_land_use_rule__behavioral_competence, base_extractiveness, 70, 0.15).
narrative_ontology:measurement_basis(ston_be_t70, observed).
narrative_ontology:measurement(ston_be_t77, stone_land_use_rule__behavioral_competence, base_extractiveness, 77, 0.14).
narrative_ontology:measurement_basis(ston_be_t77, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.13).
narrative_ontology:measurement_basis(ston_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, information_standard).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The artifact 'inundation-line stone' supports two structurally distinct constraints under one colloquial label, decomposed per the epsilon-invariance principle. This story (behavioral_competence) authors epsilon for the standing arrangement as the live-prohibition reading sees it: a binding land-use rule with accepted costs and near-zero extraction. The sibling (commemorative_husk) authors epsilon for the memorial-artifact reading: a symbol without behavioral force, whose operative harm is unwarned building below the line — different beneficiaries, different victims, different failure modes. The readings are linked because the same physical marker and the same transmission practice determine which one is true at a given site and time; transmission decay is the drift path from this reading into the sibling. They are two stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
