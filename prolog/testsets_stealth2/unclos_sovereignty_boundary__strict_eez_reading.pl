% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: Strict EEZ Reading — Exclusive 200-Nautical-Mile Zones (UNCLOS Article 57)
 *   domain: international law/maritime governance/geopolitical strategy
 *
 * SUMMARY:
 *   This story instantiates the strict_eez_reading of the contested kernel
 *   unclos_sovereignty_boundary: the claim that maritime zones extend 200
 *   nautical miles from coastal baselines, are exclusive and enforceable
 *   under the convention's Article 57, and admit no overlay claims from
 *   historical usage or non-ratifying custom. The arrangement solves a real
 *   ocean-governance problem — open-access fisheries collapse and
 *   uninvestable offshore hydrocarbons — while simultaneously transferring
 *   enormous resource value from prior users and overlapping neighbors to
 *   adjacent coastal states, enforced by coast guards, licensing regimes, and
 *   adjudicative organs. Per the epsilon-referent rule for kernel readings,
 *   the extractiveness score below is authored for the standing strict-EEZ
 *   arrangement AS THE STRICT READING ITSELF ASSESSES IT: this reading
 *   endorses the arrangement's legality, so its epsilon is lower than a
 *   historical-rights or non-ratifier reading would assign to the same
 *   referent, but it is not zero — the reading can see that enclosure
 *   displaced real users and that enforcement suppresses rival frameworks.
 *   The sibling readings are separate constraints in separate files, linked
 *   through the network section; nothing about them is averaged into this
 *   story.
 *
 * KEY AGENTS:
 *   - eez_coastal_states: Primary beneficiary and agenda-setter (institutional/constrained) — proclaims, administers, and polices the zones; collects license fees and resource rents
 *   - small_island_developing_states: Concentrated beneficiary (organized/trapped) — ocean territory dwarfs land; license revenue is a fiscal pillar; geography immovable
 *   - overlapping_claimant_states: Primary target (powerful/trapped) — claims fall inside neighbors' 200-mile circles; treaty line prevails over their assertions
 *   - traditional_fishing_communities: Diffuse target (powerless/constrained) — historic grounds now lie inside foreign zones; gear and knowledge tied to specific banks
 *   - distant_water_fishing_states: Dual-positioned payer-beneficiary (organized/mobile) — lost open access, gained predictable licensed access; fleets relocatable
 *   - nonparty_naval_powers: Excluded challenger (institutional/mobile) — never ratified, asserts navigation freedoms regardless, sits outside the treaty's dispute organs
 *   - itlos_adjudicative_bodies: Analytical observer (institutional/analytical) — interprets the 200-mile rule in hard cases; authority rests on parties bringing cases and honoring outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.63).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.79).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict EEZ Reading — Exclusive 200-Nautical-Mile Zones (UNCLOS Article 57)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international law/maritime governance/geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '7bad5bfe-499d-4644-81d6-670e989a04df').
narrative_ontology:cs_kernel_codification('7bad5bfe-499d-4644-81d6-670e989a04df', fixed_text).
narrative_ontology:cs_authority_grounding('7bad5bfe-499d-4644-81d6-670e989a04df', lineage).
narrative_ontology:cs_interpretation_layer_present('7bad5bfe-499d-4644-81d6-670e989a04df').
narrative_ontology:cs_reading_relation('7bad5bfe-499d-4644-81d6-670e989a04df', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('7bad5bfe-499d-4644-81d6-670e989a04df', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('7bad5bfe-499d-4644-81d6-670e989a04df', foundational, eez_exclusivity_within_200nm).
narrative_ontology:cs_axiom_status(eez_exclusivity_within_200nm, holdable).
narrative_ontology:cs_axiom_grounding('7bad5bfe-499d-4644-81d6-670e989a04df', eez_exclusivity_within_200nm, conventional).
narrative_ontology:cs_axiom('7bad5bfe-499d-4644-81d6-670e989a04df', foundational, no_overlay_claims_valid).
narrative_ontology:cs_axiom_status(no_overlay_claims_valid, holdable).
narrative_ontology:cs_axiom_grounding('7bad5bfe-499d-4644-81d6-670e989a04df', no_overlay_claims_valid, conventional).
narrative_ontology:cs_axiom('7bad5bfe-499d-4644-81d6-670e989a04df', secondary, ratification_confers_enforceability).
narrative_ontology:cs_axiom_status(ratification_confers_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('7bad5bfe-499d-4644-81d6-670e989a04df', ratification_confers_enforceability, conventional).
narrative_ontology:cs_reference_frame('7bad5bfe-499d-4644-81d6-670e989a04df', article57_exclusive_zone_settlement).
narrative_ontology:cs_drift_state('7bad5bfe-499d-4644-81d6-670e989a04df', contemporary_gray_zone_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7bad5bfe-499d-4644-81d6-670e989a04df', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, eez_coastal_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the convention and proclaimed 200-nautical-mile zones off their coasts. Within the zone they license foreign fishing, lease seabed blocks, police illegal catches, and set conservation measures; license fees and resource rents flow into national budgets. Renouncing the framework would mean forfeiting recognized title to waters they already police, so they defend the text while pushing its edges through baseline choices and extended-shelf submissions.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, eez_coastal_states, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, eez_coastal_states, beneficiary).

% Pacific and Caribbean island nations whose ocean territory dwarfs their land area. License fees from tuna fleets can approach a third of government revenue, and the boundary arcs drawn around tiny islands are their principal national asset. They cannot relocate their geography, and their entire fiscal position depends on sea features counting as full zones.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states, beneficiary,
    organized, generational, trapped, regional).

% Operate industrial fleets far from home waters. Before the zone regime they fished openly off foreign coasts; now they buy licenses, sign access agreements, or form joint ventures with coastal states. Fleets have relocated repeatedly as access terms hardened. They gain predictability from the license system while paying for access they once took as given.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_states, beneficiary).

% States whose coastlines face semi-enclosed seas where 200-mile circles cannot all fit — the South China Sea, the East China Sea, the Aegean. Their asserted zones overlap neighbors'. Under the strict reading the treaty line prevails and their competing assertions carry no legal weight; several reject adverse awards and back their positions with coast guards, militias, and constructed features.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states, payer,
    powerful, civilizational, trapped, regional).

% Village and artisanal fleets that worked grounds now lying inside a neighbor's zone. Their gear, home ports, and seasonal knowledge are tied to specific banks and migration routes; when enforcement excludes them, the alternatives are refitting for distant waters at ruinous cost or leaving the trade entirely.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities, payer,
    powerless, generational, constrained, local).

% Major maritime powers that never ratified the convention yet operate globally and insist navigation freedoms survive regardless. They sail deliberate passages through contested zones to assert their reading, hold that non-membership leaves treaty-based enforcement claims against them groundless, and publish interpretive guidance for allies while staying outside the convention's dispute organs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, nonparty_naval_powers, excluded,
    institutional, civilizational, mobile, global).

% The tribunal, the continental shelf commission, and arbitration panels constituted under the convention's annexes. They receive submissions, issue interpretations and awards, and publish recommendations that define how the 200-mile rule reads in hard cases. Their authority rests entirely on parties continuing to bring cases and honor outcomes.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, itlos_adjudicative_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, eez_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns a single responsible manager to the waters and seabed within 200 nautical miles of each coast, converting open-access fisheries races into licensed, quota-managed activity, giving offshore hydrocarbon development a stable jurisdiction to invest in, and turning boundary conflicts into adjudicable legal questions rather than purely naval ones.
% TRANSFER_FUNCTION: Moves exclusive control of fisheries, seabed energy, and marine research access from all prior users — neighboring claimants, distant-water fleets, traditional grounds-users — to the adjacent coastal state, along with license fees, access-agreement payments, and resource rents flowing to coastal treasuries.
% ABSENT_VOICES: Transboundary artisanal fishing associations and indigenous maritime peoples had minimal representation in the 1970s negotiations and remain voiced only through states that frequently argue against their access interests. Non-ratifying naval powers sit outside the rooms where zone rules are interpreted. The interests of future generations in deep-sea ecosystems appear only through the Area regime's institutional proxies.
% DISAPPEARANCE_RATIONALE: If exclusive zones vanished overnight, roughly a third of the ocean's fisheries and most known offshore hydrocarbons would revert to contested open access; coastal states would immediately re-proclaim unilateral zones under domestic law; semi-enclosed seas would shift from legal argument to sustained coast-guard confrontation; and the license-revenue economies of Pacific island states would collapse pending entirely new bargains.
% FOUNDING_PROBLEM: Between 1945 and the 1970s, unilateral proclamations beginning with the Truman Proclamation and escalating 200-mile claims by coastal states threatened to fragment the oceans into conflicting national enclosures; distant-water fleets were depleting stocks just offshore; the cod and tuna wars showed gunboats settling what law had not. UNCLOS III negotiated a package: coastal states received 200-mile resource zones, maritime powers received guaranteed transit, and the deep seabed was designated common heritage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: FAO global stock assessments document the continued overfishing pressure the zones were built to answer; diplomatic histories of the cod wars and the 1970s claims crisis rest on archives held by non-coastal and non-beneficiary institutions; and non-party naval powers engage the framework's terms in official limits briefs even while disputing ratification — attesting the problem's persistence from a seat outside the beneficiary set.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: substantial but bounded. The strict reading sees the transfer of resource control to coastal states as largely the lawful price of a workable ocean order, yet it cannot deny that overlapping claimants lose access without consent, that traditional users were displaced, and that license and rent flows concentrate in coastal treasuries. Suppression 0.79 is high and STRUCTURAL — the reading's defining move is declaring overlay claims invalid, and the enforcement machinery (patrols, coast-guard statutes authorizing force, radar chains, licensing denial) exists to hold that declaration against persistent challengers; suppression is authored as a raw structural property and is not scaled by power or scope. Theater 0.28: most enforcement is functional (real arrests, real licensing), but a growing share is performative — map annexations, naming exercises, symbolic sailing assertions. Accessibility_collapse 0.55: alternatives do not vanish — bilateral access deals, joint development zones, and adjudication routes remain — but once the strict reading governs, overlay claims are legally dead on arrival. Resistance 0.70: gray-zone operations, constructed features, rejected awards, and deliberate non-ratification are active, organized resistance from powerful seats. The measurement series run on ONE shared time grid (seven points, 1982-2026) with all three tracked metrics authored at every point; all three rise together, telling one story: as zone value grew (hydrocarbons, industrial tuna, strategic position), enforcement capacity was built up and extraction accumulated on top of the original coordination bargain. The rising suppression_requirement series is authored deliberately — this story tracks enforcement-capacity growth, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the coastal agenda-setter seat the arrangement is a legitimate administrative regime it built and staffed — coordination with costs it accepts. From the overlapping-claimant seat the same 200-mile line operates as enforced dispossession backed by gunboats: a trap with no geographic exit. Small island states and overlapping claimants occupy the same region at wildly different directionalities — one lives off the line, the other is cut by it. Distant-water fleets straddle: they pay for access they once took freely, yet the license system gave them certainty worth buying. Non-party naval powers experience legitimacy denial (their framework is declared invalid) while retaining full material freedom of operation — a gap between legal and physical position that no other seat exhibits. The engine computes these per-seat classifications from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive coastal states and island states toward the beneficiary end of d; their constrained/trapped exits deepen the subsidy reading for island states (deepest beneficiary position in the set — trapped AND subsidized). Victim declarations drive overlapping claimants and traditional fishing communities toward the target end; trapped geography amplifies claimants toward full-target. Distant-water fishing states appear in BOTH arrays deliberately: their loss of open access (victim) and their gain of licensed certainty (beneficiary) are both real, carried by the dual role on the stakeholder surface; the derivation should land them mid-range rather than at either pole. No directionality overrides are authored: the power atoms were chosen so the structural derivation suffices, and the one residual uncertainty — how the derivation treats the excluded non-party naval seat, which appears in neither array — is flagged in commentary rather than patched with an override, since an override keyed to their power atom would also capture other seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — postwar fragmentation of the oceans, offshore stock collapse, gunboat boundary settlement — is still live: FAO assessments show continued overfishing pressure and offshore energy competition has intensified. Status live plus disappearance verdict world_rearranges yields no zombie flag under the mismatch consumer. The arrangement carries no sunset clause and is not transitional — it presents itself as permanent settlement, which is exactly why the drift series matters: if the stewardship half (stock management, conservation measures) atrophies while enforcement keeps growing, the coordination function decays and the structure slides toward pure extraction riding on inherited legitimacy. The rising base_extractiveness series is the T17-relevant signal: accumulation, not yet critical. The classification prevents mislabeling in both directions — reading the arrangement as pure extraction ignores the documented recovery of managed stocks inside enforced zones; reading it as pure coordination ignores who pays for the order and who never consented to the line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (strict_eez_reading) of the kernel unclos_sovereignty_boundary. Would instantiating a sibling reading — historical_rights_reading or non_ratifier_enforcement_reading — change the beneficiary/victim structure and the classification?',
    'Author the sibling stories as separate files and compare compiled beneficiary/victim sets and per-seat classifications. The disagreement is located in the warrant for exclusivity: treaty text alone (this reading) versus pre-treaty historical usage versus customary freedoms enforceable without ratification.',
    'Under historical_rights_reading, prior-user states leave the victim set and enter the beneficiary set, and strict-line victims regain access claims. Under non_ratifier_enforcement_reading, non-party naval powers exit the excluded seat and the enforcement perimeter contracts to ratifying parties only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the sovereignty-boundary kernel; sibling readings redistribute the beneficiary/victim sets.').

omega_variable(
    baseline_drawing_ambiguity,
    'Do the 200 nautical miles run from normal low-water baselines or from straight and archipelagic baselines, and which maritime features generate full zones rather than mere rocks?',
    'Continental shelf commission submissions, tribunal jurisprudence on baselines and feature status, and observed state practice in semi-enclosed seas.',
    'Baseline and feature-status determinations move millions of square kilometers between the beneficiary and victim sides; a rock-status ruling deletes whole zones and their license revenues from the beneficiary set, converting beneficiaries into victims overnight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_drawing_ambiguity, empirical, 'Where the line is drawn determines who sits inside the beneficiary set.').

omega_variable(
    enforcement_ratchet_or_response,
    'Does the rising suppression series reflect an enforcement ratchet that entrenches the strict reading beyond what defiance requires, or merely proportional response to escalating challenges?',
    'Compare enforcement expenditure and incident rates against compliance rates inside enforced zones; use natural experiments where enforcement paused after adverse awards and observe whether overlay claims revived.',
    'If ratchet, effective suppression exceeds what the contest alone explains and the arrangement hardens toward pure exclusion; if proportional response, suppression tracks contest intensity and would fall if sibling-reading constituencies stood down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_or_response, empirical, 'Interpretation of the rising suppression_requirement trajectory across the interval.').

omega_variable(
    interpretive_layer_absorption,
    'Does the tribunal and commission interpretation layer absorb overlay pressure without surfacing kernel revision (stabilizing the strict reading), or accumulate unacknowledged drift that will surface discontinuously?',
    'Track whether awards and commission recommendations remain within the plain terms of the 200-mile rule or progressively accommodate historic-title and security exceptions.',
    'Absorption supports persistence of the strict reading as written; accumulated unacknowledged drift predicts a sudden revision event rather than gradual adaptation, changing the terminal attractor for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_layer_absorption, conceptual, 'Whether the interpretation layer stabilizes or masks drift in the strict reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1990, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(uncl_tr_t1998, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1998, 0.16).
narrative_ontology:measurement(uncl_tr_t2006, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(uncl_tr_t2014, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2014, 0.23).
narrative_ontology:measurement(uncl_tr_t2020, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(uncl_tr_t2026, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.42).
narrative_ontology:measurement(uncl_be_t1990, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1990, 0.46).
narrative_ontology:measurement(uncl_be_t1998, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(uncl_be_t2006, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2006, 0.54).
narrative_ontology:measurement(uncl_be_t2014, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(uncl_be_t2020, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(uncl_be_t2026, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2026, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.48).
narrative_ontology:measurement(uncl_su_t1990, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement(uncl_su_t1998, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(uncl_su_t2006, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2006, 0.64).
narrative_ontology:measurement(uncl_su_t2014, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(uncl_su_t2020, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(uncl_su_t2026, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2026, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'law of the sea sovereignty' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon values and different beneficiary/victim sets. This file is the strict treaty-text reading (epsilon 0.63 from its own lights; overlapping claimants and displaced traditional users as victims). The historical-rights sibling restores prior users to the beneficiary side and reopens closed grounds; the non-ratifier sibling shrinks the enforcement perimeter to ratifying parties and elevates excluded naval powers to principals. The strict reading is upstream — it is the arrangement the other two contest, and each sibling cites the strict line as the thing it overrides or circumvents. All three files link one another through affects_constraints; none is complete alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
