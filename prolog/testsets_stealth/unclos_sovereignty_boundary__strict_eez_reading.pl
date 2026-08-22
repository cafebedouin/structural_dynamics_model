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
 *   human_readable: Strict EEZ Reading: Exclusive Enforceable 200-Nautical-Mile Zones per UNCLOS Article 57
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   UNCLOS Article 57 grants coastal states sovereign rights over living and
 *   non-living resources within 200 nautical miles of their baselines. The
 *   strict reading treats these boundaries as exclusive and enforceable:
 *   historic-usage claims, nine-dash-line-style overlays, and other
 *   alternative sovereignty frameworks are invalid inside a delimited zone.
 *   The reading was decisively operationalized by the 2016 South China Sea
 *   arbitration, which held historic-rights claims incompatible with the
 *   Convention. Structurally the arrangement does real coordination work — it
 *   converted an open-access ocean commons into bounded stewardship zones,
 *   created investable fisheries and hydrocarbon regimes, and gave dozens of
 *   developing states (especially small island states) a fiscal foundation —
 *   while simultaneously transferring enormous resource wealth according to a
 *   geographic lottery and extinguishing traditional cross-boundary fishing
 *   access without compensation. Enforcement is active and maturing:
 *   annex-VII tribunals, ITLOS, coast-guard interdiction, and the diplomatic
 *   weight of the ratification coalition all defend the exclusivity principle
 *   against overlay claims. KEY AGENTS (by structural relationship): -
 *   favorable_geography_coastal_states: Primary beneficiary
 *   (institutional/arbitrage) — collects zone rents and administers the
 *   regime - small_island_developing_states: Secondary beneficiary
 *   (organized/constrained) — fiscal survival depends on zone exclusivity -
 *   unclos_adjudicative_bodies: Agenda-setter (institutional/analytical) —
 *   authors the reading's operative interpretations -
 *   overlapping_claimant_states: Primary target (powerful/constrained) —
 *   overlay claims declared invalid - traditional_fishing_communities:
 *   Primary target (powerless/trapped) — customary access extinguished -
 *   geographically_disadvantaged_states: Target (organized/constrained) —
 *   bear allocation outcome with no compensating zone -
 *   distant_water_fishing_nations: Payer with beneficiary residue
 *   (institutional/mobile) — buy back access they once held by custom -
 *   maritime_powers_non_parties: Excluded seat (powerful/arbitrage) —
 *   affected by a regime they never joined - law_of_the_sea_analysts:
 *   Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict EEZ Reading: Exclusive Enforceable 200-Nautical-Mile Zones per UNCLOS Article 57").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '68b82bd3-5791-46f7-b51c-ecad5b18eabe').
narrative_ontology:cs_kernel_codification('68b82bd3-5791-46f7-b51c-ecad5b18eabe', fixed_text).
narrative_ontology:cs_authority_grounding('68b82bd3-5791-46f7-b51c-ecad5b18eabe', lineage).
narrative_ontology:cs_interpretation_layer_present('68b82bd3-5791-46f7-b51c-ecad5b18eabe').
narrative_ontology:cs_reading_relation('68b82bd3-5791-46f7-b51c-ecad5b18eabe', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('68b82bd3-5791-46f7-b51c-ecad5b18eabe', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('68b82bd3-5791-46f7-b51c-ecad5b18eabe', foundational, codified_boundaries_supersede_historic_usage).
narrative_ontology:cs_axiom_status(codified_boundaries_supersede_historic_usage, holdable).
narrative_ontology:cs_axiom_grounding('68b82bd3-5791-46f7-b51c-ecad5b18eabe', codified_boundaries_supersede_historic_usage, conventional).
narrative_ontology:cs_axiom('68b82bd3-5791-46f7-b51c-ecad5b18eabe', secondary, eez_entitlement_is_geographic_not_historical).
narrative_ontology:cs_axiom_status(eez_entitlement_is_geographic_not_historical, holdable).
narrative_ontology:cs_axiom_grounding('68b82bd3-5791-46f7-b51c-ecad5b18eabe', eez_entitlement_is_geographic_not_historical, conventional).
narrative_ontology:cs_reference_frame('68b82bd3-5791-46f7-b51c-ecad5b18eabe', codified_200nm_exclusivity).
narrative_ontology:cs_drift_state('68b82bd3-5791-46f7-b51c-ecad5b18eabe', post_2016_arbitration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68b82bd3-5791-46f7-b51c-ecad5b18eabe', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, favorable_geography_coastal_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, geographically_disadvantaged_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States whose coastline length or archipelagic configuration yields very large exclusive zones. They issue fishing and hydrocarbon licenses, run enforcement patrols, lease seabed blocks, and collect the associated revenue directly. They negotiated the 200nm provisions at UNCLOS III, staff the interpretive and scientific bodies, and shape how the boundary text is applied. Leaving the regime would mean surrendering rents they currently collect, so they deepen institutional investment instead and retain full strategic optionality within it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, favorable_geography_coastal_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, favorable_geography_coastal_states, agenda_setter).

% Pacific and Caribbean island states whose maritime zones dwarf their land area. Tuna license fees and access agreements supply a large share of government revenue in several of them. Their economies depend almost entirely on the exclusivity of their zones, and they coordinate through subregional arrangements to price foreign access collectively. Loss of exclusivity would collapse their fiscal base; they have no comparable alternative revenue source at scale.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states, beneficiary,
    organized, generational, constrained, regional).

% ITLOS, Annex VII arbitral tribunals, and the Commission on the Limits of the Continental Shelf interpret and apply the boundary provisions. Their rulings — most prominently the 2016 South China Sea award — convert the strict reading from text into operative law. They command no enforcement arm of their own and depend on party compliance, coalition diplomacy, and the reputational weight of the ratification community.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_adjudicative_bodies, agenda_setter,
    institutional, generational, analytical, global).

% States whose asserted maritime claims overlap a neighbor's 200nm zone. The strict reading declares their overlay claims invalid, ruling out resource access they assert as national entitlement. They respond with coast-guard presence, island construction, rejection of adverse awards, and parallel legal argumentation, while remaining inside the treaty order because withdrawal would leave neighbors' zones uncontested and cost them more than non-compliance does.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states, payer,
    powerful, civilizational, constrained, regional).

% Coastal and island communities whose customary grounds crossed what are now several states' exclusive zones. The strict reading gives them no recognized title to negotiate with; access now depends on licenses priced beyond household means or on informal runs inside patrolled waters. Relocation means abandoning gear, accumulated knowledge of specific banks, and kin networks tied to particular landing sites. They are dispersed across jurisdictions with no transnational structure able to aggregate their position.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Landlocked states and states with marginal coastlines that caucused collectively at UNCLOS III for access guarantees. The strict reading confines them to narrow treaty carve-outs — transit provisions and surplus-sharing language — that rarely yield practical resource access. They bear the allocation outcome permanently, with coalition voice in forums but no geographic basis ever to hold a compensating zone of their own.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, geographically_disadvantaged_states, payer,
    organized, generational, constrained, global).

% Fleet states whose industries historically fished across waters now enclosed in other states' zones. They purchase access through bilateral agreements and licensing arrangements, passing costs to vessel operators and consumers. Their fleets can redeploy to waters with cheaper terms, which disciplines coastal pricing and preserves some bargaining position — but the same mobility concentrates displacement onto less mobile local fleets wherever they withdraw.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations, beneficiary).

% Major naval powers that never ratified the convention yet assert navigational freedoms they argue rest on custom. The strict reading's validity claim assigns them no voice in interpreting a boundary regime that shapes waters they operate in daily. They work through bilateral protest, naval presence programs, and selective acknowledgment of individual convention parts, leveraging capabilities outside the treaty structure entirely.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, maritime_powers_non_parties, excluded,
    powerful, generational, arbitrage, global).

% Academic commentators, legal scholars, and policy institutes tracking state practice, tribunal output, and compliance patterns over decades. They document the widening gap between the codified reading and operational behavior at sea, publish the compliance datasets other seats cite, and hold no enforcement or allocative power of their own.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, law_of_the_sea_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, favorable_geography_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an open-access ocean commons into bounded stewardship zones: fishery stocks get a manager with incentives to conserve, hydrocarbon deposits get an investor with secure tenure, and adjacent states get determinate boundaries that replace recurring resource confrontation with negotiable allocation.
% TRANSFER_FUNCTION: Moves resource access and revenue — fisheries rents, hydrocarbon royalties, license fees — from prior and would-be users (traditional fishing communities, distant-water fleets, overlapping claimants) to the coastal state holding the adjacent zone; moves license-fee income from foreign fleets into coastal and island-state treasuries.
% ABSENT_VOICES: Traditional fishing communities and indigenous maritime peoples had minimal representation in the negotiations that fixed the allocation, and none in the tribunals that apply it; non-party maritime powers are bound in practice by much of the regime's customary content while holding no formal seat; future generations of the geographically disadvantaged inherit the allocation without having been represented. Dissent exists but sits outside the rooms where the reading is operationalized.
% DISAPPEARANCE_RATIONALE: If exclusive 200nm zones vanished overnight, fisheries would revert toward open-access races on the most valuable stocks, island-state budgets built on license fees would collapse within a fiscal year, dozens of settled and pending boundary agreements would lose their legal basis, and resource confrontation at sea would intensify as claimants fell back on presence and patrol rather than title.
% FOUNDING_PROBLEM: Mid-twentieth-century oceans were an open-access regime under strain: distant-water fleets were depleting stocks faster than any body could manage, coastal states were unilaterally extending jurisdiction claim by claim (beginning with the 1945 Truman Proclamation), and navigation faced fragmentation into a patchwork of contested territorial seas. UNCLOS III's bargain traded 200nm exclusive resource zones for guaranteed transit freedoms and a deep-seabed common-heritage regime.
% FOUNDING_PROBLEM_CORROBORATION: FAO stock-assessment series and regional fisheries management organization science bodies attest that open-access depletion pressure remains real outside managed zones — corroboration from sources that collect no zone rents. Independent marine-policy scholarship documents both the regime's stewardship performance and its distributional asymmetries. The benefiting coastal-state coalition's own attestation that the problem is solved is therefore not the only source on record.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the regime's coordination value is genuine, but the allocation mechanism transfers resource rents by coastline geometry, extinguishes uncompensated traditional access, and prices re-entry (licensing) above what displaced users can pay — assessed, per the epsilon-referent rule, on the standing exclusive-zone arrangement itself, by the strict reading's own lights. Suppression 0.72: persistence depends on actively invalidating alternative frameworks — tribunal rulings declaring overlay claims void, coast-guard interdiction of unauthorized access, and diplomatic isolation of dissenting readings; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope (only extractiveness is scaled, by the engine). Theater_ratio 0.32: fisheries management, licensing, and delimitation are functional, but a growing share of activity is performative sovereignty assertion — patrol staging for domestic audiences, artificial-island construction, symbolic protests. Accessibility_collapse 0.5: once the strict reading is understood, alternatives (denunciation, non-party status, historic-rights assertion) remain partially available — the US position proves exit is survivable — so alternatives are narrowed, not eliminated. Resistance 0.6: sustained rejection of the 2016 award, maintained overlay claims, and freedom-of-navigation operations constitute real, organized resistance. The three measurement series run on ONE shared grid (t = 0, 6, 12, 18, 24, 30, 36, 44, mapping 1982 adoption through 2026; t=12 is the 1994 entry into force, t=30–36 bracket the 2012 Scarborough standoff and 2016 award), so every metric is authored at every examined time point. The suppression series is included because the story specifically tracks enforcement-capacity change: a paper regime in 1982 hardened into actively policed exclusivity, with step increases at incident nodes — a ratchet with quasi-cyclical incident drivers rather than smooth drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the adjudicative seat, the strict reading is settled law awaiting compliance. From the favorable-geography and island-state seats, it is stewardship and a development lifeline. From the overlapping-claimant seat, the identical structure operates as foreclosure of asserted national entitlement by a tribunal process it rejects. From the traditional-fisher seat, it is the quiet conversion of inherited livelihood into trespass. Same text, same article number, four different lived constraints — the engine computes this divergence from power, exit, and directional position; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for favorable_geography_coastal_states and small_island_developing_states — the zones subsidize them, and their constrained-but-voluntary participation sits near the beneficiary end. Victim declarations drive high directionality for the three victim groups, modulated by exit: traditional_fishing_communities (powerless, trapped, local) sit nearest the full-target end — no alternative grounds, no bargaining capacity, and no transnational coalition structure capable of aggregating their dispersed position; overlapping_claimant_states (powerful, constrained) carry high but not maximal directionality — they absorb the ruling's force yet retain naval, diplomatic, and factual-control levers; geographically_disadvantaged_states (organized, constrained) bear diffuse permanent costs with coalition voice but no geographic remedy. Distant_water_fishing_nations are authored as payers with beneficiary residue: their mobility moderates their target position because they can redeploy effort — which is precisely what pushes displacement down onto the immobile local fleets. Maritime_powers_non_parties are excluded rather than coordinated: the strict reading's validity claim is exactly what denies their framework standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim prevents mislabeling in both directions. Reading the regime as pure coordination (rope) would erase the extinguished traditional access, the geographic lottery, and the enforcement machinery needed to hold overlay claims down — costs borne by identifiable seats. Reading it as pure extraction (snare) would erase the real stewardship gains: rebuilt stock-management incentives, SIDS fiscal foundations built on tuna licensing, and boundary certainty that has resolved dozens of disputes by agreement. The founding problem (open-access collapse and unilateral boundary creep) remains live — FAO assessments and RFMO science corroborate continued pressure outside managed zones — so the mandate has not outlived its function and no mandatrophy resolution is declared. The risk to watch is drift, not obsolescence: the measurement series shows extraction and suppression accumulating on top of a functioning coordination core, which is the tangled-rope signature trending harder, not a dead mandate kept alive theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (strict_eez_reading) of the unclos_sovereignty_boundary kernel; how would instantiating the historical_rights_reading instead change the structural data?',
    'Comparative classification of the sibling stories: author the historical_rights_reading and non_ratifier_enforcement_reading as separate files and diff their beneficiary/victim sets and epsilon values against this one.',
    'Under the historical_rights_reading, traditional-use communities exit the victim set and coastal-state exclusivity weakens substantially; under the non_ratifier_enforcement_reading, the enforcement burden shifts from tribunals to naval presence and the ratification coalition loses its gatekeeping role. The victim set and epsilon of ''the EEZ regime'' are reading-indexed, not topic-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the sovereignty-boundary kernel is instantiated changes who counts as victim.').

omega_variable(
    entitlement_basis_legitimacy,
    'Is coastline-geometry allocation of ocean space a neutral natural fact, or a constructed distribution that froze in place the territorial holdings (including colonial-era cartography) of whichever states possessed the relevant coastlines at codification?',
    'Counterfactual analysis of alternative allocation principles (need-based quotas, historical-use grandfathering, population-proportional shares) and archival study of UNCLOS III negotiating records on how the 200nm figure was bargained.',
    'If the allocation is constructed policy rather than natural fact, the regime''s asymmetric component is a revisable choice rather than an inevitable feature, strengthening reform pressure from geographically disadvantaged states; if natural-fact-like, the extraction component reads as the unavoidable price of any boundary system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entitlement_basis_legitimacy, conceptual, 'Whether the geographic lottery at the heart of the strict reading is natural or constructed.').

omega_variable(
    customary_binding_force_of_strict_reading,
    'Does the strict reading bind non-parties as customary international law, extending its reach to maritime powers that never ratified the convention, or does it bind only the ratification coalition?',
    'Systematic survey of state practice and opinio juris: how non-parties behave inside claimed exclusive zones, and whether tribunals treat the 200nm exclusivity as custom independent of treaty consent.',
    'If custom binds non-parties, the constraint''s suppression extends globally and the excluded maritime-power seat becomes a covert target; if consent-based, the regime''s reach stops at the coalition edge and the non-party position remains a genuine alternative framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_binding_force_of_strict_reading, empirical, 'Scope of the strict reading''s binding force beyond the ratification coalition.').

omega_variable(
    enforcement_ratchet_vs_crisis_cycle,
    'Is the rising suppression trajectory a durable enforcement ratchet, or a crisis-driven cycle that relaxes between incidents (standoffs, awards, island-building episodes)?',
    'Extended time series of coast-guard interdiction rates, patrol tempo, and diplomatic protest volume across multiple incident-free windows; test whether baseline enforcement between crises returns toward earlier levels.',
    'If cyclical, the measured end-state suppression overstates steady-state coercive load and the series should be read as oscillation around a lower mean; if ratcheted, each incident permanently raises the enforcement floor and the tangled-rope reading trends toward harder forms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ratchet_vs_crisis_cycle, empirical, 'Whether suppression growth is monotonic ratchet or incident-cycle artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(uncl_tr_t6, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(uncl_tr_t12, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(uncl_tr_t18, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(uncl_tr_t36, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(uncl_tr_t44, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 44, 0.32).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t6, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(uncl_be_t12, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(uncl_be_t18, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(uncl_be_t36, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(uncl_be_t44, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 44, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(uncl_su_t6, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(uncl_su_t12, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(uncl_su_t18, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(uncl_su_t36, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 36, 0.67).
narrative_ontology:measurement(uncl_su_t44, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 44, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'the EEZ regime' decomposes into three readings of one kernel because the colloquial label conflates structurally distinct claims with different epsilon values and different victim sets. The strict reading (this file) is upstream in legitimacy terms — the 2016 award and the codified-text position are cited AGAINST the historical_rights_reading, so this story influences its sibling's operating environment while logically foreclosing its core premise within any single treaty-positivist framework. The non_ratifier_enforcement_reading coexists: most strict-reading states simultaneously hold that navigation freedoms bind everyone, proving the two premises can share a framework. Each file links the other two via affects_constraints; contamination analysis should expect strict-reading purity degradation to embolden the historical-rights sibling first.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
