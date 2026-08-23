% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical-Rights Overlay on Codified Maritime Zones (Historical-Rights Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   A major maritime power asserts that centuries of use, occupation, and
 *   administration confer sovereign rights across waters that the UN
 *   Convention on the Law of the Sea allocates to other coastal states as
 *   200-nautical-mile exclusive economic zones. The claim is administered
 *   through coast-guard interdiction, militarized artificial features, and a
 *   state-subsidized fishing fleet, and is defended by refusing the 2016
 *   arbitral award that tested it. This file instantiates ONE reading of the
 *   contested kernel unclos_sovereignty_boundary - the
 *   historical_rights_reading; the strict_eez_reading and
 *   non_ratifier_enforcement_reading are separate constraints with their own
 *   files, linked through network.affects_constraints. Claim and metrics are
 *   authored independently: the reading presents itself as prior,
 *   quasi-natural law ('rights that predate' the treaty), rhetoric
 *   characteristic of a false-summit framing, but the arrangement is
 *   enforcement-dependent with identifiable beneficiaries, so it is not
 *   authored as a mountain and emerges_naturally is not set. The epsilon
 *   referent is the standing arrangement this reading instantiates where
 *   operative - the de facto historical-rights enforcement regime in the
 *   contested waters - assessed as it operates on non-consenting parties; the
 *   reading's own restitution framing is recorded, not adopted, and the
 *   referent choice is carried as an omega.
 *
 * KEY AGENTS:
 *   - - historic_rights_claimant_states: Agenda setter and principal beneficiary (institutional/identity_locked) - administers and enforces the overlay, collects its gains
 *   - - eez_holding_coastal_states: Primary target (organized/constrained) - bear loss of exclusive jurisdiction inside treaty zones
 *   - - coastal_artisanal_fishers_of_eez_states: Deepest target (powerless/trapped) - expelled from traditional grounds, no forum
 *   - - artisanal_historic_user_communities_of_claimant: Shielded beneficiary (powerless/constrained) - subsidized access cited as living evidence
 *   - - transiting_commercial_shipping: Diffuse target (moderate/mobile) - absorbs friction costs priced into insurance and routing
 *   - - external_naval_patrol_operators: Contesting target (powerful/mobile) - challenged passage operations, directionality overridden
 *   - - asean_multilateral_forum: Excluded voice (organized/constrained) - kept out of the bilateral channel where the arrangement is administered
 *   - - permanent_court_of_arbitration_tribunal: Analytical observer (institutional/analytical) - award issued and refused recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.74).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.74).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, snare).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical-Rights Overlay on Codified Maritime Zones (Historical-Rights Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '686e1e1f-116e-48d0-bb14-1928f6a72e50').
narrative_ontology:cs_kernel_codification('686e1e1f-116e-48d0-bb14-1928f6a72e50', formalized).
narrative_ontology:cs_authority_grounding('686e1e1f-116e-48d0-bb14-1928f6a72e50', lineage).
narrative_ontology:cs_interpretation_layer_present('686e1e1f-116e-48d0-bb14-1928f6a72e50').
narrative_ontology:cs_reading_relation('686e1e1f-116e-48d0-bb14-1928f6a72e50', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('686e1e1f-116e-48d0-bb14-1928f6a72e50', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('686e1e1f-116e-48d0-bb14-1928f6a72e50', foundational, prior_usage_creates_overriding_sovereign_right).
narrative_ontology:cs_axiom_status(prior_usage_creates_overriding_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('686e1e1f-116e-48d0-bb14-1928f6a72e50', prior_usage_creates_overriding_sovereign_right, conventional).
narrative_ontology:cs_axiom('686e1e1f-116e-48d0-bb14-1928f6a72e50', secondary, continuous_effective_occupation_is_evidenced).
narrative_ontology:cs_axiom_status(continuous_effective_occupation_is_evidenced, holdable).
narrative_ontology:cs_axiom_grounding('686e1e1f-116e-48d0-bb14-1928f6a72e50', continuous_effective_occupation_is_evidenced, empirically_contingent).
narrative_ontology:cs_reference_frame('686e1e1f-116e-48d0-bb14-1928f6a72e50', pre_codification_occupancy_priority).
narrative_ontology:cs_drift_state('686e1e1f-116e-48d0-bb14-1928f6a72e50', post_arbitral_award_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('686e1e1f-116e-48d0-bb14-1928f6a72e50', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historic_rights_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, artisanal_historic_user_communities_of_claimant).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, coastal_artisanal_fishers_of_eez_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, transiting_commercial_shipping).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, external_naval_patrol_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A major maritime power publishes maps and white papers asserting that centuries of use and administration confer rights across waters lying inside other states' 200-mile zones. It administers the claim through a coast-guard statute, militarized artificial features, and a state-subsidized fishing fleet that doubles as presence. Its leadership frames the waters as recovered national territory; renouncing the claim is treated domestically as betrayal, so stepping back from enforcement is not a live option for any government that expects to survive office. Gains - fisheries access, hydrocarbon prospects, strategic depth - accrue to its ministries, fleets, and southern commands.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, historic_rights_claimant_states, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, historic_rights_claimant_states, beneficiary).

% Southeast Asian coastal states hold treaty-based exclusive rights to the waters off their coasts. Inside those zones they now face interdicted survey vessels, blocked resupply missions to grounded outposts, and exclusion from shoals they occupy. Their remedies - an arbitration won but unenforceable, a regional consensus stymied by unanimity rules, security dependence on external navies - each carries costs their economies and politics can only partially bear. Leaving the arrangement is not available to them short of conceding the waters.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    organized, biographical, constrained, regional).

% Village fishing fleets that worked particular shoals and lagoons for generations are chased from traditional grounds by coast-guard craft; some grounds have been effectively closed for over a decade. Their boats, gear, and household income are tied to specific reefs; relocating to distant ports means losing gear access and market ties they cannot rebuild. They hold no standing in any negotiating channel.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, coastal_artisanal_fishers_of_eez_states, payer,
    powerless, generational, trapped, local).

% Fishing communities on the claimant's coasts receive subsidized fuel, insurance, and protected access to grounds worked under the claim. Their seasonal circuits are cited as living proof of the historical record. Their individual livelihoods depend on continuing access that the state secures; they neither set the policy nor bear its diplomatic costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, artisanal_historic_user_communities_of_claimant, beneficiary,
    powerless, generational, constrained, regional).

% Container lines, bulk carriers, and energy tankers moving a large share of global trade through the sea face periodic challenges, shadowing, and demands near contested features. War-risk premiums and route adjustments price the friction. Individual operators can reroute through adjacent straits at meaningful cost, and collectively they lobby flag states rather than engage the administering power directly.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, transiting_commercial_shipping, payer,
    moderate, immediate, mobile, global).

% Outside navies assert that transit freedoms survive any coastal claim and schedule deliberate passage operations near contested features to demonstrate it. Their vessels are shadowed, warned, and on occasion maneuvered against; their operations carry incident risk they must plan around. They possess unmatched global reach, yet within these particular waters their presence is contested rather than accepted, and withdrawal would concede the operational precedent.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, external_naval_patrol_operators, payer,
    powerful, generational, mobile, global).

% The ten-member regional bloc is the natural venue for a unified code of conduct but operates by consensus, and members dependent on the claimant's investment block strong language. The claimant insists on one-to-one negotiation, where its size dominates. The forum issues statements but cannot seat itself at the table where the arrangement is actually administered.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, asean_multilateral_forum, excluded,
    organized, generational, constrained, regional).

% A five-member annex VII tribunal constituted at a littoral state's request heard the merits in 2016 and ruled the broad historical-rights claim incompatible with the treaty's allocation of rights, finding no evidence of exclusive historic control within the zones at issue. It possesses no marshal; its award binds only insofar as parties accept it, and the claimant announced non-recognition before the text issued. It continues to shape how third parties and courts read the record.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, permanent_court_of_arbitration_tribunal, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, historic_rights_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an administrable priority rule for maritime spaces where long-standing patterns of use and effective occupation predate formal delimitation: it tells states and user communities that continuity of use, not paper title alone, determines access, resolving conflicts between newly codified zones and settled practice without renegotiating every boundary.
% TRANSFER_FUNCTION: Moves effective jurisdiction over fisheries, hydrocarbon prospecting, shoal occupation, and transit regulation from EEZ-holding coastal states and uninvolved navigators to the historic-rights claimant state, executed through coast-guard interdiction, paramilitary fleet presence, and construction on contested features.
% ABSENT_VOICES: Neighboring states with their own documented historic-use records (whose evidence would complicate the exclusivity of the claimant's account), the multilateral regional forum (kept out by insistence on bilateral negotiation), the arbitral tribunal whose award is refused recognition, expelled EEZ-state fishing communities, and marine-science bodies documenting reef destruction from island construction - none sits inside the bilateral channel where the arrangement is administered.
% DISAPPEARANCE_RATIONALE: If the overlay vanished overnight, EEZ states would resume full jurisdiction over their 200-mile zones, the claimant's coast guard would fall back to baselines, freedom-of-navigation operation tempo would normalize into ordinary port-state relations, and the region's fisheries and hydrocarbons would reorganize around treaty boundaries - removal rearranges control over one of the world's busiest and most resource-dense sea areas.
% FOUNDING_PROBLEM: Before codified maritime zones, entitlement followed occupancy and use; the doctrine preserved vested reliance - historic bays, prescriptive title - so that codification would not retroactively extinguish settled communities and administrations.
% FOUNDING_PROBLEM_CORROBORATION: The narrow doctrine is corroborated outside the benefiting parties by international jurisprudence (the Anglo-Norwegian Fisheries line; the 2016 award, which accepts historic-title categories while finding the broad claim unproven) and by mainstream publicists. The broad override version - historic rights extinguishing codified EEZ entitlements wholesale - is attested only by the benefiting parties' own white papers; no disinterested source corroborates it, and that absence is itself signal.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.74 prices the arrangement as it operates: fisheries grounds closed to their historic users, survey and hydrocarbon activity displaced from licensed zones, transit friction priced into war-risk insurance, and strategic control consolidated over the largest share of a semi-enclosed sea - all accruing to one seat. Suppression 0.74 is authored as a raw structural property, unscaled by power or scope: persistence runs through coast-guard legislation, water-cannon interdiction, blocking of lawful resupply, AIS manipulation, and bilateralizing pressure that removes alternative forums; roughly four-fifths is structural coercion and one-fifth the elite-internalized conviction that the rights are historically self-evident. Theater 0.42 reflects a growing performative share - museum construction, map exhibitions, anniversary landings, white-paper production - alongside genuinely functional enforcement and administration. Accessibility_collapse 0.45: alternatives remain partly available (the codified regime is intact on paper, arbitration exists, coalitions are possible) but are blocked in practice inside the enforced zones. Resistance 0.60: the award, freedom-of-navigation operation tempo, littoral-state transparency initiatives, and ramming countermeasures are sustained pushback. boltzmann.coordination_type is declared resource_allocation because the arrangement's operative function is allocating access to fisheries, hydrocarbons, and transit lanes; the floor test then measures how far extraction exceeds the allocation-cost baseline, which is diagnostically appropriate for the claimed type. The measurement series share one nine-point grid (t=0..16, mapping 2009-2025, anchored at the claim's modern codification via the continental-shelf submission); incident-driven oscillation (2012 shoal seizure, 2014 rig deployment, 2021-24 blockades) rides a monotonic escalation trend, and the escalation - not the incidents - is the classified signal. suppression_requirement is tracked because the story's dynamic is enforcement-capacity intensification (interdiction posture maturing into statute and blockade tactic), not merely shifting extraction. Base scalars describe the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The same structure computes differently by seat. From the agenda-setting seat the arrangement is restored right: directionality near the beneficiary pole, identity lock converts the claim from policy into identity, and the seat experiences challenge as injury. From the EEZ-holding seats the identical structure is enforced dispossession - high directionality amplified by constrained exit; a won award changes nothing on the water. The trapped fisher seats sit nearest the full-target pole: no forum, no mobility, no compensation. External naval operators experience targeted challenge despite global mobility - derivation from victim status plus mobile exit would damp their directionality toward the beneficiary side, hence the explicit override. Commercial shipping prices the friction and routes around it, sitting mid-scale. The arbitral seat sees the whole structure and can bind no one. The divergence is not disagreement about facts; it is the structural asymmetry the engine computes from the declared data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (claimant states, shielded user communities) derive low directionality; victim declarations (EEZ states, their fishers, shipping, naval operators) derive high directionality, amplified by exit grade - trapped fishers highest, constrained EEZ states next. Two corrections and one restraint: (1) external_naval_patrol_operators hold the only 'powerful' atom, and derivation from victim-plus-mobile-exit would place them implausibly near the beneficiary side; their mobility is global repositioning capacity, not insulation from an arrangement that targets their operations directly - overridden to d=0.7. (2) The 'institutional' atom is deliberately left un-overridden because two institutional agents sit on it with opposite relationships (the claimant as beneficiary-administrator, the tribunal as analytical observer); a power-atom-keyed override cannot separate them, so structural derivation stands. Scope is regional with global spillover through shipping; the engine's scope amplification applies modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate - bridging vested reliance across the transition from occupancy-based to codified entitlement - was inherently transitional. Four decades after codification, that bridging function is spent in exactly the terrain where this reading deploys it: the zones are delimited, the reliance interests shielded are the claimant's own expanding state-backed fleets rather than pre-existing communities facing retroactive loss, and rival historic users are expelled rather than protected. mandatrophy_resolved is therefore declared true. The classification guards both mislabelings: calling the arrangement a rope launders capture behind the reliance-protection story; calling it a mountain launders construction behind 'predates the treaty' rhetoric - the false-summit signature the beneficiary declarations are positioned to catch. The snare claim keeps victims and coercion-dependence visible, while the historic_record_sufficiency omega preserves the narrow-doctrine residue that would justify movement toward tangled_rope if the evidentiary predicate ever met title standards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the historical_rights_reading of kernel unclos_sovereignty_boundary; how would instantiating a sibling reading change the structural data?',
    'Classify the sibling stories (unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading) from their own files and compare beneficiary/victim sets and epsilon.',
    'Under the strict reading the sets invert: EEZ-holding coastal states become beneficiaries and the expansive claimant becomes the target; under the non-ratifier reading navigational actors leave the victim set entirely and the claimant loses enforcement legitimacy. Per-seat classifications computed from this file are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: sibling readings instantiate different constraints with inverted structures.').

omega_variable(
    epsilon_referent_selection,
    'Is epsilon priced against the de facto historical-rights enforcement regime (the arrangement this reading instantiates where operative) or against the codified EEZ baseline the reading contests?',
    'Fix the referent by the story''s subject: the standing arrangement this reading produces and defends in the enforced waters. Cross-check by classifying the strict_eez_reading story, whose referent is the codified regime.',
    'Pricing against the codified baseline instead would invert the sign structure - the claimant would appear targeted and EEZ holders subsidized - yielding a different constraint, not a different measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_selection, conceptual, 'Referent fixation for a kernel reading: which standing arrangement epsilon describes.').

omega_variable(
    historic_record_sufficiency,
    'Do the archival records substantiate continuous, peaceable, exclusive exercise of authority over the waters at issue, at the standard historic-title doctrine requires?',
    'Evidentiary review on the Anglo-Norwegian Fisheries and 2016 arbitral standard: maps, administrative acts, acquiescence or protest by other states.',
    'If the record fails, the reliance-protection rationale collapses and the arrangement reads as capture maintained purely by coercion; if it succeeds, a genuine coordination kernel survives and a tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historic_record_sufficiency, empirical, 'Whether the factual predicate of the historical-rights claim meets title standards.').

omega_variable(
    enforcement_ratchet_path,
    'Will coercive enforcement intensity continue ratcheting upward, plateau at current levels, or relax under negotiation?',
    'Track interdiction rates, coast-guard legislation, feature construction, and standoff frequency over the forward decade.',
    'Continued ratcheting pushes suppression beyond 0.8 and dates a further degradation step; relaxation would pull the arrangement back toward contestable hybrid territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_path, empirical, 'Forward trajectory of the enforcement machinery.').

omega_variable(
    coalition_fixing_feasibility,
    'Could a coalition of EEZ-holding states, external naval operators, and shipping interests raise the cost of enforcement enough to change the cost class of removing the arrangement?',
    'Observe whether joint patrol frameworks, unified code-of-conduct language, and coordinated economic measures materialize and alter claimant behavior.',
    'Effective coalition action would move fixing_cost from prohibitive toward cheap and date a possible unraveling; persistent failure leaves the current cost class stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_fixing_feasibility, empirical, 'Whether collective action by the paying seats can change the fix economics.').

omega_variable(
    identity_lock_durability,
    'How durable is the claimant''s fusion of the maritime claim with national identity across leadership transitions and economic shocks?',
    'Observe succession episodes, nationalist media cycles, and whether any leadership faction floats compromise language without career termination.',
    'If the identity frame breaks, the agenda-setting seat''s exit shifts from identity-locked toward merely constrained and the arrangement becomes negotiable within a political generation; if durable, enforcement persists regardless of functional performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, conceptual, 'Durability of the ideological fusion binding the enforcing seat to the claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_historical_rights_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(unclos_historical_rights_tr_t2, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2, 0.21).
narrative_ontology:measurement(unclos_historical_rights_tr_t4, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(unclos_historical_rights_tr_t6, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(unclos_historical_rights_tr_t8, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(unclos_historical_rights_tr_t10, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(unclos_historical_rights_tr_t12, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(unclos_historical_rights_tr_t14, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(unclos_historical_rights_tr_t16, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 16, 0.42).

% Extraction over time
narrative_ontology:measurement(unclos_historical_rights_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unclos_historical_rights_be_t2, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(unclos_historical_rights_be_t4, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(unclos_historical_rights_be_t6, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(unclos_historical_rights_be_t8, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(unclos_historical_rights_be_t10, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(unclos_historical_rights_be_t12, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(unclos_historical_rights_be_t14, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 14, 0.72).
narrative_ontology:measurement(unclos_historical_rights_be_t16, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 16, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(unclos_historical_rights_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(unclos_historical_rights_su_t2, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(unclos_historical_rights_su_t4, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(unclos_historical_rights_su_t6, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(unclos_historical_rights_su_t8, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(unclos_historical_rights_su_t10, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(unclos_historical_rights_su_t12, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(unclos_historical_rights_su_t14, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 14, 0.71).
narrative_ontology:measurement(unclos_historical_rights_su_t16, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 16, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who owns the contested sea' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: this historical-rights overlay (epsilon 0.74, claimant-beneficiary / EEZ-victim structure), the strict_eez_reading (codified exclusivity, inverted structure), and the non_ratifier_enforcement_reading (customary navigational freedom, navigators removed from the victim set). Measuring entitlement by historical record versus by treaty text yields different epsilon because they are different constraints, not one constraint under two observables. Family structure: the codified regime (strict reading) is the upstream establishment this reading overlays and pressures; the non-ratifier reading responds operationally to this reading's enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
