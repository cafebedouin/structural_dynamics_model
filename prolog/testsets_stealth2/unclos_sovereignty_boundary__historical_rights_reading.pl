% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Historical-Rights Override of UNCLOS EEZ Exclusivity
 *   domain: international law / maritime governance / geopolitical strategy
 *
 * SUMMARY:
 *   An expansive claimant state asserts that continuous historical usage and
 *   occupation of contested waters ground sovereign rights predating the 1982
 *   UNCLOS and overriding the exclusive economic zones the treaty grants its
 *   neighbors. This story instantiates the historical_rights_reading of the
 *   unclos_sovereignty_boundary kernel: the constraint is the operative
 *   regime in which historical-rights claims are enforced over waters inside
 *   other states' treaty EEZs — coast guard and maritime-militia enforcement,
 *   fortified features, and a standing legal-historical apparatus sustaining
 *   the claim. ε's referent is that standing enforcement arrangement,
 *   assessed by this reading's own lights: the reading recognizes a genuine
 *   coordination function in historical-usage recognition (traditional-access
 *   continuity, a fallback order where treaty boundaries are contested) but
 *   its own structural delta declares the asymmetric transfer — the claimant
 *   collects, EEZ-holding coastal states lose exclusive control, navigational
 *   actors bear increased constraint — so ε is authored high rather than
 *   dissolved into the reading's endorsement of the claim's legitimacy. The
 *   sibling readings are separate constraints, not hedges inside this one:
 *   the strict_eez_reading would author higher ε for the same waters (from
 *   its seat the overlay is a lawless nullity extracting from treaty rights),
 *   and the non_ratifier_enforcement_reading would relocate the victim set
 *   toward the claimant's enforcement apparatus. The claim/metric gap is
 *   deliberate: claimed_type is authored from structure (tangled_rope — real
 *   customary-law coordination residue plus enforced asymmetric extraction),
 *   metrics from descriptive operation; the engine computes per-seat
 *   classifications, and divergence between claim and computed type is the
 *   measurement.
 *
 * KEY AGENTS:
 *   - expansive_claimant_state: agenda-setter and primary beneficiary (institutional / identity_locked) — enforces the claim, collects resource access and control, cannot exit the claim it is constituted by
 *   - eez_holding_coastal_states: primary payers (moderate / constrained) — lose exclusive treaty-zone control wherever the claimant's enforcement reaches
 *   - coastal_fishing_communities_of_eez_holders: diffuse payers (powerless / trapped) — bear the livelihood costs directly and hold no seat
 *   - claimant_state_fishing_fleets: secondary beneficiaries (organized / mobile) — access grounds under escort; their presence is an enforcement instrument
 *   - international_shipping_industry: navigational payer (organized / mobile) — carries insurance, routing, and delay costs with arbitrage-grade rerouting
 *   - foreign_naval_forces: navigational payer (institutional / mobile) — contested presence operations, escalation risk accepted
 *   - small_island_developing_states: excluded (powerless / trapped) — precedent-erosion exposure with no seat in a bilaterally managed dispute
 *   - arbitral_and_judicial_bodies: analytical observer (institutional / analytical) — ruled against the reading, holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.74).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical-Rights Override of UNCLOS EEZ Exclusivity").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international law / maritime governance / geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '4d72116b-a873-4fd6-b3eb-6e58872e4c13').
narrative_ontology:cs_kernel_codification('4d72116b-a873-4fd6-b3eb-6e58872e4c13', fixed_text).
narrative_ontology:cs_authority_grounding('4d72116b-a873-4fd6-b3eb-6e58872e4c13', distributed).
narrative_ontology:cs_reading_relation('4d72116b-a873-4fd6-b3eb-6e58872e4c13', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('4d72116b-a873-4fd6-b3eb-6e58872e4c13', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('4d72116b-a873-4fd6-b3eb-6e58872e4c13', foundational, historical_usage_grounds_sovereignty).
narrative_ontology:cs_axiom_status(historical_usage_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4d72116b-a873-4fd6-b3eb-6e58872e4c13', historical_usage_grounds_sovereignty, conventional).
narrative_ontology:cs_axiom('4d72116b-a873-4fd6-b3eb-6e58872e4c13', foundational, treaty_codification_cannot_extinguish_prior_rights).
narrative_ontology:cs_axiom_status(treaty_codification_cannot_extinguish_prior_rights, holdable).
narrative_ontology:cs_axiom_grounding('4d72116b-a873-4fd6-b3eb-6e58872e4c13', treaty_codification_cannot_extinguish_prior_rights, conventional).
narrative_ontology:cs_axiom('4d72116b-a873-4fd6-b3eb-6e58872e4c13', secondary, continuous_exclusive_exercise_documented).
narrative_ontology:cs_axiom_status(continuous_exclusive_exercise_documented, holdable).
narrative_ontology:cs_axiom_grounding('4d72116b-a873-4fd6-b3eb-6e58872e4c13', continuous_exclusive_exercise_documented, empirically_contingent).
narrative_ontology:cs_reference_frame('4d72116b-a873-4fd6-b3eb-6e58872e4c13', pre_treaty_historical_entitlement_continuity).
narrative_ontology:cs_drift_state('4d72116b-a873-4fd6-b3eb-6e58872e4c13', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d72116b-a873-4fd6-b3eb-6e58872e4c13', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, coastal_fishing_communities_of_eez_holders).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_industry).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, foreign_naval_forces).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historic_waters_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces a claim that continuous historical use and administration of the contested waters ground sovereign rights predating the maritime treaty. Maintains a standing enforcement apparatus — coast guard patrols, maritime militia, fortified artificial features — and a legal-historical bureaucracy producing archival support for the claim. Collects access to fisheries and prospective hydrocarbon deposits and controls sea space that would otherwise fall inside neighboring states' exclusive treaty zones. The claim is fused with the state's national narrative of historical restoration; no government could abandon it without existential domestic cost, so stepping back from the claim is effectively unavailable even as enforcement costs rise.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, beneficiary).

% Sovereign neighbors holding treaty-based exclusive rights over 200-nautical-mile zones that the claim overlays. Where the claimant's enforcement reaches, they lose effective control of fisheries management, hydrocarbon prospecting, and patrol of their own zones. Their options are diplomatic protest, arbitration the claimant refuses to comply with, alliance-building, and coast guard presence; none restores exclusive control where the claimant's force operates, and their geography cannot be relocated.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, generational, constrained, regional).

% Fish grounds their communities worked for generations inside what is now their state's treaty zone. Under the claim's enforcement they are driven off grounds, have catch and equipment seized, and face detention risk; they cannot switch to other grounds without losing their livelihood, and their governments' protection is slow and partial.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, coastal_fishing_communities_of_eez_holders, payer,
    powerless, immediate, trapped, regional).

% Operate under coast guard escort deep inside the contested waters, reaching grounds that would be closed to them if the treaty's exclusive zones were enforced as written. Subsidized, insured, and directed in ways that make their presence itself part of the claim's maintenance.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets, beneficiary,
    organized, biographical, mobile, regional).

% Carries a large share of global trade through the contested waters. Absorbs war-risk insurance premiums, routing adjustments, and delay where enforcement activity intensifies. Can reroute at cost, but the chokepoint geography of the sea lanes limits avoidance, and it holds no seat in the sovereignty dispute whose costs it carries.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_industry, payer,
    organized, immediate, mobile, global).

% Transit and operate in the contested waters under freedom-of-navigation doctrines. Their presence is contested under the claim: shadowed, challenged, told to give notice or cease activity in claimed zones, and drawn into confrontation incidents. They answer with presence operations of their own, accepting escalation risk as the price of contesting the claim.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, foreign_naval_forces, payer,
    institutional, generational, mobile, global).

% States whose territory is almost entirely maritime zone: their viability depends on the strict exclusivity the treaty grants. A generalizable version of the claim would erode the foundation of their sovereignty, but the dispute is prosecuted bilaterally between the claimant and the adjacent coastal states; their objection surfaces only in multilateral fora the claimant refuses to enter.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states, excluded,
    powerless, generational, trapped, global).

% Tribunal and court seats that have adjudicated the dispute: the 2016 annex VII tribunal found no legal basis under the treaty for historic rights within the claimed line. They hold interpretive authority without enforcement power; their rulings bind only insofar as parties comply, and the claimant rejects the award's validity outright.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, arbitral_and_judicial_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recognizes and stabilizes entitlements founded on continuous historical use: traditional fishing access, long-standing administration of maritime features, and continuity-based boundary expectations — providing a working order in waters where treaty boundaries are contested and where communities' use patterns predate codification.
% TRANSFER_FUNCTION: Moves effective control over fisheries, hydrocarbon access, and sea space from EEZ-holding coastal states to the expansive claimant state, and moves costs — confrontation risk, insurance premiums, routing delay, lost fishing grounds — onto navigational actors and the coastal fishing communities of the EEZ-holding states.
% ABSENT_VOICES: Small island developing states would object that a generalizable historical-rights override erodes the exclusivity their statehood depends on; they hold no seat. The broader UNCLOS states-parties community would object that a multilateral treaty is being overridden through bilateral enforcement; the claimant insists on bilateral handling, where its power advantage is greatest. The coastal fishing communities of the EEZ-holding states are spoken for but not seated — their livelihoods are negotiated over their heads.
% DISAPPEARANCE_RATIONALE: If the claim and its enforcement vanished overnight, the claimant's patrols would withdraw from zones inside neighboring EEZs, coastal states would resume exclusive fisheries and hydrocarbon management within seasons, fishing fleets would rearrange around treaty zones, and war-risk premiums and routing patterns would normalize. The claimant's fortified features would remain as facts on the water, so the underlying feature-sovereignty disputes would persist — but the overlay regime over the waters themselves would dissolve into the treaty default. Arrangements demonstrably depend on the constraint: the world rearranges.
% FOUNDING_PROBLEM: The arrangement was built to solve the codification problem: communities and states whose use, administration, and dependence on specific waters long predated modern maritime law needed recognition, so that a comprehensive treaty allocating maritime space would not extinguish established practice overnight.
% FOUNDING_PROBLEM_CORROBORATION: The claimant state's white papers and archival programs attest the founding problem remains live. Outside the beneficiary set: the 2016 annex VII arbitral tribunal found the treaty framework comprehensively allocated maritime entitlements and that no historic rights survived within the claimed line — attesting the problem was resolved by codification; UNCLOS's own text preserves narrow historic-title and traditional-fishing provisions, which independent scholarship reads as the codified residue of the founding problem; no corroborating source outside the beneficiary set attests that a general override remains necessary. The status is therefore contested between the claimant's apparatus and the treaty community's record.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.74 at interval end) because resource access and control transfer from weaker treaty-holders to the claimant with no compensating service flow, and because the transfer is enforcement-backed rather than consented. Suppression (0.72) is a raw structural property, unscaled by power or scope: the arrangement's persistence depends on actively preventing treaty exercise — blockading resupply, harassing fishing fleets, rejecting adjudication — not on participant preference. Theater (0.42) is substantial but not dominant: a real enforcement apparatus coexists with a growing performative layer of archival campaigns, white papers, map exhibitions, and naming practices that maintain the claim's evidentiary narrative. Accessibility_collapse (0.50) is moderate because the alternatives do not close: the strict-EEZ and naval-enforcement readings remain live, arbitration exists even though compliance is refused — the contest stays open. Resistance (0.72) is high: the arbitration case, freedom-of-navigation operations, diplomatic protests, and coalition statements are sustained, organized pushback. The measurement series run on one shared grid (1990, 1998, 2009, 2016, 2020, 2025) with all three tracked metrics authored at every point. Rising base_extractiveness models extraction accumulation as enforcement capacity matured; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change (fortified features, coast-guard expansion, maritime-militia institutionalization), not merely shifting extraction; rising theater tracks the growth of the evidentiary apparatus. Episodic incidents (2012, 2016, 2021 onward) ride a monotonic accumulation trend; no full oscillation cycle is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same waters. From the agenda-setter seat the arrangement is the restoration of a pre-existing right the claimant administers; from the constrained payer seats it is enforced dispossession of treaty rights; from the excluded small-island seat it is a precedent threatening statehood-level entitlements; from the observer seat it is a claim the legal record has rejected. Same-level divergence: the EEZ-holding coastal states and the claimant hold nominally equal sovereign standing, but projection power and identity-fusion differentiate their exit options — the claimant is locked into the claim by its own legitimacy narrative, while the coastal states are locked in by geography. The shipping industry and the coastal fishing communities both pay, but the shipping seat's arbitrage-grade rerouting moderates its experienced burden far below the trapped fishing communities', whose grounds cannot be exited at all. Coalition potential for the powerless seats is thin: transnational fishing-community solidarity is weak, and their states speak for them only partially.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map directly: the claimant state and its escorted fishing fleets sit near the beneficiary end (low d); the EEZ-holding coastal states and their fishing communities sit near the target end (high d), with trapped exit amplifying the fishing communities toward full-target. The navigational actors bear real constraint increase but their d should compute below the trapped victims': the shipping industry holds arbitrage-grade rerouting and the naval forces hold mobile presence — the derivation from victim declaration plus mobile exit should place them high-but-not-maximal. The claimant's identity_lock is beneficiary-side and must not be read as target-side: the lock binds it to an arrangement it profits from and is constituted by (the restoration narrative is the regime's legitimacy engine), so it should dampen, not raise, its effective extraction. No directionality overrides are authored — the structural derivation from beneficiary/victim data plus exit options is trusted; the shipping moderation is flagged here for the per-seat computation rather than forced by override. Identity-lock dynamics: the fusion is institutional-ideological — the state's legitimacy narrative has fused with the restoration claim. If the identity frame broke (domestic repudiation of the claim), enforcement capacity would persist but the claim's normative engine would stall, and the seat map would swing toward the strict reading's structure within a political generation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recognizing pre-treaty entitlement so codification would not extinguish established practice — was substantially absorbed by the treaty itself, which preserves narrow historic-title and traditional-fishing provisions. The general override reading therefore extends the mandate past the founding problem's original scope: this is mandate expansion rather than atrophy, with the arrangement growing functions (resource control, strategic depth) the founding problem never named. The founding problem's status is authored contested, not dead, because the claimant's apparatus keeps it rhetorically alive while the treaty community's record attests codification resolved it; mandatrophy_resolved is deliberately not declared. The tangled_rope claim prevents both mislabels: a pure-snare label would erase the genuine customary-law residue (traditional-access recognition is real doctrine with real protected populations), and a pure-rope label would erase the enforced asymmetric transfer the structural delta declares. If the historical record fails evidentiary tests (historical_record_sufficiency omega), the coordination story collapses toward cover and the constraint drifts snare-ward; if enforcement capacity decayed while the claim persisted rhetorically, piton dynamics would appear. The classification keeps those trajectories measurable instead of pre-deciding them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the historical_rights_reading of the unclos_sovereignty_boundary kernel; how would the strict_eez_reading and non_ratifier_enforcement_reading restructure the seat map and ε for the same waters? The disagreement is located in the override premise: whether pre-treaty usage survives codification as an overriding entitlement (this reading), is extinguished or narrowly preserved (strict reading), and who may enforce navigation freedoms (non-ratifier reading).',
    'Author the sibling stories and compare computed per-seat classifications: the strict reading would move the claimant seat into the payer set (its overlay invalid, no coordination credit) and restore EEZ-holders to beneficiaries; the non-ratifier reading would keep the claimant a payer and add its enforcement apparatus as the constraint''s object.',
    'The same waters classify differently per reading — the claimant seat computes as beneficiary under this reading and as payer under the strict reading. Cross-reading comparison is the measurement the kernel family exists to take, not a defect in this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings restructure the seat map and ε for the same waters.').

omega_variable(
    historical_record_sufficiency,
    'Does the archival record establish continuous, open, and administratively exercised authority over the contested waters sufficient to ground the claimed rights under the historic-waters tradition?',
    'Tribunal-grade evidentiary assessment and independent archival scholarship; the 2016 award already found the record did not establish historic title to the waters, while the claimant''s apparatus continues to produce supporting evidence.',
    'If the record substantiates continuous exercise, the reading''s coordination function is genuine and ε falls; if it does not, the historical narrative functions as cover for enforcement-backed transfer and the constraint drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_sufficiency, empirical, 'Whether the historical record can carry the coordination story the reading tells.').

omega_variable(
    acquiescence_vs_coercion,
    'Is the arrangement''s stability produced by coercive enforcement capacity alone, or by hardening regional acquiescence — states accommodating the claim in practice while contesting it in words?',
    'Track state practice over time: bilateral accommodation agreements, silent tolerance of enforcement activity, voting patterns on the claim''s legitimacy in multilateral fora.',
    'If acquiescence is hardening, the arrangement consolidates without further enforcement intensification and the suppression series should flatten; if not, enforcement costs keep rising and suppression_requirement should keep climbing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_vs_coercion, empirical, 'Structural coercion versus internalized accommodation at the state level as the persistence mechanism.').

omega_variable(
    precedent_generalization,
    'Does acceptance of this reading generalize beyond the specific waters — eroding EEZ exclusivity globally — or remain confined as a special claim tied to one claimant''s historical narrative?',
    'Survey whether other states invoke historical-rights overlays against neighbors'' EEZs, and watch small-island developing states and the treaty community''s responses to any such invocation.',
    'Generalization expands the victim set globally and raises accessibility_collapse as the strict reading''s alternatives close; confinement keeps the constraint regional and permanently contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_generalization, empirical, 'Whether the override precedent stays local or becomes a general solvent of treaty exclusivity.').

omega_variable(
    coordination_extraction_separability,
    'Is the traditional-access coordination function separable from the override mechanism, or does the doctrine''s coordination value depend on the override itself?',
    'Test whether narrow traditional-fishing-access arrangements of the kind the treaty''s own provisions contemplate can deliver the continuity function without the sovereignty override.',
    'If separable, the override component is pure extraction riding a real coordination function; if inseparable, part of the measured ε is the price of the continuity the reading protects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the reading''s coordination and transfer components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hist_rights_tr_t1990, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t1990, observed).
narrative_ontology:measurement(unclos_hist_rights_tr_t1998, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t1998, observed).
narrative_ontology:measurement(unclos_hist_rights_tr_t2009, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2009, 0.33).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t2009, observed).
narrative_ontology:measurement(unclos_hist_rights_tr_t2016, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t2016, observed).
narrative_ontology:measurement(unclos_hist_rights_tr_t2020, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t2020, observed).
narrative_ontology:measurement(unclos_hist_rights_tr_t2025, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(unclos_hist_rights_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(unclos_hist_rights_be_t1990, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t1990, observed).
narrative_ontology:measurement(unclos_hist_rights_be_t1998, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t1998, observed).
narrative_ontology:measurement(unclos_hist_rights_be_t2009, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t2009, observed).
narrative_ontology:measurement(unclos_hist_rights_be_t2016, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t2016, observed).
narrative_ontology:measurement(unclos_hist_rights_be_t2020, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t2020, observed).
narrative_ontology:measurement(unclos_hist_rights_be_t2025, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(unclos_hist_rights_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hist_rights_su_t1990, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t1990, observed).
narrative_ontology:measurement(unclos_hist_rights_su_t1998, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t1998, observed).
narrative_ontology:measurement(unclos_hist_rights_su_t2009, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2009, 0.58).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t2009, observed).
narrative_ontology:measurement(unclos_hist_rights_su_t2016, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t2016, observed).
narrative_ontology:measurement(unclos_hist_rights_su_t2020, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t2020, observed).
narrative_ontology:measurement(unclos_hist_rights_su_t2025, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(unclos_hist_rights_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the UNCLOS sovereignty dispute' decomposes into three structurally distinct readings of one kernel, per the ε-invariance principle. This story (historical_rights_reading) instantiates the claimant's override doctrine; strict_eez_reading instantiates the treaty community's exclusivity rule (no overlay claims valid); non_ratifier_enforcement_reading instantiates the naval-enforcement reading of customary navigation freedom. The ε values differ because the readings assess the same waters from structurally different seats: this reading authors ε for the enforcement arrangement its own claim constitutes (high, with coordination residue); the strict reading authors higher ε for the same arrangement (a lawless overlay from its seat); the non-ratifier reading relocates the victim set toward the claimant's enforcement apparatus. Upstream/downstream: the strict reading is the codified baseline this reading overrides; the naval-enforcement reading is the practice response this reading's enforcement provokes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
