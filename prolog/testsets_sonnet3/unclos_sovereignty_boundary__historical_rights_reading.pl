% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Historical Rights Overlay Claim Against UNCLOS EEZ Boundaries
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested UNCLOS
 *   sovereignty-boundary kernel: that historical usage and occupation
 *   generate sovereign rights predating and overriding the treaty's EEZ
 *   provisions. Under this reading, an expansive claimant state uses
 *   pre-treaty fishing, mapping, and administrative history to assert de
 *   facto sovereignty over waters that UNCLOS Article 57 would assign
 *   exclusively to neighboring coastal states. The claim is backed by patrol
 *   presence, artificial installations, and administrative decrees rather
 *   than by any tribunal-recognized title, and it has hardened over four
 *   decades from a diplomatic assertion into an operationally enforced
 *   overlay. This story does not describe the strict-EEZ reading or the
 *   non-ratifier customary-navigation reading — those are separate
 *   constraints (strict_eez_reading, non_ratifier_enforcement_reading) with
 *   their own ε values and stakeholder sets, linked here via
 *   network.affects_constraints per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - expansive_claimant_state: agenda-setter and primary beneficiary (institutional/arbitrage) — asserts and enforces the historical claim at near-zero direct cost
 *   - eez_holding_coastal_states: primary target (moderate/constrained) — lose treaty-guaranteed exclusive control
 *   - small_island_fishing_communities: powerless/trapped — lose subsistence access with no exit
 *   - foreign_flagged_commercial_shippers and regional_energy_exploration_firms: organized/powerful actors bearing operational costs
 *   - international_arbitral_tribunals: excluded analytical voice — has ruled against the claim but cannot enforce against a non-compliant party
 *   - regional_naval_powers: observer with a stake in freedom of navigation, distinct from either territorial claimant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.71).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Overlay Claim Against UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '6ec77e1f-1621-470b-8405-a38b84cfaa7b').
narrative_ontology:cs_kernel_codification('6ec77e1f-1621-470b-8405-a38b84cfaa7b', distributed).
narrative_ontology:cs_authority_grounding('6ec77e1f-1621-470b-8405-a38b84cfaa7b', distributed).
narrative_ontology:cs_reading_relation('6ec77e1f-1621-470b-8405-a38b84cfaa7b', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('6ec77e1f-1621-470b-8405-a38b84cfaa7b', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('6ec77e1f-1621-470b-8405-a38b84cfaa7b', foundational, prior_occupation_creates_unextinguishable_title).
narrative_ontology:cs_axiom_status(prior_occupation_creates_unextinguishable_title, holdable).
narrative_ontology:cs_axiom_grounding('6ec77e1f-1621-470b-8405-a38b84cfaa7b', prior_occupation_creates_unextinguishable_title, conventional).
narrative_ontology:cs_axiom('6ec77e1f-1621-470b-8405-a38b84cfaa7b', secondary, documented_historical_administration_outweighs_treaty_geometry).
narrative_ontology:cs_axiom_status(documented_historical_administration_outweighs_treaty_geometry, holdable).
narrative_ontology:cs_axiom_grounding('6ec77e1f-1621-470b-8405-a38b84cfaa7b', documented_historical_administration_outweighs_treaty_geometry, empirically_contingent).
narrative_ontology:cs_reference_frame('6ec77e1f-1621-470b-8405-a38b84cfaa7b', pre_unclos_customary_maritime_title).
narrative_ontology:cs_drift_state('6ec77e1f-1621-470b-8405-a38b84cfaa7b', post_pca_south_china_sea_ruling, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6ec77e1f-1621-470b-8405-a38b84cfaa7b', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, state_owned_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, state_owned_energy_developers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, small_island_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, foreign_flagged_commercial_shippers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, regional_energy_exploration_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, regional_naval_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts a historical usage and occupation basis (fishing grounds, ancient maps, dashed-line claims, prior administrative acts) predating UNCLOS, and backs the claim with coast guard patrols, militarized reef installations, and administrative decrees. Treats the historical claim as sovereign and non-negotiable, and enforces it against foreign vessels operating inside what UNCLOS would designate as another state's EEZ. Bears essentially no cost from asserting the claim; the claim is cost-free to make and only incrementally costly to enforce.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, beneficiary).

% Hold UNCLOS-ratified EEZ rights over waters now overlaid by the historical claim. Lose exclusive access to fisheries and seabed resources within their own 200-nautical-mile zone whenever the claimant's patrols or installations assert priority. Their options are diplomatic protest, arbitration filings (which the claimant may ignore), or costly naval posture they mostly cannot sustain against a larger power — exit from the dispute is not available while the coastline itself is fixed.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, generational, constrained, regional).

% Depend on traditional fishing grounds now patrolled or blockaded under the historical-rights claim. Lose direct subsistence and market access when driven off contested waters by claimant vessels, with no capacity to litigate or relocate their livelihood elsewhere; they experience the claim as an immediate, personal loss of access to waters their communities have always used.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Route cargo and tanker traffic through the contested waters as part of major global shipping lanes. Face increased insurance costs, transit friction, and occasional harassment or diversion when the historical claim is asserted against passage rights. Can reroute at real but bounded cost, making them less trapped than fishing communities but still bearing a direct operational tax from the dispute.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, foreign_flagged_commercial_shippers, payer,
    organized, biographical, constrained, global).

% Hold exploration and drilling licenses issued by EEZ-holding coastal states inside the contested zone. Face survey vessel harassment, blocked rigs, and cancelled contracts when the claimant state treats the concession area as its own historical waters, forcing costly relocation of planned operations or abandonment of already-sunk capital.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, regional_energy_exploration_firms, payer,
    powerful, biographical, constrained, regional).

% Political and media constituencies within the claimant state who derive legitimacy, identity narrative, and electoral support from the historical-rights claim. Do not bear the direct enforcement costs but benefit from the nationalist narrative the claim sustains, reinforcing domestic political capital for the state that presses it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies, beneficiary,
    organized, generational, mobile, national).

% Fishing fleets from the claimant state gain expanded, protected access to fishing grounds inside the contested zone, escorted or shielded by claimant patrol vessels, directly capturing the resource access that EEZ-holding states and local fishing communities lose.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, state_owned_fishing_fleets, beneficiary,
    organized, biographical, mobile, regional).

% State-linked energy companies from the claimant state pursue seabed exploration inside the contested zone under the cover of the historical claim, gaining access to hydrocarbon and mineral resources that would otherwise fall under a neighboring state's exclusive jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, state_owned_energy_developers, beneficiary,
    institutional, generational, arbitrage, regional).

% Bodies such as the Permanent Court of Arbitration can rule on UNCLOS disputes, and have ruled against historical-rights overlay claims where invoked, but the claimant state does not participate in or recognize the proceedings, so the ruling produces no enforcement mechanism against it — the tribunal's voice is present in international law discourse but structurally absent from the operational dispute.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_arbitral_tribunals, excluded,
    institutional, generational, analytical, global).

% External naval powers with freedom-of-navigation interests monitor and periodically contest the historical claim through patrol transits, without being direct territorial claimants themselves. They absorb diplomatic and military risk from the standoff while asserting that customary navigation rights are separate from and unaffected by either party's territorial claim.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, regional_naval_powers, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, regional_naval_powers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The historical-rights framing purports to solve a genealogical problem UNCLOS leaves unresolved: what happens when a state's documented pre-treaty usage, occupation, or administration of a maritime feature conflicts with the treaty's clean geometric formula. It offers a coordination story — continuity of long-standing use and title should not be erased by a later multilateral instrument that some claimants regard as imposed or incomplete.
% TRANSFER_FUNCTION: Moves fishing access, seabed resource rights, and unimpeded passage away from EEZ-holding coastal states, local fishing communities, foreign shippers, and licensed energy firms, toward the claimant state's fleets, energy developers, and domestic political establishment, enforced through patrol presence and administrative fait accompli rather than adjudicated title.
% ABSENT_VOICES: International arbitral tribunals and the broader international legal order that has ruled against overlay claims of this kind are structurally absent from the operational dispute because the claimant state neither submits to nor complies with rulings it does not accept; the tribunal's reasoning exists in the record but has no seat at the enforcement table.
% DISAPPEARANCE_RATIONALE: If the historical-rights claim were withdrawn overnight, EEZ-holding states would regain uncontested control of their treaty-defined zones, licensed exploration would resume without harassment, fishing communities would regain access to traditional grounds without patrol interference, and shipping insurance premiums in the contested lanes would likely fall — the claim actively reallocates real resource access and is not a passive label over an otherwise unchanged status quo.
% FOUNDING_PROBLEM: The claim was originally framed to protect long-standing fishing and navigational practices, and asserted historical administrative presence, that predated the 1982 UNCLOS framework and that the claimant state feared would be erased by a treaty formula keyed purely to coastline geometry.
% FOUNDING_PROBLEM_CORROBORATION: The claimant state and its domestic constituencies attest the founding problem remains live — that historical usage deserves legal recognition UNCLOS denies it. Independent international law scholars, the 2016 Permanent Court of Arbitration ruling (South China Sea Arbitration), and the neighboring coastal states attest that the historical-usage claim has no basis in UNCLOS and that the arrangement now functions as territorial and resource expansion rather than protection of a genuine pre-existing practice; this corroboration comes from parties outside the claimant's own political and enforcement apparatus.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as substantially high (0.71 at interval end) and rising because the historical-rights overlay has moved from rhetorical assertion toward operational control of fisheries and seabed access previously exclusive to EEZ-holding states — a genuine transfer of resource rents, not merely a legal dispute over labels. Suppression (0.62) reflects the patrol and installation infrastructure required to hold the claim against contrary international rulings and neighboring-state resistance; it is a raw structural measure of the coercive apparatus, not scaled by scope. Theater ratio (0.4) captures that a meaningful share of the claim's public presentation (historical maps, cultural-heritage framing) functions as legitimation performance layered atop the material resource capture. All three temporal series share one grid (0/8/16/24/32/40) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state's seat this is a rope — continuity of legitimate historical practice against an imperfect treaty. From the EEZ-holding coastal state and fishing-community seats it computes as extractive, actively suppressed loss of previously secure rights. The engine computes both from the same structural data; the divergence is the point, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   The claimant state and its aligned fleets/developers are near the full-beneficiary end: they collect fishing and seabed access and bear negligible enforcement cost relative to gain. EEZ-holding coastal states, small island fishing communities, and licensed regional firms sit near the full-target end: each loses treaty-recognized or license-recognized access through the same overlay mechanism. Foreign shippers occupy an intermediate position — constrained but not trapped, since rerouting is costly but possible. Domestic nationalist constituencies are declared beneficiaries by narrative/political capture rather than resource capture, which is why they are marked mobile rather than institutional — they benefit from but do not administer the claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genuine mismatch: the claimant state treats the founding problem (protecting pre-treaty usage) as live, while independent tribunals and neighboring states treat it as either never legally cognizable under UNCLOS or long since resolved through the treaty's ratification process — yet the enforcement apparatus has only intensified. This status=contested + verdict=world_rearranges pattern is exactly the corpus signal for a capture/zombie flag: a founding rationale kept alive publicly while the arrangement's operational function has shifted toward resource capture, which the tangled_rope classification (coordination story + asymmetric extraction + active enforcement) is built to hold without collapsing into either 'pure natural continuity' or 'ordinary treaty dispute.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_title_versus_treaty_supremacy,
    'Does documented pre-1982 historical usage and occupation create a sovereign title that survives and overrides a state''s own UNCLOS ratification, or does ratification extinguish any such prior claim as a matter of treaty supremacy?',
    'Authoritative international tribunal rulings (e.g., PCA precedent), and whether the claimant state''s own historical conduct (prior treaty adherence, prior acquiescence to neighboring states'' administration) is consistent with an unextinguished historical title.',
    'If historical title is found to survive ratification, the claimant''s reading gains genuine legal grounding and the constraint moves toward a rope/tangled_rope boundary case; if extinguished, the claim is purely extractive overlay with no surviving coordination function, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_title_versus_treaty_supremacy, conceptual, 'Whether historical usage can legally survive a state''s own treaty ratification.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings of the UNCLOS sovereignty kernel locate their disagreement — is it about which legal instrument is supreme (treaty vs. custom vs. historical title), or about who has standing to adjudicate supremacy absent universal compliance?',
    'Structural comparison of the three constraint stories'' cs_structure.axioms and authority_grounding fields; identify whether the axioms are genuinely incompatible (forecloses) or merely competing in different institutional venues (coexists_with).',
    'Determines whether future analysis should treat the three readings as mutually exclusive legal theories or as coexisting claims asserted by different parties with no single adjudicating authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural disagreement among the historical-rights, strict-EEZ, and non-ratifier-enforcement readings.').

omega_variable(
    narrative_versus_capture_beneficiary,
    'Are domestic nationalist constituencies genuine coordination beneficiaries (a real collective-identity good) or merely a legitimation audience for what is otherwise a resource-capture arrangement run by the state apparatus?',
    'Survey and public-opinion trend data on whether domestic support for the claim tracks material stakes (fishing income, energy revenue distribution) or purely symbolic/nationalist framing independent of material benefit.',
    'If purely symbolic, the beneficiary declaration for domestic_nationalist_constituencies should be read as thin (a narrative payoff, not a resource payoff), sharpening the case that the primary beneficiaries are the state fishing fleets and energy developers alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_versus_capture_beneficiary, empirical, 'Whether domestic political support is a genuine coordination benefit or a legitimation veneer over resource capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% Part of the unclos_sovereignty_boundary constraint family (3 readings). strict_eez_reading treats the claimant's historical assertion as void and the coastal state's EEZ as exclusively controlling — under that reading ε for the coastal state's position is near zero (a legitimate, uncontested exercise of treaty right) while this reading's ε for the same standing arrangement is high (0.71, an extractive overlay). non_ratifier_enforcement_reading is a third, partially orthogonal reading concerned with navigational freedom rather than resource title, and interacts with this reading primarily through the shipping-lane stakeholder overlap. Each reading is authored as its own constraint with its own ε, per the ε-invariance principle; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
