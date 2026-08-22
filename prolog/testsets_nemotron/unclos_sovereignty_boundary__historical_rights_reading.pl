% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Historical Rights Override of UNCLOS EEZ Boundaries
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint captures the 'historical rights reading' of the UNCLOS
 *   sovereignty boundary kernel: the claim that continuous historical usage,
 *   administration, and occupation of maritime features generate sovereign
 *   rights that legally precede and hierarchically override the
 *   200-nautical-mile EEZ regime established by UNCLOS Article 57. The
 *   reading is advanced by states including China (nine-dash line), Vietnam,
 *   and the Philippines in the South China Sea, and mirrors arguments in
 *   other contested seas. It functions as a tangled rope: it coordinates a
 *   genuine problem (the intertemporal gap between pre-UNCLOS administration
 *   and post-UNCLOS zoning) while extracting exclusive resource control from
 *   EEZ-holding states through active enforcement (coast guard, militia,
 *   island-building). The claim/metric gap is deliberate: claimant states
 *   frame the constraint as rope (restoring historical justice, stabilizing
 *   expectations), while the authored metrics describe substantial extraction
 *   from overlapping EEZ holders and rising suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override of UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'e9a498a1-7842-44ee-857d-7dc7a3857735').
narrative_ontology:cs_kernel_codification('e9a498a1-7842-44ee-857d-7dc7a3857735', formalized).
narrative_ontology:cs_authority_grounding('e9a498a1-7842-44ee-857d-7dc7a3857735', lineage).
narrative_ontology:cs_interpretation_layer_present('e9a498a1-7842-44ee-857d-7dc7a3857735').
narrative_ontology:cs_reading_relation('e9a498a1-7842-44ee-857d-7dc7a3857735', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('e9a498a1-7842-44ee-857d-7dc7a3857735', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('e9a498a1-7842-44ee-857d-7dc7a3857735', foundational, historical_title_survives_unclos).
narrative_ontology:cs_axiom_status(historical_title_survives_unclos, holdable).
narrative_ontology:cs_axiom_grounding('e9a498a1-7842-44ee-857d-7dc7a3857735', historical_title_survives_unclos, deontological).
narrative_ontology:cs_axiom('e9a498a1-7842-44ee-857d-7dc7a3857735', foundational, intertemporal_law_preserves_pre_treaty_rights).
narrative_ontology:cs_axiom_status(intertemporal_law_preserves_pre_treaty_rights, holdable).
narrative_ontology:cs_axiom_grounding('e9a498a1-7842-44ee-857d-7dc7a3857735', intertemporal_law_preserves_pre_treaty_rights, conventional).
narrative_ontology:cs_reference_frame('e9a498a1-7842-44ee-857d-7dc7a3857735', pre_unclos_maritime_administration).
narrative_ontology:cs_drift_state('e9a498a1-7842-44ee-857d-7dc7a3857735', post_2016_arbitration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e9a498a1-7842-44ee-857d-7dc7a3857735', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, state_owned_energy_enterprises).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, commercial_shipping_companies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_title_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, intertemporal_law_principle).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, sovereign_rights_primacy_over_treaty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert sovereignty over maritime features based on historical usage records, maps, and administrative acts predating UNCLOS. Enforce claims through coast guard patrols, fishing fleet subsidies, island construction, and domestic legal frameworks that treat the claimed waters as internal or territorial seas. Gain resource access, strategic depth, and nationalist legitimacy.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold UNCLOS Article 57 EEZs that overlap with historical claims. Lose exclusive control over fisheries, seabed resources, and marine scientific research in disputed zones. Face pressure to negotiate joint development or cede access. Diplomatic and legal recourse is slow; military escalation risks conflict with more powerful claimants.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, continental).

% Receive symbolic and material benefits from the state's maritime expansion: national pride, fishing access for domestic fleets, energy security narratives. Also bear costs through tax-funded patrols, reduced foreign investment, and diplomatic isolation. Identity fused to territorial narrative makes exit politically unthinkable.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, domestic_nationalist_constituencies, payer).

% Gain exploration and extraction rights in disputed hydrocarbon basins under the cover of sovereign claims. Receive state backing for operations that would be illegal under strict EEZ reading. Their commercial viability depends on the political claim holding; they cannot independently enforce it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, state_owned_energy_enterprises, beneficiary,
    organized, biographical, constrained, continental).

% Face increased transit costs, insurance premiums, and routing uncertainty in waters where historical claims restrict freedom of navigation. Can reroute at expense of time and fuel, but chokepoints (Malacca, Lombok, Taiwan Strait) limit alternatives. Compliance costs are passed to global trade.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, commercial_shipping_companies, payer,
    organized, biographical, mobile, global).

% Depend entirely on EEZ resources for revenue and food security. Lack naval capacity to enforce their UNCLOS rights against expansive claimants. Lose fisheries access and seabed mineral prospects to historical claimants' patrols. International legal victories (e.g. arbitration) are unenforceable without great-power backing.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states, payer,
    powerless, generational, trapped, regional).

% Conduct freedom of navigation operations (FONOPs) challenging excessive maritime claims. Provide security guarantees to some EEZ holders. Their presence constrains but does not eliminate historical claim enforcement; they are neither beneficiaries nor victims of the historical-rights constraint itself.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, naval_powers_enforcing_fon, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states with deep historical maritime presence to assert continuity of sovereign rights across the UNCLOS transition, preventing a legal vacuum where pre-treaty administration would otherwise be erased by a rigid 200nm rule.
% TRANSFER_FUNCTION: Moves exclusive resource rights (fisheries, hydrocarbons, seabed minerals) and strategic control over maritime space from UNCLOS EEZ holders to historical claimants, backed by coercive presence and domestic legalization.
% ABSENT_VOICES: Indigenous coastal communities whose traditional fishing grounds cross claimed boundaries; future generations who inherit a militarized legal order; environmental defenders excluded from marine protection negotiations in disputed zones.
% DISAPPEARANCE_RATIONALE: If historical-rights claims were universally abandoned, EEZ boundaries would become the sole legal basis for maritime entitlement — claimant states would withdraw patrols and construction, EEZ holders would resume exclusive exploitation, FONOPs would diminish, and the regional security architecture would reorganize around UNCLOS compliance.
% FOUNDING_PROBLEM: Decolonization and the 1982 UNCLOS created a mismatch: states with centuries of maritime administration (e.g. China, Vietnam, Philippines) found their traditional waters reclassified as EEZs of other states or high seas, with no grandfathering for historical title.
% FOUNDING_PROBLEM_CORROBORATION: Claimant states' foreign ministries and legal advisories attest the problem remains live (historical title unrecognized). UNCLOS negotiators' records and ICJ/ICLOS jurisprudence (e.g. 2016 South China Sea Arbitration) attest the problem was addressed by the treaty's framework and is dead for non-parties; legal scholars outside the beneficiary set remain divided.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the transfer of resource rents and strategic control from EEZ holders to historical claimants across overlapping claims. Suppression (0.62) captures the active exclusion of EEZ holders' vessels, the physical prevention of resource exploitation, and the legal criminalization of compliance with UNCLOS in claimed zones. Theater ratio (0.38) acknowledges that historical research, museum exhibits, and domestic law-making perform a legitimacy function that exceeds the constraint's coordination necessity. Accessibility collapse (0.52) is moderate: alternative legal frameworks (UNCLOS dispute settlement, joint development) exist but are structurally blocked by claimant refusal to participate. Resistance (0.71) is high: arbitration rulings, FONOPs, diplomatic protests, and domestic political pushback in victim states are sustained.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state's seat, the constraint is coordination-restoring: it solves the intertemporal injustice of UNCLOS erasing pre-existing administration. From the EEZ holder's seat, it is extraction-enforcing: the same structure takes resources they hold under treaty law. From the small island state's seat, it is a snare: they are trapped in a system where legal title is unenforceable. The engine computes these seat-specific types from the structural data; the authored claimed_type (tangled_rope) represents the constraint's aggregate structural character.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are structural beneficiaries (d ≈ 0.15): they set the agenda, collect resource rents, and control enforcement. EEZ-holding coastal states are primary victims (d ≈ 0.85): they lose exclusive rights, face coercive exclusion, and lack effective exit. Domestic nationalist constituencies are identity-locked beneficiaries (d ≈ 0.30): they gain symbolic validation but bear fiscal and diplomatic costs; their identity fusion prevents exit. State-owned energy enterprises are constrained beneficiaries (d ≈ 0.25): they profit from access but depend on state enforcement. Commercial shippers are mobile payers (d ≈ 0.60): they absorb compliance costs but can reroute. Small island states are trapped payers (d ≈ 0.95): no exit, no enforcement capacity. Naval powers are analytical observers (d ≈ 0.50): they neither collect nor pay from this constraint directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intertemporal gap in maritime title) was genuine in 1982. By 2025, UNCLOS dispute settlement mechanisms, joint development precedents, and state practice have created alternative coordination pathways — but claimants reject them because the constraint now primarily serves resource extraction and strategic depth. The mandate has atrophied into extraction; the coordination function is vestigial. Mandatrophy is unresolved: the arrangement persists because abandoning it would cost claimants more (resources, legitimacy, strategic position) than maintaining it costs them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_threshold,
    'What quantum and continuity of historical usage suffices to generate a sovereign right that overrides a treaty-based EEZ?',
    'ICJ/ICLOS jurisprudence on historical title (e.g. Nicaragua v. Colombia, Chagos Advisory Opinion) applied to specific feature-by-feature records; archaeological and archival verification of continuity.',
    'A high threshold would restrict the beneficiary set to few claimants and reduce extraction; a low threshold would validate expansive claims and increase the constraint''s extractive reach across multiple maritime disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_evidence_threshold, conceptual, 'The evidentiary standard for historical title overriding treaty zones').

omega_variable(
    intertemporal_law_vs_treaty_primacy,
    'Does the intertemporal law principle (rights vest under law at the time of acquisition) structurally require historical rights to override later treaties, or can UNCLOS Article 311 permit such override only by agreement?',
    'Authoritative interpretation by UNCLOS Annex VII tribunals or ICJ of the relationship between customary historical title and UNCLOS''s comprehensive zoning scheme.',
    'If intertemporal law mandates override, the constraint''s coordination function is legally necessary (rope-ward); if UNCLOS displaces it absent agreement, the constraint is legally unfounded extraction (snare-ward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intertemporal_law_vs_treaty_primacy, conceptual, 'Whether historical rights legally survive UNCLOS by operation of law or require consent').

omega_variable(
    committer_frame_structure,
    'This constraint is one reading (historical_rights_reading) of the contested kernel ''unclos_sovereignty_boundary''. What structural elements differ across the sibling readings, and where is the disagreement located?',
    'Compare the three readings'' beneficiary/victim sets, coordination functions, and enforcement mechanisms. The disagreement is located in: (1) whether historical title generates rights that survive UNCLOS (this reading: yes; strict_eez_reading: no); (2) whether non-ratifiers can enforce EEZ-like zones (non_ratifier_enforcement_reading: yes via customary law; this reading: irrelevant, it asserts pre-treaty rights); (3) whether freedom of navigation is the primary constraint on claimants (non_ratifier_enforcement_reading''s focus) or a secondary effect.',
    'Clarifies that ε-invariance holds per reading: each reading has a stable beneficiary/victim structure and extractiveness. The kernel is the dispute; the readings are distinct constraints. Prevents conflating the contest into one constraint with variable metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structure, conceptual, 'Committer structure: kernel ID, reading ID, sibling readings, and structural delta locus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1996, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(uncl_tr_t2009, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(uncl_tr_t2016, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(uncl_tr_t2020, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(uncl_tr_t2025, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.22).
narrative_ontology:measurement(uncl_be_t1996, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1996, 0.35).
narrative_ontology:measurement(uncl_be_t2009, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2009, 0.48).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(uncl_be_t2016, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2016, 0.61).
narrative_ontology:measurement(uncl_be_t2020, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(uncl_be_t2025, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.15).
narrative_ontology:measurement(uncl_su_t1996, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1996, 0.25).
narrative_ontology:measurement(uncl_su_t2009, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2009, 0.38).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2012, 0.48).
narrative_ontology:measurement(uncl_su_t2016, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(uncl_su_t2020, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(uncl_su_t2025, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__historical_rights_reading, 0.15).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, south_china_sea_fisheries_access).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, south_china_sea_hydrocarbon_development).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, asean_china_code_of_conduct_negotiations).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial 'historical rights vs UNCLOS' dispute into a distinct reading of the unclos_sovereignty_boundary kernel. The strict_eez_reading (ε ≈ 0.15, claimed mountain) and non_ratifier_enforcement_reading (ε ≈ 0.45, claimed rope) have different beneficiary/victim structures and extraction profiles. All three are linked as a constraint family. This reading's extraction is higher because it actively transfers resource rights from EEZ holders; the strict_eez_reading's extraction is near-zero because it defends the treaty status quo; the non_ratifier_enforcement_reading extracts via naval enforcement costs imposed on all actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, institutional, 0.15).
constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
