% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Maritime Sovereignty via Artificial Island Construction
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The expansive construction reading of UNCLOS maritime sovereignty holds
 *   that states which construct artificial installations on submerged
 *   features or low-tide elevations, and subsequently exercise effective
 *   occupation and administrative control, generate de facto territorial seas
 *   of twelve nautical miles or more. This reading is contested by
 *   neighboring claimant states and major maritime powers, who assert that
 *   UNCLOS Articles 60 and 121 limit artificial features to 500-meter safety
 *   zones that generate no territorial sea. The constraint operates as a
 *   standing arrangement in contested maritime regions, notably the South
 *   China Sea, where the constructing state enforces jurisdictional claims
 *   through naval patrol, administrative regulation, and exclusion of foreign
 *   vessels. The arrangement extracts maritime spatial rights from the
 *   international community and adjacent coastal states and concentrates them
 *   in the constructing state, while providing a jurisdictional ordering
 *   function that the constructing state presents as stabilizing.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary agenda-setter and beneficiary (institutional/mobile) â designs, builds, administers, and enforces artificial installations and claims territorial entitlements.
 *   - neighboring_claimant_states: Primary payer (institutional/constrained) â lose traditional fishing grounds, resource access, and navigational freedom to expanded territorial envelopes.
 *   - freedom_of_navigation_states: Secondary payer (institutional/mobile) â mount Freedom of Navigation operations and diplomatic resistance but bear costs of militarized seas and restricted routing.
 *   - international_arbitration_bodies: Analytical observer (institutional/analytical) â issue findings on UNCLOS interpretation that the constructing state rejects.
 *   - coastal_fishing_communities: Excluded payer (powerless/trapped) â displaced from traditional fishing grounds by enforcement of exclusion zones without voice in the legal process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Maritime Sovereignty via Artificial Island Construction").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'c403b2bb-7c61-4730-9abc-b592b2d62a55').
narrative_ontology:cs_kernel_codification('c403b2bb-7c61-4730-9abc-b592b2d62a55', formalized).
narrative_ontology:cs_authority_grounding('c403b2bb-7c61-4730-9abc-b592b2d62a55', lineage).
narrative_ontology:cs_interpretation_layer_present('c403b2bb-7c61-4730-9abc-b592b2d62a55').
narrative_ontology:cs_reading_relation('c403b2bb-7c61-4730-9abc-b592b2d62a55', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('c403b2bb-7c61-4730-9abc-b592b2d62a55', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('c403b2bb-7c61-4730-9abc-b592b2d62a55', foundational, effective_occupation_generates_territorial_sea).
narrative_ontology:cs_axiom_status(effective_occupation_generates_territorial_sea, holdable).
narrative_ontology:cs_axiom_grounding('c403b2bb-7c61-4730-9abc-b592b2d62a55', effective_occupation_generates_territorial_sea, conventional).
narrative_ontology:cs_axiom('c403b2bb-7c61-4730-9abc-b592b2d62a55', foundational, administrative_control_confers_maritime_sovereignty).
narrative_ontology:cs_axiom_status(administrative_control_confers_maritime_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c403b2bb-7c61-4730-9abc-b592b2d62a55', administrative_control_confers_maritime_sovereignty, conventional).
narrative_ontology:cs_reference_frame('c403b2bb-7c61-4730-9abc-b592b2d62a55', effective_occupation_maritime_doctrine).
narrative_ontology:cs_drift_state('c403b2bb-7c61-4730-9abc-b592b2d62a55', post_scs_arbitration_2016, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c403b2bb-7c61-4730-9abc-b592b2d62a55', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct artificial installations on submerged features and low-tide elevations, staff them with administrative and military personnel, declare territorial sea baselines, and enforce exclusion through naval patrols and coast guard. Collects sovereign jurisdiction over surrounding waters, airspace, and seabed resources. Could de-escalate by abandoning the installations but faces domestic nationalist constraints and strategic investment sunk costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, beneficiary).

% Lose access to traditional fishing grounds, hydrocarbon exploration blocks, and navigational freedom within the declared territorial sea of artificial installations. Diplomatic protests are ignored; legal arbitration is structurally unenforceable against the constructing state; military escalation risks open conflict with a larger power. Exit is limited to accepting reduced maritime entitlement or joining multilateral coalitions with uncertain commitment.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    institutional, generational, constrained, regional).

% Conduct Freedom of Navigation operations and overflight missions to challenge excessive maritime claims, bearing operational costs and occasional risk of confrontation. Fund diplomatic and legal support for neighboring claimants. Cannot fully exit without ceding strategic sea lanes to unilateral control; their resistance is persistent but does not reverse the installations.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, mobile, global).

% Issue binding and non-binding findings on UNCLOS interpretation, including determinations that artificial installations generate no territorial sea. Findings are systematically rejected or ignored by the constructing state. Their authority is procedural and discursive; they have no enforcement arm.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_arbitration_bodies, observer,
    institutional, civilizational, analytical, global).

% Traditionally fished the waters now enclosed by artificial-installation territorial claims. Excluded by coast guard patrols and maritime law enforcement without being parties to the interstate legal dispute. Have no standing in arbitration and no political voice in the claimant governments' strategic calculations. Displacement is economic and cultural.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, coastal_fishing_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes maritime jurisdiction in crowded seas by converting physical installations into administrative boundaries, creating a unilateral territorial and resource-management regime where natural features are absent.
% TRANSFER_FUNCTION: Moves sovereign jurisdiction over territorial sea, airspace, and seabed resources from the international community and neighboring coastal states to the island-constructing state through the doctrine of effective occupation applied to artificial structures.
% ABSENT_VOICES: Coastal fishing communities displaced by exclusion zones; smaller neighboring states without UNCLOS arbitration capacity or naval power; future generations who lose high-seas and common-heritage access. They would object to the territorialization of artificial features but are not in the interstate diplomatic room.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, the constructing state's naval and administrative control would lose its legal-territorial backing; neighboring claimants would resume fishing and exploration; freedom-of-navigation operations would become unnecessary; commercial shipping routes would revert to high-seas transit regimes; the regional maritime order would rearrange around the UNCLOS baseline or active military contestation.
% FOUNDING_PROBLEM: How to establish stable sovereignty and resource control over contested maritime features in the absence of naturally formed high-tide islands, without resorting to open warfare.
% FOUNDING_PROBLEM_CORROBORATION: Island-constructing states attest the problem remains live due to unresolved territorial instability. Neighboring claimants, freedom-of-navigation states, and the Permanent Court of Arbitration (South China Sea Arbitration, PCA Case No. 2013-19) attest that UNCLOS definitively addressed the status of artificial features in 1982; the tribunal findings and supporting diplomatic notes from outside the beneficiary set corroborate the dead-problem reading.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers territorial sea, airspace, and resource jurisdiction from the commons and neighboring states to the constructing state through a legal mechanism that UNCLOS drafters explicitly sought to prevent (Article 60). Suppression is high (0.71) because the constraint requires continuous naval and coast-guard enforcement to exclude foreign vessels and aircraft, and because legal alternatives (arbitration) are structurally ignored by the beneficiary state. Theater ratio is moderate (0.45): much of the 'effective occupation' is performative (civilian settlements, flag-raisings, tourism flights) layered onto hard enforcement, creating a spectacle of normalcy that supplements the legal claim. Accessibility collapse (0.68) is substantial because, once the installations are built and occupied, reversal requires military confrontation or sustained multilateral pressure that neighboring states cannot individually mount. Resistance (0.72) is high because Freedom of Navigation operations, diplomatic protests, and arbitration are actively pursued by targeted states. Temporal measurements track the maturation of the constraint from initial construction through hardened enforcement and performative normalization.
 *
 * PERSPECTIVAL GAP:
 *   The island-constructing state experiences the constraint as a legitimate exercise of sovereignty that stabilizes a previously ungoverned maritime space; its directionality sits near the beneficiary end. Neighboring claimant states and freedom-of-navigation states experience the identical physical installations as unilateral appropriation of public maritime space; their directionality sits near the target end. The engine should compute substantially different effective extraction across these seats despite their shared institutional power level, driven by beneficiary/victim declarations and divergent exit options (the constructing state can de-escalate at will; neighboring states are trapped by the fait accompli).
 *
 * DIRECTIONALITY LOGIC:
 *   The constructing state is both agenda-setter and beneficiary: it designs the installations, sets the administrative rules, patrols the perimeter, and collects the territorial and resource rents (d near 0.0). Neighboring claimant states are payers: they forfeit resource access and navigational rights they would hold under a strict UNCLOS reading (d near 1.0). Freedom-of-navigation states are payers: they must divert naval assets to contested zones and accept restricted operational freedom (d near 0.85). Coastal fishing communities are excluded payers with no institutional voice (d near 1.0, amplified by powerlessness and trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling the arrangement as pure extraction by acknowledging that maritime jurisdiction is a genuine coordination problem: without some regime to allocate space and resource rights, contested seas risk open conflict. However, it also prevents mislabeling as pure coordination by recording the asymmetric transfer: the expansive reading solves the boundary problem by assigning all surplus jurisdiction to the constructing state. If the coordination function were genuine and symmetric, the reading would be a Rope; if the coordination story were entirely fabricated, it would be a Snare. The Tangled Rope designation captures that the coordination is real but the distribution is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the expansive construction reading capture the correct interpretation of UNCLOS, or is it a constructed doctrine that benefits island-constructing states at the expense of neighboring claimants?',
    'Comparative legal analysis across tribunal findings (e.g., South China Sea Arbitration PCA Case No. 2013-19) and subsequent state practice to determine whether effective occupation of artificial features is accepted as a source of territorial sea under international law.',
    'If the strict geographic reading is legally correct, this constraint is a false legal summit (naturalized as law but constructed for extraction) and computes as a heavily extractive Tangled Rope or Snare; if the expansive reading is legally correct, extraction may be recast as lawful coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the expansive reading is a genuine legal interpretation or a constructed extraction mechanism').

omega_variable(
    suppression_sustainability,
    'Can the constructing state sustain the naval and administrative enforcement costs indefinitely, or will enforcement decay normalize the constraint without active maintenance?',
    'Longitudinal tracking of patrol frequency, installation resupply rates, and administrative staffing levels in the contested zone over the next decade.',
    'If enforcement is unsustainable, the constraint may drift toward Piton (theater-dominant, inertial persistence); if sustainable, it remains active Tangled Rope or hardens into Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_sustainability, empirical, 'Whether active enforcement can be maintained long-term').

omega_variable(
    arbitration_compliance_gap,
    'Does the constraint persist because the international legal system lacks enforcement mechanisms, or because the constructing state holds a good-faith legal position that tribunals have failed to properly credit?',
    'Analysis of state statements, diplomatic correspondence, and domestic legal integration of tribunal findings to distinguish defiance from genuine interpretive disagreement.',
    'If pure defiance, suppression is higher than legal metrics suggest and the constraint is more extractive; if good-faith disagreement, the constraint may be a contested coordination mechanism rather than pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arbitration_compliance_gap, conceptual, 'Whether non-compliance is power politics or genuine legal disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(uncl_tr_t32, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(uncl_be_t32, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(uncl_su_t32, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the unclos_maritime_sovereignty kernel family. The expansive_construction_reading treats artificial features as generating immediate territorial sea; strict_geographic_reading treats them as legally irrelevant to territorial sea; hybrid_effective_control_reading treats them as generating limited safety zones with possible future maturation. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as alternative decompositions of the same colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
