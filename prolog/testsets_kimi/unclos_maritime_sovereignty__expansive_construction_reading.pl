% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: UNCLOS Expansive Artificial Island Sovereignty Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the expansive construction reading of the
 *   UNCLOS maritime sovereignty kernel. Under this reading, states that
 *   construct artificial islands on submerged features or low-tide elevations
 *   and maintain effective occupation and administrative control generate de
 *   facto territorial seas and potentially broader maritime entitlements. The
 *   reading is most prominently advanced by major maritime powers in the
 *   Indo-Pacific. It extracts jurisdictional rights from neighboring
 *   claimants and the global freedom-of-navigation community by converting
 *   temporary installations into permanent sovereignty markers. The
 *   claim/metric independence is maintained: the claimed type is tangled_rope
 *   because the reading purports to solve a genuine coordination
 *   problemâwho governs distant waters?âwhile asymmetrically
 *   concentrating rights on the constructing state.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary beneficiary/agenda_setter (powerful/arbitrage) â asserts and enforces the expansive reading
 *   - neighboring_claimants: Primary target (moderate/constrained) â loses maritime entitlement to artificial construction
 *   - freedom_of_navigation_states: Secondary target (powerful/constrained) â bears operational costs of challenging excessive claims
 *   - coastal_fishing_communities: Excluded voice (powerless/trapped) â loses access without legal standing
 *   - international_arbitration_bodies: Analytical observer (institutional/analytical) â issues adverse awards that the constructing state rejects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.82).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "UNCLOS Expansive Artificial Island Sovereignty Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '9d9f4ebc-0072-4913-8e58-26225045ff2f').
narrative_ontology:cs_kernel_codification('9d9f4ebc-0072-4913-8e58-26225045ff2f', formalized).
narrative_ontology:cs_authority_grounding('9d9f4ebc-0072-4913-8e58-26225045ff2f', lineage).
narrative_ontology:cs_interpretation_layer_present('9d9f4ebc-0072-4913-8e58-26225045ff2f').
narrative_ontology:cs_reading_relation('9d9f4ebc-0072-4913-8e58-26225045ff2f', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('9d9f4ebc-0072-4913-8e58-26225045ff2f', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('9d9f4ebc-0072-4913-8e58-26225045ff2f', foundational, artificial_structures_entitled_to_territorial_sea).
narrative_ontology:cs_axiom_status(artificial_structures_entitled_to_territorial_sea, holdable).
narrative_ontology:cs_axiom_grounding('9d9f4ebc-0072-4913-8e58-26225045ff2f', artificial_structures_entitled_to_territorial_sea, conventional).
narrative_ontology:cs_axiom('9d9f4ebc-0072-4913-8e58-26225045ff2f', foundational, effective_occupation_immediate_full_effect).
narrative_ontology:cs_axiom_status(effective_occupation_immediate_full_effect, holdable).
narrative_ontology:cs_axiom_grounding('9d9f4ebc-0072-4913-8e58-26225045ff2f', effective_occupation_immediate_full_effect, conventional).
narrative_ontology:cs_reference_frame('9d9f4ebc-0072-4913-8e58-26225045ff2f', effective_control_generates_maritime_rights).
narrative_ontology:cs_drift_state('9d9f4ebc-0072-4913-8e58-26225045ff2f', post_south_china_sea_arbitration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d9f4ebc-0072-4913-8e58-26225045ff2f', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, effective_occupation_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, maritime_sovereignty_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct artificial installations on submerged features and low-tide elevations, garrison them with administrative and military personnel, and assert territorial seas and contiguous zones around them. Justifies the claims as effective occupation and administrative control under international law. Collects exclusive maritime jurisdiction, resource rights, and strategic depth.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    powerful, generational, arbitrage, global).

% Contest the maritime entitlements generated by artificial construction, asserting that the features lie within their own claimed zones or in the international commons. Face a fait accompli of physical occupation and patrols that makes legal reversal costly and politically fraught.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimants, payer,
    moderate, generational, constrained, regional).

% Conduct freedom-of-navigation operations, diplomatic protests, and alliance coordination to challenge excessive maritime claims. Bear operational, budgetary, and geopolitical escalation costs to prevent precedent-setting acceptance of the expansive reading.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    powerful, generational, constrained, global).

% Lose traditional fishing grounds to patrols and exclusion zones around artificial structures. Lack standing in interstate legal proceedings and are not consulted in sovereignty negotiations.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, coastal_fishing_communities, excluded,
    powerless, biographical, trapped, local).

% Issue awards interpreting UNCLOS provisions on artificial islands and maritime entitlement. Their authority depends on state consent; adverse awards are rejected by the constructing states, limiting practical effect.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_arbitration_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves jurisdictional ambiguity over maritime space surrounding offshore installations by establishing a single sovereign authority through physical presence and administrative control, ostensibly replacing anarchy with governance.
% TRANSFER_FUNCTION: Moves maritime jurisdictional rightsâterritorial sea, exclusive economic zone privileges, and airspace controlâfrom neighboring coastal states and the international commons to the state that constructs and occupies the artificial feature.
% ABSENT_VOICES: Coastal fishing communities and indigenous sea nomads lose traditional access but are not parties to interstate legal proceedings; environmental scientists documenting reef destruction are sidelined in sovereignty-centric forums; smaller non-claimant littoral states lack standing in bilateral disputes.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished, artificial structures would generate only 500-meter safety zones under UNCLOS Article 60. Constructing states would lose the legal basis for extensive maritime patrols, air defense identification zones, and resource exclusion, while neighboring claimants and global shipping interests would regain navigational and fisheries access.
% FOUNDING_PROBLEM: UNCLOS left interpretive ambiguity regarding whether artificial structures on submerged features or low-tide elevations generate maritime zones, and how to establish stable governance in distant waters where permanent natural land is absent.
% FOUNDING_PROBLEM_CORROBORATION: The Philippines, Vietnam, and legal scholarship outside the constructing states corroborate that the ambiguity was exploited rather than resolved; the UNCLOS III travaux prÃ©paratoires and post-2016 PCA awards support the view that artificial structures were not intended to generate full territorial seas, while the constructing states assert the opposite.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the transfer of full territorial sea rights from a 500-meter safety zone basis to a 12-nautical-mile (or broader) entitlement is a massive jurisdictional expansion relative to the physical investment. Suppression (0.78) is high because the constraint depends on coast guard patrols, naval presence, and diplomatic pressure to exclude rival users. Theater_ratio (0.52) reflects that while administrative functions (search and rescue, weather monitoring) are genuine, a substantial share of activity is performative assertion of sovereignty over an artificial substrate. Accessibility_collapse (0.68) captures the fait accompli dynamic: once an artificial island is garrisoned, reversing the situation becomes politically and militarily costly, collapsing practical alternatives. Resistance (0.75) is high due to persistent FONOPs, diplomatic protests, and the 2016 PCA award. The temporal series show extraction and enforcement ratcheting up during the 2010â2020 construction boom, with theater peaking as the performance of legitimacy reached its maximum during the building phase.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat, the constraint is a necessary defensive measure to protect sovereign rights and maritime resources in a contested environment; from the neighboring claimant's seat, it is an illegal territorial grab that rewrites geography; from the freedom-of-navigation state's seat, it is a threat to the global commons that demands active military and diplomatic countermeasures. The engine computes these divergent types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Island_constructing_states are declared beneficiaries and agenda-setters with arbitrage-grade exit options; the engine will derive a low directionality (near 0.0), damping or inverting effective extraction into subsidy. Neighboring_claimants and freedom_of_navigation_states are declared victims with constrained exit; the engine will derive high directionality (near 1.0), amplifying effective extraction. The spatial scope differential (global vs regional) further modulates the Ï computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the arrangement as pure extraction (Snare) because the constraint does address a real coordination problemâmaritime governance in areas distant from natural landâthrough a mechanism (effective occupation) that has historical roots in international law. However, the classification also prevents mislabeling it as pure coordination (Rope) because the beneficiaries and victims are structurally asymmetric: the constructing state captures full territorial rights while others lose access. If the coordination function were primary and the extraction incidental, we would expect shared governance or multilateral administration rather than unilateral exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretive_validity,
    'Does the expansive construction reading represent a defensible interpretation of UNCLOS Articles 60 and 121, or an extra-legal territorial expansion using legal rhetoric?',
    'Comparative legal analysis by neutral international law scholars; examination of UNCLOS travaux prÃ©paratoires and subsequent state practice.',
    'If the reading is legally indefensible, the constraint collapses toward Snare; if defensible, it stabilizes as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretive_validity, conceptual, 'Whether the reading is legally valid or mere cover.').

omega_variable(
    authority_grounding_ambiguity,
    'Is the authority of this reading grounded in genuine treaty lineage, or in the extracting state''s power to prevent kernel revision?',
    'Analysis of whether the state advances this reading through legal argumentation or through fait accompli backed by military and economic coercion.',
    'If the latter, authority_grounding should be extraction rather than lineage, altering drift-state analysis and the interpretation_layer assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Lineage versus extraction as authority source.').

omega_variable(
    enforcement_sustainability,
    'Can the constructing state sustain the administrative and military presence required to maintain the expansive claims indefinitely?',
    'Long-term budgetary, strategic capacity, and alliance-structure analysis.',
    'If unsustainable, the constraint will drift toward Piton or collapse; if sustainable, it stabilizes as persistent Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Whether enforcement capacity can persist over generational time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_exp_const_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unclos_exp_const_tr_t5, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(unclos_exp_const_tr_t10, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(unclos_exp_const_tr_t15, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(unclos_exp_const_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(unclos_exp_const_tr_t25, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(unclos_exp_const_tr_t30, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(unclos_exp_const_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(unclos_exp_const_be_t5, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(unclos_exp_const_be_t10, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(unclos_exp_const_be_t15, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(unclos_exp_const_be_t20, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(unclos_exp_const_be_t25, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(unclos_exp_const_be_t30, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(unclos_exp_const_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(unclos_exp_const_su_t5, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(unclos_exp_const_su_t10, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(unclos_exp_const_su_t15, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(unclos_exp_const_su_t20, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(unclos_exp_const_su_t25, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(unclos_exp_const_su_t30, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_maritime_sovereignty kernel. The kernel conflates three structurally distinct legal claims: (1) that only natural high-tide features generate maritime zones (strict_geographic_reading), (2) that artificial features generate full territorial seas immediately (this reading), and (3) that artificial features generate limited safety zones but may mature into territorial claims over time (hybrid_effective_control_reading). Each reading has distinct beneficiary/victim structures, Îµ values, and enforcement requirements. They are linked as a constraint family but are not causally dependent; they are mutually exclusive interpretations competing for institutional acceptance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
