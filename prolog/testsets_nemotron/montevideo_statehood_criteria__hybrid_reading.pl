% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Statehood Criteria: Montevideo Plus Normative Legitimacy (Hybrid Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The hybrid reading of the Montevideo criteria holds that statehood
 *   requires both the four objective criteria (permanent population, defined
 *   territory, government, capacity to enter relations) AND normative
 *   legitimacy demonstrated through democratic governance, human rights
 *   compliance, and non-aggression. This reading emerged from the UN
 *   Charter's sovereign equality principle combined with the human rights
 *   revolution, decolonization's 'self-determination = democratic governance'
 *   equation, and the post-Cold War democratic entitlement thesis. It is
 *   contested: declaratory purists say it adds non-legal conditions;
 *   constitutive purists say it still constrains political discretion too
 *   much. The constraint operates as a tangled rope because it genuinely
 *   coordinates recognition practice (providing a shared evaluative
 *   framework) while asymmetrically extracting recognition from non-liberal
 *   aspirants and transferring interpretive authority to liberal democratic
 *   states and international lawyers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Statehood Criteria: Montevideo Plus Normative Legitimacy (Hybrid Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'e6d0f87a-81d5-4cd3-af87-c336893009d3').
narrative_ontology:cs_kernel_codification('e6d0f87a-81d5-4cd3-af87-c336893009d3', formalized).
narrative_ontology:cs_authority_grounding('e6d0f87a-81d5-4cd3-af87-c336893009d3', lineage).
narrative_ontology:cs_interpretation_layer_present('e6d0f87a-81d5-4cd3-af87-c336893009d3').
narrative_ontology:cs_reading_relation('e6d0f87a-81d5-4cd3-af87-c336893009d3', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('e6d0f87a-81d5-4cd3-af87-c336893009d3', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('e6d0f87a-81d5-4cd3-af87-c336893009d3', foundational, statehood_requires_democratic_governance).
narrative_ontology:cs_axiom_status(statehood_requires_democratic_governance, holdable).
narrative_ontology:cs_axiom_grounding('e6d0f87a-81d5-4cd3-af87-c336893009d3', statehood_requires_democratic_governance, instrumental).
narrative_ontology:cs_axiom('e6d0f87a-81d5-4cd3-af87-c336893009d3', foundational, human_rights_compliance_conditions_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_compliance_conditions_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e6d0f87a-81d5-4cd3-af87-c336893009d3', human_rights_compliance_conditions_sovereignty, deontological).
narrative_ontology:cs_axiom('e6d0f87a-81d5-4cd3-af87-c336893009d3', secondary, non_aggression_as_statehood_prerequisite).
narrative_ontology:cs_axiom_status(non_aggression_as_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('e6d0f87a-81d5-4cd3-af87-c336893009d3', non_aggression_as_statehood_prerequisite, conventional).
narrative_ontology:cs_reference_frame('e6d0f87a-81d5-4cd3-af87-c336893009d3', post_charter_self_determination_framework).
narrative_ontology:cs_drift_state('e6d0f87a-81d5-4cd3-af87-c336893009d3', post_cold_war_democratic_entitlement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e6d0f87a-81d5-4cd3-af87-c336893009d3', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_legal_establishment).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_aspirant_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_under_contested_governance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, great_power_patrons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the recognition agenda through UN Security Council vetoes, diplomatic recognition practices, and control of international financial institutions. Gain normative cover to deny recognition to rivals while claiming objectivity. Can shift between declaratory and constitutive readings as politically convenient.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter).

% Interprets and applies statehood criteria in ICJ opinions, UN admission recommendations, and treaty body practice. Professional authority and institutional relevance depend on the criteria requiring expert legal judgment rather than mechanical application. Collects interpretive authority rents.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, international_legal_establishment, beneficiary).

% Use the normative legitimacy reading to justify R2P interventions, regime change, and recognition of opposition governments. Gain legal vocabulary to convert political preferences into international legal obligations. Funding and institutional access flow from this function.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates, beneficiary,
    organized, biographical, mobile, global).

% Control territory, population, and government (meet objective criteria) but are denied recognition because their governance model fails the normative legitimacy test. No exit from the criteria — cannot become liberal democratic without abandoning their political project. Bear the costs of non-recognition: no treaty access, no diplomatic protection, economic isolation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer).

% Meet objective criteria but face recognition denial or conditional engagement based on human rights/democracy deficits. Pay through sanctions, exclusion from institutions, and loss of sovereign immunities. Can sometimes buy recognition through strategic concessions or great power patronage.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_aspirant_states, payer,
    powerful, biographical, constrained, national).

% Live in entities that meet objective criteria but fail normative tests (e.g., Somaliland, Taiwan, Northern Cyprus). Suffer the practical consequences of non-recognition: no passport recognition, no international legal personality, limited humanitarian access. Have no voice in the criteria that determine their status.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, populations_under_contested_governance, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, populations_under_contested_governance, excluded).

% Use the hybrid reading's flexibility to recognize or deny statehood based on strategic interest. The normative criteria provide a vocabulary to justify either choice. Collect geopolitical leverage from the ambiguity. Not bound by the criteria they enforce on others.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, great_power_patrons, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, great_power_patrons, beneficiary).

% Analyze the criteria's operation across cases. See the structural divergence: the same criteria function as coordination for established states and extraction for aspirants. Produce the doctrinal literature that either naturalizes or critiques the arrangement.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary for the international community to assess new statehood claims without ad hoc politics dominating every case. The objective criteria (territory, population, government, capacity) create a baseline; the normative layer adds a filter for governance quality that coordinates expectations about rights and obligations.
% TRANSFER_FUNCTION: Moves recognition, treaty access, diplomatic protection, and sovereign immunities from aspirant entities that meet objective criteria but fail normative tests to the interpreting community (liberal democratic states and international institutions) who control the gateway. The transfer is not monetary but juridical-political: the status of 'state' and its attendant privileges.
% ABSENT_VOICES: The populations of aspirant entities (Somalilanders, Taiwanese, Sahrawis, etc.) who live under effective governance but are denied the international legal personality that would give them direct access to human rights mechanisms, trade agreements, and diplomatic protection. Also absent: non-liberal political movements that explicitly reject the democratic governance criterion as a Western imposition.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished and only the declaratory reading operated, 15-20 entities would gain immediate recognition, shifting UN voting balances, treaty participation, and resource access. If only the constitutive reading operated, recognition would become purely political with no legal baseline. The hybrid reading structures the middle ground where most contests actually play out.
% FOUNDING_PROBLEM: The pure declaratory reading (1933 Montevideo Convention) allowed brutal dictatorships and failed states to claim full sovereignty, while the pure constitutive reading made statehood a gift of the powerful. The hybrid reading emerged post-1945 (UN Charter, decolonization, human rights regime) to solve: how to condition statehood on governance quality without returning to pure political discretion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UN Charter drafting history (San Francisco 1945), the decolonization declarations (1960s), and the post-Cold War 'democratic entitlement' literature (Franck 1992). Critics from the Global South (Third World Approaches to International Law scholars) and realist international lawyers corroborate that the governance-quality condition was contested from the start and serves Western strategic interests — not a self-assertion by the beneficiaries.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because the normative layer gates access to the full privileges of statehood for entities that already satisfy the objective criteria — the coordination function (baseline criteria) is real but the normative layer extracts the difference. Suppression is high (0.75) because the criteria are enforced through Security Council vetoes, non-recognition policies, and conditional engagement that actively prevent aspirants from exercising sovereign rights. Theater ratio is moderate (0.42): the human rights/democracy monitoring apparatus (UNHRC, treaty bodies, election observation) performs real assessment but increasingly serves as a recognition gatekeeping mechanism. Accessibility collapse (0.62) and resistance (0.58) reflect that alternatives (pure declaratory, pure constitutive) remain intellectually coherent and politically advocated but are structurally marginalized in practice.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal democratic state seat, the constraint appears as a genuine coordination mechanism that prevents 'bad' states from claiming sovereignty's protections. From the non-liberal secessionist seat, the same structure appears as a rigged game where the rules were written by the winners to exclude challengers. The engine computes this divergence from the power/exit/spatial_scope declarations — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and the international legal establishment are structural beneficiaries (d near 0.0-0.2): they control the interpretation, collect the interpretive authority rents, and gain normative justification for discretionary recognition. Non-liberal secessionists and populations under contested governance are structural victims (d near 0.9-1.0): they meet objective criteria but face near-total exclusion with no exit from the normative test. Authoritarian aspirants sit at moderate-high d (0.6-0.7): they have some leverage (resources, great power patrons) but remain constrained. Great power patrons operate at the beneficiary end (d ~0.15) because they arbitrage the criteria. The analytical observer seat sees the full structure (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (conditioning statehood on governance quality without pure discretion) remains live but the hybrid reading has accumulated extraction: the normative criteria have expanded (human rights treaty bodies, R2P, democratic conditionality in trade agreements) while the objective criteria have attenuated (Somaliland meets all four but remains unrecognized). The mandate has not atrophied — the coordination need persists — but the extraction-to-coordination ratio has shifted. This is tangled_rope, not snare, because the coordination function remains real and the beneficiaries include the broader international order (predictable recognition practice), not just a narrow extractive coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_layer_necessity,
    'Is the normative legitimacy layer (democracy, human rights, non-aggression) structurally necessary for the international order''s coordination function, or is it an extractive overlay that could be removed while preserving the coordination baseline?',
    'Counterfactual analysis: compare recognition stability and conflict rates in periods/regions where the hybrid reading dominated vs. where declaratory or constitutive readings operated. If coordination holds without the normative layer, it is extractive overhead.',
    'If the normative layer is extractive overhead, the constraint reclassifies toward snare for the victim seats; if it is coordination-necessary, the tangled_rope classification holds across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_layer_necessity, conceptual, 'Whether the normative criteria are coordination infrastructure or extraction machinery').

omega_variable(
    committer_framing_underdetermination,
    'Does the hybrid reading''s kernel framing (statehood = objective criteria + normative legitimacy) represent the only coherent commitment-system reading, or does an alternative framing (e.g., statehood = objective criteria + great power consent) produce a different cs_pattern classification?',
    'Map the cs_structure parameters (kernel_codification, authority_grounding, interpretation_layer_present) for each viable framing of the same kernel and compute resulting cs_pattern classifications. If multiple framings produce different patterns, the framing choice is underdetermined.',
    'If framing underdetermination exists, the cs_pattern classification is not a property of the kernel but of the reading''s chosen commitment-system frame — requiring an omega to document the alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Commitment-system framing ambiguity for the Montevideo kernel').

omega_variable(
    democratic_peace_causal_claim,
    'Does the democratic peace proposition (liberal democracies don''t fight each other) causally justify the democratic governance criterion, or is the causal claim contested/empirically weak such that the criterion rests on ideological preference?',
    'Systematic review of the democratic peace literature including recent critiques (Rosato 2003, Layne 1994, Gowa 1999) and the ''democratic peace'' as a social scientific consensus vs. a normative project.',
    'If the causal claim is weak, the democratic governance criterion loses its coordination justification (preventing war) and becomes pure normative preference — increasing extractiveness for non-liberal aspirants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_peace_causal_claim, empirical, 'Empirical grounding of the democratic governance criterion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, recognition_as_political_act).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, democratic_entitlement_thesis).

% DUAL FORMULATION NOTE:
% The Montevideo kernel decomposes into three constraint stories: declaratory_reading (Mountain candidate — objective criteria as natural legal fact), constitutive_reading (Rope/Snare hybrid — recognition as political gift), and hybrid_reading (this story, Tangled Rope — coordination plus normative extraction). The hybrid reading influences both siblings: it creates downstream pressure on the declaratory reading by making 'objective criteria alone' legally incomplete, and on the constitutive reading by providing a legal vocabulary that constrains pure discretion. Neither sibling is foreclosed — all three remain live in different institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerful, 0.65).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
