% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Threshold
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The graduated sovereignty reading treats sovereignty not as a binary
 *   status but as a spectrum indexed to measurable state capacity and
 *   governance legitimacy. In practice, the thresholds are set and applied by
 *   major powers and their allied international institutions. The constraint
 *   extracts decision-making authority and resource control from weak states
 *   and transfers it to interveners, while maintaining a humanitarian
 *   coordination cover story. The claimed type (snare) reflects the
 *   structural reality: the coordination function (responding to state
 *   failure) is real but subordinate to the extraction function (neo-colonial
 *   resource and strategic access). The engine will compute per-seat types
 *   from the declared structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.6).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Threshold").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '9e5ba77b-f768-4133-88fe-cffbcc7f55c2').
narrative_ontology:cs_kernel_codification('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', formalized).
narrative_ontology:cs_authority_grounding('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', extraction).
narrative_ontology:cs_interpretation_layer_present('9e5ba77b-f768-4133-88fe-cffbcc7f55c2').
narrative_ontology:cs_reading_relation('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', foundational, sovereignty_is_continuous_not_categorical).
narrative_ontology:cs_axiom_status(sovereignty_is_continuous_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', sovereignty_is_continuous_not_categorical, conventional).
narrative_ontology:cs_axiom('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', foundational, state_capacity_and_legitimacy_are_legitimate_intervention_criteria).
narrative_ontology:cs_axiom_status(state_capacity_and_legitimacy_are_legitimate_intervention_criteria, holdable).
narrative_ontology:cs_axiom_grounding('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', state_capacity_and_legitimacy_are_legitimate_intervention_criteria, instrumental).
narrative_ontology:cs_reference_frame('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', westphalian_equality_1945).
narrative_ontology:cs_drift_state('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', post_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9e5ba77b-f768-4133-88fe-cffbcc7f55c2', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, major_power_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_ngo_network).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, fragile_state_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, regional_powers).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, regional_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the military, economic, and diplomatic capacity to enforce sovereignty reclassifications. They fund and authorize intervention mechanisms (UNSC resolutions, coalition operations, sanctions regimes) and benefit from expanded legal discretion to intervene in resource-rich or strategically positioned weak states. Their exit from any specific intervention is costless; they arbitrage across intervention opportunities.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, major_power_interveners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, major_power_interveners, beneficiary).

% Gain mandate, funding, and operational access through sovereignty reclassification. Humanitarian, development, and governance NGOs receive expanded authority to operate inside reclassified states, often with immunity from local law. They can shift focus across theaters and funding streams, making their exit mobile rather than trapped.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_ngo_network, beneficiary,
    organized, biographical, mobile, global).

% Lack the capacity to meet the graduated thresholds and the political leverage to contest reclassification. Their sovereignty is conditionally recognized and can be suspended by external actors. Exit from the international system is not viable — they depend on it for recognition, aid, and market access. They bear the costs of intervention (loss of territorial control, resource extraction, imposed governance structures) without the power to refuse.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% Experience intervention as both protection and extraction. They may receive humanitarian aid but also suffer collateral damage, displacement, and governance by external actors with no accountability to them. Their exit options are minimal — borders are closed, refugee status is uncertain, and they cannot influence the reclassification criteria. They are the ultimate bearers of the constraint's extraction.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, fragile_state_populations, payer,
    powerless, biographical, trapped, local).

% Occupy an intermediate position: they sometimes act as interveners in their neighborhood (beneficiary) and sometimes face graduated-sovereignty pressure from major powers (payer). Their exit is constrained — they cannot fully leave the international system but can build regional alternatives (e.g., AU, ASEAN frameworks). They experience the constraint from both sides.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, regional_powers, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, regional_powers, beneficiary).

% Produce the doctrinal frameworks that legitimate or contest graduated sovereignty. They do not bear the costs of intervention nor collect its rents, but their interpretations shape the constraint's evolution. Their exit is analytical — they can change frameworks without material consequence.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible legal framework for the international community to respond to state collapse, atrocity crimes, and governance vacuums without being paralyzed by absolute non-interference norms.
% TRANSFER_FUNCTION: Moves decision-making authority over territory, resources, and populations from weak state governments to external interveners (major powers, international organizations, NGOs). Moves legitimacy and legal cover from the sovereign state to the intervening coalition.
% ABSENT_VOICES: The populations of weak states who would be subject to reclassification have no seat at the tables where intervention thresholds are set (UNSC, NATO, G7, major INGO boards). They are structurally excluded from the authorization process.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty threshold vanished overnight, the legal basis for discretionary intervention would collapse. Major powers would lose the legitimating framework for regime change and resource access operations. Weak states would regain unconditional sovereignty recognition regardless of capacity. The global governance architecture would reorganize around either absolute sovereignty or a new conditional framework.
% FOUNDING_PROBLEM: The post-WWII order needed a mechanism to address state failure and mass atrocity without granting blanket intervention authority. The founding problem was how to legitimize exceptional interference in sovereign affairs when a state manifestly fails its population.
% FOUNDING_PROBLEM_CORROBORATION: Major powers and humanitarian NGOs attest the problem remains live (ongoing atrocities, state collapse). Critical international lawyers and Global South diplomats attest the problem was solved by R2P (2005) and the constraint now persists as a vehicle for neo-colonial extraction. UN General Assembly debates and Non-Aligned Movement statements corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the constraint enables major powers to legally access weak states' territory, resources, and populations on terms they set. Suppression (0.60) reflects the active enforcement required: UNSC resolutions, sanctions, military coalitions, and legal proceedings that suppress the weak state's claim to equal sovereignty. Theater ratio (0.45) is substantial — the humanitarian/development apparatus performs genuine coordination but increasingly serves as the delivery mechanism for extractive intervention. Accessibility collapse (0.40) is moderate: alternative frameworks (absolute sovereignty, R2P) remain discursively available but are politically marginalized. Resistance (0.55) is significant: weak states, Global South coalitions, and critical scholars actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the major power seat, the constraint appears as necessary coordination (rope/tangled_rope) — they built the system, fund it, and use it to solve real problems. From the weak state seat, it is pure extraction (snare) — their sovereignty is conditional on meeting standards they had no role in setting and cannot meet without the extractor's cooperation. The engine's per-seat computation will capture this divergence; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Major power interveners are structural beneficiaries (d ~ 0.15): they gain discretionary authority and resource access with minimal cost. International NGOs are beneficiaries (d ~ 0.25): they gain mandate and funding. Weak states are full targets (d ~ 0.95): they lose sovereignty attributes and bear intervention costs with no exit. Fragile state populations are trapped victims (d ~ 0.90): they experience both the humanitarian benefit and the extractive cost with zero voice. Regional powers sit near symmetric (d ~ 0.55): they sometimes benefit as junior interveners, sometimes pay as targets. The engine computes effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing exceptional intervention for state failure/atrocity) was real but has been substantially addressed by R2P and ICC frameworks. The graduated sovereignty constraint persists because it serves major power interests beyond the founding problem — it provides a gradient of intervention intensity calibrated to strategic value, not just humanitarian need. This is mandatrophy: the mandate (humanitarian intervention) has atrophied into a tool for discretionary extraction. The constraint is not a scaffold (no sunset, no transition plan) and not a piton (active enforcement, concentrated beneficiaries). It is a snare with a coordination cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_sovereignty_naturalness,
    'Is the sovereignty-capacity-legitimacy spectrum a genuine structural feature of international order, or a constructed classification that serves intervener interests?',
    'Compare intervention patterns against the declared metrics: if interventions track the metrics predictably, the spectrum has structural reality; if they track strategic resource/location value regardless of metrics, it is a constructed cover.',
    'If constructed, the constraint is a snare with a false coordination story. If structural, it may be a tangled rope (genuine coordination with asymmetric extraction). The claimed snare classification assumes the latter ambiguity resolves toward construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_sovereignty_naturalness, conceptual, 'Natural-law vs. constructed status of the graduated sovereignty spectrum').

omega_variable(
    intervention_outcome_distribution,
    'Do graduated sovereignty interventions on net improve or worsen outcomes for fragile state populations?',
    'Longitudinal comparative analysis of intervened vs. non-intervened fragile states on human development, governance, and conflict recurrence metrics, controlling for selection effects.',
    'If outcomes worsen, the constraint''s coordination function is falsified and it is pure snare. If outcomes improve for populations but extraction still occurs, it is tangled rope. The current snare claim assumes net harm or extractive dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_outcome_distribution, empirical, 'Whether the constraint''s coordination function delivers net benefit to its ostensible beneficiaries').

omega_variable(
    kernel_reading_relations,
    'What are the structural relationships between the three westphalian_sovereignty readings?',
    'Map the logical space: absolute_sovereignty forecloses both others (categorical bar); conditional_sovereignty and graduated_sovereignty coexist as competing threshold frameworks (both accept intervention legitimacy but differ on trigger structure — binary vs. continuous); graduated_sovereignty influences conditional_sovereignty by pressuring the R2P threshold toward continuous assessment.',
    'The forecloses/coexists_with/influences mapping determines whether the kernel has genuine structural fragmentation or a single dominant reading with fringe alternatives. This story declares: absolute forecloses both; conditional and graduated coexist; graduated influences conditional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between sibling readings of the westphalian_sovereignty kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(west_tr_t1960, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t2000, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(west_tr_t2010, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(west_be_t1960, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(west_be_t2000, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(west_be_t2010, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(west_su_t1960, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(west_su_t2000, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(west_su_t2010, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, humanitarian_intervention_law).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, sanctions_regimes).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, peacekeeping_mandates).

% DUAL FORMULATION NOTE:
% Part of the westphalian_sovereignty constraint family. This reading (graduated) creates a continuous spectrum that the conditional reading (R2P) treats as a binary threshold. The absolute reading rejects the kernel's revisability. The three stories form a linked set: absolute_sovereignty -> conditional_sovereignty <- graduated_sovereignty (both conditional and graduated affect R2P operationalization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
