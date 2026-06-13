% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Reading of Blood-Feud Obligations
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'Christianized Pacification' reading of
 *   blood-feud obligations, prevalent in medieval Europe. It frames feuds as
 *   violations of divine law, with legitimate violence authority residing
 *   solely with God and delegated ecclesiastical/royal institutions. This
 *   reading seeks to suppress feuds entirely, reclassifying all participants
 *   as victims (due to spiritual peril and temporal punishment) and
 *   positioning the Church and Crown as beneficiaries (gaining interpretive
 *   monopoly on violence and expanded jurisdiction). The constraint is
 *   claimed as a Snare due to its high extraction from feud participants and
 *   aggressive suppression of traditional alternatives, despite its stated
 *   goal of peace.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.8).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.9).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Reading of Blood-Feud Obligations").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f').
narrative_ontology:cs_kernel_codification('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', formalized).
narrative_ontology:cs_authority_grounding('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', lineage).
narrative_ontology:cs_interpretation_layer_present('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f').
narrative_ontology:cs_reading_relation('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', foundational, vengeance_is_sinful).
narrative_ontology:cs_axiom_status(vengeance_is_sinful, holdable).
narrative_ontology:cs_axiom_grounding('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', vengeance_is_sinful, theological).
narrative_ontology:cs_axiom('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', foundational, legitimate_violence_resides_with_god_and_delegated_institutions).
narrative_ontology:cs_axiom_status(legitimate_violence_resides_with_god_and_delegated_institutions, holdable).
narrative_ontology:cs_axiom_grounding('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', legitimate_violence_resides_with_god_and_delegated_institutions, deontological).
narrative_ontology:cs_reference_frame('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', divinely_ordained_peace_and_order).
narrative_ontology:cs_drift_state('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', late_medieval_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ffd6d09-61c3-400c-8bb9-83ce92ab7a6f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_communities).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_law_supremacy).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_jurisdiction).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, royal_peace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively preach against feuds as sin, impose penitential discipline, and offer arbitration. They gain moral authority, expanded jurisdictional reach, and material donations for their role in pacification. They frame their intervention as upholding divine law and bringing peace.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, analytical, regional).

% Supports the Church's stance, issuing edicts against feuds and attempting to establish royal courts as the sole legitimate arbiters of violence. Benefits from reduced internal conflict, increased tax revenue, and consolidation of state power. Faces resistance from local customs and powerful nobles.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Are caught between traditional obligations of honor and vengeance, and the spiritual and temporal penalties imposed by Church and Crown. They face excommunication, fines, and physical punishment for engaging in feuds, yet social identity and family honor often compel participation. They are victims of both the feud cycle and the pacification efforts.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).

% Suffer the direct violence and instability of feuds, including property destruction, loss of life, and disruption of economic activity. They are also subject to the enforcement mechanisms of the Church and Crown, which may involve collective punishment or forced arbitration. They desire peace but lack the power to enforce it themselves.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_communities, payer,
    powerless, immediate, trapped, local).

% Historically arbitrated feuds and maintained local order through customary law. Their authority is undermined by the Church and Crown's claims of exclusive legitimate violence, leading to a loss of status and power. They would argue for the legitimacy of customary justice.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, traditional_chieftains, excluded,
    powerful, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate all legitimate violence under centralized ecclesiastical and royal authority, replacing decentralized customary justice with a unified system based on divine law and state power.
% TRANSFER_FUNCTION: Transfers the right to initiate and execute violence from individuals and kin groups to the Church and Crown. It also transfers spiritual and temporal penalties (excommunication, fines, imprisonment) from feud participants to these institutions.
% ABSENT_VOICES: Traditional chieftains and kin-group elders, who historically managed feuds through customary law and arbitration, are excluded. They would argue for the legitimacy and efficacy of their traditional systems of justice and honor.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification efforts vanished, the vacuum of legitimate violence authority would likely lead to a resurgence of customary feuding, as local communities would revert to self-help mechanisms for justice and deterrence in the absence of centralized enforcement.
% FOUNDING_PROBLEM: The problem of pervasive, destabilizing blood-feuds that undermined social order, economic activity, and the authority of emerging ecclesiastical and royal institutions.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chronicles and royal charters consistently attest to the problem of feuds and the need for pacification. Modern historians and legal anthropologists corroborate that feuds were a significant societal challenge, and the Church and Crown actively sought to suppress them to consolidate their power and establish a new legal order.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because it demands a complete surrender of customary rights to vengeance and self-help, imposing severe spiritual and temporal penalties. Suppression is very high (0.9) due to the combined coercive power of excommunication, interdict, royal justice, and military force. Theater ratio is low (0.2) because the pacification efforts were genuinely aimed at suppressing feuds and consolidating power, not merely performing a function. Accessibility collapse is high (0.7) as the Church and Crown actively worked to delegitimize and eliminate alternative forms of justice. Resistance is also high (0.85) as customary practices and kin-group honor proved deeply entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical and royal institutions, this constraint is a necessary Rope or Scaffold for establishing peace and order. From the perspective of feud participants and local communities, it is a Snare that extracts their traditional rights and imposes new forms of control and punishment.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and royal authority are clear beneficiaries (d=0.0-0.1) as they gain power, legitimacy, and resources from suppressing feuds. Feud participants and local communities are targets (d=0.9-1.0) as they bear the costs of spiritual condemnation, legal penalties, and the loss of customary justice. Traditional chieftains are excluded, their authority actively undermined.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the pacification efforts as pure coordination (Rope) or temporary support (Scaffold). While peace was a stated goal, the structural outcome was a significant transfer of power and extraction of customary rights, sustained by active suppression. The 'mandate' of peace served to justify the expansion of institutional authority, making it a Snare rather than a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_law_vs_customary_law_ambiguity,
    'Is the prohibition of vengeance a universal divine law, or a specific interpretation of religious texts used to justify the expansion of ecclesiastical and royal power?',
    'Comparative analysis of other religious traditions and legal systems regarding vengeance, and historical analysis of the political context in which this interpretation gained dominance.',
    'If a universal divine law, the constraint''s ''mountain-like'' justification is strengthened. If a politically motivated interpretation, it reinforces the ''snare'' classification by revealing a constructed basis for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_vs_customary_law_ambiguity, conceptual, 'Ambiguity between natural/divine law and institutional interpretation.').

omega_variable(
    pacification_vs_power_consolidation,
    'To what extent were pacification efforts genuinely aimed at reducing violence, versus primarily serving to consolidate the power and jurisdiction of the Church and Crown?',
    'Historical analysis of resource allocation (e.g., investment in peace vs. enforcement infrastructure), and outcomes for different social classes (e.g., did peace benefit all equally, or primarily the ruling elites?).',
    'If primarily power consolidation, the extractiveness and suppression metrics are more accurately attributed to rent-seeking. If genuinely pacification, the coordination function is stronger, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pacification_vs_power_consolidation, empirical, 'Distinguishing genuine pacification from power consolidation.').

omega_variable(
    reading_location_of_disagreement,
    'This constraint is the ''christianized_pacification_reading'' of the ''feud_obligation_kernel''. Where is the core disagreement with sibling readings located?',
    'Analyze the axioms and reference frames of the ''stateless_coordination_reading'' and ''extraction_cycle_reading'' to pinpoint the specific structural elements that differ.',
    'The ''stateless_coordination_reading'' would emphasize the coordination function of feuds, challenging the ''snare'' classification. The ''extraction_cycle_reading'' would agree on high extraction but attribute it to the feud itself, not the pacification efforts, potentially shifting the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_location_of_disagreement, conceptual, 'Core disagreement with sibling readings of the feud_obligation_kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 1000, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.28).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1300, 0.22).
narrative_ontology:measurement(feud_tr_t1400, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1400, 0.21).
narrative_ontology:measurement(feud_tr_t1500, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1500, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.68).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.75).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1300, 0.78).
narrative_ontology:measurement(feud_be_t1400, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1400, 0.79).
narrative_ontology:measurement(feud_be_t1500, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1500, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.78).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.85).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1300, 0.88).
narrative_ontology:measurement(feud_su_t1400, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1400, 0.89).
narrative_ontology:measurement(feud_su_t1500, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1500, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, royal_justice_system_establishment).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feud_obligation_kernel'. It is structurally distinct from the 'stateless_coordination_reading' (which views feuds as a coordination mechanism) and the 'extraction_cycle_reading' (which views feuds as an inherent extraction cycle). Each reading has a different ε and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
