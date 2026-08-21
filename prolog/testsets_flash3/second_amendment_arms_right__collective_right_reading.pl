% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right to State Militia Authority
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as protecting
 *   the authority of state governments to maintain organized militias, rather
 *   than an individual's right to own firearms for private purposes. Under
 *   this reading, individual gun ownership is subject to plenary regulation
 *   by the states. This is one reading of the 'second_amendment_arms_right'
 *   kernel, distinct from the 'individual_right_reading' and
 *   'civic_republican_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right to State Militia Authority").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '680339da-d8c2-4a57-88ff-1fc7958aa783').
narrative_ontology:cs_kernel_codification('680339da-d8c2-4a57-88ff-1fc7958aa783', fixed_text).
narrative_ontology:cs_authority_grounding('680339da-d8c2-4a57-88ff-1fc7958aa783', lineage).
narrative_ontology:cs_interpretation_layer_present('680339da-d8c2-4a57-88ff-1fc7958aa783').
narrative_ontology:cs_reading_relation('680339da-d8c2-4a57-88ff-1fc7958aa783', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('680339da-d8c2-4a57-88ff-1fc7958aa783', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('680339da-d8c2-4a57-88ff-1fc7958aa783', foundational, state_sovereignty_over_arms).
narrative_ontology:cs_axiom_status(state_sovereignty_over_arms, holdable).
narrative_ontology:cs_axiom_grounding('680339da-d8c2-4a57-88ff-1fc7958aa783', state_sovereignty_over_arms, conventional).
narrative_ontology:cs_axiom('680339da-d8c2-4a57-88ff-1fc7958aa783', foundational, individual_arms_bearing_subordinate_to_militia).
narrative_ontology:cs_axiom_status(individual_arms_bearing_subordinate_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('680339da-d8c2-4a57-88ff-1fc7958aa783', individual_arms_bearing_subordinate_to_militia, conventional).
narrative_ontology:cs_reference_frame('680339da-d8c2-4a57-88ff-1fc7958aa783', founding_era_state_militia_model).
narrative_ontology:cs_drift_state('680339da-d8c2-4a57-88ff-1fc7958aa783', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('680339da-d8c2-4a57-88ff-1fc7958aa783', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militias).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, gun_control_advocates).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the authority to organize and arm militias for state defense, free from federal interference in this domain. This reading grants them plenary power to regulate private arms ownership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefit from the constitutional recognition of their role in state defense and the state's authority to arm and regulate them. Their existence is central to this reading's interpretation of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Bear the costs of extensive state regulation or prohibition of private arms ownership, as this reading does not recognize an individual right to bear arms for private purposes. Their ability to own firearms is entirely subject to state legislative discretion.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia, payer,
    powerless, immediate, constrained, local).

% Its power to regulate arms is limited to ensuring the states' ability to maintain militias, but it is not seen as protecting individual rights. This reading constrains federal overreach into state militia affairs.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefit from this reading as it provides a strong constitutional basis for comprehensive state-level gun control measures, unencumbered by individual rights claims.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_control_advocates, beneficiary,
    organized, biographical, mobile, national).

% Are structurally excluded from the constitutional protection they seek, as this reading denies an individual right to bear arms. Their arguments are rendered constitutionally irrelevant under this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the federal government and state governments regarding military power, ensuring states retain the authority to maintain organized militias for their security.
% TRANSFER_FUNCTION: Transfers the authority to regulate private arms ownership from individuals to state governments, and limits federal power over state militias.
% ABSENT_VOICES: Individual gun owners and gun rights advocates are effectively silenced in this constitutional interpretation, as their claims to an individual right are dismissed. They would argue for a broader individual liberty.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional landscape regarding gun rights would fundamentally shift, likely empowering individual rights claims and severely limiting state regulatory authority over private arms ownership, leading to a significant reorganization of legal and political structures.
% FOUNDING_PROBLEM: The problem of ensuring states could maintain a military force (militias) for their defense against both foreign threats and potential federal overreach, while also addressing concerns about a standing federal army.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside of direct state government or militia advocacy corroborate that the founding era debates indeed focused on state military capacity and federalism, supporting the historical context for this reading's emphasis on collective rights.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily coordinates federal-state relations and empowers states, rather than extracting from them. Suppression is moderate (0.25) as it requires active enforcement by states to regulate private ownership, but this is seen as a legitimate exercise of state power. Theater ratio is low (0.05) as the function of empowering state militias is genuine within this framework. Accessibility collapse is high (0.7) for individual rights claims, as this reading largely forecloses them. Resistance is high (0.8) from those who advocate for individual gun rights, reflecting the ongoing legal and political contestation against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this is a clear Rope, providing necessary coordination and authority. From the perspective of individual gun owners, it is a Snare, as it enables significant extraction (through regulation/prohibition) and suppression of their perceived rights. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are direct beneficiaries, gaining constitutional authority. Individual gun owners outside the militia context are targets, as their rights are curtailed. The federal government acts as an agenda-setter, defining the scope of its own non-interference. Gun control advocates are beneficiaries, as this reading supports their policy goals. Gun rights advocates are excluded, as their core claims are denied.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_militia_definition,
    'What constitutes an ''organized militia'' in contemporary society, and does it include all able-bodied citizens or only formally enrolled state forces?',
    'Legislative clarification at the state level, or Supreme Court ruling defining ''militia'' for Second Amendment purposes.',
    'A narrow definition would further empower state regulation over private ownership; a broad definition might inadvertently create a basis for individual rights claims within a collective framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_militia_definition, conceptual, 'Ambiguity in the definition of ''militia'' and its implications for arms regulation.').

omega_variable(
    collective_vs_individual_right_framing,
    'Is the Second Amendment fundamentally about collective state power or individual liberty, and can these two interpretations be reconciled within a single constitutional framework?',
    'A definitive Supreme Court ruling that explicitly forecloses one interpretation, or a constitutional amendment clarifying the intent.',
    'If the individual right reading gains ascendancy, this collective right reading would be largely superseded, leading to a reclassification of the constraint''s impact on individual citizens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_vs_individual_right_framing, conceptual, 'The core conceptual ambiguity between collective and individual rights interpretations of the Second Amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t5, second_amendment_arms_right__collective_right_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__collective_right_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(seco_tr_t15, second_amendment_arms_right__collective_right_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__collective_right_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t5, second_amendment_arms_right__collective_right_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__collective_right_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(seco_be_t15, second_amendment_arms_right__collective_right_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__collective_right_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t5, second_amendment_arms_right__collective_right_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__collective_right_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(seco_su_t15, second_amendment_arms_right__collective_right_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__collective_right_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, state_police_powers).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, federal_gun_control_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_arms_right' kernel. It emphasizes state militia authority, contrasting with individual and civic republican interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
