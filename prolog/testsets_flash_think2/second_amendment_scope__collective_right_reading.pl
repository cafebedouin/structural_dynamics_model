% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as Collective Right (State Militia Authority)
 *   domain: constitutional_law/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'collective right' reading of the
 *   Second Amendment, which holds that the amendment protects the authority
 *   of states to maintain well-regulated militias, rather than an individual
 *   right to bear arms unconnected to militia service. This reading
 *   emphasizes state sovereignty and the institutional nature of the right.
 *   The low extractiveness reflects that, from this reading's perspective,
 *   the amendment primarily grants authority to states, which is a
 *   coordination function. The high suppression and resistance reflect the
 *   active denial of individual claims and the ongoing contestation from
 *   individual rights advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.25).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.7).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as Collective Right (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '77a0f728-ac21-4384-a9b4-9a26e9b01061').
narrative_ontology:cs_kernel_codification('77a0f728-ac21-4384-a9b4-9a26e9b01061', fixed_text).
narrative_ontology:cs_authority_grounding('77a0f728-ac21-4384-a9b4-9a26e9b01061', lineage).
narrative_ontology:cs_interpretation_layer_present('77a0f728-ac21-4384-a9b4-9a26e9b01061').
narrative_ontology:cs_reading_relation('77a0f728-ac21-4384-a9b4-9a26e9b01061', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('77a0f728-ac21-4384-a9b4-9a26e9b01061', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('77a0f728-ac21-4384-a9b4-9a26e9b01061', foundational, state_sovereignty_over_arms).
narrative_ontology:cs_axiom_status(state_sovereignty_over_arms, holdable).
narrative_ontology:cs_axiom_grounding('77a0f728-ac21-4384-a9b4-9a26e9b01061', state_sovereignty_over_arms, conventional).
narrative_ontology:cs_axiom('77a0f728-ac21-4384-a9b4-9a26e9b01061', foundational, militia_as_state_instrument).
narrative_ontology:cs_axiom_status(militia_as_state_instrument, holdable).
narrative_ontology:cs_axiom_grounding('77a0f728-ac21-4384-a9b4-9a26e9b01061', militia_as_state_instrument, conventional).
narrative_ontology:cs_reference_frame('77a0f728-ac21-4384-a9b4-9a26e9b01061', founding_era_state_sovereignty).
narrative_ontology:cs_drift_state('77a0f728-ac21-4384-a9b4-9a26e9b01061', contemporary_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('77a0f728-ac21-4384-a9b4-9a26e9b01061', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, states).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militias).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_firearms_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional authority to organize and maintain militias for defense, and to regulate firearms within their borders. They interpret the Second Amendment as a grant of power to them, not to individuals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, states, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct object of the Second Amendment's protection under this reading, ensuring their existence and regulation by the states. They are the instrument through which states exercise their defense authority.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_militias, beneficiary,
    organized, biographical, constrained, local).

% Bear the cost of this interpretation as their claims to individual firearms ownership, unconnected to militia service, are denied or heavily regulated by state authority. Their ability to own firearms is entirely subject to state law.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearms_owners, payer,
    moderate, biographical, constrained, national).

% Interprets the Second Amendment, historically aligning with the collective right view for much of its history, granting states broad regulatory power. Its role is to adjudicate disputes between states and individuals, or between states and federal law.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the historical context, text, and evolving jurisprudence of the Second Amendment, often debating the merits of collective versus individual rights interpretations. They provide the intellectual framework for legal arguments.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% Are actively excluded from the core premise of this reading, as their advocacy for an individual right to bear arms is directly contradicted. They exert political and legal pressure to challenge this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the defense capacity of states by constitutionally affirming their authority to maintain well-regulated militias, ensuring a ready force for security without relying solely on a federal standing army.
% TRANSFER_FUNCTION: Transfers the primary constitutional right concerning firearms from individuals to the states, granting states the authority to organize, arm, and regulate militias, and by extension, to regulate private arms ownership.
% ABSENT_VOICES: Individual rights advocates and individual firearms owners are structurally excluded from the core premise of this reading; they would argue for an individual right to bear arms for self-defense, independent of militia service, but this reading explicitly denies that premise.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, states would lose a clear constitutional basis for their authority over militias and firearms regulation, leading to a significant shift in power dynamics towards individual claims and potentially federal preemption, reorganizing the legal landscape of arms control.
% FOUNDING_PROBLEM: Ensuring the security of the newly formed states by allowing them to maintain a 'well-regulated militia' as a necessary defense against both foreign threats and potential federal overreach, while also addressing concerns about a powerful standing army.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the Constitutional Convention, Federalist Papers, Anti-Federalist writings, and early state militia laws corroborate the intent to empower states for defense. Contemporary legal historians and some constitutional scholars (outside of direct state beneficiaries) also support this historical reading.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because, within this reading's framework, the amendment primarily serves to coordinate state defense capabilities, a beneficial function for the states. Suppression is high (0.70) because this interpretation necessitates active state regulation and denial of individual claims to an unfettered right. Resistance is also high (0.70) due to persistent challenges from individual rights advocates. The theater ratio is low (0.10) as the function of granting state authority is direct and not performative. The metrics are stable over time, reflecting the fixed nature of this constitutional interpretation, even as it faces external challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states, this reading provides a clear framework for defense and public order, functioning as a coordination mechanism. From the perspective of individual firearms owners, it is a highly suppressive and extractive constraint that denies a fundamental personal liberty. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   States and state militias are the primary beneficiaries (d near 0.0) as they gain constitutional authority and protection. Individual firearms owners are the targets (d near 1.0) as their claims to individual rights are denied or heavily constrained. The federal government acts as an agenda-setter, historically upholding this view. Individual rights advocates are excluded, actively resisting the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine reflection of the Second Amendment''s original intent, or one of several plausible readings of a contested kernel?',
    'Further historical and textual analysis, or a definitive Supreme Court ruling that explicitly forecloses alternative readings.',
    'If it is merely one reading among equals, its classification as a ''rope'' for states is contingent on that interpretive choice; if it were definitively established as the sole original intent, its ''mountain'' characteristics (emerges_naturally) might be argued more strongly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the Second Amendment kernel, with sibling readings offering alternative interpretations.').

omega_variable(
    individual_vs_collective_right,
    'Does the Second Amendment protect an individual right to bear arms, or a collective right tied to militia service?',
    'Further Supreme Court jurisprudence, or a constitutional amendment clarifying the scope of the right.',
    'If an individual right is affirmed, this ''collective right'' reading would be reclassified as a snare or piton, as its suppressive function would be seen as illegitimate extraction. If the collective right is reaffirmed, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_collective_right, conceptual, 'The core ambiguity between individual and collective rights in the Second Amendment.').

omega_variable(
    militia_definition_drift,
    'Has the meaning and function of ''militia'' changed so significantly since the founding era that the ''collective right'' reading''s justification has atrophied?',
    'Historical and sociological analysis of militia evolution, and legal rulings on the contemporary relevance of the ''well-regulated militia'' clause.',
    'If the militia''s function is deemed obsolete or fundamentally altered, the ''collective right'' reading might be reclassified as a piton, persisting due to inertia rather than a live coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_drift, empirical, 'The potential for the definition of ''militia'' to have drifted, impacting the constraint''s original justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__collective_right_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(seco_tr_t100, second_amendment_scope__collective_right_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(seco_tr_t150, second_amendment_scope__collective_right_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement(seco_tr_t200, second_amendment_scope__collective_right_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(seco_tr_t235, second_amendment_scope__collective_right_reading, theater_ratio, 235, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__collective_right_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(seco_be_t100, second_amendment_scope__collective_right_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(seco_be_t150, second_amendment_scope__collective_right_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(seco_be_t200, second_amendment_scope__collective_right_reading, base_extractiveness, 200, 0.25).
narrative_ontology:measurement(seco_be_t235, second_amendment_scope__collective_right_reading, base_extractiveness, 235, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__collective_right_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(seco_su_t100, second_amendment_scope__collective_right_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(seco_su_t150, second_amendment_scope__collective_right_reading, suppression_requirement, 150, 0.7).
narrative_ontology:measurement(seco_su_t200, second_amendment_scope__collective_right_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(seco_su_t235, second_amendment_scope__collective_right_reading, suppression_requirement, 235, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, firearms_regulation_authority).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_defense_powers).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_scope' kernel. This 'collective_right_reading' emphasizes state authority over militias, explicitly denying an individual right. It is linked to the 'individual_right_reading' and 'civic_right_reading' which assert different forms of individual rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
