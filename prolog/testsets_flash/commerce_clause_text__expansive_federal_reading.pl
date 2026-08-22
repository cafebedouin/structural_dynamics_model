% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Clause Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'expansive federal reading' of the
 *   Commerce Clause, which interprets 'interstate commerce' to include all
 *   economic activity with a substantial aggregate effect on national
 *   markets. This reading, largely solidified during the New Deal era,
 *   dramatically expanded federal regulatory power, subordinating state
 *   autonomy in many economic spheres. It is presented as a Tangled Rope
 *   because it genuinely coordinates a national economy while simultaneously
 *   extracting regulatory power from states and local entities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.65).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.75).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Clause Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'fd021b9d-9fed-406a-bf05-90502531c142').
narrative_ontology:cs_kernel_codification('fd021b9d-9fed-406a-bf05-90502531c142', fixed_text).
narrative_ontology:cs_authority_grounding('fd021b9d-9fed-406a-bf05-90502531c142', lineage).
narrative_ontology:cs_interpretation_layer_present('fd021b9d-9fed-406a-bf05-90502531c142').
narrative_ontology:cs_reading_relation('fd021b9d-9fed-406a-bf05-90502531c142', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd021b9d-9fed-406a-bf05-90502531c142', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('fd021b9d-9fed-406a-bf05-90502531c142', foundational, aggregate_effects_test_validity).
narrative_ontology:cs_axiom_status(aggregate_effects_test_validity, holdable).
narrative_ontology:cs_axiom_grounding('fd021b9d-9fed-406a-bf05-90502531c142', aggregate_effects_test_validity, conventional).
narrative_ontology:cs_axiom('fd021b9d-9fed-406a-bf05-90502531c142', foundational, national_economic_unity_priority).
narrative_ontology:cs_axiom_status(national_economic_unity_priority, holdable).
narrative_ontology:cs_axiom_grounding('fd021b9d-9fed-406a-bf05-90502531c142', national_economic_unity_priority, instrumental).
narrative_ontology:cs_reference_frame('fd021b9d-9fed-406a-bf05-90502531c142', new_deal_constitutional_settlement).
narrative_ontology:cs_drift_state('fd021b9d-9fed-406a-bf05-90502531c142', contemporary_federalism_debates, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd021b9d-9fed-406a-bf05-90502531c142', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_variation_advocates).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, implied_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This reading grants the federal government broad authority to regulate economic activity, enabling federal agencies to implement national policies across diverse sectors. They benefit from expanded jurisdiction and a more uniform regulatory environment.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter,
    institutional, generational, constrained, national).

% Groups and individuals who prioritize uniform national standards and policies over state-by-state variation. They benefit from the ability to address complex economic and social issues at a federal level, avoiding a 'race to the bottom' among states.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% The collective interest of individual states in maintaining their sovereign powers and legislative discretion. This reading subordinates state regulatory authority to federal power in many economic spheres, leading to a loss of local control and policy experimentation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_autonomy, payer,
    institutional, generational, constrained, national).

% Citizens and groups who prefer local control and diverse policy approaches tailored to specific community needs. They bear the cost of federal preemption and the imposition of national standards that may not fit local contexts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_variation_advocates, payer,
    moderate, biographical, constrained, local).

% The ultimate arbiter of the Commerce Clause's scope. Its interpretations define the boundaries of federal power. While not directly benefiting from specific policies, its institutional power is enhanced by the ability to shape the federal-state balance.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars and jurists who advocate for a narrow interpretation of the Commerce Clause based on its original public meaning. Their arguments are often marginalized in the face of established expansive precedents, though they continue to influence dissenting opinions and academic discourse.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the federal government to coordinate economic activity across state lines, preventing states from erecting trade barriers or undermining national economic policies, thereby fostering a unified national market.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from individual states to the federal government, along with the associated resources and enforcement capabilities.
% ABSENT_VOICES: Originalist and states' rights advocates, who would argue for a more limited federal role and greater state autonomy, are often outvoted or overruled in legislative and judicial contexts where this expansive reading holds sway.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, federal agencies would lose jurisdiction over vast swathes of economic activity, leading to a regulatory vacuum, increased state-level protectionism, and a fragmented national market. The entire structure of federal economic regulation would collapse, necessitating a complete reorganization of governance.
% FOUNDING_PROBLEM: The Articles of Confederation failed to prevent states from imposing tariffs and trade barriers on each other, leading to economic disunity and instability, which the Commerce Clause was intended to remedy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars widely corroborate the founding problem of interstate trade barriers under the Articles. The federal government and national business interests attest that the problem of economic fragmentation remains live, requiring a strong federal hand. States' rights advocates, however, contest the degree to which the current expansive reading is necessary to solve this problem, arguing it overshoots the original intent.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because states lose significant regulatory power and the ability to tailor policies to local needs, which is a substantial cost. Suppression is also high (0.75) as federal preemption actively suppresses state-level alternatives and resistance. The theater ratio is moderate (0.20): while the federal government genuinely coordinates national markets, a portion of its activity is performative, defending its expansive jurisdiction against states' rights challenges rather than solely addressing market failures. The historical measurements reflect the expansion of federal power from the New Deal (1937) through its peak and subsequent minor retrenchment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal agencies, this reading is a necessary Rope for national economic coordination. From the perspective of states and local advocates, it functions as a Snare, extracting their sovereign power. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal administrative state and national policy advocates are clear beneficiaries, gaining power and policy coherence. State autonomy and local variation advocates are the primary payers, losing power and flexibility. The Supreme Court acts as an agenda-setter, defining the scope of this reading. Originalist scholars are excluded, their arguments for a narrower reading largely unheeded in the prevailing legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_effects_threshold,
    'What constitutes a ''substantial aggregate effect'' on national markets, and is this threshold consistently applied or subject to judicial discretion?',
    'Empirical analysis of Supreme Court jurisprudence over time, identifying consistent criteria for ''substantiality'' versus case-by-case ad hoc determinations.',
    'If the threshold is consistently applied, it reinforces the rule-bound nature of the constraint. If it''s discretionary, it highlights a potential for arbitrary expansion of federal power, increasing extractiveness for states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_effects_threshold, empirical, 'Ambiguity in defining the scope of federal power under the ''substantial aggregate effects'' test.').

omega_variable(
    federal_vs_state_efficiency,
    'Is federal regulation under this expansive reading demonstrably more efficient or effective in solving national economic problems than a more decentralized, state-led approach?',
    'Comparative economic and policy analysis of federal versus state regulatory outcomes in specific sectors, measuring efficiency, innovation, and responsiveness to diverse needs.',
    'Evidence of federal inefficiency or overreach would weaken the coordination justification, pushing the constraint closer to a Snare. Evidence of superior federal outcomes would reinforce its Rope-like qualities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_vs_state_efficiency, empirical, 'Whether the benefits of federal uniformity outweigh the costs of lost state autonomy and local variation.').

omega_variable(
    originalist_challenge_viability,
    'Could a renewed originalist interpretation of the Commerce Clause gain sufficient judicial or political traction to significantly roll back the expansive federal reading?',
    'Analysis of judicial appointments, shifts in legal scholarship, and public opinion trends regarding federalism and constitutional interpretation.',
    'If a narrow originalist reading were to prevail, this constraint would be reclassified as a Piton or even a Snare, as its foundational premise would be undermined, and its persistence would rely on inertia or active suppression of the new interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_challenge_viability, conceptual, 'The potential for an alternative constitutional reading to displace the current expansive interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_text__expansive_federal_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_text__expansive_federal_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(comm_tr_t1990, commerce_clause_text__expansive_federal_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__expansive_federal_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__expansive_federal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.4).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(comm_be_t1990, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(comm_su_t1990, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.1).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, national_labor_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause text, each with different structural implications for federal power and state autonomy. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
