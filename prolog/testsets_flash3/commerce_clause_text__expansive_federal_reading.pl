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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Reading of the Commerce Clause
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'expansive federal reading' of the U.S.
 *   Constitution's Commerce Clause, which interprets federal power to
 *   regulate interstate commerce as extending to all economic activity with a
 *   substantial aggregate effect on national markets. This reading,
 *   solidified in the mid-20th century, has significantly expanded federal
 *   authority at the expense of state autonomy. The claimed type is
 *   'tangled_rope' because it provides a genuine coordination function
 *   (national economic coherence) but also involves substantial, asymmetric
 *   extraction of power from states and local entities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.65).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.7).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Reading of the Commerce Clause").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58').
narrative_ontology:cs_kernel_codification('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', fixed_text).
narrative_ontology:cs_authority_grounding('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', lineage).
narrative_ontology:cs_interpretation_layer_present('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58').
narrative_ontology:cs_reading_relation('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', foundational, national_economic_unity_paramount).
narrative_ontology:cs_axiom_status(national_economic_unity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', national_economic_unity_paramount, instrumental).
narrative_ontology:cs_axiom('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', foundational, congressional_power_adapts_to_modern_economy).
narrative_ontology:cs_axiom_status(congressional_power_adapts_to_modern_economy, holdable).
narrative_ontology:cs_axiom_grounding('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', congressional_power_adapts_to_modern_economy, conventional).
narrative_ontology:cs_reference_frame('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', new_deal_constitutional_revolution).
narrative_ontology:cs_drift_state('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fcb527f0-a3ed-4a2f-ad6f-d20ac2e5cb58', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_variation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from expanded jurisdiction, allowing it to regulate a vast array of economic activities previously reserved to states. It actively defends this expansive interpretation in litigation and through new legislation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for uniform national standards and policies, seeing the expansive Commerce Clause as essential for addressing complex, interconnected problems like environmental protection, civil rights, and economic stability. They benefit from the ability to implement these policies federally.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Bears the cost of federal preemption and reduced legislative scope. States lose the ability to tailor economic regulations to local conditions and preferences, leading to a more homogenized national regulatory landscape. Exit is primarily through litigation or constitutional amendment efforts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_autonomy, payer,
    institutional, generational, constrained, national).

% Local businesses and communities that prefer or benefit from distinct local economic regulations find their options constrained by federal mandates. They are often too diffuse and unorganized to effectively resist federal preemption.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_variation, payer,
    powerless, biographical, trapped, local).

% Argue for a narrower interpretation of the Commerce Clause based on the original public meaning of the text, but their views are often marginalized in contemporary federal jurisprudence that favors the expansive reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_legal_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the federal government to coordinate economic activity across state lines, preventing states from erecting trade barriers or creating regulatory 'races to the bottom' that could harm the national economy.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from individual states to the federal government, centralizing control over economic matters with substantial aggregate effects.
% ABSENT_VOICES: Advocates for a more robust vision of state sovereignty and local control are often sidelined in federal policy debates, as the expansive reading provides a strong presumption in favor of federal action. Originalist legal scholars, who would argue for a narrower interpretation, are also largely excluded from the dominant judicial discourse.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, federal agencies would lose jurisdiction over vast areas of economic regulation, leading to a rapid decentralization of power to the states. This would create a patchwork of state laws, potentially disrupting national markets and requiring a fundamental re-evaluation of federal-state relations.
% FOUNDING_PROBLEM: The Articles of Confederation failed to provide a strong central government capable of regulating interstate commerce, leading to economic disputes and protectionist policies among states that hindered national prosperity.
% FOUNDING_PROBLEM_CORROBORATION: The federal administrative state and national policy advocates attest that the problem of interstate economic fragmentation and the need for national coordination remain live. State governments and some legal scholars, while acknowledging the historical problem, argue that the current expansive reading overshoots the original intent and creates new problems of federal overreach.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because states and local entities lose significant regulatory power and the ability to tailor policies, while the federal government gains broad jurisdiction. Suppression is also high (0.70) as federal preemption actively suppresses state-level alternatives, and resistance from states, while present, has largely been unsuccessful in reversing the trend. The theater ratio is low (0.10) because the federal government actively exercises its expanded powers, and the coordination function, while contested, is genuinely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal administrative state, this reading is a necessary 'rope' for national coordination and problem-solving. From the perspective of state autonomy advocates, it is a 'snare' that extracts sovereign power. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal administrative state and national policy advocates are clear beneficiaries, gaining power and the ability to implement uniform policies. State autonomy and local economic variation are the primary victims, losing power and flexibility. Originalist legal scholars are excluded, as their preferred interpretation is not the one currently dominant.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_effects_threshold,
    'What constitutes a ''substantial aggregate effect'' on national markets, and is this threshold consistently applied or subject to political discretion?',
    'Empirical analysis of judicial decisions and legislative history to identify consistent criteria for ''substantiality,'' or a formal redefinition by Congress or the Supreme Court.',
    'A clearer, more consistently applied threshold would reduce the perceived arbitrariness of federal intervention, potentially lowering extractiveness from states. If it remains discretionary, it reinforces the federal government''s agenda-setting power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_effects_threshold, conceptual, 'Ambiguity in the ''substantial aggregate effects'' test.').

omega_variable(
    federal_vs_state_efficiency,
    'Is federal regulation of all ''substantial aggregate effects'' genuinely more efficient or effective than a more decentralized, state-led approach, considering local variation and innovation?',
    'Comparative empirical studies of policy outcomes in areas where federal preemption has occurred versus areas with greater state autonomy, or a ''laboratories of democracy'' approach allowing states to experiment.',
    'If federal regulation is demonstrably less efficient or effective in certain areas, it would weaken the coordination justification for the expansive reading, potentially reclassifying it closer to a pure snare. If more efficient, it strengthens the rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_vs_state_efficiency, empirical, 'Efficiency trade-off between federal centralization and state autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.4).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, national_labor_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
