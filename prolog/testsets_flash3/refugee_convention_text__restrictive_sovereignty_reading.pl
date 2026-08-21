% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a restrictive interpretation of the 1951
 *   Refugee Convention, emphasizing state sovereignty and limiting the scope
 *   of protection. It requires individualized proof of persecution for
 *   'well-founded fear' and narrowly defines 'particular social group' to
 *   immutable characteristics with state awareness. This reading permits
 *   practices like offshore processing and excludes those fleeing generalized
 *   violence or persecution by non-state actors. It is one reading of the
 *   broader 'refugee_convention_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.65).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '07c93fe5-7724-4de0-827c-704ed2b8d27b').
narrative_ontology:cs_kernel_codification('07c93fe5-7724-4de0-827c-704ed2b8d27b', fixed_text).
narrative_ontology:cs_authority_grounding('07c93fe5-7724-4de0-827c-704ed2b8d27b', lineage).
narrative_ontology:cs_interpretation_layer_present('07c93fe5-7724-4de0-827c-704ed2b8d27b').
narrative_ontology:cs_reading_relation('07c93fe5-7724-4de0-827c-704ed2b8d27b', refugee_convention_text__expansive_humanitarian_reading, influences).
narrative_ontology:cs_reading_relation('07c93fe5-7724-4de0-827c-704ed2b8d27b', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('07c93fe5-7724-4de0-827c-704ed2b8d27b', foundational, state_sovereignty_primacy_in_migration).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_migration, holdable).
narrative_ontology:cs_axiom_grounding('07c93fe5-7724-4de0-827c-704ed2b8d27b', state_sovereignty_primacy_in_migration, conventional).
narrative_ontology:cs_axiom('07c93fe5-7724-4de0-827c-704ed2b8d27b', foundational, individualized_persecution_as_threshold).
narrative_ontology:cs_axiom_status(individualized_persecution_as_threshold, holdable).
narrative_ontology:cs_axiom_grounding('07c93fe5-7724-4de0-827c-704ed2b8d27b', individualized_persecution_as_threshold, conventional).
narrative_ontology:cs_reference_frame('07c93fe5-7724-4de0-827c-704ed2b8d27b', westphalian_state_control_framework).
narrative_ontology:cs_drift_state('07c93fe5-7724-4de0-827c-704ed2b8d27b', contemporary_global_migration_crises, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('07c93fe5-7724-4de0-827c-704ed2b8d27b', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_state_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_immutable_psg).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Refugee Convention to maximize their discretion over who is admitted and protected, viewing it as a minimum floor rather than an expansive mandate. They benefit from reduced asylum caseloads and control over national borders.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, constrained, national).

% Implement the restrictive interpretation, benefiting from clearer, narrower criteria for admissibility and the ability to use offshore processing and other deterrent measures without perceived violation of international law. Their mandate is to control borders.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies, beneficiary,
    organized, biographical, constrained, national).

% Are denied protection because their fear of persecution stems from generalized violence or civil conflict, not individualized targeting, which this reading excludes from 'well-founded fear'. They face refoulement to dangerous situations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Are denied protection because their persecution comes from non-state actors, and this reading requires state complicity or inability to protect. They are left vulnerable to powerful non-state groups.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_state_persecution, payer,
    powerless, immediate, trapped, global).

% Are denied protection because their 'particular social group' (e.g., gender identity, sexual orientation, clan affiliation) is not recognized as 'immutable' or lacks clear state awareness of persecution, as required by this reading. They face discrimination and violence without recourse.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_immutable_psg, payer,
    powerless, immediate, trapped, global).

% Monitor and challenge the restrictive interpretation, arguing it undermines the humanitarian spirit and intent of the Convention. They provide legal aid and advocacy for asylum seekers excluded by this reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to coordinate their responses to forced migration, ensuring a minimum standard of protection while allowing for national sovereignty in implementation.
% TRANSFER_FUNCTION: Transfers the burden of protection from individuals fleeing persecution to sovereign states, but this reading limits the scope of that transfer, effectively transferring the burden back to excluded asylum seekers.
% ABSENT_VOICES: Asylum seekers themselves, particularly those falling outside the narrow definitions of 'well-founded fear' or 'particular social group', are largely absent from the interpretive process, their experiences discounted by the restrictive framework.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, states would face immediate pressure to adopt more expansive interpretations, leading to increased asylum claims, altered border policies, and a significant shift in the global governance of migration. The current system relies on this reading to manage caseloads and maintain sovereign control.
% FOUNDING_PROBLEM: The problem of mass displacement and persecution in the aftermath of World War II, requiring an international legal framework to ensure protection for those fleeing persecution.
% FOUNDING_PROBLEM_CORROBORATION: The problem of forced displacement and persecution remains acutely live, attested by UNHCR, human rights organizations, and ongoing global crises. However, the scope of the Convention's application to this problem is highly contested by various state and non-state actors.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading systematically denies protection to large classes of vulnerable individuals who would be covered by more expansive interpretations. Suppression (0.78) is also high, as states actively enforce these narrow criteria through legal and administrative means, often with significant coercive measures at borders. The theater ratio (0.20) is moderate, reflecting that while states genuinely engage in some protection, a portion of the administrative effort is directed at maintaining the restrictive interpretation rather than facilitating broad protection. The claimed type is 'tangled_rope' because it still provides a coordination function for states (managing migration flows) but does so with significant asymmetric extraction from asylum seekers.
 *
 * PERSPECTIVAL GAP:
 *   Sovereign states and border control agencies experience this as a necessary framework for managing national security and resources, a 'rope' that coordinates international obligations with national interests. Asylum seekers, however, experience it as a 'snare' that denies them fundamental protection based on narrow, often arbitrary, interpretations of their plight. Human rights advocates view it as a 'tangled_rope' where the coordination function is deeply compromised by extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and border control agencies are clear beneficiaries, as the reading grants them maximum discretion and reduces their obligations (low d). Asylum seekers, particularly those excluded by the narrow definitions, are the primary targets, bearing the full cost of denied protection (high d). Human rights advocates, while observers, are structurally aligned with the victims, pushing against the constraint's restrictive force.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (as states might claim) by highlighting the significant extraction and suppression inherent in this restrictive reading. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine, albeit limited, coordination function it provides for states in managing migration. The 'tangled_rope' classification captures the hybrid nature where a coordination mechanism is actively used to extract from a vulnerable population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_humanitarian_mandate,
    'Is the Refugee Convention primarily a tool for states to manage sovereignty, or a humanitarian mandate for individual protection?',
    'International legal consensus shift, or a new UN General Assembly resolution clarifying the Convention''s primary intent.',
    'If primarily humanitarian, this restrictive reading would be reclassified as a ''snare'' due to its high extraction from vulnerable individuals. If primarily a sovereignty tool, the ''tangled_rope'' classification would be reinforced, but with a lower expectation of protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_humanitarian_mandate, conceptual, 'Ambiguity in the foundational purpose of the Refugee Convention.').

omega_variable(
    individualized_persecution_feasibility,
    'Is it empirically feasible for individuals fleeing generalized violence or non-state persecution to provide ''individualized proof'' of persecution, as required by this reading?',
    'Empirical studies on the evidentiary capacity of asylum seekers from conflict zones, or legal reforms acknowledging the challenges of proof in such contexts.',
    'If empirically infeasible, the requirement for individualized proof becomes a de facto barrier to protection, increasing the effective extraction and potentially reclassifying the constraint towards a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualized_persecution_feasibility, empirical, 'The practical burden of proof under restrictive interpretations.').

omega_variable(
    kernel_reading_impact_on_siblings,
    'How does this restrictive sovereignty reading structurally influence or foreclose other readings of the Refugee Convention kernel?',
    'Analysis of judicial decisions, state practice, and academic discourse to map the causal and logical dependencies between readings.',
    'This reading''s entrenchment makes it harder for expansive humanitarian or procedural integrity readings to gain traction, by setting precedents and shaping institutional norms that favor state discretion. It influences the resource allocation and legitimacy conditions for alternative interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_impact_on_siblings, conceptual, 'The inter-reading dynamics within the Refugee Convention kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.4).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'refugee_convention_text' kernel, each with different ε values and structural properties. This reading emphasizes state sovereignty and restrictive interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
