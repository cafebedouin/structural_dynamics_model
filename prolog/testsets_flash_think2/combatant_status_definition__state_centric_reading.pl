% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (Geneva Article 4 Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint defines combatant status strictly according to Article 4
 *   of the Third Geneva Convention, requiring formal state military
 *   organization and adherence to specific criteria. It categorically
 *   excludes non-state armed groups from Prisoner of War (POW) protections,
 *   subjecting them to prosecution under domestic law. This reading is a
 *   foundational element of the state-centric approach to International
 *   Humanitarian Law, prioritizing state sovereignty and the traditional
 *   interstate model of warfare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.85).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.9).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (Geneva Article 4 Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'e9f612a4-f5e2-4c29-b900-7faf5b5b5703').
narrative_ontology:cs_kernel_codification('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', fixed_text).
narrative_ontology:cs_authority_grounding('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', lineage).
narrative_ontology:cs_interpretation_layer_present('e9f612a4-f5e2-4c29-b900-7faf5b5b5703').
narrative_ontology:cs_reading_relation('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', foundational, state_sovereignty_as_basis_of_law).
narrative_ontology:cs_axiom_status(state_sovereignty_as_basis_of_law, holdable).
narrative_ontology:cs_axiom_grounding('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', state_sovereignty_as_basis_of_law, conventional).
narrative_ontology:cs_axiom('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', foundational, reciprocity_among_states_only).
narrative_ontology:cs_axiom_status(reciprocity_among_states_only, holdable).
narrative_ontology:cs_axiom_grounding('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', reciprocity_among_states_only, conventional).
narrative_ontology:cs_reference_frame('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', westphalian_state_system_framework).
narrative_ontology:cs_drift_state('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9f612a4-f5e2-4c29-b900-7faf5b5b5703', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, states_parties_to_geneva_conventions).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, individual_non_state_fighters).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_sovereignty_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, monopoly_on_legitimate_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As signatories to the Geneva Conventions, these states define and enforce combatant status, benefiting from the reciprocal recognition of their own soldiers' POW protections while denying such status to non-state actors. They actively prosecute non-state fighters under domestic law.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, states_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Their personnel are guaranteed POW status and protections if captured, provided they adhere to the laws of armed conflict. This provides a crucial legal shield for their combatants, distinguishing them from criminals.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, biographical, constrained, global).

% Categorically denied POW status and protections under this reading, their captured members are subject to prosecution as criminals under domestic law, facing severe penalties. They have no legal recourse within this framework to claim combatant immunity.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, regional).

% As individuals, they bear the direct consequences of being denied combatant status, facing criminal charges, harsh detention conditions, and often the death penalty, without the legal safeguards afforded to state soldiers.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, individual_non_state_fighters, payer,
    powerless, immediate, trapped, local).

% These bodies, while operating under international law, often interpret combatant status in line with state-centric definitions, contributing to the enforcement of the distinction between state and non-state actors in terms of legal protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_criminal_courts, agenda_setter,
    institutional, generational, constrained, global).

% They argue for broader protections for all individuals in armed conflict, regardless of formal status, but their arguments are often marginalized or rejected by states adhering to the strict state-centric interpretation of combatant status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, reciprocal framework for the treatment of captured combatants between states, ensuring a baseline of humane treatment and immunity from prosecution for state soldiers.
% TRANSFER_FUNCTION: Transfers the burden of legal vulnerability and criminal prosecution from state combatants to non-state combatants, effectively granting legal immunity to the former while denying it to the latter.
% ABSENT_VOICES: Non-state armed groups and their advocates are largely excluded from the formal legal processes that define and interpret combatant status, despite being the primary targets of its most extractive applications. Their perspectives on the legitimacy of their struggle and their claims to protection are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If this state-centric definition vanished overnight, the legal landscape of armed conflict would fundamentally shift. State militaries would lose their primary legal shield, leading to widespread prosecution of captured soldiers. Non-state actors would gain a stronger claim to protections, potentially altering the dynamics of asymmetric warfare and state responses to insurgency. The entire international legal framework for conflict would require renegotiation.
% FOUNDING_PROBLEM: To regulate warfare between sovereign states, minimize suffering, and ensure humane treatment for captured state soldiers through a system of reciprocal recognition and protection.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, historical records of the Geneva Conventions' drafting, and military manuals corroborate the original intent to regulate interstate conflict. However, contemporary analyses from human rights organizations and critical legal scholars highlight the definition's current function in maintaining state power over non-state actors, often without independent corroboration from the benefiting states themselves.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this definition denies fundamental protections (POW status) to a significant class of actors in contemporary conflicts, leading to severe legal and physical consequences. Suppression is also very high (0.90) as states actively enforce this exclusion through legal frameworks, military doctrine, and judicial processes, with virtually no legal exit for non-state fighters. Theater ratio is low (0.10) because the definition is actively and consistently applied; its function is not performative but directly consequential. Accessibility collapse is high (0.75) as it severely limits legal alternatives for non-state actors, forcing them into a 'criminal' category. Resistance is high (0.70) from human rights groups and non-state actors themselves, who challenge its fairness and applicability to modern conflicts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states, this definition is a necessary framework for order and reciprocity in warfare. From the perspective of non-state actors and human rights advocates, it is an extractive tool that denies fundamental protections based on political status rather than conduct, creating a two-tiered system of justice in armed conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   States parties and their militaries are clear beneficiaries, gaining legal immunity for their combatants and maintaining a legal advantage over non-state adversaries. Non-state armed groups and individual fighters are the primary targets/victims, facing criminal prosecution and denial of POW status. International criminal courts act as agenda-setters in applying this framework. Human rights advocates are excluded from the definitional process but observe and critique its effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_extraction,
    'Is the strict state-centric definition of combatant status a necessary legal distinction for maintaining order and reciprocity in armed conflict, or does it primarily function as an extractive mechanism to maintain state power over non-state actors?',
    'Empirical analysis of conflict outcomes in contexts where alternative definitions (e.g., AP I Article 1(4) or functional approaches) have been applied, assessing impacts on civilian protection, combatant treatment, and conflict resolution.',
    'If primarily extractive, the constraint''s effective extractiveness is higher than its stated coordination function implies, warranting reclassification towards a Snare. If necessary for order, its coordination function is more robust, supporting a Tangled Rope or even Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_extraction, conceptual, 'Ambiguity between legal necessity and power-maintaining extraction.').

omega_variable(
    suppression_legitimacy,
    'Is the high level of suppression (denial of POW status, criminal prosecution) a legitimate consequence of non-state actors'' failure to meet established legal criteria, or is it an unjust application of law designed to deter and punish political opposition?',
    'Analysis of international legal precedent, state practice, and the evolving nature of armed conflict, particularly regarding the ''organized armed group'' concept and the principle of distinction.',
    'If deemed illegitimate, the suppression contributes more heavily to the constraint''s extractive nature, pushing its classification towards a Snare. If legitimate, it reinforces the state-centric framework''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_legitimacy, conceptual, 'Legitimacy of suppression for non-state actors.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this ''state_centric_reading'' the only defensible interpretation of the ''combatant_status_definition'' kernel, or are the ''national_liberation_reading'' and ''functional_protection_reading'' equally coherent and legitimate?',
    'Ongoing international legal discourse, state practice evolution, and rulings by international tribunals. The persistence and influence of sibling readings indicate the kernel''s inherent contestability.',
    'If sibling readings gain wider acceptance, this constraint''s ''state_centric_reading'' would be re-evaluated as one among several, potentially reducing its perceived legitimacy and increasing its effective extractiveness for those it targets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of a contested kernel; its status depends on the contestability of its core premises against sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1970, combatant_status_definition__state_centric_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__state_centric_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.09).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__state_centric_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.6).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__state_centric_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__state_centric_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.8).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__state_centric_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__state_centric_reading, suppression_requirement, 1970, 0.78).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__state_centric_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.87).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__state_centric_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
