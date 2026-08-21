% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (Humanitarian Ceiling Reading)
 *   domain: international_law/humanitarian_ethics/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which asserts that International Humanitarian Law
 *   (IHL) establishes absolute minimum standards for the treatment of persons
 *   in armed conflict, regardless of adversary compliance or reciprocity.
 *   This interpretation places a significant, non-negotiable burden on states
 *   to adhere to these standards, even when it may conflict with perceived
 *   security interests. The constraint is claimed as a Tangled Rope because
 *   it genuinely coordinates states towards a collective good (humanitarian
 *   protection) but does so by extracting operational flexibility from state
 *   militaries and requiring active enforcement against impulses to
 *   prioritize security above all else.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (Humanitarian Ceiling Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_law/humanitarian_ethics/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '21689d46-8cc7-48e6-92a7-e4d98b5f807b').
narrative_ontology:cs_kernel_codification('21689d46-8cc7-48e6-92a7-e4d98b5f807b', fixed_text).
narrative_ontology:cs_authority_grounding('21689d46-8cc7-48e6-92a7-e4d98b5f807b', lineage).
narrative_ontology:cs_interpretation_layer_present('21689d46-8cc7-48e6-92a7-e4d98b5f807b').
narrative_ontology:cs_reading_relation('21689d46-8cc7-48e6-92a7-e4d98b5f807b', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('21689d46-8cc7-48e6-92a7-e4d98b5f807b', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('21689d46-8cc7-48e6-92a7-e4d98b5f807b', foundational, humanity_principle_absolute).
narrative_ontology:cs_axiom_status(humanity_principle_absolute, holdable).
narrative_ontology:cs_axiom_grounding('21689d46-8cc7-48e6-92a7-e4d98b5f807b', humanity_principle_absolute, deontological).
narrative_ontology:cs_axiom('21689d46-8cc7-48e6-92a7-e4d98b5f807b', foundational, non_reciprocity_of_ihl).
narrative_ontology:cs_axiom_status(non_reciprocity_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('21689d46-8cc7-48e6-92a7-e4d98b5f807b', non_reciprocity_of_ihl, conventional).
narrative_ontology:cs_reference_frame('21689d46-8cc7-48e6-92a7-e4d98b5f807b', post_wwii_humanitarian_consensus).
narrative_ontology:cs_drift_state('21689d46-8cc7-48e6-92a7-e4d98b5f807b', contemporary_asymmetric_warfare_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('21689d46-8cc7-48e6-92a7-e4d98b5f807b', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_security_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, security_strategists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of operational constraints, rules of engagement, and accountability for actions, even when facing non-state or non-compliant adversaries. Their perceived security and tactical flexibility are limited by these absolute minimums.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, biographical, constrained, global).

% Ratify and are legally bound by the Conventions, but often face internal and external pressure to prioritize national security over strict humanitarian compliance, especially in asymmetric conflicts. They are responsible for implementing and enforcing IHL within their armed forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).

% Receive protection from direct attack, indiscriminate violence, and inhumane treatment, regardless of the nature of the conflict or the actions of their own state or adversary. Their survival and basic rights are prioritized.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Are guaranteed humane treatment, due process, and basic protections, even if they do not qualify for Prisoner of War status. Their vulnerability is mitigated by non-reciprocal standards.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Advocate for the strict interpretation and enforcement of the Conventions, monitor compliance, and provide aid under their protective framework. They rely on the absolute nature of IHL for their operational legitimacy and safety.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations, agenda_setter,
    organized, biographical, mobile, global).

% Interpret and apply the Conventions, holding individuals and states accountable for violations. Their legitimacy depends on upholding the absolute and non-reciprocal nature of IHL, often against state claims of necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% Often argue for greater flexibility in military operations and find the absolute humanitarian minimums to be an impediment to effective security policy, particularly against non-state actors. They bear the intellectual and political cost of justifying constrained options.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_strategists, payer,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a universal, non-reciprocal baseline of humane treatment and protection for all persons in armed conflict, preventing a 'race to the bottom' in violence and ensuring a minimum standard of civilization even amidst war.
% TRANSFER_FUNCTION: Transfers operational flexibility and perceived security advantages from state militaries and security apparatuses to civilian populations and detained persons, in exchange for a universal standard of humane conduct and the preservation of human dignity.
% ABSENT_VOICES: Those who advocate for absolute state sovereignty, unfettered military action, or the suspension of humanitarian law in 'existential' conflicts are often present in national security debates but are structurally excluded from the core interpretive community of IHL, which asserts the non-derogable nature of these protections.
% DISAPPEARANCE_RATIONALE: If the Geneva Conventions and their 'humanitarian ceiling' interpretation vanished overnight, the legal and moral framework for limiting state violence would collapse. This would likely lead to a significant increase in atrocities, a breakdown of international norms, and a more brutal and indiscriminate form of warfare, fundamentally reorganizing the conduct of armed conflict.
% FOUNDING_PROBLEM: The widespread atrocities, indiscriminate violence against civilians, and inhumane treatment of combatants during World War II, which demonstrated the urgent need for clear, universally binding rules of war that would apply regardless of the nature of the conflict or the identity of the belligerents.
% FOUNDING_PROBLEM_CORROBORATION: International criminal tribunals, human rights organizations, the International Committee of the Red Cross (ICRC), and historical analyses consistently corroborate the ongoing relevance of preventing atrocities and upholding humanitarian standards. While states contest specific applications, the fundamental problem of limiting the brutality of war remains live, as evidenced by contemporary conflicts and ongoing efforts to ensure compliance.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading imposes significant costs on states by limiting their perceived freedom of action and requiring adherence to strict rules even in challenging asymmetric conflicts. Suppression (0.75) is high because this reading actively suppresses security rationales and military expediency that would otherwise justify less restrained conduct. The theater ratio (0.40) is moderate; while states often pay lip service to IHL, this specific reading emphasizes its absolute nature, reducing the purely performative aspect within this interpretation, though practical compliance remains a challenge. Resistance (0.70) is high, reflecting ongoing debates and challenges from military and security establishments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state militaries and security strategists, this reading of the Conventions can feel highly extractive, imposing an asymmetric burden that hinders effective operations. From the perspective of humanitarian organizations and civilian populations, it is an essential coordination mechanism that provides vital protection and upholds fundamental human dignity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and security apparatuses are the primary targets/payers, as the constraint limits their operational freedom and imposes accountability. Civilian populations, detained combatants, and humanitarian organizations are the beneficiaries, receiving protection and a framework for aid. State governments and international courts act as agenda-setters, responsible for upholding and enforcing these standards, often mediating between the demands of security and humanitarian principles.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction from states) or a Snare (which would ignore the genuine coordination function for humanitarian protection). It accurately captures the dual nature: a collective good achieved through asymmetric costs and active enforcement against powerful actors' natural inclinations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_conditional_application,
    'Is the application of IHL truly absolute and non-reciprocal, or does state practice and interpretation implicitly allow for conditional application based on adversary compliance?',
    'Analysis of state declarations, military manuals, and judicial decisions in cases involving non-state actors or non-compliant adversaries. If a consistent pattern of conditional application emerges, the ''absolute'' claim is weakened.',
    'If conditional application is found to be prevalent, the ''humanitarian ceiling'' reading''s extractiveness from states would decrease (as they gain more flexibility), and its claimed type might shift towards a more reciprocal ''Rope'' or even ''Piton'' if the absolute claim becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_vs_conditional_application, empirical, 'Ambiguity regarding the absolute vs. conditional nature of IHL application.').

omega_variable(
    humanitarian_security_tension_resolution,
    'To what extent can the tension between humanitarian protection and state security imperatives be genuinely resolved within the existing IHL framework, or does it represent an irreducible conflict of values?',
    'Longitudinal study of state compliance in asymmetric conflicts: if states consistently find ways to achieve security objectives within IHL limits, the conflict is resolvable. If violations persist despite legal and political pressure, it suggests an irreducible conflict.',
    'If the conflict is irreducible, the ''humanitarian ceiling'' reading''s suppression and extractiveness will remain high, reflecting the constant struggle. If resolvable, the constraint might evolve towards a more stable ''Rope'' as states integrate IHL into their security doctrines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_security_tension_resolution, conceptual, 'Irreducible conflict between humanitarian and security imperatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1969, 0.25).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1989, 0.3).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2014, 0.45).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.55).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1969, 0.6).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1989, 0.63).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2004, 0.67).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2014, 0.69).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.65).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1969, 0.7).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1989, 0.73).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2004, 0.78).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2014, 0.8).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 1949 Geneva Conventions kernel, each with different structural properties and implications for state behavior. This 'humanitarian ceiling' reading emphasizes absolute, non-reciprocal protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
