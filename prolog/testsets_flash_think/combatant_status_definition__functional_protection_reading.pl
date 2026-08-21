% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Universal Protections
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'functional protection' reading of the
 *   combatant status definition kernel. It asserts that all persons detained
 *   in armed conflict are entitled to Common Article 3 minimum protections,
 *   including humane treatment and fair trial rights, regardless of their
 *   formal combatant status. This reading aims to establish a universal floor
 *   of protections, preventing states from denying basic rights by
 *   manipulating status classifications. While intended as a protective
 *   'rope', it faces significant resistance from states seeking greater
 *   discretion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.2).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Universal Protections").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '69e6d29e-1548-445c-b663-542cb17c8eb3').
narrative_ontology:cs_kernel_codification('69e6d29e-1548-445c-b663-542cb17c8eb3', fixed_text).
narrative_ontology:cs_authority_grounding('69e6d29e-1548-445c-b663-542cb17c8eb3', lineage).
narrative_ontology:cs_interpretation_layer_present('69e6d29e-1548-445c-b663-542cb17c8eb3').
narrative_ontology:cs_reading_relation('69e6d29e-1548-445c-b663-542cb17c8eb3', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('69e6d29e-1548-445c-b663-542cb17c8eb3', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('69e6d29e-1548-445c-b663-542cb17c8eb3', foundational, inherent_human_dignity).
narrative_ontology:cs_axiom_status(inherent_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('69e6d29e-1548-445c-b663-542cb17c8eb3', inherent_human_dignity, deontological).
narrative_ontology:cs_axiom('69e6d29e-1548-445c-b663-542cb17c8eb3', foundational, status_determination_is_not_a_precondition_for_basic_rights).
narrative_ontology:cs_axiom_status(status_determination_is_not_a_precondition_for_basic_rights, holdable).
narrative_ontology:cs_axiom_grounding('69e6d29e-1548-445c-b663-542cb17c8eb3', status_determination_is_not_a_precondition_for_basic_rights, deontological).
narrative_ontology:cs_reference_frame('69e6d29e-1548-445c-b663-542cb17c8eb3', universal_humanitarian_floor).
narrative_ontology:cs_drift_state('69e6d29e-1548-445c-b663-542cb17c8eb3', contemporary_counter_terrorism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('69e6d29e-1548-445c-b663-542cb17c8eb3', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, humanitarian_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, states_detaining_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals deprived of liberty during armed conflict, regardless of their formal combatant status, who receive a baseline of humane treatment and fair trial rights under this principle.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Organizations like the ICRC that advocate for and monitor the application of IHL, benefiting from a clear, universal standard of protection that simplifies their mandate and reduces protection gaps.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% States that detain persons during armed conflict are obligated to provide Common Article 3 protections, incurring costs for humane treatment, due process, and oversight, regardless of their preferred status classification for detainees. They often resist this universal application.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, states_detaining_powers, payer,
    institutional, biographical, constrained, global).

% Bodies like the ICC and ICTY that interpret and enforce IHL, including Common Article 3, by prosecuting violations. Their rulings reinforce the universal application of these protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_courts_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Legal scholars who emphasize strict, formalistic interpretations of combatant status, often arguing for a narrow application of protections based on state-centric criteria. Their arguments are directly challenged by the functional protection reading.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_centric_legal_scholars, excluded,
    analytical, biographical, analytical, global).

% While primarily seeking full combatant status, members of these groups indirectly benefit from the functional protection reading by receiving a baseline of humane treatment and fair trial rights even if their combatant status is denied by detaining powers.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, national_liberation_movements, beneficiary,
    organized, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for humane treatment and fair trial rights for all persons deprived of liberty during armed conflict, preventing a 'protection gap' based on status determination and ensuring a minimum standard of conduct for all parties to a conflict.
% TRANSFER_FUNCTION: Transfers the obligation for humane treatment and due process from being status-dependent to being universally applicable to all detainees, shifting the burden of proof for non-application onto detaining powers.
% ABSENT_VOICES: States that insist on strict status-based distinctions to deny protections to certain categories of detainees, particularly those labeled 'terrorists' or 'unlawful combatants', are structurally excluded from this reading's premise. They would argue for greater state discretion in determining who receives protections.
% DISAPPEARANCE_RATIONALE: If this principle vanished, states would revert to status-dependent protection regimes, leading to significant human rights abuses, arbitrary detention, and denial of due process for many individuals caught in armed conflicts, particularly in non-international armed conflicts. The international legal framework for detention would fragment.
% FOUNDING_PROBLEM: The historical problem of states denying basic human rights to individuals captured in armed conflict by refusing to grant them combatant status or by creating new categories of 'unlawful' combatants, thereby circumventing IHL protections.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, humanitarian organizations (e.g., ICRC), and numerous legal scholars consistently attest to the ongoing problem of states circumventing protections through status denial, supported by reports from conflict zones and international legal jurisprudence.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the principle itself is protective, aiming to reduce harm rather than extract. Suppression is also low (0.20) as it seeks to limit the suppressive power of states to deny rights. However, resistance is high (0.70) because many states, particularly those engaged in counter-terrorism operations, actively resist the universal application of these protections, preferring status-dependent regimes. The claimed type is 'rope' because it functions as a coordination mechanism for humane treatment, even if its universal acceptance is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detainees and humanitarian organizations, this constraint is a vital protective 'rope'. From the perspective of many states, it is an unwelcome imposition that limits their operational flexibility and sovereignty, potentially computing as a 'snare' on their discretion. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons and humanitarian organizations are clear beneficiaries (low d) as the constraint directly protects them and facilitates humanitarian work. States, as detaining powers, are payers (high d) as they bear the costs of compliance and face limitations on their discretion. International courts act as agenda-setters, enforcing the principle. State-centric legal scholars are excluded from this reading's premise, as their core arguments are directly contradicted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_sovereignty_vs_universal_rights,
    'To what extent does the functional protection reading genuinely constrain state sovereignty, and how much of the resistance is a legitimate concern for security versus a pretext for denying rights?',
    'Analysis of state practice in jurisdictions with strong judicial oversight versus those without, comparing security outcomes and human rights records. Examination of legislative debates and official justifications for status-based distinctions.',
    'If resistance is primarily a pretext, the effective suppression of the constraint is higher than measured, as states actively undermine its intent. If legitimate security concerns are paramount, the constraint''s ''rope'' function is more fragile and requires greater coordination to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sovereignty_vs_universal_rights, conceptual, 'Ambiguity between legitimate state security concerns and pretextual denial of universal protections.').

omega_variable(
    empirical_effectiveness_in_practice,
    'Despite its legal articulation, how effectively is the functional protection reading implemented in practice, particularly in non-international armed conflicts and against non-state armed groups?',
    'Field research, reports from humanitarian organizations, and empirical studies on detention practices in various conflict zones. Analysis of international court judgments and state compliance records.',
    'If implementation is consistently weak, the constraint''s effective extractiveness (from detainees) is higher than the baseline suggests, as the ''protection'' is largely theoretical. This would push its classification closer to a ''piton'' or ''snare'' for detainees, despite its ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_effectiveness_in_practice, empirical, 'Gap between legal principle and practical application of universal protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1969, combatant_status_definition__functional_protection_reading, theater_ratio, 1969, 0.07).
narrative_ontology:measurement(comb_tr_t1989, combatant_status_definition__functional_protection_reading, theater_ratio, 1989, 0.08).
narrative_ontology:measurement(comb_tr_t2004, combatant_status_definition__functional_protection_reading, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(comb_tr_t2014, combatant_status_definition__functional_protection_reading, theater_ratio, 2014, 0.11).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1969, combatant_status_definition__functional_protection_reading, base_extractiveness, 1969, 0.12).
narrative_ontology:measurement(comb_be_t1989, combatant_status_definition__functional_protection_reading, base_extractiveness, 1989, 0.13).
narrative_ontology:measurement(comb_be_t2004, combatant_status_definition__functional_protection_reading, base_extractiveness, 2004, 0.16).
narrative_ontology:measurement(comb_be_t2014, combatant_status_definition__functional_protection_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement(comb_su_t1969, combatant_status_definition__functional_protection_reading, suppression_requirement, 1969, 0.22).
narrative_ontology:measurement(comb_su_t1989, combatant_status_definition__functional_protection_reading, suppression_requirement, 1989, 0.2).
narrative_ontology:measurement(comb_su_t2004, combatant_status_definition__functional_protection_reading, suppression_requirement, 2004, 0.23).
narrative_ontology:measurement(comb_su_t2014, combatant_status_definition__functional_protection_reading, suppression_requirement, 2014, 0.21).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
