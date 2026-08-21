% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy in Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'parliamentary primacy' reading of the
 *   constitutional authority boundary, where the elected legislature holds
 *   final authority over constitutional meaning. It is presented as a
 *   Mountain due to its foundational role in certain constitutional systems,
 *   reflecting a deeply entrenched principle of democratic accountability.
 *   The metrics reflect low extraction and suppression, as this reading is
 *   often seen as a natural consequence of democratic principles, with
 *   minimal performative aspects. This reading is one of several competing
 *   interpretations of how constitutional authority is distributed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.25).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, mountain).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:emerges_naturally(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, 'eac5006e-fc71-4b84-9919-2a3cf998ad36').
narrative_ontology:cs_kernel_codification('eac5006e-fc71-4b84-9919-2a3cf998ad36', formalized).
narrative_ontology:cs_authority_grounding('eac5006e-fc71-4b84-9919-2a3cf998ad36', lineage).
narrative_ontology:cs_interpretation_layer_present('eac5006e-fc71-4b84-9919-2a3cf998ad36').
narrative_ontology:cs_reading_relation('eac5006e-fc71-4b84-9919-2a3cf998ad36', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('eac5006e-fc71-4b84-9919-2a3cf998ad36', constitutional_authority_boundary__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('eac5006e-fc71-4b84-9919-2a3cf998ad36', foundational, democratic_accountability_is_supreme).
narrative_ontology:cs_axiom_status(democratic_accountability_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('eac5006e-fc71-4b84-9919-2a3cf998ad36', democratic_accountability_is_supreme, deontological).
narrative_ontology:cs_axiom('eac5006e-fc71-4b84-9919-2a3cf998ad36', foundational, legislative_will_is_final_constitutional_arbiter).
narrative_ontology:cs_axiom_status(legislative_will_is_final_constitutional_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('eac5006e-fc71-4b84-9919-2a3cf998ad36', legislative_will_is_final_constitutional_arbiter, conventional).
narrative_ontology:cs_reference_frame('eac5006e-fc71-4b84-9919-2a3cf998ad36', westminster_parliamentary_tradition).
narrative_ontology:cs_drift_state('eac5006e-fc71-4b84-9919-2a3cf998ad36', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eac5006e-fc71-4b84-9919-2a3cf998ad36', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electorate).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning, either through ordinary legislation or entrenched constitutional amendments. Benefits from minimal judicial interference in policy-making and constitutional interpretation, reflecting the democratic mandate.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Exercises judicial review, but its interpretations are subordinate to legislative will. Its rulings can be overturned by ordinary legislation or constitutional amendment, limiting its power to act as a final arbiter. Bears the cost of having its constitutional interpretations overridden.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, civilizational, constrained, national).

% Benefits from the principle that elected representatives, accountable to the people, have the final say on constitutional matters, ensuring democratic responsiveness. Can hold legislators accountable through elections for their constitutional interpretations.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electorate, beneficiary,
    organized, generational, mobile, national).

% Analyze the theoretical and practical implications of parliamentary primacy, comparing it with other models of constitutional authority. Their work informs public and political discourse but does not directly alter the constraint.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of constitutional interpretation, ensuring that the will of the democratically elected legislature ultimately prevails, thereby coordinating governmental action around a single, politically accountable source of constitutional meaning.
% TRANSFER_FUNCTION: Transfers final interpretive authority over the constitutional text from the judiciary to the elected legislature, ensuring that constitutional meaning is ultimately determined by politically accountable representatives.
% ABSENT_VOICES: Advocates for strong-form judicial review, who would argue for an independent judiciary as the ultimate guardian of constitutional rights against majoritarian overreach, are structurally marginalized by this reading. Their arguments are heard in academic and legal discourse but do not alter the legislative finality.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished, the constitutional landscape would fundamentally shift. The judiciary would likely assert greater interpretive authority, potentially leading to increased judicial activism and a rebalancing of power among branches of government, altering the very nature of the political system.
% FOUNDING_PROBLEM: To ensure that constitutional interpretation remains responsive to the democratic will and that unelected bodies do not override the decisions of elected representatives, preventing judicial oligarchy.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and historical documents from the founding of parliamentary systems corroborate the intent to prioritize democratic accountability. Public opinion polls often show support for elected officials having final say on major policy, including constitutional matters, over unelected judges.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constitutional_authority_boundary__parliamentary_primacy_reading),
    narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily defines a power distribution rather than extracting resources. Suppression is also low (0.25) as the judiciary's role is structurally limited, not actively coerced in a punitive sense; its interpretations are simply not final. Theater ratio is minimal (0.05) as the system operates largely as described, without significant performative maintenance. Accessibility collapse is high (0.88) because, within this framework, alternatives to legislative finality are largely foreclosed. Resistance is low (0.1) because the principle is widely accepted in systems where it is foundational.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature and the electorate, this constraint is a natural and desirable feature of a democratic system, ensuring accountability. From the judiciary's perspective, it represents a limitation on its power, but one that is accepted as part of its institutional role. The engine's classification will reflect this structural distribution of authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and the electorate are the primary beneficiaries, as this reading empowers democratic representation. The judiciary is the primary 'victim' in the sense that its interpretive authority is constrained, but this is a structural limitation, not a punitive extraction. Constitutional scholars act as observers, analyzing the system without direct participation in its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_primacy_vs_judicial_supremacy,
    'Is this constraint a genuine ''Mountain'' reflecting an unchangeable principle of democratic governance, or a ''Tangled Rope'' where legislative power is extracted from the judiciary under the guise of democratic accountability?',
    'Analysis of historical instances where legislative interpretations have demonstrably undermined fundamental rights without effective judicial recourse, or where the legislative process itself has become unrepresentative.',
    'If it''s found to be a Tangled Rope, the classification would shift, indicating a hidden extractive function where legislative power is used to suppress judicial checks and balances, rather than merely defining their scope. This would imply higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_primacy_vs_judicial_supremacy, conceptual, 'Ambiguity between a foundational democratic principle and a mechanism for legislative overreach.').

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the principle of parliamentary primacy a natural consequence of democratic theory, or a constructed institutional choice that benefits identifiable agents (the legislature)?',
    'Comparative analysis with other democratic systems that adopt different models of constitutional authority (e.g., strong-form judicial review) to determine if parliamentary primacy is an inevitable outcome or a contingent design choice.',
    'If found to be a constructed choice, the ''emerges_naturally'' flag would be re-evaluated, potentially shifting the constraint away from a pure Mountain classification, especially given the presence of beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether parliamentary primacy is an inherent feature of democracy or a specific institutional design.').

omega_variable(
    reading_difference_on_final_arbiter,
    'The core disagreement between this ''parliamentary_primacy_reading'' and the ''judicial_supremacy_reading'' is on the identity of the final arbiter of constitutional meaning. Which structural element of the constitutional system is the locus of this disagreement?',
    'Analysis of constitutional texts and historical practice in systems embodying each reading, focusing on provisions for judicial review, legislative override, and constitutional amendment processes.',
    'If the disagreement is primarily over the ''finality'' of interpretation, it highlights a fundamental conceptual split in constitutional design. Resolution would clarify the structural implications of each reading for the balance of power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_difference_on_final_arbiter, conceptual, 'Locus of disagreement regarding the final arbiter of constitutional meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
