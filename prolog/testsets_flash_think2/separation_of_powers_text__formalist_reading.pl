% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Reading of Separation of Powers (Non-Delegation Doctrine)
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the formalist reading of the separation of
 *   powers, specifically the non-delegation doctrine, which asserts strict,
 *   impermeable boundaries between legislative, executive, and judicial
 *   functions. It holds that Congress cannot delegate its legislative
 *   authority to administrative agencies. This reading is a live legal
 *   theory, particularly influential in certain judicial and academic
 *   circles, and if fully enforced, would drastically curtail the power and
 *   scope of the modern administrative state. The high extractiveness and
 *   suppression metrics reflect the operational impact of this reading,
 *   despite its proponents claiming it as a fundamental, natural limit (a
 *   'mountain').
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.85).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.9).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, mountain).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Reading of Separation of Powers (Non-Delegation Doctrine)").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).
domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '602e0ea3-1664-4c3e-991a-c882a2f3e185').
narrative_ontology:cs_kernel_codification('602e0ea3-1664-4c3e-991a-c882a2f3e185', fixed_text).
narrative_ontology:cs_authority_grounding('602e0ea3-1664-4c3e-991a-c882a2f3e185', lineage).
narrative_ontology:cs_interpretation_layer_present('602e0ea3-1664-4c3e-991a-c882a2f3e185').
narrative_ontology:cs_reading_relation('602e0ea3-1664-4c3e-991a-c882a2f3e185', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('602e0ea3-1664-4c3e-991a-c882a2f3e185', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('602e0ea3-1664-4c3e-991a-c882a2f3e185', foundational, strict_separation_of_powers_principle).
narrative_ontology:cs_axiom_status(strict_separation_of_powers_principle, holdable).
narrative_ontology:cs_axiom_grounding('602e0ea3-1664-4c3e-991a-c882a2f3e185', strict_separation_of_powers_principle, deontological).
narrative_ontology:cs_axiom('602e0ea3-1664-4c3e-991a-c882a2f3e185', foundational, non_delegation_principle).
narrative_ontology:cs_axiom_status(non_delegation_principle, holdable).
narrative_ontology:cs_axiom_grounding('602e0ea3-1664-4c3e-991a-c882a2f3e185', non_delegation_principle, conventional).
narrative_ontology:cs_reference_frame('602e0ea3-1664-4c3e-991a-c882a2f3e185', original_constitutional_design).
narrative_ontology:cs_drift_state('602e0ea3-1664-4c3e-991a-c882a2f3e185', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('602e0ea3-1664-4c3e-991a-c882a2f3e185', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_legal_scholars).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industries_seeking_less_regulation).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, public_interest_groups_relying_on_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the non-delegation doctrine, potentially striking down statutes that delegate legislative authority to agencies. Views itself as upholding the constitutional structure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Theoretically reclaims legislative authority, but practically faces increased workload and complexity if it cannot delegate. Benefits from a clearer assertion of its constitutional role.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, legislative_branch, beneficiary,
    institutional, generational, constrained, national).

% Loses flexibility and capacity to implement policy through administrative agencies, facing reduced ability to respond to complex societal problems without direct legislative action.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_branch, payer,
    institutional, generational, constrained, national).

% Face an existential threat as their statutory basis for action is undermined. Their ability to issue rules and enforce regulations is drastically curtailed, leading to reduced capacity and potential dissolution.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Their interpretive framework, emphasizing strict adherence to constitutional text and original intent, is vindicated and gains influence in legal discourse and judicial decisions.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_legal_scholars, beneficiary,
    analytical, generational, analytical, universal).

% Benefit from a reduction in administrative regulation, potentially leading to lower compliance costs and greater operational freedom, though they may face increased uncertainty from legislative inaction.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries_seeking_less_regulation, beneficiary,
    organized, biographical, mobile, national).

% Suffer from the rollback of regulations designed to protect public health, safety, and environment. Their advocacy efforts become less effective as the primary mechanism for implementing policy is curtailed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, public_interest_groups_relying_on_regulation, payer,
    organized, biographical, constrained, national).

% Would argue for the practical necessity and constitutional permissibility of delegation in a complex modern state, but their arguments are dismissed by the formalist reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_legal_scholars, excluded,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to ensure clear lines of accountability for lawmaking, preventing the concentration of power and ensuring that legislative decisions are made by elected representatives.
% TRANSFER_FUNCTION: Transfers effective lawmaking authority from administrative agencies back to the legislative branch, and potentially from the executive (as implementer) to the legislative (as sole lawmaker).
% ABSENT_VOICES: Functionalist legal scholars and administrative law practitioners, who would argue that strict non-delegation is impractical and that modern governance requires flexible delegation to expert agencies. They are excluded from the formalist's interpretive framework.
% DISAPPEARANCE_RATIONALE: If the formalist reading of strict separation vanished, the existing administrative state, which relies on delegated authority, would be validated, and the ongoing legal challenges to agency power would cease, fundamentally altering the balance of power in government.
% FOUNDING_PROBLEM: Preventing tyranny through the concentration of legislative, executive, and judicial powers in a single body or person, ensuring democratic accountability for lawmaking.
% FOUNDING_PROBLEM_CORROBORATION: Formalist scholars and some originalist judges attest that the problem of concentrated power and unaccountable lawmaking is still live. Functionalist scholars and most administrative law practitioners argue that the founding problem has evolved and requires flexible solutions, citing the complexity of modern governance and the need for expert administration. Legislative hearing testimony and historical analysis from outside the formalist camp support the evolved-problem reading.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(separation_of_powers_text__formalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.90) reflect the severe impact this reading would have on administrative agencies and the executive branch, effectively stripping them of significant policy-making power. The accessibility collapse is high (0.92) because it aims to eliminate the alternative of agency-led regulation. Resistance is high (0.75) due to strong opposition from functionalist legal scholars, administrative law practitioners, and public interest groups. Theater ratio is low (0.10) because the formalist claim is a direct legal argument, not primarily performative. The historical measurements show a gradual increase in extractiveness and suppression as the formalist reading has gained judicial and political traction over time, particularly since the mid-20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and legislative branch (and formalist scholars), this reading is a necessary defense of constitutional structure and liberty, a 'mountain' that must be respected. From the perspective of administrative agencies, the executive, and public interest groups, it is a highly extractive and suppressive 'snare' that undermines effective governance and public welfare. The engine's classification will highlight this divergence between the claimed type and operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legislative branch are beneficiaries as they assert their constitutional prerogatives, with the judiciary acting as the primary enforcer. Formalist legal scholars also benefit from the vindication of their interpretive framework. Regulated industries seeking less oversight benefit from reduced agency power. Conversely, administrative agencies and the executive branch are targets, losing significant authority and capacity. Public interest groups relying on agency regulation for protection are also victims, as their policy goals become harder to achieve.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalism_vs_natural_law,
    'Is the strict separation of powers, as interpreted by the formalist reading, a genuine structural feature of governance (a natural law), or a constructed legal doctrine that benefits specific actors?',
    'Comparative constitutional analysis across diverse political systems: if all stable, free societies exhibit similar strict boundaries regardless of their founding texts, it supports natural law; if boundaries vary with legal tradition and political choice, it supports a constructed doctrine.',
    'If a constructed doctrine, the ''mountain'' claim is a false summit, and the constraint''s classification would shift towards a ''tangled_rope'' or ''snare'' depending on the degree of extraction and coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_vs_natural_law, conceptual, 'Ambiguity between a natural structural limit and a constructed legal interpretation.').

omega_variable(
    delegation_necessity_empirical,
    'Is extensive delegation of legislative authority to administrative agencies a practical necessity for effective governance in a complex modern state, or can Congress effectively legislate on all detailed matters?',
    'Empirical study of legislative capacity and policy outcomes in jurisdictions with varying degrees of delegation. If non-delegating legislatures are demonstrably less effective, it supports the necessity argument.',
    'If delegation is empirically necessary, the formalist reading''s high suppression of alternatives is unsustainable, and its long-term viability as a ''mountain'' is undermined, pushing it towards a ''piton'' or ''snare'' if maintained through inertia or coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_necessity_empirical, empirical, 'Whether the non-delegation doctrine is practically viable in modern governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1789, separation_of_powers_text__formalist_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(sepa_tr_t1850, separation_of_powers_text__formalist_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__formalist_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__formalist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__formalist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__formalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1789, separation_of_powers_text__formalist_reading, base_extractiveness, 1789, 0.1).
narrative_ontology:measurement(sepa_be_t1850, separation_of_powers_text__formalist_reading, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__formalist_reading, base_extractiveness, 1930, 0.4).
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__formalist_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__formalist_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__formalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1789, separation_of_powers_text__formalist_reading, suppression_requirement, 1789, 0.15).
narrative_ontology:measurement(sepa_su_t1850, separation_of_powers_text__formalist_reading, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__formalist_reading, suppression_requirement, 1930, 0.5).
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__formalist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__formalist_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__formalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
