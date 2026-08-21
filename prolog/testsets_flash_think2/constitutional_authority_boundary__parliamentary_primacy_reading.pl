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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy in Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary_primacy_reading' of
 *   the 'constitutional_authority_boundary' kernel. It describes a
 *   constitutional system where the elected legislature holds final authority
 *   to define constitutional meaning, with the constitutional text itself and
 *   judicial interpretations being subordinate to legislative will. This
 *   reading emphasizes democratic accountability and popular sovereignty as
 *   the ultimate source of constitutional legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.65).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '137dc86a-18cf-4a87-bd21-42bbbb2fa891').
narrative_ontology:cs_kernel_codification('137dc86a-18cf-4a87-bd21-42bbbb2fa891', fixed_text).
narrative_ontology:cs_authority_grounding('137dc86a-18cf-4a87-bd21-42bbbb2fa891', lineage).
narrative_ontology:cs_interpretation_layer_present('137dc86a-18cf-4a87-bd21-42bbbb2fa891').
narrative_ontology:cs_reading_relation('137dc86a-18cf-4a87-bd21-42bbbb2fa891', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('137dc86a-18cf-4a87-bd21-42bbbb2fa891', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('137dc86a-18cf-4a87-bd21-42bbbb2fa891', foundational, legislative_will_is_supreme).
narrative_ontology:cs_axiom_status(legislative_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('137dc86a-18cf-4a87-bd21-42bbbb2fa891', legislative_will_is_supreme, deontological).
narrative_ontology:cs_axiom('137dc86a-18cf-4a87-bd21-42bbbb2fa891', foundational, judicial_review_is_subordinate).
narrative_ontology:cs_axiom_status(judicial_review_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('137dc86a-18cf-4a87-bd21-42bbbb2fa891', judicial_review_is_subordinate, conventional).
narrative_ontology:cs_reference_frame('137dc86a-18cf-4a87-bd21-42bbbb2fa891', westminster_model_sovereignty).
narrative_ontology:cs_drift_state('137dc86a-18cf-4a87-bd21-42bbbb2fa891', contemporary_constitutional_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('137dc86a-18cf-4a87-bd21-42bbbb2fa891', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, citizenry).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, advocates_for_judicial_review).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_accountability_principle).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, popular_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains final authority to define constitutional meaning, allowing it to enact legislation that shapes the constitutional order without being subject to final judicial veto. Benefits from unconstrained legislative power.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Its interpretive authority is subordinate to the legislature. While it may offer advisory opinions or engage in weak-form review, its decisions can be overridden by ordinary or entrenched legislation. Bears the cost of limited constitutional power.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Benefits from the principle that constitutional meaning is ultimately determined by their elected representatives, ensuring democratic accountability. However, they may also be subject to legislative overreach without strong judicial checks.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, citizenry, beneficiary,
    organized, biographical, mobile, national).

% Benefits from a clear, legislatively defined constitutional framework, which provides stability and reduces uncertainty in policy implementation. Its actions are ultimately accountable to the legislature.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).

% Analyze the implications of parliamentary primacy for constitutional theory, institutional design, and democratic governance. Their role is to describe and critique, not to directly participate in the power dynamics.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% Argue for a stronger, independent role for the judiciary in constitutional interpretation, often citing concerns about minority rights or legislative tyranny. Their preferred model is actively suppressed by the parliamentary primacy framework.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, advocates_for_judicial_review, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the final authority for constitutional meaning, ensuring that the elected legislature retains ultimate power to define the constitutional order, thereby aligning constitutional law with democratic will.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitutional text from potentially competing judicial or distributed bodies to the elected legislature, consolidating power in the representative branch.
% ABSENT_VOICES: Proponents of strong-form judicial review or a distributed interpretive model are structurally excluded from the final decision-making process; they would argue for judicial checks on legislative power to protect rights or uphold constitutional principles.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished overnight, the constitutional order would become ambiguous, leading to inter-branch conflict over interpretive authority, potential judicial activism, and a fundamental re-ordering of institutional roles and the balance of power. The system would need to re-coordinate around a new locus of authority.
% FOUNDING_PROBLEM: To ensure that the ultimate authority for constitutional meaning rests with the democratically elected representatives, preventing unelected bodies from overriding the popular will and ensuring democratic accountability in governance.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, legal historians, and comparative constitutional analyses often corroborate the historical tension between democratic accountability and judicial power, supporting the idea that this constraint addresses a live problem of institutional design. Legislative debates and public discourse frequently reaffirm the importance of elected representatives having the final say.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (centralizing constitutional interpretation in the legislature for democratic accountability) but does so through asymmetric extraction. It extracts interpretive power from the judiciary (base_extractiveness 0.65) and actively suppresses alternative models of constitutional review (suppression 0.75). The theater_ratio is low (0.15) as the system is actively maintained and functional, not merely performative. Accessibility collapse is moderate (0.60) because while strong judicial review is foreclosed, advisory roles or weak review might still exist. Resistance is moderate (0.50) due to ongoing advocacy for stronger judicial checks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature and its supporters, this constraint is a legitimate and necessary coordination mechanism for democratic governance. From the perspective of the judiciary and advocates for judicial review, it represents an extraction of vital checks and balances, potentially leading to legislative tyranny. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the primary beneficiary and agenda-setter, gaining final interpretive authority. The citizenry also benefits from enhanced democratic accountability. The judiciary is a primary target/payer, as its interpretive power is constrained. Advocates for judicial review are excluded, as their preferred model is suppressed. The executive branch benefits from a clear legislative framework. Directionality for the legislature is near 0.0 (full beneficiary), while for the judiciary and advocates for judicial review, it is near 1.0 (full target).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging both the coordination function (democratic accountability) and the extractive component (subordination of judicial power). It is not a pure Rope because of the identifiable victims and active suppression, nor a pure Snare because of the genuine coordination problem it solves. The founding problem of ensuring democratic will is still live, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_will_representation_ambiguity,
    'Does the elected legislature truly represent the ''democratic will'' in constitutional matters, or is it susceptible to capture by special interests or transient majorities?',
    'Empirical analysis of legislative outcomes, public opinion surveys on constitutional issues, and studies of legislative capture over time.',
    'If legislative representation of the democratic will is found to be consistently flawed or captured, the justification for parliamentary primacy weakens, potentially reclassifying the constraint as more extractive from the citizenry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_will_representation_ambiguity, empirical, 'Ambiguity regarding the fidelity of legislative action to the broader democratic will.').

omega_variable(
    judicial_advisory_role_efficacy,
    'How effective is the judiciary''s advisory or weak-form review role in practice, given its subordination to parliamentary sovereignty?',
    'Case studies of judicial interventions, legislative responses to judicial opinions, and comparative analysis with systems featuring stronger judicial review.',
    'If the advisory role is found to be consistently ignored or ineffective, the suppression of judicial power is more complete than currently measured, increasing the constraint''s effective extractiveness from the judiciary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_advisory_role_efficacy, empirical, 'The practical impact of limited judicial review under parliamentary primacy.').

omega_variable(
    parliamentary_primacy_vs_judicial_supremacy_framing,
    'Is the ''parliamentary_primacy_reading'' a fundamentally distinct constitutional model from the ''judicial_supremacy_reading'', or are they two ends of a continuous spectrum of institutional power distribution?',
    'Conceptual analysis of foundational legal theory and political philosophy, focusing on the logical coherence of each model''s core premises.',
    'If they are found to be fundamentally distinct and logically contradictory, the ''forecloses'' relation is strongly validated. If they are merely points on a spectrum, the conceptual distinction might be less sharp, potentially shifting the relation to ''coexists_with'' or ''influences'' under certain conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_primacy_vs_judicial_supremacy_framing, conceptual, 'Conceptual distinction between parliamentary and judicial supremacy models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(cons_tr_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(cons_tr_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(cons_tr_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(cons_tr_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(cons_tr_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(cons_be_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(cons_be_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1940, 0.63).
narrative_ontology:measurement(cons_be_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1960, 0.64).
narrative_ontology:measurement(cons_be_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(cons_be_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(cons_su_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1920, 0.72).
narrative_ontology:measurement(cons_su_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1940, 0.73).
narrative_ontology:measurement(cons_su_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1960, 0.74).
narrative_ontology:measurement(cons_su_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(cons_su_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
