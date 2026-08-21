% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'judicial supremacy' reading of a
 *   constitutional text, where courts hold final interpretive authority and
 *   can invalidate legislation. It is one of several competing readings of
 *   the same constitutional kernel. The metrics reflect a system where
 *   judicial review is a powerful, actively enforced mechanism that extracts
 *   policy space from the legislature and democratic majorities, while
 *   providing a coordination function for rights protection. The claimed type
 *   'tangled_rope' reflects this hybrid nature: a genuine coordination
 *   function (rights protection, legal stability) coupled with asymmetric
 *   extraction (from democratic responsiveness).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '975be5eb-bf75-49cf-855c-5fb403e333e9').
narrative_ontology:cs_kernel_codification('975be5eb-bf75-49cf-855c-5fb403e333e9', fixed_text).
narrative_ontology:cs_authority_grounding('975be5eb-bf75-49cf-855c-5fb403e333e9', lineage).
narrative_ontology:cs_interpretation_layer_present('975be5eb-bf75-49cf-855c-5fb403e333e9').
narrative_ontology:cs_reading_relation('975be5eb-bf75-49cf-855c-5fb403e333e9', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('975be5eb-bf75-49cf-855c-5fb403e333e9', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('975be5eb-bf75-49cf-855c-5fb403e333e9', foundational, judicial_review_is_final).
narrative_ontology:cs_axiom_status(judicial_review_is_final, holdable).
narrative_ontology:cs_axiom_grounding('975be5eb-bf75-49cf-855c-5fb403e333e9', judicial_review_is_final, conventional).
narrative_ontology:cs_axiom('975be5eb-bf75-49cf-855c-5fb403e333e9', foundational, constitution_is_supreme_law).
narrative_ontology:cs_axiom_status(constitution_is_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('975be5eb-bf75-49cf-855c-5fb403e333e9', constitution_is_supreme_law, deontological).
narrative_ontology:cs_reference_frame('975be5eb-bf75-49cf-855c-5fb403e333e9', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('975be5eb-bf75-49cf-855c-5fb403e333e9', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('975be5eb-bf75-49cf-855c-5fb403e333e9', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text and invalidates legislation deemed unconstitutional. Its authority is final, making it the ultimate arbiter of constitutional meaning. This role is central to its institutional identity and power.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes legislation that can be struck down by the judiciary. Its policy preferences are constrained by judicial review, and it has no direct means to override a constitutional interpretation by the courts, short of a difficult amendment process.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Benefit from judicial protection of individual and minority rights against majoritarian overreach. They rely on the courts to enforce constitutional limits on legislative power, often having no other recourse.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, immediate, constrained, national).

% Bear the cost of having their policy preferences, expressed through elected representatives, invalidated by unelected judges. Their ability to enact their will is suppressed by judicial review, leading to frustration and calls for reform.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% Analyze the implications of judicial supremacy for constitutional theory, democratic legitimacy, and comparative law. They provide critical commentary and alternative framings of constitutional authority.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of constitutional meaning, preventing legislative overreach and protecting fundamental rights, thereby coordinating legal and political action around a fixed supreme law.
% TRANSFER_FUNCTION: Transfers final interpretive authority over constitutional meaning from the legislature (and by extension, democratic majorities) to the judiciary, along with the power to invalidate legislation.
% ABSENT_VOICES: Advocates for legislative sovereignty and popular sovereignty are structurally marginalized in this reading; they would argue for parliamentary supremacy or direct popular constitutional amendment, but their mechanisms for final say are foreclosed or severely constrained.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, the legislature would immediately gain unchecked power to interpret the constitution, potentially leading to rapid shifts in rights protections and a more politically responsive, but less stable, constitutional order. The entire legal and political system would reconfigure.
% FOUNDING_PROBLEM: To prevent legislative tyranny and protect fundamental rights from majoritarian impulses, ensuring a stable and supreme law that transcends ordinary politics.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and rights advocates attest that the problem of majoritarian overreach and the need for rights protection remain live. Critics from the legislature and popular sovereignty movements acknowledge the historical problem but argue that judicial supremacy has become an overreach in itself, creating a new problem of democratic deficit.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because a significant portion of legislative policy space is removed from democratic control. Suppression is also high, as the legislature has limited means to challenge or override judicial interpretations. Theater ratio is low, as the judiciary's function is genuinely active and impactful, not merely performative. Accessibility collapse is high for legislative alternatives to judicial interpretations, while resistance is moderate, reflecting ongoing political contestation and calls for reform without direct means of override.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the legislature's and democratic majorities' perspective, it is a 'snare' that extracts democratic responsiveness and policy autonomy. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary and agenda-setter, as it wields final authority. Rights claimants are beneficiaries, as their interests are protected. The legislature and democratic majorities are payers/victims, as their policy choices are constrained and invalidated. The 'identity_locked' exit for the judiciary reflects that its institutional identity is fused with its role as final arbiter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_rights_protection,
    'Is the democratic deficit created by judicial supremacy a necessary cost for robust rights protection, or an unacceptable erosion of popular sovereignty?',
    'Conceptual analysis of constitutional theory and comparative study of systems with different balances of judicial and legislative power, assessed against normative commitments to democracy and rights.',
    'If deemed an unacceptable erosion, the extractiveness of this constraint would be re-evaluated as more severe, potentially shifting its classification towards a ''snare'' from the democratic majority''s seat. If deemed a necessary cost, the coordination function would be emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection, preference, 'The normative trade-off between democratic responsiveness and counter-majoritarian rights protection.').

omega_variable(
    judicial_activism_vs_restraint,
    'To what extent does the judiciary''s exercise of final interpretive authority reflect genuine constitutional meaning versus policy preferences of unelected judges?',
    'Empirical study of judicial decision-making patterns, analysis of dissenting opinions, and comparison of judicial outcomes with public opinion and legislative intent over time.',
    'If judicial decisions consistently align with policy preferences rather than clear constitutional text or original intent, the ''theater_ratio'' would increase, and the ''extractiveness'' would be seen as less legitimate, potentially weakening the ''rope'' aspect and strengthening the ''snare'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_restraint, empirical, 'The degree to which judicial review is driven by legal interpretation versus policy preferences.').

omega_variable(
    amendment_process_efficacy,
    'Is the constitutional amendment process a viable mechanism for democratic majorities to override judicial interpretations, or is it practically inaccessible?',
    'Comparative analysis of amendment rates, success stories, and political barriers across different constitutional systems. Historical analysis of attempts to amend the constitution to overturn judicial decisions.',
    'If the amendment process is practically inaccessible, the ''suppression'' metric for democratic majorities would be higher, reinforcing the ''snare'' aspect of the constraint. If it is a viable, albeit difficult, path, suppression would be slightly lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_efficacy, empirical, 'The practical accessibility of constitutional amendment as a check on judicial power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the 'constitutional_text' kernel. This 'judicial supremacy' reading directly influences the operational space of the 'legislative sovereignty' and 'popular sovereignty' readings by asserting final interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
