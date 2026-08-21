% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts possess final
 *   authority to interpret the constitution and nullify legislative acts. It
 *   is one reading of the broader 'constitutional_interpretive_authority'
 *   kernel, which is contested among different political and legal theories.
 *   This specific reading asserts the judiciary's role as the ultimate
 *   arbiter of constitutional meaning, often framed as essential for rights
 *   protection and constitutional stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.75).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.8).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '765196ec-5a56-45e6-9297-96802d103fbe').
narrative_ontology:cs_kernel_codification('765196ec-5a56-45e6-9297-96802d103fbe', fixed_text).
narrative_ontology:cs_authority_grounding('765196ec-5a56-45e6-9297-96802d103fbe', lineage).
narrative_ontology:cs_interpretation_layer_present('765196ec-5a56-45e6-9297-96802d103fbe').
narrative_ontology:cs_reading_relation('765196ec-5a56-45e6-9297-96802d103fbe', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('765196ec-5a56-45e6-9297-96802d103fbe', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('765196ec-5a56-45e6-9297-96802d103fbe', foundational, judicial_review_is_essential_for_rights_protection).
narrative_ontology:cs_axiom_status(judicial_review_is_essential_for_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('765196ec-5a56-45e6-9297-96802d103fbe', judicial_review_is_essential_for_rights_protection, deontological).
narrative_ontology:cs_axiom('765196ec-5a56-45e6-9297-96802d103fbe', foundational, constitution_is_supreme_law_judiciary_interprets).
narrative_ontology:cs_axiom_status(constitution_is_supreme_law_judiciary_interprets, holdable).
narrative_ontology:cs_axiom_grounding('765196ec-5a56-45e6-9297-96802d103fbe', constitution_is_supreme_law_judiciary_interprets, conventional).
narrative_ontology:cs_reference_frame('765196ec-5a56-45e6-9297-96802d103fbe', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('765196ec-5a56-45e6-9297-96802d103fbe', contemporary_global_constitutionalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('765196ec-5a56-45e6-9297-96802d103fbe', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars_supporting_judicial_review).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electorate).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final interpretive authority over the constitution, including the power to nullify legislative and executive acts. Benefits from expanded institutional power and legitimacy as the guardian of fundamental rights.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacts laws that are subject to judicial review and potential nullification, thereby having its democratic will subordinated to judicial interpretation. Its power to define constitutional meaning is significantly curtailed.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Implements policies and executes laws, but its actions and the laws it enforces can be challenged and overturned by the judiciary, limiting its operational autonomy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Elects representatives whose legislative output can be invalidated by unelected judges, diminishing direct democratic agency. While theoretically benefiting from rights protection, it pays in reduced self-governance.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electorate, payer,
    powerless, generational, constrained, national).

% Their interpretive frameworks and arguments are validated and empowered by the judiciary's role, enhancing their influence in legal and political discourse. They provide intellectual justification for judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars_supporting_judicial_review, beneficiary,
    organized, biographical, mobile, national).

% Their alternative interpretive frameworks, which prioritize legislative or coordinate constitutional construction, are marginalized by the dominance of judicial supremacy in legal practice and public discourse.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars_supporting_legislative_supremacy, excluded,
    organized, biographical, mobile, national).

% Observe and comment on national constitutional arrangements, often favoring strong judicial protection of rights, which can implicitly or explicitly support the judicial supremacy model.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative, and consistent interpretation of fundamental rights and constitutional limits, preventing legislative overreach and protecting minority rights from majoritarian impulses.
% TRANSFER_FUNCTION: Transfers final interpretive authority over the constitution from the elected legislative and executive branches to the unelected judiciary, along with the power to nullify laws and policies.
% ABSENT_VOICES: Proponents of legislative supremacy or coordinate constitutional construction are structurally marginalized; they would argue for greater democratic control over constitutional meaning and inter-branch dialogue, but their frameworks are not the dominant legal paradigm.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislative and executive branches would immediately assert greater interpretive authority, leading to intense inter-branch conflict over constitutional meaning. The legal system would undergo a fundamental reorganization, potentially shifting towards parliamentary supremacy or a more politically negotiated constitutional order.
% FOUNDING_PROBLEM: To protect fundamental rights and constitutional principles from majoritarian legislative impulses and ensure a consistent, principled application of the constitution across time and political shifts.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and its proponents attest that the problem of protecting rights from majoritarianism is still live, citing historical and contemporary instances of legislative overreach. Critics, including some political scientists and legal scholars from other traditions, argue that the problem is often exaggerated or that judicial review itself creates new problems of democratic deficit, citing academic literature and historical examples of judicial activism.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant transfer of power from elected branches to the judiciary. Suppression (0.80) is high due to the judiciary's power to nullify laws, effectively suppressing the legislative will. The theater ratio (0.40) is moderate; while judicial review involves genuine legal reasoning, there's also a performative aspect in asserting and maintaining its own authority, particularly when decisions are politically charged. The claimed type 'rope' represents the judiciary's self-justification as a coordinating mechanism for constitutional order, despite the high extraction and suppression metrics that suggest a more coercive classification from other seats.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this arrangement is a necessary 'rope' for constitutional coordination and rights protection. From the perspective of the legislature and electorate, it can be experienced as a 'snare' or 'tangled_rope' due to the significant power transfer and suppression of democratic will. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the primary beneficiary, gaining institutional power and interpretive finality. Constitutional scholars who support judicial review also benefit from the validation of their framework. The legislature, executive, and electorate are targets, as their democratic agency and policy choices are subordinated to judicial interpretation. International human rights bodies act as observers, often aligning with the rights-protective aspect of judicial review.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (protecting rights and constitutional principles) is still 'contested' in its status, but the specific mechanism of 'judicial supremacy' is increasingly seen by critics as having outlived its original justification or having accumulated excessive power. The divergence between the 'rope' claim and the high extraction/suppression metrics signals a potential false summit, where a coordination narrative masks an extractive reality. The 'contested' status of the founding problem further supports this analysis, indicating that the constraint's persistence may be more about institutional power than its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_judicial_supremacy,
    'Is this constraint a genuine, universally accepted feature of constitutionalism, or one specific reading of a contested kernel?',
    'Comparative constitutional analysis across different democratic systems and historical periods, noting the prevalence and structural variations of judicial review.',
    'If it is merely one reading, its ''mountain'' or ''rope'' claim is weakened, and its classification becomes contingent on the specific political and legal context, potentially reclassifying as a ''tangled_rope'' or ''snare'' from other seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_judicial_supremacy, conceptual, 'This constraint is one reading of the ''constitutional_interpretive_authority'' kernel, specifically the ''judicial_supremacy_reading''.').

omega_variable(
    sibling_impact_parliamentary_supremacy,
    'What would be the structural impact if the ''parliamentary_supremacy_reading'' were adopted instead?',
    'Analysis of constitutional systems where parliamentary supremacy is the norm (e.g., UK, New Zealand), focusing on the distribution of interpretive authority and the mechanisms for rights protection.',
    'The judiciary would lose its nullification power, and the legislature would gain final interpretive authority, fundamentally altering the power balance and potentially shifting the constraint''s classification for the legislature from ''payer'' to ''agenda_setter''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_impact_parliamentary_supremacy, empirical, 'Structural impact of adopting parliamentary supremacy.').

omega_variable(
    sibling_impact_coordinate_construction,
    'What would be the structural impact if the ''coordinate_construction_reading'' were adopted instead?',
    'Analysis of constitutional systems or theories advocating for inter-branch dialogue and shared interpretive responsibility (e.g., some Canadian constitutional theory, ''dialogue model''), focusing on how constitutional meaning is negotiated.',
    'No single branch would have final authority, requiring inter-branch dialogue and political negotiation for constitutional meaning. This would reduce the judiciary''s unilateral power and likely distribute the ''agenda_setter'' role more broadly, potentially reclassifying the constraint as a ''rope'' or ''scaffold'' from a system-level perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_coordinate_construction, empirical, 'Structural impact of adopting coordinate construction.').

omega_variable(
    disagreement_locus,
    'Where is the core disagreement regarding constitutional interpretive authority located?',
    'Conceptual analysis of legal and political theories of constitutionalism, identifying the fundamental premises that differentiate judicial supremacy from alternative models.',
    'Clarifying the locus of disagreement helps to precisely define the boundaries between competing constitutional constraints and informs which structural elements are most critical for classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus, conceptual, 'The core disagreement lies in the locus of final constitutional interpretive authority: whether it resides solely with the judiciary, with the legislature, or is distributed among branches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(cons_tr_t1920, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1920, 0.32).
narrative_ontology:measurement(cons_tr_t1940, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1940, 0.35).
narrative_ontology:measurement(cons_tr_t1960, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1960, 0.37).
narrative_ontology:measurement(cons_tr_t1980, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(cons_tr_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(cons_tr_t2020, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(cons_be_t1920, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(cons_be_t1940, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(cons_be_t1960, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(cons_be_t1980, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1980, 0.73).
narrative_ontology:measurement(cons_be_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(cons_be_t2020, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(cons_su_t1920, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(cons_su_t1940, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1940, 0.73).
narrative_ontology:measurement(cons_su_t1960, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1960, 0.76).
narrative_ontology:measurement(cons_su_t1980, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(cons_su_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(cons_su_t2020, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_process_efficiency).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, executive_policy_implementation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
