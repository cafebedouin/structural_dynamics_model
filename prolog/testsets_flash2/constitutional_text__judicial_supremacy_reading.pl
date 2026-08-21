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
 *   constitutional text, where courts hold final interpretive authority, and
 *   their invalidation of legislation is the conclusive determination of
 *   constitutional meaning. This reading positions the judiciary as a
 *   gatekeeper, ensuring high rigidity in constitutional interpretation and
 *   protecting rights-claimants against majoritarian overreach, but at the
 *   cost of democratic responsiveness. This is one reading of the
 *   'constitutional_text' kernel, distinct from legislative or popular
 *   sovereignty readings.
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
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '38270c84-ef35-4ef8-b2ac-acfeef3061c9').
narrative_ontology:cs_kernel_codification('38270c84-ef35-4ef8-b2ac-acfeef3061c9', fixed_text).
narrative_ontology:cs_authority_grounding('38270c84-ef35-4ef8-b2ac-acfeef3061c9', lineage).
narrative_ontology:cs_interpretation_layer_present('38270c84-ef35-4ef8-b2ac-acfeef3061c9').
narrative_ontology:cs_reading_relation('38270c84-ef35-4ef8-b2ac-acfeef3061c9', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('38270c84-ef35-4ef8-b2ac-acfeef3061c9', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('38270c84-ef35-4ef8-b2ac-acfeef3061c9', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('38270c84-ef35-4ef8-b2ac-acfeef3061c9', judicial_finality_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('38270c84-ef35-4ef8-b2ac-acfeef3061c9', foundational, constitution_as_supreme_law_enforced_by_judiciary).
narrative_ontology:cs_axiom_status(constitution_as_supreme_law_enforced_by_judiciary, holdable).
narrative_ontology:cs_axiom_grounding('38270c84-ef35-4ef8-b2ac-acfeef3061c9', constitution_as_supreme_law_enforced_by_judiciary, deontological).
narrative_ontology:cs_reference_frame('38270c84-ef35-4ef8-b2ac-acfeef3061c9', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('38270c84-ef35-4ef8-b2ac-acfeef3061c9', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38270c84-ef35-4ef8-b2ac-acfeef3061c9', '').
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

% Interprets the constitutional text and invalidates legislation deemed unconstitutional. Its authority is final, making it the ultimate arbiter of constitutional meaning. Its institutional identity is fused with this interpretive role.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes legislation that can be invalidated by the judiciary. Its policy choices are constrained by judicial review, and it lacks a direct override mechanism for constitutional interpretations. Bears the cost of legislative efforts being struck down.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Benefit from judicial protection against majoritarian overreach, ensuring their constitutional rights are upheld even against popular legislative initiatives. Their recourse is through the courts.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, immediate, constrained, national).

% Bear the cost of their democratically elected representatives' will being thwarted by judicial decisions. Their ability to enact policy through the legislature is limited by judicial review, leading to frustration and a sense of disempowerment.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% Analyze the implications of judicial supremacy for constitutional theory, democratic legitimacy, and comparative law. They provide critical commentary and alternative framings without directly participating in the enforcement or bearing its costs.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of the constitutional text, preventing legislative overreach and ensuring consistent application of fundamental rights across different political cycles.
% TRANSFER_FUNCTION: Transfers final interpretive authority over constitutional meaning from the legislature (and by extension, democratic majorities) to the judiciary, thereby shifting the power to define the limits of state action.
% ABSENT_VOICES: Advocates for legislative sovereignty and popular sovereignty are structurally marginalized in this reading; they would argue for greater democratic control over constitutional meaning, but their mechanisms for asserting this are foreclosed or severely constrained by judicial finality.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the constitutional landscape would immediately shift. Legislatures would assert greater interpretive authority, potentially leading to a period of constitutional instability or a re-negotiation of interpretive roles. Rights previously protected by judicial review might become vulnerable to majoritarian shifts.
% FOUNDING_PROBLEM: To prevent legislative tyranny and ensure the enduring protection of fundamental rights against transient political majorities, establishing a stable, independent arbiter of constitutional meaning.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many rights advocacy groups attest that the problem of majoritarian overreach remains live, necessitating judicial protection. Critics (legislators, some political scientists) argue that while the problem of tyranny is real, judicial supremacy creates its own form of unaccountable power, making the 'solution' itself a problem.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates (stable constitutional interpretation, rights protection) but also involves significant asymmetric extraction (from the legislature and democratic majorities) and requires active enforcement (judicial review). Extractiveness is high (0.65) due to the power imbalance and the inability of other branches to easily override judicial decisions. Suppression (0.70) reflects the active enforcement of judicial decisions and the structural barriers to legislative or popular challenges. Theater ratio is low (0.10) as the judicial function is largely genuine and effective, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary Rope for constitutional stability and rights protection. From the legislature's and democratic majorities' perspective, it is a Snare that extracts democratic self-governance. The engine's computation will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary and agenda-setter, as it wields final interpretive power. Rights claimants are beneficiaries, as their interests are protected. The legislature and democratic majorities are victims/payers, as their policy preferences are constrained and sometimes invalidated. Constitutional scholars are observers, analyzing the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting rights, ensuring constitutional stability) is still live, but its implementation through judicial supremacy is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The ongoing contestation over its legitimacy and democratic costs is central to its character.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_judicial_review,
    'Is judicial invalidation of democratically enacted legislation a legitimate exercise of constitutional authority, or an anti-democratic usurpation of power?',
    'Conceptual analysis of constitutional theory, political philosophy, and comparative legal systems, focusing on the source and limits of judicial power in a democracy.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be re-evaluated as more purely coercive; if legitimate, its coordination function would be emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_judicial_review, conceptual, 'The fundamental tension between judicial review and democratic self-governance.').

omega_variable(
    judicial_activism_vs_restraint,
    'To what extent do courts interpret the constitutional text versus imposing their own policy preferences?',
    'Empirical analysis of judicial decisions over time, comparing outcomes to original intent, evolving societal norms, and judicial philosophy statements. Expert legal and political science analysis.',
    'If judicial activism is prevalent, the ''theater_ratio'' might be higher than currently assessed, as the stated function (interpreting text) would mask a different function (policy-making). This would also increase perceived extractiveness from democratic processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_restraint, empirical, 'The degree to which judicial decisions reflect interpretation versus policy-making.').

omega_variable(
    constitutional_rigidity_optimal_level,
    'Is the high rigidity in constitutional interpretation imposed by judicial supremacy optimal for societal stability and progress, or does it hinder necessary adaptation?',
    'Comparative studies of constitutional systems with varying degrees of rigidity and judicial power, analyzing long-term societal and political outcomes. Historical analysis of constitutional crises and adaptations.',
    'If the rigidity is found to be suboptimal, the ''accessibility_collapse'' and ''suppression'' metrics might be re-interpreted as more detrimental, suggesting a need for mechanisms to allow greater constitutional flexibility or popular input.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_rigidity_optimal_level, preference, 'Whether the level of constitutional rigidity is beneficial or detrimental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.1).
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
% This constraint is one of three distinct readings of the 'constitutional_text' kernel. Each reading defines a different locus of final interpretive authority, leading to different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
