% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction Reading of Constitutional Authority
 *   domain: constitutional/political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the coordinate construction reading of the
 *   constitutional authority boundary kernel: the claim that the
 *   constitutional text establishes three co-equal branches with distributed
 *   interpretive authority, such that no single branch serves as the final,
 *   unchallengeable arbiter of constitutional meaning. Each branch interprets
 *   the Constitution within its own sphere of operations, with mechanisms
 *   like executive non-acquiescence and legislative jurisdiction control
 *   serving as expressions of this distributed authority rather than
 *   pathologies. The reading presents a tangled rope: it genuinely
 *   coordinates power distribution and prevents tyranny, but asymmetrically
 *   extracts from rights-bearers and litigants who bear the uncertainty and
 *   enforcement gaps of inter-branch conflict. The constraint is claimed as
 *   tangled_rope independently of the metrics; the moderate extraction
 *   (Îµ=0.42) and suppression (0.45) reflect the genuine coordination costs
 *   and active inter-branch resistance, not pure extraction.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary agenda-setter (institutional/constrained) â defends interpretive autonomy through non-acquiescence and enforcement discretion.
 *   - legislative_branch: Primary agenda-setter (institutional/constrained) â defends autonomy through jurisdiction control and impeachment.
 *   - judiciary: Primary agenda-setter (institutional/constrained) â interprets within judicial sphere but lacks finality over coordinate branches.
 *   - rights_bearers_and_litigants: Primary target (powerless/trapped) â bear uncertainty and delayed rights resolution when branches conflict.
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â documents debates without institutional authority to resolve them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.45).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional/political/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '611dfc6c-5dd2-4d01-af88-e3e1f312e9e6').
narrative_ontology:cs_kernel_codification('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', fixed_text).
narrative_ontology:cs_authority_grounding('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', distributed).
narrative_ontology:cs_reading_relation('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', foundational, no_final_interpreter_premise).
narrative_ontology:cs_axiom_status(no_final_interpreter_premise, holdable).
narrative_ontology:cs_axiom_grounding('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', no_final_interpreter_premise, conventional).
narrative_ontology:cs_axiom('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', foundational, departmental_autonomy_principle).
narrative_ontology:cs_axiom_status(departmental_autonomy_principle, holdable).
narrative_ontology:cs_axiom_grounding('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', departmental_autonomy_principle, instrumental).
narrative_ontology:cs_reference_frame('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', departmental_equality_framework).
narrative_ontology:cs_drift_state('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', contemporary_judicial_supremacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('611dfc6c-5dd2-4d01-af88-e3e1f312e9e6', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, rights_bearers_and_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional limits on its own authority, directs enforcement discretion, and occasionally declines to enforce statutes or judicial orders it deems unconstitutional. Defends a sphere of autonomous constitutional judgment against judicial or legislative encroachment.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).

% Enacts legislation with independent constitutional judgment, controls federal court jurisdiction through Article III powers, and impeaches judges. Asserts coordinate authority to interpret the Constitution rather than deferring to judicial finality.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).

% Resolves cases and controversies with constitutional interpretation but lacks final authority over coordinate branches. Its interpretations bind parties to cases but do not automatically command executive enforcement or legislative acquiescence beyond the judicial sphere.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bear the uncertainty and delay when branches disagree on constitutional meaning. Their rights and obligations may shift depending on which branch's interpretation prevails in a given crisis, with no guaranteed final resolution.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, rights_bearers_and_litigants, payer,
    powerless, biographical, trapped, national).

% Analyze and debate the legitimacy of competing branch interpretations, documenting historical instances of non-acquiescence and coordinate construction without institutional authority to resolve disputes.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes constitutional interpretive authority across three co-equal branches so that no single institution can monopolize constitutional meaning, preserving mutual checking and branch autonomy against tyrannical consolidation.
% TRANSFER_FUNCTION: Moves the power of final constitutional determination away from any single branch and into the inter-branch friction zone; rights-bearers and litigants bear the costs of uncertainty, delay, and potential non-enforcement when branches conflict.
% ABSENT_VOICES: Advocates of a unitary constitutional arbiter (whether judicial or legislative) who would prefer the clarity and predictability of a single final interpreter; also, legal systems outside the separation-of-powers tradition that reject branch autonomy in favor of parliamentary sovereignty.
% DISAPPEARANCE_RATIONALE: If coordinate construction vanished overnight, one branch would likely consolidate final interpretive supremacy (most plausibly the judiciary in the US context), collapsing the mutual-checking structure and reorganizing constitutional politics around a single, final arbiter with concomitant shifts in rights enforcement and policy stability.
% FOUNDING_PROBLEM: Preventing the concentration of tyrannical power in a single governmental organ by ensuring each branch could check the others' constitutional interpretations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Federalist Papers and anti-federalist writings attesting fear of consolidated power; contested by modern constitutional lawyers who argue the founding generation actually intended judicial supremacy, and by comparative constitutional scholars who note parliamentary systems solve the same problem differently. Outside the benefiting branches, independent historians and political scientists provide cross-cutting attestation.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the distributed authority system creates real costs of constitutional uncertainty and non-enforcement, particularly for rights-holders caught between conflicting branch interpretations. Suppression is moderate (0.45) because the constraint suppresses the alternative of a single final arbiter through institutional inertia, branch self-interest, and normative attachment to separation-of-powers mythology rather than through transparent coercion. Theater is moderate-low (0.25): some branch assertions of autonomy are performative, but the inter-branch friction is often functionally real. Accessibility collapse is 0.60 because alternatives (judicial supremacy, parliamentary sovereignty) are thinkable and historically available, yet institutionally distant due to the entrenched three-branch structure. Resistance is 0.55 because each branch actively resists encroachments by the others, keeping the constraint in dynamic tension.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter branches (executive, legislative, judicial) experience this constraint as preserving their essential autonomy and institutional dignity; from their seats, the arrangement is primarily coordination. The payer seat (rights-bearers and litigants) experiences the same structure as a source of unpredictable rights enforcement and constitutional ambiguity. The engine will compute low directionality for the branches (beneficiaries with constrained but real institutional power) and high directionality for rights-bearers (victims with trapped exit), producing divergent seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches are declared beneficiaries because the constraint's primary structural effect is to preserve each branch's interpretive autonomy against absorption by the others. Their directionality is pushed toward the beneficiary end by their institutional power and the subsidy-like effect of autonomy protection. Rights-bearers and litigants are declared victims because they bear the extraction: when branches disagree on constitutional meaning, rights remain unresolved and obligations uncertain. Their directionality is pushed toward the full-target end by their powerlessness and trapped exit (citizenship is not voluntarily escapable). No override is needed because the structural derivation chain produces the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordinate construction reading prevents mislabeling by requiring both coordination and extraction to be present for tangled_rope classification. The constraint has a genuine coordination function (preventing tyranny through mutual checking) AND identifiable victims (rights-bearers bearing uncertainty costs). If the coordination function were absent, the constraint would be a snare of institutional confusion; if the extraction were absent, it would be a pure rope of separated powers. The moderate theater ratio (0.25) prevents piton misclassification by showing the constraint is still largely functional, not merely performative inertia. The founding problem (preventing tyranny) is contested, preventing automatic scaffold classification; the absence of a sunset clause confirms it is not transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_uncertainty_distribution,
    'Does the uncertainty generated by distributed interpretive authority fall disproportionately on politically marginalized groups, or is it evenly distributed across the polity?',
    'Empirical analysis of constitutional crises and non-acquiescence episodes tracking which demographic groups bear the material costs of unresolved constitutional ambiguity.',
    'If costs are concentrated, the constraint is more extractive than its symmetrical framing suggests; if evenly distributed, the extraction is closer to a coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_uncertainty_distribution, empirical, 'Distribution of uncertainty costs across the polity').

omega_variable(
    coordinate_vs_supremacy_kernel,
    'Does the constitutional kernel textually mandate coordinate construction, or is coordinate construction itself an interpretive gloss that obscures the text''s actual grant of final authority to one branch?',
    'Historical-linguistic analysis of the constitutional text and ratification debates; structural analysis of Article III and Article II powers.',
    'If the kernel itself establishes a final arbiter, coordinate construction is a false reading that extracts legitimacy from the text; if the kernel genuinely distributes authority, the moderate extraction is an inherent cost of the design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_vs_supremacy_kernel, conceptual, 'Whether the kernel textually supports coordinate construction or vests final authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_auth_coord_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(const_auth_coord_tr_t43, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 43, 0.22).
narrative_ontology:measurement(const_auth_coord_tr_t100, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(const_auth_coord_tr_t160, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(const_auth_coord_tr_t200, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(const_auth_coord_tr_t235, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 235, 0.25).

% Extraction over time
narrative_ontology:measurement(const_auth_coord_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(const_auth_coord_be_t43, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 43, 0.38).
narrative_ontology:measurement(const_auth_coord_be_t100, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(const_auth_coord_be_t160, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 160, 0.4).
narrative_ontology:measurement(const_auth_coord_be_t200, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement(const_auth_coord_be_t235, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 235, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(const_auth_coord_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(const_auth_coord_su_t43, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 43, 0.4).
narrative_ontology:measurement(const_auth_coord_su_t100, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(const_auth_coord_su_t160, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 160, 0.45).
narrative_ontology:measurement(const_auth_coord_su_t200, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(const_auth_coord_su_t235, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 235, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_authority_boundary kernel. The kernel decomposes into at least three structurally distinct constraints (coordinate_construction_reading, judicial_supremacy_reading, parliamentary_primacy_reading) because each reading assigns a different Îµ and a different beneficiary/victim structure to the same constitutional text. They form a constraint family linked through cs_structure.reading_relations rather than unidirectional causal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
