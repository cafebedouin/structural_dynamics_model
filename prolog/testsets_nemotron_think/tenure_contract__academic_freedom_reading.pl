% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Academic Freedom Guarantee
 *   domain: higher_education_governance
 *
 * SUMMARY:
 *   This constraint story represents the academic_freedom_reading of the
 *   tenure_contract kernel. It models tenure as a coordination mechanism that
 *   solves a genuine collective-action problem: protecting inquiry that power
 *   would suppress. The reading claims rope — pure coordination with
 *   negligible extraction. Beneficiaries are faculty (tenured and aspiring)
 *   and students; external political actors are excluded from direct control.
 *   The claimed type and metrics are authored independently: the reading
 *   asserts rope, while the metrics describe a low-extraction, actively
 *   enforced arrangement with moderate accessibility collapse and rising
 *   theater as contingent labor grows.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.22).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Guarantee").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '89c34039-77bc-44ba-aa32-e1a408ad0969').
narrative_ontology:cs_kernel_codification('89c34039-77bc-44ba-aa32-e1a408ad0969', formalized).
narrative_ontology:cs_authority_grounding('89c34039-77bc-44ba-aa32-e1a408ad0969', practice).
narrative_ontology:cs_interpretation_layer_present('89c34039-77bc-44ba-aa32-e1a408ad0969').
narrative_ontology:cs_reading_relation('89c34039-77bc-44ba-aa32-e1a408ad0969', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('89c34039-77bc-44ba-aa32-e1a408ad0969', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('89c34039-77bc-44ba-aa32-e1a408ad0969', foundational, tenure_necessary_for_high_risk_inquiry).
narrative_ontology:cs_axiom_status(tenure_necessary_for_high_risk_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('89c34039-77bc-44ba-aa32-e1a408ad0969', tenure_necessary_for_high_risk_inquiry, empirically_contingent).
narrative_ontology:cs_axiom('89c34039-77bc-44ba-aa32-e1a408ad0969', foundational, academic_freedom_requires_economic_independence).
narrative_ontology:cs_axiom_status(academic_freedom_requires_economic_independence, holdable).
narrative_ontology:cs_axiom_grounding('89c34039-77bc-44ba-aa32-e1a408ad0969', academic_freedom_requires_economic_independence, deontological).
narrative_ontology:cs_reference_frame('89c34039-77bc-44ba-aa32-e1a408ad0969', professional_self_governance_model).
narrative_ontology:cs_drift_state('89c34039-77bc-44ba-aa32-e1a408ad0969', contemporary_contingent_majority_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89c34039-77bc-44ba-aa32-e1a408ad0969', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, early_career_researchers).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, academic_freedom_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, truth_seeking_independence).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, professional_self_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments with protection from dismissal without cause. Their research agendas are insulated from direct administrative or political pressure. The security enables long-term, high-risk projects. Exit means leaving academia entirely or accepting a non-tenured position elsewhere — a significant career disruption.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, biographical, constrained, national).

% Doctoral students, postdocs, and tenure-track assistant professors working toward tenure. They benefit from the prospect of future protection and from the research culture tenure sustains. Their exit options include leaving the academic track for industry or alternative careers — costly but feasible at this stage.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, early_career_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Receive education and mentorship from faculty whose inquiry is not distorted by fear of reprisal. The benefit is diffuse and indirect — quality of research environment, exposure to contested ideas. Exit means transferring institutions, which carries credit and financial costs.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, constrained, national).

% Administer tenure policies, conduct reviews, and manage budgets. They set procedural rules but are constrained by faculty governance norms and AAUP standards. They cannot unilaterally remove tenured faculty. Exit means moving to another administration role; the constraint travels with the sector.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Legislators, donors, interest groups, and media figures who would influence research directions through funding threats or public pressure. Tenure blocks the most direct lever — firing faculty for unpopular conclusions. They remain excluded from direct control over personnel decisions, though they exert influence through funding and legislation.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, excluded,
    powerful, biographical, analytical, national).

% Scholars in the same fields at other institutions, including international peers. They evaluate tenure cases, set disciplinary standards, and constitute the audience for high-risk work. Their judgment is the ultimate validator of the research tenure protects. Exit is not applicable — they observe from outside the local constraint.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, disciplinary_peers, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects high-risk, long-horizon inquiry by decoupling researcher survival from the displeasure of administrators, donors, legislators, or public opinion. Solves the coordination problem: how to sustain research that may be unpopular, offensive to power, or simply unlikely to succeed, without each researcher individually bargaining for protection.
% TRANSFER_FUNCTION: Moves the power to terminate faculty appointments from unilateral administrative/political control to a peer-review process governed by disciplinary standards. Administration gives up direct hiring/firing authority over tenured positions; faculty gain career security contingent on professional judgment rather than institutional favor.
% ABSENT_VOICES: Contingent faculty (adjuncts, lecturers, non-tenure-track researchers) who perform a majority of teaching but lack tenure's protections. Their precarity is the structural counterpart to tenure's security, but this reading does not center them — they would object to a system that concentrates security in a shrinking tier while expanding insecure labor.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, universities would revert to at-will employment for all faculty. Political and administrative pressure on research topics would become direct and immediate. High-risk inquiry — climate science, critical race theory, genome editing ethics, economic heterodoxy — would face existential threat. The entire research ecosystem would reorganize around funder and administrator preferences.
% FOUNDING_PROBLEM: Early 20th century political firings of professors for anti-war views (WWI), economic dissent, and evolutionary biology teaching. The 1915 AAUP Declaration and 1940 Statement of Principles codified tenure as the institutional solution to external coercion of academic judgment.
% FOUNDING_PROBLEM_CORROBORATION: AAUP historical archives and institutional histories document the founding firings. The 'threat is gone' position is advanced by some administrators and legislators who cite formal academic freedom policies as sufficient. Current academic freedom advocates (AAUP, FIRE, disciplinary associations) attest that new coercion forms — legislative gag orders, donor conditions, social media campaigns — recreate the founding problem. No consensus exists.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the reading assesses the standing arrangement as primarily subsidizing its beneficiaries — faculty receive security, students receive uncoerced inquiry. The cost to administration/political actors is the loss of a control lever, not a transfer to faculty. Suppression is low (0.15) because tenure *reduces* suppression of research; the suppression_requirement series tracks how much enforcement the tenure system itself needs to maintain its protections (peer review, due process), which has declined then modestly risen. Theater rises as the tenure-line share of faculty falls — the protection becomes less universally available, making the coordination story less complete.
 *
 * PERSPECTIVAL GAP:
 *   The academic_freedom_reading sees coordination; the institutional_extraction_reading sees rent extraction by early winners; the demographic_reproduction_reading sees gatekeeping. These are not perspectives on one constraint — they are distinct constraints with different ε, different beneficiaries/victims, different types. This story authors ONLY the academic_freedom_reading's ε-invariant view. The divergence between readings is the kernel's contest, captured in omegas and cs_structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty and early-career researchers are structural beneficiaries (d near 0): the constraint subsidizes their independence. Administration is agenda_setter (d ~ 0.5): they bear administrative costs but gain institutional stability. External political actors are excluded (d not computed — they are not governed by the constraint; they are blocked by it). Students are diffuse beneficiaries (d ~ 0.3): they gain indirectly, bear no direct cost. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (political firing) is contested as live or dead. If dead, tenure persists as scaffold without sunset or piton. If live, it remains rope. The reading's axioms treat it as live (holdable). The mandatrophy question is whether the arrangement has outlived its function — this reading says no, but the contest is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the tenure contract a single constraint with multiple perspectives, or are the academic_freedom, institutional_extraction, and demographic_reproduction readings structurally distinct constraints with different ε, beneficiaries, and types?',
    'Apply the ε-invariance test: if measuring tenure''s extraction from faculty yields low ε (this reading) but measuring extraction from contingent faculty yields high ε (institutional_extraction_reading), and measuring demographic exclusion yields high ε (demographic_reproduction_reading), then the label ''tenure'' covers multiple constraints. Decompose per DP-001.',
    'If decomposed, each reading gets its own constraint story with independent classification. The academic_freedom_reading would likely remain rope; institutional_extraction_reading would be snare or tangled_rope; demographic_reproduction_reading would be tangled_rope or snare. The kernel linkage would be traced via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the tenure contract kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    marginal_protection_vs_formal_policy,
    'How much incremental protection does tenure provide beyond formal academic freedom policies (AAUP 1940 Statement, faculty handbooks, statutory protections)?',
    'Compare research-topic diversity and controversy engagement in tenure-line vs. non-tenure-line faculty at institutions with identical formal academic freedom policies. Natural experiments: institutions that abolished tenure (e.g., some UK post-1988, US for-profits).',
    'If tenure adds negligible protection beyond formal policies, its coordination function is largely theatrical — theater_ratio understates performativity. If tenure adds substantial protection, the low extractiveness claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_protection_vs_formal_policy, empirical, 'Whether tenure''s protection is marginal or essential given existing formal guarantees.').

omega_variable(
    contingent_labor_structural_coupling,
    'Does the security tenure provides for some faculty structurally require the precarity of contingent faculty, or are they independent variables?',
    'Track tenure-line share and contingent share over time at system level. If they are inversely coupled (tenure lines shrink as contingent lines grow, holding total instructional capacity constant), the coupling is structural. If both can grow/shrink independently, they are decoupled.',
    'If structurally coupled, the academic_freedom_reading''s beneficiary set (tenured_faculty, early_career) is sustained by extraction from an excluded group (contingent_faculty) — making this reading a tangled_rope or snare from a wider frame. If decoupled, the reading''s rope claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_labor_structural_coupling, empirical, 'Whether tenure security and contingent precarity are structurally linked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 84).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_afr_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tenure_afr_tr_t14, tenure_contract__academic_freedom_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(tenure_afr_tr_t28, tenure_contract__academic_freedom_reading, theater_ratio, 28, 0.14).
narrative_ontology:measurement(tenure_afr_tr_t42, tenure_contract__academic_freedom_reading, theater_ratio, 42, 0.16).
narrative_ontology:measurement(tenure_afr_tr_t56, tenure_contract__academic_freedom_reading, theater_ratio, 56, 0.17).
narrative_ontology:measurement(tenure_afr_tr_t70, tenure_contract__academic_freedom_reading, theater_ratio, 70, 0.18).
narrative_ontology:measurement(tenure_afr_tr_t84, tenure_contract__academic_freedom_reading, theater_ratio, 84, 0.18).

% Extraction over time
narrative_ontology:measurement(tenure_afr_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tenure_afr_be_t14, tenure_contract__academic_freedom_reading, base_extractiveness, 14, 0.12).
narrative_ontology:measurement(tenure_afr_be_t28, tenure_contract__academic_freedom_reading, base_extractiveness, 28, 0.14).
narrative_ontology:measurement(tenure_afr_be_t42, tenure_contract__academic_freedom_reading, base_extractiveness, 42, 0.18).
narrative_ontology:measurement(tenure_afr_be_t56, tenure_contract__academic_freedom_reading, base_extractiveness, 56, 0.2).
narrative_ontology:measurement(tenure_afr_be_t70, tenure_contract__academic_freedom_reading, base_extractiveness, 70, 0.22).
narrative_ontology:measurement(tenure_afr_be_t84, tenure_contract__academic_freedom_reading, base_extractiveness, 84, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(tenure_afr_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tenure_afr_su_t14, tenure_contract__academic_freedom_reading, suppression_requirement, 14, 0.2).
narrative_ontology:measurement(tenure_afr_su_t28, tenure_contract__academic_freedom_reading, suppression_requirement, 28, 0.15).
narrative_ontology:measurement(tenure_afr_su_t42, tenure_contract__academic_freedom_reading, suppression_requirement, 42, 0.12).
narrative_ontology:measurement(tenure_afr_su_t56, tenure_contract__academic_freedom_reading, suppression_requirement, 56, 0.14).
narrative_ontology:measurement(tenure_afr_su_t70, tenure_contract__academic_freedom_reading, suppression_requirement, 70, 0.15).
narrative_ontology:measurement(tenure_afr_su_t84, tenure_contract__academic_freedom_reading, suppression_requirement, 84, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, contingent_labor_expansion).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, academic_freedom_legislation).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, university_governance_reform).

% DUAL FORMULATION NOTE:
% This story is the academic_freedom_reading of the tenure_contract kernel. It decomposes the colloquial 'tenure' into ε-invariant constraints per DP-001. The institutional_extraction_reading and demographic_reproduction_reading are sibling constraints with different ε, beneficiaries, and types, linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
