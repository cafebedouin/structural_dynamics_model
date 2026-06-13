% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold final, binding
 *   authority over the meaning of the constitution. This reading emphasizes
 *   judicial independence and specialized legal expertise as essential for
 *   upholding the rule of law and protecting rights. It is one of several
 *   competing readings of how constitutional meaning is determined.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.6).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '97f733c8-84a2-4409-8a47-58d50aef81e6').
narrative_ontology:cs_kernel_codification('97f733c8-84a2-4409-8a47-58d50aef81e6', fixed_text).
narrative_ontology:cs_authority_grounding('97f733c8-84a2-4409-8a47-58d50aef81e6', lineage).
narrative_ontology:cs_interpretation_layer_present('97f733c8-84a2-4409-8a47-58d50aef81e6').
narrative_ontology:cs_reading_relation('97f733c8-84a2-4409-8a47-58d50aef81e6', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('97f733c8-84a2-4409-8a47-58d50aef81e6', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('97f733c8-84a2-4409-8a47-58d50aef81e6', foundational, judicial_independence_essential_for_rights).
narrative_ontology:cs_axiom_status(judicial_independence_essential_for_rights, holdable).
narrative_ontology:cs_axiom_grounding('97f733c8-84a2-4409-8a47-58d50aef81e6', judicial_independence_essential_for_rights, deontological).
narrative_ontology:cs_axiom('97f733c8-84a2-4409-8a47-58d50aef81e6', foundational, specialized_legal_expertise_required_for_interpretation).
narrative_ontology:cs_axiom_status(specialized_legal_expertise_required_for_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('97f733c8-84a2-4409-8a47-58d50aef81e6', specialized_legal_expertise_required_for_interpretation, conventional).
narrative_ontology:cs_reference_frame('97f733c8-84a2-4409-8a47-58d50aef81e6', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('97f733c8-84a2-4409-8a47-58d50aef81e6', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97f733c8-84a2-4409-8a47-58d50aef81e6', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of constitutional meaning, exercising judicial review to strike down legislation deemed unconstitutional. Benefits from enhanced institutional prestige and power, grounded in claims of legal expertise and independence.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes legislation that can be invalidated by judicial review, leading to frustration of democratic mandates and legislative gridlock. Bears the cost of having its policy choices overridden by an unelected body.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    institutional, biographical, constrained, national).

% See their policy preferences, expressed through elected representatives, blocked by judicial decisions. Experience a dilution of their democratic power and a sense of disenfranchisement when courts act as a counter-majoritarian force.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, immediate, constrained, national).

% Benefits from the complexity and specialized nature of constitutional law, which judicial supremacy entrenches. Expertise in constitutional interpretation becomes a valuable commodity, enhancing professional status and influence.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, generational, mobile, national).

% Must implement laws as interpreted by the judiciary, even if it disagrees with the interpretation. Its policy agenda can be constrained by judicial rulings, but it also benefits from the stability and finality of judicial decisions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, observer,
    institutional, biographical, constrained, national).

% Analyze the implications of judicial supremacy, debating its historical origins, democratic legitimacy, and practical effects on governance. Their work informs public discourse and legal education but does not directly alter the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative arbiter for constitutional disputes, ensuring a consistent and stable interpretation of the basic law across different political cycles and preventing legislative overreach.
% TRANSFER_FUNCTION: Transfers ultimate decision-making power on constitutional matters from elected political branches to the unelected judiciary, along with the associated institutional prestige and influence.
% ABSENT_VOICES: Advocates for parliamentary sovereignty and popular constitutionalism are structurally marginalized in this framework; they would argue for greater democratic control over constitutional meaning but are excluded from the final interpretive authority.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, the legislative and executive branches would immediately assert greater interpretive authority, leading to potential constitutional instability, inter-branch conflict over legal meaning, and a shift towards more politically driven constitutional evolution.
% FOUNDING_PROBLEM: To prevent legislative tyranny and protect fundamental rights by establishing an independent body to ensure fidelity to a higher law, insulating constitutional principles from transient political majorities.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and legal profession assert the problem is live, citing the need for rights protection and constitutional stability. Elected legislatures and popular constitutionalists argue the problem is largely solved, and the arrangement now primarily serves to entrench judicial power, with corroboration from political scientists and public opinion surveys.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (finality and consistency in constitutional interpretation) but also involves significant asymmetric extraction. The judiciary and legal profession benefit from enhanced authority and influence, while the elected legislature and electoral majorities bear the cost of having their policy choices overridden. Active enforcement is required to maintain this interpretive hierarchy against political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary Rope for constitutional stability and rights protection. From the perspective of the legislature and electoral majorities, it can feel like a Snare, as their democratic will is suppressed by an unelected body. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legal profession are beneficiaries (low d) due to the institutional power and professional status derived from this authority. The elected legislature and electoral majorities are victims (high d) as their policy outcomes are subject to judicial veto. The executive branch and constitutional scholars are observers, with varying degrees of constraint on their actions or influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing legislative tyranny) is contested. While the problem of protecting rights remains, critics argue that judicial supremacy has accumulated power beyond its original mandate, becoming a source of extraction rather than pure coordination. The rising extractiveness and suppression over time in the measurements reflect this drift towards a more extractive function, suggesting a potential Mandatrophy where the original coordination function is increasingly overshadowed by institutional self-preservation and power accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_parliamentary_sovereignty,
    'Is judicial supremacy a necessary check on legislative power, or an undemocratic usurpation of interpretive authority that should reside with the elected legislature?',
    'Comparative analysis of constitutional systems: examine outcomes in systems with parliamentary sovereignty versus judicial supremacy regarding rights protection, legislative effectiveness, and democratic accountability.',
    'If parliamentary sovereignty is deemed more legitimate or effective, this reading''s claim to ''final authority'' would be reclassified as a Snare, as its coordination function would be seen as cover for institutional power. If judicial supremacy is validated, its Rope-like coordination function would be emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_parliamentary_sovereignty, conceptual, 'This constraint is one reading of the ''basic_law_interpretive_authority'' kernel. This omega addresses the core disagreement with the ''parliamentary_sovereignty_reading'' sibling, which asserts that elected legislatures retain final interpretive authority through democratic mandate.').

omega_variable(
    judicial_supremacy_vs_popular_constitutionalism,
    'Does constitutional meaning derive from specialized judicial expertise, or from ongoing democratic contestation and popular engagement?',
    'Empirical study of constitutional change: analyze whether significant constitutional shifts primarily originate from judicial decisions or from broader social movements and political processes.',
    'If popular constitutionalism is validated, the ''judicial_supremacy_reading'' would be reclassified as a Snare, as its claim to expertise-based finality would be seen as suppressing legitimate popular constitutional discourse. If judicial expertise is validated, its coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_popular_constitutionalism, empirical, 'This constraint is one reading of the ''basic_law_interpretive_authority'' kernel. This omega addresses the core disagreement with the ''popular_constitutionalism_reading'' sibling, which asserts that constitutional meaning emerges from ongoing democratic contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_process_efficiency).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, rights_protection_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_authority' kernel. Each reading represents a different structural claim about where final interpretive authority resides, leading to different extraction profiles and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
