% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Constitutional Text: Legislative Sovereignty Reading
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the 'legislative sovereignty' reading of a
 *   constitutional text, where parliament holds ultimate authority over
 *   constitutional meaning, with judicial review being advisory rather than
 *   final. Mechanisms like 'notwithstanding clauses' or simple legislative
 *   override ensure that the elected legislature can assert its
 *   interpretation. This reading prioritizes majoritarian democracy and
 *   legislative flexibility, but at the potential cost of minority rights
 *   protection. The constraint is claimed as a Rope from the perspective of
 *   its proponents, as it provides a clear framework for governance, but its
 *   operation involves active enforcement against judicial overreach and can
 *   lead to extraction from minority groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.45).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.6).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Text: Legislative Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '47bd138a-ff8e-464b-bff7-bd035348ec9b').
narrative_ontology:cs_kernel_codification('47bd138a-ff8e-464b-bff7-bd035348ec9b', fixed_text).
narrative_ontology:cs_authority_grounding('47bd138a-ff8e-464b-bff7-bd035348ec9b', lineage).
narrative_ontology:cs_interpretation_layer_present('47bd138a-ff8e-464b-bff7-bd035348ec9b').
narrative_ontology:cs_reading_relation('47bd138a-ff8e-464b-bff7-bd035348ec9b', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('47bd138a-ff8e-464b-bff7-bd035348ec9b', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('47bd138a-ff8e-464b-bff7-bd035348ec9b', foundational, legislative_supremacy_principle).
narrative_ontology:cs_axiom_status(legislative_supremacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('47bd138a-ff8e-464b-bff7-bd035348ec9b', legislative_supremacy_principle, conventional).
narrative_ontology:cs_axiom('47bd138a-ff8e-464b-bff7-bd035348ec9b', foundational, democratic_accountability_priority).
narrative_ontology:cs_axiom_status(democratic_accountability_priority, holdable).
narrative_ontology:cs_axiom_grounding('47bd138a-ff8e-464b-bff7-bd035348ec9b', democratic_accountability_priority, deontological).
narrative_ontology:cs_reference_frame('47bd138a-ff8e-464b-bff7-bd035348ec9b', parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('47bd138a-ff8e-464b-bff7-bd035348ec9b', contemporary_rights_charter_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('47bd138a-ff8e-464b-bff7-bd035348ec9b', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majority_will).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme interpreter of the constitution, the legislature has the final say on constitutional meaning, often through mechanisms like notwithstanding clauses. This allows it to enact laws reflecting the majority will, even if they conflict with judicial interpretations.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Courts provide advisory opinions on constitutional matters and review legislation for compliance, but their interpretations are not final. Their role is to inform the legislature, not to override its ultimate authority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, courts, observer,
    institutional, generational, constrained, national).

% The legislative sovereignty reading ensures that the will of the elected majority can be directly translated into law, reflecting current public sentiment and policy preferences without being unduly constrained by judicial review.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majority_will, beneficiary,
    organized, biographical, mobile, national).

% Minority rights advocates bear the cost of this reading, as their protections can be overridden by legislative action. They rely on political processes and public opinion rather than judicial enforcement for their claims.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates, payer,
    powerless, generational, trapped, national).

% Analyze the implications of legislative supremacy for constitutional stability, democratic accountability, and rights protection. They contribute to the ongoing debate about the proper balance of power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, final arbiter of constitutional meaning, ensuring that legislative action can proceed with certainty and reflect the current democratic mandate, avoiding perpetual judicial deadlock.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the judiciary to the legislature, thereby empowering the elected majority to define constitutional boundaries.
% ABSENT_VOICES: Advocates for strong judicial review and entrenched minority rights, who would argue that legislative supremacy risks tyranny of the majority and undermines fundamental protections, are structurally marginalized in this framework.
% DISAPPEARANCE_RATIONALE: If legislative sovereignty vanished, the constitutional landscape would immediately shift towards judicial supremacy or popular sovereignty, leading to a re-evaluation of legislative powers, judicial roles, and the enforceability of rights. The balance of power would fundamentally reorganize.
% FOUNDING_PROBLEM: To ensure that the elected representatives of the people have the final say in governing, preventing an unelected judiciary from thwarting the democratic will and ensuring flexibility in constitutional interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and some legal scholars, outside the legislature itself, corroborate that the tension between democratic accountability and judicial review remains a live problem in many constitutional systems, justifying mechanisms for legislative finality.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the potential for the majority to impose its will on minorities, but not necessarily a pure rent-seeking mechanism. Suppression (0.6) is present as judicial alternatives are actively constrained, and minority voices may find their avenues for redress limited. The theater ratio (0.1) is low, as the legislative supremacy is generally a functional aspect of the constitutional system, not merely performative. Accessibility collapse (0.7) is relatively high because the ultimate interpretive authority is concentrated, limiting alternative paths for constitutional interpretation. Resistance (0.3) is moderate, as there is ongoing debate and advocacy for stronger judicial review, but it does not fundamentally challenge the legislative's final say within this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and the majority, this is a legitimate and necessary coordination mechanism for democratic governance. From the perspective of minority rights advocates, it can be experienced as a snare, as their protections are vulnerable to legislative override. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and the majority will are clear beneficiaries, as the constraint empowers them to shape constitutional meaning. Minority rights advocates are the primary victims, as their claims can be overridden. Courts act as observers or advisors, their directionality is near symmetric as they perform their function but do not hold ultimate power. Constitutional scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_power_legitimacy,
    'Is the balance of power established by legislative sovereignty genuinely democratic and legitimate, or does it risk majoritarian tyranny?',
    'Comparative analysis of constitutional outcomes in systems with varying degrees of legislative supremacy, focusing on long-term stability, rights protection, and public trust.',
    'If it consistently leads to erosion of rights or instability, the ''rope'' classification would shift towards ''tangled_rope'' or ''snare'' for minority seats; if it proves robust and adaptable, the ''rope'' classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_power_legitimacy, conceptual, 'Assessing the normative implications of legislative supremacy.').

omega_variable(
    notwithstanding_clause_use_frequency,
    'How frequently are ''notwithstanding clauses'' or similar override mechanisms actually invoked, and what is the political cost of their use?',
    'Empirical study of legislative history and political discourse surrounding the use of override clauses in relevant jurisdictions.',
    'Frequent, low-cost use would indicate higher effective extractiveness and suppression, pushing the classification towards ''snare'' for minority seats. Infrequent, high-cost use would suggest a more constrained legislative power, reinforcing the ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notwithstanding_clause_use_frequency, empirical, 'Empirical frequency and political cost of legislative overrides.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(cons_tr_t1930, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1930, 0.09).
narrative_ontology:measurement(cons_tr_t1960, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(cons_be_t1930, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1930, 0.42).
narrative_ontology:measurement(cons_be_t1960, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(cons_be_t1990, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1990, 0.47).
narrative_ontology:measurement(cons_be_t2024, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(cons_su_t1930, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1930, 0.58).
narrative_ontology:measurement(cons_su_t1960, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(cons_su_t2024, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel, each representing a different locus of ultimate constitutional authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
