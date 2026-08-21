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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint represents a reading of constitutional text where the
 *   legislature holds ultimate authority over constitutional meaning, with
 *   judicial review being advisory rather than final. This is a common
 *   feature in parliamentary systems with 'notwithstanding' clauses or
 *   similar legislative override mechanisms. The reading prioritizes
 *   majoritarian democracy and legislative accountability over judicial
 *   entrenchment of rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.4).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.3).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Text: Legislative Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'a7b7a8ef-3a6a-40f1-97cc-eef1615fd758').
narrative_ontology:cs_kernel_codification('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', fixed_text).
narrative_ontology:cs_authority_grounding('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', lineage).
narrative_ontology:cs_interpretation_layer_present('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758').
narrative_ontology:cs_reading_relation('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', foundational, legislative_supremacy_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', legislative_supremacy_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', foundational, democratic_accountability_trumps_judicial_review).
narrative_ontology:cs_axiom_status(democratic_accountability_trumps_judicial_review, holdable).
narrative_ontology:cs_axiom_grounding('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', democratic_accountability_trumps_judicial_review, deontological).
narrative_ontology:cs_reference_frame('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', contemporary_rights_charter_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('a7b7a8ef-3a6a-40f1-97cc-eef1615fd758', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_will).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme body, the legislature has the final say on constitutional meaning, often through mechanisms like notwithstanding clauses or simple override. It benefits from direct implementation of majoritarian will.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Courts provide advisory opinions on constitutional matters but their interpretations can be overridden by the legislature. Their role is to interpret and apply law, but not to be the final arbiter of constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judiciary, observer,
    institutional, generational, constrained, national).

% The collective preference of the majority of citizens, which is directly translated into law by the supreme legislature. This reading ensures that the will of the majority is paramount in constitutional interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_will, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_non_agent(constitutional_text__legislative_sovereignty_reading, majoritarian_will).

% Groups and individuals advocating for the protection of minority rights, which may be vulnerable to legislative override under a system of parliamentary supremacy. They bear the cost of potential legislative actions that may infringe upon these rights.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates, payer,
    moderate, generational, constrained, national).

% Academics and experts who analyze and debate the implications of different constitutional interpretations. They observe the practical effects of legislative supremacy on constitutional development and rights protection.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, final authority for constitutional interpretation, preventing deadlock between branches and ensuring that the democratic will, as expressed through the legislature, can be enacted.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the judiciary to the legislature, ensuring that legislative decisions, even those impacting constitutional matters, are final.
% ABSENT_VOICES: Advocates for strong judicial review and entrenched minority rights, who would argue that legislative supremacy risks tyranny of the majority and undermines fundamental protections. Their arguments are often heard but not given final authority.
% DISAPPEARANCE_RATIONALE: If legislative sovereignty vanished, the constitutional system would immediately face an interpretive vacuum, leading to potential judicial activism, legislative paralysis, and a fundamental reordering of power dynamics between branches of government.
% FOUNDING_PROBLEM: To ensure democratic accountability and prevent an unelected judiciary from thwarting the will of the people, while still providing a mechanism for constitutional review.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and historical analyses of parliamentary systems corroborate that this reading addresses the tension between judicial power and democratic accountability, even if the balance remains contested by legal scholars and rights advocates.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.4) as it primarily extracts from minority rights advocates by making their protections vulnerable to legislative override, but it also provides a clear, democratically accountable path for constitutional evolution. Suppression is low (0.3) because while judicial alternatives are suppressed, the legislative process itself is open to public participation and debate. Theater ratio is low (0.1) as the legislative supremacy is a genuine, functional aspect of the constitutional design, not merely performative. The claimed type is 'rope' because, from this reading's perspective, it coordinates democratic governance effectively, even if it entails costs for some parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature, this is a legitimate and necessary coordination mechanism for democratic governance. From the perspective of minority rights advocates, it can be seen as an extractive mechanism that undermines fundamental protections. The engine's per-seat classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and majoritarian will are clear beneficiaries, as this reading empowers them. Minority rights advocates are the primary payers, as their protections are less secure. The judiciary acts as an observer, providing input but not final decisions. Constitutional scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rigidity_vs_flexibility_tradeoff,
    'Is the flexibility afforded by legislative sovereignty a net benefit for constitutional evolution, or does it lead to insufficient protection for fundamental rights?',
    'Comparative analysis of constitutional stability and rights protection outcomes in systems with and without legislative override mechanisms over long historical periods.',
    'If flexibility is a net benefit, this reading''s ''rope'' classification is strengthened. If it leads to systemic rights erosion, the ''extractiveness'' metric might be re-evaluated upwards, potentially shifting the classification towards ''tangled_rope'' for minority rights seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_vs_flexibility_tradeoff, empirical, 'Assessing the long-term consequences of legislative supremacy on constitutional integrity and rights.').

omega_variable(
    democratic_accountability_vs_tyranny_of_majority,
    'At what point does the pursuit of democratic accountability through legislative supremacy cross into the ''tyranny of the majority''?',
    'Conceptual analysis of historical cases where legislative overrides have been used, combined with normative philosophical arguments about the limits of majoritarian rule.',
    'If a clear threshold for ''tyranny'' is identified, and this reading''s operation frequently crosses it, the ''suppression'' metric for minority rights advocates would be re-evaluated as higher, potentially leading to a ''snare'' classification for those seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_accountability_vs_tyranny_of_majority, conceptual, 'Defining the boundary between legitimate majoritarian rule and oppressive majoritarianism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cons_su_t50, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel, each representing a different allocation of ultimate interpretive authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
