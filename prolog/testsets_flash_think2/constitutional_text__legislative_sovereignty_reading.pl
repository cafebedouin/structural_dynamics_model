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
 *   human_readable: Constitutional Legislative Sovereignty (Reading)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'legislative sovereignty' reading
 *   of a constitutional text, where parliament is established as supreme, and
 *   courts provide advisory opinions but the legislature retains the final
 *   say on constitutional meaning, often through mechanisms like
 *   'notwithstanding' clauses. This reading prioritizes majoritarian will and
 *   legislative efficiency over judicial finality in rights protection. The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function for governance (clear law-making authority) but
 *   with significant asymmetric extraction from minority rights and judicial
 *   authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.68).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.75).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Legislative Sovereignty (Reading)").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '130665ec-e077-4412-9fce-724b3d9ceb33').
narrative_ontology:cs_kernel_codification('130665ec-e077-4412-9fce-724b3d9ceb33', fixed_text).
narrative_ontology:cs_authority_grounding('130665ec-e077-4412-9fce-724b3d9ceb33', lineage).
narrative_ontology:cs_interpretation_layer_present('130665ec-e077-4412-9fce-724b3d9ceb33').
narrative_ontology:cs_reading_relation('130665ec-e077-4412-9fce-724b3d9ceb33', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('130665ec-e077-4412-9fce-724b3d9ceb33', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('130665ec-e077-4412-9fce-724b3d9ceb33', foundational, legislative_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(legislative_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('130665ec-e077-4412-9fce-724b3d9ceb33', legislative_finality_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('130665ec-e077-4412-9fce-724b3d9ceb33', secondary, majority_rule_as_constitutional_principle).
narrative_ontology:cs_axiom_status(majority_rule_as_constitutional_principle, holdable).
narrative_ontology:cs_axiom_grounding('130665ec-e077-4412-9fce-724b3d9ceb33', majority_rule_as_constitutional_principle, conventional).
narrative_ontology:cs_reference_frame('130665ec-e077-4412-9fce-724b3d9ceb33', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('130665ec-e077-4412-9fce-724b3d9ceb33', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('130665ec-e077-4412-9fce-724b3d9ceb33', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judicial_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme body, it has the final say on constitutional meaning, often through 'notwithstanding' clauses or simple override mechanisms. It benefits from the ability to enact its policy agenda without judicial veto, reflecting the will of the majority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the ability of its elected representatives to implement policies and constitutional interpretations that align with its preferences, unhindered by judicial review. Its will is paramount in this system.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Provides constitutional advice and interpretation, but its rulings are ultimately subject to legislative override. It bears the cost of its interpretive authority being subordinated to the legislature, potentially seeing its efforts to protect rights undone.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Their efforts to protect minority rights through judicial means are vulnerable to legislative override. They bear the cost of a system where majoritarian will can legally diminish protections for vulnerable groups, with limited recourse.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates, payer,
    organized, generational, constrained, national).

% Analyze the functioning and implications of legislative sovereignty, comparing it with other constitutional models. They observe the practical effects on governance, rights, and the balance of power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear and final authority for constitutional interpretation and law-making, ensuring governmental action can proceed efficiently and reflect the will of elected representatives, avoiding perpetual judicial deadlock.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the judiciary to the legislature, effectively empowering the elected majority to define constitutional meaning and limits, potentially at the expense of minority protections.
% ABSENT_VOICES: Proponents of strong judicial review and entrenched minority rights would object, arguing for judicial finality in protecting fundamental rights against majoritarian overreach. Their arguments are heard in public discourse but lack structural leverage within this constitutional framework.
% DISAPPEARANCE_RATIONALE: If legislative sovereignty vanished, the constitutional order would immediately become ambiguous, leading to judicial activism, legislative paralysis, or a shift towards popular sovereignty mechanisms, fundamentally altering the balance of power and the process of law-making.
% FOUNDING_PROBLEM: To establish a clear and efficient mechanism for governance and law-making, ensuring that the will of the elected representatives could ultimately prevail in constitutional matters, avoiding judicial obstruction and ensuring democratic accountability.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and legal historians outside the legislature corroborate that the problem of balancing legislative will with constitutional limits remains central to the system's design, even if its resolution in favor of the legislature is contested by some.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the system allows the majority to impose its will, potentially at the cost of minority rights, by overriding judicial interpretations. Suppression is also high (0.75) as it structurally suppresses judicial finality and alternative avenues for rights protection. The accessibility collapse is substantial (0.80) because the legislative override mechanism effectively closes off judicial review as a final alternative. Resistance is moderate (0.55) from those advocating for stronger judicial review and minority protections. Theater ratio is low (0.20) as the system is largely functional in its stated purpose, even if that purpose is extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and the majority, this system is a legitimate expression of democratic will and efficient governance. From the perspective of the judiciary and minority rights advocates, it is a mechanism that can enable majoritarian tyranny and undermine fundamental protections. The engine's per-seat classification will reflect this divergence based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and the majority electorate are clear beneficiaries (low directionality) as they gain the power of final constitutional interpretation. The judicial branch and minority rights advocates are targets (high directionality) as their authority and protections are subordinated to legislative will. The system is designed to channel power to the legislature, making it a structural beneficiary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_vs_judicial_finality,
    'Is the constitutional text''s grant of legislative supremacy an explicit, unambiguous statement, or is it an interpretation that downplays judicial review''s implicit authority?',
    'Detailed textual analysis of the constitutional document''s drafting history, original intent, and subsequent amendments, alongside comparative analysis with other constitutional texts.',
    'If the text is ambiguous, the ''legislative sovereignty'' reading is a conceptual choice, not a textual imperative, potentially weakening its claim to naturalness. If explicit, it reinforces the structural basis of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_vs_judicial_finality, conceptual, 'Ambiguity in the constitutional text regarding final interpretive authority.').

omega_variable(
    minority_protection_efficacy,
    'How effective are non-judicial mechanisms (e.g., legislative supermajorities, political conventions, international treaties) at protecting minority rights in a system of legislative sovereignty?',
    'Empirical study of legislative outcomes, historical analysis of minority rights violations, and comparative legal analysis across jurisdictions with similar constitutional structures.',
    'If non-judicial mechanisms are consistently ineffective, the extraction from minority rights is higher and less mitigated than this reading might imply, pushing the classification closer to a Snare. If effective, it supports the ''tangled_rope'' classification by demonstrating some coordination for all parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_efficacy, empirical, 'Effectiveness of non-judicial minority rights protections.').

omega_variable(
    reading_impact_on_judicial_supremacy,
    'How would the structural adoption of the ''judicial_supremacy_reading'' alter the balance of power and rights protection compared to this ''legislative_sovereignty_reading''?',
    'Counterfactual analysis and comparative legal studies of jurisdictions where judicial supremacy is entrenched, focusing on legislative deference, judicial activism, and the scope of rights protection.',
    'The ''judicial_supremacy_reading'' would shift the beneficiary/victim structure, likely making the judicial branch a beneficiary and the legislature a payer, with potentially enhanced protections for minority rights. This highlights the zero-sum nature of the interpretive contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_impact_on_judicial_supremacy, conceptual, 'Structural changes under a judicial supremacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(cons_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(cons_su_t50, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
