% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story analyzes the supermajority threshold from the
 *   'minoritarian veto' reading, where it functions as a mechanism for
 *   blocking minorities to entrench the status quo against majoritarian will.
 *   The constraint is claimed as a Snare because its primary effect, from
 *   this reading's perspective, is extraction from contemporary majorities
 *   and the suppression of reform, rather than genuine coordination. The
 *   metrics reflect this extractive and suppressive reality, which has
 *   intensified over time as political polarization has increased and the gap
 *   between majoritarian preferences and constitutional outcomes has widened.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'ac53b397-0e89-4188-b49d-15f5307232a1').
narrative_ontology:cs_kernel_codification('ac53b397-0e89-4188-b49d-15f5307232a1', formalized).
narrative_ontology:cs_authority_grounding('ac53b397-0e89-4188-b49d-15f5307232a1', lineage).
narrative_ontology:cs_interpretation_layer_present('ac53b397-0e89-4188-b49d-15f5307232a1').
narrative_ontology:cs_reading_relation('ac53b397-0e89-4188-b49d-15f5307232a1', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac53b397-0e89-4188-b49d-15f5307232a1', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('ac53b397-0e89-4188-b49d-15f5307232a1', foundational, minority_veto_is_anti_democratic).
narrative_ontology:cs_axiom_status(minority_veto_is_anti_democratic, holdable).
narrative_ontology:cs_axiom_grounding('ac53b397-0e89-4188-b49d-15f5307232a1', minority_veto_is_anti_democratic, deontological).
narrative_ontology:cs_axiom('ac53b397-0e89-4188-b49d-15f5307232a1', secondary, historical_privilege_entrenched).
narrative_ontology:cs_axiom_status(historical_privilege_entrenched, holdable).
narrative_ontology:cs_axiom_grounding('ac53b397-0e89-4188-b49d-15f5307232a1', historical_privilege_entrenched, empirically_contingent).
narrative_ontology:cs_reference_frame('ac53b397-0e89-4188-b49d-15f5307232a1', democratic_majoritarianism).
narrative_ontology:cs_drift_state('ac53b397-0e89-4188-b49d-15f5307232a1', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac53b397-0e89-4188-b49d-15f5307232a1', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups or factions that benefit from the existing distribution of power and resources, whose interests are protected by the supermajority threshold. They actively leverage the threshold to block reforms that would challenge their position, often framing their actions as defending constitutional principles.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, agenda_setter,
    institutional, generational, identity_locked, national).

% Various groups, industries, or regions that benefit from specific policies or arrangements enshrined in the status quo. They may not directly set the agenda but exert influence to ensure the supermajority threshold remains an effective barrier to change that would disrupt their advantages.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, constrained, national).

% The collective will of the majority of citizens, expressed through elections and public opinion, which is repeatedly frustrated by the inability to enact desired reforms due to the supermajority requirement. They bear the cost of delayed or blocked progress on issues they deem critical.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, constrained, national).

% Activists, political movements, and legislators pushing for changes to the constitutional or legal framework. They expend significant effort and resources attempting to overcome the supermajority barrier, often facing repeated failure and delegitimization by those who benefit from the status quo.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, reform_advocates, excluded).

% Academics and legal experts who analyze the function and effects of supermajority rules. They provide critical commentary on whether these thresholds genuinely serve their stated purpose or have become tools for entrenchment, often highlighting the gap between theory and practice.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated coordination function is to ensure that fundamental constitutional changes reflect a broad, deep, and enduring consensus, preventing transient majorities from undermining long-term stability or minority rights.
% TRANSFER_FUNCTION: This arrangement transfers the power to block constitutional or fundamental legal changes from a simple majority to a minority faction, effectively converting historical privilege or entrenched interests into a permanent veto over majoritarian will.
% ABSENT_VOICES: Future generations, who are bound by constitutional structures they had no hand in creating and cannot easily alter. Disenfranchised or marginalized groups whose interests are consistently overridden by entrenched minorities, and who lack the political power to form a supermajority.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, the political landscape would fundamentally rearrange. Majoritarian reforms, currently blocked, would likely pass, leading to significant shifts in power, resource allocation, and social policy. The entrenched elites would lose their veto power, and the pace of constitutional evolution would accelerate.
% FOUNDING_PROBLEM: The supermajority threshold was originally designed to protect fundamental rights, prevent tyranny of the majority, and ensure constitutional stability by requiring broad agreement for foundational changes.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (often entrenched elites) argue the founding problem of majoritarian overreach remains live, citing the need for stability. Critics (contemporary majorities, reform advocates, many constitutional scholars) argue the problem has shifted: the threshold now entrenches historical injustices and blocks necessary adaptation, converting a safeguard into a snare. This is corroborated by empirical studies of legislative gridlock and public opinion data showing broad support for reforms that cannot pass.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant cost borne by majorities in terms of blocked reforms and perpetuated inequalities. Suppression (0.78) is high because the threshold actively prevents alternatives to the status quo from emerging through normal democratic processes. The theater ratio (0.45) indicates that while the 'safeguard' narrative persists, a substantial portion of the constraint's operation is performative maintenance of minority power. Accessibility collapse (0.70) is high because the threshold makes constitutional change extremely difficult, effectively collapsing many reform alternatives. Resistance (0.75) is also high, as majoritarian movements and reform advocates actively push against this entrenched barrier. The temporal measurements show a clear trend of increasing extractiveness, suppression, and theatricality, suggesting a drift from its original stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrenched elites and status quo beneficiaries, the supermajority threshold is a legitimate safeguard (a Rope or even a Mountain of stability). However, from the perspective of contemporary majorities and reform advocates, it operates as a Snare, extracting their political agency and suppressing their will. The engine's computation of per-seat classifications will highlight this divergence, with beneficiaries experiencing it as a protective mechanism and victims experiencing it as an extractive barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are clear beneficiaries (low d) as the constraint directly protects their interests. Contemporary majorities and reform advocates are targets (high d) as they bear the costs of blocked reforms and suppressed political will. Constitutional scholars are observers (d=0.5) as they analyze its effects without direct benefit or cost. The 'identity_locked' exit for entrenched elites reflects their deep investment in the existing constitutional order, which is integral to their power and self-conception.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the supermajority threshold has undergone mandatrophy, where its original mandate (safeguarding consensus) has atrophied, and its function has shifted to entrenching minority power. The classification as a Snare, despite its historical framing as a Rope or Mountain, directly addresses this mandatrophy by focusing on its current structural operation and identifiable victims. The high extractiveness and suppression, coupled with the contested founding problem status, prevent mislabeling it as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safeguard_vs_entrenchment,
    'Is the supermajority threshold primarily functioning as a legitimate safeguard for fundamental rights and stability, or as a tool for minoritarian entrenchment and the blocking of necessary reforms?',
    'Empirical analysis of legislative outcomes over time, comparing the nature of blocked reforms (e.g., protecting minority rights vs. preserving economic privilege) and the demographic composition of blocking minorities versus the broader population.',
    'If primarily entrenchment, the Snare classification is strongly validated. If it demonstrably protects vulnerable minorities from majoritarian overreach, a reclassification towards Tangled Rope or even Rope might be warranted, though the extraction from the majority would still need accounting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safeguard_vs_entrenchment, empirical, 'Distinguishing the actual function from the stated purpose of the supermajority rule.').

omega_variable(
    cost_of_stability_vs_reform,
    'What is the optimal balance between constitutional stability (cost of frequent change) and constitutional adaptability (cost of blocked reform), and where does the current supermajority threshold sit on this curve?',
    'Comparative constitutional studies, economic modeling of the costs of gridlock versus the benefits of timely policy adaptation, and public deliberation on the normative value of stability versus responsiveness.',
    'If the costs of blocked reform (e.g., social unrest, economic stagnation) significantly outweigh the benefits of stability, the high extractiveness of this Snare reading is further justified. If instability is shown to be a greater threat, the ''consensus_safeguard_reading'' gains more weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_stability_vs_reform, conceptual, 'Assessing the trade-offs between constitutional rigidity and flexibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t1950, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(supe_tr_t1965, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(supe_tr_t1980, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(supe_tr_t1995, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(supe_tr_t2010, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(supe_tr_t2024, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(supe_be_t1950, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(supe_be_t1965, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(supe_be_t1980, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(supe_be_t1995, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(supe_be_t2010, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(supe_be_t2024, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t1950, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(supe_su_t1965, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(supe_su_t1980, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(supe_su_t1995, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(supe_su_t2010, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(supe_su_t2024, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, legislative_gridlock).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, economic_redistribution_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'supermajority_threshold' kernel. It focuses on the threshold's function as a minoritarian veto, contrasting with the 'consensus_safeguard_reading' (which emphasizes stability) and the 'adaptive_gradient_reading' (which views it as a tunable tool).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
