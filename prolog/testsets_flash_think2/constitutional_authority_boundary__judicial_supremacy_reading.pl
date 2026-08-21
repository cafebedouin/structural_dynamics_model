% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'judicial supremacy' reading of
 *   the constitutional authority boundary kernel. It posits that the
 *   constitutional text establishes courts as the final, unchallengeable
 *   arbiters of all constitutional questions, with the power to invalidate
 *   legislative and executive acts without remedy. This reading is contested
 *   by alternative interpretations that advocate for distributed interpretive
 *   authority or legislative sovereignty. The high extractiveness and
 *   suppression reflect the judiciary's counter-majoritarian veto power and
 *   the foreclosure of alternative interpretive mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '738c9236-9e1f-4b70-a292-e7b2bdc2b394').
narrative_ontology:cs_kernel_codification('738c9236-9e1f-4b70-a292-e7b2bdc2b394', fixed_text).
narrative_ontology:cs_authority_grounding('738c9236-9e1f-4b70-a292-e7b2bdc2b394', lineage).
narrative_ontology:cs_interpretation_layer_present('738c9236-9e1f-4b70-a292-e7b2bdc2b394').
narrative_ontology:cs_reading_relation('738c9236-9e1f-4b70-a292-e7b2bdc2b394', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('738c9236-9e1f-4b70-a292-e7b2bdc2b394', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('738c9236-9e1f-4b70-a292-e7b2bdc2b394', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('738c9236-9e1f-4b70-a292-e7b2bdc2b394', judicial_finality_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('738c9236-9e1f-4b70-a292-e7b2bdc2b394', foundational, constitution_as_supreme_law).
narrative_ontology:cs_axiom_status(constitution_as_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('738c9236-9e1f-4b70-a292-e7b2bdc2b394', constitution_as_supreme_law, deontological).
narrative_ontology:cs_reference_frame('738c9236-9e1f-4b70-a292-e7b2bdc2b394', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('738c9236-9e1f-4b70-a292-e7b2bdc2b394', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('738c9236-9e1f-4b70-a292-e7b2bdc2b394', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, citizenry).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the final arbiter of constitutional questions, the judiciary invalidates legislative and executive acts, establishing its interpretive monopoly. It benefits from this authority by shaping legal and policy outcomes.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% The legislative branch creates laws, but its policy space is constrained by the judiciary's power of constitutional review. Its acts can be invalidated without direct legislative remedy.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% The executive branch implements laws and issues executive orders, but its actions are also subject to judicial review and potential invalidation, limiting its policy discretion.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Benefits from the stability and rights protection afforded by a final constitutional arbiter, but also bears the costs of potentially undemocratic policy outcomes when judicial decisions override popular will.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, citizenry, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, citizenry, payer).

% Analyze, interpret, and critique the doctrine of judicial supremacy, influencing legal education, public discourse, and potential future legal reforms.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% Argue for a system of co-equal interpretive authority among branches, where no single branch holds final say. Their alternative vision is structurally foreclosed by the judicial supremacy reading.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_advocates, excluded,
    organized, generational, constrained, national).

% Advocate for legislative sovereignty, where the elected legislature retains final authority over constitutional meaning. Their position is fundamentally incompatible with judicial supremacy and is structurally excluded.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative mechanism for resolving constitutional disputes, ensuring legal certainty, consistency, and stability across the branches of government and over time.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and the power to invalidate acts from the elected legislative and executive branches to the unelected judiciary, granting the judiciary significant control over policy and governance.
% ABSENT_VOICES: Advocates for coordinate construction and parliamentary primacy are structurally excluded from the operational framework of judicial supremacy. They would argue for distributed or legislative finality in constitutional interpretation, challenging the judiciary's exclusive claim.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the balance of power would fundamentally shift. Legislative and executive branches would likely assert their own final interpretive authority, leading to potential constitutional crises, legal uncertainty, and a reorganization of governance around different principles of inter-branch relations.
% FOUNDING_PROBLEM: To establish a stable and authoritative framework for constitutional governance, resolve inter-branch disputes, and protect fundamental rights from potential majoritarian overreach by elected branches.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and some legal scholars corroborate the ongoing necessity of judicial finality for constitutional stability and rights protection. However, critics (including political scientists, other legal scholars, and political actors) argue that the founding problem has been over-solved, leading to judicial overreach and an undemocratic concentration of power. Legislative hearing testimony and academic critiques from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high due to the judiciary's power to invalidate acts, effectively extracting policy space from elected branches. Suppression (0.75) is also high because this reading actively forecloses legislative or executive remedies and alternative interpretive frameworks. The theater ratio is low (0.15) because the function of judicial review is central and actively performed, not merely theatrical. Accessibility collapse is high (0.70) as it significantly limits the options for other branches to assert constitutional meaning. Resistance (0.55) is moderate, reflecting ongoing political and academic challenges to this doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this reading ensures constitutional fidelity and protects rights. From the perspective of the legislature and executive, it can be seen as an undemocratic usurpation of power. The engine's per-seat classification will reflect this divergence, with the judiciary computing as a beneficiary of a coordination mechanism, while the other branches compute as targets of an extractive one.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the primary beneficiary and agenda-setter, gaining interpretive monopoly rents and significant influence over policy. The legislature and executive branches are the primary payers, as their policy-making and implementation powers are constrained. The citizenry is both a beneficiary (constitutional stability, rights protection) and an indirect payer (through policy outcomes they may not support). Advocates for alternative readings are excluded, as their positions are structurally incompatible with judicial supremacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope helps to distinguish the genuine coordination function (resolving constitutional disputes) from the asymmetric extraction (judicial interpretive monopoly). It prevents mislabeling the constraint as a pure Rope (ignoring the extraction) or a pure Snare (ignoring the coordination function of dispute resolution). The high extractiveness and suppression, despite the coordination function, highlight the potential for power consolidation under this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_coordinate_construction,
    'Is judicial supremacy a necessary feature for constitutional stability and rights protection, or does it foreclose a more democratic and distributed model of constitutional interpretation (coordinate construction)?',
    'Comparative analysis of constitutional systems with different models of interpretive authority, assessing their stability, rights protection, and democratic accountability over time.',
    'If coordinate construction proves viable and more democratic, the extraction and suppression associated with judicial supremacy would be reclassified as unnecessary and illegitimate. If judicial supremacy is shown to be uniquely effective, its coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_coordinate_construction, conceptual, 'Ambiguity regarding the necessity and legitimacy of judicial interpretive finality.').

omega_variable(
    counter_majoritarian_difficulty_legitimacy,
    'To what extent does the counter-majoritarian difficulty (unelected judges overriding elected representatives) undermine the democratic legitimacy of judicial supremacy?',
    'Empirical studies on public trust in the judiciary versus other branches, and analysis of the responsiveness of judicial decisions to evolving societal norms and values.',
    'If democratic legitimacy is significantly undermined, the constraint''s effective extraction would be amplified due to the lack of consent from the governed. If legitimacy is robust, the extraction might be seen as a necessary cost for rights protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty_legitimacy, empirical, 'The democratic legitimacy of judicial review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t6, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(cons_tr_t12, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(cons_tr_t18, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t6, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(cons_be_t12, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(cons_be_t18, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t6, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(cons_su_t12, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(cons_su_t18, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_authority_boundary' kernel. Each reading represents a different structural claim about where final constitutional authority resides, leading to different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
