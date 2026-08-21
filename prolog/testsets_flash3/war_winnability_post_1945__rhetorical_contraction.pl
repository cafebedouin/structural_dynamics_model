% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear War Winnability: Rhetorical Contraction
 *   domain: strategic_studies/nuclear_deterrence_theory/international_relations
 *
 * SUMMARY:
 *   This constraint describes the post-1945 phenomenon where the concept of
 *   'winnability' in nuclear war became a rhetorical taboo in public
 *   discourse, while simultaneously remaining an active, if constrained,
 *   subject of classified strategic and operational planning. It's a
 *   dual-layer contraction: public space for discussion shrank, but strategic
 *   space for planning persisted. This story is one reading of the
 *   'war_winnability_post_1945' kernel, focusing on the
 *   'rhetorical_contraction' aspect.
 *
 * KEY AGENTS:
 *   - strategic_planners: Agenda setter (institutional/identity_locked) — maintains operational flexibility.
 *   - political_leadership: Beneficiary (institutional/constrained) — manages public perception.
 *   - democratic_oversight: Payer (organized/constrained) — bears cost of reduced transparency.
 *   - public_discourse: Payer (powerless/trapped) — constrained by rhetorical taboo.
 *   - nuclear_deterrence_theorists: Analytical observer (analytical/analytical) — documents the gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear War Winnability: Rhetorical Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence_theory/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '1bce2b57-a332-4323-bd89-699d5ad9c19a').
narrative_ontology:cs_kernel_codification('1bce2b57-a332-4323-bd89-699d5ad9c19a', distributed).
narrative_ontology:cs_authority_grounding('1bce2b57-a332-4323-bd89-699d5ad9c19a', extraction).
narrative_ontology:cs_interpretation_layer_present('1bce2b57-a332-4323-bd89-699d5ad9c19a').
narrative_ontology:cs_reading_relation('1bce2b57-a332-4323-bd89-699d5ad9c19a', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('1bce2b57-a332-4323-bd89-699d5ad9c19a', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_axiom('1bce2b57-a332-4323-bd89-699d5ad9c19a', foundational, rhetorical_taboo_is_strategic_tool).
narrative_ontology:cs_axiom_status(rhetorical_taboo_is_strategic_tool, holdable).
narrative_ontology:cs_axiom_grounding('1bce2b57-a332-4323-bd89-699d5ad9c19a', rhetorical_taboo_is_strategic_tool, instrumental).
narrative_ontology:cs_axiom('1bce2b57-a332-4323-bd89-699d5ad9c19a', foundational, operational_flexibility_is_paramount).
narrative_ontology:cs_axiom_status(operational_flexibility_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('1bce2b57-a332-4323-bd89-699d5ad9c19a', operational_flexibility_is_paramount, empirically_contingent).
narrative_ontology:cs_reference_frame('1bce2b57-a332-4323-bd89-699d5ad9c19a', post_hiroshima_strategic_ambiguity).
narrative_ontology:cs_drift_state('1bce2b57-a332-4323-bd89-699d5ad9c19a', contemporary_public_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bce2b57-a332-4323-bd89-699d5ad9c19a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, political_leadership).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, public_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain operational plans for nuclear war scenarios, including concepts of 'victory' or 'prevailing' under various conditions. They benefit from the rhetorical taboo as it shields their work from public scrutiny and allows for strategic flexibility without public debate.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the public perception that nuclear war is unwinnable, which reinforces deterrence and avoids difficult public discussions about strategic realities. Simultaneously, they retain the option of operational planning for winnability in classified contexts.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, political_leadership, beneficiary,
    institutional, biographical, constrained, national).

% Bears the cost of reduced transparency and accountability regarding nuclear strategy. The rhetorical taboo makes it difficult to scrutinize or challenge operational plans for winnability, limiting public and legislative input.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight, payer,
    organized, biographical, constrained, national).

% Is constrained by the rhetorical taboo, which limits the range of acceptable discussion about nuclear war. It is difficult to articulate or debate concepts of winnability without being seen as irresponsible or dangerous, even if such concepts are part of actual strategic planning.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public_discourse, payer,
    powerless, immediate, trapped, global).

% Analyze the gap between public rhetoric and operational reality, documenting the persistence of winnability concepts in strategic planning despite their discursive suppression. They are not directly subject to the taboo but observe its effects.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_deterrence_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public perception of nuclear war as unthinkable, reinforcing deterrence and managing public anxiety, while simultaneously allowing for the continuation of strategic planning for various conflict outcomes, including 'victory'.
% TRANSFER_FUNCTION: Transfers rhetorical flexibility and strategic ambiguity to political and military leadership, at the cost of transparency and democratic accountability for nuclear policy.
% ABSENT_VOICES: Advocates for greater transparency in nuclear strategy, and those who believe open debate about winnability is necessary for effective deterrence, are marginalized by the rhetorical taboo. They are excluded from mainstream public discourse on the topic.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished overnight, public discourse on nuclear war would become more complex and potentially alarming. Strategic planners would face increased scrutiny, and political leaders would be forced to reconcile public rhetoric with operational realities, leading to significant shifts in policy and public engagement.
% FOUNDING_PROBLEM: The problem of managing public fear and maintaining deterrence credibility after the advent of nuclear weapons, where the destructive potential made traditional notions of 'victory' problematic.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Cold War, political scientists, and independent strategic analysts corroborate that managing the public perception of nuclear war's consequences remains a live problem for nuclear-armed states, distinct from the technical challenges of deterrence itself.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it serves a genuine coordination function (managing public fear and reinforcing deterrence) while simultaneously enabling asymmetric extraction (strategic planners and political leadership gain flexibility and reduced accountability at the expense of democratic oversight and public discourse). Extractiveness is high (0.65) due to the significant cost to transparency and public agency. Suppression is high (0.78) because the rhetorical taboo is actively maintained through political messaging, media framing, and classification, making it difficult to challenge. Theater ratio is moderate (0.55) as the public performance of 'unwinnability' masks the underlying operational reality of 'constrained winnability' planning.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners and political leadership experience this as a necessary, if complex, coordination mechanism for national security. Democratic oversight and public discourse, however, experience it as a form of epistemic capture and a barrier to informed debate, leading to a significant divergence in perceived constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and political leadership are beneficiaries (low d) as they gain operational flexibility and public management. Democratic oversight and public discourse are targets (high d) as they bear the costs of reduced transparency and constrained debate. The 'identity_locked' exit for strategic planners reflects their professional identity being deeply intertwined with maintaining strategic options, even if publicly unsayable.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Snare by acknowledging the genuine deterrence and public anxiety management functions. However, it also prevents mislabeling it as a pure Rope by highlighting the asymmetric extraction and active suppression involved in maintaining the rhetorical-operational gap. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating an ongoing tension between original intent and current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_rhetorical_gap_size,
    'What is the precise quantitative and qualitative gap between classified operational planning for winnability and public rhetoric of unwinnability?',
    'Declassification of historical strategic documents, expert testimony from former planners, and comparative analysis of public statements versus declassified doctrine.',
    'A larger gap would strengthen the ''extraction'' component of the constraint, indicating greater opacity and less accountability. A smaller gap would suggest a more integrated, albeit complex, strategic communication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_rhetorical_gap_size, empirical, 'Quantifying the divergence between public and private nuclear war discourse.').

omega_variable(
    necessity_of_taboo_for_deterrence,
    'Is the rhetorical taboo on nuclear war winnability genuinely necessary for maintaining effective deterrence, or does it primarily serve to shield strategic planning from public scrutiny?',
    'Comparative analysis of deterrence stability in states with varying levels of transparency regarding nuclear war planning, or theoretical modeling of deterrence with and without such a taboo.',
    'If the taboo is found to be non-essential for deterrence, the ''coordination'' function of the constraint would be significantly weakened, pushing its classification closer to a Snare. If essential, the ''coordination'' function would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_taboo_for_deterrence, conceptual, 'Assessing the functional necessity of the rhetorical taboo for deterrence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of winnability discourse structural (e.g., classification, media gatekeeping) or internalized (e.g., self-censorship by academics, public aversion to the topic)?',
    'Post-taboo-removal discourse trajectory: if suppression persists after formal barriers are removed, reclassify as partially internalized. Content analysis of academic and public discourse for evidence of self-censorship.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them. If purely structural, removing formal barriers would lead to a rapid expansion of discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for winnability discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.4).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
