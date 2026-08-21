% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of 'All Men Are Created Equal'
 *   domain: Constitutional Law / Political Philosophy / American Studies
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the Declaration
 *   of Independence's phrase 'all men are created equal.' This reading
 *   interprets equality as a universal, evolving principle that demands
 *   iterative expansion to include all persons, regardless of the founders'
 *   original intent or limited application. It views the principle as a moral
 *   imperative that society must continually strive to fulfill, challenging
 *   existing inequalities and expanding the scope of rights and recognition.
 *   The constraint functions as a 'tangled rope' because it genuinely
 *   coordinates societal progress towards equality, but this coordination
 *   inherently involves extraction from those who benefit from existing
 *   hierarchies and resistance to change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.65).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.7).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "Constitutional Law / Political Philosophy / American Studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '7955f836-9ff7-4cc5-9c56-e4c02253d61a').
narrative_ontology:cs_kernel_codification('7955f836-9ff7-4cc5-9c56-e4c02253d61a', fixed_text).
narrative_ontology:cs_authority_grounding('7955f836-9ff7-4cc5-9c56-e4c02253d61a', lineage).
narrative_ontology:cs_interpretation_layer_present('7955f836-9ff7-4cc5-9c56-e4c02253d61a').
narrative_ontology:cs_reading_relation('7955f836-9ff7-4cc5-9c56-e4c02253d61a', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7955f836-9ff7-4cc5-9c56-e4c02253d61a', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('7955f836-9ff7-4cc5-9c56-e4c02253d61a', foundational, inherent_human_dignity).
narrative_ontology:cs_axiom_status(inherent_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('7955f836-9ff7-4cc5-9c56-e4c02253d61a', inherent_human_dignity, deontological).
narrative_ontology:cs_axiom('7955f836-9ff7-4cc5-9c56-e4c02253d61a', secondary, moral_arc_of_history_bends_towards_justice).
narrative_ontology:cs_axiom_status(moral_arc_of_history_bends_towards_justice, holdable).
narrative_ontology:cs_axiom_grounding('7955f836-9ff7-4cc5-9c56-e4c02253d61a', moral_arc_of_history_bends_towards_justice, conventional).
narrative_ontology:cs_reference_frame('7955f836-9ff7-4cc5-9c56-e4c02253d61a', post_enlightenment_universalism).
narrative_ontology:cs_drift_state('7955f836-9ff7-4cc5-9c56-e4c02253d61a', contemporary_social_justice_movements, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7955f836-9ff7-4cc5-9c56-e4c02253d61a', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, advocates_for_equality).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_resisting_equality_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, institutions_maintaining_inequality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups actively invoke the universalist principle to demand equal rights, protections, and opportunities. They are beneficiaries as the principle, when successfully applied, grants them status and access previously denied. Their exit options are constrained by the social and legal structures they seek to change.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    moderate, biographical, constrained, national).

% Legal scholars, activists, and social movements who champion the expansive interpretation of equality. They benefit from the principle's legitimacy as a tool for social change and legal reform. Their mobility comes from their ability to shift strategies and platforms, but they are constrained by the pace of societal acceptance.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, advocates_for_equality, beneficiary,
    organized, generational, mobile, national).

% Individuals and communities whose existing social hierarchies, privileges, or cultural norms are challenged by the expansion of equality. They bear the costs of adapting to new legal and social realities, potentially losing status or economic advantage. Their exit is constrained by their embeddedness in existing social structures.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_resisting_equality_expansion, payer,
    powerful, biographical, constrained, local).

% Legal, economic, or social institutions (e.g., discriminatory laws, segregated systems, biased hiring practices) that historically or currently perpetuate unequal status. They are targets of the universalist principle's enforcement and bear the costs of mandated reform or legal challenge. Their 'exit' is the dismantling of their unequal structures, which they resist.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, institutions_maintaining_inequality, payer,
    institutional, generational, trapped, national).

% The ultimate arbiter of constitutional meaning, including the scope of equality. Its rulings shape the interpretation and enforcement of the principle, driving or resisting its expansion. Its 'exit' is analytical, through reinterpretation or overturning precedent.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Academics and legal experts who analyze, debate, and theorize about the meaning and application of the equality principle. They provide intellectual frameworks that influence judicial and public understanding, but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate societal understanding and legal practice around an evolving, expansive definition of human equality, ensuring that new groups and contexts are brought under its protective umbrella.
% TRANSFER_FUNCTION: Transfers rights, recognition, and social status from those who previously held exclusive claims or benefited from their denial, to marginalized groups demanding inclusion. It also transfers the burden of proof and justification onto those who would maintain inequality.
% ABSENT_VOICES: Historically, the voices of enslaved people, women, indigenous populations, and other marginalized groups were absent from the founding discourse. Even today, those whose experiences fall outside dominant narratives may find their claims to equality unheard or dismissed.
% DISAPPEARANCE_RATIONALE: If the universalist reading of 'All Men Are Created Equal' vanished, the foundational legal and moral justification for civil rights, anti-discrimination laws, and social justice movements would collapse. Society would rapidly revert to more explicit hierarchies, and the legal system would lose its primary tool for challenging systemic inequality, leading to a profound reorganization of social and political life.
% FOUNDING_PROBLEM: The inherent contradiction of declaring universal equality while simultaneously upholding chattel slavery and denying rights to women and indigenous peoples. The universalist reading attempts to resolve this by prioritizing the universal language over the founders' limited intent.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars, and social justice advocates from outside the immediate beneficiaries of the principle's expansion consistently corroborate the ongoing tension between the ideal of universal equality and its incomplete realization in practice.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the expansion of equality often requires challenging entrenched power structures and redistributing resources or status, which is experienced as a cost by those who previously benefited from inequality. Suppression (0.70) is also moderate-high, reflecting the active legal and social enforcement required to overcome resistance to equality's expansion (e.g., through civil rights legislation, court rulings, and social pressure). Theater ratio (0.10) is low because this reading is genuinely about active, iterative expansion and not merely performative adherence; the principle is a live tool for change. Accessibility collapse (0.85) is high as the universalist ideal aims to make alternatives to equality (i.e., systemic inequality) morally and legally untenable. Resistance (0.75) is high, as the principle consistently meets opposition from groups and institutions whose privileges are challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups and advocates, this constraint is a vital 'rope' or 'scaffold' for justice and progress, offering a path to inclusion. From the perspective of groups and institutions resisting expansion, it is a 'snare' that extracts their traditional privileges and forces unwanted change. The Supreme Court, as agenda-setter, navigates these competing perspectives, with its rulings often reflecting a contested balance between expansion and restraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and advocates are clear beneficiaries, gaining rights and recognition. Groups and institutions resisting expansion are victims, as the constraint extracts their historical privileges and forces costly adaptation. The Supreme Court acts as an agenda-setter, mediating the application of the principle. Constitutional scholars observe and influence, but do not directly benefit or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled rope' prevents mislabeling the universalist reading as either a pure 'rope' (ignoring the extraction from those resisting change) or a pure 'snare' (ignoring its genuine coordination function in expanding rights). The iterative expansion of equality is a complex social coordination problem, but it is one that inherently challenges existing power dynamics, leading to asymmetric costs for those who benefit from the status quo. The 'live' status of the founding problem further reinforces that the constraint's mandate is active and contested, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_universalism_ambiguity,
    'What is the ultimate, non-contestable scope of ''universal'' equality? Does it extend to non-human entities, future generations, or artificial intelligences?',
    'Ongoing philosophical debate, scientific advancements in understanding consciousness, and future legal/social consensus.',
    'If the scope is definitively bounded, the constraint''s expansionary pressure would diminish. If it is found to be truly boundless, the ''extractiveness'' and ''suppression'' metrics would likely increase as new frontiers of equality are contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_universalism_ambiguity, conceptual, 'The irreducible uncertainty regarding the ultimate boundaries of the universalist principle.').

omega_variable(
    judicial_vs_social_mandate,
    'To what extent is the iterative expansion of equality driven by judicial interpretation versus broader social movements and legislative action?',
    'Historical analysis of specific civil rights advancements, examining the sequence and causal weight of court decisions, legislative acts, and social protests.',
    'If primarily judicial, the constraint''s persistence depends heavily on the composition and interpretive philosophy of the Supreme Court. If primarily social, its resilience is tied to public will and organized advocacy, potentially making it more robust against judicial retrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_social_mandate, empirical, 'The relative influence of judicial vs. social forces in driving equality''s expansion.').

omega_variable(
    cost_of_equality_distribution,
    'Are the ''costs'' of equality''s expansion (e.g., loss of privilege, economic restructuring) distributed equitably among those who previously benefited from inequality, or are they disproportionately borne by certain segments?',
    'Socio-economic analysis of policy impacts, wealth redistribution patterns, and demographic shifts following major equality-expanding reforms.',
    'If costs are inequitably distributed, it could fuel greater resistance and social instability, potentially increasing the ''suppression'' required to maintain the constraint. If more equitably distributed, it might lead to more stable, albeit slower, progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_equality_distribution, empirical, 'Equity of cost distribution for equality''s expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.25).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(all__tr_t1964, all_men_created_equal__universalist_reading, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.3).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.5).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(all__be_t1964, all_men_created_equal__universalist_reading, base_extractiveness, 1964, 0.6).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.4).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.6).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(all__su_t1964, all_men_created_equal__universalist_reading, suppression_requirement, 1964, 0.7).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, voting_rights_act).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, equal_protection_clause_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel, each representing a distinct structural claim about the principle's scope and application. This universalist reading emphasizes iterative expansion, while the originalist reading emphasizes founder intent, and the textualist paradox reading highlights the inherent contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
