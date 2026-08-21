% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Application of Geneva Protections and Human Rights Law in Armed Conflict
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universal_rights_reading' of the Geneva
 *   Conventions' protective scope, asserting that protections extend to all
 *   persons affected by armed conflict, regardless of combatant status, and
 *   that Common Article 3 plus human rights law create a universal floor.
 *   This reading is a 'Tangled Rope' because it genuinely coordinates
 *   protection for vulnerable populations (beneficiaries) but does so by
 *   imposing significant, often resisted, extraction on state military
 *   operational flexibility (victims). Its persistence relies on active
 *   enforcement and advocacy against state resistance. The claimed type
 *   (Rope) reflects the aspirational framing by its proponents, while the
 *   metrics reflect the reality of its contested and extractive application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.75).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Application of Geneva Protections and Human Rights Law in Armed Conflict").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '29987a55-a6b2-4cc7-9c54-9ce078cc7d41').
narrative_ontology:cs_kernel_codification('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', fixed_text).
narrative_ontology:cs_authority_grounding('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', lineage).
narrative_ontology:cs_interpretation_layer_present('29987a55-a6b2-4cc7-9c54-9ce078cc7d41').
narrative_ontology:cs_reading_relation('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', foundational, human_dignity_is_universal).
narrative_ontology:cs_axiom_status(human_dignity_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', human_dignity_is_universal, deontological).
narrative_ontology:cs_axiom('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', foundational, common_article_3_is_minimum_standard).
narrative_ontology:cs_axiom_status(common_article_3_is_minimum_standard, holdable).
narrative_ontology:cs_axiom_grounding('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', common_article_3_is_minimum_standard, conventional).
narrative_ontology:cs_reference_frame('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', post_wwii_human_dignity_framework).
narrative_ontology:cs_drift_state('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', contemporary_counterterrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('29987a55-a6b2-4cc7-9c54-9ce078cc7d41', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocates).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and litigate for the expansive interpretation of IHL and human rights law, pushing for universal application. They benefit from the adoption of this reading but face significant institutional resistance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Are the primary intended beneficiaries of universal protections, receiving a baseline of humane treatment regardless of conflict type or their status. They are often trapped in conflict zones and rely on these protections for survival.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from receiving protections under Common Article 3 and human rights law, which grants them a degree of legal status and humane treatment. However, they are also constrained by the obligations these laws impose and their status is often contested by states.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    moderate, biographical, constrained, regional).

% Bear the costs of compliance with an expansive interpretation of IHL, which restricts their targeting, detention, and interrogation practices. Their operational flexibility is significantly reduced, and personnel face increased accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces, payer,
    institutional, biographical, constrained, global).

% Are legally bound to uphold IHL and human rights law, but often resist the most expansive interpretations that limit their sovereignty or military options, particularly in counter-terrorism operations. They pay the political and legal costs of non-compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, state_governments, payer).

% Interpret and enforce IHL and human rights law, often pushing for the universal application reading. Their power to compel compliance is limited by state cooperation, but their rulings shape the legal landscape and increase pressure on states.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Analyze, debate, and critique the evolution of IHL, with some advocating for more state-centric or proportionality-based interpretations. They do not directly enforce but influence legal discourse and policy debates.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, traditional_ihl_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment and protection for all individuals affected by armed conflict, preventing arbitrary violence and ensuring a minimum standard of human dignity regardless of combatant status or conflict classification.
% TRANSFER_FUNCTION: Transfers operational flexibility and potential impunity from state military forces to the protected status and rights of all individuals, regardless of their role in conflict, by imposing stricter legal obligations on states.
% ABSENT_VOICES: Victims of conflict who lack legal representation or voice in international forums, particularly those in areas where state authority is weak or actively hostile to human rights norms. Their experiences would underscore the necessity of universal protections.
% DISAPPEARANCE_RATIONALE: If this universal rights reading vanished overnight, states would likely revert to more restrictive interpretations of IHL, leading to increased civilian casualties, arbitrary detention, and reduced accountability for human rights violations in conflict, fundamentally altering the landscape of protection and human suffering.
% FOUNDING_PROBLEM: The historical failure of traditional IHL to adequately protect civilians and non-combatants in non-international armed conflicts, and the desire to prevent atrocities by establishing a universal floor of human dignity and accountability for all persons affected by conflict.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous academic legal scholars (outside state military establishments) consistently corroborate the ongoing need for and contestation of this universal floor, citing contemporary conflicts and state practices.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.75) is high because this reading significantly limits state military action, demanding high standards of care and accountability that states often resist. Suppression (0.70) is substantial as states actively attempt to limit or circumvent this expansive interpretation through legal arguments, political pressure, and operational practices. Theater ratio (0.40) is moderate, reflecting that while states often publicly affirm human rights, their actual practices in conflict zones may diverge, creating a gap between declared commitment and real-world application. Resistance (0.80) is very high, primarily from state military and political establishments that view this reading as an undue constraint on their ability to conduct operations. The temporal measurements show a clear trend of increasing extractiveness and suppression as this reading gains legal and normative traction, leading to greater state resistance and efforts to limit its scope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civilian populations and human rights advocates, this constraint is a vital 'Rope' or even a 'Mountain' of universal human dignity, providing essential coordination for protection. However, from the perspective of state military forces and governments, it operates as a 'Snare' or 'Tangled Rope', imposing significant costs and restrictions on their operational freedom, which they perceive as an overreach of legal authority. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and non-state armed groups are beneficiaries, as they gain protections. Human rights advocates are agenda-setters and beneficiaries, pushing for and benefiting from the expansion of these rights. State military forces and governments are payers and victims, as their operational flexibility is curtailed, and they bear the costs of compliance and accountability. International courts and tribunals act as agenda-setters, interpreting and enforcing the law, while traditional IHL scholars observe and influence the discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its founding problem (protecting all persons in armed conflict) remains acutely live. The contestation and high extractiveness arise from the ongoing tension between the universalist mandate and state interests, not from an outdated function. The classification as Tangled Rope correctly identifies both its genuine coordination function and the asymmetric extraction it imposes, preventing mislabeling it as a pure Snare (which would ignore its protective coordination) or a pure Rope (which would ignore the state resistance and extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_state_centric_application,
    'To what extent is the ''universal'' application of IHL and human rights law genuinely accepted by states, versus being a contested interpretation imposed by international bodies and advocacy groups?',
    'Analysis of state practice, military manuals, and reservations to treaties; judicial decisions by national courts; and the outcomes of international accountability mechanisms.',
    'If state acceptance is low, the constraint''s effective suppression and extractiveness are higher, as it relies more heavily on external enforcement against state will. If acceptance is high, it functions more as a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_state_centric_application, empirical, 'Ambiguity regarding the actual acceptance of universal application by states.').

omega_variable(
    military_necessity_vs_human_rights,
    'How is the balance between military necessity and human rights obligations interpreted and applied in practice, and does this interpretation consistently uphold the ''universal floor''?',
    'Case studies of specific conflicts, analysis of targeting policies, detention practices, and rules of engagement, particularly in asymmetric warfare contexts.',
    'If military necessity consistently overrides the ''universal floor'', the constraint''s effective protection function is diminished, and its theater ratio increases. If the ''universal floor'' holds, the constraint''s coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_necessity_vs_human_rights, conceptual, 'The practical tension between military necessity and universal human rights.').

omega_variable(
    enforceability_against_powerful_states,
    'Is the expansive ''universal rights'' reading genuinely enforceable against powerful states, or does it primarily constrain weaker actors?',
    'Examination of accountability outcomes for powerful states versus weaker states in international and national legal forums, and the impact of political leverage on enforcement.',
    'If enforcement is asymmetric, the constraint''s effective extractiveness is lower for powerful states (damped directionality) and higher for weaker states (amplified directionality), revealing a structural inequality in its application.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforceability_against_powerful_states, empirical, 'The differential enforceability of universal rights against states of varying power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1964, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1964, 0.15).
narrative_ontology:measurement(gene_tr_t1979, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(gene_tr_t1994, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement(gene_tr_t2009, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2009, 0.35).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(gene_be_t1964, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1964, 0.5).
narrative_ontology:measurement(gene_be_t1979, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1979, 0.6).
narrative_ontology:measurement(gene_be_t1994, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1994, 0.68).
narrative_ontology:measurement(gene_be_t2009, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2009, 0.72).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(gene_su_t1964, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1964, 0.45).
narrative_ontology:measurement(gene_su_t1979, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement(gene_su_t1994, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(gene_su_t2009, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'geneva_conventions_protective_scope' kernel. It asserts a universal application of protections, contrasting with state-centric and hybrid-proportionality readings. Each reading is modeled as a separate constraint due to differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
