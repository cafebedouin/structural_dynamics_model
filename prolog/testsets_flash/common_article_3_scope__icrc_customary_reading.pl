% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope (ICRC Customary Law Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the ICRC's reading of Common Article 3 (CA3)
 *   of the Geneva Conventions, which holds that the scope of CA3 is
 *   determined by evolving state practice and opinio juris, as tracked
 *   through customary international law. This reading allows for a dynamic
 *   and expanding application of minimum humanitarian standards in
 *   non-international armed conflicts, adapting to new forms of organized
 *   violence without requiring formal treaty amendments. It functions as a
 *   coordination mechanism for states to gradually align their practices with
 *   evolving humanitarian norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.3).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.2).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope (ICRC Customary Law Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'b3ea3c8d-9076-4507-b13b-d34664a2f5a9').
narrative_ontology:cs_kernel_codification('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', formalized).
narrative_ontology:cs_authority_grounding('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', lineage).
narrative_ontology:cs_interpretation_layer_present('b3ea3c8d-9076-4507-b13b-d34664a2f5a9').
narrative_ontology:cs_reading_relation('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', foundational, customary_law_as_dynamic_source).
narrative_ontology:cs_axiom_status(customary_law_as_dynamic_source, holdable).
narrative_ontology:cs_axiom_grounding('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', customary_law_as_dynamic_source, conventional).
narrative_ontology:cs_axiom('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', secondary, opinio_juris_as_legal_conviction).
narrative_ontology:cs_axiom_status(opinio_juris_as_legal_conviction, holdable).
narrative_ontology:cs_axiom_grounding('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', opinio_juris_as_legal_conviction, conventional).
narrative_ontology:cs_reference_frame('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', evolving_customary_standards).
narrative_ontology:cs_drift_state('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b3ea3c8d-9076-4507-b13b-d34664a2f5a9', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_organizations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_seeking_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_resisting_expansion).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, evolving_standards_of_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross (ICRC) actively researches, documents, and promotes the customary international law interpretation of Common Article 3, influencing states and international bodies. They benefit from the expansion of IHL's protective scope.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, analytical, global).

% These organizations rely on the broadest possible application of IHL to protect civilians and provide aid in armed conflicts. The customary law reading provides a flexible mechanism for expanding protection, even if slower than other readings.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_organizations, beneficiary,
    organized, biographical, constrained, global).

% States that wish to be seen as compliant with international law, or to influence its development, benefit from a framework that allows for the gradual evolution of norms through practice and opinio juris, rather than rigid treaty interpretation. They can selectively adopt practices to shape custom.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_seeking_legitimacy, beneficiary,
    powerful, generational, mobile, global).

% Some states resist the expansion of CA3's scope, preferring a more restrictive, state-centric interpretation to preserve sovereignty and operational flexibility. They bear the cost of having to justify their practices against an evolving customary standard.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_resisting_expansion, payer,
    institutional, generational, constrained, global).

% These bodies interpret and apply international law, including customary IHL. They observe state practice and opinio juris to determine the scope of CA3, influencing its practical application through their judgments.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible, non-treaty mechanism for states to coordinate on the minimum humanitarian standards applicable in non-international armed conflicts, allowing for the gradual evolution and expansion of these standards through shared practice and legal conviction.
% TRANSFER_FUNCTION: Transfers interpretive authority and normative pressure from rigid treaty text to the evolving consensus of state practice and opinio juris, influencing how states conduct military operations and treat non-state armed groups.
% ABSENT_VOICES: Non-state armed groups, while often subject to CA3, are not formal participants in the creation of customary international law and thus have no direct voice in shaping its scope, despite being primary targets of its application.
% DISAPPEARANCE_RATIONALE: If the customary international law reading of CA3's scope vanished, the legal framework for non-international armed conflicts would become significantly more rigid and less adaptable. States would lose a key mechanism for evolving humanitarian norms, leading to greater legal uncertainty and potentially reduced protection for victims, as the only remaining interpretations would be either strictly state-centric or purely aspirational human rights-based.
% FOUNDING_PROBLEM: The original Geneva Conventions did not adequately address non-international armed conflicts, leaving a gap in humanitarian protection. Common Article 3 was a minimal attempt to fill this, but its scope required further development.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, numerous humanitarian organizations, and many states attest that the problem of ensuring adequate humanitarian protection in diverse and evolving non-international armed conflicts remains live. Scholarly legal analysis from independent experts also corroborates the ongoing need for a dynamic interpretive framework for CA3.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because this reading primarily facilitates coordination and expansion of protective norms, rather than imposing heavy costs or extracting rents. Suppression is low (0.2) as adherence relies on states' voluntary adoption of practice and opinio juris, rather than coercive enforcement. Theater ratio is low (0.1) because the ICRC's work in documenting customary law is genuinely functional, though some states may engage in performative compliance. The metrics reflect a constraint that is largely beneficial for humanitarian protection, even if its expansion is slower than some would prefer.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of humanitarian organizations, this reading is a vital, if sometimes slow, mechanism for expanding protection. From the perspective of states resisting expansion, it can be seen as an encroachment on sovereignty. The engine's classification will reflect the overall coordination function with some friction for resistant states.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC and humanitarian organizations are clear beneficiaries, as this reading expands the protective scope of IHL. States seeking legitimacy also benefit by having a flexible framework for demonstrating compliance. States resisting expansion are payers, as they face pressure to conform to evolving customary norms. International courts and tribunals act as observers, interpreting and applying these norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_treaty_rigidity,
    'Is the flexibility of customary international law in determining CA3''s scope a strength (adaptability) or a weakness (lack of clarity/enforceability)?',
    'Empirical study of state compliance and judicial application in diverse conflict scenarios: if flexibility leads to consistent, effective protection, it''s a strength; if it leads to arbitrary application or non-compliance, it''s a weakness.',
    'If a strength, the ''rope'' classification is reinforced. If a weakness, the constraint might lean towards ''tangled_rope'' due to the ambiguity creating opportunities for selective application and extraction by powerful states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_rigidity, conceptual, 'Ambiguity of customary law''s role in IHL scope.').

omega_variable(
    state_practice_vs_humanitarian_imperative,
    'To what extent does ''state practice'' genuinely reflect a humanitarian imperative, versus states'' self-interest in limiting obligations?',
    'Detailed analysis of state declarations and actions in specific conflicts, cross-referenced with their geopolitical interests and human rights records. Divergence would indicate self-interest overriding humanitarian concerns.',
    'If state practice is primarily driven by self-interest, the extractiveness of this reading would be higher, as it would allow powerful states to shape norms to their advantage, potentially shifting it towards a ''tangled_rope'' or ''snare'' for vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_practice_vs_humanitarian_imperative, empirical, 'Motivation behind state practice in customary law formation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''icrc_customary_reading'' of the ''common_article_3_scope'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of judicial decisions or state declarations explicitly adopting a different reading.',
    'If the ''state_centric_reading'' were adopted, the scope of CA3 would narrow, reducing protection. If the ''expansive_human_rights_reading'' were adopted, the scope would broaden more rapidly, potentially increasing state resistance. This reading provides a middle ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this as one reading of a contested kernel and outlines the impact of adopting sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__icrc_customary_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__icrc_customary_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.2).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
