% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Interpretation of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'restrictive_originalist' reading of the
 *   equality clause scope, prevalent in 18th-century American constitutional
 *   thought. It posits that the concept of equality, as enshrined in founding
 *   documents, primarily applied to propertied white males as political
 *   actors within a specific social contract framework. Rights and franchises
 *   for other groups (women, racial minorities, non-propertied males) were
 *   not considered inherent or constitutionally guaranteed under this
 *   interpretation, requiring separate legislative or amendment processes for
 *   their recognition. This reading is a 'tangled_rope' because it provided a
 *   coordination function for the political class of the time while actively
 *   extracting from and suppressing the rights of excluded groups.
 *
 * KEY AGENTS:
 *   - propertied_white_males_historical: Primary beneficiary (institutional/arbitrage) — defined the scope of equality to their benefit.
 *   - originalist_legal_scholars: Agenda setter (institutional/analytical) — interpret and perpetuate this restrictive view.
 *   - women: Primary victim (powerless/trapped) — denied political and often civil equality.
 *   - racial_minorities: Primary victim (powerless/trapped) — subjected to systemic inequality and denial of rights.
 *   - non_propertied_males: Victim (powerless/constrained) — excluded from full political participation based on economic status.
 *   - lgbtq_individuals: Excluded (powerless/trapped) — their existence and rights were entirely outside the original framework's consideration.
 *   - conservative_political_factions: Beneficiary (organized/mobile) — leverage this interpretation to resist contemporary rights expansions.
 *   - civil_rights_advocates: Payer (organized/constrained) — bear the cost of challenging this interpretation through activism and litigation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.6).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.7).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Interpretation of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '0e685564-d641-4203-85d1-f88ca15cf4ca').
narrative_ontology:cs_kernel_codification('0e685564-d641-4203-85d1-f88ca15cf4ca', fixed_text).
narrative_ontology:cs_authority_grounding('0e685564-d641-4203-85d1-f88ca15cf4ca', lineage).
narrative_ontology:cs_interpretation_layer_present('0e685564-d641-4203-85d1-f88ca15cf4ca').
narrative_ontology:cs_reading_relation('0e685564-d641-4203-85d1-f88ca15cf4ca', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('0e685564-d641-4203-85d1-f88ca15cf4ca', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('0e685564-d641-4203-85d1-f88ca15cf4ca', foundational, original_intent_supremacy).
narrative_ontology:cs_axiom_status(original_intent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('0e685564-d641-4203-85d1-f88ca15cf4ca', original_intent_supremacy, conventional).
narrative_ontology:cs_axiom('0e685564-d641-4203-85d1-f88ca15cf4ca', foundational, equality_as_political_franchise_not_universal_right).
narrative_ontology:cs_axiom_status(equality_as_political_franchise_not_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('0e685564-d641-4203-85d1-f88ca15cf4ca', equality_as_political_franchise_not_universal_right, conventional).
narrative_ontology:cs_reference_frame('0e685564-d641-4203-85d1-f88ca15cf4ca', id_18th_century_social_contract).
narrative_ontology:cs_drift_state('0e685564-d641-4203-85d1-f88ca15cf4ca', contemporary_civil_rights_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0e685564-d641-4203-85d1-f88ca15cf4ca', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males_historical).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, conservative_political_factions).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, racial_minorities).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, lgbtq_individuals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is substantial because it systematically denied fundamental rights and political agency to large segments of the population, channeling power and resources to the defined 'equals'. Suppression (0.7) was high, enforced through legal statutes, social norms, and violence, preventing excluded groups from asserting their claims. The theater ratio (0.2) is relatively low, as the constraint was genuinely functional in maintaining the social and political order of the time, with less performative pretense than later, more eroded constraints. The accessibility collapse (0.4) is moderate, as alternatives (e.g., abolitionist movements, women's suffrage movements) did exist but were severely constrained. Resistance (0.75) was high, reflecting the ongoing struggle against these exclusions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'propertied_white_males_historical' and 'originalist_legal_scholars', this constraint might be viewed as a 'rope' or even a 'mountain' – a natural or self-evident ordering of society. However, from the perspective of 'women', 'racial_minorities', and 'non_propertied_males', it was clearly a 'snare' or 'tangled_rope', actively extracting their rights and suppressing their agency. The engine's computation will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'propertied_white_males_historical' and 'originalist_legal_scholars' are clear beneficiaries, as the constraint directly served their interests and worldview. 'Women', 'racial_minorities', and 'non_propertied_males' are victims, bearing the direct costs of exclusion. 'Conservative_political_factions' are also beneficiaries, as they gain political leverage from this interpretation. 'Civil_rights_advocates' are payers, expending significant resources to challenge it. 'LGBTQ_individuals' are excluded, as their rights were not even conceived within this framework, making them targets of its implicit suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (to define political equality for a specific subset of the population) has largely atrophied in modern legal and social discourse, yet the 'restrictive_originalist' interpretation persists. The classification as 'tangled_rope' prevents mislabeling it as a 'mountain' (natural law) by highlighting its active enforcement and identifiable victims, even as its original coordination function for a narrow elite has become increasingly contested. The persistence of this interpretation, despite its outdated mandate, points to its continued utility for certain beneficiaries in contemporary political struggles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reflection of the original intent, or a selective interpretation that serves contemporary political agendas?',
    'Comprehensive historical analysis of primary sources from the founding era, including debates, private correspondence, and contemporary legal commentaries, cross-referenced with the political and economic interests of the framers.',
    'If a selective interpretation, the constraint''s claimed ''mountain'' status (as natural law or original intent) would be reclassified as a ''snare'' or ''tangled_rope'', revealing its constructed and extractive nature. If a genuine reflection, its historical accuracy would be affirmed, though its moral implications would remain open to critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Distinguishing genuine original intent from selective historical interpretation.').

omega_variable(
    scope_of_equality_kernel,
    'This constraint is the ''restrictive_originalist'' reading of the ''equality_clause_scope'' kernel. What would change if the ''expansive_universalist'' reading were adopted?',
    'Judicial precedent shift or constitutional amendment explicitly adopting a universalist interpretation, followed by legislative and executive action to enforce it.',
    'The beneficiary set would expand to include all humans, and the victim set would dissolve. The constraint would likely reclassify from ''tangled_rope'' to ''rope'' or even ''mountain'' (if seen as a self-evident truth), with significantly lower extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_equality_kernel, conceptual, 'Impact of adopting the ''expansive_universalist'' reading on constraint classification.').

omega_variable(
    legitimacy_threshold_for_expansion,
    'This constraint is the ''restrictive_originalist'' reading of the ''equality_clause_scope'' kernel. What would change if the ''progressive_textualist'' reading were adopted?',
    'A sustained period of democratic legislative action and constitutional amendments expanding rights, coupled with judicial deference to these processes rather than reinterpretation of original intent.',
    'The beneficiary set would expand through formal processes, but the constraint would retain its ''tangled_rope'' or ''rope'' classification, as the mechanism for expansion (amendment) would still be a constructed coordination. Extractiveness would decrease for newly included groups, but the high legitimacy threshold for expansion would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_threshold_for_expansion, conceptual, 'Impact of adopting the ''progressive_textualist'' reading on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 1887).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__restrictive_originalist, theater_ratio, 50, 0.15).
narrative_ontology:measurement(equa_tr_t100, equality_clause_scope__restrictive_originalist, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__restrictive_originalist, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(equa_be_t100, equality_clause_scope__restrictive_originalist, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__restrictive_originalist, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(equa_su_t100, equality_clause_scope__restrictive_originalist, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_expansion).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, civil_rights_legislation).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equal_protection_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'equality_clause_scope' kernel. Its restrictive interpretation directly influences the perceived legitimacy and difficulty of expanding voting rights and civil rights protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
