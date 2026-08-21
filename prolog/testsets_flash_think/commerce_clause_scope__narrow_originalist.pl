% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope (Narrow Originalist Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the 'narrow originalist' reading of
 *   the Commerce Clause, which holds that 'commerce among states' refers
 *   strictly to trade crossing state lines, and 'regulate' means to
 *   facilitate, not restrict. Federal power is thus limited to removing
 *   state-imposed barriers to interstate trade and ensuring uniform
 *   commercial rules, leaving substantial regulatory authority to the states.
 *   This reading views the Commerce Clause as a fixed, foundational limit on
 *   federal power, consistent with a claim of 'mountain'. The metrics reflect
 *   this reading's internal consistency and low perceived extraction from its
 *   own perspective, even as it is contested by other interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.15).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.1).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.15).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, mountain).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).
domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '1c1931dc-cf13-4918-8ddf-289a092fe669').
narrative_ontology:cs_kernel_codification('1c1931dc-cf13-4918-8ddf-289a092fe669', fixed_text).
narrative_ontology:cs_authority_grounding('1c1931dc-cf13-4918-8ddf-289a092fe669', lineage).
narrative_ontology:cs_interpretation_layer_present('1c1931dc-cf13-4918-8ddf-289a092fe669').
narrative_ontology:cs_reading_relation('1c1931dc-cf13-4918-8ddf-289a092fe669', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('1c1931dc-cf13-4918-8ddf-289a092fe669', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('1c1931dc-cf13-4918-8ddf-289a092fe669', foundational, commerce_is_trade_not_production).
narrative_ontology:cs_axiom_status(commerce_is_trade_not_production, holdable).
narrative_ontology:cs_axiom_grounding('1c1931dc-cf13-4918-8ddf-289a092fe669', commerce_is_trade_not_production, conventional).
narrative_ontology:cs_axiom('1c1931dc-cf13-4918-8ddf-289a092fe669', foundational, regulate_means_facilitate_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('1c1931dc-cf13-4918-8ddf-289a092fe669', regulate_means_facilitate_not_prohibit, conventional).
narrative_ontology:cs_reference_frame('1c1931dc-cf13-4918-8ddf-289a092fe669', founding_era_limited_federalism).
narrative_ontology:cs_drift_state('1c1931dc-cf13-4918-8ddf-289a092fe669', contemporary_post_new_deal_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1c1931dc-cf13-4918-8ddf-289a092fe669', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_advocates).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, originalism_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, limited_government_principle).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, state_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from retained autonomy over intrastate economic activity, allowing for diverse regulatory approaches without federal interference. They are the primary recipients of the federal power limitation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    powerful, generational, mobile, national).

% Benefit from not being subject to extensive federal regulation for purely local activities, reducing compliance burdens and fostering local economic experimentation. They operate primarily within state lines.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, mobile, local).

% Under this reading, its power is strictly limited to facilitating interstate trade and removing state barriers, not regulating intrastate production or non-commercial activities. It acts as an enforcer of these limits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_government, agenda_setter,
    institutional, civilizational, constrained, national).

% Suffers from the inability to impose uniform federal standards across states for issues like environmental protection, labor laws, or civil rights, leading to a patchwork of regulations and potential 'races to the bottom'.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).

% Bear the cost of limited federal power, as it restricts the federal government's ability to enforce civil rights protections in states that might otherwise resist. Their ability to achieve national standards is hampered.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% Interprets the Commerce Clause, and under this reading, would strictly limit federal legislative authority. Its decisions shape the boundaries of federal power and state autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, supreme_court, observer).

% Analyze and advocate for this interpretation, grounding their arguments in historical texts and original intent. They seek to persuade legal and political actors to adopt this view.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, originalist_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a free flow of commerce among states by preventing protectionist state legislation and establishing a common market, while preserving state sovereignty over intrastate affairs.
% TRANSFER_FUNCTION: Transfers regulatory authority over non-interstate commercial activities from the federal government to state governments, and limits the scope of federal legislative power.
% ABSENT_VOICES: Advocates for a robust federal role in addressing national problems (e.g., environmental degradation, labor exploitation, civil rights violations) that may have local origins but aggregate to national effects. They are excluded by the strict definitional boundaries of 'commerce' and 'regulate' in this reading.
% DISAPPEARANCE_RATIONALE: If this narrow originalist interpretation vanished, the federal government's power under the Commerce Clause would expand significantly, leading to more extensive national regulation of economic and social life, and a corresponding reduction in state autonomy. The balance of federalism would fundamentally shift.
% FOUNDING_PROBLEM: To prevent states from erecting trade barriers against each other, which had plagued the nation under the Articles of Confederation, and to create a unified national market, while simultaneously respecting the reserved powers of the states.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and some state's rights advocates attest that the problem of federal overreach and the erosion of state sovereignty remains live. Critics, however, argue that the original problem of state protectionism is largely solved, and new national problems require broader federal action.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, ExtMetricName, E),
    domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From the narrow originalist perspective, federal extractiveness is low (0.15) because the federal government is constrained from overreaching into state affairs. Suppression is low (0.10) as it primarily suppresses federal overreach, not state autonomy. Theater ratio is low (0.05) because the interpretation is presented as a direct, principled application of original meaning, not a performance. Accessibility collapse is high (0.85) because, from this viewpoint, alternative interpretations are structurally unsound. Resistance is moderate (0.40) as this reading faces significant opposition from those advocating for broader federal power.
 *
 * PERSPECTIVAL GAP:
 *   The 'narrow originalist' reading is itself a perspective. From the viewpoint of state governments and local businesses, this constraint is a beneficial limit on federal power. From the perspective of civil rights advocates or those seeking national regulatory uniformity, this same constraint is a barrier to addressing national problems, effectively extracting from their goals by limiting federal action.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are beneficiaries, as this reading maximizes their autonomy and minimizes federal regulatory burden. The federal government, while an agenda-setter, is structurally constrained by this reading, preventing it from acting as a broad extractor. 'National regulatory uniformity' and 'civil rights enforcement in recalcitrant states' are victims, as the constraint directly limits the mechanisms by which they could be achieved. Civil rights advocates are payers, bearing the cost of limited federal enforcement capacity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ambiguity,
    'Is the ''narrow originalist'' interpretation truly the fixed, original meaning of the Commerce Clause, or a selective reading influenced by contemporary political preferences?',
    'Comprehensive historical and linguistic analysis of founding-era documents and debates, cross-referenced with evolving legal scholarship and judicial precedent.',
    'If it is demonstrably a selective reading, its claim to ''mountain'' status is undermined, and it would be reclassified as a constructed constraint (e.g., a ''snare'' for federal power or a ''tangled_rope'' for federalism) that benefits specific political actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, conceptual, 'Ambiguity regarding the true ''original meaning'' of the Commerce Clause.').

omega_variable(
    impact_of_broader_readings,
    'What would be the full structural impact on federalism and national policy if the ''broad_effects_test'' or ''intermediate_channels'' readings were to fully displace the ''narrow_originalist'' view?',
    'Comparative legal analysis of jurisdictions where broader interpretations prevail, coupled with counterfactual historical analysis of policy outcomes under different Commerce Clause regimes.',
    'If broader readings prevail, federal extractiveness from states would increase, state autonomy would decrease, and the federal government''s role in areas like environmental protection and civil rights would expand significantly. This would fundamentally alter the classification of the Commerce Clause itself from a ''mountain'' (as claimed here) to a ''tangled_rope'' or ''snare'' from the perspective of states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_of_broader_readings, empirical, 'Consequences of alternative Commerce Clause interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1937, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.05).
narrative_ontology:measurement(comm_tr_t1960, commerce_clause_scope__narrow_originalist, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_scope__narrow_originalist, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__narrow_originalist, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(comm_tr_t2023, commerce_clause_scope__narrow_originalist, theater_ratio, 2023, 0.05).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_scope__narrow_originalist, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_scope__narrow_originalist, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__narrow_originalist, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(comm_be_t2023, commerce_clause_scope__narrow_originalist, base_extractiveness, 2023, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.1).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_scope__narrow_originalist, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_scope__narrow_originalist, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__narrow_originalist, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(comm_su_t2023, commerce_clause_scope__narrow_originalist, suppression_requirement, 2023, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'commerce_clause_scope' kernel. Each reading instantiates a different constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
