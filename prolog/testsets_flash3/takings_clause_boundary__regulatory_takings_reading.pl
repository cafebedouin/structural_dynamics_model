% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'regulatory takings' reading of the Fifth
 *   Amendment's Takings Clause, primarily articulated in Penn Central
 *   Transportation Co. v. City of New York (1978). It holds that regulations
 *   which 'go too far' in diminishing property value, even without physical
 *   appropriation, may constitute a taking requiring compensation. This
 *   reading expanded the scope of property protection beyond direct physical
 *   seizures, introducing an ad hoc, fact-specific balancing test (the Penn
 *   Central factors) to determine when a regulation crosses the line. This
 *   creates a complex legal landscape, benefiting property owners by
 *   providing a potential check on government power, but imposing costs on
 *   regulatory bodies and public interest advocates due to increased
 *   litigation risk and compensation requirements. The claimed type is
 *   'tangled_rope' because it genuinely attempts to coordinate competing
 *   interests (property rights vs. police power) but does so with significant
 *   asymmetric extraction and requires active judicial enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.45).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '693c2cb8-e615-4914-9cb5-b58c3ada9eca').
narrative_ontology:cs_kernel_codification('693c2cb8-e615-4914-9cb5-b58c3ada9eca', fixed_text).
narrative_ontology:cs_authority_grounding('693c2cb8-e615-4914-9cb5-b58c3ada9eca', lineage).
narrative_ontology:cs_interpretation_layer_present('693c2cb8-e615-4914-9cb5-b58c3ada9eca').
narrative_ontology:cs_reading_relation('693c2cb8-e615-4914-9cb5-b58c3ada9eca', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('693c2cb8-e615-4914-9cb5-b58c3ada9eca', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('693c2cb8-e615-4914-9cb5-b58c3ada9eca', foundational, economic_value_diminution_as_taking).
narrative_ontology:cs_axiom_status(economic_value_diminution_as_taking, holdable).
narrative_ontology:cs_axiom_grounding('693c2cb8-e615-4914-9cb5-b58c3ada9eca', economic_value_diminution_as_taking, conventional).
narrative_ontology:cs_axiom('693c2cb8-e615-4914-9cb5-b58c3ada9eca', secondary, ad_hoc_balancing_test_necessity).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_test_necessity, holdable).
narrative_ontology:cs_axiom_grounding('693c2cb8-e615-4914-9cb5-b58c3ada9eca', ad_hoc_balancing_test_necessity, conventional).
narrative_ontology:cs_reference_frame('693c2cb8-e615-4914-9cb5-b58c3ada9eca', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('693c2cb8-e615-4914-9cb5-b58c3ada9eca', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('693c2cb8-e615-4914-9cb5-b58c3ada9eca', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, economic_liberty_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, private_property_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the potential for compensation when regulations severely diminish their property's economic value, even without physical appropriation. This provides a check on government power but introduces uncertainty into regulatory planning.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, local).

% Leverage the doctrine to challenge land-use or environmental regulations that would significantly reduce the profitability of their projects, potentially securing compensation or forcing regulatory concessions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, developers, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of potential compensation claims when their regulations are deemed 'takings.' This creates a chilling effect on public welfare regulations, forcing them to balance public good against fiscal risk.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, local_governments, payer,
    institutional, generational, constrained, local).

% Face legal challenges and potential compensation requirements when implementing regulations to protect natural resources or public health, leading to more cautious or diluted regulatory approaches.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_regulators, payer,
    institutional, generational, constrained, national).

% Advocate for regulations that serve collective goods (e.g., environmental protection, historic preservation) but find their efforts hampered by the threat of takings claims, which can make such regulations politically and fiscally unfeasible.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates, payer,
    moderate, generational, constrained, national).

% The ultimate arbiter of what constitutes a regulatory taking, applying the ad hoc Penn Central balancing test. Their decisions shape the boundaries of property rights and government regulatory power, but the test's flexibility leads to ongoing litigation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze the evolving jurisprudence of regulatory takings, critiquing the coherence and application of the Penn Central test and its impact on property rights and public welfare.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between private property rights and the government's police power to regulate for public welfare, ensuring that property owners are not unfairly burdened by regulations that 'go too far' in diminishing economic value.
% TRANSFER_FUNCTION: Potentially transfers public funds (compensation) from government entities to private property owners when regulations are deemed to have excessively diminished property value, or transfers regulatory burden from property owners back to the public by deterring regulation.
% ABSENT_VOICES: Future generations and unrepresented ecosystems, who would advocate for stronger environmental and land-use regulations without the chilling effect of takings claims, are not directly represented in the balancing test.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, governments would have significantly more freedom to regulate land use, environmental protection, and public health without fear of compensation claims. This would likely lead to more robust public welfare regulations, but also potentially to greater economic burdens on property owners, fundamentally altering the balance of power.
% FOUNDING_PROBLEM: To prevent government from effectively confiscating private property through regulation without paying just compensation, thereby protecting individual economic liberty and incentivizing investment.
% FOUNDING_PROBLEM_CORROBORATION: Property rights organizations and some legal scholars attest that the problem of over-regulation remains live, requiring judicial oversight. Public interest groups and other legal scholars argue that the doctrine itself has become a problem, chilling necessary public welfare regulations.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.65) because the doctrine shifts significant costs from private property owners to the public (via compensation or deterred regulation). Suppression (0.45) is moderate; while property owners have legal avenues, the ad hoc nature of the Penn Central test creates uncertainty, and the cost of litigation can be prohibitive for smaller owners. For governments, the threat of takings claims suppresses their ability to enact robust public welfare regulations. Theater ratio (0.20) is low; the doctrine is actively litigated and shapes real regulatory outcomes, though some argue its application can be inconsistent or performative. The Penn Central factors (economic impact, interference with investment-backed expectations, character of the governmental action) are the core of the ad hoc balancing test.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, the doctrine is a crucial 'rope' protecting fundamental rights against government overreach. From the perspective of regulators and public interest advocates, it can function as a 'snare' that extracts public resources and suppresses necessary public welfare regulations. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are beneficiaries (low d) as the doctrine protects their economic interests and provides a mechanism for compensation. Local governments, environmental regulators, and public interest advocates are payers (high d) as they bear the costs of compensation, litigation, and the chilling effect on regulation. The Supreme Court, as the agenda-setter, defines and enforces the boundaries of this doctrine, shaping its directionality for all other parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate to balance property rights and police power is still live, but its application is contested. The 'going too far' standard is inherently vague, leading to ongoing disputes about whether the doctrine is fulfilling its original coordination function or has drifted into an extractive mechanism that disproportionately benefits certain property interests at public expense. The ad hoc nature of the Penn Central test prevents clear resolution, allowing the contest to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_coherence,
    'Is the Penn Central ad hoc balancing test a coherent and predictable framework for identifying regulatory takings, or does its flexibility lead to arbitrary outcomes?',
    'Empirical analysis of lower court decisions for consistency and predictability, or a Supreme Court decision establishing clearer, more objective criteria.',
    'If incoherent, the doctrine''s legitimacy as a coordination mechanism is undermined, increasing its effective extractiveness due to litigation costs and regulatory uncertainty. If coherent, it functions more effectively as a ''rope'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_coherence, conceptual, 'The coherence and predictability of the Penn Central balancing test.').

omega_variable(
    chilling_effect_quantification,
    'To what extent does the threat of regulatory takings claims actually deter or dilute public welfare regulations, and what is the economic cost of this chilling effect?',
    'Comparative studies of regulatory outcomes in jurisdictions with different takings jurisprudence, or economic modeling of regulatory decision-making under takings risk.',
    'Quantifying a significant chilling effect would strengthen the ''snare'' argument for regulators and public interest advocates, highlighting the doctrine''s suppressive and extractive impact on public goods. A negligible effect would support the ''rope'' framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'The actual impact of regulatory takings on public welfare regulations.').

omega_variable(
    kernel_reading_divergence,
    'How does the ''regulatory takings'' reading structurally differ from the ''physical appropriation'' and ''categorical takings'' readings in its impact on property rights and government power?',
    'Comparative legal analysis of case outcomes under each reading, focusing on the types of government actions challenged and the remedies sought.',
    'This reading expands the victim set to include those suffering severe value diminution without physical loss, and introduces a flexible balancing test. The other readings are narrower, focusing on per se rules. This reading''s flexibility creates more litigation but also more avenues for property protection against non-physical extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between regulatory, physical, and categorical takings readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(taki_tr_t1990, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(taki_tr_t2010, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.5).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, land_use_zoning_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_protection_laws).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, historic_preservation_ordinances).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'takings_clause_boundary' kernel. It focuses on regulatory takings, distinct from physical appropriations or categorical takings, but all three readings influence the overall interpretation of the Takings Clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
