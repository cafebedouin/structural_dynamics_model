% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary: Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'physical appropriation' reading of the
 *   Fifth Amendment's Takings Clause, which holds that only direct physical
 *   seizures or permanent physical occupations of private property by the
 *   government trigger a compensation requirement. Under this reading,
 *   regulations that merely diminish property value, even severely, do not
 *   constitute a 'taking.' This interpretation grants broad power to
 *   government regulators and places the burden of most regulatory losses on
 *   property owners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.4).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.6).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '937995c5-375a-4415-a2ab-8b4422ac7a18').
narrative_ontology:cs_kernel_codification('937995c5-375a-4415-a2ab-8b4422ac7a18', fixed_text).
narrative_ontology:cs_authority_grounding('937995c5-375a-4415-a2ab-8b4422ac7a18', lineage).
narrative_ontology:cs_interpretation_layer_present('937995c5-375a-4415-a2ab-8b4422ac7a18').
narrative_ontology:cs_reading_relation('937995c5-375a-4415-a2ab-8b4422ac7a18', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('937995c5-375a-4415-a2ab-8b4422ac7a18', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('937995c5-375a-4415-a2ab-8b4422ac7a18', foundational, physical_invasion_is_the_sine_qua_non_of_a_taking).
narrative_ontology:cs_axiom_status(physical_invasion_is_the_sine_qua_non_of_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('937995c5-375a-4415-a2ab-8b4422ac7a18', physical_invasion_is_the_sine_qua_non_of_a_taking, conventional).
narrative_ontology:cs_axiom('937995c5-375a-4415-a2ab-8b4422ac7a18', secondary, diminution_of_value_is_not_a_taking).
narrative_ontology:cs_axiom_status(diminution_of_value_is_not_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('937995c5-375a-4415-a2ab-8b4422ac7a18', diminution_of_value_is_not_a_taking, conventional).
narrative_ontology:cs_reference_frame('937995c5-375a-4415-a2ab-8b4422ac7a18', original_textualist_interpretation).
narrative_ontology:cs_drift_state('937995c5-375a-4415-a2ab-8b4422ac7a18', contemporary_jurisprudence, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('937995c5-375a-4415-a2ab-8b4422ac7a18', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement land use, environmental, and public health regulations without incurring compensation costs, so long as they avoid direct physical seizures or permanent occupations. This reading grants them broad discretion.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Are the only property owners who can claim compensation under this reading, when their property is directly seized or permanently occupied. They bear the full cost of regulatory burdens that do not involve physical appropriation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners, payer,
    moderate, biographical, constrained, local).

% Bear significant economic losses from regulations (e.g., zoning changes, environmental restrictions) that do not involve physical appropriation. Under this reading, they have no claim to compensation and must absorb these losses as a cost of ownership.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulatory_burdens, excluded,
    organized, biographical, constrained, national).

% Benefit from the government's ability to enact regulations for public welfare (e.g., environmental protection, historic preservation) without the fiscal burden of compensating every affected property owner. This reading facilitates their policy goals.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% The ultimate arbiter of Takings Clause interpretations. Its rulings shape which reading prevails and how compensation requirements are applied, influencing the balance between private property rights and public regulatory power.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between private property rights and public regulatory power by providing a clear, albeit narrow, trigger for compensation, allowing government to regulate broadly without constant litigation over economic impacts.
% TRANSFER_FUNCTION: Transfers the cost of most regulatory burdens from the government (and thus taxpayers) to individual property owners, except in cases of direct physical seizure or permanent occupation, where compensation flows from government to the dispossessed owner.
% ABSENT_VOICES: Property owners facing significant regulatory burdens that fall short of physical appropriation are effectively excluded from compensation claims under this reading; they would argue for a broader interpretation of 'taking' to include severe economic impacts.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for property rights and regulation would be thrown into chaos. Without a clear (even if narrow) rule, every regulation affecting property value could become a potential takings claim, paralyzing government action and leading to massive litigation and fiscal uncertainty.
% FOUNDING_PROBLEM: The Fifth Amendment's 'nor shall private property be taken for public use, without just compensation' clause required interpretation to define 'taken' beyond explicit eminent domain, particularly in the context of government regulation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, constitutional historians, and dissenting judicial opinions consistently attest to the ongoing interpretive challenge of the Takings Clause, with different factions advocating for various readings based on textualism, original intent, or evolving societal needs. The Supreme Court's continued engagement with takings cases corroborates the live status of this foundational problem.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).
:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) is moderate because while it shifts significant regulatory costs to property owners, it does provide a clear, albeit narrow, path to compensation for direct physical takings. Suppression (0.6) is present as it actively suppresses claims for compensation based on economic impact alone, requiring property owners to absorb these losses. Theater ratio (0.1) is low, as the enforcement of this boundary is largely functional and not performative; the legal system genuinely adjudicates claims based on this distinction. Accessibility collapse (0.7) is high because for most regulatory impacts, the 'alternative' of seeking compensation is legally foreclosed. Resistance (0.3) is moderate, as property rights advocates consistently challenge this narrow reading, but it remains a dominant interpretive framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of government regulators, this reading is a necessary 'rope' for effective governance, allowing them to pursue public welfare without prohibitive costs. From the perspective of property owners whose land value is severely diminished by regulation without physical taking, it operates as a 'snare,' extracting value without compensation. The Supreme Court, as an analytical observer, navigates these competing interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and public interest advocates are beneficiaries (d near 0.0) as this reading enables broad regulatory action without fiscal penalty. Property owners who are physically dispossessed are victims (d near 1.0) as they are directly targeted for compensation, but their numbers are small. Property owners facing regulatory burdens are excluded from compensation, bearing costs without recourse, placing them effectively as victims of the broader regulatory regime enabled by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling broad regulatory power as pure extraction by acknowledging a genuine coordination function (enabling public welfare regulation). However, its narrow scope for compensation means it risks becoming a 'tangled rope' if the 'coordination' of public welfare is achieved by disproportionately extracting from a specific class of property owners without adequate justification beyond fiscal convenience. The ongoing contestation over 'regulatory takings' highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_regulatory_distinction,
    'Is the distinction between physical appropriation and regulatory impact a principled, natural boundary, or a constructed legal fiction to limit government liability?',
    'Analysis of historical intent and comparative legal systems: if other common law systems developed similar bright-line rules independently, it suggests a more natural boundary; if it''s unique to US jurisprudence and driven by fiscal concerns, it suggests a constructed boundary.',
    'If constructed, the ''physical appropriation'' reading''s low extractiveness for government regulators is artificially maintained, and the constraint leans more towards a Snare for property owners. If natural, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_regulatory_distinction, conceptual, 'Ambiguity of the physical vs. regulatory takings distinction.').

omega_variable(
    scope_of_public_use,
    'How broadly should ''public use'' be interpreted in the context of the Takings Clause, and does this reading''s narrow compensation trigger incentivize an overly broad interpretation of public use?',
    'Empirical study of eminent domain cases where compensation was paid under this reading: if ''public use'' is consistently stretched to include private economic development, it suggests an incentive problem.',
    'An overly broad ''public use'' combined with a narrow compensation trigger would amplify the constraint''s extractiveness for dispossessed property owners, pushing it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_public_use, empirical, 'Impact of narrow compensation on ''public use'' interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1922, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1922, 0.05).
narrative_ontology:measurement(taki_tr_t1950, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(taki_tr_t1980, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1922, 0.3).
narrative_ontology:measurement(taki_be_t1950, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(taki_be_t1980, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1922, 0.5).
narrative_ontology:measurement(taki_su_t1950, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(taki_su_t1980, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'takings_clause_boundary' kernel. Its narrow interpretation of 'taking' directly influences the scope and application of the 'regulatory_takings_reading' and 'categorical_takings_reading' by defining what falls outside their purview.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
