% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Doctrine (Loretto/Lucas Per Se Rules + Penn Central Balancing)
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The categorical takings reading (rooted in Loretto v. Teleprompter and
 *   Lucas v. South Carolina Coastal Council, layered onto Penn Central v. New
 *   York City) treats permanent physical occupation and total economic
 *   wipeout as per se takings requiring compensation, while everything else —
 *   the vast majority of actual regulatory disputes — is evaluated under Penn
 *   Central's three-factor ad hoc balancing test. This is a hybrid
 *   architecture: bright lines at the poles, contextual discretion in the
 *   middle. This story instantiates ONLY the categorical/hybrid reading of
 *   the takings-clause-boundary kernel; the physical_appropriation_reading
 *   (only direct seizure counts) and regulatory_takings_reading (any
 *   regulation going 'too far' counts) are separate constraints with their
 *   own ε and stakeholder structures, linked here via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.42).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.38).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Doctrine (Loretto/Lucas Per Se Rules + Penn Central Balancing)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0').
narrative_ontology:cs_kernel_codification('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', formalized).
narrative_ontology:cs_authority_grounding('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', lineage).
narrative_ontology:cs_interpretation_layer_present('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0').
narrative_ontology:cs_reading_relation('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', foundational, bright_line_rules_warranted_only_at_extremes).
narrative_ontology:cs_axiom_status(bright_line_rules_warranted_only_at_extremes, holdable).
narrative_ontology:cs_axiom_grounding('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', bright_line_rules_warranted_only_at_extremes, instrumental).
narrative_ontology:cs_axiom('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', foundational, contextual_balancing_is_administrable_for_middle_cases).
narrative_ontology:cs_axiom_status(contextual_balancing_is_administrable_for_middle_cases, holdable).
narrative_ontology:cs_axiom_grounding('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', contextual_balancing_is_administrable_for_middle_cases, conventional).
narrative_ontology:cs_reference_frame('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', penn_central_hybrid_equilibrium).
narrative_ontology:cs_drift_state('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', post_lucas_categorical_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bb3e0f8-2a89-4284-9b2f-dee95e7ed0e0', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, municipal_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, courts_administering_takings_claims).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_the_poles).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_in_the_middle_zone).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, long_term_land_use_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, long_term_land_use_planners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, hybrid_categorical_and_balancing_framework_is_administrable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce land-use, environmental, and zoning regulations. Under the categorical reading, they can regulate freely as long as they avoid permanent physical occupation and total wipeout of value, using Penn Central's multi-factor test as a flexible shield for everything short of those two poles. They set the litigation posture and often win the 'is this categorical?' threshold fight by careful drafting.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, municipal_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Owners who suffer a permanent physical occupation (a cable box bolted to their building) or a regulation that destroys all economically viable use of land get an automatic win under the bright-line rules, no balancing required. They benefit from predictability precisely because their facts sit at the extremes the doctrine was built to resolve cleanly.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_at_the_poles, beneficiary,
    moderate, biographical, mobile, national).

% Owners facing partial value diminution, temporary occupation, or regulation that leaves some but much-diminished use, must litigate under Penn Central's ad hoc, three-factor balancing test (economic impact, interference with investment-backed expectations, character of the government action). They bear the cost, delay, and unpredictability of a standard designed explicitly to avoid bright lines; most lose, and even winners spend years and substantial legal fees to find out.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_in_the_middle_zone, payer,
    powerless, biographical, trapped, national).

% Government planning bodies rely on the doctrine's flexibility to implement complex zoning and environmental schemes, but face recurring uncertainty about which regulations will later be found compensable, since Penn Central provides no reliable ex ante test. They benefit from the room to regulate but pay in unpredictable liability exposure and constant relitigation of settled plans.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, long_term_land_use_planners, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, long_term_land_use_planners, beneficiary).

% Federal and state courts apply the hybrid framework, gaining a workable doctrinal toolkit that avoids deciding every regulatory dispute as an all-or-nothing constitutional question. Courts benefit from the discretion Penn Central affords while bearing the institutional burden of inconsistent outcomes across circuits and endless line-drawing at the categorical/non-categorical boundary.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts_administering_takings_claims, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, courts_administering_takings_claims, beneficiary).

% People who will acquire property after a regulation is enacted are not parties to the doctrinal contest over the scope of investment-backed expectations, yet the doctrine's treatment of 'notice' of pre-existing regulation directly determines whether their future takings claims will succeed. They have no voice in how the boundary is drawn but inherit its consequences.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, future_property_owners, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides government with a stable, litigable line for the extreme cases (total physical occupation, total value destruction) where per se rules are administrable and predictable, while preserving discretionary balancing for the vast middle range of regulation, so that neither every regulation nor no regulation triggers compensation.
% TRANSFER_FUNCTION: Moves the burden of uncertainty from government (which gets bright-line safety everywhere except the two poles) to middle-zone property owners (who must litigate ad hoc, multi-year, multi-factor claims to establish compensable takings, with courts free to weigh factors in ways that favor the regulating government).
% ABSENT_VOICES: Future property owners who take title after a regulation's enactment have no say in how 'reasonable investment-backed expectations' get defined against them; small landowners without resources to litigate a Penn Central claim through years of appeals are functionally excluded from the compensation regime even when their factual claims resemble winning cases.
% DISAPPEARANCE_RATIONALE: If the categorical/Penn Central hybrid vanished, either a pure physical-appropriation standard (dramatically narrowing compensable takings) or a pure diminution-of-value standard (dramatically expanding them) would have to fill the vacuum — municipal regulatory practice, land-use litigation strategy, and property valuation doctrine would all reorganize around whichever single test replaced the hybrid.
% FOUNDING_PROBLEM: Courts needed a way to decide takings claims that avoided two unworkable extremes: treating every regulation that reduces property value as a taking (which would paralyze government), and treating no regulation short of outright physical seizure as a taking (which would let government destroy property value through regulation without compensation).
% FOUNDING_PROBLEM_CORROBORATION: Government defendants and municipal law associations attest the hybrid remains necessary to preserve regulatory flexibility. Property-rights advocacy organizations and a substantial minority of legal academics (writing independently of any litigation stake) attest that Penn Central's ad hoc balancing has become primarily a mechanism for courts to defer to government almost automatically in the middle zone, meaning the 'balancing' function is largely pretextual outside a narrow band of outlier cases.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the doctrine genuinely protects owners at the extremes while structurally disadvantaging the much larger middle-zone population through procedural cost and outcome unpredictability — the extraction is diffuse and litigation-mediated rather than direct confiscation. Suppression is moderate (0.38): owners are not barred from filing claims, but the multi-factor test's indeterminacy functions as a soft barrier, since most middle-zone claimants cannot afford the multi-year litigation needed to test whether their facts satisfy Penn Central. Theater ratio (0.31) reflects a real but growing gap between the doctrine's stated purpose (principled balancing) and its practical operation (near-automatic deference to government in non-extreme cases) — rising modestly since 1978 as courts have increasingly resolved Penn Central claims via summary judgment for defendants rather than genuine multi-factor weighing.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/court seat, the hybrid appears as principled, non-extractive coordination: bright lines where clarity is achievable, discretion where circumstances vary too much for a rule. From the middle-zone owner's seat, the same structure appears as an engineered escape hatch — government drafts around the two per se triggers and then wins the balancing test in the overwhelming majority of cases, making 'Penn Central review' functionally close to rational-basis deference dressed as heightened scrutiny. The engine computing divergent seat-level types from this data is the expected and correct behavior, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Municipal regulators and reviewing courts are the structural beneficiaries: they get maximal flexibility (regulate everywhere except two narrow, avoidable poles) while bearing minimal compensation exposure. Property owners at the poles are also beneficiaries in the narrow sense that the bright-line rules were built to resolve their cases favorably and predictably. Middle-zone owners are the structural targets: they carry the cost of the doctrine's indeterminacy in the form of litigation expense, multi-year delay, and a balancing test whose outcome is difficult to predict ex ante and often resolves against them. Long-term planners sit in a mixed position — beneficiaries of the flexibility, payers of the resulting relitigation risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding both takings-clause paralysis of government and unconstrained regulatory destruction of property value without compensation) remains partly live — some regulatory disputes genuinely require line-drawing between legitimate police power and confiscation. But the specific hybrid architecture increasingly functions to insulate government from compensation liability across an expanding middle zone rather than to genuinely balance the competing interests the Penn Central factors purport to weigh, which is the contested-status finding reflected in founding_problem_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_balancing_is_genuine_or_pretextual,
    'Does Penn Central''s three-factor balancing test perform genuine case-by-case weighing of economic impact, investment-backed expectations, and character of government action, or has it hardened into near-automatic deference to the regulating government except in a narrow band of outlier fact patterns?',
    'Empirical coding of circuit and Supreme Court Penn Central outcomes over time, tracking win rates for property owners and the degree to which opinions engage all three factors versus disposing of claims via a single dispositive factor (usually economic impact or character of the action).',
    'If balancing is substantially pretextual, the categorical_takings_reading''s claimed coordination function (genuine contextual balancing) is largely theater layered atop a de facto government-favoring rule, pushing the computed type toward snare at the middle-zone-owner seat; if balancing is genuine and outcome-variable, the tangled_rope characterization (real coordination plus real, non-trivial asymmetric cost) is the more accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_balancing_is_genuine_or_pretextual, empirical, 'Whether Penn Central balancing is a genuine multi-factor test or a pretextual deference mechanism.').

omega_variable(
    boundary_line_drawing_indeterminacy,
    'Is the line between ''categorical'' (triggering per se rules) and ''non-categorical'' (triggering Penn Central balancing) itself administrable, or is it manipulable by how government drafts a regulation''s temporal scope, physical footprint, or value-elimination percentage?',
    'Comparative analysis of regulations drafted after Lucas/Loretto specifically to avoid triggering per se review (e.g., temporary rather than permanent occupations, regulations preserving nominal residual value just above zero), tracking whether drafting-around behavior increased post-1992.',
    'If the categorical/non-categorical line is readily manipulable by drafting, the doctrine''s claimed predictability benefit at the poles is substantially undermined, and the effective extraction on middle-zone owners is higher than the doctrine''s formal structure suggests, since government can strategically keep marginal cases out of the favorable per se track.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_line_drawing_indeterminacy, empirical, 'Whether the categorical/balancing boundary is stable or subject to strategic drafting manipulation.').

omega_variable(
    cs_framing_kernel_versus_doctrine_legitimacy,
    'Is the appropriate CS framing this doctrinal test itself (the Loretto/Lucas/Penn Central hybrid as the kernel), or is the deeper kernel the constitutional text (''nor shall private property be taken for public use, without just compensation'') with this hybrid as one layer of judicial interpretation resting atop it?',
    'Track whether future doctrinal shifts (e.g., a narrowing or broadening of Penn Central) are understood by courts and commentators as revising the kernel itself or as reinterpreting a fixed constitutional text; a genuine kernel-level shift would require overruling rather than distinguishing.',
    'If the doctrine itself is the kernel, foreclosure/coexistence relations among the three readings operate directly; if the constitutional text is the deeper kernel, this reading and its siblings are better modeled as competing interpretations layered above a fixed_text kernel, which would change authority_grounding from lineage to a fixed_text-plus-lineage hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_doctrine_legitimacy, conceptual, 'Whether the kernel is the doctrinal test itself or the underlying constitutional text the test interprets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(taki_tr_t1988, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1988, 0.22).
narrative_ontology:measurement(taki_tr_t1998, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1998, 0.26).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.29).
narrative_ontology:measurement(taki_tr_t2016, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1988, 0.34).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(taki_be_t2016, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1988, 0.31).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1998, 0.34).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.36).
narrative_ontology:measurement(taki_su_t2016, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2016, 0.37).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the takings_clause_boundary kernel. physical_appropriation_reading is the narrower sibling (compensation only for direct seizure/permanent occupation, rejecting Penn Central balancing); regulatory_takings_reading is the broader sibling (any regulation 'going too far' in diminishing value triggers compensation, collapsing the categorical/balancing distinction). Each carries its own ε, beneficiary/victim structure, and claimed type; they are not alternative measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
