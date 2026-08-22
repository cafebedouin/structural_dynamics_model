% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary — Physical Appropriation Only Reading
 *   domain: constitutional/property/regulatory
 *
 * SUMMARY:
 *   This story instantiates the 'physical appropriation only' reading of the
 *   Takings Clause boundary — the position that only direct physical seizures
 *   or permanent physical occupations of property trigger the Fifth
 *   Amendment's compensation requirement. All regulatory diminutions of
 *   value, no matter how severe, are non-compensable background risks of
 *   ownership. This reading dominated early takings jurisprudence
 *   (pre-Pennsylvania Coal v. Mahon) and persists as a theoretical pole and
 *   in certain doctrinal niches (e.g., physical invasion per se rules). It is
 *   one of three sibling readings of the contested kernel
 *   'takings_clause_boundary'; the others are 'categorical_takings_reading'
 *   (Lucas per se total wipeout + Penn Central) and
 *   'regulatory_takings_reading' (regulations going 'too far' are takings).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.28).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.12).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary — Physical Appropriation Only Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/property/regulatory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '6702ce58-c58a-4356-8359-69741ad125ce').
narrative_ontology:cs_kernel_codification('6702ce58-c58a-4356-8359-69741ad125ce', formalized).
narrative_ontology:cs_authority_grounding('6702ce58-c58a-4356-8359-69741ad125ce', lineage).
narrative_ontology:cs_interpretation_layer_present('6702ce58-c58a-4356-8359-69741ad125ce').
narrative_ontology:cs_reading_relation('6702ce58-c58a-4356-8359-69741ad125ce', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('6702ce58-c58a-4356-8359-69741ad125ce', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('6702ce58-c58a-4356-8359-69741ad125ce', foundational, only_physical_appropriation_triggers_compensation).
narrative_ontology:cs_axiom_status(only_physical_appropriation_triggers_compensation, holdable).
narrative_ontology:cs_axiom_grounding('6702ce58-c58a-4356-8359-69741ad125ce', only_physical_appropriation_triggers_compensation, conventional).
narrative_ontology:cs_axiom('6702ce58-c58a-4356-8359-69741ad125ce', foundational, regulatory_value_diminution_is_background_risk).
narrative_ontology:cs_axiom_status(regulatory_value_diminution_is_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('6702ce58-c58a-4356-8359-69741ad125ce', regulatory_value_diminution_is_background_risk, conventional).
narrative_ontology:cs_reference_frame('6702ce58-c58a-4356-8359-69741ad125ce', founding_era_police_power_plenary).
narrative_ontology:cs_drift_state('6702ce58-c58a-4356-8359-69741ad125ce', post_penn_central_doctrinal_field, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6702ce58-c58a-4356-8359-69741ad125ce', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, legislative_bodies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, municipal_planning_authorities).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulatory_loss).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_use_restrictions).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, police_power_broad_scope).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, regulatory_flexibility_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, background_risk_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce land-use, environmental, and economic regulations without automatic compensation obligations unless physical appropriation occurs. They set the regulatory agenda and define the boundary of what counts as a taking. Their exit is arbitrage-grade: they can shift regulatory tools, reclassify actions, or amend enabling statutes.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Enact statutes that diminish property values (zoning, environmental, historic preservation) while avoiding fiscal liability for compensation. Benefit from regulatory flexibility to pursue public goals without budgetary constraint. Can modify the legal framework itself — exit is structural, not individual.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, legislative_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, legislative_bodies, agenda_setter).

% Implement local land-use plans that restrict development rights without compensation. Gain planning certainty and public goods provision without direct fiscal cost. Exit is constrained: they operate within state enabling acts and political accountability, but can redesign regulatory schemes within broad boundaries.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, municipal_planning_authorities, beneficiary,
    organized, biographical, constrained, local).

% Bear economic losses from regulations that reduce property value, limit use, or impose compliance costs — with no compensation unless physical seizure or permanent occupation occurs. Exit options are constrained: they can sell (often at reduced value), litigate (high cost, low success under this reading), or comply. Political organizing is possible but diffusely effective.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulatory_loss, payer,
    moderate, biographical, constrained, local).

% Face near-total use restrictions (wetlands, endangered species habitat, historic districts) that render property economically idle. No compensation trigger under this reading because no physical occupation occurs. Exit is trapped: cannot develop, cannot sell at meaningful value, litigation foreclosed by precedent. The constraint's boundary is experienced as a one-way extraction.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_use_restrictions, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_use_restrictions, excluded).

% Adjudicate the boundary between regulation and taking. Under this reading, they apply a bright-line physical appropriation test, deferring to legislative judgments on regulatory scope. Their analytical seat sees the full structure: the rule they apply allocates loss systematically to property owners while insulating the fisc.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Advocate for environmental protection, affordable housing, historic preservation — goals advanced by uncompensated regulation. They are excluded from the compensation calculus because their interests are served by the constraint's narrow boundary. Exit is mobile: they can shift forums, strategies, or jurisdictions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocacy_groups, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the cost of public welfare regulation between the public fisc and individual property owners by drawing a bright line: only physical appropriation triggers compensation; regulatory value diminution is a background risk of ownership. This avoids the coordination nightmare of compensating every regulatory loser, which would paralyze governance.
% TRANSFER_FUNCTION: Transfers the economic burden of regulation from the public treasury (which would pay compensation under a broader reading) to property owners who suffer value loss from valid exercises of police power. The transfer is diffuse — each regulation imposes modest losses on many owners, aggregating to substantial public savings.
% ABSENT_VOICES: Future property owners who will inherit a regulatory baseline they did not choose; small landowners lacking litigation capacity; communities bearing concentrated regulatory burdens (e.g., environmental justice areas) whose losses are not aggregated into a compensation claim. They are absent because the constraint defines their losses as non-compensable background risk.
% DISAPPEARANCE_RATIONALE: If this reading vanished and a broader regulatory takings standard applied, governments would face massive new compensation liabilities for zoning, environmental, and land-use regulations. Fiscal discipline would force either dramatic regulatory rollback or tax increases. The regulatory state's operating calculus would fundamentally restructure.
% FOUNDING_PROBLEM: How to permit robust government regulation for public health, safety, and welfare without either (a) bankrupting the treasury with compensation claims for every value-diminishing regulation, or (b) allowing government to destroy property value with impunity. The physical appropriation line was the early Court's answer: a clear, administrable boundary.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Epstein, Merrill) argue the founding problem is dead — the Constitution's text and history demand compensation for regulatory overreach. Living constitutionalists (e.g., Sax, Michelman) and modern Court majorities (Penn Central line) argue the problem is live and the physical appropriation rule is a pragmatic accommodation, not a constitutional command. No consensus outside the benefiting institutional actors.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.28) but nonzero: the constraint systematically allocates regulatory losses to property owners while insulating the public fisc, a structural transfer. However, the coordination function is genuine — a broader rule would impose massive administrative and fiscal costs, potentially paralyzing regulation. Suppression is low (0.12) because the constraint operates through legal doctrine, not coercive enforcement against resistance; property owners comply because the legal consequence of noncompliance is ordinary enforcement, not because the constraint itself suppresses alternatives. Theater is low (0.15) — the rule is straightforward and honestly applied within its domain. Accessibility collapse is moderately high (0.72): once the physical/regulatory line is drawn, alternatives (compensation for regulatory loss) are legally foreclosed. Resistance is moderate (0.38): property owners litigate, lobby, and organize, but the doctrinal barrier is high.
 *
 * PERSPECTIVAL GAP:
 *   From the government regulator seat (agenda_setter, institutional, arbitrage exit), this is a genuine coordination mechanism — a clear rule enabling governance. From the property owner seat (payer, moderate/powerless, constrained/trapped exit), the same structure operates as a one-way loss allocation: they bear costs the public would otherwise pay. The constitutional court seat (observer, analytical) sees the full structure but applies the rule as given. The engine computes per-seat types from these structural asymmetries; this reading claims 'rope' overall because the coordination function is real and extraction is moderate, but the payer seats may compute differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and legislative bodies are structural beneficiaries (d near 0.0): they gain regulatory freedom without fiscal liability. Municipal planners are beneficiaries with constrained exit (d ~0.2). Property owners facing regulatory loss are targets (d ~0.7-0.8): they pay the transfer with limited exit. Landowners under severe use restrictions are near-full targets (d ~0.9): trapped, powerless, bearing concentrated losses. Courts are analytical (d=0.5). Public interest groups are excluded beneficiaries — they gain from the rule but do not administer it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling regulation without fiscal paralysis) remains live — the regulatory state has only grown. But the physical appropriation line has atrophied as the sole boundary: Penn Central (1978) and Lucas (1992) created a more complex doctrinal field where this reading now operates as a residual category (physical invasions) rather than the governing rule. The constraint persists in diminished form — not a piton (the coordination function remains real) but a narrowed rope segment within a tangled doctrinal field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the physical appropriation reading a coherent, stable constitutional position, or is it an artifact of early doctrinal history that no longer functions as an independent reading?',
    'Track whether courts and scholars treat ''physical appropriation only'' as a live interpretive option or as a historical antecedent absorbed into the categorical/regulatory synthesis. A reading that no one affirmatively defends as a complete theory is a zombie reading.',
    'If this reading is a zombie, its low extractiveness metrics describe a constraint that no longer operates independently — the real constraint is the hybrid categorical/regulatory field. Classification would shift from analyzing this reading in isolation to analyzing its residual role in the composite field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this reading exists as a live structural position or a historical fossil').

omega_variable(
    regulatory_loss_as_background_risk,
    'Does treating all regulatory value diminution as ''background risk'' accurately describe the economic reality for property owners, or does it mask a systematic transfer that would be classified as extraction under a different doctrinal framing?',
    'Empirical study of the aggregate value transferred from property owners to the public via uncompensated regulation vs. the administrative cost of a compensation regime. Comparative analysis of jurisdictions with broader takings doctrines.',
    'If the transfer is large and systematic, the ''rope'' classification (coordination with modest extraction) may understate the constraint''s extractive character from the payer seat. The coordination function might be a cover for what is structurally a snare for trapped landowners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_loss_as_background_risk, empirical, 'Whether the background risk framing is descriptive or ideological').

omega_variable(
    physical_regulatory_boundary_stability,
    'Is the line between ''physical appropriation'' and ''regulation'' stable and administrable, or does it collapse under pressure (e.g., permanent flooding, regulatory regimes that functionally occupy land, conservation easements)?',
    'Doctrinal analysis of edge cases: Loretto (cable occupation), Lucas (total wipeout), Tahoe-Sierra (moratoria), Cedar Point Nursery (access easements). If the boundary requires constant patching, its coordination value degrades and theater rises.',
    'Boundary instability increases theater_ratio (doctrinal patches as performative maintenance) and may increase suppression (complexity as barrier to resistance). Could shift classification toward tangled_rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_regulatory_boundary_stability, conceptual, 'Whether the defining boundary of this reading is structurally coherent or fracturing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_physical_approp_tr_t1789, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1789, 0.02).
narrative_ontology:measurement(takings_physical_approp_tr_t1870, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1870, 0.03).
narrative_ontology:measurement(takings_physical_approp_tr_t1922, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1922, 0.05).
narrative_ontology:measurement(takings_physical_approp_tr_t1978, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(takings_physical_approp_tr_t1992, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(takings_physical_approp_tr_t2005, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(takings_physical_approp_tr_t2024, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(takings_physical_approp_be_t1789, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(takings_physical_approp_be_t1870, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1870, 0.08).
narrative_ontology:measurement(takings_physical_approp_be_t1922, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1922, 0.12).
narrative_ontology:measurement(takings_physical_approp_be_t1978, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(takings_physical_approp_be_t1992, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(takings_physical_approp_be_t2005, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2005, 0.27).
narrative_ontology:measurement(takings_physical_approp_be_t2024, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(takings_physical_approp_su_t1789, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1789, 0.03).
narrative_ontology:measurement(takings_physical_approp_su_t1870, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1870, 0.04).
narrative_ontology:measurement(takings_physical_approp_su_t1922, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1922, 0.06).
narrative_ontology:measurement(takings_physical_approp_su_t1978, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1978, 0.09).
narrative_ontology:measurement(takings_physical_approp_su_t1992, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1992, 0.11).
narrative_ontology:measurement(takings_physical_approp_su_t2005, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(takings_physical_approp_su_t2024, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, penn_central_balancing_test).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, lucas_total_wipeout_rule).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the takings_clause_boundary kernel. The three readings form a constraint family linked by network edges. This reading (physical appropriation only) is the historical antecedent; categorical_takings_reading is the modern doctrinal synthesis; regulatory_takings_reading is the expansive property-rights position. Each has distinct ε, victim sets, and classification. The family structure captures the doctrinal evolution and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, organized, 0.15).
constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
