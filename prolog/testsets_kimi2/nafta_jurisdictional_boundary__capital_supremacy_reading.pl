% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA-Style Capital Supremacy Treaty Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint instantiates the capital_supremacy_reading of the
 *   nafta_jurisdictional_boundary kernel. Under this reading, trade agreement
 *   text (exemplified by NAFTA Chapter 11 and successor regimes) functions as
 *   supreme law that overrides domestic regulatory standards, with capital
 *   mobility and regulatory harmonization treated as mandatory treaty
 *   obligations. The structural delta from sibling readings is that domestic
 *   labor and environmental standards enter the victim set, regulatory
 *   agencies lose jurisdictional authority, and extraction flows upward to
 *   capital mobility beneficiaries.
 *
 * KEY AGENTS:
 *   - isds_tribunals: Primary agenda-setter (institutional/analytical) â administers binding dispute resolution interpreting treaty text as supreme.
 *   - multinational_enterprises: Primary beneficiary (powerful/mobile) â captures gains from regulatory harmonization and ISDS access.
 *   - domestic_regulatory_agencies: Primary target (moderate/constrained) â bears loss of jurisdictional authority to treaty tribunals.
 *   - domestic_labor_constituencies and domestic_environmental_constituencies: Secondary targets (powerless/trapped) â bear externalized costs of regulatory chill and downward harmonization.
 *   - trade_law_scholars: Analytical observer (analytical/analytical) â maps the doctrinal divergence between readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA-Style Capital Supremacy Treaty Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '23412301-d73d-4df5-837f-fe095e4aa2e6').
narrative_ontology:cs_kernel_codification('23412301-d73d-4df5-837f-fe095e4aa2e6', formalized).
narrative_ontology:cs_authority_grounding('23412301-d73d-4df5-837f-fe095e4aa2e6', lineage).
narrative_ontology:cs_interpretation_layer_present('23412301-d73d-4df5-837f-fe095e4aa2e6').
narrative_ontology:cs_reading_relation('23412301-d73d-4df5-837f-fe095e4aa2e6', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('23412301-d73d-4df5-837f-fe095e4aa2e6', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('23412301-d73d-4df5-837f-fe095e4aa2e6', foundational, capital_mobility_as_treaty_mandate).
narrative_ontology:cs_axiom_status(capital_mobility_as_treaty_mandate, holdable).
narrative_ontology:cs_axiom_grounding('23412301-d73d-4df5-837f-fe095e4aa2e6', capital_mobility_as_treaty_mandate, conventional).
narrative_ontology:cs_axiom('23412301-d73d-4df5-837f-fe095e4aa2e6', foundational, domestic_regulatory_subordination).
narrative_ontology:cs_axiom_status(domestic_regulatory_subordination, holdable).
narrative_ontology:cs_axiom_grounding('23412301-d73d-4df5-837f-fe095e4aa2e6', domestic_regulatory_subordination, conventional).
narrative_ontology:cs_reference_frame('23412301-d73d-4df5-837f-fe095e4aa2e6', capital_mobility_supremacy).
narrative_ontology:cs_drift_state('23412301-d73d-4df5-837f-fe095e4aa2e6', contemporary_geopolitical_erosion, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('23412301-d73d-4df5-837f-fe095e4aa2e6', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_constituencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate investor-state disputes under trade treaty text, interpreting capital mobility and national treatment clauses as hierarchically supreme over domestic regulatory standards. Issue binding awards that can override legislative and administrative measures. Their mandate is defined by the treaty and accumulated precedent, with limited accountability to domestic publics.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Deploy capital across borders under treaty protections, benefiting from regulatory harmonization and the ability to challenge domestic labor, environmental, and health standards through ISDS. Receive compensation or regulatory rollback when standards are deemed expropriatory or discriminatory. Can relocate operations or forum-shop among treaty jurisdictions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises, beneficiary,
    powerful, biographical, mobile, global).

% Enact and enforce labor, environmental, and health standards within their territorial mandate. Under this treaty reading, their jurisdiction is preempted by trade obligations and tribunal rulings; they must either harmonize standards downward or expose the state to liability. No unilateral exit from the treaty framework.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies, payer,
    moderate, biographical, constrained, national).

% Workers and unions whose wage, safety, and collective bargaining protections depend on domestic regulatory capacity. Bear the costs of regulatory chill and downward harmonization when agencies preemptively weaken standards to avoid ISDS claims. Excluded from treaty dispute proceedings and lack mobility to exit the jurisdiction costlessly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_constituencies, payer,
    powerless, immediate, trapped, national).

% Communities and advocates relying on domestic environmental regulations to protect ecosystems and public health. Face weakened standards when regulations are challenged as barriers to investment. No standing in ISDS and limited ability to relocate from environmentally degraded areas.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_constituencies, payer,
    powerless, immediate, trapped, national).

% Analyze the doctrinal conflict between treaty supremacy and regulatory autonomy, documenting the distributional effects of ISDS jurisprudence. Neither collect from the arrangement nor bear its direct costs; they map the structural divergence between seats.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates cross-border investment and trade by creating binding, predictable rules that reduce regulatory fragmentation and sovereign risk for international capital.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy space from domestic agencies and constituencies to international adjudicators and multinational enterprises, moving extraction upward to capital mobility beneficiaries.
% ABSENT_VOICES: Domestic courts, civil society groups, and affected local communities are structurally excluded from ISDS proceedings; labor and environmental advocates who would argue for regulatory autonomy are not parties to the treaty enforcement framework.
% DISAPPEARANCE_RATIONALE: If the supremacy clause and mandatory capital mobility obligations vanished, domestic regulatory agencies would reclaim jurisdictional authority, labor and environmental standards would no longer face preemption by trade tribunals, and multinational enterprises would lose the ability to directly challenge public interest regulations â the global investment governance architecture would rearrange toward territorial sovereignty.
% FOUNDING_PROBLEM: Post-war fragmentation of international investment rules created uncertainty and risk for cross-border capital; inconsistent regulatory environments were seen as barriers to efficient global resource allocation and development finance.
% FOUNDING_PROBLEM_CORROBORATION: Neoclassical trade economists and multinational enterprise associations attest the problem remains live, citing persistent regulatory barriers in emerging markets. Domestic regulatory agencies, labor unions, and critical political economists attest the founding problem has been overtaken by regulatory chill and democratic deficits; their testimony from outside the capital-mobility beneficiary set supports the shifted-function reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because capital mobility beneficiaries capture regulatory rent through enforceable treaty supremacy. Suppression is high (0.72) because the constraint's persistence depends on ISDS actively overriding domestic law and precluding regulatory alternatives. Theater ratio is moderate (0.42): the coordination function (reducing investment uncertainty) is real, but an increasing share of enforcement activity defends extraction rather than genuine market failure correction. Accessibility collapse is substantial (0.68) because domestic regulatory alternatives are preempted once the treaty framework is in force. Resistance is moderate (0.55) because affected constituencies mobilize politically but lack institutional leverage within the treaty architecture.
 *
 * PERSPECTIVAL GAP:
 *   The multinational enterprise seat experiences the constraint as protective coordination (reduced sovereign risk, predictable rules), while the domestic regulatory and labor seats experience it as coercive extraction (lost autonomy, regulatory chill). The ISDS tribunal seat experiences it as neutral legal interpretation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational_enterprises are declared beneficiaries (low d, damped effective extraction). Domestic_regulatory_agencies, domestic_labor_constituencies, and domestic_environmental_constituencies are declared victims (high d, amplified effective extraction). ISDS tribunals sit near symmetric: they administer the constraint but do not personally collect the extraction. Trade_law_scholars are analytical observers with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfragmented investment rules creating uncertaintyâwas arguably live in the post-war period. Under the capital supremacy reading, the arrangement has persisted and intensified beyond the problem's original scope, accumulating extraction through ISDS expansion. The R5 genealogy flags a potential mandatrophy: founding_problem_status is contested, disappearance_verdict is world_rearranges, and measurements show rising extraction over the interval. The classification as tangled_rope (not rope) depends on the asymmetric victim/beneficiary structure and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_supremacy_naturalness,
    'Is the supremacy of trade agreement text over domestic law a necessary structural feature of international economic integration, or a constructed extraction mechanism benefiting mobile capital?',
    'Comparative analysis of trade agreements with and without ISDS supremacy clauses, measuring regulatory autonomy retention and capital flow volumes.',
    'If supremacy is not necessary for integration, the constraint is more extractive than coordinated; if necessary, a larger share of measured extraction is the price of coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_supremacy_naturalness, conceptual, 'Whether treaty supremacy is structurally necessary or constructed extraction.').

omega_variable(
    regulatory_chill_quantification,
    'To what extent does the threat of ISDS liability cause regulatory chill in domestic standard-setting?',
    'Empirical tracking of withdrawn or weakened regulations in signatory states following ISDS claims or threats.',
    'Confirmed regulatory chill would raise effective extraction and suppression; absence would suggest the constraint is less coercive than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_quantification, empirical, 'Empirical magnitude of regulatory chill from ISDS threat.').

omega_variable(
    capital_mobility_beneficiary_concentration,
    'Do the gains from capital mobility and regulatory harmonization diffuse broadly across economies, or concentrate in specific sectors and investor classes?',
    'Sectoral profit-flow analysis and investment-distribution studies in signatory states.',
    'Concentrated gains support the tangled_rope or snare classification; diffuse gains would support a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_beneficiary_concentration, empirical, 'Whether gains from capital mobility concentrate or diffuse.').

omega_variable(
    kernel_reading_contest,
    'Does the capital_supremacy_reading capture the true structural operation of the treaty, or is the embedded_liberalism_reading a more accurate description of the same institutional arrangement?',
    'Comparative doctrinal analysis of tribunal awards versus treaty preamble language and state regulatory practice.',
    'If the embedded liberalism reading is descriptively more accurate, the constraint''s extraction is lower than this reading measures and the classification shifts toward rope. If capital supremacy is accurate, the extraction and victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between kernel readings for descriptive accuracy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the capital_supremacy_reading of the nafta_jurisdictional_boundary kernel. It is structurally paired with embedded_liberalism_reading and sovereignty_primacy_reading as sibling readings of the same treaty text. The epsilon values differ because the referentâthe standing arrangement under contestâis interpreted through different normative frameworks: supremacy versus balance versus subordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
