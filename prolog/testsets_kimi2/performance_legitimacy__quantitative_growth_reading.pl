% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy via Quantitative GDP Growth
 *   domain: political_economy/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates the quantitative_growth_reading of the
 *   performance_legitimacy kernel. State legitimacy is indexed to measurable
 *   GDP growth rates, operationalized through five-year targets, cadre
 *   evaluation systems that reward local officials for hitting growth
 *   numbers, and macroeconomic policy that channels credit toward investment
 *   and industrial expansion. The constraint coordinates national development
 *   but asymmetrically extracts from household consumption and future fiscal
 *   capacity. Sibling readings distribute legitimacy across qualitative
 *   development, technological sovereignty, and livelihood security; this
 *   reading treats aggregate quantitative growth as the primary constraint.
 *
 * KEY AGENTS:
 *   - state_party_apparatus: Agenda-setter (institutional/analytical) â sets GDP targets and operates the cadre evaluation system that encodes growth as legitimacy.
 *   - local_government_officials: Dual-positioned agent (institutional/constrained) â administers local investment and debt to hit targets while benefiting politically from target achievement.
 *   - industrial_export_complex: Beneficiary (powerful/constrained) â receives subsidized credit and regulatory forbearance; expansion is the vehicle for growth.
 *   - household_sector: Payer (moderate/constrained) â bears financial repression, suppressed wage share, and implicit debt liability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.72).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via Quantitative GDP Growth").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '0a2b8ecf-342d-44c5-a670-62e382593ae6').
narrative_ontology:cs_kernel_codification('0a2b8ecf-342d-44c5-a670-62e382593ae6', formalized).
narrative_ontology:cs_authority_grounding('0a2b8ecf-342d-44c5-a670-62e382593ae6', extraction).
narrative_ontology:cs_interpretation_layer_present('0a2b8ecf-342d-44c5-a670-62e382593ae6').
narrative_ontology:cs_reading_relation('0a2b8ecf-342d-44c5-a670-62e382593ae6', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('0a2b8ecf-342d-44c5-a670-62e382593ae6', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a2b8ecf-342d-44c5-a670-62e382593ae6', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('0a2b8ecf-342d-44c5-a670-62e382593ae6', foundational, gdp_growth_as_legitimacy_foundation).
narrative_ontology:cs_axiom_status(gdp_growth_as_legitimacy_foundation, holdable).
narrative_ontology:cs_axiom_grounding('0a2b8ecf-342d-44c5-a670-62e382593ae6', gdp_growth_as_legitimacy_foundation, empirically_contingent).
narrative_ontology:cs_axiom('0a2b8ecf-342d-44c5-a670-62e382593ae6', foundational, aggregate_target_supersedes_distributional_outcomes).
narrative_ontology:cs_axiom_status(aggregate_target_supersedes_distributional_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('0a2b8ecf-342d-44c5-a670-62e382593ae6', aggregate_target_supersedes_distributional_outcomes, conventional).
narrative_ontology:cs_reference_frame('0a2b8ecf-342d-44c5-a670-62e382593ae6', rapid_growth_developmental_state).
narrative_ontology:cs_drift_state('0a2b8ecf-342d-44c5-a670-62e382593ae6', post_2015_slowdown_acknowledgment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a2b8ecf-342d-44c5-a670-62e382593ae6', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national GDP growth targets as a core legitimacy metric, operates the cadre evaluation and promotion system, controls state banks and macroeconomic levers to channel credit toward investment, and defends the quantitative growth narrative in official discourse.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_party_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Administer local economies to meet GDP targets through investment attraction, land sales, debt-financed infrastructure, and project launches. Their political survival, promotion prospects, and fiscal resources depend directly on reported growth rates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary).

% Receives subsidized credit, tax incentives, regulatory forbearance, and implicit guarantees to expand production and exports. Tolerates overcapacity as a necessary condition of the growth target regime.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, constrained, global).

% Bears the cost of the investment-driven model through financial repression on savings, a suppressed wage share relative to output, environmental degradation, and implicit liability for local government debt and overcapacity liquidation. Exit is limited by capital controls and the lack of organized political voice.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_sector, payer,
    moderate, generational, constrained, national).

narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national capital allocation, inter-regional competition, and employment maintenance around a single measurable aggregate target, solving the information and incentive problem of directing investment in a large developing economy.
% TRANSFER_FUNCTION: Transfers surplus from household savers and future taxpayers to the industrial-export complex and local government officials via financial repression, subsidized credit, implicit debt guarantees, and land-fiscal mechanisms, in exchange for reported growth and employment.
% ABSENT_VOICES: Organized consumer advocates, environmental movements prioritizing ecological limits over output, and structural reformers advocating a consumption-led or welfare-state model are present in academic and social discourse but excluded from target-setting and cadre evaluation.
% DISAPPEARANCE_RATIONALE: If the GDP growth legitimacy constraint vanished overnight, cadre evaluation would lose its primary metric, local government financing models built on investment and land sales would collapse, credit channels to heavy industry would contract, and political discourse would reorganize around alternative legitimacy sources such as welfare provision, technological sovereignty, or national security.
% FOUNDING_PROBLEM: Post-socialist underdevelopment and industrial collapse in the late twentieth century: low output, rural poverty, unemployment, and weak state capacity to coordinate capital accumulation.
% FOUNDING_PROBLEM_CORROBORATION: Independent development economists and international institutions (World Bank, IMF, cross-national poverty data) attest that absolute underdevelopment and industrial collapse have been largely resolved. The continuing narrative of developmental urgency is primarily voiced by state-affiliated economists and party documents that benefit from the growth target regime.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the investment-driven model systematically transfers surplus from household savers to industrial borrowers and local government projects through financial repression and implicit guarantees. Suppression (0.72) is higher than extraction because the constraint's persistence depends on active enforcement: cadre evaluation tied to GDP, censorship of counter-narratives, and capital controls that prevent exit to alternative savings vehicles. Theater_ratio (0.45) reflects substantial performative maintenance â local officials inflate figures, build white-elephant infrastructure, and stage project launches to demonstrate target compliance. Accessibility_collapse (0.65) is high because alternative development models are structurally marginalized in official discourse and budgeting. Resistance (0.55) captures internal technocratic pushback and household discontent, but not open political rejection.
 *
 * PERSPECTIVAL GAP:
 *   The state-party apparatus and local officials experience the constraint as a coordination mechanism that solves the incentive problem of directing capital in a large economy. The household sector experiences the same structure as extraction: low deposit returns, environmental costs, and implicit liability for local debt. The industrial complex experiences subsidy. The engine computes these divergent seat types from the structural data; the authored claim (tangled_rope) captures the hybrid nature without collapsing it to either pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The state-party apparatus and local officials sit near the beneficiary end because the constraint subsidizes their authority and careers; the industrial-export complex sits near the beneficiary end due to subsidized credit. The household sector sits near the target end because financial repression and suppressed consumption flow directly from the growth target regime. Exit modulation is low for all non-agenda-setter parties: households face capital controls and political atomization, while the industrial complex faces credit dependence. The agenda-setter itself has analytical exit (it could change the metric) but is identity-locked to the growth model as a source of legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â underdevelopment and industrial collapse in the late twentieth century â is largely solved. The constraint persists because it has become the primary legitimacy mechanism for the state-party apparatus, not because it continues to solve the original coordination problem. This prevents mislabeling the arrangement as a rope (pure coordination) because the coordination function has atrophied into a legitimacy-extraction device. It also prevents mislabeling it as a snare because the genuine historical coordination function (employment, capital accumulation) is real and partially ongoing, even as extraction has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the quantitative_growth_reading of the performance_legitimacy kernel. Would a structural shift to the qualitative_development_reading or livelihood_security_reading require a revision of the core kernel, or can the quantitative growth axiom be subordinated within the same authority framework?',
    'Track official five-year plan language and cadre evaluation criteria: if GDP growth targets remain hard constraints while qualitative indicators are rhetorically appended, the quantitative reading persists; if targets are formally dropped and cadre evaluation shifts to welfare metrics, the reading has transitioned.',
    'If the kernel can absorb subordination without revision, this constraint may drift toward piton (inertial performance); if the kernel must be revised, the transition is a constraint replacement, not a reading shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the quantitative growth reading is contingently subordinate or structurally foundational to the performance legitimacy kernel.').

omega_variable(
    growth_legitimacy_empirical_status,
    'Does continued GDP growth still generate political legitimacy and social stability, or has the relationship decoupled as growth slows and inequality rises?',
    'Panel surveys linking self-reported legitimacy perceptions to regional growth rates, controlling for inequality and environmental quality; or natural experiments from provinces with divergent growth-inflation combinations.',
    'If decoupled, the constraint''s extractiveness is divorced from its claimed coordination function and it drifts toward snare; if still coupled, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_legitimacy_empirical_status, empirical, 'Empirical status of the growth-legitimacy causal claim.').

omega_variable(
    debt_transfer_mechanism,
    'Is the extraction from the household sector explicit (taxation and direct charges) or implicit (local government debt monetization, implicit guarantees, and inflation)?',
    'Audit of local government financing vehicle balance sheets, household wealth surveys, and central bank balance sheet analysis to trace the incidence of debt resolution.',
    'Explicit extraction would raise measured suppression and resistance; implicit extraction through financial repression lowers visible resistance while maintaining high effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_transfer_mechanism, empirical, 'Mechanism of household sector extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_tr_t7, performance_legitimacy__quantitative_growth_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(perf_tr_t14, performance_legitimacy__quantitative_growth_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(perf_tr_t21, performance_legitimacy__quantitative_growth_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement(perf_tr_t28, performance_legitimacy__quantitative_growth_reading, theater_ratio, 28, 0.43).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__quantitative_growth_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perf_be_t7, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(perf_be_t14, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(perf_be_t21, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(perf_be_t28, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t7, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(perf_su_t14, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 14, 0.65).
narrative_ontology:measurement(perf_su_t21, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(perf_su_t28, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel, which decomposes into four structurally distinct constraints. The quantitative_growth_reading treats aggregate output as the legitimacy metric; siblings distribute legitimacy across quality, technology, and welfare. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
