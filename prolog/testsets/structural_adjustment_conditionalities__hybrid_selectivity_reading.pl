% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities as Selective Discipline (Hybrid Selectivity Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are formal requirements imposed by
 *   the International Monetary Fund and World Bank on debtor states seeking
 *   financing. These conditions (privatization of public enterprises,
 *   elimination of subsidies, trade liberalization, public sector wage
 *   controls, currency devaluation) are presented as universal, objective
 *   discipline designed to restore fiscal discipline and capital-flow
 *   efficiency. This reading instantiates the hybrid selectivity
 *   interpretation: conditionalities contain genuine coordination benefits
 *   (standardized governance reduces information asymmetry for international
 *   capital) but are systematically enforced with selectivity determined by
 *   geopolitical value. Non-strategic debtors face harsh enforcement;
 *   geopolitically strategic debtors (Egypt, Pakistan, Mexico during peso
 *   crisis, Ukraine post-2022) receive waived or loosened conditions with
 *   minimal penalty. The selectivity is not incidental variation—it is
 *   structural. This constraint exhibits the core dynamic of Tangled Rope: a
 *   coordination mechanism (standardized discipline) coexists with an
 *   extraction mechanism (asymmetric enforcement that uses geopolitical value
 *   as the hidden determinant). The hybrid reading positions selectivity as
 *   irreducible: the system requires the coordination component to justify
 *   capital discipline, and the extraction component to maintain hegemonic
 *   leverage. Removing either component dissolves the constraint into either
 *   pure coordination (Rope, creditor reading) or pure extraction (Snare,
 *   debtor reading). The hybrid reading claims both components are genuine
 *   and entangled.
 *
 * KEY AGENTS:
 *   - Non-strategic Debtor States: Primary victims (powerless/trapped) — face full enforcement of conditionalities with no negotiating power; cannot exit without default and collateral seizure. Examples: Bolivia, Tanzania, Zambia receiving standard terms.
 *   - Geopolitically Strategic Debtor States: Secondary victims with agency (powerful/arbitrage) — receive waived or loosened conditionalities; can negotiate exemptions by leveraging strategic value. Examples: Egypt (Suez Canal), Pakistan (Afghanistan border, nuclear power), Mexico (NAFTA leverage), Ukraine (NATO strategic value).
 *   - IMF/World Bank Apparatus: Primary beneficiary (institutional/arbitrage) — controls discretionary waiver authority; maintains credibility of universal discipline while preserving flexibility to accommodate strategic priorities. Preserves institutional authority.
 *   - Core Creditor States and Financial Institutions: Secondary beneficiary (institutional/arbitrage) — benefit from enforced austerity reducing default risk for non-strategic debtors; benefit from geopolitical leverage over strategic debtors gained through waiver authority.
 *   - International Development Community: Constrained moderate (moderate/constrained) — economists and practitioners see both coordination benefits and extraction costs; career dependent on IMF/World Bank access.
 *   - Populations Subject to Austerity: Tertiary victims (powerless/trapped) — experience welfare loss from privatization, subsidy elimination, wage controls. No direct agency in the constraint; affected through national-level enforcement.
 *   - Analytical Observer: Detects hybrid structure (analytical/analytical) — sees selectivity as structural, not incidental, and as core to the constraint's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.62).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities as Selective Discipline (Hybrid Selectivity Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '673f8104-391f-43f3-9717-79881e266390').
narrative_ontology:cs_kernel_codification('673f8104-391f-43f3-9717-79881e266390', formalized).
narrative_ontology:cs_authority_grounding('673f8104-391f-43f3-9717-79881e266390', extraction).
narrative_ontology:cs_interpretation_layer_present('673f8104-391f-43f3-9717-79881e266390').
narrative_ontology:cs_reading_relation('673f8104-391f-43f3-9717-79881e266390', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('673f8104-391f-43f3-9717-79881e266390', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('673f8104-391f-43f3-9717-79881e266390', foundational, selectivity_is_structural).
narrative_ontology:cs_axiom_status(selectivity_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('673f8104-391f-43f3-9717-79881e266390', selectivity_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('673f8104-391f-43f3-9717-79881e266390', foundational, coordination_extraction_entanglement).
narrative_ontology:cs_axiom_status(coordination_extraction_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('673f8104-391f-43f3-9717-79881e266390', coordination_extraction_entanglement, instrumental).
narrative_ontology:cs_reference_frame('673f8104-391f-43f3-9717-79881e266390', universal_objective_discipline).
narrative_ontology:cs_drift_state('673f8104-391f-43f3-9717-79881e266390', contemporary_post_cold_war_selectivity_visible, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('673f8104-391f-43f3-9717-79881e266390', '2026-02-27T14:32:15Z').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, populations_subject_to_austerity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STRATEGIC DEBTOR STATE (SNARE) — A weak state with no geopolitical value faces enforced conditionalities: privatization, public sector wage freezes, trade liberalization, subsidy elimination. No exit option (cannot access alternative financing without capitulating on core terms; defaulting triggers collateral seizure). Experiences pure extraction masked as coordination.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GEOPOLITICALLY STRATEGIC DEBTOR STATE (SCAFFOLD) — A state of strategic value (military location, resource access, alliance leverage) negotiates waived or loosened conditionalities. Experiences the constraint as temporary coordination: formal compliance theater with de facto exemption. Can exit by switching allegiance or playing creditors against each other. The conditionality apparatus becomes a sunset mechanism — applied selectively, not enforced universally.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CORE CREDITOR INSTITUTION (ROPE) — The institution sees conditionalities as coordination mechanism: enforcing market discipline, preventing moral hazard, standardizing governance. Experiences the constraint as Rope from their institutional context — they benefit from the coordination function (enforced compliance reduces default risk) and from the selective waiver authority (maintains flexibility and geopolitical influence without sacrificing the disciplinary apparatus's credibility).
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL DEVELOPMENT COMMUNITY (TANGLED ROPE) — Economists, development practitioners, and development-focused NGOs see genuine coordination benefits (standardized governance reduces information asymmetry for capital flows) alongside extractive costs (austerity harms populations; conditionality selectivity undermines stated universality). Constrained by career and funding dependence on IMF/World Bank; cannot exit without losing institutional access. Experience mixed extraction and coordination.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS LEGITIMACY NARRATIVE (PITON) — The formal claim that conditionalities are universal, objective, and applied consistently has become substantially theatrical. Post-Cold War evidence shows selective enforcement, waived conditions for strategic allies (Egypt, Pakistan, Mexico during peso crisis), and unchanged conditions for non-strategic states. The narrative persists through institutional inertia — abandoning universality would admit hegemonic selectivity. Theater ratio high because the constraint's actual operation (selective extraction based on geopolitical value) is obscured by universalist rhetoric.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HYBRID SELECTIVITY READING (TANGLED ROPE) — The constraint is genuinely tangled: real coordination function (standardized governance, reduced information asymmetry, legitimate capital-flow discipline) coexists with real extraction (asymmetric enforcement creates a two-tier system where non-strategic debtors bear the full weight while strategic debtors gain negotiating waivers). The selectivity is not incidental — it is structural to the system. The hybrid nature is irreducible: removing the coordination component abandons legitimate capital discipline; removing the extraction component eliminates the hegemonic leverage.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__hybrid_selectivity_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, TR),
    TR >= 0.70.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not maximal. The constraint contains genuine coordination benefits (standardized governance, information transparency, capital-flow discipline) that benefit debtors by enabling access to financing on more favorable terms than alternative (no-discipline-required) lending from private creditors. However, extractiveness is substantially above the rope floor (0.35) because the selectivity ensures that non-strategic debtors bear the full weight of discipline while strategic debtors gain exemptions. The base value of 0.62 reflects that the extraction component is significant and structural—not incidental variation. Suppression (0.68): High. Non-strategic debtors face severe barriers to exit: (1) Default triggers collateral seizure and exclusion from official lending markets; (2) No alternative financing available at comparable terms without accepting stricter discipline from private creditors; (3) Once conditionalities are agreed, reversing them is politically costly (sovereign default risk, currency collapse). Strategic debtors face lower suppression because they can credibly threaten to exit (geopolitical alternative lenders, military realignment). Theater ratio (0.58): Moderate-high. The universalist rhetoric ('objective discipline for all') obscures the selective enforcement mechanism. Post-Cold War evidence shows systematic waiver patterns that contradict universality claims. Theater has increased over time as selectivity has become more visible (Cold War ambiguity about US/Soviet sphere influence gave way to transparent post-Cold War selectivity). The constraint requires the theatrical universality claim to justify the discipline; without the universalist cover story, the selective extraction becomes undeniable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are extreme, reflecting the selectivity mechanism at work. The strategic debtor sees a temporary, negotiable constraint (Scaffold) — can play creditors against each other or invoke geopolitical value to waive terms. The non-strategic debtor sees a permanent extraction mechanism (Snare) — faces enforced austerity with no exit. The creditor institution sees coordination (Rope) — discipline that benefits capital flows. The development community sees mixed extraction and coordination (Tangled Rope) — genuine benefits alongside severe costs. The analytical observer sees the hybrid structure (Tangled Rope) — both coordination and extraction are real and entangled. The Piton perspective (theatrical maintenance of universality) reveals that the constraint's legitimacy depends on obscuring its selectivity. The perspectival gaps between strategic and non-strategic debtors are the core evidence for the constraint's hybrid nature: if the constraint were pure coordination (Rope), all debtors would experience equivalent benefit; if it were pure extraction (Snare), all debtors would experience equivalent harm. The fact that experienced constraint type varies by geopolitical position is the diagnostic signal that selectivity is structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position relative to the extraction flow and their ability to exercise leverage within the constraint. Non-strategic debtors (powerless/trapped) have d ≈ 0.95 — they are full targets with no negotiating power. Strategic debtors (powerful/arbitrage) have d ≈ 0.30 — they are structurally positioned as beneficiaries through waiver authority and threat capacity. The IMF/World Bank apparatus (institutional/arbitrage) has d ≈ 0.15 — they are net beneficiaries controlling the discretionary mechanism. The development community (moderate/constrained) has d ≈ 0.60 — they experience mixed extraction (career dependent on institutions) and victim status (cannot exit without losing access). The asymmetry in d values across agents is precisely the selectivity mechanism: the same formal constraint produces radically different experienced extraction depending on geopolitical positioning. This is the structural signature of Tangled Rope: the beneficiary's arbitrage exit (strategic debtors) coexists with the victim's trapped exit (non-strategic debtors) within a single institutional apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via Hybrid Selectivity reading. The resolution is NOT 'which type is correct?' but 'the constraint is genuinely both coordination and extraction, and the selectivity mechanism determines which agent experiences which component.' The mandatrophy dissolves when the selectivity is recognized as irreducible: the system requires both components to function. The coordination component (standardized discipline) justifies the institutional apparatus and attracts debtor participation (alternative financing is worse). The extraction component (selective enforcement) provides geopolitical leverage for the core powers and the IMF/World Bank. Attempting to remove either component breaks the constraint: universal discipline without waivers eliminates hegemonic leverage; unlimited waivers eliminate the discipline that justifies capital-flow benefits. The Hybrid Selectivity reading resolves the mandatrophy by accepting the irreducible hybridity: this is a Tangled Rope, not a Rope or Snare. The creditor coordination reading and debtor extraction reading both contain valid observations—coordination benefits are real, extraction is real—but neither is complete without acknowledging selectivity as structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_visibility_threshold,
    'At what level of enforcement differential does the system transition from ''contextual application of standards'' (defensible as coordination) to ''transparent two-tier system'' (unambiguous extraction)?',
    'Comparative analysis of conditionality variance across debtor states controlling for economic fundamentals (debt-to-GDP, fiscal deficit, inflation) vs geopolitical variables (military alliance, resource control, strategic location). If geopolitical variables explain >60% of conditionality variance: system is transparently two-tier.',
    'If <40% variance explained by geopolitics: creditor narrative of universal discipline holds; constraint remains Rope from analyst perspective. If >60%: Snare classification becomes defensible; hybrid nature dissolves into transparent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selectivity_visibility_threshold, empirical, 'Variance in conditional enforcement explained by geopolitics vs fundamentals').

omega_variable(
    waiver_enforceability_asymmetry,
    'When a conditionality is formally waived for a strategic debtor but the underlying claim about market discipline remains justified, does the waiver prove that the condition was unnecessary, or that the condition serves a discretionary hegemonic function?',
    'Post-waiver outcome analysis: Do outcomes improve relative to the forecasts conditionality was supposed to ensure? If outcomes are equivalent or better (controlling for initial conditions): condition was extraction, not discipline. If outcomes deteriorate: condition had legitimate disciplinary function.',
    'If waivers reveal unnecessary conditions: ''market discipline'' framing is a cover story for extraction. If waivers reveal real efficiency loss: conditions serve legitimate purpose, but selectivity undermines universality claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waiver_enforceability_asymmetry, empirical, 'Whether conditionality waivers reveal unnecessary conditions or legitimate discipline').

omega_variable(
    coordination_vs_hegemony_observability,
    'Is the genuine coordination function (standardized governance, information transparency) separable from the extraction function (enforced austerity, privatization mandates), or are they necessarily entangled in the institutional apparatus?',
    'Counterfactual design: Would a universal, never-waived conditionality regime (true coordination) deliver equivalent information-standardization benefits while eliminating extraction? If yes: functions are separable and the selectivity IS the extraction mechanism. If no: functions are entangled.',
    'If separable: constraint could be decomposed into two stories (coordination + selective extraction). If entangled: hybrid nature is irreducible; Tangled Rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_hegemony_observability, conceptual, 'Whether coordination and hegemonic extraction functions are separable').

omega_variable(
    creditor_coordination_reading_relation,
    'Does the Hybrid Selectivity reading (selective enforcement reveals structural hegemony) foreclose the Creditor Coordination reading (conditionalities solve capital-flow information asymmetry), or do they coexist as competing legitimate framings?',
    'Framework compatibility test: Can an institution simultaneously hold that conditionalities are universal, objective discipline (creditor coordination axiom) AND that they are selectively enforced based on geopolitical value (hybrid selectivity axiom)? If holding both requires cognitive dissonance or explicit contradiction: foreclosed. If institutions routinely maintain both framings: coexists.',
    'If foreclosed: creditor coordination reading is logically impossible under hybrid selectivity. If coexists: both readings remain live policy positions, allowing institutions to invoke coordination language when standardizing with non-strategic debtors and invoke geopolitical language when negotiating with strategic debtors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coordination_reading_relation, conceptual, 'Whether hybrid selectivity forecloses creditor coordination reading').

omega_variable(
    debtor_extraction_reading_relation,
    'Does the Hybrid Selectivity reading (tangled rope: mixed coordination and extraction) foreclose the Debtor Extraction reading (pure snare: all coordination is cover for extraction), or do they coexist as competing debtor-side perspectives?',
    'Empirical test via credible impact assessment: Do non-strategic debtors show measurable welfare gains (controlling for initial conditions) from any component of conditionality compliance? If yes: some coordination function is real; snare reading is not defensible. If no: coordination benefit is zero; extraction reading is vindicated.',
    'If snare reading is defensible: Hybrid Selectivity reading understates the constraint''s extractive severity. If snare reading is foreclosed: genuine coordination benefits exist even for non-strategic debtors, but are outweighed by extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debtor_extraction_reading_relation, empirical, 'Whether non-strategic debtors receive measurable welfare gains from conditionality compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacond_theater_t0_early_bretton_woods, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sacond_theater_t15_post_cold_war_selectivity_visible, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(sacond_theater_t30_contemporary_full_performance, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(sacond_extractiveness_t0_bretton_woods_establishment, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sacond_extractiveness_t15_selective_enforcement_visible, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(sacond_extractiveness_t30_contemporary, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sacond_suppression_t0_early_enforcement, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sacond_suppression_t15_hardened_enforcement, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(sacond_suppression_t30_stable_high_suppression, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, capital_account_liberalization).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debt_denominated_foreign_currency).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, trade_liberalization_enforcement).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, public_enterprise_privatization).

% DUAL FORMULATION NOTE:
% Structural adjustment conditionalities decompose into multiple structurally distinct constraints: each specific policy mandate (privatization, trade liberalization, wage controls) has its own ε value and its own perspectives. This story captures the meta-constraint: the apparatus that enforces these individual policies selectively based on geopolitical value. The individual policy stories have lower extractiveness (many genuinely serve legitimate coordination functions); the apparatus story has higher extractiveness because selectivity is its defining feature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
