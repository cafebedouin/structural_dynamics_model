% ============================================================================
% CONSTRAINT STORY: income_support_commitment__care_economy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__care_economy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: income_support_commitment__care_economy_reading
 *   human_readable: Income Support Commitment: Care Economy Reading
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   income support commitment. The care economy reading frames income support
 *   through the lens of recognizing and valuing unpaid productive labor —
 *   childcare, eldercare, care for disabled family members — as socially
 *   necessary work that grounds market participation. Under this reading,
 *   income support becomes an acknowledgment that care work is labor (not
 *   merely duty), that its performance benefits society, and that caregivers
 *   should not be economically penalized for performing it. This reading
 *   creates a fundamentally different beneficiary structure and extraction
 *   logic than alternative readings (a freedom floor reading centered on
 *   subsistence or a dependency trap reading centered on labor market
 *   lock-in). The care economy reading sits at the boundary between genuine
 *   coordination (recognizing care as labor, redistributing care burden,
 *   enabling caregiver participation in market work) and extractive
 *   exploitation (using 'recognition' rhetoric to legitimate low wages for
 *   formal care workers and continued unpaid labor by informal caregivers).
 *   The constraint's structural signature reflects this ambiguity: moderate
 *   extractiveness, moderate theater, and asymmetric beneficiary/victim
 *   positioning that depends on which actors are measured.
 *
 * KEY AGENTS:
 *   - Unpaid caregivers (parents, adult children caring for elders, disability care providers): Structurally trapped by care obligations; primary beneficiary-target of the reading's framing; also primary victim of ongoing unpaid labor.
 *   - Formal care sector workers (childcare providers, home health aides, nursing assistants): Employed in care work; wages legitimized as partially subsidized by income support rather than market compensation; victim (wage suppression via recognition rhetoric) and ambiguous beneficiary (income support framework improves legitimacy of care work as labor).
 *   - Taxpayers funding income support: Distributed group; high-income households experience arbitrage benefit (access to low-wage care labor); moderate-income households experience mixed coordination and extraction.
 *   - Market-wage employers (firms, industries dependent on flexible worker availability): Beneficiary via coordination (income support enables worker scheduling flexibility, reduces childcare cost burden on workers, reduces wage pressure in care sectors).
 *   - Care dependent populations (infants, elderly, disabled): Primary beneficiaries of care provision; structurally unable to exit or resist; dependent on caregiver availability and quality.
 *   - Political advocates for care work recognition (feminist economists, disability justice movements, care worker unions): Organized agents pushing the care economy reading; see the constraint as pure coordination and political opportunity.
 *   - Analytical observer: Risks naturalizing contingent institutional choice (unpaid care as default) as inevitable social fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__care_economy_reading, 0.38).
domain_priors:suppression_score(income_support_commitment__care_economy_reading, 0.52).
domain_priors:theater_ratio(income_support_commitment__care_economy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__care_economy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__care_economy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__care_economy_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__care_economy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__care_economy_reading, "Income Support Commitment: Care Economy Reading").
narrative_ontology:topic_domain(income_support_commitment__care_economy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_commitment__care_economy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__care_economy_reading, '1987b853-1bdb-4742-9d3a-a6bb820df465').
narrative_ontology:cs_kernel_codification('1987b853-1bdb-4742-9d3a-a6bb820df465', formalized).
narrative_ontology:cs_authority_grounding('1987b853-1bdb-4742-9d3a-a6bb820df465', extraction).
narrative_ontology:cs_interpretation_layer_present('1987b853-1bdb-4742-9d3a-a6bb820df465').
narrative_ontology:cs_reading_relation('1987b853-1bdb-4742-9d3a-a6bb820df465', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('1987b853-1bdb-4742-9d3a-a6bb820df465', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('1987b853-1bdb-4742-9d3a-a6bb820df465', foundational, unpaid_care_is_productive_labor).
narrative_ontology:cs_axiom_status(unpaid_care_is_productive_labor, holdable).
narrative_ontology:cs_axiom_grounding('1987b853-1bdb-4742-9d3a-a6bb820df465', unpaid_care_is_productive_labor, deontological).
narrative_ontology:cs_axiom('1987b853-1bdb-4742-9d3a-a6bb820df465', foundational, care_recognition_enables_market_participation).
narrative_ontology:cs_axiom_status(care_recognition_enables_market_participation, holdable).
narrative_ontology:cs_axiom_grounding('1987b853-1bdb-4742-9d3a-a6bb820df465', care_recognition_enables_market_participation, instrumental).
narrative_ontology:cs_reference_frame('1987b853-1bdb-4742-9d3a-a6bb820df465', care_work_as_socially_valuable).
narrative_ontology:cs_drift_state('1987b853-1bdb-4742-9d3a-a6bb820df465', contemporary_labor_market_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1987b853-1bdb-4742-9d3a-a6bb820df465', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__care_economy_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__care_economy_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__care_economy_reading, care_dependent_populations).
narrative_ontology:constraint_beneficiary(income_support_commitment__care_economy_reading, market_wage_earners_via_care_access).
narrative_ontology:constraint_victim(income_support_commitment__care_economy_reading, taxpayers_subsidizing_care).
narrative_ontology:constraint_victim(income_support_commitment__care_economy_reading, care_sector_workers_at_wage_floor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FULL-TIME UNPAID CAREGIVER (SNARE) — Structurally trapped by caregiving obligation (children, elderly parent, disabled family member). Cannot exit without abandoning dependents. Income support in this reading is framed as recognition/validation, but the caregiver remains trapped within the caregiving role itself. The constraint is not the income support mechanism but the broader social organization that treats caregiving as unpaid obligation. Experiences pure extraction: labor is extracted (demanded by society), suppression is high (exit via market employment requires institutional childcare that may not exist or may consume most earnings), and theater is present (the 'recognition' of care work often masks its continued devaluation).
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARTIALLY EMPLOYED PARENT (TANGLED ROPE) — Combines paid work with unpaid care (part-time employment, flexible arrangements, care gig work). Income support provides partial subsidy; also benefits from reduced full-time work pressure. Mixed experience: some extraction (uncompensated care hours), some genuine coordination (income support + informal arrangements + care infrastructure enable household stability). Suppression is moderate (can exit via full-time employment or care outsourcing, but at significant cost). Not trapped, not fully mobile — constrained by care obligations and labor market structure.
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER / MARKET-WAGE WORKERS' INSTITUTIONAL CONTEXT (ROPE) — Benefits from the care economy recognition reading because it legitimizes low wages in formal care sectors (childcare, elder care, nursing) and subsidizes household-level unpaid care that reduces employer burden for worker schedule flexibility. Experiences the constraint as pure coordination: income support enables worker availability, reduces turnover costs, and legitimizes low formal-sector care wages as 'compensated labor' even when compensation is minimal. Has arbitrage options (can lobby for tax benefits, shift costs to state). From this perspective, the constraint solves a coordination problem without extraction.
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CARE SECTOR UNION / ADVOCACY COALITION (ROPE) — Organized agents (childcare unions, care worker associations, feminist economists, disability justice advocates) see income support for unpaid care as a coordination mechanism that legitimizes care work as labor and redistributes social burden. Mobile (can shift advocacy focus, exit particular policy battles). The constraint solves collective action: recognizing unpaid care work politically increases pressure to fund care infrastructure and professionalizes care labor. From this perspective, the constraint is pure coordination with real beneficiaries (organized care workers, caregivers) and no significant extraction — the benefit flows to intended targets.
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-INCOME HOUSEHOLD (TANGLED ROPE) — Pays income support taxes but experiences net benefit from the care economy recognition if it outsources care to lower-wage formal or informal workers (nannies, housekeepers, eldercare providers) whose wages are held low by both unpaid labor competition and care economy categorization. Experiences genuine coordination (access to care labor at below-market rates) + extraction (pays subsidies that ultimately go to low-wage care workers rather than being returned as household-level benefit). The constraint both coordinates and extracts. Has arbitrage options (can exit via market purchases of premium care, relocation, tax optimization). Effective extractiveness from this perspective is moderate — some extraction burden is offset by coordination benefits.
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL CARE SECTOR WORKER AT WAGE FLOOR (SNARE) — Employed in formal care work (childcare provider, home health aide, elder care worker) at or near minimum wage. Income support legitimizes low wages by reframing care work as partially a social commitment (subsidized by society) rather than market labor. Worker is trapped: exit from care sector is difficult (limited transferable credentials, care qualification lock), and wages remain suppressed by the framing that society partially 'funds' care through income support rather than employers paying full market wage. Experiences pure extraction: labor is extracted at below what full-market rate would be; suppression is high (care credential lock + emotional labor + physical dependency of care recipients makes exit costly); theater is high (the reading 'recognizes' care work while keeping wages low).
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, some unpaid care work may be inherent to human social reproduction — parental care of infants, end-of-life care, emotional labor in intimate relationships. The constraint might appear as a natural feature of any society: care must be performed, and any society must organize who performs it and how its legitimacy is framed. This perspective risks naturalizing what is actually a contingent institutional choice (unpaid care as default vs. publicly funded care as social infrastructure). The engine may flag this as a false summit if the reading's actual structure shows identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__care_economy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_commitment__care_economy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(income_support_commitment__care_economy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting the tangled structure. The constraint coordinates genuine care provision (caregivers receive rhetorical and sometimes financial recognition; care-dependent populations receive care). But it simultaneously extracts by legitimizing below-market wages in formal care work and normalizing unpaid care labor by framing it as a valued commitment rather than compensable labor. The value is not as low as pure coordination (0.10–0.15) because suppression is present (caregivers cannot easily exit), but not as high as pure extraction (0.70+) because some genuine benefits flow to caregivers and genuine coordination solves real collective action problems (how to organize care in a market economy). The rising trajectory over the interval (0.28 → 0.38) reflects growing extractiveness as the rhetoric of 'care economy recognition' becomes established without corresponding wage increases for formal care workers or exit options for unpaid caregivers. Suppression (0.52): Moderate-high, reflecting caregiving obligation lock (biological dependency of infants, legal obligations for parental care, care credential lock for formal workers), emotional labor barriers to exit, and inadequate childcare infrastructure. Suppression is not total (0.85+) because some caregivers can and do exit via outsourcing, market employment, or care system alternatives. Theater ratio (0.48): Moderate, below the piton threshold (0.70). The constraint includes genuine functional elements (care is performed, income support does reach some intended beneficiaries) but includes performative elements (recognition rhetoric without wage backing, policy framing that treats care work as intrinsically different/lower-value, legitimacy claims that mask continued extraction).
 *
 * PERSPECTIVAL GAP:
 *   The care economy reading produces maximum perspectival divergence because it names care work as the beneficiary rather than treating caregivers as dependent/marginalized. The full-time unpaid caregiver sees snare (trapped, no exit). The formal care sector worker sees snare (wages suppressed by recognition rhetoric). The employer sees rope (coordination benefit from reliable worker availability). The care sector union sees rope (political opportunity to legitimize and resource care work). The high-income household sees tangled rope (pays taxes, receives care access benefit, experiences mixed flows). The analytical observer risks seeing mountain (naturalizing care organization as inherent law). The gap between snare and rope perspectives reveals the constraint's hybrid character: it is simultaneously pure extraction (for trapped caregivers and low-wage care workers) and pure coordination (for organized advocates and beneficiary populations receiving care).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Unpaid caregivers are victims with trapped exit (d ≈ 0.95, f(d) ≈ 1.42, maximum experienced extractiveness). Formal care workers are victims with constrained exit (care credential lock limits mobility; d ≈ 0.85, f(d) ≈ 1.15). Employers are beneficiaries with arbitrage options (d ≈ 0.15, f(d) ≈ -0.01, negative experienced extractiveness). Care sector unions are beneficiaries with mobile options (d ≈ 0.40, f(d) ≈ 0.40, low positive extractiveness that they experience as coordination). Taxed households are mixed (beneficiary via care access, victim via tax burden; d varies by income level and care needs). The engine derives these directionalities from the declared beneficiary/victim groups and exit options; the commentary layer explains why each group occupies its structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (ambiguous categorization as both coordination and extraction) is NOT resolved by this constraint; it is the constraint's core diagnostic feature. The care economy reading simultaneously performs genuine coordination (recognizing care as labor, redirecting resources toward care provision, enabling caregiver participation in market work) and extractive constraint (legitimizing low wages for care workers, normalizing unpaid care labor, suppressing exit options for full-time caregivers). These are not illusory categories from two different perspectives — they are simultaneous structural features. The constraint is Tangled Rope precisely because it genuinely coordinates AND genuinely extracts. Resolving the mandatrophy would require either: (a) showing that recognition rhetoric produces actual wage increases and exit options (would shift toward pure Rope), or (b) showing that the care economy framing is pure cover story for wage suppression with no functional coordination benefit (would shift toward pure Snare). The measurement trajectory (rising extractiveness over time despite stable care rhetoric) suggests motion toward pure Snare as the coordination promise fails to materialize, but the constraint remains Tangled Rope as long as genuine care coordination occurs alongside extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    care_vs_commodity_boundary,
    'Is ''recognizing non-market productive labor'' a structural reclassification of care as socially valuable, or does it naturalize care as intrinsically different from market labor and therefore inherently lower-wage?',
    'Comparison of wage trajectories: formal care sectors in jurisdictions where care is rhetorically ''recognized'' vs. those where care is explicitly commodified. Tracking whether recognition rhetoric correlates with wage increases or wage suppression relative to comparable skilled work.',
    'If recognition increases wages: the constraint is genuine coordination (beneficiaries gain). If recognition masks wage suppression: the constraint is extractive (framing legitimizes low wages). This determines whether the care_economy_reading is Rope or Tangled Rope from the worker perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(care_vs_commodity_boundary, empirical, 'Whether care recognition raises or suppresses wages for care workers').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the ''income_support_commitment'' kernel. The contested question is: what commitment does an income support system instantiate? Is it a commitment to recognize care work as productive (this reading), to provide a freedom floor independent of market participation (freedom_floor_reading), or to manage the risks of market dependency (dependency_trap_reading)?',
    'Textual analysis of income support legislation, policy justifications, and case law; ethnographic documentation of how different political actors frame income support; longitudinal tracking of policy design evolution.',
    'The reading chosen shapes who is categorized as beneficiary vs. victim. Care economy reading puts caregivers as beneficiaries (recognition focus). Freedom floor reading puts all income-support recipients as beneficiaries (subsistence focus). Dependency trap reading puts workers exiting the program as victims (lock-in focus). The three readings cannot coexist in a single policy frame, but they can coexist across different nations, eras, or political coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the income support commitment kernel applies in this jurisdiction and era').

omega_variable(
    unpaid_care_exit_costliness,
    'What is the actual cost structure for unpaid caregivers exiting caregiving? Does it vary by care type (child, elder, disabled adult), market context (urban/rural), and infrastructure availability?',
    'Longitudinal studies of caregiving exit patterns; cost-benefit analysis of childcare outsourcing in different jurisdictions; tracking of caregiver employment before/after care responsibility shifts.',
    'If exit costs are uniformly high (trapped profile): snare classification is robust. If exit costs vary widely by context: some caregivers are trapped, others constrained. This shapes whether the powerless/trapped perspective is stable or context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unpaid_care_exit_costliness, empirical, 'Cost structure and variability of unpaid caregiver exit').

omega_variable(
    income_support_flow_traceability,
    'When income support is framed as ''recognizing care work,'' does the actual fiscal flow match the rhetorical intent? Does support reach unpaid caregivers, formal care workers, or primarily subsidize market-wage households'' purchase of care?',
    'Fiscal flow analysis of income support: who receives how much and at what condition; tracing tax incidence of income support funding; comparing intended beneficiaries (caregivers) to actual recipients (may include employers, care institutions, other taxpayer groups).',
    'If flow reaches caregivers: coordination function is real. If flow is captured by employers or other groups: the constraint is more extractive than the care economy reading suggests. This determines the magnitude of extractiveness and the plausibility of the rope vs tangled rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_support_flow_traceability, empirical, 'Actual fiscal flow of income support to declared beneficiaries').

omega_variable(
    care_economy_reading_foreclosure,
    'Does the care economy reading foreclose the dependency trap reading? If income support is framed as recognition of care work, can the same system simultaneously be understood as creating lock-in to care dependency?',
    'Logical/normative analysis: can both readings be held in the same policy framework? Can a policymaker recognize care work AND worry about lock-in simultaneously, or do the framings contradict?',
    'If readings foreclose each other: they are structurally incompatible; policymakers must choose one framing. If readings coexist: different actors hold different framings of the same policy; the policy is structurally ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_economy_reading_foreclosure, conceptual, 'Whether care economy and dependency trap readings logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__care_economy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(care_econ_theater_t0, income_support_commitment__care_economy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(care_econ_theater_t5, income_support_commitment__care_economy_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(care_econ_theater_t10, income_support_commitment__care_economy_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(care_econ_extract_t0, income_support_commitment__care_economy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(care_econ_extract_t5, income_support_commitment__care_economy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(care_econ_extract_t10, income_support_commitment__care_economy_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(care_econ_suppress_t0, income_support_commitment__care_economy_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(care_econ_suppress_t5, income_support_commitment__care_economy_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(care_econ_suppress_t10, income_support_commitment__care_economy_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__care_economy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(income_support_commitment__care_economy_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__care_economy_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__care_economy_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__care_economy_reading, formal_care_sector_wage_suppression).
narrative_ontology:affects_constraint(income_support_commitment__care_economy_reading, unpaid_care_labor_lock_in).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel admits three structurally distinct readings, each a separate constraint story with different ε values and beneficiary/victim structures. All three readings operate the same policy mechanism (income support cash/benefits) but frame it against different normative commitments (care recognition vs. freedom floor vs. dependency management). This file (care_economy_reading) decomposes the ambiguous kernel into one clean constraint; sibling readings are separate files. The network links all three readings plus two downstream constraints (formal sector wage suppression and unpaid care lock-in) that are affected by the reading chosen.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__care_economy_reading, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
