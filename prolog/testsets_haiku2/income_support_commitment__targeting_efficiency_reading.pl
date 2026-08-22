% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted Income Support Over Universal Distribution (Efficiency Reading)
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   The targeting efficiency reading of the income support commitment asserts
 *   that public resources are best deployed by concentrating support on those
 *   with demonstrated greatest need rather than distributing unconditionally
 *   to all citizens. This is one reading of a contested kernel about what
 *   income support should do. Under this reading, the constraint does genuine
 *   coordination work (solves the allocation problem where budgets are
 *   finite) AND extractive work (concentrates administrative power, creates
 *   surveillance overhead, generates benefit cliffs that penalize work, and
 *   disadvantages recipients relative to what universal distribution would
 *   deliver). The same recipients are both the claimed beneficiaries (support
 *   is targeted at them) and the actual victims (they lose in the shift from
 *   targeting to universals, and they bear the compliance costs of the
 *   targeting apparatus). This is the classic snare structure: coordination
 *   cover over extractive machinery.
 *
 * KEY AGENTS:
 *   - means_tested_recipients: powerless, trapped exit — nominal beneficiaries and actual victims of the constraint's dual function
 *   - program_administrators: institutional power — set and defend the targeting rules, collect authority and permanence from the constraint
 *   - fiscal_conservatives: powerful, mobile — benefit from the budget discipline and moral framing of rationed welfare
 *   - middle_income_tax_base: organized, mobile — benefit from the targeting logic's justification of limited redistribution
 *   - universal_income_advocates: excluded, moderate power — would dispute the targeting reading but kept outside the operative decision space
 *   - welfare_eligibility_auditors: institutional, identity_locked — professional identity fused to the targeting apparatus, collect power to investigate
 *   - labor_economists: institutional, analytical — produce evidence on work disincentives that vindicates the targeting reading
 *   - dependent_care_providers: moderate, constrained — receive targeted support but insufficient to cover costs, forced into supplementary work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.71).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted Income Support Over Universal Distribution (Efficiency Reading)").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '7dfd8975-3479-400b-85f0-9ee0bb07b2d6').
narrative_ontology:cs_kernel_codification('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', formalized).
narrative_ontology:cs_authority_grounding('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', extraction).
narrative_ontology:cs_interpretation_layer_present('7dfd8975-3479-400b-85f0-9ee0bb07b2d6').
narrative_ontology:cs_reading_relation('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', income_support_commitment__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', foundational, scarcity_doctrine_foundational).
narrative_ontology:cs_axiom_status(scarcity_doctrine_foundational, holdable).
narrative_ontology:cs_axiom_grounding('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', scarcity_doctrine_foundational, empirically_contingent).
narrative_ontology:cs_axiom('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', foundational, moral_hazard_framework_foundational).
narrative_ontology:cs_axiom_status(moral_hazard_framework_foundational, holdable).
narrative_ontology:cs_axiom_grounding('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', moral_hazard_framework_foundational, empirically_contingent).
narrative_ontology:cs_axiom('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', secondary, targeting_efficiency_instrumental).
narrative_ontology:cs_axiom_status(targeting_efficiency_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', targeting_efficiency_instrumental, instrumental).
narrative_ontology:cs_reference_frame('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', welfare_budget_constraint_framework).
narrative_ontology:cs_drift_state('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', contemporary_evidence_proliferation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7dfd8975-3479-400b-85f0-9ee0bb07b2d6', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, program_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_income_tax_base).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, means_tested_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, means_tested_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, dependent_care_providers).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, welfare_scarcity_doctrine).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, moral_hazard_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive targeted income support conditioned on demonstrated need through TANF, SNAP, housing assistance, Medicaid. Their situation exemplifies the constraint's dual character: they are the nominal beneficiaries (the constraint is designed to help them) and the actual victims (the constraint extracts from them through verification overhead, benefit cliffs that penalize work, and the administrative costs that reduce what they receive). A parent earning $31,100 annually receives approximately $31,100 in targeted benefits; if the same budget were converted to universal distribution, they would receive approximately $50,000 annually (scaled to national per-capita distribution). The targeting apparatus forces them to repeatedly verify need, restricts their work hours to stay eligible, and requires them to accept the identity of 'deserving poor' to justify the support. Exit means earning above the threshold (losing eligibility abruptly) or accepting poverty without support.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, means_tested_recipients, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, means_tested_recipients, beneficiary).

% Design, operate, defend, and expand the means-testing apparatus: income verification systems, asset tests, categorical eligibility rules (TANF requires work, SNAP has asset limits, Medicaid has medical categories). Collect institutional authority, budget control, employment, and permanent organizational standing from the constraint's existence and complexity. Justify the system publicly as preventing waste, targeting resources efficiently to those in greatest need, and preserving work incentives. Their institutional mandate depends on maintaining the distinction between the deserving and undeserving poor and on complexity that requires their expertise to navigate.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, program_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the constraint's budget discipline and the moral framing that welfare must be rationed to preserve work incentives and fiscal sustainability. The constraint validates their political narrative that unconditional transfers create dependency, erode work ethic, and constitute irresponsible spending. Their direct interest is in keeping the total size of redistribution capped — targeted programs accomplish this more convincingly than universals because the targeting frame provides moral justification for the limits. They have the political power to set the constraint's terms and defend it against challenge.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives, beneficiary,
    powerful, biographical, mobile, national).

% Benefits from the constraint's targeting logic and the reassurance it provides. The targeting narrative justifies redistribution as necessary only for the provably needy, reducing the total redistributive burden on middle-income taxpayers and reassuring them that public resources are not squandered on 'free riders' or universal 'welfare bums.' Their political support for any redistribution depends on confidence that it is aimed and proportionate, not punitive or universally profligate. They organize around tax policy and welfare reform to preserve this targeting logic.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_income_tax_base, beneficiary,
    organized, biographical, mobile, national).

% Are excluded from the operative decision space of welfare policy because their core proposal (unconditional, universal income support) challenges the constraint's foundational premise. They argue that universal distribution reduces administrative overhead (saving 15-20% of current budget for delivery rather than verification), eliminates benefit cliffs (removing the work penalty), preserves dignity by removing the need to prove deserving-ness, serves the targeted population better dollar-for-dollar, and simplifies the political space by removing the moral hazard narrative. Their exclusion is maintained by the fiscal conservative coalition's control of the legislative and administrative agenda, by the institutional entrenchment of the targeting apparatus, and by the targeting reading's intellectual dominance in the policy class.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_income_advocates, excluded,
    moderate, biographical, constrained, national).

% Perform the verification and monitoring work the targeting apparatus requires: income documentation, asset discovery, housing status verification, work-search reporting review. Their professional identity and employment are directly fused to the constraint's complexity and persistence. They benefit from the institutional power to investigate recipients' circumstances, demand documentation, and make determining decisions about eligibility. They are invested in the legitimacy of the targeting mission and in the assumption that verification is necessary and appropriate. Exit from the constraint would mean loss of professional role, retraining, and loss of the institutional power they currently hold.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_eligibility_auditors, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, welfare_eligibility_auditors, observer).

% Produce empirical evidence on behavioral responses to income support design: labor supply elasticity, work disincentive effects of means testing versus unconditional transfers, benefit cliff effects. They are analytical seats that feed into legitimacy debates without being direct parties. Their evidence on work disincentives from unconditional transfers is cited to validate the targeting efficiency reading's claim that universals would reduce work incentives unacceptably. However, their evidence does not settle the normative question of whether reducing work incentives is a cost worth bearing in exchange for eliminating overhead and benefit cliffs.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, labor_economists, observer,
    institutional, biographical, analytical, global).

% Receive targeted support for childcare, eldercare, and disability care expenses (subsidized care, dependent care tax credits, Medicaid coverage of care services). They are nominal beneficiaries of the constraint but actual victims: the targeted support is designed to help them but is often insufficient to cover actual care costs (childcare costs $10,000-$15,000 annually per child in urban areas; subsidies typically cover $3,000-$5,000). They are forced into supplementary work, debt, or family care arrangements. The constraint's verification requirements create administrative friction that reduces effective benefit delivery and forces them to repeatedly prove their need.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, dependent_care_providers, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, program_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the allocation problem where public welfare budgets are finite: distributes limited resources to those with the greatest demonstrated need rather than spreading them universally across the entire population, so each recipient of the targeted pool receives a larger per-person benefit than they would under universal distribution.
% TRANSFER_FUNCTION: Moves public funds from taxpayers (middle-income and above) to recipients below a specified income threshold, conditioned on documented need and behavioral compliance. Also moves administrative authority and institutional power from the democratic process to the targeting apparatus (program administrators, auditors, regulators). Also extracts administrative overhead and compliance costs from recipients themselves in the form of verification labor, benefit cliff penalties, and identity conformance costs.
% ABSENT_VOICES: Universal income advocates, recipients who would prefer unconditional support, labor economists who study work disincentives but question the normative interpretation, and the beneficiaries of the cost reduction that universal distribution would enable are excluded from the operative decision space. They are kept out by the fiscal conservative coalition's control of the legislative and administrative agenda and the intellectual entrenchment of the targeting reading in the policy class.
% DISAPPEARANCE_RATIONALE: If the targeting efficiency reading of the income support constraint disappeared and was replaced by universal income distribution at equivalent budget, the world would reorganize substantially: the administrative apparatus would collapse (eliminating 15-20% overhead), benefit cliffs would vanish (removing work disincentives), the same recipients would receive higher net transfers ($19,100 more annually in the median example), the distinction between deserving and undeserving would no longer organize the policy space, and the institutional power to determine who is worthy of support would shift from administrators to the law itself. The dependency trap reading would also change because the work disincentive would operate differently in a universal context.
% FOUNDING_PROBLEM: Public welfare budgets are finite; if all citizens receive unconditional support, either the amount per person becomes negligible (universal pittance that helps no one) or the total tax burden becomes unsustainable (forcing capital flight, reducing growth, or becoming politically infeasible). The founding problem is: how do you provide meaningful support to those in greatest distress without either spreading so thin that you help no one or taxing the productive base so heavily that you destroy the economy?
% FOUNDING_PROBLEM_CORROBORATION: Fiscal conservatives and program administrators attest the founding problem is live: the tension between adequacy and sustainability persists and proves intractable. They cite budget constraints and work disincentive evidence. Universal income advocates contest this, citing evidence that the budget constraint is partly constructed (chosen tax rates, chosen exemptions for capital income, chosen growth models that prioritize property rights over redistribution) rather than natural, and that distributional efficiency actually improves under universals because overhead shrinks. They argue that a universal income at $15,000 annually per adult (within most welfare budgets) is more adequate than the current targeted system because it avoids cliffs and overhead. Labor economists provide evidence on work disincentive magnitudes but do not adjudicate whether the founding problem justifies the current targeting arrangement. The contest is live and unresolved.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint concentrates gains (administrative authority, budget discipline, moral legitimacy of rationing) on a coalition of administrators and fiscal conservatives while concentrating costs (overhead, surveillance, benefit cliffs, forgone income from lost work) on recipients. The shift from targeting to universal distribution would move $19,100 annually to the median recipient in the example while eliminating the administrative overhead that currently consumes roughly 15-20% of the targeted budget — the recipients lose under universals because the same total budget dilutes across the entire population rather than concentrating on the needy. Suppression (0.71) is high because the constraint's persistence depends on actively excluding universal alternatives from the operative policy space: the targeting narrative must be defended against the empirical claim that universals deliver more efficiently and against the normative claim that dignity is better served by unconditional support. Theater (0.42) is moderate-low: the efficiency narrative is real (targeting does concentrate resources), but an increasing share of the system's activity serves to maintain the verification apparatus and defend the targeting reading against its challengers, rather than serving recipients. The accessibility_collapse (0.48) is moderate because alternatives to the targeting reading exist and are articulated by excluded parties — the constraint does not collapse alternatives as completely as a natural law would. The resistance (0.62) is moderate-to-high because the constraint meets real resistance from recipients (who experience the benefit cliffs and surveillance as punitive), from universal income advocates (who dispute the efficiency claim), and from labor economists who have produced evidence on work disincentives but whose data do not settle the normative question. All measurements use a shared time grid (every metric at every time point) so temporal analysis can proceed without interpolation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (program administrators, fiscal conservatives) perceives the constraint as genuine coordination: necessary discipline on scarce resources, prevention of waste, targeted support to the provably needy. From this seat, the constraint is legitimate because the founding problem is live and targeting is the rational solution. The payer seat (recipients) perceives the same constraint as extraction: they see the surveillance, the benefit cliffs, the administrative overhead, and the fact that under universals they would receive more in absolute terms ($19,100 more annually in the median case). From this seat, the constraint is extractive machinery dressed up in the efficiency narrative. The engine computes both perspectives from the structural data — the authored claim reflects the targeting reading's own framing (rope-level coordination), while the authored metrics reflect what actual operation looks like (snare-level extraction). The divergence is intentional and diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Means-tested recipients are full targets: the constraint extracts from them (administrative overhead, benefit cliffs, surveillance costs, forgone income). Their d is near 1.0. Program administrators and fiscal conservatives are beneficiaries: they collect authority, institutional permanence, and legitimacy from the constraint. Their d is near 0.0-0.25. Middle-income taxpayers are mild beneficiaries: they benefit from the budget discipline and reassurance that redistribution is aimed rather than universally profligate. Their d is around 0.15-0.30. Universal income advocates are neither beneficiaries nor payers of the constraint itself — they are excluded and their directionality is analytical (d = 0.5 by convention). The structural asymmetry is stark: powerless recipients bear concentrated costs; powerful administrators and taxpayers collect concentrated gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (budget scarcity, tension between adequacy and sustainability) is contested and live, but the solution (targeting efficiency) has developed substantial inertia. The constraint could be classified as piton if the founding problem were clearly dead — if universal distribution were empirically demonstrable as superior and the targeting apparatus persisted only through institutional theater. However, the targeting reading is still making a live claim (that efficiency requires concentration), and the suppression required to maintain it remains substantial but not yet overwhelming. The classification as snare (not piton) reflects that the extraction is still actively defended on substantive grounds (efficiency, moral hazard, work incentives) rather than merely performed theatrically. Mandatrophy would arise if the founding problem became clearly obsolete (if technological change made verification costless, or if evidence decisively showed universals superior) but the targeting apparatus persisted anyway. We are not yet at mandatrophy, but the rising theater_ratio (from 0.28 to 0.42) and rising extractiveness (from 0.61 to 0.78) suggest the system is moving toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contention,
    'Is the income support commitment fundamentally about providing unconditional cash (freedom floor reading) or about rationing scarce resources efficiently (targeting efficiency reading)?',
    'The kernel is formally ambiguous and admits both readings. Resolution comes from observing which reading controls the operative policy space and what evidence would shift control. Temporal measurement of which reading dominates political discourse and institutional resource allocation.',
    'If freedom floor reading gains control, the constraint type shifts from snare (extraction via targeting) to rope (genuine coordination via universals). If dependency trap reading gains control, the constraint type becomes tangled_rope (coordination for work incentives + extraction of unpaid labor through behavioral conditions). The core omega is the kernel reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Which reading of the income support kernel controls the policy space and institutional reality?').

omega_variable(
    founding_problem_scarcity_doctrine_contestation,
    'Is the budget constraint on welfare fundamentally real (limited resources, inescapable trade-off between redistribution and growth) or substantially constructed (chosen tax rates, chosen exemptions for capital income, chosen growth models)?',
    'Comparative institutional analysis: jurisdictions with universal income show whether the scarcity doctrine is natural or constructed. Evidence on whether universals actually reduce economic growth or whether targeting overhead consumes enough resources to make universals net-cheaper.',
    'If scarcity is constructed, the founding problem is partly artificial and the targeting efficiency reading loses its primary legitimacy claim. The constraint would reclassify from snare (coordinating around real scarcity) to pure extraction (redistributing from poor to administrators under false scarcity narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_scarcity_doctrine_contestation, empirical, 'Whether the welfare budget constraint is natural or constructed by policy choice.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression that maintains the targeting efficiency reading structural (institutional barriers, legal penalties for universal income proposals, administrative entrenchment) or internalized (recipients accept the deservingness narrative, believe they should prove need, have fused their identity with the role of ''welfare recipient'')?',
    'Post-constraint-removal trajectory: if suppression persists after the targeting apparatus is eliminated and replaced with universals, suppression is partly internalized. Survey data on whether recipients report shame or legitimacy acceptance of means-testing.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — recipients carry the targeting logic with them even after exit. Reclassify as substantially internalized suppression rather than purely structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in the recipient''s identity and self-perception.').

omega_variable(
    moral_hazard_vindication_contestation,
    'Do unconditional cash transfers actually reduce labor supply significantly (validating the moral hazard framework that the targeting reading vindicated) or is the empirical evidence contestable and primarily used to justify the targeting reading post-hoc?',
    'Systematic review of cash transfer experiments and natural experiments comparing conditional, targeted, and universal designs. Meta-analysis of labor supply elasticity estimates and their variance.',
    'If labor supply reductions are substantial and robust, the moral hazard framework is vindicated and the targeting reading retains empirical support. If reductions are small or contestable, the vindicated proposition becomes weaker and the targeting reading''s efficiency claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_vindication_contestation, empirical, 'Whether unconditional transfers actually reduce labor supply as the moral hazard framework claims.').

omega_variable(
    identity_lock_auditor_fusion,
    'Are welfare eligibility auditors genuinely locked into the targeting apparatus through professional identity fusion, or could they transition to different income support models (universals, conditional designs) without loss of professional standing or economic security?',
    'Natural experiment from jurisdictions that transition from targeted to universal income: are auditors absorbed into different administrative functions or displaced? Career-path analysis of auditor professions across different welfare regimes.',
    'If identity-locked, auditors are trapped in defending the targeting reading and constitute a structural coalition against alternatives. If mobile, auditors represent a swing constituency that could support alternative readings if the policy landscape shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_auditor_fusion, empirical, 'Whether auditor professional identity is fused to the targeting apparatus or mobile across models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__targeting_efficiency_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(inco_tr_t8, observed).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__targeting_efficiency_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(inco_tr_t16, observed).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(inco_tr_t24, observed).
narrative_ontology:measurement(inco_tr_t32, income_support_commitment__targeting_efficiency_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(inco_tr_t32, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__targeting_efficiency_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(inco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement_basis(inco_be_t8, observed).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement_basis(inco_be_t16, observed).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement_basis(inco_be_t24, observed).
narrative_ontology:measurement(inco_be_t32, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement_basis(inco_be_t32, observed).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(inco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(inco_su_t8, observed).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(inco_su_t16, observed).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(inco_su_t24, observed).
narrative_ontology:measurement(inco_su_t32, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(inco_su_t32, observed).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(inco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel admits three structurally distinct constraint readings. The targeting_efficiency_reading (this file) instantiates the reading that concentrates support on demonstrated need and frames universals as wasteful. The freedom_floor_reading instantiates the opposing reading that frames unconditional support as enabling autonomy. The dependency_trap_reading instantiates a third reading that frames the constraint around work incentives and behavioral conditioning. All three share the same kernel (the commitment to provide income support) and the same empirical domain (welfare policy) but produce different ε values, different beneficiary/victim structures, and different type classifications. The three readings are linked via network.affects_constraints: each reading depends on the others' existence as the interpretive contest that determines which reading controls the policy space. They are sibling constraints, not a single constraint viewed from three angles. The ε-invariance principle requires decomposition: do not attempt to model the kernel-reading contest as measurement-basis variance within a single constraint; write three separate JSON files and link them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, powerless, 0.95).
constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, institutional, 0.08).
constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
