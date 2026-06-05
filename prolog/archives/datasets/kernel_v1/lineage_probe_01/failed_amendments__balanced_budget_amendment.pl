% ============================================================================
% CONSTRAINT STORY: failed_amendments__balanced_budget_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failed_amendments__balanced_budget_amendment, []).

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
 *   constraint_id: failed_amendments__balanced_budget_amendment
 *   human_readable: Balanced Budget Amendment: Constitutionalizing Fiscal Suppression (One Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Balanced Budget Amendment represents one reading of a contested
 *   constitutional kernel: the question of what constraints should apply to
 *   federal fiscal policy. This constraint story instantiates the balanced
 *   budget amendment as a mechanism for suppressing deficit finance through
 *   constitutional law, refused in ratification but repeatedly advanced. From
 *   the perspective of countercyclical policy and present low-income
 *   populations, the constraint is a snare — a mechanism that eliminates
 *   fiscal flexibility precisely when it is needed most (recessions,
 *   financial crises, pandemics). From the perspective of deficit hawks and
 *   future creditors, the amendment solves a coordination problem: it
 *   provides a credible commitment device to fiscal restraint. From the
 *   perspective of institutional politics, the repeated passage without
 *   ratification is theatrical — a performative affirmation of fiscal
 *   responsibility that carries zero enforcement cost. The extractiveness has
 *   risen over time (0.32 → 0.58) as fiscal crises (2008, 2020) have
 *   demonstrated the real cost of constraint adoption, while theater has
 *   simultaneously increased (0.48 → 0.65) as the amendment has become
 *   increasingly divorced from actual legislative progression. This story is
 *   one reading of the failed_amendments kernel; alternative readings present
 *   the child labor amendment, DC voting rights, and equal rights amendment
 *   as different constitutional contests. These readings coexist — they are
 *   not mutually exclusive — and the kernel itself (the institution of failed
 *   amendments) remains contested across all readings.
 *
 * KEY AGENTS:
 *   - Fiscal Deficit Hawks Coalition: Organized agents seeking credible commitment to fiscal restraint; primary beneficiary of coordination function; see amendment as solution to collective action problem of deficit spending
 *   - Future Creditors / Bond Markets: Institutional beneficiary experiencing the constraint as pure coordination; benefit from lower risk premium on government debt; have exit via capital reallocation
 *   - Countercyclical Fiscal Policy: Abstract victim with no exit; suppressed capacity to deploy deficit spending during recessions and crises; forced to operate under structural constraint
 *   - Present Low-Income Populations: Trapped victim bearing the cost of austerity during crises when fiscal transfers and job programs are most needed; face biographical and intergenerational poverty pressure under constraint
 *   - Congressional Theater (House Members, Interest Groups): Institutional actors maintaining the amendment in legislative culture through repeated passage; experience zero cost from performing fiscal responsibility without ratification risk
 *   - Post-Crisis Reform Movements: Organized agents developing alternative fiscal frameworks (MMT, automatic stabilizers, countercyclical redesign) that offer exit from constitutional constraint logic
 *   - Analytical Observer: Risks naturalizing a contested political choice as economic law; needs framework to detect false summit (the claim that fiscal constraint is a natural law)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failed_amendments__balanced_budget_amendment, 0.58).
domain_priors:suppression_score(failed_amendments__balanced_budget_amendment, 0.68).
domain_priors:theater_ratio(failed_amendments__balanced_budget_amendment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failed_amendments__balanced_budget_amendment, extractiveness, 0.58).
narrative_ontology:constraint_metric(failed_amendments__balanced_budget_amendment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(failed_amendments__balanced_budget_amendment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failed_amendments__balanced_budget_amendment, tangled_rope).
narrative_ontology:human_readable(failed_amendments__balanced_budget_amendment, "Balanced Budget Amendment: Constitutionalizing Fiscal Suppression (One Reading)").
narrative_ontology:topic_domain(failed_amendments__balanced_budget_amendment, "political/constitutional").

domain_priors:requires_active_enforcement(failed_amendments__balanced_budget_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failed_amendments__balanced_budget_amendment, 'c98d1b32-cf15-43c4-a926-cdaa87628ee9').
narrative_ontology:cs_kernel_codification('c98d1b32-cf15-43c4-a926-cdaa87628ee9', formalized).
narrative_ontology:cs_authority_grounding('c98d1b32-cf15-43c4-a926-cdaa87628ee9', extraction).
narrative_ontology:cs_interpretation_layer_present('c98d1b32-cf15-43c4-a926-cdaa87628ee9').
narrative_ontology:cs_reading_relation('c98d1b32-cf15-43c4-a926-cdaa87628ee9', failed_amendments__equal_rights_amendment, coexists_with).
narrative_ontology:cs_reading_relation('c98d1b32-cf15-43c4-a926-cdaa87628ee9', failed_amendments__child_labor_amendment, coexists_with).
narrative_ontology:cs_reading_relation('c98d1b32-cf15-43c4-a926-cdaa87628ee9', failed_amendments__dc_voting_rights_amendment, coexists_with).
narrative_ontology:cs_axiom('c98d1b32-cf15-43c4-a926-cdaa87628ee9', foundational, deficit_spending_intergenerational_extraction).
narrative_ontology:cs_axiom_status(deficit_spending_intergenerational_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c98d1b32-cf15-43c4-a926-cdaa87628ee9', deficit_spending_intergenerational_extraction, instrumental).
narrative_ontology:cs_axiom('c98d1b32-cf15-43c4-a926-cdaa87628ee9', secondary, constitutional_constraint_as_credible_commitment).
narrative_ontology:cs_axiom_status(constitutional_constraint_as_credible_commitment, holdable).
narrative_ontology:cs_axiom_grounding('c98d1b32-cf15-43c4-a926-cdaa87628ee9', constitutional_constraint_as_credible_commitment, empirically_contingent).
narrative_ontology:cs_reference_frame('c98d1b32-cf15-43c4-a926-cdaa87628ee9', constitutional_fiscal_restraint).
narrative_ontology:cs_drift_state('c98d1b32-cf15-43c4-a926-cdaa87628ee9', contemporary_post_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c98d1b32-cf15-43c4-a926-cdaa87628ee9', '').
narrative_ontology:cs_kernel_id(failed_amendments__balanced_budget_amendment, failed_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failed_amendments__balanced_budget_amendment, deficit_opposed_coalition).
narrative_ontology:constraint_beneficiary(failed_amendments__balanced_budget_amendment, future_taxpayers_rhetorical).
narrative_ontology:constraint_victim(failed_amendments__balanced_budget_amendment, countercyclical_fiscal_capacity).
narrative_ontology:constraint_victim(failed_amendments__balanced_budget_amendment, present_low_income_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUNTERCYCLICAL FISCAL CAPACITY (SNARE) — Trapped in the constraint's logic. A constitutional balanced budget requirement suppresses the capacity to deploy deficit spending during recessions, financial crises, or pandemics. Once constitutionalized, this constraint cannot be suspended without amendment — a multi-generational exit barrier. The extraction is the forced elimination of a policy tool when most needed. No alternatives exist within the constitutional framework.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRESENT LOW-INCOME POPULATIONS (SNARE) — Trapped by the structural lock. In recessions triggered by financial crisis (2008) or pandemic (2020), deficit spending enables countercyclical transfer programs, unemployment insurance extension, and job programs. A balanced budget constraint converts fiscal crises into social crises, forcing immediate austerity on those least able to absorb it. The constraint's adoption would be an intergenerational transfer: present borrowing capacity is sacrificed for future fiscal orthodoxy, shifting costs from future creditors onto present poor.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL DEFICIT HAWKS / BALANCED BUDGET COALITION (TANGLED ROPE) — Organized agents (Tea Party, fiscal conservative foundations, deficit hawks in Congress and media) experience genuine coordination: a balanced budget amendment solves a coordination problem among themselves — the problem of not being able to credibly commit to fiscal restraint in a collective action setting where politicians face strong incentives to spend. BUT this coordination function is paired with asymmetric extraction: the constraint imposes costs on countercyclical policy and low-income populations. The coalition has agency and exit (they can advocate for repeal if the constraint proves destructive), so experienced extraction is moderate rather than maximal.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE CREDITORS AND DEBT MARKETS (ROPE) — Institutional beneficiary with structural exit (creditors can move capital elsewhere; bond markets can move to other sovereigns). The balanced budget amendment is pure coordination from this perspective: it solves the creditor's problem of whether to extend credit to a government that has demonstrated high deficit spending and debt accumulation. Constitutional fiscal constraint is a coordination mechanism that increases creditor confidence. No extraction is experienced by this agent — the constraint benefits them (lower risk premium, larger bond market), and they have alternatives.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL THEATER OF FISCAL RESPONSIBILITY (PITON) — The balanced budget amendment is largely theatrical in its current form: it has never passed both chambers and been sent to the states. The repeated passage in the House without progression to ratification is itself performative — members vote to affirm fiscal responsibility without real probability of enforcement. The theater ratio is high (0.65) because the constraint exists primarily as a rhetorical commitment, not as functional fiscal policy. The institutional actors (House members, fiscal conservative organizations) maintain the theatrical commitment because its cost is zero: no actual amendment has ever been ratified. This is inertial — the amendment persists in legislative culture and interest-group platforms despite repeated failure.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-CRISIS FISCAL REFORM (SCAFFOLD) — Progressive and pragmatic reform movements see the constraint as a temporary problem to be superseded, not constitutionalized. Their exit option is the development of alternative fiscal frameworks: Modern Monetary Theory approaches, automatic stabilizer redesign, countercyclical tax policies that don't require legislative discretion. This perspective experiences low extraction because it has agency and sees a sunset: the balanced budget amendment's power wanes as alternative fiscal frameworks gain legitimacy and institutions adapt to post-crisis realities. The constraint appears as a temporary political artifact that reform movements can outlast.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational perspective, the balanced budget amendment risks being naturalized as an immutable constitutional principle reflecting fundamental economic law: 'governments must not spend more than they take in, just as households must not.' This framing presents deficit spending as inherently unsustainable, making the constitutional constraint appear natural rather than political. However, the structural data undermines this: identifiable beneficiaries (deficit hawks, future creditors) and clear victims (countercyclical policy, present poor) reveal this as a political choice, not a natural law. The analytical observer risks mistaking a contested reading of fiscal governance for an economic universal.
constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failed_amendments__balanced_budget_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failed_amendments__balanced_budget_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failed_amendments__balanced_budget_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failed_amendments__balanced_budget_amendment, TR),
    TR >= 0.70.

:- end_tests(failed_amendments__balanced_budget_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The amendment's extractiveness derives from two mechanisms: (1) elimination of a policy tool whose actual deployment during crises would produce substantial aggregate benefit, and (2) the intergenerational transfer of fiscal burden (present borrowing capacity is surrendered so future creditors can lend at lower risk premium). The value is not higher (snare territory, ≥0.66) because the constraint has not been constitutionalized — it exists as a threatened constraint rather than an enforced one. If ratified, extractiveness would rise to snare-range (0.75+). The rising trajectory reflects accumulating evidence (2008, 2020) that the constraint's adoption would have imposed severe costs. Suppression (0.68): Moderate-high and stable. The suppression mechanism is constitutional — once adopted, deficit spending cannot be deployed without a supermajority amendment process. Presently, suppression is weaker (legislative rejection, state-level refusal) but the constitutional threat is substantial. Theater ratio (0.65): Moderate-high, increasing. The amendment's passage in the House without progression to ratification is quintessentially theatrical — members vote to affirm fiscal responsibility without bearing the risk of actual enforcement. The theater has increased as the amendment has become more ceremonial and further from ratification, suggesting it functions primarily as a rhetorical marker of fiscal conservatism rather than a serious constitutional proposal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a stark perspectival divide. Deficit hawks and future creditors see a coordination mechanism solving the problem of unconstrained fiscal expansion and creditor uncertainty. Countercyclical policy and present poor see suppression of necessary tools and forced austerity. Congressional actors see low-cost theater. Reform movements see a temporary constraint being superseded by better fiscal frameworks. The analytical observer risks seeing constitutional necessity where there is actually political choice. This gap is not resolvable by more data about fiscal sustainability — it hinges on the normative question of intergenerational distribution (how much should present fiscal capacity be constrained to benefit future creditors?) and empirical questions about alternative mechanisms (can non-constitutional rules, political culture, or market discipline achieve similar constraint without constitutional amendment?). The kernel reading structure captures this: the balanced budget amendment and its alternatives (unconstrained deficit spending, soft fiscal rules, reformed countercyclical mechanisms) are genuinely competing framings of fiscal governance, not different observations of one constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from agent power, exit options, and beneficiary/victim status. Deficit hawks (organized/constrained) experience the constraint as net beneficial despite its extraction costs — they value the coordination function. Future creditors (institutional/arbitrage) experience pure benefit — no extraction from their perspective. Countercyclical policy (powerless/trapped) experiences maximum extraction with no exit. Present low-income populations (powerless/trapped in crisis periods) experience extraction when the constraint would bite hardest. Congressional theater (institutional/arbitrage) experiences zero extraction — the cost of the constraint is paid by others. Reform movements (organized/mobile) experience low extraction because they have exit. The perspectival gap is large: beneficiaries experience coordination (rope) while victims experience suppression (snare). The tangled rope classification reflects the hybrid function: genuine coordination (deficit hawks' credible commitment problem) paired with asymmetric extraction (fiscal suppression during crises).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination function from extraction mechanism. The balanced budget amendment has a genuine coordination function (solving the collective action problem of deficit spending incentives) but also real extraction costs (suppression of countercyclical capacity). The classification as tangled rope rather than rope or snare captures both: the constraint coordinates (creates credible commitment) AND extracts (suppresses policy tools needed during crises). The piton perspective (theater) and scaffold perspective (temporary, to be superseded) show that the constraint's current form is partly ceremonial. The mandatrophy is resolved by noting that the amendment's failure to ratify is itself part of the structure: the constraint exists as a threat and cultural artifact, not as enforceable law, which keeps extractiveness below snare threshold. If ratified, extractiveness would increase to snare-range and the classification would shift. The analytical observer's false-summit risk documents the temptation to naturalize this political choice as economic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_macroeconomic_impact,
    'Under a balanced budget constitutional constraint, how much worse would macroeconomic outcomes have been during the 2008 financial crisis and 2020 pandemic without access to deficit spending?',
    'Counterfactual macroeconomic modeling; comparison to austerity-constrained economies (EU post-2010, UK 2010-2015); estimation of unemployment, poverty, and output loss under structural balanced budget rule',
    'High negative impact (unemployment +5-8%, poverty +2-3%): constitutionalizes a constraint that would have caused severe present harm for future creditor benefit. This would validate the snare classification for present low-income populations. If minimal impact: the constraint''s extraction is overstated and the rope/scaffold perspectives gain relative weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_macroeconomic_impact, empirical, 'Macroeconomic impact of balanced budget constraint on crisis response').

omega_variable(
    fiscal_sustainability_empirical_necessity,
    'Is a balanced budget constitutional constraint empirically necessary for fiscal sustainability, or do other mechanisms (institutional rules, political culture, market discipline) provide sufficient constraint?',
    'Comparative analysis of high-debt-to-GDP countries with and without constitutional balanced budget rules; correlation between constitutional constraint and long-term fiscal outcomes; counterfactual: did UK, Canada, Germany fiscal rules (non-constitutional) achieve similar outcomes without amendment?',
    'If constitutional constraint empirically necessary: validates the deficit hawks'' coordination problem framing and future creditor benefit. If other mechanisms suffice: the constitutional amendment is redundant extraction, and the snare classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_empirical_necessity, empirical, 'Whether constitutional balanced budget constraint is empirically necessary for fiscal sustainability').

omega_variable(
    intergenerational_extraction_direction,
    'Does the balanced budget amendment represent extraction from present to future (present borrowing is irresponsible; future generations must inherit the constraint), or extraction from future to present (future generations lose fiscal capacity so present creditors can lend at lower risk premium)?',
    'Distributional analysis across generations: if present-to-future, deficit spending is the extraction and the constraint is justice. If future-to-present, the constraint is the extraction and present poor bear the cost. The empirical question: who actually pays the cost of constraint enforcement?',
    'This is a conceptual/preference omega as much as empirical. The reading instantiated here assumes future-to-present extraction (the constraint harms present low-income populations for future creditor benefit). An alternative reading would reverse the direction. Both are coherent; the data cannot determine which frame is ''correct.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_extraction_direction, preference, 'Direction of intergenerational extraction in deficit vs. constraint logic').

omega_variable(
    constitutional_amendment_failure_cause,
    'Why has the balanced budget amendment repeatedly passed one chamber but never been sent to ratification? Is this suppression of a genuine popular fiscal preference, or manifestation of latent recognition that constitutionalizing fiscal constraint would be economically destructive?',
    'Analysis of voting patterns, interest group opposition, state-level ratification dynamics; public opinion polling on abstract balanced budget vs. concrete spending program preferences; comparison to other failed amendments (ERA, DC voting rights) to identify structural barriers vs. genuine preference divergence',
    'If suppression of genuine preference: the amendment failure itself demonstrates the constraint mechanism (fiscal hawks cannot overcome the structural barriers). If latent recognition of destructiveness: the repeated failure is adaptation, not suppression, and the constraint''s power lies in preventing adoption, not in enforcement post-adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_failure_cause, empirical, 'Structural causes of balanced budget amendment failure to ratify').

omega_variable(
    kernel_reading_contest,
    'Is this constraint (balanced budget amendment) competing with a different reading that would reframe deficit spending as the constraint (intergenerational extraction through borrowing against future tax revenue)?',
    'This is flagged as a kernel reading with sibling readings (equal rights amendment, child labor amendment, DC voting rights amendment). The contest is not empirical but conceptual: does the kernel itself (fiscal constitutional governance) have an alternative reading where the problem is deficit spending rather than the amendment''s suppression?',
    'This omega documents that the reading instantiated here (BBA as constraint suppressing countercyclical capacity) is ONE reading of fiscal constitutional governance. An alternative reading would present unconstrained deficit spending as the constraint and the balanced budget amendment as attempted solution. Neither reading forecloses the other — they coexist as live political positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Alternative reading of fiscal constitutional kernel: deficit spending as constraint rather than BBA suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failed_amendments__balanced_budget_amendment, 1976, 2006).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bba_tr_t0, failed_amendments__balanced_budget_amendment, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bba_tr_t15, failed_amendments__balanced_budget_amendment, theater_ratio, 15, 0.6).
narrative_ontology:measurement(bba_tr_t30, failed_amendments__balanced_budget_amendment, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(bba_be_t0, failed_amendments__balanced_budget_amendment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bba_be_t15, failed_amendments__balanced_budget_amendment, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(bba_be_t30, failed_amendments__balanced_budget_amendment, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bba_su_t0, failed_amendments__balanced_budget_amendment, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(bba_su_t15, failed_amendments__balanced_budget_amendment, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(bba_su_t30, failed_amendments__balanced_budget_amendment, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failed_amendments__balanced_budget_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(failed_amendments__balanced_budget_amendment, failed_amendments__equal_rights_amendment).
narrative_ontology:affects_constraint(failed_amendments__balanced_budget_amendment, failed_amendments__child_labor_amendment).
narrative_ontology:affects_constraint(failed_amendments__balanced_budget_amendment, failed_amendments__dc_voting_rights_amendment).

% DUAL FORMULATION NOTE:
% The balanced budget amendment is one constraint within the failed_amendments kernel family. Each amendment reading (BBA, ERA, child labor, DC voting rights) has a different ε value reflecting different empirical deadness and different victim/beneficiary structures. BBA is downstream of the other amendments in the sense that it represents a different constitutional politics (future vs. present beneficiaries), not a causal dependency. All are linked to the kernel structure: constitutional amendments that achieve congressional passage but fail ratification, reflecting persistent contests over fundamental law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(failed_amendments__balanced_budget_amendment, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
