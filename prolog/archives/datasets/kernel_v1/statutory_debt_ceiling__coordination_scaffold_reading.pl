% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Procedural Coordination Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling originated in 1917 (Pittman Act) as a
 *   procedural simplification: rather than requiring Congress to authorize
 *   each Treasury bond issuance individually, a single aggregate limit was
 *   set, and Treasury could borrow up to that limit without repeated
 *   congressional approval. This constraint is ONE READING of a contested
 *   constitutional kernel: the ceiling can be understood as a coordination
 *   mechanism (this reading) that reduces transaction costs, or as an
 *   extraction mechanism (sibling reading: extraction_snare_reading) that
 *   enables partisan minorities to hold fiscal policy hostage, or as a
 *   constitutional nullity (sibling reading: constitutional_nullity_reading)
 *   that violates the Constitution's debt-authorization clause and should be
 *   unenforceable. This story instantiates the COORDINATION_SCAFFOLD_READING:
 *   the ceiling functions as a low-extractiveness temporary coordination
 *   mechanism with routine periodic adjustment logic. Under this reading, the
 *   ceiling achieves its original function (avoiding micromanagement of
 *   Treasury operations) with minimal friction when Congress adjusts it on
 *   schedule. The constraint is 'temporary' (scaffold) because Congress has
 *   full sovereignty to adjust the limit and routinely does so — the sunset
 *   logic is embedded in periodic adjustment votes. Theater ratio is low
 *   (0.35) under this reading because the adjustment procedure is transparent
 *   and routine. Extractiveness is low (0.22) because there is no systematic
 *   hostage-taking: the majority adjusts the ceiling when it approaches,
 *   minority leverage is episodic rather than structural, and financial
 *   markets price in high-confidence ceiling adjustment despite rhetorical
 *   crises.
 *
 * KEY AGENTS:
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — gains operational autonomy within aggregate limit without need for individual bond authorizations; captures efficiency gains of coordination mechanism
 *   - Legislative Majority: Secondary beneficiary (powerful/mobile) — avoids transaction costs of per-bond authorization; retains full fiscal sovereignty by adjusting ceiling on schedule
 *   - Minority Party: Partial victim with leverage opportunity (moderate/constrained) — constrained by inability to unilaterally raise ceiling, but can extract concessions via leverage during adjustment window; experiences mixed coordination and extraction
 *   - Financial Markets and Creditors: Analytical observer (institutional/arbitrage, analytical perspective) — experience ceiling primarily as source of pricing uncertainty rather than structural default risk; markets consistently price in high-confidence congressional adjustment
 *   - Congress (Generational View): Beneficiary at system level (institutional/mobile) — delegation of authority to Treasury avoids continuous micromanagement; periodic adjustment votes suffice for legislative control
 *   - Constitutional Interpreters: Analytical observer (analytical/analytical) — observe the ceiling as a policy choice, not an immutable constitutional requirement; question whether it serves its original coordination function in era of polarized politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Procedural Coordination Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'abc2f55d-51bd-4359-927b-52033d527db6').
narrative_ontology:cs_kernel_codification('abc2f55d-51bd-4359-927b-52033d527db6', fixed_text).
narrative_ontology:cs_authority_grounding('abc2f55d-51bd-4359-927b-52033d527db6', lineage).
narrative_ontology:cs_interpretation_layer_present('abc2f55d-51bd-4359-927b-52033d527db6').
narrative_ontology:cs_reading_relation('abc2f55d-51bd-4359-927b-52033d527db6', statutory_debt_ceiling__extraction_snare_reading, influences).
narrative_ontology:cs_reading_relation('abc2f55d-51bd-4359-927b-52033d527db6', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('abc2f55d-51bd-4359-927b-52033d527db6', foundational, routine_adjustment_assumption).
narrative_ontology:cs_axiom_status(routine_adjustment_assumption, holdable).
narrative_ontology:cs_axiom_grounding('abc2f55d-51bd-4359-927b-52033d527db6', routine_adjustment_assumption, empirically_contingent).
narrative_ontology:cs_axiom('abc2f55d-51bd-4359-927b-52033d527db6', foundational, coordination_function_ceiling_efficient).
narrative_ontology:cs_axiom_status(coordination_function_ceiling_efficient, holdable).
narrative_ontology:cs_axiom_grounding('abc2f55d-51bd-4359-927b-52033d527db6', coordination_function_ceiling_efficient, instrumental).
narrative_ontology:cs_reference_frame('abc2f55d-51bd-4359-927b-52033d527db6', routine_procedural_coordination).
narrative_ontology:cs_drift_state('abc2f55d-51bd-4359-927b-52033d527db6', contemporary_polarized_legislature, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abc2f55d-51bd-4359-927b-52033d527db6', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operational_autonomy).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_planning_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREASURY OPERATIONAL VIEW (ROPE) — The debt ceiling functions as pure coordination: a single aggregate limit on borrowing authority eliminates need for Congress to approve each individual Treasury bond issuance. Treasury experiences this as a low-friction authorization mechanism. No systematic extraction occurs when the ceiling is adjusted routinely on schedule. The constraint solves the collective action problem of continuous appropriations without requiring legislative micromanagement of every transaction. Arbitrage exit available: Treasury can maintain operational continuity if Congress fails to adjust (short-term cash management workarounds exist, though they degrade over weeks).
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE MAJORITY ROUTINE SCENARIO (SCAFFOLD) — Under the coordination reading, Congress experiences the debt ceiling as a temporary coordination mechanism with built-in sunset logic: periodic (typically biennial) adjustment votes are scheduled events, not crisis management. The ceiling constrains fiscal discretion only until the majority votes to raise it. Low theater because the procedure is transparent and predictable. Extraction is minimal because the legislature retains full sovereignty — it can adjust the ceiling whenever it chooses, no hostage-taking, no procedural asymmetry. Sunset rationale: Congress can and does adjust the limit routinely. If Congress fails to adjust, the constraint collapses (Treasury finds workarounds, or Congress acts under perceived crisis). The procedure persists only through regular legislative renewal.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: MINORITY PARTY CONSTRAINED VIEW (TANGLED ROPE) — The minority party faces the coordination function (they too benefit from automatic authorization without micromanagement), but also sees the ceiling as a leverage point: the majority must act to raise the limit, creating an asymmetric window where minority demands can extract concessions on unrelated fiscal measures. This perspective acknowledges genuine coordination benefit (not a pure extraction snare) but adds asymmetric extraction via the leverage mechanism. Constrained exit: the minority cannot unilaterally raise the ceiling; their exit option is costly political opposition or withdrawal of procedural cooperation. Classification is tangled rope because both coordination (the ceiling itself) and extraction (the leverage window) are structurally present.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL DEBT MARKET / PITON VIEW — Financial markets experience the ceiling as largely theatrical: the limit itself does not constrain Treasury borrowing capacity (Congress adjusts it whenever approached) and imposes minimal friction on market operations. The ceiling's primary function — coordination of legislative authorization — has been substantially degraded by institutional inertia and repeated crises. Markets price in ceiling-adjustment risk, but the risk is not to actual borrowing capacity (Congress always adjusts eventually) but to the theater of uncertainty. Theater ratio is moderate (0.40) because the adjustment ritual has become performative rather than substantive. The piton classification reflects that the constraint persists through institutional habit and political theater, not because it serves the coordination function that once justified it.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE SUPERMAJORITY / INSULATED VIEW (ROPE) — A supermajority controlling both chambers experiences the debt ceiling as pure coordination with no extraction risk: they can raise the limit without leverage constraints. For the dominant coalition, the ceiling is merely a procedural formality that avoids transaction costs of individual borrowing authorizations. The coordination function is visible, extraction is absent, exit is trivial (they choose the schedule). This perspective assumes stable supermajority control and no partisan leverage dynamics.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FINANCIAL MARKETS / CREDITOR POWERLESSNESS (SNARE) — Under the extraction reading (not this one, but present for comparison), creditors and financial markets appear as trapped victims: they must accept the risk that a legislative impasse could delay Treasury payments, yet they have no exit option and no leverage. From this perspective, the ceiling becomes a hostage-taking mechanism where partisan minorities extract concessions by threatening default. Theater is high (crisis rhetoric without actual default) and extraction is severe. However, this reading treats the ceiling as primarily extractive rather than coordinative. The coordination reading denies this framing, arguing that actual default is so costly that the threat is not credible as a leverage mechanism.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY (MOUNTAIN) — From a civilizational view, some argue the debt ceiling is an immutable feature of constitutional governance: the legislature must authorize borrowing, and the ceiling is the natural expression of this authorization requirement in a unified aggregate limit. This perspective treats the ceiling as an emergent property of constitutional structure rather than a contingent policy choice. However, the structural data contradicts mountain classification — many constitutions and governance systems achieve the coordination function without an aggregate debt limit (automatic appropriations, Treasury autonomy within baseline rules, rolling authorizations). The mountain framing naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_debt_ceiling__coordination_scaffold_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, TR),
    TR >= 0.70.

:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.22): Low. Under the coordination reading, the ceiling does not systematically extract from identifiable victims. Treasury captures efficiency gains (avoids transaction costs), but these are coordination gains, not extraction. The majority adjusts the ceiling routinely, so there is no sustained fiscal constraint on elected representatives. The minority party can occasionally extract concessions via leverage, but this is episodic rather than structural: in most adjustment cycles, the minority has limited leverage (supermajority control by majority, time pressure favoring adjustment, creditor pressure to avoid default). The trajectory from 1917 (ε=0.08, pure coordination) to 2017 (ε=0.22) reflects increasing politicization and leverage-seeking, not a shift in the ceiling's fundamental mechanism. The ceiling still functions as coordination; extraction emerges when partisan actors treat it as a hostage mechanism. SUPPRESSION (0.18): Low. Treasury has genuine exit options (short-term cash management, extraordinary measures, pressure on Congress to adjust). Congress has full authority to adjust. Markets can adjust pricing and credit terms. Suppression is low because the constraint is not coercive — it is a procedural arrangement that Congress voluntarily maintains and can easily modify. THEATER_RATIO (0.35): Moderate-low. Under the coordination reading, the adjustment procedure is relatively transparent. Congress debates the appropriate fiscal level, adjusts the limit, and Treasury continues operations. However, theater has increased over time as partisan actors use ceiling negotiations for rhetorical effect (threats of default, crisis framing) disproportionate to the actual default risk. The 2017 value (0.35) reflects that some theatrical crisis-management has become routine, but the procedure retains substantial substantive content (actual fiscal discussions, genuine legislative negotiation). The measurement trajectory shows creeping theater (1917→2017: 0.15→0.35) as the ceiling has become increasingly politicized. CLAIMED_TYPE (scaffold): Justified by has_sunset_clause=true and routine periodic adjustment votes, which constitute the sunset mechanism. Low theater and low extractiveness confirm the scaffold classification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this reading is between routine-adjustment and crisis scenarios. When Congress adjusts the ceiling routinely (supermajority control, biennial rhythm), all perspectives converge on low-extractiveness readings: Treasury sees coordination, majority sees procedural simplification, minority sees acceptable negotiation, markets see routine adjustment. When Congress polarizes and adjustment becomes uncertain (minority leverage increases, default rhetoric escalates), the perspectives diverge: minority sees extraction opportunity, markets price in default risk, observers see the ceiling shift toward snare classification. The coordination reading explicitly assumes routine adjustment. If that assumption erodes, the reading becomes unstable. The critical perspectival difference: does the observer expect Congress to adjust the ceiling before default (coordination assumption), or does the observer believe Congress might fail to adjust and allow default (extraction assumption)? The coordination reading says the former; the snare reading says the latter. This is not a difference in observed facts but in the prior distribution over congressional behavior — empirical, but determined by political-economy priors.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury's directionality (d): Low (≈0.10). Treasury is a net beneficiary of the coordination mechanism (efficiency gains from not needing per-bond authorization). Arbitrage exit option (can invoke extraordinary measures, Treasury Secretary can manage cash flow creatively) → low d → low f(d) → low chi. Legislative majority's directionality (d): Low (≈0.08). The majority controls the adjustment process and incurs no extraction — they adjust the ceiling when appropriate. Mobile exit option (can call a vote at any time) → very low d → negative f(d). Minority party's directionality (d): Moderate (≈0.45). The minority has some leverage (majority must act to adjust) but cannot prevent adjustment indefinitely. Constrained exit option → moderate d → moderate f(d). Financial markets' directionality (d): Moderate (≈0.50). Markets experience pricing uncertainty during ceiling crises, but they expect adjustment with high confidence. Analytical position with high prior on congressional adjustment → symmetric d → moderate f(d). The 2026 framework allows context-dependent d derivation via the sigmoid f(d) = -0.20 + 1.70 / (1 + e^(-6*(d-0.50))), and the directionality derivation chain automatically computes d from beneficiary/victim declarations + exit options. This story declares Treasury and legislative majority as beneficiaries (low d), minority and markets as constrained but not victimized (moderate d), and no universal victims (which would require trapped exit and high d).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by positioning the debt ceiling as a coordination mechanism with low extractiveness and built-in temporal limitation (periodic adjustment). The challenge to the coordination reading comes from the empirical trajectory: theater_ratio has risen from 0.15 to 0.35 over a century, and extractiveness has risen from 0.08 to 0.22. Does this trajectory threaten the reading's stability? The mandatrophy resolution argues: no, because the measured increases remain within the scaffold envelope (theater ≤ 0.70, extractiveness ≤ 0.30 for low-extraction scaffolds). The theater increase reflects rising politicization, not degradation of the coordination function. The extractiveness increase reflects episodic minority leverage, not systematic extraction. If theater were to exceed 0.70 or extractiveness were to exceed 0.35 (the tangled_rope threshold), the reading would become empirically untenable and would require reclassification toward piton (degraded coordination) or snare (pure extraction). The omega variable 'hostage_taking_threshold' addresses this: if hostage-taking crosses from political theater to credible default threat, the reading's assumption (Congress will adjust before default) becomes false, and the snare reading becomes more accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hostage_taking_threshold,
    'At what point does leverage-seeking on the debt ceiling cross from negotiation to extractive hostage-taking? Is the line structural (actual default risk) or political (partisan rhetoric about default)?',
    'Historical analysis of debt ceiling standoffs: track the gap between threatened default and actual Treasury payment delays. Distinguish genuine cash-flow constraints from political theater. Survey creditor risk assessments during impasse periods.',
    'If hostage-taking is structural (actual default risk): constraint shifts toward snare. If hostage-taking is political theater (Congress always adjusts before actual default): constraint remains scaffold, and minority leverage is political negotiation within a coordination frame, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hostage_taking_threshold, empirical, 'Whether ceiling crises reflect genuine default risk or political theater').

omega_variable(
    reading_boundary_partition,
    'Is the coordination reading sustainable under conditions of extreme partisan polarization? Does the reading rely on a baseline assumption of legislative good-faith adjustment that has eroded?',
    'Comparison of ceiling-adjustment timelines across eras: pre-2000 (routine), 2000-2010 (occasional theater), 2010-present (frequent crises). Track the emergence of explicit threats to cause default. Assess whether minority leverage has shifted from normal legislative negotiation to credible default threats.',
    'If polarization has made routine adjustment uncertain: the scaffold reading becomes unstable, and the snare reading becomes more empirically accurate. The constraint''s classification depends on whether Congress will reliably adjust before default.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_partition, empirical, 'Whether partisan polarization has degraded the coordination reading''s assumptions').

omega_variable(
    sunset_clause_mechanism,
    'What constitutes an effective sunset clause for the debt ceiling? Does ''periodic adjustment required'' suffice, or must adjustment be truly automatic (triggering without further legislative action)?',
    'Comparative analysis: debt ceiling vs other temporary fiscal measures (continuing resolutions, disaster appropriations) with explicit sunset dates. Track whether measures with periodic adjustment requirements (requiring affirmative legislative action) are sustained at higher rates than truly automatic mechanisms.',
    'If explicit periodic adjustment is ineffective as a sunset mechanism: the scaffold reading overstates the temporary nature of the constraint. If automatic mechanisms are necessary for true temporality: the current ceiling (which requires affirmative congressional action) is not a true scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_mechanism, empirical, 'Whether periodic adjustment requirements constitute effective sunset logic').

omega_variable(
    coordination_function_dependence,
    'Is the debt ceiling the only mechanism that achieves the coordination function (avoiding transaction-cost burden of per-bond authorization)? Or could the same function be achieved through alternative structures (Treasury autonomy within baseline rules, rolling multi-year authorizations)?',
    'Institutional comparison: countries and sub-national governments that achieve equivalent Treasury operational autonomy without aggregate debt ceilings. Analysis of the specific transaction costs the ceiling avoids vs the costs of alternative mechanisms.',
    'If alternatives exist with lower theater and equivalent coordination: the ceiling is not the natural/inevitable solution, and false-summit risk increases. If the ceiling is uniquely efficient: the coordination reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_dependence, empirical, 'Whether the debt ceiling is the unique mechanism for Treasury coordination').

omega_variable(
    kernel_reading_interpretation_ambiguity,
    'Does the debt ceiling''s original 1917 intent (coordination mechanism to simplify authorization) remain normatively binding? Or has contemporary political practice redefined the ceiling as a constraint-mechanism rather than a coordination-mechanism, such that the coordination reading represents an obsolete historical intent rather than the current structural reality?',
    'Historical constitutional analysis: intent of the Pittman Act (1917). Contemporary practice analysis: how Congress and Treasury actually use the ceiling (Do they treat it as a limit on borrowing, or as an occasion for unrelated fiscal negotiations?). Gap analysis between intent and practice.',
    'If historical intent is authoritative: the coordination reading is normatively correct, and contemporary hostage-taking represents a corruption of the mechanism. If contemporary practice redefines the mechanism: the coordination reading is aspirational rather than descriptive, and the snare reading may be more accurate to current institutional reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_interpretation_ambiguity, conceptual, 'Whether historical coordination intent remains authoritative or has been redefined by practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 0, 109).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_ceil_theater_1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(debt_ceil_theater_1967, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(debt_ceil_theater_2017, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(debt_ceil_extract_1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(debt_ceil_extract_1967, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(debt_ceil_extract_2017, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling is a single kernel with three competing readings. Each reading is a structurally distinct constraint with different extractiveness, beneficiary/victim profiles, and classifications. The coordination_scaffold_reading instantiates low-extractiveness, routine-adjustment framing. The extraction_snare_reading instantiates high-extractiveness, hostage-taking framing. The constitutional_nullity_reading instantiates the ceiling as constitutionally invalid and empirically unsustainable. The three readings' network relationships reflect conceptual rather than causal influence: the snare reading 'influences' the scaffold reading by demonstrating that the coordination assumption (routine adjustment) can erode under polarization; the nullity reading 'coexists_with' both by claiming the entire ceiling is unconstitutional rather than reinterpreting its function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
