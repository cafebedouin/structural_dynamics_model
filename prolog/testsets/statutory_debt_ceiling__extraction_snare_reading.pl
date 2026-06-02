% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Extraction Snare (Minority Hostage Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The US statutory debt ceiling, codified in 31 U.S.C. § 3101, operates as
 *   a weaponized extraction mechanism under the 'snare' reading: a
 *   legislative minority faction uses the threat of sovereign default to
 *   extract fiscal policy concessions from the majority, while the broader
 *   populace of federal beneficiaries, creditors, and the global financial
 *   system bears the suppression and extraction cost. This reading differs
 *   from sibling interpretations—a 'coordination scaffold' reading that sees
 *   the ceiling as a legitimate budget negotiation framework, and a
 *   'constitutional nullity' reading that treats the ceiling as legally
 *   superseded by the Fourteenth Amendment—by modeling the ceiling primarily
 *   as a hostage mechanism that generates asymmetric bargaining power for
 *   numerical minorities. The snare reading's extractiveness (ε = 0.68)
 *   reflects systematic use of default threat to compel policy concessions
 *   (spending cuts, revenue rollbacks, regulatory freezes) that would not
 *   emerge from standard fiscal negotiation. The measurable trajectory shows
 *   extractiveness rising from 0.35 (1995, when ceiling was routine
 *   reauthorization) through 0.52 (2005, when partisan tension began
 *   weaponizing the mechanism) to 0.68 (2015–2025, when default threat became
 *   chronic hostage tool). Theater ratio rising from 0.32 to 0.58 reflects
 *   increasing performative content: emergency treasury accounting maneuvers,
 *   Fed communication theater, and legislative brinkmanship gestures that
 *   delay binding without resolving structural constraint. Suppression
 *   (rising from 0.48 to 0.72) reflects accumulated constraints on fiscal
 *   governance: federal employees uncertain of paychecks, benefit program
 *   planning under default risk, capital market hedging costs, and
 *   international creditor premium pricing for US default probability.
 *
 * KEY AGENTS:
 *   - Legislative Minority Faction: Primary beneficiary (organized/arbitrage) — extracts policy concessions through default threat; low cost to exit (can vote to raise ceiling); net receiver of extracted resources and policy concessions
 *   - Social Security/Medicare Beneficiaries and Federal Employees: Primary victims (powerless/trapped) — face delayed payments, potential benefit cuts, or default if ceiling binds; cannot exit or organize alternative funding; absorb full suppression cost
 *   - Majority Legislative Faction: Secondary beneficiary/victim (moderate/constrained) — coordinates fiscal governance but constrained by minority hostage position; high cost to override minority veto (constitutional norm violation); mixed extraction experience
 *   - Global Bond Markets and International Creditors: Secondary victims (institutional/constrained) — face recurring default risk, hedging costs, and credit premium pricing; constrained exit (US debt too systemically important); absorb extraction via rate premiums
 *   - Federal Reserve and Treasury Apparatus: Institutional performers (institutional/arbitrage) — deploy emergency measures and communication theater to manage crisis; high agency but use it performatively rather than structurally; maintain piton-class dysfunction
 *   - Fiscal Governance Integrity (Abstract): Tertiary victim (powerless/trapped) — normative framework for budget authority, separation of powers, and fiscal credibility; cannot organize or exit; bears cumulative reputational extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.68).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Extraction Snare (Minority Hostage Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '4a39f364-a7e8-455a-86da-80febee53604').
narrative_ontology:cs_kernel_codification('4a39f364-a7e8-455a-86da-80febee53604', formalized).
narrative_ontology:cs_authority_grounding('4a39f364-a7e8-455a-86da-80febee53604', lineage).
narrative_ontology:cs_interpretation_layer_present('4a39f364-a7e8-455a-86da-80febee53604').
narrative_ontology:cs_reading_relation('4a39f364-a7e8-455a-86da-80febee53604', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a39f364-a7e8-455a-86da-80febee53604', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_axiom('4a39f364-a7e8-455a-86da-80febee53604', foundational, ceiling_as_hostage_authorization).
narrative_ontology:cs_axiom_status(ceiling_as_hostage_authorization, holdable).
narrative_ontology:cs_axiom_grounding('4a39f364-a7e8-455a-86da-80febee53604', ceiling_as_hostage_authorization, empirically_contingent).
narrative_ontology:cs_axiom('4a39f364-a7e8-455a-86da-80febee53604', secondary, benign_coordination_framing_overridden).
narrative_ontology:cs_axiom_status(benign_coordination_framing_overridden, overridden).
narrative_ontology:cs_axiom_grounding('4a39f364-a7e8-455a-86da-80febee53604', benign_coordination_framing_overridden, empirically_contingent).
narrative_ontology:cs_reference_frame('4a39f364-a7e8-455a-86da-80febee53604', legislative_fiscal_sovereignty).
narrative_ontology:cs_drift_state('4a39f364-a7e8-455a-86da-80febee53604', contemporary_hostage_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4a39f364-a7e8-455a-86da-80febee53604', '2025-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, fiscal_governance_integrity).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, credit_system_stability).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, defaulted_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFAULTED BENEFICIARIES AND FISCAL SYSTEM (SNARE) — Social Security recipients, Medicare beneficiaries, bond holders, and federal employees face immediate default if the ceiling binds. These agents cannot exit: obligations to seniors, healthcare networks, and creditors are non-negotiable. They bear full extraction cost: forced austerity, benefit cuts, delayed payments, credit rating damage. No agency, no alternatives, maximum asymmetric exposure.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAJORITY LEGISLATIVE FACTION (TANGLED ROPE) — Majority party experiences mixed extraction and coordination. They coordinate fiscal governance (genuine function: debate and set revenue/spending policy) but are constrained by the minority's hostage position. Constrained exit: they could breach the ceiling via simple majority amendment, but constitutional norm violation and precedent-setting costs are high. Moderate extraction imposed by the minority, but some agency and some coordination benefit remain.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE MINORITY FACTION (ROPE) — Primary beneficiary of the extraction mechanism. The minority experiences the debt ceiling as pure coordination: it enables them to negotiate policy concessions (spending cuts, regulatory rollbacks, tax changes) by threatening default. High agency, low cost to exit (they can vote to raise the ceiling), asymmetric extraction flows toward them. They perceive the mechanism as legitimate negotiation leverage.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL CREDIT SYSTEM (SNARE) — International creditors, bond traders, and sovereign wealth funds face recurring default risk from US Treasury debt. Constrained exit: US debt is too central to global portfolios to abandon entirely, but cost of hedging and uncertainty premiums rise with each ceiling crisis. Multiple instances of brinksmanship (2011, 2013, 2015, 2021, 2023) have degraded perceptions of US creditworthiness. Extraction mechanism: majority concessions benefit minority at cost of global credit stability and US borrowing costs.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL RESERVE AND TREASURY (PITON) — Central bank and executive treasury function is substantially performative. Theater includes: emergency accounting maneuvers (extraordinary measures, pension fund raids, cash flow manipulation) that delay but do not resolve the ceiling constraint. Fed performs open-market operations to stabilize rates while Congress performs partisan brinkmanship. Both institutions have arbitrage options (Fed chair public statements about ceiling unconstitutionality, Treasury flexibility in payment prioritization) but exercise them theatrically rather than structurally. The apparatus persists despite acknowledged dysfunction.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From a civilizational analytical stance, some budget constraint is inherent to fiscal governance: every government faces limits on borrowing, and markets price risk. This view sees the debt ceiling as a natural law — an immutable fiscal reality rather than a constructed institutional mechanism. However, this reading naturalizes what is structurally contingent: the US is the sole issuer of a fiat reserve currency with global demand; no hard constraint exists except politically constructed ones. The mountain classification triggers the false summit detector (beneficiaries declared), revealing that the 'natural fiscal limit' framing legitimizes minority extraction.
constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_debt_ceiling__extraction_snare_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, TR),
    TR >= 0.70.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The debt ceiling systematically generates extractive asymmetry through default threat mechanism. Legislative minorities use the ceiling to compel majority concessions on spending, revenue, or regulatory policy that would not emerge from standard budget negotiation processes. Historical data shows minorities have extracted: spending sequestration (2011), tax code changes (2011, 2013), regulatory freezes (2013, 2015), and benefit reforms (2011). The extraction is not absolute (majorities retain authority to breach the ceiling) but is substantial enough that minorities maintain chronic bargaining advantage. The measurable rise from 0.35 to 0.68 reflects increasing institutionalization of the hostage mechanism — early ceiling increases were routine; contemporary ceiling debates are structured extortion events. Suppression (0.72): High and rising. Multiple agents face binding constraints: beneficiaries cannot exit benefit programs, creditors cannot abandon US debt, employees cannot exit federal employment instantly, international markets cannot decouple from US fiscal stability. The threat itself (not just actual default) creates suppression by forcing contingency planning around default risk. Credit rating downgrades in 2011 and potential downgrades in subsequent crises demonstrate that suppression extends beyond immediate participants to the global credit system. Theater (0.58): Moderate-high. The ceiling mechanism exhibits increasing performative content. Emergency treasury measures (pension fund raiding, extraordinary cash management, payment prioritization) delay binding without resolving structural constraint. Federal Reserve communication (chair statements about ceiling unconstitutionality) and legislative theater (countdown clocks, last-minute negotiations) perform the drama of governance while the underlying hostage mechanism persists unchanged. Theater has risen because each crisis resolution triggers expectation of future crises, forcing actors to perform preparation gestures.
 *
 * PERSPECTIVAL GAP:
 *   The snare reading generates maximal perspectival divergence. The minority faction perceives rope (pure coordination of budget negotiation), with low extraction cost and high benefit from leverage position. Majority perceives tangled_rope (genuine fiscal coordination mixed with imposed extraction from minority veto threat). Default beneficiaries perceive snare (pure extraction with no coordination benefit — they gain no leverage or policy input from the ceiling mechanism). Global creditors perceive snare (pure extraction in the form of credit risk premium and hedging costs). The treasury/fed perceive piton (a theatrical crisis ritual that persists despite acknowledged dysfunction). The analytical observer at civilizational scope risks perceiving mountain (natural fiscal constraint) but structural data reveals false summit — beneficiaries declared, institutional contingency established, alternative frameworks available (UK, Canada governance models).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural position relative to the extraction flow. Legislative minority with arbitrage exit (can vote to raise ceiling, minimal reputational/political cost at scale) experiences low d ≈ 0.20 (beneficiary position), producing negative/low f(d) and net-positive effective extraction (chi benefits them). Powerless default beneficiaries with trapped exit face maximum d ≈ 0.95 (target position), producing f(d) ≈ 1.42 and high experienced chi (extraction imposed on them). Majority faction with constrained exit (can breach ceiling but norm violation is costly) occupies middle d ≈ 0.60, experiencing moderate chi. Global creditors with constrained exit (cannot abandon US debt but can hedge, price risk) occupy d ≈ 0.68, experiencing high moderate chi. Treasury/Fed apparatus with arbitrage (high flexibility in emergency measures) derives low d from their beneficiary-adjacent position, but their piton classification comes from the theater gate rather than from low chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved by the reading specificity: the snare reading models the ceiling as a pure extraction mechanism, decoupled from any legitimate coordination function. The sibling coordination_scaffold reading models genuine budget negotiation function; the constitutional_nullity reading models the ceiling as legally void. The mandatrophy dissolves when the three readings are recognized as alternative coherent framings of the same mechanism. Snare reading asserts: the extraction function has become dominant and the coordination function residual (theater). Scaffold reading asserts: coordination remains primary; extraction is secondary framing of normal legislative negotiation. Nullity reading asserts: the ceiling has no legitimate constitutional foundation, rendering classification irrelevant. No single reading eliminates the others within a single party's framework — all three remain live political and legal positions. The snare reading's mandatrophy is resolved by acknowledging that it represents one legitimate interpretation grounded in observable patterns (rising extractiveness, chronic default threat, compelled policy concessions) without requiring that all parties accept it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hostage_mechanism_vs_negotiation,
    'Is the debt ceiling fundamentally a hostage mechanism (threat of default to extract concessions) or a legitimate budget negotiation framework?',
    'Comparative historical analysis: count policy concessions extracted during ceiling negotiations vs. comparable non-ceiling spending/revenue disputes; identify causality between threat and concession timing; assess whether non-hostage negotiation frameworks (e.g., continuing resolutions, reconciliation process) yield different concession distributions',
    'If hostage mechanism confirmed: snare classification robust, extractiveness ≥ 0.66 justified, minority faction derives power from default threat asymmetry. If negotiation framework confirmed: classification downgrades to tangled_rope, extractiveness drops to 0.45–0.55 range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hostage_mechanism_vs_negotiation, empirical, 'Whether ceiling functions as extraction hostage or legitimate budget process').

omega_variable(
    constitutional_contingency_vs_natural_limit,
    'Is the debt ceiling a natural fiscal law or a contingent institutional construction?',
    'Comparative analysis: examine fiscal governance in peer democracies (UK, Canada, Germany, Australia) that lack explicit debt ceiling statutes; assess whether their credit ratings, fiscal discipline, or default risk differ systematically; investigate whether US would have similar default risk without ceiling if markets believed Congress could unilaterally authorize spending via reconciliation',
    'If natural limit: mountain classification partially defensible (though beneficiaries present trigger FSM reclassification). If contingent: mountain is false summit, snare classification robust, alternative governance frameworks (cap-and-trade spending, sliding-scale revenue rules) are structurally available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_contingency_vs_natural_limit, empirical, 'Whether ceiling is natural fiscal law or contingent construction').

omega_variable(
    minority_veto_legitimacy_frame,
    'Under what normative framework is minority veto over fiscal policy legitimate?',
    'Constitutional jurisprudence on separation of powers and budget authority; philosophical analysis of whether supermajority requirements (implied by hostage mechanism) align with legislative design intent; historical investigation of what framers anticipated regarding fiscal policy deadlock resolution',
    'If supermajority requirement legitimate (constitutional design): snare classification involves contested normative frame (both sides claim constitutionality); ambiguity shifts from empirical to preference omega. If supermajority framework is post-hoc rationalization: snare classification is robust, minority extraction lacks normative cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_veto_legitimacy_frame, conceptual, 'Legitimacy of minority veto over fiscal authorization').

omega_variable(
    extraction_vs_policy_outcome_causality,
    'Do the policy concessions extracted during ceiling negotiations represent genuine change in governing program, or reallocation of priorities that would have shifted anyway?',
    'Counterfactual analysis: model fiscal policy outcomes absent ceiling mechanism using comparable periods without active ceiling constraint (e.g., 1964–1979 automatic increase period); compare actual spending/revenue trajectories during ceiling-constrained periods to counterfactual paths; assess whether concession timeline causally precedes policy change or merely correlates with independently-driven shifts',
    'If concessions are genuine extraction: snare classification confirmed, extractiveness 0.65–0.75. If concessions reflect underlying factional power that would manifest anyway: extractiveness drops to 0.50–0.60, classification moves toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_policy_outcome_causality, empirical, 'Causal mechanism linking ceiling threat to policy concessions').

omega_variable(
    reading_specificity_boundary,
    'Is this snare reading of the debt ceiling distinct from sibling readings (coordination_scaffold, constitutional_nullity), or do they describe overlapping observations that should be modeled as single constraint with multiple observables?',
    'Comparison of ε values across readings: if coordination reading produces ε ≈ 0.30 (ceiling as legitimate budget-debate framework) and snare reading produces ε ≈ 0.68 (ceiling as hostage mechanism), readings are structurally distinct and justify separate stories per ε-invariance principle. If ε values differ by <0.15, reconsider whether reading distinction maps to observable/framing choice rather than structural difference.',
    'If structurally distinct: all three readings warrant separate constraint stories linked via network.affects_constraints, supporting kernel decomposition. If same constraint with measurement variance: consolidate to single story with omega-variable framing distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_boundary, conceptual, 'Whether snare reading represents structurally distinct constraint or measurement variance on single constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debtceil_snare_theater_1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(debtceil_snare_theater_2005, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(debtceil_snare_theater_2015, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(debtceil_snare_extract_1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(debtceil_snare_extract_2005, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(debtceil_snare_extract_2015, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(debtceil_snare_suppress_1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(debtceil_snare_suppress_2005, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(debtceil_snare_suppress_2015, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, budgetary_authorization_majority_rule).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_benefit_program_default_risk).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling decomposes into three structurally distinct constraint stories grounded in the contested kernel statutory_debt_ceiling: (1) extraction_snare_reading (ε=0.68, this story) models the ceiling as hostage mechanism for minority extraction; (2) coordination_scaffold_reading (ε=0.35, sibling story) models the ceiling as legitimate budget-negotiation framework; (3) constitutional_nullity_reading (ε=0.05, sibling story) models the ceiling as legally void under the Fourteenth Amendment. The three readings produce radically different extractiveness values because they are grounded in different observable frames and normative interpretations of the ceiling's operative function. All three stories should be generated separately and linked via network.affects_constraints to model the contested kernel as a presheaf structure rather than a single constraint evaluated from multiple perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
