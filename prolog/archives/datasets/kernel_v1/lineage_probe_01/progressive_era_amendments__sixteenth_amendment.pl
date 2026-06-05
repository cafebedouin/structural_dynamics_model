% ============================================================================
% CONSTRAINT STORY: progressive_era_amendments__sixteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_progressive_era_amendments__sixteenth_amendment, []).

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
 *   constraint_id: progressive_era_amendments__sixteenth_amendment
 *   human_readable: The Sixteenth Amendment: Progressive Federal Income Tax Without Apportionment
 *   domain: constitutional_law/fiscal_policy
 *
 * SUMMARY:
 *   The Sixteenth Amendment (ratified 1913) fundamentally restructured the
 *   constitutional relationship between federal authority and wealth
 *   accumulation by authorizing direct taxation of income without
 *   apportionment. Pre-1913, the Constitution's apportionment clause (Article
 *   I, Section 2) required any direct tax to be divided among states by
 *   population, making a federal income tax administratively infeasible and
 *   effectively shielding accumulated wealth from centralized extraction. The
 *   amendment suppressed this shield and created the fiscal foundation of the
 *   modern redistributive state, financing Social Security, Medicare, and
 *   federal public works at scale previously impossible. Yet the same
 *   mechanism that funds public goods also concentrates extraction power in
 *   federal hands, suppresses alternative income strategies (cash economies,
 *   state-level taxation autonomy, capital accumulation pathways), and
 *   generates theater through an increasingly complex compliance ritual. From
 *   different structural positions, the Sixteenth Amendment appears as pure
 *   coordination (rope), mixed coordination-extraction (tangled rope), pure
 *   extraction (snare), degraded administration (piton), or immutable fiscal
 *   law (mountain). The constraint exemplifies how a single constitutional
 *   choice generates radically different experiences depending on the
 *   observer's relationship to its extraction and coordination functions.
 *   This reading instantiates one side of an ongoing constitutional contest:
 *   the progressive reading that frames the income tax as legitimate
 *   democratic expansion of federal fiscal authority. Sibling readings
 *   (Eighteenth, Nineteenth, Seventeenth amendments) represent alternative
 *   constitutional solutions to Progressive Era problems (direct social
 *   regulation, democratic participation, legislative corruption), each with
 *   their own extractiveness profiles and legitimacy grounds.
 *
 * KEY AGENTS:
 *   - Federal Treasury / Redistributive State Apparatus: Primary beneficiary (institutional/arbitrage) — gains unprecedented fiscal capacity for public works, social insurance, military spending, and centralized economic coordination
 *   - High-Income Wealth Concentrators: Primary victim (powerful/arbitrage-constrained) — face direct extraction of accumulated income and capital gains; retain escape routes unavailable to wage earners (offshore accounts, corporate structures, tax-advantaged securities)
 *   - Wage Earners: Secondary victim (powerless/trapped) — subject to automatic withholding that suppresses individual tax resistance and creates all-encompassing extraction mechanism
 *   - Professional Middle Class: Mixed (moderate/constrained) — experience extraction (progressive rates, wealth accumulation barriers) and coordination benefits (publicly funded education, Social Security, infrastructure)
 *   - State Governments / Corporate Interests: Displaced (institutional/constrained) — lose fiscal autonomy as federal income taxation replaces tariffs and state-level revenue sources
 *   - IRS / Federal Tax Administration: Maintains theater (institutional/arbitrage) — operates increasingly performative compliance ritual (deductions, audit procedures) while real extraction flows through withholding system
 *   - Analytical Observer / Fiscal Theorist: Risks naturalizing contingency (analytical/analytical) — perceives income tax as immutable necessity of modern statecraft rather than contingent political choice about who bears extraction burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(progressive_era_amendments__sixteenth_amendment, 0.48).
domain_priors:suppression_score(progressive_era_amendments__sixteenth_amendment, 0.62).
domain_priors:theater_ratio(progressive_era_amendments__sixteenth_amendment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(progressive_era_amendments__sixteenth_amendment, extractiveness, 0.48).
narrative_ontology:constraint_metric(progressive_era_amendments__sixteenth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(progressive_era_amendments__sixteenth_amendment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(progressive_era_amendments__sixteenth_amendment, tangled_rope).
narrative_ontology:human_readable(progressive_era_amendments__sixteenth_amendment, "The Sixteenth Amendment: Progressive Federal Income Tax Without Apportionment").
narrative_ontology:topic_domain(progressive_era_amendments__sixteenth_amendment, "constitutional_law/fiscal_policy").

domain_priors:requires_active_enforcement(progressive_era_amendments__sixteenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(progressive_era_amendments__sixteenth_amendment, '9d01d720-d757-45c1-aa7b-9585b91bf183').
narrative_ontology:cs_kernel_codification('9d01d720-d757-45c1-aa7b-9585b91bf183', formalized).
narrative_ontology:cs_authority_grounding('9d01d720-d757-45c1-aa7b-9585b91bf183', lineage).
narrative_ontology:cs_interpretation_layer_present('9d01d720-d757-45c1-aa7b-9585b91bf183').
narrative_ontology:cs_reading_relation('9d01d720-d757-45c1-aa7b-9585b91bf183', progressive_era_amendments__progressive_era_eighteenth_amendment, forecloses).
narrative_ontology:cs_reading_relation('9d01d720-d757-45c1-aa7b-9585b91bf183', progressive_era_amendments__progressive_era_nineteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('9d01d720-d757-45c1-aa7b-9585b91bf183', progressive_era_amendments__progressive_era_seventeenth_amendment, coexists_with).
narrative_ontology:cs_axiom('9d01d720-d757-45c1-aa7b-9585b91bf183', foundational, federal_fiscal_authority_legitimacy).
narrative_ontology:cs_axiom_status(federal_fiscal_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9d01d720-d757-45c1-aa7b-9585b91bf183', federal_fiscal_authority_legitimacy, deontological).
narrative_ontology:cs_axiom('9d01d720-d757-45c1-aa7b-9585b91bf183', foundational, apportionment_rule_administratively_contingent).
narrative_ontology:cs_axiom_status(apportionment_rule_administratively_contingent, holdable).
narrative_ontology:cs_axiom_grounding('9d01d720-d757-45c1-aa7b-9585b91bf183', apportionment_rule_administratively_contingent, conventional).
narrative_ontology:cs_reference_frame('9d01d720-d757-45c1-aa7b-9585b91bf183', federalist_fiscal_coordination).
narrative_ontology:cs_drift_state('9d01d720-d757-45c1-aa7b-9585b91bf183', contemporary_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d01d720-d757-45c1-aa7b-9585b91bf183', '').
narrative_ontology:cs_kernel_id(progressive_era_amendments__sixteenth_amendment, progressive_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(progressive_era_amendments__sixteenth_amendment, federal_treasury).
narrative_ontology:constraint_beneficiary(progressive_era_amendments__sixteenth_amendment, redistributive_state_apparatus).
narrative_ontology:constraint_victim(progressive_era_amendments__sixteenth_amendment, high_income_wealth_concentrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WAGE EARNER (SNARE) — Income tax with automatic withholding creates an inescapable extraction mechanism. The wage earner cannot arbitrage jurisdictions, cannot hide income from employers, cannot exit the national labor market without radical life disruption. Suppression is high: the withholding system itself suppresses alternatives (cash economies, barter, relocation). The extractiveness is high because the mechanism is coercive and all-encompassing. Experienced as pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PROFESSIONAL MIDDLE CLASS (TANGLED ROPE) — Faces genuine extraction (progressive rates, closing of wealth accumulation paths) but also benefits from the state services financed by the income tax: public infrastructure, public education, Social Security, Medicare. Exit is constrained but possible (relocation, entrepreneurship, tax avoidance strategies). The constraint functions as both coordination (funding collective goods) and asymmetric extraction (progressive marginal rates target this class's accumulated capital gains and higher incomes). Experienced as a mixed mechanism.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE FEDERAL TREASURY (ROPE) — The Sixteenth Amendment is, from this perspective, a coordination mechanism that solved a structural problem: how to finance a continental state and redistributive programs without tariff revenue or indirect taxation. The beneficiary experiences the constraint as pure coordination with no perceived extraction cost. The treasury gains unprecedented capacity for public works, social insurance, and military capability. The extractiveness is dampened from this perspective because the benefits align perfectly with the extraction mechanism.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE STATE LEGISLATIVE/CORPORATE ELITE (TANGLED ROPE) — State governments and corporations previously extracted revenue through tariffs, excise taxes, and federal land grants. The Sixteenth Amendment's centralization of income taxation transfers extraction power from state and corporate actors to the federal level. This is both a loss of coordination autonomy and a loss of extraction opportunity. Suppression increases because federal enforcement mechanisms replace state-level revenue negotiation. The constraint is experienced as extractive (loss of local fiscal sovereignty) and coercive (federal enforcement supersedes state authority).
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE WEALTHY CAPITAL ACCUMULATOR (TANGLED ROPE) — High-income and wealth-concentrated actors face the strongest extraction. However, they retain arbitrage options unavailable to wage earners: offshore accounts, corporate structures, tax-advantaged securities, jurisdictional shopping, political influence on tax code rewrites. The constraint is extractive but not absolute. Suppression is high (IRS enforcement, reporting requirements, asset tracing) but not total. The wealthy experience this as extraction they can partially circumvent, generating a perception of partial coordination (tax code as negotiable rather than immutable). This perspective shows the perspectival gap most clearly: the wealthy see options the wage earner does not.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE FEDERAL BUREAUCRACY (PITON) — The IRS and federal tax administration apparatus maintain a theater of individualized assessment and negotiation (tax deductions, credits, exemptions, audit procedures) that creates the appearance of customized fairness while the real extraction mechanism is the withholding system (80% of federal revenue arrives without individual negotiation). The bureaucracy sees its own procedures as increasingly performative — the complexity of the tax code generates theater that obscures the underlying extraction. The extraction function is robust; the assessment ritual is degraded. Theater ratio high because compliance ritual (filing, documentation, audit risk) far exceeds actual assessment or negotiation.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / FISCAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, the Sixteenth Amendment appears to instantiate an unchangeable law of modern statecraft: a continental nation-state requires a reliable revenue source beyond tariffs and commodity taxes, and direct income taxation is the only mechanism that can finance modern social insurance, defense, and public works at scale. This perspective sees the income tax as an immutable structural necessity, not a contingent political choice. However, the structural data contradicts this mountain classification: the beneficiaries are identifiable, the victims are identifiable, enforcement is required, and suppression is substantial. The engine will compute this as a false summit, revealing that 'fiscal necessity' naturalizes what is actually a political choice about whose wealth is vulnerable to taxation and whose is protected.
constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(progressive_era_amendments__sixteenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(progressive_era_amendments__sixteenth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(progressive_era_amendments__sixteenth_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(progressive_era_amendments__sixteenth_amendment, TR),
    TR >= 0.70.

:- end_tests(progressive_era_amendments__sixteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48, mid-range tangled rope): The Sixteenth Amendment creates genuine extraction — it subjects previously untaxed wealth accumulation to federal seizure. However, the mechanism is not pure extraction because it also funds coordination goods (public goods, social insurance, infrastructure) that benefit even the taxed actors. The 0.48 value reflects measurement at contemporary era (1973): the initial extractiveness (0.18 in 1913) was low because federal income tax rates were modest (7% top marginal rate) and applied only to very high incomes. Extractiveness increased sharply with wartime (1943: 0.42) when top marginal rates reached 94% and middle-income earners became subject. Contemporary value (0.48) reflects stabilization around 39.6% (2012) to 37% (2017) top rate with broader application across income distribution. Suppression (0.62, high): The mechanism is deliberately designed to suppress alternatives. Automatic withholding (adopted 1943, time_point 30) dramatically increased suppression by removing individual agency from tax payment. Initial suppression (0.35 in 1913) reflected that income taxes required individual filing and assessment, creating openings for negotiation and tax resistance. Withholding increased suppression to 0.68 (1943) by making tax collection automatic and employer-mediated. Contemporary value (0.62) reflects some suppression decline due to tax avoidance infrastructure (offshore accounts, tax shelters, carried-interest loopholes) that high-income actors have reconstructed, but withholding remains total for wage earners. Theater ratio (0.35, low-moderate): The Sixteenth Amendment's actual extraction mechanism is relatively functional rather than performative. Unlike many institutional constraints, income taxation works as designed — money flows from earner/filer to treasury with minimal functional degradation. However, theater increased slightly over time (0.28 → 0.38 → 0.35) as tax code complexity grew. The contemporary value reflects that while the extraction mechanism is functional (lower theater), the compliance ritual (filing, documentation, audit procedures) creates appearance of individualized assessment that obscures the underlying withholding mechanism's totality. Theater is lower than for constraints like peer review or jury trials because extraction is the intended function, not a side effect.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across seven different observation positions. The wage earner (powerless/trapped) experiences pure snare-level extraction with no escape options and no coordination benefit. The wealthy capital accumulator (powerful/arbitrage) experiences tangled rope because they retain exit strategies (tax shelters, offshore accounts, political influence) unavailable to wage earners, creating a gap between perceived suppression levels. The federal treasury (institutional/arbitrage) experiences pure rope-level coordination because it solves the fundamental problem of financing a continental state. The professional middle class (moderate/constrained) experiences tangled rope because they both pay significant extraction and receive significant coordination benefits (public education, Social Security, infrastructure they use). State governments (institutional/constrained) experience tangled rope because they lose fiscal autonomy while losing extraction opportunities previously available through tariffs and land grants. The IRS bureaucracy (institutional/arbitrage) experiences piton because its actual assessment procedures are increasingly performative theater while the real extraction mechanism (withholding) operates without bureaucratic mediation. The analytical observer (analytical/analytical) risks mountain-level naturalization, seeing income taxation as an immutable requirement of modern fiscal capacity rather than a contingent political choice. The perspectival gap is not about disagreement but about genuine structural differences in experienced extraction and suppression levels — the constraint really is different for the wage earner than for the multinational corporation's tax planner.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the extraction flow. Federal treasury: beneficiary + arbitrage exit = low d (~0.05) → negative f(d) → negative effective extraction (experiences as pure coordination). High-income accumulators: victims + arbitrage exit (tax shelter access) = moderate d (~0.45) → moderate f(d) → moderate chi (experiences partial extraction, partial escape). Wage earners: victims + trapped exit (withholding is unavoidable) = high d (~0.85) → high f(d) (~1.15) → high chi (experiences maximum extraction). Professional middle class: both benefits and bearers + constrained exit = neutral d (~0.50) → sigmoid peak f(d) (~0.65) → moderate chi (experiences tangled coordination-extraction). The derivation captures the real structural phenomenon: different actors experience radically different effective extraction rates despite the same nominal tax code because their exit options differ. A wealthy individual can relocate to low-tax jurisdiction or use corporate structures; a wage earner cannot. This is not a matter of perception — it is a structural fact about the constraint's design.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Sixteenth Amendment is genuinely tangled (coordination + extraction hybrid) at the system level, not a mislabeling of pure coordination as extraction. The coordination function is real: federal income tax finances public goods that broad populations benefit from (infrastructure, education, social insurance) and solves a genuine problem (how to finance a continental state without regressive tariffs). The extraction function is also real: the mechanism concentrates wealth-reduction capacity in federal hands and eliminates previous shielding (apportionment). The tangled classification holds because both functions are structural, both are intended, and the constraint cannot function as pure coordination without the extraction mechanism (the extraction IS the coordination fund; you cannot have one without the other). The perspectival divergence (snare from powerless, rope from beneficiary, piton from bureaucracy) is not a failure of classification but evidence that the constraint genuinely is tangled — different agents experience different ratios of coordination to extraction based on their structural position. The constraint also demonstrates a false summit candidate: the mountain classification from the analytical observer ('fiscal necessity'). This is a genuine risk in constitutional constraint modeling — the observer can naturalize a contingent political choice (who bears taxation, whether wealth is protected, whether apportionment was shield or mere rule) as an immutable necessity. The engine's false summit detector would identify this by noting that beneficiaries (federal treasury, redistributive apparatus) are present, triggering the FSM signature override.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apportionment_shield_constitutional_status,
    'Is the apportionment requirement (pre-1913) a constitutionally immutable shield on wealth or a mere administrative rule subject to amendment?',
    'Historical analysis of founding-era intent and state-ratification debates; examination of whether apportionment served genuine federalism or was primarily a wealth-protection mechanism; comparison to other constitutional shields that have been amended or reinterpreted',
    'If immutable natural structure: income tax is violation of constitutional order (nullification reading). If administratively contingent: income tax is legitimate amendment of tax authority (progressive reading). This determines whether the constraint itself is legitimate or extracted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apportionment_shield_constitutional_status, conceptual, 'Constitutional status of the apportionment requirement').

omega_variable(
    progressive_extraction_vs_coordination_boundary,
    'Does progressive income taxation primarily serve coordination (funding collective goods for all citizens) or extraction (concentrating wealth in state apparatus and middle-class beneficiaries at the expense of capital accumulation)?',
    'Comparative institutional analysis: what state services do different income quintiles receive from federal revenue vs what tax they pay; correlation between public goods provision and income level; cross-national comparison of income tax structures and welfare outcomes; accounting for wealth effects (does progressive taxation actually reduce inequality or merely tax cash income while allowing capital appreciation?)',
    'If primarily coordination: constraint is Rope or Tangled Rope (legitimate mixed mechanism). If primarily extraction: constraint is Snare (coercive wealth capture). The answer depends on what counts as ''coordinated'' public goods and whether capital appreciation counts as income.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(progressive_extraction_vs_coordination_boundary, conceptual, 'Whether progressive income taxation is coordination or extraction mechanism').

omega_variable(
    tax_avoidance_suppression_adequacy,
    'Has the IRS enforcement capability actually suppressed alternative income strategies (offshore accounts, corporate tax shelters, carried-interest loopholes) or merely created a suppression theater that high-income actors routinely circumvent?',
    'Empirical measurement of tax compliance rates by income level; analysis of effective tax rates vs statutory rates; documentation of major tax avoidance strategies and their prevalence; IRS audit rate trends and case outcomes; comparison of wealth concentration before and after major enforcement periods (1960s Great Society vs 1980s Reagan deregulation vs contemporary)',
    'If suppression is real and enforced: high-income victims experience snare-level constraints. If suppression is theatrical: arbitrage options remain open and extraction is lower than claimed (tangled_rope floor holds). This measures whether the constraint''s enforcement is functional or degraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_avoidance_suppression_adequacy, empirical, 'Whether IRS enforcement actually suppresses tax avoidance or creates performative theater').

omega_variable(
    kernel_reading_contest,
    'Is the Sixteenth Amendment a legitimate expansion of federal fiscal authority to fund social coordination (progressive reading) or a constitutional coup that subjugated state autonomy and wealth to federal extraction (conservative reading)?',
    'This omega documents that the Sixteenth Amendment reading instantiates one side of an ongoing constitutional contest. No empirical data resolves this — the contest is about whether the legitimacy grounding the amendment is still valid. See cs_structure.reading_relations for how this reading relates to sibling readings (Eighteenth, Nineteenth, Seventeenth) in the Progressive Era cluster.',
    'This reading (progressive/expansion/legitimate) would classify the Sixteenth Amendment as founding act of the modern redistributive state. The conservative sibling reading (reaction/sovereignty violation) would classify it as extractive coup. The engine does not resolve this omega — it documents that both readings are live positions in constitutional jurisprudence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Constitutional legitimacy of the Sixteenth Amendment in Progressive Era kernel').

omega_variable(
    withholding_system_design_intentionality,
    'Was the adoption of automatic payroll withholding (1943) a necessary administrative efficiency or a deliberately designed suppression mechanism that made tax resistance infeasible by removing individual agency?',
    'Historical analysis of wartime tax policy decisions; comparison of pre-withholding (1913-1943) tax compliance rates and tax resistance activism to post-withholding era; examination of Treasury Department deliberations and explicit cost-benefit analyses; comparison to other democracies'' withholding adoption decisions and timing',
    'If necessary efficiency: withholding is coordination theater (Piton classification). If deliberately suppressive: withholding is extraction mechanism engineering (Snare floor). This determines whether the theater ratio should be higher (if suppression is deliberate) or whether the constraint has degraded into pure administrative routine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withholding_system_design_intentionality, empirical, 'Intentional design of automatic payroll withholding as suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(progressive_era_amendments__sixteenth_amendment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sixteenth_theater_1913, progressive_era_amendments__sixteenth_amendment, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sixteenth_theater_1943, progressive_era_amendments__sixteenth_amendment, theater_ratio, 30, 0.38).
narrative_ontology:measurement(sixteenth_theater_1973, progressive_era_amendments__sixteenth_amendment, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(sixteenth_extract_1913, progressive_era_amendments__sixteenth_amendment, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sixteenth_extract_1943, progressive_era_amendments__sixteenth_amendment, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(sixteenth_extract_1973, progressive_era_amendments__sixteenth_amendment, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sixteenth_suppress_1913, progressive_era_amendments__sixteenth_amendment, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sixteenth_suppress_1943, progressive_era_amendments__sixteenth_amendment, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(sixteenth_suppress_1973, progressive_era_amendments__sixteenth_amendment, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(progressive_era_amendments__sixteenth_amendment, resource_allocation).
narrative_ontology:affects_constraint(progressive_era_amendments__sixteenth_amendment, progressive_era_eighteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__sixteenth_amendment, progressive_era_nineteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__sixteenth_amendment, progressive_era_seventeenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__sixteenth_amendment, wage_withholding_suppression_mechanism).
narrative_ontology:affects_constraint(progressive_era_amendments__sixteenth_amendment, tax_shelter_escape_arbitrage).

% DUAL FORMULATION NOTE:
% The Sixteenth Amendment as a constitutional authorization (this story) should be distinguished from the wage withholding system (separate story, downstream) and from specific tax-avoidance mechanisms (separate stories). The amendment establishes federal authority; withholding operationalizes suppression of that authority; tax shelters represent counter-mechanisms that actors have constructed within the amendment's framework. Each has different ε (amendment: 0.48; withholding system: 0.72; tax shelters: 0.35) reflecting different structural functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
