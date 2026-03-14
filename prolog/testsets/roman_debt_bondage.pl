% ============================================================================
% CONSTRAINT STORY: roman_debt_bondage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_debt_bondage, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_debt_bondage
 *   human_readable: Roman Debt Bondage and Nexum
 *   domain: economic/legal/social
 *
 * SUMMARY:
 *   Roman debt bondage (nexum) represents one of the earliest documented
 *   systems of formalized labor extraction through financial obligation. From
 *   approximately the 5th century BCE (traditional founding of the Republic)
 *   through the 4th century CE (late Empire), nexum created a legal mechanism
 *   by which a debtor's default on a loan could result in permanent servitude
 *   to the creditor — a form of debt slavery distinct from chattel slavery
 *   but functionally similar. The constraint exhibits the full spectrum of DR
 *   classifications: snare from the debtor's position (permanent extraction,
 *   no exit), rope from the creditor's position (coordination mechanism for
 *   credit distribution), scaffold from the perspective of reformers (sunset
 *   clause via Lex Poetelia and subsequent legislation), tangled rope in
 *   post-reform periods (mixed coordination and extraction), and piton in
 *   late antiquity (theatrical persistence after functional mechanism
 *   declined). The extractiveness declined over 450 years as legal reforms
 *   gradually replaced personal bondage with property foreclosure and other
 *   mechanisms, but the institutional structure persisted through inertia
 *   even after its primary function was displaced by slavery and
 *   feudal-precursor arrangements.
 *
 * KEY AGENTS:
 *   - Debtors: Primary victims (powerless/trapped) — plebeian smallholders and urban poor facing no alternative credit; bear full extraction cost through lifetime servitude
 *   - Creditor Class: Primary beneficiaries (institutional/arbitrage) — patrician landowners and moneylenders; extract labor value and interest premium during debt servitude
 *   - Plebeian Resistance Coalition: Organized agents (organized/constrained) — political factions, secessions, and reform advocates pushing for nexum restrictions; represent organized exit threat
 *   - Patrician Reformers: Institutional actors (institutional/constrained) — elite factions supporting nexum restrictions for pragmatic reasons (political stability, labor system alternatives)
 *   - Imperial State: Institutional actor (institutional/arbitrage) — initially enforces nexum through legal machinery; later subsumes it under property law and slave law
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees nexum as structural solution to debtors' liquidity crises that became permanent extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_debt_bondage, 0.78).
domain_priors:suppression_score(roman_debt_bondage, 0.82).
domain_priors:theater_ratio(roman_debt_bondage, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_debt_bondage, extractiveness, 0.78).
narrative_ontology:constraint_metric(roman_debt_bondage, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(roman_debt_bondage, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_debt_bondage, snare).
narrative_ontology:human_readable(roman_debt_bondage, "Roman Debt Bondage and Nexum").
narrative_ontology:topic_domain(roman_debt_bondage, "economic/legal/social").

domain_priors:requires_active_enforcement(roman_debt_bondage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_debt_bondage, creditors).
narrative_ontology:constraint_beneficiary(roman_debt_bondage, patrician_landowning_class).
narrative_ontology:constraint_victim(roman_debt_bondage, debtors).
narrative_ontology:constraint_victim(roman_debt_bondage, plebeian_smallholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BOUND DEBTOR (SNARE) — A debtor who enters nexum has no structural exit. Legal prohibition on breach, no alternative credit sources, economic dependency on creditor for subsistence. Debt accrues through interest and penalties, lifetime servitude common. Maximum suppression through legal enforcement and social prohibition. The constraint extracts labor value while stripping legal personhood.
constraint_indexing:constraint_classification(roman_debt_bondage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE DEBTOR'S HOUSEHOLD (SNARE) — Intergenerational bondage: children of nexum debtors inherit debt status. No generational escape clause. Household labor becomes collateral. Suppression operates through legal status degradation — nexal persons lose protection under early Roman law.
constraint_indexing:constraint_classification(roman_debt_bondage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE CREDITOR CLASS (ROPE) — Benefits from debt-collection enforcement and labor extraction. Experiences the constraint as pure coordination: nexum solves the creditor's verification problem (how to ensure repayment) and labor-availability problem (securing workers for estates). The system is coordination from the beneficiary's position — it solves real problems of credit distribution and labor supply.
constraint_indexing:constraint_classification(roman_debt_bondage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE REFORM COALITION / LEX POETELIA (SCAFFOLD) — Organized plebeian resistance and patrician reformers (260s BCE onwards) gradually restricted nexum: debt bondage prohibited, debt-servitude limited, personal guarantee replaced with property seizure. The constraint had a sunset clause: Lex Poetelia (circa 326 BCE) and subsequent legislation gradually dismantled nexum as a mechanism. Suppression declining over time as legal alternatives (property foreclosure, bankruptcy proceedings) replaced personal servitude.
constraint_indexing:constraint_classification(roman_debt_bondage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TRANSITION-PHASE DEBTOR / POST-LEX POETELIA (TANGLED ROPE) — After legal reform, a debtor faces high but surmountable costs to exit (property seizure, debt sale, manumission purchase). The constraint coordinates credit access (debtors can still obtain loans because creditors have recovery mechanisms) while extracting asymmetrically (property loss, extended repayment terms, social penalty). Both genuine coordination function and asymmetric extraction coexist.
constraint_indexing:constraint_classification(roman_debt_bondage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: DEGRADED IMPERIAL SYSTEM / LATE ANTIQUITY (PITON) — By late Empire, nexum had become largely theatrical — its legal form persisted but its functional mechanism (debt enforcement via personal bondage) was superseded by property law, slave markets (a parallel coercive institution), and estate-based labor systems. The institutional structure remained through inertia but was no longer the primary extraction mechanism. Theater ratio high: the form of nexal obligation persisted while substance migrated to slavery and feudal-precursor arrangements.
constraint_indexing:constraint_classification(roman_debt_bondage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE AT STRUCTURAL LEVEL) — From a civilizational perspective, Roman debt bondage was a mechanism for converting liquidity crises into permanent labor extraction. The constraint existed because creditors could extract value through time-binding: converting a temporary debt into lifetime servitude. The analytical observer sees the snare as the core structure — all other perspectives (rope, scaffold, piton) are variations in how the extraction mechanism operated or degraded over time.
constraint_indexing:constraint_classification(roman_debt_bondage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_debt_bondage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_debt_bondage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_debt_bondage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_debt_bondage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_debt_bondage, TR),
    TR >= 0.70.

:- end_tests(roman_debt_bondage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Nexum extracts the maximum value a debtor can produce over their lifetime. Interest accruals and penalties make repayment structurally difficult; creditors benefit from protracted servitude. The 0.78 value (not maximal 1.0) reflects that nexum required explicit legal contract — some debtors could avoid it by refusing the formal nexum procedure, though alternatives were scarce. Over 450 years, extractiveness declines to 0.45 as legal reform and alternative labor systems (slavery, estate labor) reduce nexum's utility. Suppression (0.82): Very high. Multiple enforcement layers: legal prohibition on breach, social stigma, loss of legal personhood, no legitimate exit routes, creditor control over subsistence. Suppression is enforced through state legal machinery and social hierarchy. Theater ratio (0.35): Low. Nexum was functionally direct extraction — the legal form matched the operational mechanism. By late antiquity, theater increases (0.35 by 450 CE) as the form persists while substance migrates to slavery and property law, indicating piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   Between the creditor's rope (coordination) and the debtor's snare (extraction), the gap is irreducible to measurement error. From the creditor's structural position, the nexum mechanism solves a genuine coordination problem: how to distribute credit to those without collateral or reputation. From the debtor's position, the same mechanism is pure extraction: it transforms temporary borrowing into permanent servitude. Both perspectives are accurate from their positions. The snare perspective (from analytical/powerless observer) is the structural truth: nexum exists because creditors can extract value by time-binding temporary debts into lifetime obligations. The rope perspective (from creditor/institutional observer) is their legitimate experience: they are coordinating credit distribution. The scaffold perspective (from organized reformers) adds temporal dimension: legal reform gradually weakens the extraction mechanism, creating a real sunset. The piton perspective (from late empire) reveals institutional decay: when slavery and estate labor systems displaced nexum as primary extraction mechanisms, the legal form persisted through inertia even though substance had migrated elsewhere.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (creditor, institutional power, arbitrage exit): d ≈ 0.10. Creditors can always exit the nexum relationship (by freeing the debtor, by shifting to property-based debt collection); they benefit from the mechanism. Derived chi scales low or negative — they experience the constraint as advantageous. Victim (debtor, powerless, trapped exit): d ≈ 0.95. Debtors have no exit, no alternatives, and bear maximum extraction cost. Derived chi scales high — maximum effective extractiveness from this perspective. Organized reformers (moderate power, constrained exit): d ≈ 0.55. They are neither pure victims nor beneficiaries; they can exit the system through political action or geographic relocation, but face high costs (political opposition, loss of social standing). Their classification (scaffold) reflects that they can envision and partially actualize an exit path (legal reform). Transition-phase debtors (post-reform, moderate power, constrained exit): d ≈ 0.58. They face high but surmountable costs (property loss, extended repayment); the constraint now coordinates credit access while extracting asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through temporal and perspectival analysis. At any single time point, the classification depends entirely on the observer's structural position. The debtor sees a snare; the creditor sees coordination; the reformer sees a temporary system with a sunset. All three perspectives are structurally correct — they describe the same mechanism from different positions. The mandatrophy resolves by recognizing that nexum's core function (extracting labor value through time-binding debt) makes the snare classification the structural truth, while rope and scaffold are valid perspectival readings that obscure the extraction. By late antiquity (450 CE), extractiveness has declined enough that nexum is functionally degraded (piton) — slavery and property law have taken over its extraction role. The institutional form persists (still called nexum in law codes) but the substance has migrated, indicating that the constraint's primary mechanism is no longer operable. This temporal degradation is diagnostic: constraints that classify differently over time are revealing how institutional forms persist after their functional mechanisms are displaced — classic piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_accrual_mechanism,
    'Did compound interest and penalties on debt operate as interest-rate escalation or as deliberate debt-extension mechanism?',
    'Historical analysis of nexum contracts, interest rates recorded in legal texts, and empirical modeling of debt repayment capacity vs. interest accrual',
    'If compound interest was explicit: extraction mechanism was structural (creditors designed unpayability). If rates were standard/low: debt accrual was side effect of default, reducing extraction classification severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_accrual_mechanism, empirical, 'Role of compound interest in debt extension').

omega_variable(
    alternative_credit_availability,
    'What proportion of Roman debtors had access to non-nexum credit sources (patronage loans, family credit, collective lending)?',
    'Historical survey of credit access patterns; analysis of debt contracts vs. informal agreements in archaeological and legal records',
    'If nexum was the only credit source for plebeians: suppression maximal, snare classification solid. If alternatives existed but were blocked by status or cost: suppression moderate, shifts to tangled_rope perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_credit_availability, empirical, 'Availability of alternative credit mechanisms').

omega_variable(
    plebeian_coalition_efficacy,
    'Did organized plebeian resistance (secession, political coalition) directly cause nexum restrictions, or were reforms driven by elite conflict and pragma?',
    'Historical chronology of plebeian secessions vs. nexum legislation; analysis of patrician reformer motivations (political appeasement vs. structural innovation)',
    'If plebeian pressure was decisive: scaffold perspective confirmed (organized exit threats enabled reform). If elite pragmatism drove reform: scaffold is less structural, reform follows from beneficiary loss-aversion rather than victim organizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_coalition_efficacy, empirical, 'Role of plebeian organizing in nexum reform').

omega_variable(
    extraction_vs_coordination_boundary,
    'At what point does debt-collection enforcement become labor extraction rather than legitimate credit recovery?',
    'Comparative analysis: nexum suppression mechanisms vs. other Roman legal debt procedures; social outcomes (lifetime servitude vs. property loss) across mechanisms',
    'If debt recovery requires personal servitude: extraction classification is correct. If alternative mechanisms could recover debt without bondage: nexum''s snare character is choice, not necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Boundary between debt enforcement and labor extraction').

omega_variable(
    identity_lock_mechanism,
    'Did nexal persons internalize subordinate status, or was compliance purely forced through legal coercion?',
    'Analysis of legal status terminology, social role descriptions, and narrative framing of nexal debt in Roman historical sources',
    'If identity-locked: suppression contains internalized component, increases actual behavioral compliance beyond legal enforcement. If purely coerced: suppression is structural, psychological resistance remains latent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Role of identity internalization in nexal compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_debt_bondage, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roma_tr_t0, roman_debt_bondage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(roma_tr_t150, roman_debt_bondage, theater_ratio, 150, 0.25).
narrative_ontology:measurement(roma_tr_t300, roman_debt_bondage, theater_ratio, 300, 0.35).
narrative_ontology:measurement(roma_tr_t450, roman_debt_bondage, theater_ratio, 450, 0.52).

% Extraction over time
narrative_ontology:measurement(roma_be_t0, roman_debt_bondage, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(roma_be_t150, roman_debt_bondage, base_extractiveness, 150, 0.78).
narrative_ontology:measurement(roma_be_t300, roman_debt_bondage, base_extractiveness, 300, 0.65).
narrative_ontology:measurement(roma_be_t450, roman_debt_bondage, base_extractiveness, 450, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_debt_bondage, resource_allocation).
narrative_ontology:affects_constraint(roman_debt_bondage, roman_chattel_slavery).
narrative_ontology:affects_constraint(roman_debt_bondage, feudal_debt_servitude).

% DUAL FORMULATION NOTE:
% Nexum is part of a constraint family spanning Roman labor extraction mechanisms. Chattel slavery (ε ≈ 0.95) is a distinct constraint with direct ownership and no legal sunset. Feudal debt servitude (ε ≈ 0.65) is downstream: it inherits debt-bondage mechanisms from nexum but embeds them in land-holding relationships. Nexum's extractiveness (0.78) is intermediate: higher extraction than generalized resource-allocation coordination, lower than pure slavery, and with a genuine historical sunset clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
