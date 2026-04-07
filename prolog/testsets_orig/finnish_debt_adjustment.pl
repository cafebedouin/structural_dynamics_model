% ============================================================================
% CONSTRAINT STORY: finnish_debt_adjustment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finnish_debt_adjustment, []).

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
 *   constraint_id: finnish_debt_adjustment
 *   human_readable: Finnish Private Debt Adjustment System
 *   domain: economic/political
 *
 * SUMMARY:
 *   Finland's debt adjustment system emerged from the 1990s economic crisis
 *   when over-indebtedness became widespread across the population. The legal
 *   mechanism allows court-administered restructuring of private debts:
 *   debtors negotiate reduced payment schedules with creditors (coordinated
 *   by court trustees), and remaining debt can be discharged after 5 years of
 *   compliance. The system demonstrates how a single structural arrangement
 *   can legitimately classify as six different constraint types depending on
 *   the observer's position. For debtors trapped without alternatives, it is
 *   a Snare — years of income disclosure and consumption restriction. For
 *   participants within it, it is a Tangled Rope mixing coordination benefits
 *   (creditor relief) with extraction (fees, monitoring). For financial
 *   institutions, it is a Rope solving the collective action problem of
 *   competing creditors. For consumer advocates, it is a Scaffold with a
 *   sunset as wages rise and prevention becomes viable. For court
 *   administrators, it is a Tangled Rope generating rent. The vestigial
 *   stigma attached to debt adjustment (echoing abolished debtor's prison) is
 *   a Piton — performed shame serving no functional purpose. The false
 *   mountain perspective naturalizes the system as an immutable requirement
 *   of credit economies, but the extractiveness metrics reveal it as a
 *   contingent institutional design. The constraint's theater ratio has
 *   declined over 30 years (0.72 → 0.58) as the system matured and
 *   formalized, and extractiveness has declined (0.48 → 0.38) as credit
 *   regulation improved and household income recovered. The trend supports
 *   the scaffold perspective: the system is solving a temporary problem with
 *   declining severity.
 *
 * KEY AGENTS:
 *   - Over-indebted Debtors: Primary victims (powerless/trapped) — face years of income disclosure, consumption restriction, and social stigma; trapped by lack of alternative insolvency relief
 *   - Financial Institutions: Primary beneficiaries (institutional/arbitrage) — benefit from coordinated creditor structure that enables debt recovery and reduces legal uncertainty
 *   - Debt Adjustment Administrators (Court Trustees): Secondary beneficiaries (organized/mobile) — extract professional fees and maintain institutional power over debtor finances; coordinate creditor claims
 *   - Consumer Protection Movement: Organized advocates (organized/constrained) — view system as temporary intervention with sunset; work toward systemic prevention through income support and credit regulation
 *   - Creditor Coordination: Implicit beneficiary — the system solves the multiple-creditor race problem; without it, creditors would compete destructively
 *   - Vestigial Debtor Shame: Institutional actor (institutional/arbitrage) — social norm of debtor stigma persists through courts; maintains performative control despite official compassion framing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent coordination mechanism as law of credit economies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_debt_adjustment, 0.38).
domain_priors:suppression_score(finnish_debt_adjustment, 0.48).
domain_priors:theater_ratio(finnish_debt_adjustment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_debt_adjustment, extractiveness, 0.38).
narrative_ontology:constraint_metric(finnish_debt_adjustment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(finnish_debt_adjustment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finnish_debt_adjustment, tangled_rope).
narrative_ontology:human_readable(finnish_debt_adjustment, "Finnish Private Debt Adjustment System").
narrative_ontology:topic_domain(finnish_debt_adjustment, "economic/political").

domain_priors:requires_active_enforcement(finnish_debt_adjustment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, financial_institutions).
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, debt_adjustment_administrators).
narrative_ontology:constraint_victim(finnish_debt_adjustment, over_indebted_individuals).
narrative_ontology:constraint_victim(finnish_debt_adjustment, creditor_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-INDEBTED DEBTOR (SNARE) — Trapped in debt cycle with minimal exit options. Adjustment program is technically available but requires acceptance of income cuts, asset restructuring, and years of supervision. Cannot exit the financial system; lives with debt stigma and constrained consumption throughout the adjustment period. Suppression is high: creditors retain leverage through wage garnishment threats, collateral seizure, and the debtor's psychological entrapment.
constraint_indexing:constraint_classification(finnish_debt_adjustment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADJUSTMENT PARTICIPANTS (TANGLED ROPE) — Those formally enrolled in adjustment programs experience genuine coordination benefits (predictable payment schedules, reduced creditor harassment, partial debt forgiveness after 5 years) alongside extraction (court fees, mandatory financial counseling costs, years of income disclosure and supervisory control). They have entered a binding coordination structure that solves the collective action problem of multiple creditors but at significant personal cost.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — Banks and credit companies benefit from the adjustment system as a coordination mechanism: instead of chaotic individual bankruptcy proceedings, they coordinate recovery through the court system with predictable outcomes and reduced legal uncertainty. The system enables them to recover some portion of bad debts while avoiding default cascades. They experience this as pure coordination (solving the creditor competition problem) rather than extraction.
constraint_indexing:constraint_classification(finnish_debt_adjustment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMER PROTECTION MOVEMENT (SCAFFOLD) — Organized consumer advocates see the adjustment program as a temporary intervention with a sunset clause: as wage levels rise, household financial literacy improves, and credit regulation tightens, the need for debt adjustment should structurally decline. The program solves an immediate crisis (over-indebtedness from 1990s recession) but advocates view it as transitional, aiming toward systemic prevention (income support, credit regulation) that would make adjustment unnecessary. Theater remains moderate because the intervention addresses real coordination failures.
constraint_indexing:constraint_classification(finnish_debt_adjustment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VESTIGIAL DEBTOR SHAME (PITON) — Finland abolished debtor's prison in 1883, yet the social stigma and psychological treatment of debtors retains significant performative content. The adjustment program preserves (rather than eliminates) this theatrical stigma: public court proceedings, years of income monitoring, and social exclusion continue the ancient function of shaming debtors, even though the system is officially designed for relief. The theater ratio is high because the stigma serves no functional coordination purpose but remains through institutional inertia.
constraint_indexing:constraint_classification(finnish_debt_adjustment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DEBT ADJUSTMENT ADMINISTRATORS (TANGLED ROPE) — Court trustees and debt adjustment administrators operate a coordination mechanism (managing creditor claims, scheduling payments, enforcing compliance) while also extracting rents through mandatory fees, professional control over debtor finances, and the power to extend or terminate adjustment status. They benefit from the system's continuation (job creation, fee revenue) while coordinating what would otherwise be chaotic multi-creditor disputes.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some form of insolvency resolution is a fundamental requirement of any credit system. Without structured debt adjustment mechanisms, either debtors must face debtor's prison (pre-1883) or credit markets collapse entirely (no one lends if unpayable debts cannot be discharged). From this perspective, the Finnish system appears to solve an immutable coordination problem inherent to credit economies. However, the structural data reveals this as a false summit: the system's extractiveness (0.38) and suppression (0.48) indicate contingent institutional design choices, not natural law.
constraint_indexing:constraint_classification(finnish_debt_adjustment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_debt_adjustment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(finnish_debt_adjustment, TR),
    TR >= 0.70.

:- end_tests(finnish_debt_adjustment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, declining to 0.38 by year 30): Moderate. The system extracts from debtors through mandatory fees (typically 2-3% of restructured debt), years of income monitoring, consumption restrictions, and psychological costs. However, extractiveness is tempered by genuine coordination benefits: debtors receive partial debt forgiveness, protection from wage garnishment escalation, and structured predictability replacing chaotic multi-creditor enforcement. The declining trajectory (0.48 → 0.38 over 30 years) reflects improved economic conditions and reduced over-indebtedness, making the extraction less severe. Suppression (0.48): Moderate-high. Debtors face significant coercive mechanisms: income disclosure requirements, asset controls, mandatory financial counseling (sometimes) that reduces actual exit options. However, suppression is not total — adjustment is technically voluntary (though alternatives are unattractive), and the system provides structured relief rather than pursuing maximum extraction. The suppression reflects real institutional control but not absolute coercion. Theater ratio (0.58, declining from 0.72): Moderate-high. The adjustment program retains significant performative elements: court proceedings are public, income monitoring is documented and socially visible, and the debtor carries years of marked status within the financial system. However, theater has declined as digital processes replaced paper-based public record-keeping and as credit system professionalization normalized debt restructuring rather than viewing it as moral failure. The theater reflects vestigial stigma from abolished debtor's prison rather than modern functional requirements.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across the six types: Debtors see a Snare (pure coercion, extraction, no coordination benefit). Adjustment participants see a Tangled Rope (mixed benefits and extraction). Financial institutions see a Rope (pure coordination solving creditor competition). Consumer advocates see a Scaffold (temporary intervention with sunset). Administrators see a Tangled Rope (rent extraction + coordination function). Vestigial stigma is a Piton (performative without function). The analytical observer risks a false Mountain (naturalizing design as law). The gap exists because each agent occupies a fundamentally different structural position: debtors are extraction targets; institutions are coordination beneficiaries; administrators are hybrid beneficiary-extractors; advocates see systemic change; social stigma operates mechanically. The classification gap is not a measurement problem but a reflection of genuine structural asymmetry. The debtor's Snare view is not wrong; neither is the institution's Rope view. Both are correct from their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agents' structural relationships to the system: Debtors (powerless/trapped) experience maximum negative d → high f(d) → high effective extraction (chi). Financial institutions (institutional/arbitrage) experience beneficiary d → low/negative f(d) → negative chi (they experience coordination benefit, not extraction cost). Adjustment administrators (organized/mobile) experience ambiguous d: they benefit from the system's continuation and extract fees, but also solve creditor coordination problems. The system-level d for administrators is set to 0.35 (partial beneficiary with extraction capacity), producing moderate f(d) and moderate chi. Consumer advocates (organized/constrained) experience d = 0.60 (they bear some of the system's costs through advocacy effort while seeing progress toward sunset) producing moderate f(d). The analytical observer's d at civilizational scale is set to 0.72 (neutral between natural law and contingency) which the engine interprets as false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival honesty rather than collapse to a single type. The false mountain perspective (natural law view) is explicitly identified and marked as analytical naturalization: insolvency resolution is necessary in all credit systems, but the Finnish implementation is not a natural law — it is a contingent institutional design with extractiveness (0.38) and suppression (0.48) that reflect policy choices, not physical necessity. The system genuinely provides coordination benefits (solving creditor competition, structured payment paths) for some actors (institutions, administrators) and genuine extraction for others (debtors). It is not pure extraction misclassified as coordination (the snare mandatrophy), nor is it pure coordination misclassified as extraction (the rope mandatrophy). The Tangled Rope classification for adjustment participants and administrators correctly captures the hybrid nature: the system coordinates creditor claims AND extracts from debtors simultaneously. The declining extractiveness trajectory (0.48 → 0.38) and declining theater ratio (0.72 → 0.58) suggest the system is trending toward pure Rope (coordination without extraction) as external conditions improve and design matures. This is consistent with the scaffold sunset hypothesis: as the underlying problem (over-indebtedness) declines, the system's extraction component should also decline, leaving only the coordination machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'Do court-administered fees and income restrictions in the adjustment program constitute legitimate coordination costs or extractive rent-seeking by administrators?',
    'International comparison of debt adjustment systems (Sweden, Germany, France) and their fee structures; analysis of trustee fee distributions vs. actual case management costs; surveys of debtor outcomes with varying fee levels',
    'If coordination: system should classify as Rope from administrator perspective. If rent-seeking: should classify as Snare from debtor perspective despite availability of adjustment option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether administrator fees are coordination costs or extraction').

omega_variable(
    voluntary_participation_constraint,
    'Is entry into debt adjustment effectively voluntary (genuine exit option for debtors) or coercive (implicit threat of wage garnishment / asset seizure forces participation)?',
    'Analysis of alternative exit routes for debtors who refuse adjustment (actual enforcement rates of wage garnishment vs. announced rates); interviews with debtors about participation decisions; historical data on debtor outcomes comparing adjustment participants vs. non-participants',
    'If voluntary: debtor exit_options should be ''mobile'' (actual choice available). If coercive: should be ''trapped'' (participation forced by threat). Shifts debtor classification from Snare to potentially Tangled_Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_constraint, empirical, 'Whether adjustment participation is genuinely voluntary').

omega_variable(
    sunset_mechanism_realism,
    'Is the scaffold perspective''s claim about a sunset clause realistic, or is the adjustment system structural and permanent?',
    'Longitudinal data on adjustment program caseloads (1990-2026); relationship between household debt levels and adjustment program demand; analysis of whether credit regulation and income support are actually reducing over-indebtedness or merely displacing it',
    'If sunset realistic: Scaffold classification is valid and extractiveness should decline over interval. If permanent: system should classify as persistent extraction mechanism (Snare or Tangled_Rope) regardless of intended temporality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_realism, empirical, 'Whether the debt adjustment system has a realistic sunset').

omega_variable(
    psychological_credibility_collapse,
    'How much of the adjustment program''s control over debtors (income disclosure, consumption restrictions, years of monitoring) rests on psychological credibility vs. legal enforcement power?',
    'Analysis of debtor compliance rates when monitoring is reduced; international comparison of systems with similar legal power but different psychological framing; studies of placebo enforcement (announced monitoring vs. actual auditing)',
    'If psychological: suppression (0.48) overstates actual coercive power; theater_ratio should be higher. If legal enforcement: suppression reflects real structural coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(psychological_credibility_collapse, empirical, 'How much suppression is psychological vs. legal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finnish_debt_adjustment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finda_tr_t0, finnish_debt_adjustment, theater_ratio, 0, 0.72).
narrative_ontology:measurement(finda_tr_t15, finnish_debt_adjustment, theater_ratio, 15, 0.62).
narrative_ontology:measurement(finda_tr_t30, finnish_debt_adjustment, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(finda_be_t0, finnish_debt_adjustment, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(finda_be_t15, finnish_debt_adjustment, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(finda_be_t30, finnish_debt_adjustment, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finnish_debt_adjustment, enforcement_mechanism).
narrative_ontology:affects_constraint(finnish_debt_adjustment, nordic_social_contract).
narrative_ontology:affects_constraint(finnish_debt_adjustment, household_debt_accumulation_cycle).

% DUAL FORMULATION NOTE:
% The Finnish adjustment system is downstream of broader Nordic welfare policy (income support, unemployment insurance, credit regulation) and upstream of household financial stability. The system's extractiveness depends on the adequacy of social safety nets: strong income support and employment policy reduce adjustment demand and lower extraction; weak safety nets increase debtor desperation and raise effective suppression. This constraint family requires analysis of the welfare infrastructure supporting or undermining the adjustment mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
