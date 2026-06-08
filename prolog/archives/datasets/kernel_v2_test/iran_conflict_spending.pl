% ============================================================================
% CONSTRAINT STORY: iran_conflict_spending
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_conflict_spending, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iran_conflict_spending
 *   human_readable: Iran Conflict Defense Spending Unpredictability
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The Iran conflict spending constraint describes the structural
 *   unpredictability imposed on federal fiscal planning by potential defense
 *   budget increases of up to 42% plus supplemental appropriations for
 *   weaponry replenishment and operational costs. This constraint operates
 *   through the constitutional appropriations process but exhibits snare
 *   characteristics from multiple perspectives: fiscal predictability as a
 *   collective good is trapped and bears maximum extraction; domestic program
 *   managers face constrained exit and high extraction through zero-sum
 *   budget competition; general taxpayers are powerless and trapped. The
 *   constraint has genuine coordination content (democracies must respond to
 *   security threats with fiscal flexibility) but the mechanism is
 *   asymmetrically extractive: unpredictability is amplified by institutional
 *   arrangements that concentrate discretion in the executive branch,
 *   suppress alternatives, and create ratchet effects where temporary
 *   increases become permanent. The theater ratio (0.58) reflects substantial
 *   performative content: conflict justifications are used to accelerate
 *   pre-planned procurement, and the framing of supplementals as 'emergency'
 *   or 'unforeseen' masks baseline budget expansion. Suppression (0.72) is
 *   high: alternatives like advance appropriations with transparent triggers
 *   are not seriously considered, and congressional override capacity during
 *   conflict escalation is nominal rather than real.
 *
 * KEY AGENTS:
 *   - Fiscal Predictability: Primary victim (powerless/trapped) — abstract collective good with no advocate; bears full cost of budget volatility
 *   - Domestic Discretionary Program Managers: Secondary victim (moderate/constrained) — face reactive cuts and planning disruption; constrained exit within appropriations process
 *   - Congressional Budget Committees: Institutional victim (institutional/constrained) — lose budget control and multi-year planning capacity despite formal authority; tangled_rope experience
 *   - General Taxpayers: Victim (powerless/trapped) — bear fiscal cost through deficits or displaced spending; cannot exit tax base or organize resistance
 *   - Defense Contractors and Pentagon Procurement: Primary beneficiaries (institutional/arbitrage) — capture replenishment contracts and expanded budgets; arbitrage exit to commercial or international markets
 *   - Hawkish Policy Coalitions: Secondary beneficiaries (organized/mobile) — conflict escalation vindicates perpetual readiness doctrine and expands defense policy influence
 *   - Fiscal Responsibility Advocacy Coalitions: Organized agents (organized/constrained) — advocate for discipline but systematically overridden during conflict; tangled_rope experience
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees genuine coordination function but recognizes asymmetric extraction through institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_conflict_spending, 0.68).
domain_priors:suppression_score(iran_conflict_spending, 0.72).
domain_priors:theater_ratio(iran_conflict_spending, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_conflict_spending, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_conflict_spending, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iran_conflict_spending, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_conflict_spending, snare).
narrative_ontology:human_readable(iran_conflict_spending, "Iran Conflict Defense Spending Unpredictability").
narrative_ontology:topic_domain(iran_conflict_spending, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(iran_conflict_spending).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_conflict_spending, defense_contractors).
narrative_ontology:constraint_beneficiary(iran_conflict_spending, pentagon_procurement_offices).
narrative_ontology:constraint_beneficiary(iran_conflict_spending, hawkish_policy_coalitions).
narrative_ontology:constraint_victim(iran_conflict_spending, fiscal_predictability).
narrative_ontology:constraint_victim(iran_conflict_spending, domestic_discretionary_programs).
narrative_ontology:constraint_victim(iran_conflict_spending, congressional_budget_authority).
narrative_ontology:constraint_victim(iran_conflict_spending, taxpayers_general).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCAL PREDICTABILITY (SNARE) — The abstract collective good of budget stability cannot exit the constraint. Unpredictable defense outlays cascade through the entire federal budget process, forcing reactive cuts to domestic programs and undermining multi-year planning. Maximum extraction: the commons bears full cost of volatility with no advocate and no exit.
constraint_indexing:constraint_classification(iran_conflict_spending, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC PROGRAM MANAGERS (SNARE) — Constrained by appropriations process but cannot exit the zero-sum budget competition. When supplementals pass for defense, domestic discretionary faces cuts or freezes to maintain aggregate spending caps. High extraction: career stability and program continuity are sacrificed to absorb defense volatility.
constraint_indexing:constraint_classification(iran_conflict_spending, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL BUDGET COMMITTEES (TANGLED ROPE) — Institutional actors with formal authority over appropriations but constrained by political pressure to approve defense supplementals during conflict escalation. Genuine coordination function exists (allocating resources to national defense) but extraction is asymmetric: committees lose budget control and multi-year planning capacity while executive branch gains discretion. Mixed experience: some agency, substantial cost.
constraint_indexing:constraint_classification(iran_conflict_spending, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Primary beneficiaries with arbitrage-level exit options (can shift to commercial aerospace, international arms sales, or other sectors). Experience the constraint as pure coordination: conflict escalation triggers replenishment contracts and expanded procurement budgets. Net beneficiaries: extraction flows toward them, not away.
constraint_indexing:constraint_classification(iran_conflict_spending, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL ADVOCACY COALITIONS (TANGLED ROPE) — Organized agents (deficit hawks, budget watchdog groups, bipartisan fiscal commissions) with some coordination capacity but constrained by political dynamics during conflict. Genuine coordination function: advocating for budget discipline and transparency. But extraction is real: their advocacy is systematically overridden during security escalations, and the unpredictability they oppose becomes normalized. Mixed experience: agency without efficacy.
constraint_indexing:constraint_classification(iran_conflict_spending, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GENERAL TAXPAYERS (SNARE) — Powerless and trapped: cannot exit the tax base, cannot organize effectively to resist supplemental appropriations during conflict escalation, and bear the fiscal cost through either higher deficits (future tax burden) or displaced domestic spending (immediate service cuts). Maximum extraction with no exit.
constraint_indexing:constraint_classification(iran_conflict_spending, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, defense spending volatility serves a genuine coordination function: democracies must be able to respond to security threats with fiscal flexibility. But the mechanism is asymmetrically extractive: the unpredictability is not inherent to conflict response but is amplified by institutional arrangements that concentrate discretion in the executive branch, suppress alternatives (advance appropriations, contingency reserves with transparent triggers), and create ratchet effects where temporary increases become permanent baselines. The analytical classification is tangled_rope, not mountain: the constraint is changeable through institutional reform.
constraint_indexing:constraint_classification(iran_conflict_spending, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_conflict_spending_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_conflict_spending, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_conflict_spending, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_conflict_spending, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(iran_conflict_spending_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from fiscal predictability, domestic programs, congressional budget authority, and taxpayers while concentrating benefits on defense contractors, Pentagon procurement offices, and hawkish coalitions. The 42% potential budget increase plus supplementals represents substantial extraction: not all spending is genuinely unforeseen (omega variable tracks pre-planned vs. responsive spending), and historical patterns show conflict-driven increases become permanent baselines (ratchet effect). The value reflects that a large portion of the extraction is structural rather than inherent to conflict response. Suppression (0.72): High. Alternatives to ad-hoc supplementals (advance appropriations with transparent triggers, contingency reserves, multi-year authorizations) are systematically excluded from consideration. Congressional override capacity during conflict is nominal: political costs of voting against defense supplementals during escalation are prohibitive, making executive requests effectively compulsory. Exit options for victims are minimal: fiscal predictability cannot exit, domestic programs cannot escape zero-sum competition, taxpayers cannot exit the tax base. Theater ratio (0.58): Moderate-high. Substantial performative content: conflict justifications are used to accelerate procurement that was already in development pipelines (unfunded priorities lists become supplemental line items), and the framing of spending as 'emergency' or 'unforeseen' masks baseline expansion. The theater has increased over the interval as the gap between claimed necessity and actual novelty has widened. However, some spending is genuinely responsive to operational needs, preventing a higher theater value.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a clear extraction gradient across power and exit dimensions. Powerless/trapped agents (fiscal predictability, general taxpayers) experience pure snare: maximum extraction with no exit and no coordination benefit. Moderate/constrained agents (domestic program managers) experience snare with slightly lower extraction: they have some agency within the appropriations process but face systematic displacement. Institutional/constrained agents (congressional budget committees, fiscal advocacy coalitions) experience tangled_rope: genuine coordination function exists (allocating resources to national defense, advocating for discipline) but extraction is asymmetric (loss of budget control, systematic override of advocacy). Institutional/arbitrage agents (defense contractors) experience rope: net beneficiaries who see the constraint as pure coordination enabling their business model. The analytical observer sees tangled_rope at civilizational scope: the coordination function is real (democracies need fiscal flexibility for security threats) but the mechanism is extractive through institutional design choices that are changeable, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Defense contractors and Pentagon procurement are declared beneficiaries with arbitrage exit → low d → negative or low chi (they experience subsidy or minimal extraction). Fiscal predictability and general taxpayers are declared victims with trapped exit → high d → high chi (maximum extraction). Domestic program managers are victims with constrained exit → moderately high d → substantial chi. Congressional budget committees are victims with constrained exit but institutional power → moderate d → moderate chi (some agency reduces experienced extraction despite victim status). Fiscal advocacy coalitions are not declared victims but experience extraction through systematic override → moderate d derived from organized power + constrained exit. The analytical observer is neither beneficiary nor victim → d near 0.5 → moderate chi, but the classification is tangled_rope based on structural data (coordination function + asymmetric extraction + active enforcement) rather than high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that snare and tangled_rope classifications can coexist for the same structural phenomenon when measured from different perspectives. The mandate (fiscal flexibility for national defense) has not outlived its function — democracies genuinely need capacity to respond to security threats. But the mechanism through which that mandate is executed (ad-hoc supplementals with concentrated executive discretion and suppressed alternatives) is extractive. The snare classification from powerless/trapped perspectives captures the extraction experienced by those with no exit. The tangled_rope classification from institutional/constrained and analytical perspectives captures that coordination and extraction coexist in the same mechanism. The constraint is not mandatrophy (function outlived) but structural extraction (function real, mechanism extractive). The perspectival gap is the resolution: both classifications are correct from their respective observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supplemental_necessity_threshold,
    'What portion of the projected 42% defense budget increase and supplemental appropriations represents genuine unforeseen conflict costs versus pre-planned procurement accelerated under conflict justification?',
    'Forensic budget analysis comparing pre-conflict Pentagon unfunded priorities lists with post-escalation supplemental line items; tracking which programs were already in development pipelines versus genuinely new requirements',
    'If >60% represents pre-planned procurement: extraction is higher than base metric suggests, and the conflict is being used as cover for baseline budget expansion. If <40%: more of the spending is genuinely responsive to unforeseen operational needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supplemental_necessity_threshold, empirical, 'Proportion of supplemental spending that is genuinely unforeseen versus pre-planned').

omega_variable(
    ratchet_permanence,
    'Do temporary conflict-driven defense budget increases return to baseline after conflict de-escalation, or do they become permanent through bureaucratic and contractor lock-in?',
    'Historical analysis of post-conflict defense budget trajectories (post-Iraq drawdown, post-Afghanistan); identification of which supplemental-funded programs were terminated versus absorbed into base budgets',
    'If increases are permanent: extractiveness is substantially higher because the ''temporary'' framing is theater masking permanent extraction. If increases reverse: the constraint is more genuinely responsive to transient security needs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratchet_permanence, empirical, 'Whether conflict-driven budget increases become permanent baseline').

omega_variable(
    alternative_mechanism_viability,
    'Could advance appropriations with transparent escalation triggers (similar to disaster relief contingency funds) provide equivalent fiscal flexibility with lower extraction and suppression?',
    'Comparative institutional analysis: countries with pre-authorized defense contingency mechanisms versus ad-hoc supplemental systems; assessment of whether transparency and pre-commitment reduce rent-seeking',
    'If viable alternatives exist: the current mechanism''s high suppression (0.72) is not inherent to democratic conflict response but is a choice that benefits discretion-holders. If alternatives fail: the constraint may be closer to mountain (inherent trade-off between flexibility and predictability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_viability, conceptual, 'Whether transparent contingency mechanisms could replace ad-hoc supplementals').

omega_variable(
    congressional_override_capacity,
    'Does Congress retain meaningful capacity to reject or substantially modify defense supplemental requests during conflict escalation, or is approval effectively compulsory due to political dynamics?',
    'Historical voting analysis: frequency and magnitude of congressional modifications to executive supplemental requests during active conflicts; identification of political costs faced by members who vote against defense supplementals',
    'If override capacity is real: suppression is lower and congressional budget authority is less eroded. If approval is compulsory: suppression is higher and the constraint operates more as executive discretion than legislative appropriation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_override_capacity, empirical, 'Whether congressional appropriations authority is real or nominal during conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_conflict_spending, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_spend_tr_t0, iran_conflict_spending, theater_ratio, 0, 0.38).
narrative_ontology:measurement(iran_spend_tr_t2, iran_conflict_spending, theater_ratio, 2, 0.42).
narrative_ontology:measurement(iran_spend_tr_t4, iran_conflict_spending, theater_ratio, 4, 0.48).
narrative_ontology:measurement(iran_spend_tr_t6, iran_conflict_spending, theater_ratio, 6, 0.52).
narrative_ontology:measurement(iran_spend_tr_t8, iran_conflict_spending, theater_ratio, 8, 0.55).
narrative_ontology:measurement(iran_spend_tr_t10, iran_conflict_spending, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(iran_spend_extract_baseline, iran_conflict_spending, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(iran_spend_be_t2, iran_conflict_spending, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(iran_spend_be_t4, iran_conflict_spending, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(iran_spend_be_t6, iran_conflict_spending, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(iran_spend_be_t8, iran_conflict_spending, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(iran_spend_be_t10, iran_conflict_spending, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(iran_spend_su_t0, iran_conflict_spending, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(iran_spend_su_t3, iran_conflict_spending, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(iran_spend_su_t6, iran_conflict_spending, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(iran_spend_su_t10, iran_conflict_spending, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_conflict_spending, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a single structural phenomenon (unpredictable defense spending during Iran conflict escalation) rather than a decomposed family. The observable (Pentagon monthly obligations, supplemental appropriation size, FY2027 budget authority) is stable and does not produce different epsilon values under different measurement approaches. If future analysis identifies structurally distinct sub-constraints (e.g., weaponry replenishment vs. operational costs vs. forward deployment infrastructure), those would warrant separate stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_conflict_spending, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
