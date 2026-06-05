% ============================================================================
% CONSTRAINT STORY: amendment_history__debt_brake_2009
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_history__debt_brake_2009, []).

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
 *   constraint_id: amendment_history__debt_brake_2009
 *   human_readable: The 2009 Debt Brake Constitutional Amendment
 *   domain: political/legal/fiscal_policy
 *
 * SUMMARY:
 *   The 2009 debt brake (Schuldenbremse) constitutionalized fiscal limits in
 *   the German Basic Law, Article 115. Structural deficits are capped at
 *   0.35% of GDP for the federal government and structural balance is
 *   required for Länder by 2020. The amendment was authored as a response to
 *   fiscal pressures from the financial crisis but implemented the dream of
 *   fiscal conservatives: budget discipline locked into constitutional text,
 *   removed from ordinary electoral politics. The constraint exhibits the
 *   structural pattern of a tangled rope: it coordinates fiscal expectations
 *   (beneficiaries include bond markets and future taxpayers who benefit from
 *   credible deficit limits), but it suppresses countercyclical spending
 *   capacity and long-term public investment (victims include those who need
 *   fiscal stimulus during recessions). The debt brake is the reading of the
 *   'amendment_history' kernel that emphasizes the permanence of fiscal
 *   discipline through constitutionalization. It coexists with other readings
 *   of the same kernel (asylum_compromise, emergency acts, rearmament,
 *   reunification) in the sense that the Basic Law's amendment authority
 *   (Article 79 para. 1) permits multiple distinct constitutional revisions
 *   across time. However, the debt brake's axiom — that fiscal discipline is
 *   a foundational constitutional commitment — influences the subsequent
 *   trajectory of fiscal policy and may constrain future amendments that
 *   would relax spending limits. The extractiveness measurements show rising
 *   trend: as the constraint matures and countries face genuine crises
 *   (COVID-19, energy crisis), the suppression of fiscal response capacity
 *   becomes more visible and more costly.
 *
 * KEY AGENTS:
 *   - Future taxpayers and citizens: Primary beneficiary (institutional/arbitrage) — benefit from debt-sustainable fiscal path and lower future tax burdens or inflation
 *   - Bond markets and creditor nations: Primary beneficiary (institutional/arbitrage) — benefit from reduced perceived default risk, lower borrowing costs, and credible deficit limits
 *   - Countercyclical fiscal policy advocates: Primary victim (moderate/constrained) — face extraction through capped deficit spending during recessions; must accept higher unemployment or underemployment to stay within constitutional limits
 *   - Public investment sector: Secondary victim (organized/constrained) — infrastructure, education, climate adaptation spending constrained by structural balance requirement; can organize to demand supermajority amendment but at high cost
 *   - Fiscal conservative coalition: Institutional beneficiary (institutional/arbitrage) — achieve through constitutional text what electoral politics alone could not lock in; can shape subsequent fiscal discourse by framing debt limits as foundational
 *   - Constitutional Court: Institutional referee (institutional/constrained) — responsible for policing the mathematical ceiling; role is largely self-enforcing and thus performative
 *   - Analytical observer: Universal perspective (analytical/analytical) — risks naturalizing a contingent institutional choice as immutable fiscal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_history__debt_brake_2009, 0.38).
domain_priors:suppression_score(amendment_history__debt_brake_2009, 0.62).
domain_priors:theater_ratio(amendment_history__debt_brake_2009, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_history__debt_brake_2009, extractiveness, 0.38).
narrative_ontology:constraint_metric(amendment_history__debt_brake_2009, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(amendment_history__debt_brake_2009, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_history__debt_brake_2009, tangled_rope).
narrative_ontology:human_readable(amendment_history__debt_brake_2009, "The 2009 Debt Brake Constitutional Amendment").
narrative_ontology:topic_domain(amendment_history__debt_brake_2009, "political/legal/fiscal_policy").

domain_priors:requires_active_enforcement(amendment_history__debt_brake_2009).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_history__debt_brake_2009, '10f6a7c5-4c3c-4e72-8fac-2992b6afb31e').
narrative_ontology:cs_kernel_codification('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', formalized).
narrative_ontology:cs_authority_grounding('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', lineage).
narrative_ontology:cs_interpretation_layer_present('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e').
narrative_ontology:cs_reading_relation('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', amendment_history__asylum_compromise_1993, coexists_with).
narrative_ontology:cs_reading_relation('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', amendment_history__emergency_acts_1968, influences).
narrative_ontology:cs_reading_relation('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', amendment_history__rearmament_1956, influences).
narrative_ontology:cs_reading_relation('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', amendment_history__reunification_amendments_1990, coexists_with).
narrative_ontology:cs_axiom('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', foundational, fiscal_discipline_is_foundational).
narrative_ontology:cs_axiom_status(fiscal_discipline_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', fiscal_discipline_is_foundational, conventional).
narrative_ontology:cs_axiom('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', secondary, structural_balance_over_discretionary_spending).
narrative_ontology:cs_axiom_status(structural_balance_over_discretionary_spending, holdable).
narrative_ontology:cs_axiom_grounding('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', structural_balance_over_discretionary_spending, instrumental).
narrative_ontology:cs_reference_frame('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', fiscal_discretion_constitutionally_unrestricted).
narrative_ontology:cs_drift_state('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', post_2009_financial_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10f6a7c5-4c3c-4e72-8fac-2992b6afb31e', '').
narrative_ontology:cs_kernel_id(amendment_history__debt_brake_2009, amendment_history).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_history__debt_brake_2009, future_taxpayers).
narrative_ontology:constraint_beneficiary(amendment_history__debt_brake_2009, bond_market_credibility).
narrative_ontology:constraint_beneficiary(amendment_history__debt_brake_2009, creditor_nations).
narrative_ontology:constraint_victim(amendment_history__debt_brake_2009, countercyclical_fiscal_capacity).
narrative_ontology:constraint_victim(amendment_history__debt_brake_2009, public_investment).
narrative_ontology:constraint_victim(amendment_history__debt_brake_2009, deficit_spending_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Bound by constitutional text they did not negotiate; structural deficits are capped in perpetuity. No exit from the constraint; bears the opportunity cost of constrained investment spending across generational time. The ceiling is locked into foundational law, not subject to ordinary political override.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COUNTERCYCLICAL AGENTS (TANGLED ROPE) — Benefit from certainty that fiscal rules apply uniformly (coordination function: budget stability is coordinated across electoral cycles). But face high-cost extraction: crisis response capacity is constrained. The escape clause (exception for economic catastrophe) requires supermajority agreement, raising the cost of exit. Mixed experience: coordination discipline benefits long-term credibility; extraction limits crisis response.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BOND MARKETS / CREDITOR NATIONS (ROPE) — Net beneficiaries. The debt brake coordinates expectations: fiscal discipline is locked into constitutional text, reducing perceived default risk and lowering borrowing costs. Can arbitrage by exiting or entering German debt positions. Experiences the constraint as pure coordination: others' budget discipline is their gain.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC INVESTMENT COALITION (TANGLED ROPE) — Organized agents (infrastructure advocates, labor unions, regional governments) benefit from budget stability (coordination) but suffer extraction through capped capital spending. The constraint coordinates across political cycles but suppresses long-term infrastructure investment. Supermajority requirement for escape means high organizational cost to amend.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL CONSERVATIVES (ROPE) — See the constraint as coordination mechanism solving a collective action problem: political pressures for spending create a tragedy of the commons. The debt brake coordinates all parties toward long-term fiscal health. Mobile because they can advocate repeal if conditions change; powerful because their policy preference is now constitutionalized. Net benefit — they achieve through constitutional text what electoral politics alone could not lock in.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL COURT (PITON) — Responsible for policing the debt brake's mathematical ceiling, but the constraint is largely self-enforcing (hard ceiling in accounting). The court's role is performative: reviewing edge cases and escape clause invocations. Theater persists because constitutional adjudication ritualizes what is structurally mechanical.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational scale, the debt brake appears to encode an immutable constraint of modern fiscal reality: no state can indefinitely spend beyond revenues without facing catastrophic currency or bond-market failure. The ceiling mirrors physical laws governing resource scarcity. However, the structural data contradicts this reading — identifiable beneficiaries (creditors, fiscal conservatives) benefit from naturalizing a contingent institutional choice as economic necessity.
constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_history__debt_brake_2009_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_history__debt_brake_2009, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(amendment_history__debt_brake_2009, TR),
    TR >= 0.70.

:- end_tests(amendment_history__debt_brake_2009_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising over time. The constraint does coordinate expectations and lock in fiscal discipline that benefits long-term credibility — these are genuine coordination functions. But the extraction cost is real and measurable: foregone countercyclical spending during recessions, constrained public investment, and reduced fiscal autonomy. The rising trajectory (0.22→0.38 over 15 years) reflects that as crises occur (financial crisis spillovers, energy crisis, pandemic), the extraction becomes more visible — the constraint forces governments to choose between health spending and structural balance, between unemployment relief and deficit limits. Suppression (0.62): High and rising. The constraint suppresses alternatives through multiple mechanisms: (1) the constitutional ceiling is harder to change than ordinary legislation (requires 2/3 majority per Article 79); (2) the escape clause requires supermajority agreement, raising coalition costs; (3) the mathematical constraint is self-enforcing and automatic — no political negotiation can override accounting reality. The rising suppression (0.48→0.65) reflects that the constraint's enforcement architecture strengthens over time as courts establish precedent and accounting procedures harden. Theater ratio (0.35): Low-moderate, rising slowly. The constraint is structurally transparent — the mathematical ceiling is explicit and hard to game. Theater is not the mechanism. However, some theater does accumulate: debate over what counts as 'structural' deficit (one-time costs vs. permanent changes), discussion of escape clause invocations, and ritualized constitutional court review (piton perspective). The rising trajectory (0.25→0.42) shows that as the constraint's real cost becomes apparent, political actors invest in theater (framing arguments, technical definitional disputes) to cope with the suppression.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is profound and grows over time. Fiscal conservatives see coordination (rope) — fiscal discipline is achieved through constitutional commitment. Bond markets see pure coordination (rope) — others' budget constraints are their gain. Public investment advocates see extraction (snare) — the constraint locks in austerity regardless of crisis. Future generations see snare — they inherit a constitution that constrains responses to crises they did not create. The countercyclical agents and public investment coalition see tangled rope — genuine coordination benefits (fiscal credibility, long-term stability) mixed with real extraction costs (crisis response capacity suppressed). The constitutional court sees piton — their enforcement role is largely mechanical and performative. The analytical observer at civilizational scale risks seeing a natural law (mountain) — the debt brake appears to encode inevitable fiscal constraints — but the structural data (identifiable beneficiaries, deliberate constitutional design, contingent political choice) reveals this as a false summit. The gap between fiscal conservative/creditor perspectives (rope) and public investment/countercyclical perspectives (snare/tangled rope) is not merely empirical disagreement — it reflects fundamentally different structural relationships to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality analysis shows why different agents experience the constraint so differently. Future taxpayers benefit from fiscal discipline (d ≈ 0.15) and experience low effective extraction. Fiscal conservatives benefit from policy lock-in (d ≈ 0.10) and experience negative chi (the constraint subsidizes their policy preference). Bond markets and creditors are pure beneficiaries (d ≈ 0.05). But countercyclical agents face a different structure: they are victims of the constraint (d ≈ 0.75), experience high f(d), and see extraction scaling upward with crisis severity. Public investment coalition members are also victims (d ≈ 0.70) but have organized power (power=organized reduces experienced extraction somewhat relative to powerless agents). The supermajority requirement for amendment creates an 'authority ceiling' on exit cost — changing the constraint requires not just mobilization but 2/3 legislative supermajority, effectively creating a victim-group coordination problem with very high friction. This explains why the constraint persists even as its cost becomes apparent: the victims face structural barriers to amendment that lock in the extraction mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escape_clause_effectiveness,
    'Does the supermajority requirement for escape clauses (Article 115 para. 2) constitute genuine emergency flexibility or performative exception theater?',
    'Historical record of escape clause invocations (2008 financial crisis, COVID-19 pandemic); analysis of supermajority coalition costs vs. actual fiscal response capacity; cross-national comparison with other sovereign debt limits with different escape mechanisms',
    'If escape clauses are used as designed: constraint is genuine tangled rope (coordination + constrained extraction). If escape clauses are invoked performatively or repeatedly stalled: constraint becomes snare (locked-in extraction with cosmetic flexibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_clause_effectiveness, empirical, 'Whether escape clauses provide substantive or performative emergency flexibility').

omega_variable(
    countercyclical_necessity_empirics,
    'Is countercyclical fiscal spending (deficit spending during recessions) empirically necessary for labor market stabilization and growth recovery, or can monetary policy + automatic stabilizers substitute?',
    'Econometric analysis of recession dynamics in countries with strict vs. flexible fiscal rules; empirical assessment of automatic stabilizer sufficiency; labor market recovery trajectories post-2009',
    'If empirically necessary: debt brake extracts real economic capacity (snare tendencies higher). If substitutable: debt brake coordinates toward long-term fiscal health without severe extraction cost (rope tendencies higher).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countercyclical_necessity_empirics, empirical, 'Whether countercyclical fiscal spending is empirically necessary for crisis response').

omega_variable(
    intergenerational_extraction_direction,
    'Does the debt brake extract FROM future generations (by preventing investment in their productive capacity) or extract FROM present generations (by preventing consumption smoothing during crisis) or is the extraction direction ambiguous across different crisis scenarios?',
    'Long-run computable general equilibrium modeling of fiscal paths under debt brake vs. without; measurement of foregone investment in human capital, infrastructure, climate adaptation; correlation analysis of debt brake adoption timing and subsequent growth trajectories',
    'If extraction flows clearly toward future: snare from future-generation perspective is the true reading (they bear cost without consent). If extraction is reciprocal or ambiguous: tangled_rope classification (both generations bear costs and receive coordination benefits) is more precise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_direction, empirical, 'Direction and magnitude of intergenerational extraction').

omega_variable(
    amendment_kernel_contest,
    'Is the debt brake reading of constitutional amendment an instance of the broader ''amendment_history'' kernel, and if so, what structural relationship does it bear to competing readings of German constitutional change (asylum_compromise_1993, emergency_acts_1968, rearmament_1956, reunification_amendments_1990)?',
    'Doctrinal analysis of amendment legitimacy claims across all five readings; comparison of authority-grounding structures (How does each reading claim legitimacy for rewriting the Basic Law?); assessment of whether the readings foreclose, coexist, or influence one another',
    'If readings genuinely coexist: each amendment is a legitimate (if contested) exercise of Article 79 para. 1 authority. If debt_brake_2009 forecloses certain other readings: the amendment contest is zero-sum, not pluralistic. If readings influence without foreclosing: constitutional change is path-dependent and structured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_kernel_contest, conceptual, 'The debt brake as one reading of a contested constitutional amendment kernel').

omega_variable(
    false_summit_fiscal_inevitability,
    'Does the mountain classification at civilizational scale represent genuine structural inevitability (fiscal constraints are laws of nature) or naturalization of a contingent institutional choice?',
    'Comparative institutional analysis: do nations without constitutional debt limits experience inevitable fiscal catastrophe, or do different institutional designs (inflation tolerance, different escape clause designs, central bank coordination, fiscal unions) produce different equilibria? Historical counterfactual: would post-2009 Germany have faced default absent the debt brake?',
    'If inevitability is genuine: mountain classification is correct. If contingent: false summit signature detects naturalization of a political choice as law of nature. The presence of identifiable beneficiaries (creditors, fiscal conservatives) supports false summit hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_fiscal_inevitability, conceptual, 'Whether fiscal limits are natural law or naturalized institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_history__debt_brake_2009, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debtbrake_theater_2009, amendment_history__debt_brake_2009, theater_ratio, 0, 0.25).
narrative_ontology:measurement(debtbrake_theater_2014, amendment_history__debt_brake_2009, theater_ratio, 5, 0.28).
narrative_ontology:measurement(debtbrake_theater_2019, amendment_history__debt_brake_2009, theater_ratio, 10, 0.35).
narrative_ontology:measurement(debtbrake_theater_2024, amendment_history__debt_brake_2009, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(debtbrake_extractiveness_2009, amendment_history__debt_brake_2009, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(debtbrake_extractiveness_2014, amendment_history__debt_brake_2009, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(debtbrake_extractiveness_2019, amendment_history__debt_brake_2009, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(debtbrake_extractiveness_2024, amendment_history__debt_brake_2009, base_extractiveness, 15, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(debtbrake_suppression_2009, amendment_history__debt_brake_2009, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(debtbrake_suppression_2014, amendment_history__debt_brake_2009, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(debtbrake_suppression_2019, amendment_history__debt_brake_2009, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(debtbrake_suppression_2024, amendment_history__debt_brake_2009, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_history__debt_brake_2009, resource_allocation).
narrative_ontology:boltzmann_floor_override(amendment_history__debt_brake_2009, 0.18).
narrative_ontology:affects_constraint(amendment_history__debt_brake_2009, amendment_history__asylum_compromise_1993).
narrative_ontology:affects_constraint(amendment_history__debt_brake_2009, amendment_history__emergency_acts_1968).
narrative_ontology:affects_constraint(amendment_history__debt_brake_2009, amendment_history__rearmament_1956).
narrative_ontology:affects_constraint(amendment_history__debt_brake_2009, amendment_history__reunification_amendments_1990).

% DUAL FORMULATION NOTE:
% The debt brake is one reading of the amendment_history kernel. It is linked to all sibling readings as part of the same contested constitutional history. The constraint family consists of five structurally distinct constraints, each representing a different constitutional amendment with different beneficiary/victim structures and different ε values. The debt brake exhibits tangled rope structure (moderate extractiveness with coordination function); other readings may exhibit different types (asylum compromise likely tangled rope or snare; emergency acts likely mountain or scaffold; rearmament likely rope or tangled rope; reunification likely piton). Each story should be authored separately with its own ε, perspectives, and measurements. This story focuses exclusively on the debt brake reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_history__debt_brake_2009, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
