% ============================================================================
% CONSTRAINT STORY: debt_brake_2009__fiscal_credibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_brake_2009__fiscal_credibility_reading, []).

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
 *   constraint_id: debt_brake_2009__fiscal_credibility_reading
 *   human_readable: Debt Brake as Fiscal Credibility Commitment (2009 German Constitutional Reading)
 *   domain: constitutional_law/fiscal_policy
 *
 * SUMMARY:
 *   The German debt brake (Schuldenbremse), adopted in the 2009
 *   constitutional amendment (Article 115 of the Basic Law), is a fiscal
 *   commitment device framed as a solution to structural deficit bias. This
 *   reading treats the debt brake as credibility made law: a constitutional
 *   structure that binds future legislatures and reassures bond markets. The
 *   constraint redistributes political autonomy across time: present
 *   legislatures surrender discretion over structural deficits to lock in
 *   fiscal credibility for long-run solvency. This story instantiates ONE of
 *   three contested readings of the kernel (the 2009 constitutional text and
 *   its interpretation chain). The sibling readings interpret the same text
 *   as (a) a rule that survives through its exceptions
 *   (exception_pressure_reading) and (b) a constraint that starves future
 *   infrastructure investment in the name of fiscal protection
 *   (investment_starvation_reading). Each reading produces different ε
 *   values, different beneficiary/victim sets, and different classifications.
 *   This constraint story generates ONLY the fiscal_credibility_reading — the
 *   interpretation that emphasizes credibility, market trust, and long-run
 *   solvency as the genuine coordination function.
 *
 * KEY AGENTS:
 *   - Present-Biased Legislature (powerless/trapped): Loses discretionary deficit capacity. Experiences the constraint as suppression of structural legislative tendencies. Victim in this reading.
 *   - Bond Market and Sovereign Creditors (institutional/arbitrage): Beneficiary. Receives lower interest rates and reduced default risk premium. Coordinates on German fiscal stability.
 *   - Long-Run Solvency / Intergenerational Fiscal Stability (powerless/analytical): Primary beneficiary group (abstracted). Benefits from debt sustainability and lower future tax burdens.
 *   - Future-Oriented Fiscal Constituency (moderate/constrained): Mixed experience. Gains fiscal stability but bears costs of reduced public investment. Victim set member.
 *   - Countercyclical Spending Capacity (powerless/trapped): Victim in this reading. Constraint suppresses spending flexibility during recessions.
 *   - Emergency Clause Coalition (organized/constrained): Organized actors (executive, disaster agencies) experience the constraint as scaffold — uses exceptions as relief valves.
 *   - Eurozone Stability Framework (powerful/constrained): Powerful actor benefiting from German credibility anchor but constrained by German investment decline's eurozone effects.
 *   - Formal Budget Ritual (institutional/arbitrage): Institutional mechanism maintaining the constraint through transparent accounting. Experiences degradation (piton) as off-balance-sheet accounting increases.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_brake_2009__fiscal_credibility_reading, 0.35).
domain_priors:suppression_score(debt_brake_2009__fiscal_credibility_reading, 0.62).
domain_priors:theater_ratio(debt_brake_2009__fiscal_credibility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_brake_2009__fiscal_credibility_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(debt_brake_2009__fiscal_credibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(debt_brake_2009__fiscal_credibility_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_brake_2009__fiscal_credibility_reading, tangled_rope).
narrative_ontology:human_readable(debt_brake_2009__fiscal_credibility_reading, "Debt Brake as Fiscal Credibility Commitment (2009 German Constitutional Reading)").
narrative_ontology:topic_domain(debt_brake_2009__fiscal_credibility_reading, "constitutional_law/fiscal_policy").

domain_priors:requires_active_enforcement(debt_brake_2009__fiscal_credibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(debt_brake_2009__fiscal_credibility_reading, 'd927a791-5a85-4e55-8898-bb8b916a7e20').
narrative_ontology:cs_kernel_codification('d927a791-5a85-4e55-8898-bb8b916a7e20', formalized).
narrative_ontology:cs_authority_grounding('d927a791-5a85-4e55-8898-bb8b916a7e20', lineage).
narrative_ontology:cs_interpretation_layer_present('d927a791-5a85-4e55-8898-bb8b916a7e20').
narrative_ontology:cs_reading_relation('d927a791-5a85-4e55-8898-bb8b916a7e20', debt_brake_2009__exception_pressure_reading, coexists_with).
narrative_ontology:cs_reading_relation('d927a791-5a85-4e55-8898-bb8b916a7e20', debt_brake_2009__investment_starvation_reading, influences).
narrative_ontology:cs_axiom('d927a791-5a85-4e55-8898-bb8b916a7e20', foundational, credibility_solves_deficit_bias).
narrative_ontology:cs_axiom_status(credibility_solves_deficit_bias, holdable).
narrative_ontology:cs_axiom_grounding('d927a791-5a85-4e55-8898-bb8b916a7e20', credibility_solves_deficit_bias, empirically_contingent).
narrative_ontology:cs_axiom('d927a791-5a85-4e55-8898-bb8b916a7e20', foundational, extraction_acceptable_for_stability).
narrative_ontology:cs_axiom_status(extraction_acceptable_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('d927a791-5a85-4e55-8898-bb8b916a7e20', extraction_acceptable_for_stability, instrumental).
narrative_ontology:cs_reference_frame('d927a791-5a85-4e55-8898-bb8b916a7e20', discretionary_deficit_as_default).
narrative_ontology:cs_drift_state('d927a791-5a85-4e55-8898-bb8b916a7e20', contemporary_accounting_drift, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d927a791-5a85-4e55-8898-bb8b916a7e20', '').
narrative_ontology:cs_kernel_id(debt_brake_2009__fiscal_credibility_reading, debt_brake_2009).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_brake_2009__fiscal_credibility_reading, long_run_solvency).
narrative_ontology:constraint_beneficiary(debt_brake_2009__fiscal_credibility_reading, bond_market_trust).
narrative_ontology:constraint_beneficiary(debt_brake_2009__fiscal_credibility_reading, intergenerational_fiscal_stability).
narrative_ontology:constraint_victim(debt_brake_2009__fiscal_credibility_reading, present_biased_budgeting).
narrative_ontology:constraint_victim(debt_brake_2009__fiscal_credibility_reading, countercyclical_spending_capacity).
narrative_ontology:constraint_victim(debt_brake_2009__fiscal_credibility_reading, future_public_investment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT-BIASED LEGISLATURE (SNARE) — Trapped by the constitutional rule; cannot exercise the structural deficit bias that legislatures carry (the tendency to defer costs, front-load benefits). The constraint explicitly suppresses the legislative strategy that would otherwise dominate. Zero exit: the rule is entrenched in the Basic Law and requires supermajority amendment. Maximum extraction from the present generation's policy autonomy.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOND MARKET AND LONG-RUN SOLVENCY (ROPE) — Benefits from the credibility the debt brake provides. Lower interest rates on German sovereign debt, reduced default risk premium, and stable fiscal expectations across generations. Experiences the constraint as pure coordination: it solves the collective action problem of runaway deficits. Arbitrage exit: if the market doubted the commitment, arbitrage actors would withdraw; the constraint's survival depends on market confidence, not on enforcement.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FUTURE-ORIENTED FISCAL CONSTITUENCY (TANGLED ROPE) — Constrained by both benefits and costs. Gains from fiscal stability and lower future tax burdens (genuine coordination benefit). But also bears costs: public investment capacity is reduced, countercyclical spending is limited, intergenerational borrowing for infrastructure is suppressed. High barriers to exit — would require constitutional amendment. Mixed extraction and coordination.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGENCY CLAUSE COALITION (SCAFFOLD) — Organized actors (executive branch, disaster-response agencies) see the debt brake as a temporary rule with built-in sunset logic: Article 115 (2) of the Basic Law permits suspension for national disasters and emergency situations (pandemic, war, systemic financial crisis). These exceptions provide outlet valves that preserve the core rule by releasing pressure when it would fail. Low effective extraction because exit exists through the formal exception mechanism.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EUROZONE STABILITY FRAMEWORK (TANGLED ROPE) — Powerful institutional actor (ECB, eurozone governance). Benefits from German fiscal credibility anchoring eurozone stability. But constrained by the debt brake's spillovers: German public investment decline affects eurozone growth dynamics; reduced demand pressures eurozone monetary transmission. Constrained exit: cannot easily exit without reframing German role. Mixed experience of coordination (German credibility supports euro) and extraction (growth opportunity costs).
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: FORMAL BUDGET RITUAL (PITON) — Theater ratio 0.38 reflects that the debt brake operates through straightforward accounting: actual deficits are measured against a constitutionally specified threshold. Unlike complex regulatory theater (health-and-safety compliance, environmental impact assessment), the debt brake's enforcement is transparent and mechanistic. The ritual has low performative content because the rule is explicit and verifiable. The piton classification emerges from the constraint's degradation over time: special budgets (Extrahaushalte), off-balance-sheet entities, and creative accounting have eroded functional enforcement while preserving formal compliance.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN, NATURAL LAW VIEW) — From a civilizational/universal vantage, the fiscal constraint mirrors a natural law: every government carries deficit bias; the debt brake is not a policy choice but a structural necessity for long-run solvency. This perspective risks naturalizing what is actually a contingent constitutional choice. The false summit detector will flag this as beneficiary-driven naturalization: the long-run solvency constituency benefits from treating the debt brake as inevitable rather than contingent.
constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_brake_2009__fiscal_credibility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_brake_2009__fiscal_credibility_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_brake_2009__fiscal_credibility_reading, TR),
    TR >= 0.70.

:- end_tests(debt_brake_2009__fiscal_credibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The debt brake extracts policy autonomy from the present legislature and reallocates it to future solvency. This is genuine extraction from the present-biased agent, but bounded: the constraint does not extract resources (money), only spending authority. The 0.35 value reflects moderate asymmetry — the present legislature bears a real cost (reduced fiscal space), and future solvency receives a genuine benefit (lower default risk), but the extraction is not maximal because both agents theoretically benefit from fiscal stability in the long run. The value has drifted upward from 0.22 (2009) to 0.38 (2024) as off-balance-sheet mechanisms (Extrahaushalte, KfW-driven borrowing) have reduced the constraint's formal reach while its political bite has sharpened. Suppression (0.62): Moderate-high. The debt brake suppresses the structural deficit bias that every legislature carries — the tendency to defer costs and front-load benefits. The suppression operates through constitutional entrenchment (supermajority amendment required to change the rule). But suppression is not total: exceptions exist (emergency clauses), accounting loopholes have expanded, and political pressure to modify the rule periodically builds. Theater ratio (0.38): Low-moderate. The debt brake's enforcement is relatively transparent — the rule is explicit, the measured deficit is published, and compliance is mechanically verifiable. Unlike complex regulatory theater (environmental impact statements, health-and-safety certification), the debt brake's functional content aligns reasonably well with its formal content. Theater has drifted upward (0.28 in 2009 → 0.43 in 2024) as off-balance-sheet mechanisms have proliferated, requiring audiences to interpret beyond the published budget deficit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits substantial perspectival divergence across the six types. The present-biased legislature classifies as Snare (maximum extraction of spending autonomy). The bond market classifies as Rope (pure coordination of fiscal expectations). The future-oriented constituency classifies as Tangled Rope (mixed benefit and cost). The emergency coalition sees Scaffold (temporary rule with exit through exceptions). The eurozone framework sees Tangled Rope (coordination benefit for German credibility, extraction in the form of growth drag). The formal budget ritual sees Piton (increasingly performative as accounting workarounds proliferate). The civilizational analytical observer risks seeing Mountain (natural law of fiscal sustainability), a false summit. The gap between Snare (present legislature), Rope (bond market), and the multiple Tangled Rope perspectives reveals the constraint's true structure: it is a coordination mechanism for credibility that operates by extracting from present policy autonomy. The analytical observer's mountain classification is diagnostic: it reveals that the fiscal credibility reading naturalizes what is actually a contingent constitutional choice. Treating fiscal limits as laws of nature ('a government must live within its means') obscures the distributional choice implicit in the debt brake (present restraint for future stability).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's structural data produces characteristic directionality values. Beneficiaries (bond market, long-run solvency) occupy institutional power positions with arbitrage exit options. Victims (present legislatures, countercyclical spending capacity) occupy powerless positions with trapped exit options. The derivative directionality is sharp: beneficiary d ≈ 0.15 (institutional + arbitrage) vs. victim d ≈ 0.95 (powerless + trapped). This sharp gradient is why the analytical observer risks the mountain classification — high beneficiary-victim asymmetry is sometimes misread as a 'natural' law rather than a constructed extraction. The emergency coalition (organized + constrained) derives d ≈ 0.55, producing the moderate experienced extractiveness characteristic of Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by declaring the fiscal credibility reading as ONE coherent reading of the kernel. The constraint is tangled_rope (not snare, not pure rope) because: (1) it has genuine coordination function (credibility for bond markets, fiscal stability anchoring eurozone); (2) it has asymmetric extraction (present legislatures lose authority; future solvency gains security); (3) it requires active enforcement (constitutional supermajority to modify, regular compliance monitoring). The false summit detection (mountain classification at analytical level) reveals that framing the debt brake as a natural law obscures its distributional intent. The sibling readings (exception_pressure, investment_starvation) would produce different ε values and different classification sequences. The mandatrophy is not resolved by picking one type as 'correct' but by recognizing that all six types are legitimate perspectival readings of the same constraint from different agent positions and time horizons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_deficit_bias_reality,
    'Is the structural deficit bias that the debt brake suppresses a universal property of democratic legislatures or a contingent feature of political economy contexts?',
    'Comparative constitutional analysis: survey legislatures in comparable democracies (France, UK, Canada, Japan) for evidence of systematic deficit bias absent formal constitutional constraints. Examine whether deficit drift correlates with institutions (multiparty systems, proportional representation, short electoral cycles) or is universal across institutional types.',
    'If universal: the debt brake solves a genuine coordination problem (rope classification remains primary). If contingent: the constraint may be manufacturing the very deficit bias it claims to suppress (snare reclassification becomes more likely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_deficit_bias_reality, empirical, 'Whether structural deficit bias is universal or contingent').

omega_variable(
    exception_clause_effectiveness,
    'Do the emergency exceptions (Article 115(2)) actually function as intended, or have they become normalization mechanisms that undermine the core constraint?',
    'Audit of exception invocations: frequency, scope, and justification for declared emergencies (COVID-19, financial crisis, military mobilization). Measure actual deficit consequences during exception periods vs. non-exception periods. Track whether exceptions are time-limited or effectively permanent.',
    'If exceptions are genuinely temporary and used sparingly: the scaffold classification is accurate and the constraint retains structure. If exceptions become permanent or expand in scope: the constraint degrades toward piton (performative) or erodes functionally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exception_clause_effectiveness, empirical, 'Whether emergency exceptions preserve or erode the debt brake').

omega_variable(
    off_balance_sheet_accounting_capture,
    'To what extent have off-balance-sheet budgets (Extrahaushalte), development banks, and financial vehicles displaced measured deficits without reducing actual fiscal stimulus?',
    'Financial flow analysis: consolidate all public-sector borrowing (federal budget, special funds, national development bank KfW, railroad debt DBAG, health insurance funds) and compare consolidated deficit to reported budget deficit. Measure functional deficit (actual government spending that expands money supply) vs. formal deficit (Article 115 measured deficit).',
    'If off-balance-sheet capture is substantial (>30% of functional deficit): the constraint is largely theatrical and piton classification becomes appropriate. If marginal (<10%): enforcement remains functional and tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(off_balance_sheet_accounting_capture, empirical, 'Degree of off-balance-sheet accounting displacement of measured deficits').

omega_variable(
    future_investment_decapitalization,
    'Has the debt brake contributed to measurable decline in public capital stock (roads, rail, schools, digital infrastructure) relative to other comparable democracies?',
    'Infrastructure capital stock measurement: asset accounting for public capital (buildings, infrastructure, equipment) as % of GDP, comparing Germany to France, UK, Canada across 2009-2025. Control for business-cycle variation. Examine maintenance backlogs and replacement rates.',
    'If public capital stock declined significantly relative to peers: the extraction from future-oriented agents (victims in this reading) is substantial and concrete. If decapitalization is not significant: the constraint''s costs are distributional rather than aggregate productive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_investment_decapitalization, empirical, 'Public capital stock trends since 2009 debt brake adoption').

omega_variable(
    reading_boundary_ambiguity,
    'Is the fiscal credibility reading coherent, or does it conflate credibility (market trust) with solvency (ability to service debt)? If the debt brake''s credibility is purchased at the cost of solvency (by preventing investment), has the trade-off been analyzed?',
    'Decompose credibility into: (a) market trust (measured by bond spreads, CDS pricing, rating agencies); (b) solvency (measured by long-term debt-to-GDP sustainability under alternative growth scenarios); (c) productive capacity (public capital adequacy for future growth). Determine if low-debt-now + decapitalization produces long-run solvency or undermines it.',
    'If credibility is genuine but purchased by solvency trade-off: this reading''s beneficiary (''long_run_solvency'') may be harmed by the constraint. The reading may be incoherent — what appears as solvency protection may be strategic default prevention for present debt, at the cost of future default risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether credibility and solvency align or trade off under the debt brake').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_brake_2009__fiscal_credibility_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dbfcr_theater_t0_2009, debt_brake_2009__fiscal_credibility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dbfcr_theater_t5_2014, debt_brake_2009__fiscal_credibility_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(dbfcr_theater_t10_2019, debt_brake_2009__fiscal_credibility_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(dbfcr_theater_t15_2024, debt_brake_2009__fiscal_credibility_reading, theater_ratio, 15, 0.43).

% Extraction over time
narrative_ontology:measurement(dbfcr_extractiveness_t0_2009, debt_brake_2009__fiscal_credibility_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dbfcr_extractiveness_t5_2014, debt_brake_2009__fiscal_credibility_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(dbfcr_extractiveness_t10_2019, debt_brake_2009__fiscal_credibility_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(dbfcr_extractiveness_t15_2024, debt_brake_2009__fiscal_credibility_reading, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dbfcr_suppression_t0_2009, debt_brake_2009__fiscal_credibility_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(dbfcr_suppression_t10_2019, debt_brake_2009__fiscal_credibility_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_brake_2009__fiscal_credibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(debt_brake_2009__fiscal_credibility_reading, debt_brake_2009__exception_pressure_reading).
narrative_ontology:affects_constraint(debt_brake_2009__fiscal_credibility_reading, debt_brake_2009__investment_starvation_reading).
narrative_ontology:affects_constraint(debt_brake_2009__fiscal_credibility_reading, eurozone_fiscal_governance_framework).
narrative_ontology:affects_constraint(debt_brake_2009__fiscal_credibility_reading, german_sovereign_credit_rating).

% DUAL FORMULATION NOTE:
% The debt brake 2009 kernel admits three structurally distinct readings with different ε values and beneficiary/victim sets. This constraint (fiscal_credibility_reading, ε=0.35, Tangled Rope) decomposes from the sibling readings: exception_pressure_reading (ε ≈ 0.25–0.30, Scaffold) and investment_starvation_reading (ε ≈ 0.50–0.60, Snare). Each story must generate independently. Network edges establish kinship: all three are readings of the same kernel, and the fiscal_credibility_reading influences both siblings through its characterization of the constraint's core function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
