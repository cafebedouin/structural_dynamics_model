% ============================================================================
% CONSTRAINT STORY: debt_brake_2009__investment_starvation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_brake_2009__investment_starvation_reading, []).

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
 *   constraint_id: debt_brake_2009__investment_starvation_reading
 *   human_readable: Debt Brake Investment Starvation (2009 Constitutional Rule)
 *   domain: legal/doctrinal/fiscal_constitutional
 *
 * SUMMARY:
 *   The debt brake (Schuldenbremse, introduced in Germany's 2009
 *   constitutional framework) is a formal rule that caps borrowing
 *   independent of what the borrowing finances. From the investment
 *   starvation reading, this rule suppresses debt-financed capital formation
 *   (roads, grids, schools) while permitting consumption-financed debt to
 *   accumulate unchecked through off-budget mechanisms. The rule creates a
 *   genuine coordination benefit — it prevents legislatures from running
 *   permanent deficits — but does so by instrumenting a categorical error:
 *   treating all borrowing equivalently regardless of whether it creates
 *   productive assets or consumptive flows. The result is that the rule's
 *   beneficiaries (deficit hawks, creditor interests, accounting orthodoxy)
 *   benefit from fiscal discipline while its victims (infrastructure systems,
 *   long-term capital formation, future generations) bear extraction through
 *   deferred maintenance. The constraint is contested: different actors and
 *   readings of the debt brake kernel emphasize different structural
 *   functions. This story instantiates the investment starvation reading: the
 *   rule's core function is asset suppression masked as fiscal
 *   responsibility.
 *
 * KEY AGENTS:
 *   - Accounting Rule Orthodoxy: Primary beneficiary (institutional/constrained) — the rule's legitimacy rests on the accounting frame (borrowing=cost, independent of asset creation); the orthodoxy controls the narrative
 *   - Deficit Hawk Coalition: Primary beneficiary (institutional/arbitrage) — benefits from commitment device against deficit spending; can arbitrage between jurisdictions with/without fiscal discipline
 *   - Long-Term Infrastructure Commons: Primary victim (powerless/trapped) — roads, grids, schools, rail systems cannot organize or exit; bear extraction through deferred maintenance compounding over generational time
 *   - Infrastructure Operators (municipal/regional authorities): Secondary victim (organized/constrained) — operate within debt brake constraints; perceive both coordination and extraction; can petition for exceptions but face barriers
 *   - National Finance Ministry: Institutional actor (institutional/constrained) — maintains and enforces the rule; benefits from political authority it provides; increasingly constrained by its rigidity; no arbitrage exit without constitutional amendment
 *   - Future Generations: Tertiary victim (analytical/analytical) — intergenerational extraction through deferred maintenance compounding; absent from political representation; highest long-term extraction cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_brake_2009__investment_starvation_reading, 0.58).
domain_priors:suppression_score(debt_brake_2009__investment_starvation_reading, 0.65).
domain_priors:theater_ratio(debt_brake_2009__investment_starvation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_brake_2009__investment_starvation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(debt_brake_2009__investment_starvation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(debt_brake_2009__investment_starvation_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_brake_2009__investment_starvation_reading, tangled_rope).
narrative_ontology:human_readable(debt_brake_2009__investment_starvation_reading, "Debt Brake Investment Starvation (2009 Constitutional Rule)").
narrative_ontology:topic_domain(debt_brake_2009__investment_starvation_reading, "legal/doctrinal/fiscal_constitutional").

domain_priors:requires_active_enforcement(debt_brake_2009__investment_starvation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(debt_brake_2009__investment_starvation_reading, '48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8').
narrative_ontology:cs_kernel_codification('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', formalized).
narrative_ontology:cs_authority_grounding('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', lineage).
narrative_ontology:cs_interpretation_layer_present('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8').
narrative_ontology:cs_reading_relation('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', debt_brake_2009__exception_pressure_reading, coexists_with).
narrative_ontology:cs_reading_relation('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', debt_brake_2009__fiscal_credibility_reading, influences).
narrative_ontology:cs_axiom('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', foundational, debt_productive_consumptive_distinction_is_operationalizable).
narrative_ontology:cs_axiom_status(debt_productive_consumptive_distinction_is_operationalizable, holdable).
narrative_ontology:cs_axiom_grounding('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', debt_productive_consumptive_distinction_is_operationalizable, empirically_contingent).
narrative_ontology:cs_axiom('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', foundational, categorical_equivalence_of_debt_extracts_from_capital_formation).
narrative_ontology:cs_axiom_status(categorical_equivalence_of_debt_extracts_from_capital_formation, holdable).
narrative_ontology:cs_axiom_grounding('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', categorical_equivalence_of_debt_extracts_from_capital_formation, deontological).
narrative_ontology:cs_reference_frame('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', productive_debt_financing_as_legitimate_fiscal_function).
narrative_ontology:cs_drift_state('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', contemporary_post_pandemic_inflation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('48a5a9e6-f88e-4cc9-b338-e8e53a22c6f8', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(debt_brake_2009__investment_starvation_reading, debt_brake_2009).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_brake_2009__investment_starvation_reading, accounting_rule_orthodoxy).
narrative_ontology:constraint_beneficiary(debt_brake_2009__investment_starvation_reading, deficit_hawk_coalition).
narrative_ontology:constraint_victim(debt_brake_2009__investment_starvation_reading, long_term_infrastructure).
narrative_ontology:constraint_victim(debt_brake_2009__investment_starvation_reading, public_capital_formation).
narrative_ontology:constraint_victim(debt_brake_2009__investment_starvation_reading, intergenerational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFERRED INFRASTRUCTURE (SNARE) — Roads, electrical grids, water systems, and schools cannot exit the debt brake's constraint. These collective goods are structurally unable to organize politically and cannot refinance their own maintenance. The decaying asset base is the extraction vector: maintenance is suppressed off-books; decay accelerates; replacement costs compound. The powerless agent bears full extraction with zero exit options.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFRASTRUCTURE OPERATORS (TANGLED ROPE) — Municipal and regional authorities that own grids, rails, and schools operate within the debt brake's constraints. They perceive genuine coordination (the fiscal rule prevents race-to-the-bottom debt spirals) but also asymmetric extraction (they bear the cost of deferred maintenance while the rule benefits from their compliance). Constrained exit: they can petition for exceptions or reclassifications, but face political and legal barriers. The gap between experienced benefit (coordination of fiscal discipline) and experienced cost (infrastructure decay) marks tangled rope.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONAL FINANCE MINISTRY (TANGLED ROPE) — The ministry maintains the rule and extracts political authority from its enforcement (demonstrates fiscal rectitude). But the ministry also coordinates genuine policy discipline — prevents runaway debt accumulation. However, the ministry is increasingly constrained by the rule's rigidity: unable to finance productive investments or countercyclical spending without political crisis. Constrained exit: can propose constitutional amendment but faces strong deficit-hawk resistance. The ministry benefits from the rule's coordination function but is asymmetrically bound by it.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFICIT HAWKS (ROPE) — Conservative fiscal actors (some economists, bondholders, structural fund ideologues) perceive the debt brake as pure coordination: a commitment device against the deficit bias inherent to democratic legislatures. For this perspective, the rule solves a collective action problem by removing the temptation to overspend. They have arbitrage exit: can choose markets and jurisdictions that enforce fiscal discipline. The beneficiary perspective sees coordination without extraction.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTING ORTHODOXY (PITON) — The debt brake is grounded in a specific accounting frame: borrowing is a cost/liability, repayment is a burden, and all debt is equivalent regardless of what it finances. This orthodoxy has lost functional force — it prevents measurement of asset creation (schools built, grids modernized) and conflates consumptive debt with productive debt. The rule persists through institutional inertia and ideology rather than analytical force. Theater ratio is high for this perspective: the rule's legitimacy rests on theatrical simplification ('debt is debt') that obscures structural distinction between investment and consumption. Constrained exit: the orthodoxy can be revised but faces strong institutional resistance.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective spanning multiple readings of the debt brake kernel, this reading (investment starvation) reveals that the rule coordinates fiscal discipline while extracting from long-term capital formation. The analytical observer sees how the rule's legitimacy ('preventing deficit bias') masks its extractive function ('suppressing productive investment'). The rule creates genuine coordination benefit but does so asymmetrically, benefiting deficit hawks and creditor interests at the expense of infrastructure and growth. The classification is tangled rope: coordination + asymmetric extraction + active enforcement.
constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_brake_2009__investment_starvation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_brake_2009__investment_starvation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_brake_2009__investment_starvation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_brake_2009__investment_starvation_reading, TR),
    TR >= 0.70.

:- end_tests(debt_brake_2009__investment_starvation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The debt brake creates asymmetric extraction by suppressing investment financing while permitting consumption financing to operate through off-budget mechanisms (special funds, exceptional clauses). The initial extractiveness (0.32) reflects the rule's first decade when constitutional enforcement was still establishing; as the rule hardened and exceptions became more rigid, extractiveness rose to 0.58. The rule extracts from long-term capital formation while benefiting deficit hawks and the accounting orthodoxy that legitimizes it. Suppression (0.65): Moderate-high and rising. Infrastructure operators face high barriers to debt-financed investment: formal borrowing caps, political costs of exception requests, alternative financing at higher cost. The suppression mechanism is dual: formal legal constraint (the rule itself) and structural constraint (exceptions require supermajorities or emergency declarations). Theater ratio (0.35): Low and stable. Unlike the accounting orthodoxy's perspective (which sees the rule as legitimately simplifying a complex problem), this reading sees the rule's legitimacy as resting on false equivalence between productive and consumptive debt, but the theater is not high because the rule's mechanism is transparently written into constitutional text. The rule does what it says it does; the category error is the doctrinal artifact, not a performance layer.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between the deficit hawk coalition's rope classification and the infrastructure commons' snare classification. The deficit hawks perceive the rule as coordination (solving collective action problem of deficit bias) with low or no extraction. The infrastructure commons perceives pure extraction with no exit. The gap reveals the distribution of benefit and burden: those who benefit from fiscal discipline (creditors, conservative fiscal actors) classify the rule as coordination; those who bear the cost of deferred maintenance (infrastructure systems, communities dependent on public capital) classify it as extraction. The secondary gap exists between the institutional perspectives (ministry, deficit hawks, accounting orthodoxy) and the analytical observer: the analytical observer sees that all three institutional perspectives are partly captured by the rule's framing — they describe the rule as if it were a neutral coordination mechanism when it is actually distributing costs asymmetrically. The tertiary gap is intergenerational: the biographical perspectives (finance ministry, operators) do not register the generational extraction that becomes visible at the civilizational time horizon, where deferred maintenance compounds and future generations bear the full cost of present suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by structural position. Deficit hawks as beneficiaries with arbitrage options (can exit to other jurisdictions, alternative asset classes) have low d (~0.15); they experience the rule as beneficiary protection. Infrastructure operators as victims with constrained exit (must serve their regions; can petition for exceptions but face barriers) have moderate-high d (~0.70); they experience mixed cost and benefit. The powerless infrastructure commons as victims with trapped exit have maximum d (~0.95); they experience only extraction with zero exit options. The accounting orthodoxy as institutional beneficiary with constrained exit (can be revised but faces ideological resistance) has moderate d (~0.35). The national finance ministry, though institutional, has constrained exit (cannot unilaterally override constitutional rule) and is asymmetrically bound by the rule, producing higher d (~0.60) than the deficit hawks. The analytical observer, by definition, has neither beneficiary nor victim status; d is assigned via canonical fallback (~0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (0.58 < ε ≤ 0.70) for this constraint remains unresolved. The investment starvation reading resolves part of the mandatrophy by clarifying that the tangled rope classification derives from two distinct structural functions: (1) genuine coordination function (preventing deficit spiral — legitimate), and (2) asymmetric extraction function (suppressing investment financing — harmful). The rule simultaneously solves a coordination problem and extracts from capital formation. Resolving which function dominates would require empirical analysis of long-term economic outcomes: Does the fiscal discipline imposed by the debt brake produce net economic growth (suggesting coordination dominates) or does the suppression of productive investment produce net economic drag (suggesting extraction dominates)? The question is not which classification is 'correct' but which structural function has larger real-world consequences. The measurement trajectory (extractiveness rising from 0.32 to 0.58) suggests the extraction function is amplifying over time as the rule's exceptions ossify and deferred maintenance compounds. If extractiveness continues rising above 0.70, the constraint could reclassify toward snare (from the infrastructure commons' perspective) or even piton (if the rule's legitimacy erodes while enforcement persists through inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productive_vs_consumptive_debt_distinction,
    'Is the distinction between debt-financed investment and debt-financed consumption structurally clear enough to operationalize in fiscal rules, or does the debt brake''s category error (treating all debt equivalently) reflect a genuine epistemic limit?',
    'Historical analysis of sovereign debt outcomes: long-term economic growth correlates with debt composition (infrastructure-financed vs consumption-financed). Comparative analysis across jurisdictions with (debt brake) vs without (UK, US) explicit investment carve-outs. Measurement of implicit asset creation in GDP calculations vs explicit borrowing caps.',
    'If distinction is clear and operationalizable: the investment starvation reading stands — the rule is a category error with extractive consequences. If distinction is epistemically intractable: the debt brake''s simplification is unavoidable, and the extraction is a coordination cost rather than a design failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productive_vs_consumptive_debt_distinction, empirical, 'Whether productive vs consumptive debt distinction is operationalizable').

omega_variable(
    intergenerational_extraction_compounding,
    'Does deferred maintenance compound across generations in a way that produces exponential extraction cost, or does the cost remain linear (deferred maintenance is simply postponed, not amplified)?',
    'Longitudinal cost analysis: comparison of maintenance costs for delayed vs timely intervention (e.g., road surface vs structural failure, early grid modernization vs systemic blackout response). Tracking of decay trajectories across multiple infrastructure systems under debt brake constraints vs comparative jurisdictions without constraints.',
    'If exponential: the constraint''s extraction function is severe and accelerates over time (snare reclassification for long time horizons). If linear: extraction is capped and the rule''s harm is bounded (tangled rope holds across time). Compounding changes the prospective victim set from ''next generation'' to ''all future generations.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_compounding, empirical, 'Compounding rate of deferred maintenance costs').

omega_variable(
    kernel_reading_identity,
    'Is the investment starvation reading a distinct structural interpretation of the debt brake kernel, or is it merely a negative reframing of the fiscal credibility reading?',
    'Examination of whether the two readings differ in their account of what the kernel IS (its core function), not merely in their valuation of that function. The fiscal credibility reading holds the kernel IS a commitment device against deficit bias. Does the investment starvation reading hold a different account of the kernel''s core function, or does it agree on the function and merely highlight distributional harm?',
    'If truly distinct readings: both are structurally coherent and coexist within the kernel contest. If merely reframing: the starvation reading is a critique of the credibility reading rather than an alternative reading (conceptual clarity rather than structural distinction). This affects whether reading_relations includes ''coexists_with'' or a different relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether investment starvation is a distinct kernel reading or critique of fiscal credibility reading').

omega_variable(
    exception_mechanism_capture,
    'Does the exception pressure reading (debt brake survives via exceptions: emergency clauses, special funds) describe a necessary valve that prevents snare reclassification, or does it describe how the extractive mechanism adapts?',
    'Historical analysis of exception usage: Do exceptions systematically relieve pressure on productive investment, or do they create new extractive mechanisms (special funds with higher interest costs, emergency declarations that require legislative majorities)? Tracking of which sectors and actors gain exception access.',
    'If exceptions relieve extraction: the debt brake remains tangled rope with an outlet. If exceptions concentrate power: exceptions become a secondary extraction mechanism (beneficiaries gain carve-outs; non-beneficiaries remain suppressed). This affects whether the investment starvation reading''s suppression level (0.65) understates the actual suppression by treating exceptions as legitimate relief valves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exception_mechanism_capture, empirical, 'Whether exceptions relieve extraction or concentrate power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_brake_2009__investment_starvation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(db_inv_starv_theater_t0, debt_brake_2009__investment_starvation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(db_inv_starv_theater_t5, debt_brake_2009__investment_starvation_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(db_inv_starv_theater_t10, debt_brake_2009__investment_starvation_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(db_inv_starv_extr_t0, debt_brake_2009__investment_starvation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(db_inv_starv_extr_t5, debt_brake_2009__investment_starvation_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(db_inv_starv_extr_t10, debt_brake_2009__investment_starvation_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(db_inv_starv_supp_t0, debt_brake_2009__investment_starvation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(db_inv_starv_supp_t5, debt_brake_2009__investment_starvation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(db_inv_starv_supp_t10, debt_brake_2009__investment_starvation_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_brake_2009__investment_starvation_reading, resource_allocation).
narrative_ontology:affects_constraint(debt_brake_2009__investment_starvation_reading, debt_brake_2009__exception_pressure_reading).
narrative_ontology:affects_constraint(debt_brake_2009__investment_starvation_reading, debt_brake_2009__fiscal_credibility_reading).

% DUAL FORMULATION NOTE:
% The debt brake kernel is instantiated by three distinct readings, each emphasizing different structural functions of the same constitutional rule. The investment starvation reading (this constraint) emphasizes the rule's suppression of productive debt-financed investment. The exception pressure reading emphasizes how the rule's exceptions function as structural relief valves. The fiscal credibility reading emphasizes the rule's commitment-device function against deficit bias. Each reading has its own ε, its own beneficiary/victim structure, and its own classification signature. The three readings are linked via network edges and share the same kernel_id but different reading_ids. The constraint family is complete with three members; no reading has been omitted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_brake_2009__investment_starvation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
