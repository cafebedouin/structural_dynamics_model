% ============================================================================
% CONSTRAINT STORY: debt_brake_2009__exception_pressure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_brake_2009__exception_pressure_reading, []).

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
 *   constraint_id: debt_brake_2009__exception_pressure_reading
 *   human_readable: Debt Brake Exception Pressure (Reads the 2009 German/European Debt Brake via Its Emergency Clauses)
 *   domain: legal/fiscal_constitutional
 *
 * SUMMARY:
 *   The German debt brake (constitutional rule limiting borrowing to 0.35% of
 *   GDP structural deficit for the federal government, near-zero for Länder)
 *   was adopted in 2009 and incorporated into EU fiscal rules. This
 *   constraint is ONE READING of that institutional kernel: the
 *   exception_pressure reading interprets the debt brake as a rule that
 *   survives by the holes punched in it. Emergency clauses for pandemic and
 *   war, special funds outside the main budget, European Stability Mechanism
 *   and other contingent vehicles — these are not violations of the rule but
 *   its functional mechanism. The rule constrains normal spending while
 *   allowing crises to be rerouted through side doors. This reading
 *   emphasizes that the constraint operates through the permissibility of
 *   exceptions, not through rigid adherence. The reading coexists with two
 *   siblings: the fiscal_credibility_reading (the debt brake is a commitment
 *   device that reduces borrowing costs) and the
 *   investment_starvation_reading (the brake prevents necessary capital
 *   investment and impoverishes the future). This story instantiates only the
 *   exception_pressure reading, with ε-invariant metrics and structural data
 *   specific to how this reading understands the constraint.
 *
 * KEY AGENTS:
 *   - Executive Authority (Federal Government): Institutional beneficiary (arbitrage exit) — benefits from both the rule's credibility and the exceptions' flexibility for crisis spending. Can move spending off-budget.
 *   - Parliament/Budget Committee: Organized actor (constrained exit) — has formal authority but faces real coordination constraints (discipline against bias) while also experiencing extraction (executive discretion over exceptions).
 *   - Fiscal Transparency Norm: Powerless victim (trapped) — the abstract commitment to transparent budgeting is systematically violated by exception clauses and special funds; cannot exit and cannot negotiate.
 *   - Structural Investment (Infrastructure/Education): Moderate victim (constrained exit) — prevented from rational long-term borrowing; benefits from rule's credibility but extraction dominates.
 *   - European Institutions: Powerful organizer (mobile exit) — sees debt brake as temporary scaffolding; creating alternative structures (green transformation funds, Next Generation EU) that provide exit routes.
 *   - Fiscal Credibility Advocates: Institutional defenders (arbitrage) — argue the rule delivers genuine benefit through lower borrowing costs; dispute the extraction narrative.
 *   - Analytical Observer: Civilizational perspective (analytical) — risks naturalizing the rule as immutable constraint on fiscal capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_brake_2009__exception_pressure_reading, 0.52).
domain_priors:suppression_score(debt_brake_2009__exception_pressure_reading, 0.58).
domain_priors:theater_ratio(debt_brake_2009__exception_pressure_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_brake_2009__exception_pressure_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(debt_brake_2009__exception_pressure_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(debt_brake_2009__exception_pressure_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_brake_2009__exception_pressure_reading, tangled_rope).
narrative_ontology:human_readable(debt_brake_2009__exception_pressure_reading, "Debt Brake Exception Pressure (Reads the 2009 German/European Debt Brake via Its Emergency Clauses)").
narrative_ontology:topic_domain(debt_brake_2009__exception_pressure_reading, "legal/fiscal_constitutional").

domain_priors:requires_active_enforcement(debt_brake_2009__exception_pressure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(debt_brake_2009__exception_pressure_reading, '4ab97217-4fa2-4a95-af42-94c07313afe6').
narrative_ontology:cs_kernel_codification('4ab97217-4fa2-4a95-af42-94c07313afe6', formalized).
narrative_ontology:cs_authority_grounding('4ab97217-4fa2-4a95-af42-94c07313afe6', extraction).
narrative_ontology:cs_interpretation_layer_present('4ab97217-4fa2-4a95-af42-94c07313afe6').
narrative_ontology:cs_reading_relation('4ab97217-4fa2-4a95-af42-94c07313afe6', debt_brake_2009__fiscal_credibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ab97217-4fa2-4a95-af42-94c07313afe6', debt_brake_2009__investment_starvation_reading, influences).
narrative_ontology:cs_axiom('4ab97217-4fa2-4a95-af42-94c07313afe6', foundational, rules_survive_through_exceptions).
narrative_ontology:cs_axiom_status(rules_survive_through_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('4ab97217-4fa2-4a95-af42-94c07313afe6', rules_survive_through_exceptions, empirically_contingent).
narrative_ontology:cs_axiom('4ab97217-4fa2-4a95-af42-94c07313afe6', secondary, transparency_loss_via_procedural_bypass).
narrative_ontology:cs_axiom_status(transparency_loss_via_procedural_bypass, holdable).
narrative_ontology:cs_axiom_grounding('4ab97217-4fa2-4a95-af42-94c07313afe6', transparency_loss_via_procedural_bypass, empirically_contingent).
narrative_ontology:cs_reference_frame('4ab97217-4fa2-4a95-af42-94c07313afe6', unified_transparent_budget).
narrative_ontology:cs_drift_state('4ab97217-4fa2-4a95-af42-94c07313afe6', contemporary_exception_proliferation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('4ab97217-4fa2-4a95-af42-94c07313afe6', '').
narrative_ontology:cs_kernel_id(debt_brake_2009__exception_pressure_reading, debt_brake_2009).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_brake_2009__exception_pressure_reading, crisis_spending_constituencies).
narrative_ontology:constraint_beneficiary(debt_brake_2009__exception_pressure_reading, executive_discretion_holders).
narrative_ontology:constraint_victim(debt_brake_2009__exception_pressure_reading, fiscal_transparency_norm).
narrative_ontology:constraint_victim(debt_brake_2009__exception_pressure_reading, structural_investment_capacity).
narrative_ontology:constraint_victim(debt_brake_2009__exception_pressure_reading, parliamentary_spending_discipline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCAL TRANSPARENCY (SNARE) — The formal rule (3% deficit limit, structural constraint on borrowing) cannot exit and cannot adapt; it is locked in constitutional form. But the rule is violated routinely through emergency clauses and special funds that exist beside the main budget. The trapped agent here is not a person but a normative commitment: transparency of what the state spends. Emergency clauses and special vehicles (Sondervermögen, financial stability funds) systematically hide spending from parliamentary oversight and public scrutiny. Maximum extraction: the rule's own integrity is the victim.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENT & FISCAL OVERSIGHT (TANGLED ROPE) — Parliament has formal authority over spending but faces genuine coordination constraints: pandemic requires rapid response; climate transition requires massive investment; both trigger emergency clauses that sidestep normal process. Parliament benefits from having a rule that constrains populist spending pressures (a credibility commitment). But Parliament also experiences extraction: executive authority expands during emergencies, the rule's rigidity forces off-budget structures, and the rule prevents rational countercyclical spending. Mixed: real coordination function (discipline against bias) coupled with asymmetric extraction (executive discretion over exceptions).
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE & CRISIS SPENDING (ROPE) — From the executive perspective, the debt brake is a coordination mechanism that solves a real problem: legislatures have deficit bias and tend to overspend in normal times. The rule constrains this. But the exception clauses are not seen as extraction by executives — they are seen as rational flexibility for genuine emergencies. The executive benefits from both the rule (credibility) and the exceptions (flexibility to respond). Net beneficiary: experiences the constraint as coordination because the exceptions exist and work.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INFRASTRUCTURE & FUTURE GENERATIONS (TANGLED ROPE) — Schools, railways, water systems, power grids suffer from underinvestment because borrowing for capital is constrained equally with current spending. Emergency clauses bypass the brake for pandemic response or bank stabilization but not for structural investment. This constituency is constrained but also partially benefits from the rule's credibility (lower interest rates on existing debt). But extraction is real: the rule prevents long-term borrowing for assets, creating a debt-avoidance bias that compounds across decades. The constraint coordinates fiscal discipline while extracting from future capacity.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EUROPEAN INSTITUTIONS (SCAFFOLD) — The debt brake is a temporary coordination device at the European level, a sunset mechanism disguised as permanent rule. It was adopted post-2008 to rebuild credibility after the fiscal crisis. From this perspective, the emergency clauses are sunset mechanisms themselves — emergency instruments that should be time-limited and then expire. The EU perspective sees this constraint as a scaffolding structure: holds up fiscal discipline in the short/medium term, but the architecture should evolve toward genuine fiscal federalism or national flexibility. Mobile because escape routes (ECB purchases, green transformation funds, Next Generation EU) are available and expanding.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ECONOMIC CONSTITUTIONALISM (PITON) — From the long view of constitutional doctrine, the debt brake is a degraded institution: it persists as a binding rule in the German constitution and EU law, but its actual functioning is increasingly theatrical. The exceptions have become the rule. Special funds proliferate. Off-budget vehicles evolve to comply with the letter while violating the spirit. The constraint survives through performative enforcement of procedures around exceptions rather than through substantive constraint on spending. Theater ratio 0.65 reflects that the rule's primary function is now legitimacy-signaling (we are fiscally disciplined) rather than actual limitation.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, some limits on state borrowing are inherent to any monetary system: debt cannot exceed feasible repayment capacity; default risk rises monotonically with leverage. The debt brake can be read as codifying this immutable fact — that states, like households, face a hard constraint. However, the structural data contradicts this: the constraint operates through formal legal instruments, not physical/mathematical limits; it has identifiable beneficiaries; exceptions can be and are renegotiated. This is a false summit: the 'immutable fact' framing naturalizes a contingent institutional choice about fiscal rules.
constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_brake_2009__exception_pressure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_brake_2009__exception_pressure_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_brake_2009__exception_pressure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_brake_2009__exception_pressure_reading, TR),
    TR >= 0.70.

:- end_tests(debt_brake_2009__exception_pressure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint reroutes spending through off-budget mechanisms rather than preventing it entirely. This is extraction because (1) transparency is lost, (2) parliamentary oversight is weakened through special-fund procedures, (3) the rule's stated purpose (fiscal discipline) is undermined by routine exception invocation. But extractiveness is not severe (0.72+) because exceptions actually function — the rule does deliver some fiscal credibility (lower borrowing costs, market discipline signaling) and does prevent the worst forms of profligacy in normal times. The measurement trajectory shows rising extractiveness over the interval as exceptions normalize and special funds proliferate: early years (2009-2014) saw exceptions as truly extraordinary; by 2019, pandemic relief and climate spending triggered exception clauses routinely. Suppression (0.58): Moderate-high. The rule is formally intact (not suppressed as text) but practically negotiated. Parliamentary authority remains formally supreme but is constrained by the constitutional rule and executive discretion over exceptions. Alternative spending pathways exist (off-budget vehicles) but require navigating special procedures. Suppression is not total because exceptions are explicit and debatable, but the rule's form constrains options. Theater Ratio (0.65): Moderate-high. The rule increasingly functions performatively: the main budget is constrained while spending continues through special funds and contingent vehicles. The performative dimension is the procedure around exceptions — emergency declarations, special-fund legislation, ECB purchases — which allows the constraint's form to be preserved while its substance is negotiated. As theater rises, the constraint degrades toward piton (institutional inertia maintaining a rule whose function has atrophied).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the executive/credibility beneficiaries (Rope/immediate horizon: the rule and exceptions work together to provide flexibility and signal discipline) and the transparency/investment victims (Snare/biographical horizon: the constraint destroys parliamentary oversight and prevents necessary spending). A moderate agent like parliament sees Tangled Rope: real coordination benefit (fiscal discipline against bias) coupled with real extraction (executive discretion, off-budget opacity). The scaffold perspective (European institutions) diverges by time horizon: immediate/biographical views see the rule as constraining; generational view sees it as temporary scaffolding (sunset mechanism in disguise). The piton perspective (constitutional doctrine) sees the rule as increasingly theatrical — the exceptions and special funds are the real mechanism; the rule itself is maintained through performative compliance. The false-summit perspective naturalizes the constraint as inherent to monetary systems, but the structural data reveals it as a contingent choice about fiscal rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position (beneficiary vs victim, power level, exit options) determines directionality (d) and thus experienced extractiveness. Executive authority benefits from the constraint (both rule and exceptions) and has arbitrage exit (can access off-budget mechanisms or lobby for new special funds) — derives low d (~0.15), experiences negative effective extraction (the rule is coordination). Parliament is constrained but also partially benefits (credibility) — derives moderate d (~0.50), experiences moderate extraction. Fiscal transparency is powerless and trapped (no exit from the commitment to budgetary honesty; the rule violates it systematically) — derives high d (~0.95), experiences high extraction. Infrastructure is moderately powered, constrained (can borrow for some categories but not others) — derives moderate-high d (~0.65-0.75), experiences high-to-very-high effective extraction. The perspectival gap is wide: executives see rope (coordination), parliament sees tangled_rope (mixed), transparency victims see snare (pure extraction). The shared metrics (ε=0.52, suppression=0.58) apply to all — the indexed classification at each (P,T,E,S) tuple explains the gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy within a single institution (Parliament, the constitutional order) rather than across agents. The mandate is: ensure fiscal discipline while preserving democratic spending authority. The constraint (debt brake) achieves the first while extracting authority on the second (exceptions route to executive discretion; special funds bypass normal appropriation). The resolution is not 'pick one type' but to recognize that this reading (exception pressure) diagnoses the mandatrophy: the rule survives precisely because it is violated in structured ways. The mandate is achieved through the holes punched in the rule, not despite them. This is why the constraint is Tangled Rope, not Rope — the coordination (fiscal discipline) requires the extraction (executive discretion over exceptions). From the fiscal credibility reading, mandatrophy is resolved differently (the rule is pure coordination because exceptions are rational emergency relief). From the investment starvation reading, mandatrophy goes unresolved (no amount of exceptions can restore long-term capital investment capacity under a borrowing constraint). The three readings of this kernel instantiate three different mandatrophy resolutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exception_scope_expansion,
    'Are emergency exceptions narrowing (genuinely temporary measures for true crises) or expanding (being progressively normalized and redefined to include routine challenges)?',
    'Historical analysis of exception invocations: count, duration, nominal trigger (pandemic, war, financial stability, climate transition, housing crisis, demographic shift), and whether exceptions were formally repealed or allowed to persist indefinitely. Track the scope-creep of what counts as ''emergency.''',
    'If narrowing: exceptions are Scaffold (genuine sunset mechanism, temporary bypass). If expanding: exceptions are becoming the structural rule, and the debt brake is degrading to Piton (performative). If exceptions normalize into standard categories: the constraint has effectively been rewritten through practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exception_scope_expansion, empirical, 'Whether emergency exceptions are narrowing or expanding over time').

omega_variable(
    off_budget_vehicle_financing_source,
    'Do special funds (Sondervermögen) and off-budget vehicles finance spending that would otherwise be authorized through normal appropriation, or do they fund genuinely additional activities?',
    'Comparative budget analysis: measure total state spending (on-budget + special funds + off-budget vehicles + contingent liabilities) across time; decompose by spending category; identify whether special funds are additive or substitutional.',
    'If substitutional (special funds = off-budget moving of normal spending): extractiveness rises (deception mechanism is the constraint itself). If additive (special funds enable new spending that budget rule would prevent): extractiveness stays moderate (constraint is working as designed, with exceptions as relief valves).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(off_budget_vehicle_financing_source, empirical, 'Whether off-budget vehicles finance additional spending or merely hide normal spending').

omega_variable(
    reading_contest_under_determination,
    'Does this reading (exception pressure: the rule survives by the holes punched in it) foreclose the sibling readings (fiscal credibility, investment starvation), or do they coexist as live positions held by different institutional actors?',
    'Institutional analysis: Do actors who hold the fiscal credibility reading acknowledge the exception pressure reading as legitimate, or do they deny it? Do actors who hold the investment starvation reading engage with the credibility argument, or dismiss it as rhetorical cover? Map which institutional communities hold which reading.',
    'If forecloses: the readings are logically incompatible (e.g., ''the rule is credible'' and ''the rule is undermined by exceptions'' cannot both be true in the same framework). If coexists: different parties hold different readings simultaneously, and the kernel contest is unresolved. If influences: this reading creates pressure on the others without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_under_determination, conceptual, 'Logical relationship between this reading and sibling readings of the debt brake kernel').

omega_variable(
    countercyclical_spending_necessity,
    'Is the rule''s rigidity (preventing borrowing during downturns) a feature (enforcing discipline) or a bug (preventing necessary crisis response)? Does the empirical record show that emergency exceptions are necessary to enable appropriate macroeconomic stabilization?',
    'Macroeconomic analysis: Compare spending patterns pre- and post-debt brake during crises (2008 financial crisis, 2020 pandemic, etc.). Measure: Did emergency exceptions enable sufficient countercyclical spending? Would pre-brake spending patterns have been superior? Did the rule''s existence prevent spending that would have been welfare-improving?',
    'If exceptions are necessary: the rule exhibits extraction (prevents appropriate spending; exceptions are relief valve, not democratic choice). If exceptions are excessive: the rule works as designed (exceptions are emergency override, not evasion). This determines whether the constraint is primarily Tangled Rope (mixed coordination and extraction) or primarily Rope (coordination with justified exceptions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countercyclical_spending_necessity, empirical, 'Whether emergency exceptions are necessary for appropriate macroeconomic stabilization').

omega_variable(
    fiscal_credibility_grounding,
    'Does the debt brake actually reduce borrowing costs (the stated credibility mechanism), or is fiscal credibility grounded in other factors (central bank policies, fiscal track record independent of the rule, demographic stability, export capacity)?',
    'Comparative credit analysis: Measure borrowing costs (bond yield spreads) for debt-brake-bound countries vs non-bound comparables, controlling for central bank policy, inflation regime, and growth trajectory. Identify the causal contribution of the rule itself to credibility.',
    'If causal (rule → credibility → lower rates): the coordination function is real, and the constraint is Rope/Tangled Rope with genuine benefit. If non-causal (other factors drive rates): the rule''s credibility function is rhetorical (theater), and extractiveness rises (the rule constrains without delivering the promised benefit). This determines whether beneficiaries are genuinely receiving coordination services or merely signaling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_credibility_grounding, empirical, 'Whether the debt brake causally produces fiscal credibility and lower borrowing costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_brake_2009__exception_pressure_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dbex_theater_t0_2009_formal_constraint, debt_brake_2009__exception_pressure_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dbex_theater_t5_2014_exception_procedures, debt_brake_2009__exception_pressure_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dbex_theater_t10_2019_performative_compliance, debt_brake_2009__exception_pressure_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dbex_extractiveness_t0_2009, debt_brake_2009__exception_pressure_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dbex_extractiveness_t5_2014_crisis_response, debt_brake_2009__exception_pressure_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dbex_extractiveness_t10_2019_pandemic_approach, debt_brake_2009__exception_pressure_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dbex_suppression_t0_2009_formal_rule, debt_brake_2009__exception_pressure_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(dbex_suppression_t5_2014_exception_normalization, debt_brake_2009__exception_pressure_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(dbex_suppression_t10_2019_off_budget_expansion, debt_brake_2009__exception_pressure_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_brake_2009__exception_pressure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(debt_brake_2009__exception_pressure_reading, debt_brake_2009__fiscal_credibility_reading).
narrative_ontology:affects_constraint(debt_brake_2009__exception_pressure_reading, debt_brake_2009__investment_starvation_reading).
narrative_ontology:affects_constraint(debt_brake_2009__exception_pressure_reading, european_monetary_union_stability_constraint).
narrative_ontology:affects_constraint(debt_brake_2009__exception_pressure_reading, special_purpose_vehicle_proliferation).

% DUAL FORMULATION NOTE:
% The debt brake kernel instantiates three structurally distinct constraints with different ε values. This story (exception_pressure_reading, ε=0.52) reads the constraint as Tangled Rope through exception clauses and off-budget mechanisms. Sibling stories (fiscal_credibility_reading, investment_starvation_reading) read the same institutional text with different ε values and different classification types. All three should be generated and linked via affects_constraints. The kernel contest is not resolved by choosing one reading — it is a genuine institutional contest between coexisting positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_brake_2009__exception_pressure_reading, institutional, 0.2).
constraint_indexing:directionality_override(debt_brake_2009__exception_pressure_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
