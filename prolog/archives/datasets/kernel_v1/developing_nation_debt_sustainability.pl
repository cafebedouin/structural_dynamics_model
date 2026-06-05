% ============================================================================
% CONSTRAINT STORY: developing_nation_debt_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developing_nation_debt_sustainability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: developing_nation_debt_sustainability
 *   human_readable: Developing Nation Debt Sustainability Trap
 *   domain: economic_policy/development_finance/geopolitics
 *
 * SUMMARY:
 *   The developing nation debt sustainability constraint operates at the
 *   intersection of macroeconomic necessity, institutional governance, and
 *   geopolitical power. It appears as an immutable fact of global capitalism
 *   — poor nations lack capital, borrowing requires creditors to enforce
 *   discipline, and fiscal responsibility is non-negotiable. Yet the
 *   constraint is fundamentally about power: who controls capital allocation,
 *   whose definition of 'sustainability' prevails, and how are the benefits
 *   and costs distributed. The constraint exhibits genuine hybridity: debtor
 *   nation governments do face real fiscal constraints and benefit from
 *   access to external capital and technical expertise (tangled rope
 *   elements). Simultaneously, the IMF-anchored system extracts substantial
 *   rents through policy control, asset sales, and reduced domestic
 *   investment capacity (snare elements for powerless populations). The
 *   constraint has been tightening since the 1980s but is now experiencing
 *   decomposition as alternative financing sources mature. The Piton
 *   classification of the Washington Consensus framework reflects that the
 *   empirical case for structural adjustment has substantially eroded
 *   (decades of evidence show it harms immediate welfare without improving
 *   long-term growth), yet the institutional apparatus persists through
 *   inertia and self-interest. The false summit (mountain from analytical
 *   perspective) naturalizes what is actually a constructed institutional
 *   arrangement benefiting creditor nations and the IMF apparatus.
 *
 * KEY AGENTS:
 *   - Debtor Nation Populations: Primary victims (powerless/trapped) — bear full cost of austerity, privatization, reduced public investment; no exit option
 *   - Debtor Nation Governments: Moderate agent (moderate/constrained) — face genuine fiscal constraints and benefit from IMF coordination; simultaneously lose policy autonomy to external conditionality
 *   - Creditor Nations and IMF Apparatus: Primary beneficiaries (institutional/arbitrage) — receive reliable debt service, maintain policy influence, access high-yield investment; ample exit options and arbitrage capacity
 *   - Alternative Development Finance Coalition: Organized agents (organized/mobile) — BRICS Bank, bilateral development partners, climate funds building parallel financing with lower conditionality; represent sunset pressure on traditional constraint
 *   - Washington Consensus Institutional Framework: Degraded institution (institutional/constrained) — persists through inertia despite empirical failure; maintains staff legitimacy and creditor nation preferences through increasing institutional theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent power structure as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developing_nation_debt_sustainability, 0.58).
domain_priors:suppression_score(developing_nation_debt_sustainability, 0.72).
domain_priors:theater_ratio(developing_nation_debt_sustainability, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, extractiveness, 0.58).
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developing_nation_debt_sustainability, tangled_rope).
narrative_ontology:human_readable(developing_nation_debt_sustainability, "Developing Nation Debt Sustainability Trap").
narrative_ontology:topic_domain(developing_nation_debt_sustainability, "economic_policy/development_finance/geopolitics").

domain_priors:requires_active_enforcement(developing_nation_debt_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, creditor_nations).
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, imf_institutional_apparatus).
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, multinational_corporations).
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, capital_exporters).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, debtor_nation_populations).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, domestic_policy_autonomy).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, public_investment_capacity).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, long_term_development_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION POPULATION (SNARE) — Structurally trapped: capital flight, currency devaluation, external debt obligations denominated in foreign currency, and creditor-mandated austerity leave no exit option. Generational time horizon captures that debt servicing consumes resources across generations; immediate exit appears impossible. Experiences maximum extraction: austerity reduces public health, education, and infrastructure investment; structural adjustment eliminates price controls and subsidies; privatization sells assets at distressed valuations. No coordination benefit — the constraint transfers wealth outward.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEBTOR NATION GOVERNMENT (TANGLED ROPE) — Structurally constrained: defaulting risks capital market access, sanctions, asset seizure, and diplomatic isolation. But structural adjustment programs do provide genuine coordination benefits: debt restructuring avoids immediate default, access to IMF credit lines enables bridge financing, technical assistance builds tax collection and fiscal monitoring capacity. The constraint exhibits both: legitimate coordination (fiscal discipline, capital allocation) AND asymmetric extraction (creditors dictate policy, enforcement capacity flows toward external authorities, sovereignty erosion). Active enforcement via conditionality agreements and market discipline.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR NATIONS AND IMF (ROPE) — Experiences the constraint as pure coordination: the IMF framework ensures debtor nations maintain fiscal discipline, service debt reliably, and remain open to capital flows. Creditor nations and the IMF apparatus benefit from this arrangement — repayment rates are historically high, market confidence in emerging-market bonds stabilizes, and policy influence is substantial. From this perspective, the constraint solves a real collective action problem: without coordination, debtors would default systematically, capital markets would freeze, and creditor nations would lose access to high-yield investment opportunities. Arbitrage options (alternative capital sources, policy influence over multiple debtors) are ample. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE FINANCE COALITION (SCAFFOLD) — Organized agents (BRICS development banks, Islamic Finance institutions, climate funds, bilateral development partners) are building parallel financing pathways that bypass IMF conditionality. These alternatives have lower conditionality overhead and higher development-aligned incentives. The traditional debt sustainability framework is experiencing sunset pressure as alternative sources of capital (Chinese development finance, BRICS Bank, green bonds, remittances) expand debtor autonomy. Theater is moderate (0.65) — the IMF's technical apparatus produces genuine fiscal analysis, but much of the conditionality enforcement is performative (symbolic policy commitments that governments manipulate or ignore in practice). As alternatives mature, the traditional constraint's extraction mechanism loses force — debtors gain negotiating leverage and can credibly threaten to shift to alternative financing.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WASHINGTON CONSENSUS FRAMEWORK (PITON) — The institutional logic of structural adjustment programs (privatization, trade liberalization, deregulation, fiscal austerity) was grounded in neoclassical growth theory and 1980s policy consensus. The empirical case for these policies has substantially eroded — countries that diverged from the consensus (South Korea, Vietnam, China) achieved faster development than consensus-followers (Argentina under De la Rúa, sub-Saharan Africa under structural adjustment). The framework persists through institutional inertia: IMF staff are trained in these principles, creditor nations prefer them, and exiting requires acknowledging decades of policy error. Theater has risen (0.65+) — staff produce increasingly sophisticated technical analysis to justify policies whose real-world effects contradict the theory. The piton classification reflects that the primary coordination function (ensuring fiscal discipline and capital allocation) has atrophied; what remains is institutional theater (maintaining staff legitimacy) and residual extraction (policy influence).
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, capital constraints are immutable features of global development: poor nations lack capital, rich nations have surplus capital, and rational capital allocation requires debtors to service obligations reliably. The 'debt sustainability' framework appears as a natural law of economics — no nation can sustainably spend more than it earns indefinitely; creditors rationally demand collateral and conditionality; fiscal discipline is inherent to sustainable development. However, the structural data reveals this as a false summit: the constraint benefits identifiable institutional actors (creditor nations, IMF apparatus, multinational corporations) and harms identifiable populations (debtor nation citizens, domestic policy autonomy). The 'natural law' framing naturalizes what is actually a contingent institutional arrangement grounded in creditor power and asymmetric capital market structure.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developing_nation_debt_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developing_nation_debt_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developing_nation_debt_sustainability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developing_nation_debt_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developing_nation_debt_sustainability, TR),
    TR >= 0.70.

:- end_tests(developing_nation_debt_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts substantial value: policy control flows to IMF/creditor nations rather than elected governments; asset privatization sells public resources at distressed valuations; debt servicing consumes resources that could fund public investment; capital flight risk disciplines policy choices toward creditor preferences. However, extraction is not maximal (0.70+) because some debtor nation governments actively manage programs, some alternative financing exists, and debt relief mechanisms have reduced worst cases. The measurement trajectory shows extractiveness rising from 0.35 (early 1980s crisis phase) to peak of 0.62 (mid-1990s, maximum creditor leverage) and declining to 0.48-0.52 as alternatives emerge. Suppression (0.72): High and sustained. The mechanism operates through capital market discipline (threat of exclusion from credit markets), IMF conditionality enforcement (tied disbursements), sanctions threat, currency devaluation risks, and asset seizure (sovereign immunity increasingly contested). The suppression requirement measurement peaks at 0.78 (mid-1990s) when enforcement infrastructure was most developed and alternative financing minimal; declining to 0.68 as alternatives mature but remaining high. Theater ratio (0.65): The IMF technical apparatus produces genuine fiscal analysis and policy advice, but the structural adjustment ideology persists despite mounting empirical evidence of failure. Theater has risen from 0.38 (pragmatic 1980s crisis management) to 0.71 (peak institutional defense of Washington Consensus) and slightly declining to 0.62 as rhetorical loosening occurs, but the institutional core persists. The theater reflects that staff must justify policies whose real-world effects contradict the underlying theory — requiring increasingly sophisticated technical arguments to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exemplifies how identical structural dynamics can classify as six different constraint types depending on the observer's position. This is not epistemic relativism — each classification is structurally accurate for that observer. The debtor population genuinely experiences a Snare (no exit, no coordination benefit). The debtor government genuinely experiences Tangled Rope (real coordination value, real extraction). The creditor nation genuinely experiences Rope (coordination benefit, no extraction cost). The alternative finance coalition genuinely sees sunset pressure (Scaffold). The IMF framework genuinely exhibits institutional theater masking degraded function (Piton). The civilizational observer genuinely risks naturalizing a contingent power structure (false summit Mountain). The perspectival gap is diagnostic: it reveals the constraint's asymmetry and the fallacy of claiming a single 'correct' classification across all positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is computed from the base extractiveness (ε=0.58), the agent's directionality value (d), and scope modifier. Beneficiaries with arbitrage options (creditor nations/IMF, d ≈ 0.08-0.15) experience low or negative effective extraction — the constraint distributes benefits toward them. Constrained agents who are partial victims (debtor governments, d ≈ 0.55-0.65) experience moderate extraction — they lose policy autonomy and face costs, but also benefit from capital access and coordination. Trapped agents who are full victims (debtor populations, d ≈ 0.92-0.95) experience maximum extraction chi — the constraint's burden falls fully on them. The organized coalition with mobile exit options (d ≈ 0.35-0.40) experiences moderate extraction — they face costs but can adapt and find alternatives. The analytical observer (d ≈ 0.72) experiences extractiveness as a pervasive structural feature, revealing the constraint's asymmetry. The directionality derivation follows the canonical structure: beneficiary status + exit options → low d → negative chi; victim status + trapped exit → high d → high chi; organized agent status + mobile exit → moderate d → moderate chi. No directionality overrides are needed — the canonical derivation accurately captures the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival decomposition and temporal measurement. The question 'Is this Tangled Rope or Snare?' has different answers depending on the observer: rope from the perspective of the government/creditor (genuine coordination), snare from the perspective of the population (pure extraction). Both are correct within their observational frames. The measurement trajectory (extractiveness rising to 0.62 then declining; theater rising to 0.71 then slightly declining; suppression remaining high but declining) shows that the constraint is evolving — it was more purely extractive (moving toward snare) in the 1990s-2000s peak, is becoming slightly less extractive as alternatives mature, but remains substantially tangled (both coordination and extraction elements persist). The Piton classification identifies the Washington Consensus framework as a degraded institution — the theoretical case for it has collapsed, but the institutional apparatus persists through inertia and bureaucratic self-interest. The false summit (mountain from analytical perspective) naturalizes what the structural data reveals as a contingent power arrangement: if debtors had genuine alternatives and creditors lost enforcement capacity, the constraint would dissolve or be radically renegotiated. The mandatrophy is resolved by recognizing that mandatrop is not a failure of the framework but a feature — it reflects that the constraint is genuinely mixed (both coordination and extraction), genuinely contested (different actors see it differently), and genuinely evolving (measurement shows structural change over time).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_effectiveness_ambiguity,
    'Do IMF structural adjustment conditions actually improve long-term development outcomes for debtor nations, or do they primarily transfer policy control to external authorities while harming immediate welfare and social investment?',
    'Longitudinal analysis of countries under IMF conditionality vs. alternative financing: comparison of GDP growth rates, poverty reduction, human development indicators, public investment in health/education, and long-term fiscal sustainability 10-30 years post-program',
    'If conditions improve outcomes: constraint is genuine Tangled Rope (coordination function is real). If conditions harm outcomes: constraint is Snare with institutional theater (coordination function is nominal; extraction is real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_effectiveness_ambiguity, empirical, 'Whether structural adjustment conditions improve long-term development outcomes').

omega_variable(
    capital_scarcity_structural_vs_artificial,
    'Is capital scarcity in developing nations a structural feature of global economics, or is it partially artificial — maintained by capital controls, debt-denominated-in-foreign-currency bias, and extractive investment structures that prevent domestic capital accumulation?',
    'Historical analysis of capital flows, foreign direct investment conditions, repatriation of profits, and debt denomination patterns; comparison with periods/countries that accumulated domestic capital despite external debt constraints',
    'If structural: debt sustainability is Mountain (inherent limit). If artificial: constraint is Snare (extract-then-suppress mechanism that appears inevitable but is contingent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_scarcity_structural_vs_artificial, empirical, 'Whether capital scarcity is structural or maintained by institutional arrangements').

omega_variable(
    alternative_financing_maturation_timeline,
    'Will alternative development finance sources (BRICS Bank, bilateral development finance, climate funds, Islamic finance) mature fast enough to provide genuine exit option for debtor nations, or will traditional IMF-anchored financing remain dominant due to scale and creditor-nation backing?',
    'Tracking of alternative financing volume, terms, and conditionality over next 10-15 years; measurement of debtor nation switching behavior when alternative sources become available',
    'If alternatives mature: Scaffold sunset timeline is real (~15-20 years). If alternatives remain marginal: traditional constraint persists indefinitely (no exit path for powerless agents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_maturation_timeline, empirical, 'Maturation timeline for alternative development finance sources').

omega_variable(
    debt_sustainability_definition_contestation,
    'What constitutes ''sustainable'' debt? Creditor nations define it as debt-to-GDP ratios allowing reliable service with market-priced capital. Debtor nations define it as debt levels permitting public investment in development priorities (health, education, infrastructure). These definitions produce different policy thresholds and can generate irreconcilable conflicts.',
    'Formal definition analysis: which definition is embedded in IMF/World Bank criteria; historical cases where definitions collided and which party''s definition prevailed; hypothetical tests of which definition a debtor nation would adopt if exit were available',
    'If creditor definition dominates: constraint is extractive (imposes external definition against local interests). If both definitions coexist: constraint is contested but genuine Tangled Rope (both coordination and extraction are real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_sustainability_definition_contestation, conceptual, 'Definitional contestation over ''sustainable debt'' between creditors and debtors').

omega_variable(
    sovereignty_erosion_measurement,
    'How much policy autonomy do debtor nations actually lose under structural adjustment vs. how much is preserved? Are many conditionality commitments nominal (governments ignore them) or actively enforced?',
    'Analysis of IMF programs: which conditions are actually enforced (linked to disbursements) vs. which are nominal (included in letters of intent but not monitored); case studies of government defection and enforcement response; measurement of policy divergence between conditionality requirements and actual government action',
    'If highly enforced: suppression is high, sovereignty erosion is real. If mostly nominal: suppression is lower, governments retain de facto autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_erosion_measurement, empirical, 'Degree of actual enforcement of IMF conditionality vs. nominal commitments').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is the debt sustainability constraint a natural law of economics (immutable capital allocation principle), or is it a constructed institutional arrangement that benefits specific actors and could be structured differently if political will existed?',
    'Comparative institutional analysis: historical alternatives (Brady Plan debt forgiveness, HIPC Initiative, Iceland''s unilateral debt repudiation effects); hypothetical tests of what happens when debtors gain credible exit options or when creditors face losses; analysis of whether creditor preferences align with ''natural'' economics or with extraction',
    'If natural law: Mountain classification holds (no beneficiary/victim distinction). If constructed: constraint is Snare (beneficiaries revealed, false summit triggers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether debt sustainability is immutable natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developing_nation_debt_sustainability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devdebt_tr_t0, developing_nation_debt_sustainability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(devdebt_tr_t5, developing_nation_debt_sustainability, theater_ratio, 5, 0.52).
narrative_ontology:measurement(devdebt_tr_t10, developing_nation_debt_sustainability, theater_ratio, 10, 0.68).
narrative_ontology:measurement(devdebt_tr_t15, developing_nation_debt_sustainability, theater_ratio, 15, 0.71).
narrative_ontology:measurement(devdebt_tr_t20, developing_nation_debt_sustainability, theater_ratio, 20, 0.65).
narrative_ontology:measurement(devdebt_tr_t25, developing_nation_debt_sustainability, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(devdebt_be_t0, developing_nation_debt_sustainability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devdebt_be_t5, developing_nation_debt_sustainability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(devdebt_be_t10, developing_nation_debt_sustainability, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(devdebt_be_t15, developing_nation_debt_sustainability, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(devdebt_be_t20, developing_nation_debt_sustainability, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(devdebt_be_t25, developing_nation_debt_sustainability, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(devdebt_su_t0, developing_nation_debt_sustainability, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(devdebt_su_t10, developing_nation_debt_sustainability, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(devdebt_su_t20, developing_nation_debt_sustainability, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(devdebt_su_t25, developing_nation_debt_sustainability, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developing_nation_debt_sustainability, resource_allocation).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, structural_adjustment_policy_autonomy).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, developing_nation_capital_flight).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, commodity_price_volatility_trap).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, imf_conditionality_enforcement).

% DUAL FORMULATION NOTE:
% The debt sustainability constraint family decomposes into linked structural stories: (1) macroeconomic debt-to-GDP sustainability (ε≈0.40, mathematical constraint), (2) IMF-anchored institutional governance (ε≈0.58, this story), (3) creditor enforcement mechanisms (ε≈0.65, separate story), (4) geopolitical capital control dynamics (ε≈0.70, separate story). This story focuses on the institutional hybrid coordination-extraction mechanism. Each story in the family links to structural antecedents (capital scarcity) and structural consequences (policy autonomy loss, capital flight acceleration, alternative financing emergence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developing_nation_debt_sustainability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
