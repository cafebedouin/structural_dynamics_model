% ============================================================================
% CONSTRAINT STORY: dual_career_household_economics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_career_household_economics, []).

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
 *   constraint_id: dual_career_household_economics
 *   human_readable: Dual Career Household Economics and Domestic Labor Extraction
 *   domain: household_economics/gender_dynamics/labor
 *
 * SUMMARY:
 *   The dual-career household economics constraint structures the allocation
 *   of paid work, domestic labor, and caregiving within households where both
 *   partners hold formal employment. This constraint exhibits the full
 *   six-type DR classification spectrum: from the secondary earner's
 *   perspective it appears as pure extraction (snare) when they are
 *   economically trapped, as hybrid coordination-extraction (tangled rope)
 *   when they recognize genuine economic benefit alongside unequal labor
 *   distribution, and from the primary earner's view it appears as pure
 *   coordination (rope) solving the middle-class stability problem. The
 *   constraint combines genuine coordination function (dual incomes enable
 *   middle-class household stability in high-cost regions) with systematic
 *   extraction (unequal domestic labor burden, career penalties for secondary
 *   earner, concentration of economic power with primary earner). The theater
 *   ratio (0.48) reflects that roughly half the enforcement is actual
 *   childcare/household necessity, half is theatrical gender performance and
 *   cultural expectation. The extractiveness has increased from 0.38 to 0.52
 *   over the measurement interval, indicating that while the necessity of
 *   dual income has remained constant, the unequal labor distribution has
 *   become more pronounced as dual-career norms have spread without
 *   corresponding shifts in domestic labor or caregiving norms.
 *
 * KEY AGENTS:
 *   - Secondary Earner: Primary victim (powerless/trapped or moderate/constrained depending on economic feasibility of exit) — bears disproportionate domestic labor burden, career interruptions, wage penalties
 *   - Primary Earner: Primary beneficiary (institutional/arbitrage) — concentrates economic power and career advancement; experiences constraint as pure coordination
 *   - Children/Dependents: Implicit victim (powerless/trapped) — dependent on caregiving arrangements that are privatized and unequally distributed
 *   - Household as Economic Unit: Beneficiary (institutional/arbitrage) — dual incomes enable middle-class stability that is no longer achievable on single income
 *   - Feminist Organizing Coalitions: Organized agents (organized/constrained) — identify and challenge extraction mechanism through policy advocacy (childcare funding, workplace flexibility, labor norm change)
 *   - Labor Market Structure: Institutional actor (institutional/arbitrage) — creates and maintains wage gaps, motherhood penalties, occupational segregation that reinforce extraction
 *   - Patriarchal Gender Norms: Institutional mechanism (institutional/arbitrage) — enforcement through cultural expectations, family obligation framing, gendered labor norm reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_career_household_economics, 0.52).
domain_priors:suppression_score(dual_career_household_economics, 0.58).
domain_priors:theater_ratio(dual_career_household_economics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_career_household_economics, extractiveness, 0.52).
narrative_ontology:constraint_metric(dual_career_household_economics, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dual_career_household_economics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_career_household_economics, tangled_rope).
narrative_ontology:human_readable(dual_career_household_economics, "Dual Career Household Economics and Domestic Labor Extraction").
narrative_ontology:topic_domain(dual_career_household_economics, "household_economics/gender_dynamics/labor").

domain_priors:requires_active_enforcement(dual_career_household_economics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_career_household_economics, primary_earner).
narrative_ontology:constraint_beneficiary(dual_career_household_economics, household_stability_beneficiary).
narrative_ontology:constraint_victim(dual_career_household_economics, secondary_earner).
narrative_ontology:constraint_victim(dual_career_household_economics, household_reproductive_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECONDARY EARNER (SNARE) — Trapped by economic dependency, childcare responsibility, and the gap between dual-income household expenses and actual wage equity. Faces maximum extraction: carries disproportionate domestic labor burden while unable to exit due to childcare constraints and financial interdependency. No meaningful alternatives; exit costs (custody battles, economic precarity, housing loss) are prohibitive.
constraint_indexing:constraint_classification(dual_career_household_economics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SECONDARY EARNER (TANGLED ROPE) — Constrained but not trapped. Recognizes genuine coordination benefit: dual incomes enable middle-class stability, housing, children's education, healthcare access. But also recognizes extraction: unequal domestic labor distribution, career interruptions, wage penalties for caregiving gaps. Exit is possible at high cost (relationship dissolution, career reset, lifestyle reduction). Experiences the constraint as hybrid: coordination with embedded asymmetric extraction.
constraint_indexing:constraint_classification(dual_career_household_economics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIMARY EARNER (ROPE) — Experiences the constraint as pure coordination. Two incomes solve the fundamental household economic problem: funding middle-class stability on single earner is now structurally impossible in high-cost regions. The constraint produces genuine benefit (dual purchasing power, risk diversification, career advancement for both). Primary earner has arbitrage options: can allocate labor toward higher-earning activities. Extraction appears minimal from this vantage — the secondary earner's domestic labor contribution is naturalized as family obligation, not extraction.
constraint_indexing:constraint_classification(dual_career_household_economics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEMINIST ORGANIZING (TANGLED ROPE) — Sees both coordination function and extractive mechanism. Dual-career structure genuinely coordinates household stability and reduces dependency-related violence and control. But also sees systematic extraction: unequal domestic labor distribution documented in time-use surveys (secondary earner performs 65-80% of household/childcare work despite earning 40-50% of household income), wage penalties for motherhood, occupational segregation, and the second shift. Active enforcement (gender norms, workplace inflexibility, childcare privatization) maintains the extraction. Organized agents can identify and challenge the mechanism, but structural barriers persist.
constraint_indexing:constraint_classification(dual_career_household_economics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATRIARCHAL LABOR NORM (PITON) — The constraint is institutionally degraded. The underlying function (primary provider / secondary homemaker division) no longer matches economic reality — dual incomes are now necessary for middle-class stability, and the secondary earner typically cannot exit the labor market. Yet the domestic labor extraction persists through theatrical enforcement: performative household management expectations, invisible emotional labor norms, and the culturally maintained fiction that unpaid domestic work is 'family obligation' rather than labor. Theater ratio (0.48) reflects that roughly half the enforcement is actual childcare/household necessity, half is theatrical gender performance. The piton exists because the primary function (providing an alternative to dual-income necessity) has atrophied, but the extraction mechanism (unequal domestic labor expectation) persists through institutional inertia.
constraint_indexing:constraint_classification(dual_career_household_economics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — The risk here is naturalizing a contingent institutional arrangement as an immutable economic law. The argument: 'Dual-career households require someone to manage domestic production; household management is time-consuming; asymmetric labor division is an inevitable consequence of economics.' This perspective treats the secondary earner's domestic labor extraction as a structural feature of dual-income necessity, not as a choice or enforcement mechanism. The engine will flag this as a false summit: the constraint is not natural law, but a contingent institutional arrangement (workplace inflexibility, privatized childcare, gendered labor norms) that makes extraction appear necessary. Alternative institutional arrangements (workplace flexibility, public childcare, egalitarian labor norms) would reduce or eliminate the extraction while maintaining the coordination function.
constraint_indexing:constraint_classification(dual_career_household_economics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_career_household_economics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dual_career_household_economics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dual_career_household_economics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_career_household_economics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dual_career_household_economics, TR),
    TR >= 0.70.

:- end_tests(dual_career_household_economics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from the secondary earner through unequal domestic labor burden, documented in time-use surveys as 65-80% of household/childcare work despite 40-50% of household income. This is not purely coordinative work (which would be split equally) nor is it purely extractive (which would eliminate the secondary earner's income benefit entirely). The secondary earner gains from dual-income household stability but loses through time extraction and career penalties. The rising trend (0.38 → 0.52) reflects increasing divergence between dual-income necessity (which plateaued) and labor norm change (which has stalled). Suppression (0.58): Moderate-high, reflecting multiple barriers: childcare costs create economic dependency, workplace inflexibility prevents egalitarian labor sharing, cultural norms frame household management as family obligation rather than labor, and exit costs (custody barriers, housing instability, income insufficiency) are prohibitive for many secondary earners. Theater ratio (0.48): Moderate. Roughly half the constraint is actual childcare and household necessity; half is theatrical enforcement through gender performance expectations and the cultural fiction that unpaid domestic work is not 'real' labor. The theatrical component includes performative household standards, invisible emotional labor expectations, and the social sanctioning of mothers who prioritize paid work over household performance.
 *
 * PERSPECTIVAL GAP:
 *   The secondary earner's snare classification results from high d (victim + trapped) producing chi ≥ 0.66. The tangled rope classification from constrained perspective results from moderate d producing 0.40 ≤ chi ≤ 0.90 with both coordination benefit and asymmetric extraction present. The primary earner's rope classification results from low d (beneficiary + arbitrage) producing chi ≤ 0.35 with high coordination benefit. The feminist perspective's tangled rope arises from organized power status allowing the agent to see and challenge both the coordination function and extraction mechanism simultaneously. The piton perspective reflects that the constraint's primary function (providing alternative to economic necessity) has atrophied — dual income is now necessary for middle-class stability, not optional — but the extraction mechanism (unequal domestic labor) persists through theatrical gender norm enforcement. The false mountain perspective risks seeing the constraint as natural law when it is contingent on institutional arrangements that could be changed (workplace flexibility, public childcare, labor norm equalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is derived from their structural position relative to the constraint. The secondary earner occupies the victim position — they bear the domestic labor burden. Their exit options determine whether this victimhood produces a snare or tangled rope: trapped exit (economic dependency, childcare responsibility) means the extraction is maximal and inescapable (snare); constrained exit (high-cost but possible exit through relationship dissolution) means extraction is high but not absolute (tangled rope). The primary earner occupies the beneficiary position — they gain economic power and career advancement. Their arbitrage exit options mean they experience low or negative effective extraction; the constraint appears to them as pure coordination. The household unit benefits from dual-income stability; this is a genuine coordination function that solves a real problem (middle-class stability requires two incomes in high-cost regions). The feminist perspective's organized power status allows them to simultaneously perceive both the coordination function and the extraction mechanism, identifying the constraint as a true tangled rope: coordination (dual income enables stability) + enforcement (gender norms maintain unequal labor distribution) + asymmetric extraction (secondary earner bears burden despite income contribution).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing how institutional decomposition creates the appearance of pure coordination while masking systematic extraction. At the household level, dual-income coordination is real: two incomes are necessary for middle-class stability in high-cost regions. But this coordination has been layered with an extraction mechanism (unequal domestic labor distribution) that the coordination does not require. The mandatrophy is resolved by recognizing that the constraint is NOT JUST coordination, nor JUST extraction, but a true tangled rope where institutional enforcement (gendered labor norms, workplace inflexibility, childcare privatization) maintains extraction that persists beyond what economic coordination actually requires. The false mountain perspective demonstrates the mandatrophy trap: calling the constraint 'natural economic necessity' naturalizes what is actually a choice to maintain unequal labor distribution. The resolution requires showing that alternative institutional arrangements (workplace flexibility, public childcare, equalized parental leave, egalitarian labor norms) would preserve the coordination function (dual income) while eliminating much of the extraction (unequal domestic labor). The constraint's type is NOT dependent on whether dual income is necessary; it is dependent on whether the unequal labor distribution that accompanies dual income is itself necessary or enforced. Since the latter is true, classification as tangled rope is justified, and mandatrophy is resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_labor_valuation_ambiguity,
    'Is the measured suppression (0.58) reflecting structural economic barriers or internalized identity-locked acceptance of unequal labor distribution?',
    'Post-exit trajectory analysis: if secondary earner reports persistent guilt/obligation regarding household management after relationship dissolution, the suppression includes internalized identity lock. If suppression drops rapidly after economic independence is achieved, it was primarily structural.',
    'If internalized (identity-locked): constraint is stronger than structural measure suggests — the secondary earner carries the extraction mechanism with them after exiting the relationship. If structural: constraint can be reduced by economic independence alone. Affects classification of exit_options: trapped (structural) vs identity_locked (internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_labor_valuation_ambiguity, empirical, 'Structural vs internalized suppression in domestic labor extraction').

omega_variable(
    coordination_necessity_threshold,
    'What portion of the dual-income household is genuinely necessary coordination (funding middle-class stability) versus extraction (unequal domestic labor)?',
    'Comparative household economics analysis: cost of childcare, housing, education in dual-income regions vs single-income adequacy; time-use survey data on domestic labor distribution controlling for workplace hours and earning differential; cross-cultural comparison with egalitarian labor norms and public childcare provision.',
    'If coordination necessity ≥ 0.70: the constraint is primarily rope (with embedded extraction as a side effect). If coordination necessity < 0.50: the constraint is primarily snare (the dual income is justified, but extraction exceeds what the economic necessity requires). Affects chi formula and classification from moderate/constrained perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Proportion of dual-income necessity vs extractive labor asymmetry').

omega_variable(
    institutional_childcare_availability,
    'How much of the secondary earner''s extraction is due to childcare privatization (private market pricing) vs cultural gender norms (belief that parent should provide childcare)?',
    'Jurisdictional comparison: regions with subsidized public childcare and workplace flexibility (Scandinavia, Quebec) show significantly lower secondary-earner domestic labor burden and different classification patterns. Controlled experiment: does access to publicly funded childcare reduce extractiveness regardless of earnings gap?',
    'If childcare privatization is primary driver: constraint is contingent on economic policy. Public childcare and workplace flexibility would reduce extractiveness to 0.25-0.35 range (pure coordination). If gender norms are primary driver: institutional change required to reduce extraction. Affects mandatrophy resolution and policy implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_childcare_availability, empirical, 'Childcare privatization vs cultural norms as drivers of extraction').

omega_variable(
    secondary_earner_income_adequacy,
    'Can a secondary earner unilaterally exit the household (with custody and support) and maintain middle-class stability on their solo earnings?',
    'Comparative income analysis: secondary earner''s wage relative to regional cost of living, childcare costs, housing costs for a single parent; availability of spousal/child support mechanisms; labor market penalties for employment gaps (due to childcare). Geographic and temporal variation.',
    'If exit is economically impossible: exit_options are ''trapped'' (maximum suppression, maximum extraction). If exit is possible at high cost: exit_options are ''constrained'' (high suppression, lower extraction). If exit is economically feasible: exit_options are ''mobile'' (low suppression, lower extraction). This directly determines d value and f(d) in chi formula.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_earner_income_adequacy, empirical, 'Economic feasibility of secondary earner solo exit with childcare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_career_household_economics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dche_tr_t0, dual_career_household_economics, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dche_tr_t5, dual_career_household_economics, theater_ratio, 5, 0.45).
narrative_ontology:measurement(dche_tr_t10, dual_career_household_economics, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(dche_be_t0, dual_career_household_economics, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dche_be_t5, dual_career_household_economics, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dche_be_t10, dual_career_household_economics, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_career_household_economics, resource_allocation).
narrative_ontology:affects_constraint(dual_career_household_economics, gendered_wage_gap).
narrative_ontology:affects_constraint(dual_career_household_economics, motherhood_career_penalty).
narrative_ontology:affects_constraint(dual_career_household_economics, childcare_privatization).
narrative_ontology:affects_constraint(dual_career_household_economics, workplace_inflexibility_norms).

% DUAL FORMULATION NOTE:
% The dual-career household constraint family includes separate constraints for (1) resource coordination (dual income enabling middle-class stability: ε~0.15, pure rope), (2) domestic labor extraction (unequal task distribution: ε~0.65, snare), and (3) institutional labor norm enforcement (gender-scripted caregiving expectations: ε~0.42, piton). This story integrates all three around the extractiveness value (0.52) representing the hybrid mechanism. Decomposition into separate stories would be possible but would lose the essential insight: the coordination and extraction are institutionally fused. The remedy (workplace flexibility, public childcare, labor norm equalization) must address all three simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
