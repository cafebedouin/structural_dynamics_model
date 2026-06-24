% ============================================================================
% CONSTRAINT STORY: gendered_outcome_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_outcome_asymmetry, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_outcome_asymmetry
 *   human_readable: Gray Divorce Gendered Outcome Asymmetry
 *   domain: social/economic/demographic
 *
 * SUMMARY:
 *   Gray divorce—marital dissolution after age 50—has doubled since 1990.
 *   Women initiate 60-70% of these divorces but face systematically worse
 *   financial outcomes: income drops of 40-50% versus 20-25% for men, wealth
 *   losses, lower re-partnering rates, and elevated food insecurity risk. The
 *   legal framework presents itself as gender-neutral but operates on
 *   property division and alimony rules that systematically undervalue
 *   decades of unpaid domestic labor and career interruption for caregiving.
 *   The constraint is claimed as snare: exit is formally available but
 *   carries asymmetric extraction that falls on the party most likely to
 *   initiate exit.
 *
 * KEY AGENTS:
 *   - divorcing_women_50_plus: Primary victim (moderate/identity_locked) — initiate majority of gray divorces, bear asymmetric financial penalty
 *   - divorcing_men_50_plus: Primary beneficiary (moderate/mobile) — retain higher earning capacity and re-partnering probability
 *   - family_law_practitioners: Agenda setter (organized/mobile) — administer the legal framework producing asymmetric outcomes
 *   - financial_services_industry: Secondary beneficiary (institutional/arbitrage) — capture fees from divorce-related transactions
 *   - feminist_legal_scholars: Excluded (organized/analytical) — document systematic undervaluation, excluded from doctrine-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_outcome_asymmetry, 0.78).
domain_priors:suppression_score(gendered_outcome_asymmetry, 0.71).
domain_priors:theater_ratio(gendered_outcome_asymmetry, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_outcome_asymmetry, extractiveness, 0.78).
narrative_ontology:constraint_metric(gendered_outcome_asymmetry, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gendered_outcome_asymmetry, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_outcome_asymmetry, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gendered_outcome_asymmetry, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_outcome_asymmetry, snare).
narrative_ontology:human_readable(gendered_outcome_asymmetry, "Gray Divorce Gendered Outcome Asymmetry").
narrative_ontology:topic_domain(gendered_outcome_asymmetry, "social/economic/demographic").

domain_priors:requires_active_enforcement(gendered_outcome_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_outcome_asymmetry, divorcing_men_50_plus).
narrative_ontology:constraint_beneficiary(gendered_outcome_asymmetry, financial_services_industry).
narrative_ontology:constraint_victim(gendered_outcome_asymmetry, divorcing_women_50_plus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_outcome_asymmetry, adult_children).
narrative_ontology:constraint_vindicates(gendered_outcome_asymmetry, marriage_as_economic_partnership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate approximately 60-70% of gray divorces, often after decades of career interruption for caregiving. Face immediate income drops of 40-50%, wealth losses, pension division disadvantages, and substantially lower re-partnering rates than male counterparts. Exit from unsatisfying marriage is available but carries asymmetric financial penalty that compounds over remaining lifespan. The identity-lock operates through internalized expectations about post-divorce economic viability and age-based re-partnering prospects.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, divorcing_women_50_plus, payer,
    moderate, biographical, identity_locked, national).

% Experience smaller income declines post-divorce, retain higher earning capacity due to uninterrupted career trajectories, and re-partner at substantially higher rates. Benefit from accumulated human capital and social capital that was partially subsidized by partner's domestic labor during marriage. Exit carries lower financial penalty and higher probability of forming new household partnerships.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, divorcing_men_50_plus, beneficiary,
    moderate, biographical, mobile, national).

% Captures fees from divorce-related asset division, retirement account splits, new account establishment, and financial planning services for newly single households. The asymmetric outcome pattern generates predictable demand for wealth management services targeting the male beneficiary cohort and debt/survival services targeting the female victim cohort.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, financial_services_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Administer the legal framework that produces the asymmetric outcomes through property division rules, alimony calculations, and pension valuation methods that systematically undervalue unpaid domestic labor and career interruption costs. Present the framework as gender-neutral while its operation produces gendered extraction.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, family_law_practitioners, agenda_setter,
    organized, biographical, mobile, regional).

% Often provide financial support to divorced mothers experiencing income collapse, absorbing costs that the legal framework failed to allocate. Witness the asymmetric outcomes directly and may adjust their own partnership formation decisions in response.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, adult_children, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(gendered_outcome_asymmetry, adult_children, observer).

% Tracks the downstream poverty and food insecurity rates among divorced women 50-plus, which are substantially higher than married or divorced male counterparts. The administrative data reveals the constraint's operation but the agency has no mandate to address the structural asymmetry.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, social_security_administration, observer,
    institutional, generational, analytical, national).

% Document the systematic undervaluation of domestic labor in property division frameworks and advocate for reforms that would account for career interruption costs and human capital subsidies. Their analyses are largely excluded from the legal doctrine that family law practitioners apply.
narrative_ontology:constraint_stakeholder(gendered_outcome_asymmetry, feminist_legal_scholars, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_outcome_asymmetry, divorcing_men_50_plus).
narrative_ontology:fixing_cost_class(gendered_outcome_asymmetry, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for dissolving marriages that have become unsustainable, allowing both parties to exit and form new household arrangements.
% TRANSFER_FUNCTION: Moves wealth, income security, and re-partnering probability from divorcing women 50-plus to divorcing men 50-plus and to financial services intermediaries, through property division rules that systematically undervalue unpaid domestic labor and through labor market structures that penalize career interruption.
% ABSENT_VOICES: Feminist legal scholars and economists who have documented the systematic undervaluation of domestic labor are structurally excluded from the doctrine-setting process. Their reform proposals—imputed income for caregiving years, human capital subsidy accounting, mandatory pension equalization—are not incorporated into the legal frameworks that family law practitioners apply.
% DISAPPEARANCE_RATIONALE: If the asymmetric outcome structure vanished overnight—if property division rules fully accounted for career interruption costs and domestic labor subsidies, and if re-partnering probabilities equalized—the financial penalty for female-initiated gray divorce would compress substantially, divorce initiation patterns might shift, and the downstream poverty and food insecurity rates among divorced women 50-plus would decline. The current pattern of women initiating majority of gray divorces while bearing majority of financial costs would not persist.
% FOUNDING_PROBLEM: Mid-20th century no-fault divorce reforms were built to address the problem of women trapped in abusive or dead marriages by fault-based divorce regimes that required proof of wrongdoing and often left women destitute.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—legal barriers to divorce exit—is solved. What persists is a different extraction mechanism: exit is legally available but carries asymmetric financial penalty. This status is corroborated by demographic researchers outside the legal profession (sociologists, economists studying gray divorce outcomes) whose data show high female initiation rates coupled with severe post-divorce economic outcomes, and by adult children who witness and subsidize their mothers' post-divorce income collapse.
narrative_ontology:disappearance_verdict(gendered_outcome_asymmetry, world_rearranges).
narrative_ontology:founding_problem_status(gendered_outcome_asymmetry, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_outcome_asymmetry, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-24',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(gendered_outcome_asymmetry, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_outcome_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_outcome_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_outcome_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the financial penalty for female-initiated gray divorce substantially exceeds any coordination cost of the legal mechanism—the gap between women's post-divorce income collapse and men's modest decline is not explained by administrative overhead. Suppression is substantial (0.71) because the constraint's persistence depends on maintaining property division rules that undervalue domestic labor and on labor market structures that penalize career interruption; alternatives (reformed valuation methods, mandatory pension equalization) are actively resisted by the legal profession. Theater ratio is moderate (0.42): the gender-neutral framing of property division rules is increasingly recognized as performative given the systematic gendered outcomes, but the legal framework still performs real coordination function in dissolving marriages. Accessibility collapse is moderate (0.48) because alternatives to the current framework are visible and advocated by excluded scholars, but resistance is substantial (0.62) as evidenced by organized advocacy for reform and adult children absorbing costs the legal framework failed to allocate. The measurement series shows extraction accumulation over 35 years as the gray divorce rate increased and the systematic asymmetry became more visible in administrative data.
 *
 * PERSPECTIVAL GAP:
 *   The victim seat and the beneficiary seat should compute very differently. From the divorcing women's position, the constraint operates as a snare: exit is available but carries prohibitive financial penalty that compounds over remaining lifespan. From the divorcing men's position, the same legal framework operates as coordination with modest cost: it dissolves the marriage without imposing the career-interruption penalty they did not bear. The agenda-setter seat (family law practitioners) should compute as rope or tangled_rope: they administer a framework they present as neutral coordination, but the systematic gendered outcomes reveal extraction they benefit from maintaining. The engine computes this divergence from the structural data—the authored claim (snare) represents the victim seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Divorcing women 50-plus are the structural victims: they initiate exit but bear the asymmetric financial penalty through property division rules that undervalue their domestic labor subsidy and through labor markets that penalize their career interruption. Their exit_options are identity_locked because the decision to divorce at 50-plus involves identity fusion with expectations about post-divorce economic viability and age-based re-partnering prospects—the financial penalty is internalized as 'what divorce costs women like me.' Divorcing men 50-plus are beneficiaries: they retain higher earning capacity that was partially subsidized by partner's domestic labor, experience smaller income declines, and re-partner at higher rates. Their exit_options are mobile because the financial penalty is lower and re-partnering probability is higher. Family law practitioners are agenda setters administering the framework; financial services industry captures fees from the asymmetric outcome pattern. The directionality derivation should produce high d (near 1.0) for divorcing women and low d (near 0.2-0.3) for divorcing men, reflecting the structural asymmetry in who bears the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate—providing legal mechanism for dissolving unsustainable marriages—is live and necessary. What has shifted is the function: the founding problem was legal barriers to exit (fault-based divorce trapping women in abusive marriages); that problem is solved. What persists is asymmetric financial penalty for exit, which is a different extraction mechanism. The mandatrophy analysis prevents mislabeling this as pure coordination (it solves a real problem) while recognizing the extraction layered onto that coordination (the systematic undervaluation of domestic labor in property division). The R5 genealogy interview documents this shift: founding_problem_status is 'dead' (legal barriers solved) but the arrangement persists with different function (extracting from female-initiated exits through property division rules that undervalue caregiving).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_labor_valuation_method,
    'What is the economically accurate method for valuing decades of unpaid domestic labor and career interruption costs in property division, and would applying it compress the gendered outcome asymmetry?',
    'Jurisdictional natural experiment: states that adopt imputed income methods for caregiving years or mandatory human capital subsidy accounting in property division, compared to control states. Measure post-divorce income gaps and re-partnering rate differentials before and after reform.',
    'If reformed valuation methods substantially compress the outcome asymmetry, the current extraction is attributable to systematic undervaluation in legal doctrine. If the gap persists, other structural factors (labor market penalties, re-partnering market dynamics) are driving the asymmetry independently of property division rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_labor_valuation_method, empirical, 'Whether reformed property division rules would compress gendered outcome asymmetry').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-locked exit pattern for divorcing women 50-plus primarily driven by internalized expectations about post-divorce economic viability, or by structural barriers (labor market re-entry difficulty, age discrimination in re-partnering market)?',
    'Longitudinal survey data tracking divorce decision-making process: measure stated reasons for delaying or proceeding with divorce, perceived economic consequences, and actual post-divorce outcomes. Distinguish between women who anticipated severe financial penalty and proceeded anyway versus those who delayed due to economic fear.',
    'If identity-lock is primarily internalized expectation, interventions targeting financial literacy and post-divorce planning could shift exit patterns. If it is primarily structural barrier, the suppression is external and requires labor market or property division reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is internalized expectation or structural barrier').

omega_variable(
    re_partnering_asymmetry_driver,
    'Is the re-partnering rate asymmetry (men re-partner at substantially higher rates than women post-gray-divorce) driven by age-based partner preferences in the dating market, by women''s reduced economic attractiveness post-divorce, or by women''s reduced interest in re-partnering after asymmetric domestic labor in first marriage?',
    'Survey data on re-partnering preferences and dating market behavior post-gray-divorce, controlling for income and wealth. Measure whether women with compressed post-divorce income gaps re-partner at rates closer to men, and whether stated preferences differ by post-divorce economic outcome.',
    'If re-partnering asymmetry is driven by economic outcomes, property division reform could compress it. If driven by age-based preferences or reduced interest after first-marriage experience, the asymmetry persists independently of property division rules and represents a separate extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(re_partnering_asymmetry_driver, empirical, 'What drives the re-partnering rate asymmetry post-gray-divorce').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (property division rules, labor market barriers) or internalized (women believe they deserve the financial penalty, or that divorce at 50-plus is inherently costly for women)?',
    'Post-divorce trajectory analysis: if women who divorce despite anticipating severe financial penalty show persistent economic anxiety and reduced well-being even after stabilizing income, suppression is partially internalized. If well-being recovers once income stabilizes, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest—women carry the suppression with them after exit. If structural, interventions targeting property division and labor market re-entry can reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_outcome_asymmetry, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_outcome_asymmetry, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(gend_tr_t0, observed).
narrative_ontology:measurement(gend_tr_t7, gendered_outcome_asymmetry, theater_ratio, 7, 0.32).
narrative_ontology:measurement_basis(gend_tr_t7, observed).
narrative_ontology:measurement(gend_tr_t14, gendered_outcome_asymmetry, theater_ratio, 14, 0.35).
narrative_ontology:measurement_basis(gend_tr_t14, observed).
narrative_ontology:measurement(gend_tr_t21, gendered_outcome_asymmetry, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(gend_tr_t21, observed).
narrative_ontology:measurement(gend_tr_t28, gendered_outcome_asymmetry, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(gend_tr_t28, observed).
narrative_ontology:measurement(gend_tr_t35, gendered_outcome_asymmetry, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(gend_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_outcome_asymmetry, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(gend_be_t0, observed).
narrative_ontology:measurement(gend_be_t7, gendered_outcome_asymmetry, base_extractiveness, 7, 0.67).
narrative_ontology:measurement_basis(gend_be_t7, observed).
narrative_ontology:measurement(gend_be_t14, gendered_outcome_asymmetry, base_extractiveness, 14, 0.71).
narrative_ontology:measurement_basis(gend_be_t14, observed).
narrative_ontology:measurement(gend_be_t21, gendered_outcome_asymmetry, base_extractiveness, 21, 0.75).
narrative_ontology:measurement_basis(gend_be_t21, observed).
narrative_ontology:measurement(gend_be_t28, gendered_outcome_asymmetry, base_extractiveness, 28, 0.77).
narrative_ontology:measurement_basis(gend_be_t28, observed).
narrative_ontology:measurement(gend_be_t35, gendered_outcome_asymmetry, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(gend_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_outcome_asymmetry, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gend_su_t0, observed).
narrative_ontology:measurement(gend_su_t7, gendered_outcome_asymmetry, suppression_requirement, 7, 0.62).
narrative_ontology:measurement_basis(gend_su_t7, observed).
narrative_ontology:measurement(gend_su_t14, gendered_outcome_asymmetry, suppression_requirement, 14, 0.65).
narrative_ontology:measurement_basis(gend_su_t14, observed).
narrative_ontology:measurement(gend_su_t21, gendered_outcome_asymmetry, suppression_requirement, 21, 0.68).
narrative_ontology:measurement_basis(gend_su_t21, observed).
narrative_ontology:measurement(gend_su_t28, gendered_outcome_asymmetry, suppression_requirement, 28, 0.7).
narrative_ontology:measurement_basis(gend_su_t28, observed).
narrative_ontology:measurement(gend_su_t35, gendered_outcome_asymmetry, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(gend_su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(gend_grid_01, gendered_outcome_asymmetry, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement_basis(gend_grid_01, observed).
narrative_ontology:measurement(gend_grid_02, gendered_outcome_asymmetry, accessibility_collapse(class), 35, 0.55).
narrative_ontology:measurement_basis(gend_grid_02, observed).
narrative_ontology:measurement(gend_grid_03, gendered_outcome_asymmetry, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement_basis(gend_grid_03, observed).
narrative_ontology:measurement(gend_grid_04, gendered_outcome_asymmetry, accessibility_collapse(individual), 35, 0.51).
narrative_ontology:measurement_basis(gend_grid_04, observed).
narrative_ontology:measurement(gend_grid_05, gendered_outcome_asymmetry, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement_basis(gend_grid_05, observed).
narrative_ontology:measurement(gend_grid_06, gendered_outcome_asymmetry, accessibility_collapse(organizational), 35, 0.44).
narrative_ontology:measurement_basis(gend_grid_06, observed).
narrative_ontology:measurement(gend_grid_07, gendered_outcome_asymmetry, accessibility_collapse(structural), 0, 0.52).
narrative_ontology:measurement_basis(gend_grid_07, observed).
narrative_ontology:measurement(gend_grid_08, gendered_outcome_asymmetry, accessibility_collapse(structural), 35, 0.58).
narrative_ontology:measurement_basis(gend_grid_08, observed).
narrative_ontology:measurement(gend_grid_09, gendered_outcome_asymmetry, resistance(class), 0, 0.58).
narrative_ontology:measurement_basis(gend_grid_09, observed).
narrative_ontology:measurement(gend_grid_10, gendered_outcome_asymmetry, resistance(class), 35, 0.68).
narrative_ontology:measurement_basis(gend_grid_10, observed).
narrative_ontology:measurement(gend_grid_11, gendered_outcome_asymmetry, resistance(individual), 0, 0.52).
narrative_ontology:measurement_basis(gend_grid_11, observed).
narrative_ontology:measurement(gend_grid_12, gendered_outcome_asymmetry, resistance(individual), 35, 0.61).
narrative_ontology:measurement_basis(gend_grid_12, observed).
narrative_ontology:measurement(gend_grid_13, gendered_outcome_asymmetry, resistance(organizational), 0, 0.64).
narrative_ontology:measurement_basis(gend_grid_13, observed).
narrative_ontology:measurement(gend_grid_14, gendered_outcome_asymmetry, resistance(organizational), 35, 0.72).
narrative_ontology:measurement_basis(gend_grid_14, observed).
narrative_ontology:measurement(gend_grid_15, gendered_outcome_asymmetry, resistance(structural), 0, 0.48).
narrative_ontology:measurement_basis(gend_grid_15, observed).
narrative_ontology:measurement(gend_grid_16, gendered_outcome_asymmetry, resistance(structural), 35, 0.55).
narrative_ontology:measurement_basis(gend_grid_16, observed).
narrative_ontology:measurement(gend_grid_17, gendered_outcome_asymmetry, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement_basis(gend_grid_17, observed).
narrative_ontology:measurement(gend_grid_18, gendered_outcome_asymmetry, stakes_inflation(class), 35, 0.68).
narrative_ontology:measurement_basis(gend_grid_18, observed).
narrative_ontology:measurement(gend_grid_19, gendered_outcome_asymmetry, stakes_inflation(individual), 0, 0.64).
narrative_ontology:measurement_basis(gend_grid_19, observed).
narrative_ontology:measurement(gend_grid_20, gendered_outcome_asymmetry, stakes_inflation(individual), 35, 0.73).
narrative_ontology:measurement_basis(gend_grid_20, observed).
narrative_ontology:measurement(gend_grid_21, gendered_outcome_asymmetry, stakes_inflation(organizational), 0, 0.45).
narrative_ontology:measurement_basis(gend_grid_21, observed).
narrative_ontology:measurement(gend_grid_22, gendered_outcome_asymmetry, stakes_inflation(organizational), 35, 0.52).
narrative_ontology:measurement_basis(gend_grid_22, observed).
narrative_ontology:measurement(gend_grid_23, gendered_outcome_asymmetry, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement_basis(gend_grid_23, observed).
narrative_ontology:measurement(gend_grid_24, gendered_outcome_asymmetry, stakes_inflation(structural), 35, 0.71).
narrative_ontology:measurement_basis(gend_grid_24, observed).
narrative_ontology:measurement(gend_grid_25, gendered_outcome_asymmetry, suppression(class), 0, 0.62).
narrative_ontology:measurement_basis(gend_grid_25, observed).
narrative_ontology:measurement(gend_grid_26, gendered_outcome_asymmetry, suppression(class), 35, 0.71).
narrative_ontology:measurement_basis(gend_grid_26, observed).
narrative_ontology:measurement(gend_grid_27, gendered_outcome_asymmetry, suppression(individual), 0, 0.68).
narrative_ontology:measurement_basis(gend_grid_27, observed).
narrative_ontology:measurement(gend_grid_28, gendered_outcome_asymmetry, suppression(individual), 35, 0.76).
narrative_ontology:measurement_basis(gend_grid_28, observed).
narrative_ontology:measurement(gend_grid_29, gendered_outcome_asymmetry, suppression(organizational), 0, 0.48).
narrative_ontology:measurement_basis(gend_grid_29, observed).
narrative_ontology:measurement(gend_grid_30, gendered_outcome_asymmetry, suppression(organizational), 35, 0.55).
narrative_ontology:measurement_basis(gend_grid_30, observed).
narrative_ontology:measurement(gend_grid_31, gendered_outcome_asymmetry, suppression(structural), 0, 0.58).
narrative_ontology:measurement_basis(gend_grid_31, observed).
narrative_ontology:measurement(gend_grid_32, gendered_outcome_asymmetry, suppression(structural), 35, 0.66).
narrative_ontology:measurement_basis(gend_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_outcome_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(gendered_outcome_asymmetry, womens_financial_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is downstream of womens_financial_autonomy (rope): women's formal legal right to own property and earn income independently created the possibility of divorce exit, but the property division rules that govern that exit systematically undervalue the domestic labor subsidy women provided during marriage. The upstream rope (financial autonomy) enables exit; this constraint (gendered outcome asymmetry) extracts from that exit through valuation rules that treat decades of caregiving as economically invisible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
