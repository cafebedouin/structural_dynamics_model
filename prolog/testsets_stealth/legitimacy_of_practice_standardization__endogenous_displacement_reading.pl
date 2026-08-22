% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous-Displacement Legitimacy Standard for Practice Change
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   This story authors the endogenous-displacement reading of the
 *   practice-standardization legitimacy kernel: the governing norm that
 *   calendar, dress, script, and administrative-practice changes are
 *   legitimate only when they emerge from voluntary uptake driven by
 *   perceived utility or cultural evolution. The standing arrangement under
 *   contest — and the ε referent — is the voluntariness test as it is
 *   actually enforced: historiographic sanction of imposed reforms, donor
 *   conditionality requiring demonstrated uptake, and the scholarly canon
 *   that grades transitions as organic or decreed. Per the ε-invariance
 *   principle, the colloquial label 'legitimacy of practice standardization'
 *   decomposes into three structurally distinct constraints (this reading,
 *   the exogenous override reading, the dual-practice equilibrium reading),
 *   each with its own ε, victim set, and classification; they are linked as a
 *   constraint family through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - traditional_practice_communities: Primary
 *   protected beneficiary (moderate/constrained) — practice continuity
 *   shielded from decree - incumbent_traditional_elites: Beneficiary with
 *   enforcement hand (moderate/identity_locked) — certify what counts as
 *   voluntary - development_finance_institutions: Enforcing beneficiary
 *   (institutional/arbitrage) — conditionality leverages the test -
 *   modernization_theorists: Agenda-setting evaluator
 *   (institutional/analytical) — operationalize and police the test -
 *   central_modernizing_states: Primary payer (institutional/constrained) —
 *   bear persuasion costs and legitimacy denial -
 *   transition_period_populations: Diffuse payer (powerless/trapped) — carry
 *   decades of dual-calendar, dual-dress burden - early_voluntary_adopters:
 *   Dual-positioned movers (moderate/mobile) — blessed by the test, pay
 *   first-mover social costs - women_subject_to_dress_contests: Excluded seat
 *   (powerless/trapped) — the contested object, absent from the fora
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.4).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous-Displacement Legitimacy Standard for Practice Change").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '923a7df9-9752-4a47-8914-4bd8029998f2').
narrative_ontology:cs_kernel_codification('923a7df9-9752-4a47-8914-4bd8029998f2', distributed).
narrative_ontology:cs_authority_grounding('923a7df9-9752-4a47-8914-4bd8029998f2', expertise).
narrative_ontology:cs_interpretation_layer_present('923a7df9-9752-4a47-8914-4bd8029998f2').
narrative_ontology:cs_reading_relation('923a7df9-9752-4a47-8914-4bd8029998f2', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('923a7df9-9752-4a47-8914-4bd8029998f2', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('923a7df9-9752-4a47-8914-4bd8029998f2', foundational, voluntary_uptake_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_uptake_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('923a7df9-9752-4a47-8914-4bd8029998f2', voluntary_uptake_confers_legitimacy, deontological).
narrative_ontology:cs_axiom('923a7df9-9752-4a47-8914-4bd8029998f2', secondary, imposed_standardization_breeds_hidden_resistance).
narrative_ontology:cs_axiom_status(imposed_standardization_breeds_hidden_resistance, holdable).
narrative_ontology:cs_axiom_grounding('923a7df9-9752-4a47-8914-4bd8029998f2', imposed_standardization_breeds_hidden_resistance, empirically_contingent).
narrative_ontology:cs_reference_frame('923a7df9-9752-4a47-8914-4bd8029998f2', autonomous_diffusion_as_default_legitimacy).
narrative_ontology:cs_drift_state('923a7df9-9752-4a47-8914-4bd8029998f2', contemporary_authoritarian_developmentalism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('923a7df9-9752-4a47-8914-4bd8029998f2', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_traditional_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_finance_institutions).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, central_modernizing_states).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, transition_period_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_voluntary_adopters).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_voluntary_adopters).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, gradual_diffusion_model).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, revealed_preference_legitimacy_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities whose calendars, dress codes, inheritance customs, and ritual schedules predate the modernizing state. The standard shields them: outsiders cannot legitimately replace their practices, only wait and offer alternatives they may accept. What reaches them is persuasion, subsidy offers, and example; what they keep is the choice to decline. Declining has costs — roads, schools, and markets increasingly run on the newcomers' calendar — but the choice formally remains theirs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_communities, beneficiary,
    moderate, generational, constrained, regional).

% Clerics, elders, guild heads, and customary-office holders whose standing depends on the practices everyone else is being asked to change. They speak for their communities in disputes over change, certify which adoptions are genuinely wanted, and can rally refusal when a proposal threatens their office. Their authority would shrink sharply if the practices they anchor were displaced, so they hold a standing interest in how 'voluntary' is judged.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_traditional_elites, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_traditional_elites, agenda_setter).

% Lenders and aid agencies that attach conditions to funds: governance reforms, legal harmonization, and statistical modernization must show uptake rather than decree. They gain leverage and insulation from backlash accusations by requiring evidence of local buy-in, and they shift criteria and portfolios when a borrower defies the test, which makes their commitment to it strategic rather than absolute.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, development_finance_institutions, beneficiary).

% Scholars and policy analysts who operationalize the test: they define what counts as an adoption curve, grade transitions as organic or imposed, train the next cohort of evaluators, and referee the journals where the standard is defended or attacked. Their careers and curricula are built on the framework; anomalies such as staged consultations are absorbed as measurement problems rather than verdicts on the test itself.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, agenda_setter,
    institutional, civilizational, analytical, global).

% Governments trying to unify calendars, weights, scripts, dress norms, or administrative procedure across the territory they govern. Under the standard they may persuade, subsidize, and demonstrate but not decree; when they legislate anyway they forfeit legitimacy, credit lines, and scholarly endorsement. Their alternatives are slow consensus-building, open defiance at a price, or quiet hybrid decrees dressed as facilitation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, central_modernizing_states, payer,
    institutional, biographical, constrained, national).

% Households living through long diffusions: keeping two calendars (one for taxes and school, one for festivals), owning two wardrobes, learning a new script while elders read the old one. Because change must arrive gradually, they carry the double burden for decades, and no forum of the standard's enforcement asks them whether the pace suits them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, transition_period_populations, payer,
    powerless, immediate, trapped, regional).

% Migrants, students, and younger townspeople who take up the new calendar, dress, or script ahead of their neighbors because it pays — jobs, schooling, mobility. The standard blesses their choice as the very thing that legitimates change, yet they pay the social price of moving first: ridicule at home, awkwardness in both systems, and the unpaid work of demonstrating the new practice to skeptics.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_voluntary_adopters, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_voluntary_adopters, beneficiary).

% Women whose clothing and bodily practice are the explicit object of standardization fights — targeted by unveiling or veiling campaigns, dress codes, and their reversals. Neither the modernizers nor the defenders of custom put their preferences at the center; both sides claim to know what women would choose freely. They rarely hold seats in the ministries, journals, or loan boards where the standard is applied.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, women_subject_to_dress_contests, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, incumbent_traditional_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives states, communities, funders, and historians a shared test for sorting legitimate from imposed change, so each side can predict how a proposal will be judged without case-by-case bargaining, and minority practices receive a default protection while persuasion proceeds.
% TRANSFER_FUNCTION: Moves decision rights over practice change from central authorities to adoption sequences and incumbent practice-holders; moves the costs of transition onto households living through extended dual-practice periods; moves legitimacy, creditworthiness, and scholarly endorsement toward changes that can display gradual uptake.
% ABSENT_VOICES: Transition-period populations and women whose dress is contested live the standard's costs but hold no agenda-setting seat anywhere: the test is applied in ministries, loan boards, and journals populated by states, elites, donors, and theorists, and their 'voluntariness' is routinely attested second-hand by the very elites whose office depends on the answer.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, states would resume decreeing calendar, script, and dress changes without legitimacy penalty, communities would lose their default shield and bargain case-by-case from weakness, donors would need an entirely new conditionality vocabulary, and historiography would reorganize around a different test of which reforms counted. Adoption behavior itself would not change instantly, but the entire governance of change would.
% FOUNDING_PROBLEM: The nineteenth- and early-twentieth-century record of imposed standardization — revolutionary calendars, forced dress laws, language decrees — produced hidden non-compliance, backlash, and occasional violence. The standard was articulated to separate reforms that endure because people want them from reforms that survive only under continuous compulsion.
% FOUNDING_PROBLEM_CORROBORATION: Historical and anthropological scholarship outside the beneficiary set documents the pattern: studies of failed top-down schemes and everyday resistance (the 'seeing like a state' literature), archival records of clandestine old-calendar observance and tax strikes after imposed reforms. Some development economists dispute how general the backlash pattern is, which keeps the corroboration contested rather than unanimous — but no corroborator stands inside the elite or donor beneficiary set.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) because the standard genuinely protects communities from coercive homogenization while simultaneously transferring decision rights to incumbent elites and shifting transition costs onto households — the protection and the cost-shift ride the same structure. Suppression (0.40) is a raw structural property, unscaled by power or scope: the standard does not coerce practice-holders at all; its coercive force targets would-be imposers through conditionality, legitimacy denial, and career sanction, all of which leave the target alternatives (defiance at a price). Accessibility collapse is low (0.35): the sibling readings remain live, states can defy the test, hybrid strategies abound. Resistance is moderate (0.50): developmentalist governments and heterodox economists actively contest the test while most actors comply. Theater (0.38) reflects a growing share of performative activity — staged consultations, subsidized uptake presented as spontaneous demand, adoption curves curated for lenders — alongside a real measurement function. The suppression mechanism is predominantly structural (roughly 70%: funding gates, publication gates, diplomatic cost) with an internalized layer (roughly 30%: evaluators trained in the paradigm experience imposed reform as self-evidently illegitimate, a disposition that would persist after the enforcement machinery weakened). The measurement series run on one shared time grid (T=0..100, six points, all three metrics at every point). The trajectories show enforcement maturing through the decolonization and donor eras, peaking late, then easing slightly at interval end as authoritarian developmentalism openly defies the test — the small terminal dips in extractiveness and suppression are enforcement decay, not satisfaction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the community and elite seats the standard is a protective charter — the reason no one can legislate their festival calendar away. From the central-state seat it is a gag rule on effective governance: the state sees the same structure as a tax on its capacity, payable in persuasion budgets and forgone speed. From the transition-household seat it is a prolonged double burden nobody asked them to price. From the theorist seat it is a neutral evidentiary instrument. The engine computes these per-seat classifications from the structural data; the divergence between the protective experience and the extracting experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional practice communities sit near the beneficiary pole (d low): the standard subsidizes them with protection at little cost beyond transition friction they would face under any regime. Incumbent elites sit near-beneficiary but slightly inward: they collect veto power but expend enforcement labor certifying voluntariness. Development finance institutions derive low-to-moderate d — they collect leverage but pay monitoring costs. Central modernizing states derive high d: they bear the persuasion subsidies, the forgone speed, and the legitimacy penalty, with only constrained exit (defiance is priced, not impossible). Transition-period populations sit nearest the full-target pole: powerless, trapped between both systems, bearing the standard's cost-shifting directly. Early voluntary adopters are genuinely dual-positioned — the test validates them (pulling d down) while first-mover social costs push d up. Women subject to dress contests are excluded rather than coordinated: the standard speaks about their choices through other mouths, which is itself part of the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — backlash and hidden resistance against imposed standardization — remains live, so the mandate is intact and mandatrophy is not resolved. The tangled_rope classification guards against both mislabels: reading the standard as pure rope ignores the measurable asymmetry (elite veto capture, cost-shifting onto trapped households, donor leverage) that requires active enforcement to hold; reading it as pure snare erases the genuine coordination function (a shared legitimacy test, default protection for minority practice) that explains why pluralist actors defend it. The theater series is the early-warning line: if enforcement capacity continues to decay while performative certification grows, the structure drifts toward inertial maintenance — the terminal plateau in extractiveness alongside rising theater is the signature to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the kernel legitimacy_of_practice_standardization; would the sibling readings (exogenous_override_reading, dual_practice_equilibrium_reading) assign different victim sets and different epsilon to the same modernization episodes?',
    'Compile the sibling stories and compare computed per-seat classifications on shared episodes (Atatürk dress law, Soviet calendar reform, Meiji time unification, French revolutionary calendar).',
    'Under the exogenous reading the same episodes flip from illegitimate to legitimate and the victim set shrinks to diehard resisters; under the dual reading the victim set partitions by domain. Classification of any episode is indexical to the reading chosen — the kernel alone decides nothing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading choice determines victim set and epsilon; the kernel under-determines them.').

omega_variable(
    voluntariness_observability_problem,
    'Voluntary adoption can only be inferred from uptake curves and stated satisfaction, both of which can be manufactured through subsidy, signaling cascades, and staged consultation — how much measured endogeneity is real consent?',
    'Instrumented comparisons: adoption under randomized subsidy withdrawal, panel data on private practice after public conformity pressure lifts, audits of consultation processes behind ''participatory'' reforms.',
    'If a large share of ''voluntary'' uptake is engineered, the standard''s enforcement rewards performance over consent; theater_ratio understates the rot and the structure drifts toward theatrical maintenance of a consent fiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_observability_problem, empirical, 'Whether observable adoption curves track consent or manufactured appearance.').

omega_variable(
    elite_veto_entrenchment_ambiguity,
    'Does the standard protect community voice, or entrench incumbent elites who monopolize the certification of what communities want?',
    'Compare episodes where surveyable household preferences diverge from elite-declared refusal; track whether change follows expressed preference or office-holder interest.',
    'If entrenchment dominates, the beneficiary declaration overstates community protection and understates elite capture; effective extraction on states and transition households rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_veto_entrenchment_ambiguity, empirical, 'Community-protection versus incumbent-elite-capture reading of who the standard serves.').

omega_variable(
    imposed_reform_success_counterexamples,
    'Do durable imposed standardizations (Turkish script and dress law, Japanese calendar-time unification, French decimalization) refute the reading''s empirical premise that imposed change breeds hidden resistance and decay?',
    'Systematic survival analysis of imposed versus endogenous standardizations controlling for enforcement duration, initial fit with utility, and follow-up coercion.',
    'A robust counterexample class deepens the reading''s axiom-overriding drift from substantial toward severe and pressures the foundational axiom; a thin one confirms the premise and stabilizes the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imposed_reform_success_counterexamples, empirical, 'Whether successful imposed reforms are a refuting class or exceptions explained by enforcement duration.').

omega_variable(
    double_life_persistence_question,
    'The reading predicts dual practice is a transitional phase that converges; if regional variation persists for generations without convergence, is the double-life burden on households a permanent feature rather than a temporary price of consent?',
    'Long-run panels on calendar, dress, and script pluralism within single jurisdictions; measure convergence rates by region and cohort.',
    'Permanent dual practice would convert the standard''s cost-shifting onto transition populations into a steady-state transfer rather than a transitional cost, raising effective extraction on the payer seats and weakening the reading''s own justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_life_persistence_question, empirical, 'Whether the predicted transitional phase of dual practice actually terminates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(legi_tr_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(legi_tr_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(legi_be_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 80, 0.49).
narrative_ontology:measurement(legi_be_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(legi_su_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement(legi_su_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimacy of practice standardization' decomposes into three structurally distinct constraints per the ε-invariance principle — this endogenous-displacement reading, the exogenous override reading, and the dual-practice equilibrium reading. Each carries its own ε, beneficiaries, victims, and classification; none hedges across readings. Edges run from this story to both siblings because the diffusion literature canonized under this reading supplies the evidentiary baseline the other two argue against: the exogenous reading cites its successes as counterexamples, and the dual reading cites its blind spots as reasons for partition. Changing the observable (whose consent counts, which domains are governed by whom) yields a different constraint, not a different measurement of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
