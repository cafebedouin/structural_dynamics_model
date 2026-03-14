% ============================================================================
% CONSTRAINT STORY: basic_law_supremacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_supremacy, []).

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
 *   constraint_id: basic_law_supremacy
 *   human_readable: Basic Law Supremacy and Constitutional Entrenchment
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Basic law supremacy establishes that certain constitutional provisions
 *   are more difficult to amend than ordinary legislation, typically through
 *   supermajority requirements (2/3, 3/5, or higher) or unamendable
 *   provisions. This constraint exhibits a fundamental tension: protecting
 *   rights and constitutional stability from casual majoritarian revision
 *   versus disempowering contemporary majorities from changing foundational
 *   rules they regard as unjust or obsolete. The constraint generates six
 *   distinct perspectival classifications reflecting how different agents
 *   experience the entrenchment mechanism. Constitutional framers and
 *   judicial gatekeepers experience it as protective coordination —
 *   preserving the settlement they negotiated. Electoral majorities
 *   experience it as a snare — their democratic will is suspended on core
 *   questions. Future legislatures experience it as mixed
 *   coordination-extraction — genuine protection of rights across time, but
 *   also genuine disempowerment. Reform coalitions experience it as a
 *   temporary scaffold with a sunset clause — rigidity produces periodic
 *   legitimacy crises that eventually force constitutional revision. The
 *   institutional view (piton) observes that formal entrenchment is often
 *   undermined by informal amendment through reinterpretation, making the
 *   theater ratio significant. The analytical observer (mountain) risks
 *   naturalizing the *particular choice* to entrench *particular content* as
 *   a logical necessity rather than a contingent institutional design.
 *
 * KEY AGENTS:
 *   - Constitutional Framers: Primary beneficiary (institutional/arbitrage) — lock in their values and institutional design through entrenchment; experience basic law as protecting the constitutional settlement
 *   - Judicial Gatekeepers: Secondary beneficiary (institutional/arbitrage) — gain structural authority through constitutional supremacy; serve as arbiters of basic law interpretation
 *   - Electoral Majority: Primary victim (powerless/trapped) — structurally unable to change foundational rules through ordinary democracy; subject to supermajority requirements that exceed realistic coalition thresholds
 *   - Future Legislatures: Secondary victim (moderate/constrained) — cannot unilaterally override basic law; constrained by entrenchment but possess some agency through interpretation and incremental amendment
 *   - Reform Coalition: Organized agents (organized/constrained) — seek constitutional change through legitimate pathways (conventions, supermajority coalitions); perceive entrenchment as temporary bottleneck with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_supremacy, 0.58).
domain_priors:suppression_score(basic_law_supremacy, 0.68).
domain_priors:theater_ratio(basic_law_supremacy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_supremacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_supremacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(basic_law_supremacy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_supremacy, tangled_rope).
narrative_ontology:human_readable(basic_law_supremacy, "Basic Law Supremacy and Constitutional Entrenchment").
narrative_ontology:topic_domain(basic_law_supremacy, "political/constitutional").

domain_priors:requires_active_enforcement(basic_law_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_supremacy, constitutional_framers).
narrative_ontology:constraint_beneficiary(basic_law_supremacy, judicial_gatekeepers).
narrative_ontology:constraint_victim(basic_law_supremacy, electoral_majority).
narrative_ontology:constraint_victim(basic_law_supremacy, future_legislatures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTORAL MAJORITY (SNARE) — Current voters are structurally trapped by constitutional entrenchment. Cannot change foundational rules through ordinary democratic process. Supermajority requirements (often 2/3 or 3/5) exceed any realistic coalition threshold. No exit available — must accept the framework imposed by prior generations. Maximum experienced extraction — democracy is suspended on core constitutional questions.
constraint_indexing:constraint_classification(basic_law_supremacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE LEGISLATURES (TANGLED ROPE) — Constrained by entrenchment but possess some agency through constitutional interpretation, judicial appointments, and incremental amendments. Benefit from constitutional stability and preserved rights; also bear the cost of rigidity. Can coordinate legislative action but cannot unilaterally override basic law. Mixed experience — genuine coordination function (protecting rights across time) with asymmetric extraction (disempowerment on structural change).
constraint_indexing:constraint_classification(basic_law_supremacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL FRAMERS (ROPE) — Primary beneficiaries. Establish the framework that locks in their values and institutional design. Experience basic law as coordination: preserving the constitutional settlement they negotiated. Low extraction cost — they designed the constraint. Can arbitrage across constitutional amendment procedures (proposing amendments that benefit their faction). Net beneficiary of the entrenchment.
constraint_indexing:constraint_classification(basic_law_supremacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL GATEKEEPERS (ROPE) — Courts gain structural authority through constitutional supremacy. Serve as arbiters of basic law interpretation. Benefit from expanded institutional role and veto power over legislation. Experience the constraint as coordination: preserving judicial independence and constitutional interpretation authority. Low extraction relative to institutional power — gatekeeping role is by design.
constraint_indexing:constraint_classification(basic_law_supremacy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents seeking structural constitutional change perceive a temporary bottleneck with potential sunset. Constitutional conventions, citizen assemblies, and supermajority coalitions represent pathways to amendment. The constraint operates with sunset logic: if legitimacy crisis deepens, constitutional revision becomes politically inevitable. High organizing difficulty but not impossible — multiple democracies have successfully amended even rigid constitutions.
constraint_indexing:constraint_classification(basic_law_supremacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL INERTIA (PITON) — From a long-term comparative view, basic law supremacy often becomes ritualistic. Court deference to electoral pressure, informal amendment through interpretation, and constitutional reinterpretation undermine the theoretical rigidity. The entrenchment persists through institutional muscle memory — honored in form but widely violated in practice. Theater ratio reflects that constitutional supremacy doctrine is often performed more than enforced.
constraint_indexing:constraint_classification(basic_law_supremacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL CONSTRAINT (MOUNTAIN) — At civilizational/universal scope, basic law supremacy appears as a logical necessity: any legal system requires a foundational rule that is not itself subject to lower-order change. The supremacy is inherent to the concept of law itself — you cannot have a legal system where the constitution is subordinate to legislation. However, this naturalizes the *particular choice* to entrench *particular content* (specific rights, specific institutions) at the basic law level. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(basic_law_supremacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_supremacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_supremacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_supremacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_supremacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(basic_law_supremacy, TR),
    TR >= 0.70.

:- end_tests(basic_law_supremacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The initial measurement (0.35) reflects that early-stage constitutional systems use entrenchment primarily as protective mechanism with limited perceived extraction. Over time, extractiveness increases as the constraint's disempowering effect accumulates — majorities that cannot amend foundational rules they reject experience the entrenchment as increasingly extractive. The rise to 0.58 reflects genuine temporal drift from protection toward extraction as rigidity encounters generational value change. Suppression (0.68): High. Multiple barriers to constitutional amendment exist: supermajority requirements are often impossible for any coalition to achieve; unamendable provisions eliminate exit entirely; veto players (second chambers, regional governments, courts) multiply decision points. Voters cannot exit the constitutional framework without leaving the nation. Theater ratio (0.55): Moderate. Constitutional supremacy doctrine is partly real (courts do strike down legislation) and partly performative (reinterpretation undermines formal rigidity; informal amendment via doctrine drift is common). The theater ratio increases over time as formal amendment becomes sufficiently difficult that informal reinterpretation becomes the primary mechanism of constitutional change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the framework's core diagnostic: structural arrangements that appear as coordination from beneficiary perspectives appear as extraction from victim perspectives. The framers' 'protection' is the majority's 'disempowerment.' The judicial gatekeeper's 'constitutional authority' is the legislature's 'structural veto.' The gap reveals that entrenchment conflates two distinct coordination problems: (1) protecting rights and constitutional structure from erosion over time (genuine coordination function) and (2) locking in the framers' specific values and institutional design against future challenge (extraction mechanism). These are not the same thing. A constraint that solved (1) without (2) would be rope. A constraint that imposed only (2) would be snare. The tangled rope classification reflects that basic law supremacy genuinely does both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation in this constraint reveals institutional asymmetry despite shared national scope. Constitutional framers and judicial gatekeepers occupy beneficiary positions with arbitrage exit options — they can propose amendments favorable to their interests and possess institutional authority to interpret supremacy doctrine. This produces low d values and negative/minimal effective extraction (chi). Electoral majorities occupy victim positions with trapped exit options — they can neither exit the constitutional framework nor change it through ordinary democracy. This produces high d values and high effective extraction (chi). Future legislatures occupy intermediate victim positions with constrained exit options — they face real barriers to constitutional change but possess some agency through interpretation and supermajority coalitions. This produces moderate-high d and moderate chi. The scaffold perspective (reform coalition) occupies an organized/constrained position that moderates the d value — they have agency through collective action, reducing experienced extraction relative to isolated powerless agents. The directionality divergence between beneficiaries (d ≈ 0.15) and trapped victims (d ≈ 0.95) produces the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that basic law supremacy is not purely extractive (snare) disguised as protective (mountain/rope). The constraint has genuine coordination content: constitutional stability across generations, protection of fundamental rights, predictability of legal structure. Electoral majorities do benefit from these protection mechanisms, even if they also experience disempowerment. The snare classification at the powerless/trapped perspective is not the 'true' classification — it is the victim's structural experience. The rope classification from the beneficiary perspective is not false — it is the beneficiary's structural experience. The tangled rope is the medium perspective that shows both effects simultaneously. The mandatrophy is resolved by accepting that (a) the constraint is genuinely mixed, not pure extraction, and (b) the distributional asymmetry between framers/judges and majorities/future legislatures is real and structural, not illusory. The constraint solves a genuine coordination problem (constitutional stability) while simultaneously extracting from those who disagree with the entrenched content (disempowerment of future majorities). Both mechanisms are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_depth_threshold,
    'At what depth of entrenchment does constitutional rigidity transition from protective (preserving rights) to extractive (disempowering democratic change)?',
    'Comparative constitutional analysis: correlation between entrenchment depth and constitutional amendment frequency; measurement of legitimacy crises in rigid vs flexible systems',
    'If threshold is shallow (< 2/3 supermajority): most basic law supremacy classified as extractive snare. If threshold is deep (> 4/5 supermajority): majority of entrenchment seen as coordination. Current extractiveness (0.58) assumes mid-depth; threshold determination would shift classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_depth_threshold, empirical, 'Entrenchment depth at which rigidity becomes extractive').

omega_variable(
    judicial_independence_extraction,
    'Does judicial gatekeeping over basic law represent genuine separation of powers (coordination) or institutional self-dealing (extraction benefiting courts)?',
    'Historical analysis of court decisions overturning constitutional amendments; measurement of temporal drift in judicial deference to electoral majorities; comparison of court power growth relative to legislative/executive institutional change',
    'If genuine separation: judicial beneficiary status is justified coordination incentive. If self-dealing: judicial gatekeeper role shifts from rope/arbitrage to snare/trapped relationship relative to democratic sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_independence_extraction, empirical, 'Whether judicial gatekeeping extracts from democratic process').

omega_variable(
    constitutional_amendment_feasibility,
    'Is the amplitude of constitutional amendment requirements (2/3, 3/5, 4/5 supermajorities) empirically calibrated to genuine cross-party constitutional consensus or does it systematically exclude legitimate majorities?',
    'Longitudinal study of failed amendment attempts: what proportion failed despite having simple majority support; analysis of which coalitions can meet entrenchment threshold vs which cannot; measurement of amendment success rates across different entrenchment depths',
    'If calibrated to consensus: entrenchment requirement is protective (rope/mountain). If systematically exclusive: requirement is extractive (snare/tangled rope from majority perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_feasibility, empirical, 'Whether amendment requirements match empirical consensus thresholds').

omega_variable(
    unamendable_provision_legitimacy,
    'Can entrenchment of unamendable provisions (eternity clauses) be justified through coordination logic, or does it represent pure institutional lock-in by framers?',
    'Comparative analysis: legitimacy levels in systems with vs without eternity clauses; frequency of perceived constitutional crisis; measurement of elite-mass opinion divergence on unamendable provisions',
    'If justified: unamendable provisions protect against tyranny of majority (rope classification upheld). If lock-in: classification shifts toward snare for any majority seeking to change entrenched content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unamendable_provision_legitimacy, conceptual, 'Legitimacy of unamendable constitutional provisions').

omega_variable(
    informal_amendment_escape_valve,
    'Does constitutional interpretation (living constitutionalism, purposivist reinterpretation) provide a sufficient escape valve for rigid entrenchment, converting snare into tangled rope?',
    'Measurement of constitutional doctrine drift: how much have courts reinterpreted foundational provisions without formal amendment; comparison of formal amendment requirement vs effective substantive change frequency',
    'If sufficient escape valve: entrenchment is more rope-like than snare-like (constrained rather than trapped). If inadequate: entrenchment is snare from majority perspective, piton from institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_amendment_escape_valve, empirical, 'Whether interpretation provides escape from rigid entrenchment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_supremacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_supremacy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(basi_tr_t5, basic_law_supremacy, theater_ratio, 5, 0.42).
narrative_ontology:measurement(basi_tr_t10, basic_law_supremacy, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_supremacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(basi_be_t5, basic_law_supremacy, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(basi_be_t10, basic_law_supremacy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_supremacy, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_supremacy, majoritarian_supermajority_tradeoff).
narrative_ontology:affects_constraint(basic_law_supremacy, judicial_review_constitutional_limits).
narrative_ontology:affects_constraint(basic_law_supremacy, amendment_procedure_veto_players).

% DUAL FORMULATION NOTE:
% Basic law supremacy is upstream of specific amendment procedures and judicial review authority. The supremacy doctrine establishes that certain provisions are harder to change; separate constraints model specific amendment requirements (2/3 vs 3/5 thresholds), unamendable provisions (eternity clauses), and judicial authority to strike down legislation. These are distinct constraints with their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
