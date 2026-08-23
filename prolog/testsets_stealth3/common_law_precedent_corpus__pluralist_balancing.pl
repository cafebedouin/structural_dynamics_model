% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Pluralist Balancing of Precedent Weight (Case-by-Case Stability-Adaptation Equilibrium)
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   In common-law systems the accumulated corpus of judicial decisions does
 *   not bind at a single fixed weight. Under the arrangement this story
 *   describes, each court weighs the relevant lines of prior decision against
 *   the domain, the stakes, the maturity of the doctrine, and the fit to the
 *   case at hand, and the resulting assignment of weight is itself revisable
 *   by the next court. The practice solves a real problem — a corpus that
 *   bound absolutely would entrench past errors, and one that bound loosely
 *   would guide no one — but the burden of its variability falls unevenly:
 *   participants who litigate once absorb unpredictability they cannot price,
 *   participants who litigate continually convert the same variability into
 *   positioning advantage, and the bench that arbitrates weight accumulates
 *   discretion and interpretive authority. Enforcement is normative and
 *   hierarchical rather than coercive: collegial expectation, reversal risk,
 *   and career incentive do the work a sanction schedule would do elsewhere.
 *   KEY AGENTS (by structural relationship): - appellate_judiciary:
 *   Agenda-setting seat (institutional/identity_locked) — assigns weight case
 *   by case, collects discretion and interpretive authority -
 *   institutional_repeat_litigants: Primary beneficiary (powerful/arbitrage)
 *   — converts weight variance into durable competitive advantage -
 *   one_time_litigants: Primary payer (moderate/constrained) — absorbs
 *   unpredictability costs in a single dispute - lower_court_judges: Payer
 *   (moderate/identity_locked) — bears reversal risk from guessing weight
 *   assignments made above them - deterred_prospective_claimants: Excluded
 *   payer (powerless/trapped) — priced out of the forum before entry -
 *   legal_commentary_establishment: Secondary beneficiary (organized/mobile)
 *   — synthesizes contested weight into salable authority -
 *   legislative_reformers: Observer (institutional/analytical) — monitors
 *   instability, intermittently codifies around it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.38).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Balancing of Precedent Weight (Case-by-Case Stability-Adaptation Equilibrium)").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '36edf196-6112-4f00-8601-0bf4fc49efe3').
narrative_ontology:cs_kernel_codification('36edf196-6112-4f00-8601-0bf4fc49efe3', formalized).
narrative_ontology:cs_authority_grounding('36edf196-6112-4f00-8601-0bf4fc49efe3', practice).
narrative_ontology:cs_interpretation_layer_present('36edf196-6112-4f00-8601-0bf4fc49efe3').
narrative_ontology:cs_reading_relation('36edf196-6112-4f00-8601-0bf4fc49efe3', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('36edf196-6112-4f00-8601-0bf4fc49efe3', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('36edf196-6112-4f00-8601-0bf4fc49efe3', foundational, precedent_weight_is_a_situated_judgment).
narrative_ontology:cs_axiom_status(precedent_weight_is_a_situated_judgment, holdable).
narrative_ontology:cs_axiom_grounding('36edf196-6112-4f00-8601-0bf4fc49efe3', precedent_weight_is_a_situated_judgment, instrumental).
narrative_ontology:cs_axiom('36edf196-6112-4f00-8601-0bf4fc49efe3', secondary, no_general_rule_fixes_stability_adaptation_balance).
narrative_ontology:cs_axiom_status(no_general_rule_fixes_stability_adaptation_balance, holdable).
narrative_ontology:cs_axiom_grounding('36edf196-6112-4f00-8601-0bf4fc49efe3', no_general_rule_fixes_stability_adaptation_balance, instrumental).
narrative_ontology:cs_reference_frame('36edf196-6112-4f00-8601-0bf4fc49efe3', context_calibrated_precedent_hierarchy).
narrative_ontology:cs_drift_state('36edf196-6112-4f00-8601-0bf4fc49efe3', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36edf196-6112-4f00-8601-0bf4fc49efe3', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_commentary_establishment).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, one_time_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_judges).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, deterred_prospective_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides, case by case, how much weight each line of prior decision carries in the matter at hand — upholding, narrowing, extending, or declining to follow. Writes the opinions that fix weight for everyone below. Gains standing, discretion, and the authority of being the arbiter of weight; is bound by collegial expectation, confirmation politics, and the impossibility of announcing a general rule governing its own balancing.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, beneficiary).

% Corporations, government agencies, insurers, and industry associations that litigate continually across decades. They select which disputes to press, choose forums, fund the cases that establish how weight gets assigned in their domains, and maintain in-house expertise to read weight signals. Variance in precedent weight is a resource to them: they can absorb an adverse weighing and position the next case, while an opponent litigating once cannot.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_litigants, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and small businesses usually involved in a single significant dispute. They must argue not only their case but the antecedent question of how much the opposing side's favored authorities should count — a question they are poorly equipped to price. Settling early or abandoning the claim are the realistic exits once engaged; both surrender value.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, one_time_litigants, payer,
    moderate, immediate, constrained, national).

% Trial and intermediate appellate judges who must apply superior-court precedent while guessing how much weight it will receive if the matter travels upward. They draft opinions hedged against reversals they cannot predict, and their advancement depends on reversal rates they only partially control.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, payer,
    moderate, biographical, identity_locked, regional).

% People and firms with grievances strong enough to justify litigation under stable rules, who decline to file because no one can tell them what their domain's precedents will weigh. They sit outside every courtroom conversation; their absence registers only as filings that never happen.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, deterred_prospective_claimants, excluded,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, deterred_prospective_claimants, payer).

% Law professors, treatise writers, and specialist practitioners who synthesize contested weight into citable authority. Ambivalence in how precedent weighs sustains demand for their interpretations; settled weight assignments would shrink their market.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_commentary_establishment, beneficiary,
    organized, biographical, mobile, national).

% Legislatures and law-reform bodies that watch the balancing from outside and occasionally codify around precedent lines whose weight has become too unstable to rely on. They do not take part in the day-to-day assignment of weight but retain the power to replace whole domains with statutes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legislative_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_litigants).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps several thousand judges deciding related disputes on recognizably connected terms: the corpus supplies shared material, and case-by-case weight assignment lets doctrine stay coherent enough to guide conduct while remaining correctable — solving the problem that absolute binding would entrench errors and loose binding would guide no one.
% TRANSFER_FUNCTION: Moves decisional certainty and litigation resources: from one-time litigants (who pay in unpredictability, weight-argument overhead, and abandoned claims) and lower-court judges (who pay in reversal risk and hedging effort) toward repeat players (who convert variance into positioning advantage) and the appellate bench (which accumulates discretion and interpretive authority).
% ABSENT_VOICES: Prospective claimants deterred before filing are not in any courtroom where weight is argued; future generations bound by today's weight assignments have no seat; non-party stakeholders affected by the underlying decisions enter only through the parties' framing.
% DISAPPEARANCE_RATIONALE: If case-by-case weight assignment vanished overnight, every court would need a replacement rule — rigid adherence or free reconsideration. Doctrine would polarize quickly along whichever rule was chosen: rigid adherence would freeze existing error distributions in place; free reconsideration would collapse the guidance function and strand every litigant mid-strategy. Repeat-player portfolios built on weight-gaming would lose their value; the commentary industry's product would become either redundant or worthless; dockets would reorder around wholesale relitigation.
% FOUNDING_PROBLEM: The early common law needed to reconcile the authority of accumulated decisions with the need to correct mistakes and meet new circumstances: precedent had to mean something without meaning everything, and no legislature existed to redraw the line periodically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: dissenting opinions and granted reversals explicitly contest weight assignments while presupposing the stability-adaptation tension; legislative codifications are enacted in response to precedent lines whose weight became unreliable; comparative scholarship across independent common-law jurisdictions documents the same tension in systems with no shared personnel. None of these seats gains from the balancing practice, and none treats the tension as resolved.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): the arrangement imposes real recurring burdens — the unpredictability premium, the overhead of arguing weight rather than merits, claims never filed — while simultaneously delivering genuine doctrinal coherence, so the score sits well below pure-extraction territory. Suppression (0.38) reflects enforcement that is structural-normative rather than coercive: hierarchy discipline, reversal risk, and professional socialization, with no sanction machinery aimed at participants who depart openly. Theater ratio (0.28) captures the growing share of citation activity that is ritual — citing an authority precisely in order to distinguish it away — while most citation still performs guiding work. Accessibility collapse is low (0.35): departure from precedent remains genuinely available under this arrangement, which is its defining feature, so alternatives close only partially. Resistance (0.42) is sustained: dissents, scholarly attack on inconsistent weighing, forum selection, and legislative codification around unstable lines. The measurement series runs on one shared grid (points 0, 10, 20, 30, 40, 50) with every tracked metric authored at every point; trajectories show extractiveness and theatricality compounding slowly as the corpus grows (more material to weigh, deeper specialist advantage), and suppression requirement nearly flat with a mild ratchet as collegial deference norms hardened. Coalition note: the payer seats are episodic and diffuse — each one-time litigant's stake ends with a single dispute — so coalition formation among victims is structurally weak, which helps explain the arrangement's persistence despite broad dissatisfaction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the appellate bench, the arrangement is experienced as craft: situated judgment exercised with authority, its burdens borne willingly as the price of the role. From the repeat-player seat, it is experienced as terrain: variance to be mapped and exploited, with losses recoverable across a portfolio. From the one-time litigant's seat, the same structure operates as an unpredictable tax levied on someone who cannot spread it. Lower-court judges experience it as anticipatory anxiety — writing for balancers whose balancing they cannot forecast. The identity-lock mechanism on the two judicial seats is professional: the bench's self-concept is constituted by the balancer role, and a court that abandoned situated weighing for a fixed-weight rule would not merely change policy but dissolve its own operative identity; exit is unthinkable from inside the role rather than blocked from outside. If that identity frame broke — for instance, if a jurisdiction publicly adopted mechanical weight rules — the judicial seats' classifications would shift sharply, since their apparent acceptance currently rests on fusion with the practice rather than on net benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the appellate judiciary (agenda-setter and collector of discretion) and institutional repeat litigants (arbitrage-grade exit, portfolio absorption of variance) sit near the beneficiary end; the commentary establishment benefits incidentally through demand for synthesis. Victim declarations drive high directionality: one-time litigants (constrained exit, single-shot exposure) and lower-court judges (identity-locked, unable to relocate out from under superiors' weight assignments) sit near the target end, with deterred prospective claimants furthest of all — they bear the arrangement's costs without ever entering it, which places them at the extreme target position despite having no procedural presence. Scope amplification applies modestly: the arrangement operates nationally, making weight-consistency verification harder than in a single locality and scaling effective extraction upward for targets. Suppression, by contrast, enters the computation unscaled — it is a raw structural property of the enforcement style, and the commentary treats it as such.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure coordination would erase the asymmetric burden: the deterred-claimant population, the weight-argument overhead shifted onto one-shot litigants, and the reversal-risk tax on lower courts are real transfers that a coordination-only account would dissolve into 'system costs.' Reading it as pure extraction would erase the delivered good: doctrinal coherence, error correction, and the guidance function are demonstrably produced, and every jurisdiction that tried to operate without any precedent-weighting discipline reverted. The tangled characterization holds both facts in one structure — genuine coordination function, actively enforced, with identifiable seats paying more than they receive. On obsolescence: the founding problem (reconciling accumulated authority with correction) remains live in every generation, no sunset exists or could exist for it, and the mandate has not outlived its function — so no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the pluralist_balancing reading of the common_law_precedent_corpus kernel; how would the sibling readings restructure the constraint if adopted instead?',
    'Generate and compare the sibling stories (common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework): differences in beneficiary/victim sets, epsilon, and enforcement profile locate the structural delta between readings.',
    'Under strict_stare_decisis, weight variation collapses into uniform hierarchy-based binding — departure becomes the costly act and the burden map inverts toward deviating courts. Under evolutionary_framework, arbitration of weight shifts from situated judicial prudence to normative-evolution criteria, moving discretion toward courts claiming evolutionary insight and reshaping the beneficiary set around them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame record: one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    binding_force_source_disagreement,
    'Where exactly do the readings disagree — what is the source of a precedent''s binding force: hierarchical position, normative evolution, or situated judicial weighing?',
    'Doctrinal analysis of how each reading grounds obedience: the strict reading locates force in rank and history; the evolutionary reading in moral progress; this reading in the judge''s case-specific weighing. The contest is located in the arbitration criterion, not in whether precedent matters.',
    'Whichever criterion prevails determines who holds the balancing power and therefore who captures the arrangement''s gains; the victim set shifts with it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_force_source_disagreement, conceptual, 'Location of the kernel contest: the criterion that fixes precedent weight.').

omega_variable(
    balancing_principled_or_gamed,
    'Is case-by-case weight assignment genuinely principled craft, or systematically gamed by repeat players who engineer the contexts in which weight gets assigned?',
    'Matched-case studies of weight assignment across jurisdictions and eras, comparing outcomes when repeat-player presence varies while doctrinal content is held constant.',
    'If gaming dominates, the effective burden on one-time litigants exceeds the authored epsilon and payer-seat classifications harden toward pure extraction; if principled, the coordination component strengthens and the tangled characterization is confirmed from the benign side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_principled_or_gamed, empirical, 'Whether the balancing method is neutral craft or capturable terrain.').

omega_variable(
    deterred_claimant_population_size,
    'How large is the population of prospective litigants deterred from ever filing by outcome unpredictability — the invisible payer seat?',
    'Natural experiments: compare filing rates before and after a jurisdiction clarifies precedent weight in a domain (codification, landmark clarification, adoption of fixed-weight rules).',
    'A large deterred population raises the true victim count and the arrangement''s effective burden; a small one confines costs to the visible litigating seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterred_claimant_population_size, empirical, 'Size of the deterred-entry victim population.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the written corpus of decisions (a formalized text) or the balancing practice itself (an implicit method)? The two framings yield different commitment-system classifications.',
    'Test which element courts treat as authoritative when weight is disputed: citation to specific opinions supports the text framing; appeal to judging craft and prudence supports the practice framing.',
    'Text framing supports formalized codification with distributed adjudication; practice framing supports implicit codification with practice-grounded authority; drift diagnostics and foreclosure computations differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing choice for the kernel: corpus-as-text versus balancing-as-method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.23).
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.25).
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.27).
narrative_ontology:measurement(clpc_pluralist_balancing_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(clpc_pluralist_balancing_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clpc_pluralist_balancing_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(clpc_pluralist_balancing_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(clpc_pluralist_balancing_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(clpc_pluralist_balancing_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(clpc_pluralist_balancing_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clpc_pluralist_balancing_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(clpc_pluralist_balancing_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(clpc_pluralist_balancing_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(clpc_pluralist_balancing_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(clpc_pluralist_balancing_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(clpc_pluralist_balancing_su_t50, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% The colloquial label 'stare decisis' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the common_law_precedent_corpus kernel: strict_stare_decisis, evolutionary_framework, and this pluralist_balancing reading. Each carries its own epsilon, beneficiary/victim map, and enforcement profile because the referent arrangement — who fixes precedent weight and by what criterion — differs across readings; averaging over them would fabricate a single epsilon for what are three different constraints. Edges run from this file to both siblings; the sibling files carry reciprocal links and document the same decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
