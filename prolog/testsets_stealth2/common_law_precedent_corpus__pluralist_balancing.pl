% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Pluralist Balancing of Precedent Weight Across Domains
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   In most common-law jurisdictions the operative method for handling
 *   precedent is neither categorical adherence nor free reassessment: judges
 *   assign weight domain by domain and case by case, treating some lines of
 *   authority as effectively settled and others as open, and articulating
 *   balancing factors (age of the precedent, reliance interests, workability,
 *   changed understanding) that justify following, distinguishing, or
 *   departing. This story instantiates that pluralist-balancing arrangement
 *   as one reading of the common_law_precedent_corpus kernel; the
 *   strict-stare-decisis and evolutionary-framework readings are separate
 *   constraints in separate files, linked through the network. The epsilon
 *   authored here refers to the standing pluralist calibration arrangement
 *   itself, assessed by this reading's own lights: a method this reading
 *   regards as sound, whose costs it nonetheless records honestly. The claim
 *   (tangled_rope) and the metrics are independent authored facts: the claim
 *   states what this reading believes is structurally true of the
 *   arrangement, and the metrics describe its actual operation, including the
 *   asymmetric burden it places on those without amortizable expertise.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Agenda-setting beneficiary (institutional/identity_locked) — assigns precedent weight, collects doctrinal authority and agenda control
 *   - institutional_repeat_players: Primary beneficiary (powerful/mobile) — converts repeated appearances into durable doctrinal advantage
 *   - specialized_appellate_counsel: Secondary beneficiary (organized/arbitrage) — sells navigation of the very variability the regime produces
 *   - one_shot_litigants: Primary target (powerless/trapped) — bears unpredictability and unrecoverable expertise costs
 *   - generalist_trial_lawyers: Secondary target (moderate/constrained) — competes against specialists without amortizable doctrinal capital
 *   - lower_court_judges: Target-administrator (institutional/identity_locked) — applies the hierarchy daily, bears reversal risk
 *   - legislatures: Excluded corrector (institutional/mobile) — holds override power, sits outside the calibration conversation
 *   - jurisprudential_commentators: Analytical observer (moderate/analytical) — maps the gap between professed fidelity and actual change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.55).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Balancing of Precedent Weight Across Domains").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '9668963b-6d5e-47c4-9b2a-3ed22c572ec6').
narrative_ontology:cs_kernel_codification('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', distributed).
narrative_ontology:cs_authority_grounding('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', practice).
narrative_ontology:cs_interpretation_layer_present('9668963b-6d5e-47c4-9b2a-3ed22c572ec6').
narrative_ontology:cs_reading_relation('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', foundational, precedential_weight_is_domain_and_context_variable).
narrative_ontology:cs_axiom_status(precedential_weight_is_domain_and_context_variable, holdable).
narrative_ontology:cs_axiom_grounding('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', precedential_weight_is_domain_and_context_variable, conventional).
narrative_ontology:cs_axiom('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', foundational, stability_adaptation_balance_requires_case_by_case_judgment).
narrative_ontology:cs_axiom_status(stability_adaptation_balance_requires_case_by_case_judgment, holdable).
narrative_ontology:cs_axiom_grounding('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', stability_adaptation_balance_requires_case_by_case_judgment, instrumental).
narrative_ontology:cs_axiom('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', secondary, doctrinal_domains_track_error_cost_asymmetries).
narrative_ontology:cs_axiom_status(doctrinal_domains_track_error_cost_asymmetries, holdable).
narrative_ontology:cs_axiom_grounding('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', doctrinal_domains_track_error_cost_asymmetries, empirically_contingent).
narrative_ontology:cs_reference_frame('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', context_calibrated_precedential_weight).
narrative_ontology:cs_drift_state('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', contemporary_common_law_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9668963b-6d5e-47c4-9b2a-3ed22c572ec6', '2026-06-14T09:30:00Z').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_players).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, specialized_appellate_counsel).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, generalist_trial_lawyers).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, legal_pragmatism).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, case_by_case_adjudication_method).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, like_cases_alike_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the precedent hierarchy and decides, opinion by opinion, how much weight past decisions carry in each domain: which lines of cases are settled, which remain open to reconsideration, and which factors justify departure. Collects agenda control and doctrinal authority from the discretion the balancing method reserves to it. Its authority is constituted by the very practice it administers, so stepping outside the method is not available to it as an option.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, beneficiary).

% Litigates constantly: insurers, government agencies, platform companies, large employers. Converts repeated appearances into doctrinal familiarity, knowing which courts treat which lines of cases as firm, which factors move a given panel, and when a departure argument will land. Shapes precedent through strategic case selection and amicus participation. Bears litigation costs but recovers them across a portfolio of matters, and can route around unfavorable doctrine by lobbying for statutes or allocating risk contractually.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_players, beneficiary,
    powerful, generational, mobile, national).

% Elite appellate practices whose product is precisely the judgment the regime rewards: reading a domain's precedent landscape, pricing the odds that a court will follow, distinguish, or discard a line of cases, and writing briefs that render one outcome fidelity and another necessary adaptation. Demand for that judgment is created by the variability itself; a regime of flat rules would compress their market. Skills port readily across clients and matters.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, specialized_appellate_counsel, beneficiary,
    organized, biographical, arbitrage, national).

% Individuals and small businesses who encounter the courts once or twice in a lifetime: a custody fight, a foreclosure, an injury claim. They cannot amortize the cost of learning how much weight precedent carries in their particular corner of law, face wide outcome variance between superficially similar cases, and settle under that uncertainty. Once a dispute is filed, leaving the system means forfeiting the claim.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants, payer,
    powerless, immediate, trapped, local).

% Small-firm and solo practitioners advising clients across many fields without deep specialization in any. Each matter requires reconstructing the precedent-weight landscape of a domain they touch occasionally, against opponents who specialize in it. Losing on navigational rather than substantive grounds pushes clients toward larger firms; moving to a niche practice means abandoning established local client relationships and sunk reputational capital.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, generalist_trial_lawyers, payer,
    moderate, biographical, constrained, regional).

% Trial and intermediate appellate judges who apply the weight hierarchy daily: deciding which precedents control, which can be distinguished, and how far analogical extension may run, under continuous exposure to reversal when their weighing disagrees with a higher court's. They administer the regime at the point of application while carrying its compliance burdens; their careers and professional self-conception are bound to the craft of faithful-but-workable application.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, payer,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, agenda_setter).

% Hold plenary power to displace any judge-made rule by statute yet stand outside the judicial conversation in which precedent weight is assigned. Would object that case-by-case calibration lets unelected judges set the effective content of law domain by domain, and that unpredictability shifts policy-making into courtrooms. In practice they intervene episodically, after doctrines have crystallized, rather than participating in the ongoing assignment of weight.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legislatures, excluded,
    institutional, biographical, mobile, national).

% Academic lawyers, legal historians, and comparative scholars who map how the balancing regime actually behaves: documenting divergence between stated fidelity and actual doctrinal change, comparing jurisdictions, and supplying the vocabulary (strong presumption, dictum weight, horizontal versus vertical stare decisis) through which the practice understands itself. They collect no fees and bear no compliance burden; their critiques circulate back into opinions as citation currency.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, jurisprudential_commentators, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, institutional_repeat_players).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal expectations across time and across cases: like cases treated alike, reliable ground for contracting, insuring, and planning, and dispute resolution without relitigating every settled question. The pluralist variant coordinates by calibrating rigidity to domain: firm adherence where the error costs of change are high (property, commercial law, procedure), deliberate flexibility where moral and technological circumstances move (constitutional guarantees, tort duties, emerging-technology liability).
% TRANSFER_FUNCTION: Moves decisional authority upward from trial courts to appellate benches, which alone assign definitive weight; moves litigation wealth from one-shot litigants and generalist practitioners toward repeat players and specialized counsel, who convert doctrinal knowledge into favorable outcomes; and moves effective policy-setting into courtrooms, since domain-by-domain weight assignment determines what the law actually requires.
% ABSENT_VOICES: Future litigants whose disputes will be governed by today's balancing have no seat at any table. Unrepresented persons affected by doctrine appear only through counsel or not at all. Legislatures hold override power but participate only episodically. Parties to disputes in newly emerging fields (algorithmic liability, climate attribution, genomic privacy) have conduct standards set for them by balancing exercises they never joined and could not have predicted.
% DISAPPEARANCE_RATIONALE: If the weight-assignment regime vanished overnight, every pending dispute would be argued from first principles; contracts, insurance pricing, and regulatory compliance built on predictable doctrine would be repriced; the appellate agenda would collapse into universal relitigation; and the system would immediately reorganize around one of the sibling regimes, either categorical adherence or open recalibration, because some method of weighing the past is indispensable to adjudication.
% FOUNDING_PROBLEM: Reconciling the two characteristic failure modes of adjudication: unconstrained discretion, which produces arbitrary and unequal outcomes, and total rigidity, which perpetuates past errors and cannot accommodate changed circumstances. The pluralist arrangement was built to answer how past decisions should govern present ones when neither complete binding nor complete freedom is tolerable.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship documents the stability-versus-adaptation tension operating in every era of the common law, predating and outlasting any current beneficiary class. Comparative-law work shows civil-law systems confronting the same tension through different machinery, indicating the problem is structural rather than an artifact of this arrangement. Most tellingly, the strict-stare-decisis and evolutionary camps, who reject this reading's solution, nonetheless attest that the underlying problem is live; they dispute the answer, not the question.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.55 because the burden is multi-tiered: repeat players experience the regime as near-costless craft they have already paid to master, while one-shot litigants pay full freight for expertise they use once, and outcome variance between similarly situated parties functions as a regressive tax on infrequent court users. Suppression is 0.58 and is authored as a raw structural property, unscaled by power or scope: no participant may opt out of the precedent regime, lower courts face reversal for deviant weighing, and the enforcement infrastructure (discretionary review, citator services, published syllabi, hierarchical supervision) has matured steadily across the interval, which is why suppression_requirement is tracked rather than left static. Theater is 0.30: opinions routinely perform fidelity to precedent while distinguishing lines of cases beyond recognition, and the gap between professed deference and actual doctrinal movement widens as the corpus grows, but the majority of judicial activity remains functional application. Accessibility collapse is 0.40 because exits persist: arbitration clauses, contractual risk allocation, administrative tribunals, and statutory override all remain available, though none reaches the core of ordinary private disputes. Resistance is 0.45: codification movements, academic criticism, periodic legislative overrides, and internal calls for clarified stare decisis factors recur without ever displacing the method. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the rising trajectories reflect doctrine accumulating faster than any simplification occurs, enforcement capacity maturing alongside it, and fidelity rhetoric thickening as departures accelerate.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different classifications from identical structural data. From the repeat-player seat the arrangement is a meritocracy of accumulated craft: predictable enough to plan around, fair because mastery is available to anyone willing to invest. From the one-shot litigant seat the same structure is a lottery with expensive tickets, where superficially similar cases reach opposite results and the difference is invisible without retained specialists. From the appellate bench the method is the only responsible course, wisdom applied case by case; from the strict-adherence camp it is incoherence dressed as judgment; from the legislature it is encroachment by installment. Class-action aggregation partially supplies the coalition power that individually powerless litigants lack, but aggregation itself requires specialized counsel, reproducing the asymmetry one level up. The engine computes these per-seat divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the appellate judiciary, repeat players, and specialized counsel; the mobile and arbitrage-grade exits available to the latter two push them toward the full-beneficiary end, since they can reprice or reroute around adverse doctrine. Victim declarations drive high directionality for one-shot litigants, generalist practitioners, and lower-court judges; trapped and identity-locked exits push the one-shots and the judges toward the full-target end, because neither can take their dispute or their career outside the regime. The appellate judiciary is deliberately dual-positioned: it administers the arrangement and collects authority from it, so its derived directionality sits near the beneficiary pole despite its enforcement labors. National scope modestly amplifies effective extraction for the paying seats, since verifying how much weight precedent carries in a distant domain is harder at scale, and verification difficulty falls hardest on those with the fewest resources to verify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, reconciling stability with adaptation, is live: every new domain reopens it, and the arrangement's mandate has not outlived its function, so no mandatrophy resolution is declared. The classification work here is preventive in both directions. Reading the arrangement as pure coordination would erase the documented asymmetry between those who amortize doctrinal expertise and those who buy it once at retail; reading it as pure extraction would erase the genuine coordination goods, predictability, equal treatment, and error correction, that no serious participant proposes abolishing. The tangled-rope classification keeps both facts visible and forces the analytical question onto the actual fault line: not whether precedent should bind, but who pays for the privilege of finding out how much. Under the mismatch consumer, founding_problem_status=live combined with disappearance_verdict=world_rearranges yields no zombie flag, correctly, since the arrangement's function and its persistence are still aligned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (pluralist_balancing) of the common_law_precedent_corpus kernel; how would epsilon and per-seat classification of the same corpus shift under the sibling readings strict_stare_decisis and evolutionary_framework, and is the disagreement located in the weight-assignment premise itself?',
    'Compile the two sibling stories and compare epsilon values, victim sets, and per-seat classifications across the three readings; locate any divergence in the weight-assignment premise (categorical versus variable versus evolution-triggered).',
    'Divergent classifications across readings would confirm that the corpus label conceals three structurally distinct constraints and validate the family decomposition; convergence would suggest the readings are rhetorical variants of a single constraint and should be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints over the same corpus.').

omega_variable(
    weight_assignment_rule_location,
    'Is the operative weight-assignment rule genuinely case-by-case judicial judgment, or is it converging on informal fixed hierarchies (super-precedents, reaffirmance counts, panel composition) that would make this reading behave like the strict sibling in practice?',
    'Code a sample of departure and adherence decisions for whether the cited justification tracks the articulated balancing factors or stable structural cues such as precedent age, number of reaffirmances, and panel composition.',
    'If informal hierarchies dominate, the pluralist arrangement''s unpredictability costs are overstated and its extraction concentrates differently than authored; the reading would be drifting toward the strict sibling''s structure without acknowledging it, sharpening the practice_drift vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weight_assignment_rule_location, empirical, 'Whether the calibration ideal describes actual judicial behavior or masks an emergent fixed hierarchy.').

omega_variable(
    domain_extraction_variance,
    'Does the burden vary substantially across doctrinal domains (commercial and property law versus constitutional and tort law), such that the multi-tier extractiveness attributed to the whole is actually an average over structurally different sub-regimes?',
    'Per-domain outcome data: repeat-player win rates, settlement-discount spreads, and reversal rates by field; test whether domain explains more variance than litigant resources do.',
    'High variance would warrant decomposing this story into per-domain constraints, each with its own epsilon and possibly its own type; low variance supports treating the corpus-wide arrangement as a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_extraction_variance, empirical, 'Whether the arrangement is one constraint or an average over heterogeneous domain-level regimes.').

omega_variable(
    opacity_rent_vs_complexity,
    'Is the burden falling on one-shot litigants and generalist practitioners a genuine coordination cost of legal complexity, or rent sustained by keeping the weight-assignment criteria implicit?',
    'Natural experiment across jurisdictions or eras that published explicit stare-decisis factors or weighting frameworks: compare outcome convergence and litigation expenditure before and after publication.',
    'If explicit frameworks compress outcome variance without degrading stability, part of the measured burden is removable rent and the arrangement sits closer to the extractive end than authored; if variance persists, it is the irreducible price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opacity_rent_vs_complexity, empirical, 'Separating genuine complexity costs from manufactured opacity in the burden on infrequent court users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t5, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(comm_tr_t5, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(comm_tr_t25, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t5, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 5, 0.43).
narrative_ontology:measurement_basis(comm_be_t5, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 25, 0.54).
narrative_ontology:measurement_basis(comm_be_t25, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t5, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(comm_su_t5, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(comm_su_t25, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the doctrine of precedent' covers three structurally distinct claims about how past decisions bind, and forcing them into one story would make epsilon observer-relative. The strict reading (upstream, historically prior, the formal classical statement) instantiates categorical backward constraint with negligible tolerance for departure; the pluralist reading (this file, currently operative in most common-law courts) instantiates domain-variable weight with case-by-case calibration; the evolutionary reading (downstream challenger) instantiates normative-evolution licensing of reinterpretation. Epsilon differs across the family because the victim sets differ: the strict arrangement burdens those needing correction of past errors, the pluralist arrangement burdens those without amortizable expertise, and the evolutionary arrangement burdens those relying on settled expectations. Each member links to the others through affects_constraints; the upstream strict claim is routinely cited as the baseline against which the pluralist arrangement defines its departures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
