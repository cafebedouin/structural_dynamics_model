% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Complementarity Gate — National Primacy Reading (Article 17, Rome Statute)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute bars the International Criminal Court from
 *   acting over a situation when a state with jurisdiction is carrying out
 *   genuine proceedings of its own. Under the national-primacy instantiation
 *   authored here, national courts are presumptively adequate unless
 *   demonstrated to be a sham, and the burden of demonstrating
 *   inadmissibility falls on the side seeking international action. The
 *   operative consequences: a high admissibility threshold; victims in states
 *   running weak-but-real proceedings fall outside the Court's reach, with
 *   the reachable victim set effectively restricted to situations of complete
 *   judicial collapse; state cooperation is treated as the system's
 *   load-bearing resource and is prioritized accordingly. The arrangement
 *   coordinates (it is the compromise that made a permanent court ratifiable
 *   and keeps primary justice responsibility with states) while it extracts
 *   (an identifiable class of atrocity victims bears the cost of whatever
 *   justice their state chooses to provide). The claim and the metrics are
 *   independent authored facts: claimed_type records the structural judgment
 *   that both functions are present; the metric values record the
 *   arrangement's observed operation, including its rising extractiveness as
 *   states learned to deploy domestic proceedings as admissibility shields.
 *
 * KEY AGENTS:
 *   - sovereignty_maximizing_states: Agenda setter (institutional/constrained) — authored the threshold into the Statute, defends admissibility through challenges and assembly politics, collects the retained adjudicative authority
 *   - national_judiciaries: Primary beneficiary (institutional/constrained) — retain primary jurisdiction, receive capacity-building flows, hold the prestige of adjudicating their own
 *   - domestic_elite_perpetrators: Secondary beneficiary (powerful/constrained) — shielded whenever a nominally genuine domestic process satisfies the gate
 *   - weak_proceeding_atrocity_victims: Primary target (powerless/trapped) — bear the denial of international recourse while their state's inadequate process holds the gate
 *   - victims_of_selective_transitional_justice: Target (powerless/trapped) — excluded when adversary-only trials satisfy the genuineness test
 *   - icc_office_of_the_prosecutor: Gate operator (institutional/constrained) — administers the threshold, bears the demonstration burden, inherits the collapse-case residue
 *   - un_security_council_p5: Structural arbiter (institutional/arbitrage) — refers and defers situations, self-insulated by veto and non-party status
 *   - human_rights_litigation_ngos: Advocacy observer (organized/mobile) — document genuineness, press for broader findings, redirect to third-state venues when the gate holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.5).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.6).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Complementarity Gate — National Primacy Reading (Article 17, Rome Statute)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'aba59813-cc65-414a-ac01-819bdd72966b').
narrative_ontology:cs_kernel_codification('aba59813-cc65-414a-ac01-819bdd72966b', fixed_text).
narrative_ontology:cs_authority_grounding('aba59813-cc65-414a-ac01-819bdd72966b', lineage).
narrative_ontology:cs_interpretation_layer_present('aba59813-cc65-414a-ac01-819bdd72966b').
narrative_ontology:cs_reading_relation('aba59813-cc65-414a-ac01-819bdd72966b', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('aba59813-cc65-414a-ac01-819bdd72966b', foundational, territorial_state_primary_responsibility).
narrative_ontology:cs_axiom_status(territorial_state_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('aba59813-cc65-414a-ac01-819bdd72966b', territorial_state_primary_responsibility, deontological).
narrative_ontology:cs_axiom('aba59813-cc65-414a-ac01-819bdd72966b', secondary, icc_residual_forum_of_last_resort).
narrative_ontology:cs_axiom_status(icc_residual_forum_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('aba59813-cc65-414a-ac01-819bdd72966b', icc_residual_forum_of_last_resort, instrumental).
narrative_ontology:cs_reference_frame('aba59813-cc65-414a-ac01-819bdd72966b', national_court_sufficiency_presumption).
narrative_ontology:cs_drift_state('aba59813-cc65-414a-ac01-819bdd72966b', contemporary_post_kenya_jurisprudence, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('aba59813-cc65-414a-ac01-819bdd72966b', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, domestic_elite_perpetrators).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, weak_proceeding_atrocity_victims).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_selective_transitional_justice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, sovereign_equality_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote the presumptive-adequacy threshold into the Rome Statute bargain and defend it through admissibility challenges, assembly-of-states-politics, and cooperation conditionality. They weigh treaty obligations against retained control over justice on their territory. Withdrawal from the Statute exists as an exit but carries reputational cost and does not extinguish proceedings already opened.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, agenda_setter,
    institutional, generational, constrained, national).

% Retain primary jurisdiction over atrocity crimes committed on their territory or by their nationals, receive capacity-building assistance channeled through the complementarity framework, and hold the institutional prestige of adjudicating their own. Ceding primacy to an international forum would diminish their standing; they are bound into the arrangement they benefit from.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Atrocity-era officials who retain influence over domestic prosecutorial priorities after the transition. A domestic process that is procedurally real — indictments filed, trials held, convictions secured against subordinate or adversary figures — satisfies the gate and forecloses international scrutiny of the principals. Their exposure narrows to the rare scenario of total judicial collapse.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, domestic_elite_perpetrators, beneficiary,
    powerful, biographical, constrained, national).

% Survivors of mass crimes in states whose courts operate but inadequately: few perpetrators charged, narrow offense framing, no reach to the command level. Because their state's proceedings are genuine enough to hold the gate, the international forum is closed to them, and no alternative body holds jurisdiction over their case. The justice available to them is exactly what their state chooses to deliver.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, weak_proceeding_atrocity_victims, payer,
    powerless, biographical, trapped, national).

% Victims on the losing side of a conflict in which the victorious government prosecutes only its adversaries. The trials are procedurally genuine — counsel, evidence, verdicts — so the gate holds, and the perpetrators of crimes against the victims' side remain untouched. Like the weak-proceeding victims, they cannot move their case to another forum.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_selective_transitional_justice, payer,
    powerless, biographical, trapped, national).

% Operates the admissibility gate: screens situations, assesses the genuineness of domestic proceedings, and bears the practical burden of demonstrating inadmissibility when states challenge. It inherits the residual caseload of complete-collapse situations and depends on state cooperation for evidence, arrests, and sentence enforcement — dependence that disciplines how far it pushes against the gate's presumptions.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, beneficiary).

% Can refer situations to the Court irrespective of territorial consent and can defer investigations, making it a structural arbiter of when the gate opens. Several permanent members are not party to the Statute and hold veto power, insulating themselves and aligned states from the gate's operation while retaining the ability to aim it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, un_security_council_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% Document the quality of domestic proceedings, submit communications and amicus material, and press for findings that domestic process is a sham or that the state is unwilling. When the gate holds, they redirect campaigns toward universal-jurisdiction proceedings in third states — an exit the victims themselves do not have.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_litigation_ngos, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of creating any permanent international criminal court at all: states would not ratify a court with automatic primacy over their own justice systems, so the presumptive-adequacy gate secures ratification, ongoing cooperation (surrender, evidence, enforcement), and keeps primary responsibility for atrocity justice with national institutions, with capacity-building flows attached.
% TRANSFER_FUNCTION: Moves adjudicative authority and the accountability it yields: by default it reserves atrocity-crime adjudication to national institutions, transferring to the Court only the residue of complete judicial collapse; correspondingly it transfers to victims in weak-but-genuine states the burden of accepting whatever justice their state provides. It also moves prestige and capacity resources toward national judiciaries.
% ABSENT_VOICES: Atrocity victims in gated states have no formal seat in admissibility determinations — they appear only through victims' counsel in related proceedings and through advocacy organizations, never as principals deciding whether the gate opens for them. Holders of a broader reading of the unwillingness standard likewise stand outside the operative rule this arrangement enforces; their objections arrive as argument, not as procedure.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight and the Court gained automatic primacy, the ratification bargain unravels: states that joined on complementarity terms would withdraw or refuse cooperation, surrender and evidence channels would close, national judiciaries would lose the capacity flows and retained jurisdiction the arrangement guarantees, and the Court would face simultaneous universal jurisdiction it cannot enforce. The entire Rome-system architecture reorganizes around whichever replacement legitimacy settlement the major powers will tolerate.
% FOUNDING_PROBLEM: The impunity gap of the 1990s: ad hoc tribunals were expensive, selective, and hostage to Security Council politics, and no permanent court existed; states demanded assurance that a permanent court would supplement rather than supplant their own justice systems. Complementarity was the compromise that made ratification arithmetically possible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: contemporaneous diplomatic records of the Rome Conference and scholarship by international criminal law academics unaffiliated with state delegations attest that presumptive national primacy was the price of universality; United Nations commission-of-inquiry reporting and the continuing docket of mass-atrocity situations attest that the underlying impunity problem the arrangement was built for persists. No attestation relies solely on the states or judiciaries that collect the arrangement's benefits.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.5: substantial but bounded. The referent is the standing arrangement — the complementarity gate as operated under presumptive national adequacy — and the value is reading-indexed: even by this reading's own lights, a large class of victims bears real uncompensated costs (whatever justice their state provides, and nothing else), costs the reading accepts as the system's price rather than denies. Suppression (0.6) is a raw structural property, unscaled by power or scope: the gate forecloses the international forum for gated victims, and its persistence depends on active machinery — admissibility challenges, cooperation dependencies, assembly politics — not on participant preference. Theater_ratio (0.35) reflects a real function performed with a growing performative shell: as states learned that a plausible domestic proceeding blocks the Court, the production of genuineness became a strategy in itself ('complementarity gaming'), so a rising share of activity around the gate is performance aimed at the gate rather than justice aimed at perpetrators. Accessibility_collapse (0.55): once a victim understands the gate, alternatives do not vanish — the inability prong, Council referral, and third-state universal jurisdiction remain — but none is available on demand. Resistance (0.55): states actively contest adverse readings while victims' advocates and parts of the prosecutorial office press the other way. The temporal series run on one shared grid (t=0..24 at four-year steps) with all three tracked metrics authored at every point; all series rise monotonically, matching the historical arc from a dormant gate to a contested, gamed, and hardened one.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats compute differently from the same structure. From the state seat, the gate is a legitimate sovereignty protection they designed and defend: primary responsibility belongs to domestic institutions, and the Court is a backstop, not a supervisor. From the trapped victim seats, the same gate is the wall between them and the only forum that could reach the architects of the crimes — their state's process is the ceiling of achievable justice, and the gate locks that ceiling in place. The gate operator sits between: it administers the threshold it did not design, bears the demonstration burden, and depends for its effectiveness on the very states the gate protects. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereignty-maximizing states and national judiciaries sit near the beneficiary end: the gate subsidizes them with retained authority, capacity flows, and cooperation incentives, and neither can exit without forfeiting those goods. Domestic elite perpetrators derive a derivative but sharp benefit — shielding proportional to their influence over domestic prosecutorial priorities — placing them near the beneficiary end despite bearing no formal role in the arrangement. Weak-proceeding victims and victims of selective transitional justice sit near the full-target end: they pay in foreclosed recourse, they are trapped (the crime's territory and their state's forum are not choosable), and their powerlessness forecloses coalition routes to reopening the gate. The prosecutor's office occupies a mid-range position: it operates the extraction machinery yet collects the residual caseload, so its derived directionality blends gatekeeping and residual benefit. The permanent five sit nearest the beneficiary end of any seat: arbitrage-grade insulation (veto, non-party status) lets several of them shape the gate while standing outside its reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabels. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function: without presumptive national primacy, the treaty likely never achieves universality, cooperation collapses, and the Court loses the enforcement capacity that is its only lever — the coordination is load-bearing, not cover. Reading it as pure coordination (rope) would erase the asymmetric transfer: a specific, identifiable victim class pays for the coordination with foreclosed recourse, and the payment is not incidental but structural — the gate's high threshold is precisely what produces their exclusion. Tangled rope holds both truths: coordination that works and extraction that rides on it. On the genealogy interview, the founding problem (an impunity gap requiring a permanent court, solvable only with state buy-in) remains live, and the disappearance verdict is world_rearranges: overnight removal of the gate would unravel ratification bargains, cooperation, and the Court's caseload logic simultaneously. Live founding problem plus world-rearranging dependence means no zombie flag: this is a functioning hybrid, not an atrophied husk — though the rising theater and extractiveness series mark where decay would register first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_of_article_17,
    'This constraint is one reading of the kernel article_17_complementarity — the national_primacy_reading, holding national courts presumptively adequate with the inadmissibility burden on the Court. What would the sibling reading (international_oversight_reading) change structurally over the same referent?',
    'Generate the sibling as its own constraint story and compare epsilon, victim sets, and burden allocation across the two files; the disagreement is located in the admissibility threshold and the allocation of the demonstration burden.',
    'The sibling authors substantially higher epsilon over the identical standing arrangement, expands the reachable victim set beyond complete judicial collapse, and shifts the burden to states; per-seat classifications of the shared label diverge accordingly. Neither file''s classification adjudicates the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_of_article_17, conceptual, 'Committer structure: reading-indexed epsilon over a shared kernel; sibling deltas routed here rather than folded into this constraint.').

omega_variable(
    sham_genuineness_boundary,
    'Where does ''proven sham'' end and ''weak-but-genuine'' begin, and who effectively bears the burden at that boundary?',
    'Accumulated Article 19 jurisprudence and Office of the Prosecutor screening practice on same-conduct/same-person testing, traced across situations where admissibility was contested.',
    'A lower operational threshold moves weak-but-genuine-state victims into the Court''s reach, raising measured extraction and shifting the arrangement''s profile toward a wider-reach instantiation; a higher threshold restricts the reachable victim set to complete judicial collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_genuineness_boundary, empirical, 'Operational content of the genuineness test determines the effective victim set.').

omega_variable(
    cooperation_priority_effect,
    'Does prioritizing state cooperation preserve the Court''s enforcement capacity, or does it convert the admissibility gate into leverage that states hold over the institution policing them?',
    'Comparative case outcomes where cooperation was refused versus granted: non-surrender episodes, evidence-sharing failures, and situations closed for want of cooperation, contrasted with completed cases.',
    'If state leverage dominates, the coordination function decays and the arrangement drifts toward pure extraction riding on a shrinking cooperative core; if capacity is genuinely preserved, the hybrid coordination-plus-extraction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperation_priority_effect, empirical, 'Whether the cooperation priority is load-bearing coordination or convertible leverage.').

omega_variable(
    elite_shielding_selectivity,
    'Does the high threshold systematically shield principals who retain influence over domestic prosecutorial priorities, while exposing only low-level or adversary perpetrators?',
    'Cross-case comparison of who is actually tried domestically in situations where the gate held, against the command structure of the alleged crimes.',
    'Confirmation identifies a structural victim class invisible in the complete-collapse framing and raises effective extraction for captured-process victims; disconfirmation supports the reading''s claim that genuine domestic process is a reliable proxy for adequate justice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_shielding_selectivity, empirical, 'Whether nominally genuine proceedings function as elite shields in practice.').

omega_variable(
    kernel_framing_text_vs_custom,
    'Is the kernel the statutory text of Article 17 alone, or the longer-standing customary practice of territorial subsidiarity that the text codifies?',
    'Doctrinal analysis separating treaty-text interpretation from the pre-Statute practice of comity and territorial jurisdiction; test whether the drift vector differs under each framing.',
    'Under the text-only framing, interpretive movement registers as codification strain; under the custom framing, the same movement registers as minor variation within a centuries-old practice, changing the drift magnitude and what counts as departure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_text_vs_custom, conceptual, 'Framing under-determination in the declared kernel: fixed text versus codified custom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__national_primacy_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(arti_tr_t24, article_17_complementarity__national_primacy_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__national_primacy_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(arti_be_t24, article_17_complementarity__national_primacy_reading, base_extractiveness, 24, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__national_primacy_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(arti_su_t24, article_17_complementarity__national_primacy_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, international_oversight_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 17 complementarity' decomposes, per the epsilon-invariance principle, into at least two structurally distinct readings of one kernel. This file instantiates the national-primacy reading (presumptive national adequacy, Court-borne inadmissibility burden, victim set restricted to complete judicial collapse). The sibling file instantiates the oversight reading (Court as impunity guardian, broad unwillingness/unability capture, expanded victim set). The two stories share a referent — the standing complementarity arrangement — and differ in reading-indexed epsilon, beneficiary/victim emphasis, and burden allocation; they are linked here rather than merged, because forcing one story to span both readings would make epsilon observer-dependent. The upstream/downstream pressure between them runs through shared jurisprudence: each admissibility decision feeds both readings' precedents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
