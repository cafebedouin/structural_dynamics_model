% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalism: Adaptive Interpretation Regime
 *   domain: constitutional law / legal philosophy / interpretive theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel
 *   us_constitution_text: the living-constitutionalist reading, under which
 *   constitutional meaning evolves with society and interpretation must adapt
 *   the text's principles to contemporary circumstances. The standing
 *   arrangement under contest — and the epsilon referent — is the actual
 *   adaptive-interpretation regime as it operates: life-tenured judges
 *   empowered to read broad principles in light of changed conditions,
 *   post-ratification practice and precedent treated as authoritative, rival
 *   fixed-meaning methods subordinated within doctrine. Epsilon is authored
 *   for THIS arrangement as the living-constitutionalist reading itself
 *   assesses it — not for the fixed-meaning arrangement a sibling reading
 *   would install, and not for any idealized alternative. Per the
 *   claim/metric independence rule, claimed_type (tangled_rope) is stated
 *   from the structural data — a genuine coordination function (a short
 *   18th-century text with a nearly unreachable amendment threshold kept
 *   governable), an asymmetric transfer (interpretive authority moved from
 *   the ratification-era understanding and from living legislative majorities
 *   to sitting judges), and active enforcement (judicial supremacy, stare
 *   decisis, doctrinal gatekeeping) — while the metrics are authored as
 *   descriptively true of the regime's operation; the engine computes
 *   per-seat classifications, and any divergence from the claim is the
 *   measurement the corpus exists to take. Sibling readings
 *   (us_constitution_text__originalist_reading,
 *   us_constitution_text__positivist_reading) are separate constraint files
 *   with their own epsilon, beneficiary/victim sets, and classifications; the
 *   family decomposition follows the epsilon-invariance principle because the
 *   colloquial label 'constitutional interpretation' conflates structurally
 *   distinct claims with different extraction profiles.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and primary collector (institutional/identity_locked) — decides which principles adapt and how, accumulates interpretive authority, administers the regime through precedent
 *   - rights_claimants_in_changed_contexts: primary beneficiary (moderate/trapped) — receive recognition of rights under evolved meanings (reproductive autonomy, marriage equality); the court is their only forum
 *   - democratic_majorities: primary target/payer (organized/constrained) — bear invalidated enactments and lost policy fights; no opt-out from judicial supremacy
 *   - state_legislatures: secondary payer (organized/constrained) — regional enactments struck down on adapted federal meanings their own state constitutions may permit
 *   - fixed_meaning_advocates: payer and internal dissident (organized/identity_locked) — bear structural subordination of their interpretive method within the profession
 *   - constitutional_law_academy: beneficiary-observer (institutional/identity_locked) — produces the doctrine, trains the interpreters, collects career capital from the regime's openness
 *   - ratifying_generation: excluded non-agent (the dead hand) — the public understanding adapted away; cannot object; proxied by the fixed-meaning movement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.5).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.35).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalism: Adaptive Interpretation Regime").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional law / legal philosophy / interpretive theory").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '40fe4497-d218-466f-b4e4-bc6a71f2423a').
narrative_ontology:cs_kernel_codification('40fe4497-d218-466f-b4e4-bc6a71f2423a', fixed_text).
narrative_ontology:cs_authority_grounding('40fe4497-d218-466f-b4e4-bc6a71f2423a', practice).
narrative_ontology:cs_interpretation_layer_present('40fe4497-d218-466f-b4e4-bc6a71f2423a').
narrative_ontology:cs_reading_relation('40fe4497-d218-466f-b4e4-bc6a71f2423a', us_constitution_text__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('40fe4497-d218-466f-b4e4-bc6a71f2423a', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('40fe4497-d218-466f-b4e4-bc6a71f2423a', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('40fe4497-d218-466f-b4e4-bc6a71f2423a', constitutional_meaning_evolves_with_society, deontological).
narrative_ontology:cs_axiom('40fe4497-d218-466f-b4e4-bc6a71f2423a', foundational, post_ratification_practice_is_authoritative).
narrative_ontology:cs_axiom_status(post_ratification_practice_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('40fe4497-d218-466f-b4e4-bc6a71f2423a', post_ratification_practice_is_authoritative, conventional).
narrative_ontology:cs_axiom('40fe4497-d218-466f-b4e4-bc6a71f2423a', secondary, lochner_era_economic_adaptation_authoritative).
narrative_ontology:cs_axiom_status(lochner_era_economic_adaptation_authoritative, overridden).
narrative_ontology:cs_axiom_grounding('40fe4497-d218-466f-b4e4-bc6a71f2423a', lochner_era_economic_adaptation_authoritative, instrumental).
narrative_ontology:cs_reference_frame('40fe4497-d218-466f-b4e4-bc6a71f2423a', living_framework_of_principles).
narrative_ontology:cs_drift_state('40fe4497-d218-466f-b4e4-bc6a71f2423a', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('40fe4497-d218-466f-b4e4-bc6a71f2423a', '2026-08-03T14:22:07Z').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, constitutional_law_academy).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, democratic_majorities).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, state_legislatures).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, common_law_constitutionalism).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, evolving_standards_conception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal courts, apex at the Supreme Court, decide which constitutional principles adapt and how. Life-tenured judges author binding doctrine, lower courts apply and extend it, and precedent accumulates under their control. The arrangement concentrates interpretive authority in this class: what the text means, as applied, is ultimately what these judges say it means. Individual judges come and go, but the office persists; members cannot resign their way out of the role without vacating the institution's function, and the institution's identity is bound up with being the authoritative interpreter.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Litigants who ask courts to recognize rights under understandings the ratifying public did not hold — reproductive autonomy, contraception, marriage equality, evolving standards in punishment. Their claims succeed only through litigation: in adverse jurisdictions no legislative path exists, which is why they are in court at all. Exit means abandoning the claim; there is no alternative forum with power to grant the right.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts, beneficiary,
    moderate, biographical, trapped, national).

% Voters and the elected majorities they produce. When courts adapt constitutional meaning, enactments these majorities won — on abortion regulation, marriage definition, criminal justice, campaign finance — are invalidated on grounds the majorities do not accept. They cannot opt out of a Supreme Court ruling; the formal remedy, Article V amendment, requires supermajorities that have not been assembled for a structural change in decades. Their practical lever is presidential appointment politics, which operates on a horizon of decades.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_majorities, payer,
    organized, generational, constrained, national).

% State legislatures enact policy for their populations within a federal constitutional frame. A share of their enactments is struck down on adapted federal meanings — meanings the legislature's own state constitution may permit. They can amend state constitutions and litigate, but they cannot escape federal judicial supremacy; their room to legislate narrows with each adaptive precedent that touches their policy space.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, state_legislatures, payer,
    organized, biographical, constrained, regional).

% The organized movement of scholars, lawyers, and judges committed to recovering the Constitution's fixed original meaning. Inside the arrangement their method loses: their claims are rejected in court, their approach is excluded from controlling doctrine, and their project is treated as a minority position within the profession. They contest through scholarship, judicial appointments, and model-statute campaigns over generational horizons. Leaving would mean dissolving the movement's defining commitment, so members persist inside a structure that subordinates them.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates, payer,
    organized, generational, identity_locked, national).

% Law faculties, treatise writers, and the casebook industry. They produce the interpretive frameworks judges draw on and train each cohort of clerks and lawyers. The regime's openness — a text whose meaning is continually elaborated — sustains an unbounded scholarly agenda; a regime of fixed historical meaning would shrink the project to archival recovery. They observe and criticize the arrangement from inside it, and their careers, citations, and schools' reputations are built within its tradition.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_law_academy, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, constitutional_law_academy, observer).

% The public that ratified the text in 1788 and the framers and ratifiers of the Fourteenth Amendment in 1868. The arrangement adapts away the public understanding they enacted; they cannot object, having no seat in any living forum. Their interests are proxied by the fixed-meaning movement, which argues in their name. No exit exists because no participation is possible. Listed as a non-agent for narrative completeness; it feeds no derivation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, ratifying_generation, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(us_constitution_text__living_constitutionalist_reading, ratifying_generation).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the obsolescence problem: a short 18th-century text with a prohibitively hard amendment threshold governs circumstances its drafters did not foresee. Adaptation lets one text remain operative across technological, social, and moral change without requiring constant Article V amendment, and lets precedent accumulate as shared doctrine across courts and generations.
% TRANSFER_FUNCTION: Moves interpretive authority from the ratification-era public understanding (and, case by case, from contemporary legislative majorities) to sitting judges; moves concrete policy outcomes to litigants who prevail in court on evolved meanings — reproductive autonomy, marriage equality, criminal-procedure protections — that they could not obtain legislatively in adverse jurisdictions.
% ABSENT_VOICES: The ratifying generation is structurally absent — it cannot object to the adaptation of the understanding it enacted; its proxy inside the discourse is the fixed-meaning movement. Also underrepresented: citizens in jurisdictions whose majorities lose policy fights in court and whose formal remedy (Article V) is prohibitively costly, and future generations who inherit evolved doctrine without having consented to it.
% DISAPPEARANCE_RATIONALE: If adaptive interpretation vanished overnight, courts would decide by original public meaning; landmark precedents resting on evolved-meaning reasoning (the substantive due process line, evolving-standards doctrine) would lose their foundation; Article V would become the only adaptation path; and the legal academy's interpretive project would reorganize around historical recovery. The constitutional order would not collapse — it would rearrange around fixed meaning, which is precisely what the sibling reading proposes.
% FOUNDING_PROBLEM: How can a short, old text with a nearly unreachable amendment threshold continue to govern a nation facing circumstances its drafters did not foresee, without either abandoning the text or entrenching the moral understandings of the past?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional historians across interpretive camps document the amendment-threshold and obsolescence pressure; originalist scholars — opponents of this reading's remedy — concede the problem is real (their fixed-meaning proposal is itself an answer to it); political scientists document the near-impossibility of Article V structural amendment in the modern era. What is disputed is the solution, not the problem.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.5: the reading itself concedes the counter-majoritarian transfer is real — adapted doctrine overrides enactments that legislative majorities won — and assesses it as substantially paid for by the coordination delivered (a text kept governing across two centuries of changed circumstances without Article V action). The value sits at symmetry: costs roughly matched by delivered coordination, as the reading's own lights assess them. Suppression 0.35: a raw, unscaled structural property; the regime binds all comers once the Court rules and subordinates rival interpretive methods within doctrine, but it directs no one's conduct directly, dissenting methods persist outside doctrine, and democratic processes continue. Theater 0.25: first-order adaptation is real and load-bearing; a minority of regime activity is legitimacy rhetoric and precedent management. Accessibility_collapse 0.35: understanding the regime does not collapse alternatives — the fixed-meaning method remains fully available as a competing practice, though precedent forecloses rival rulings inside doctrine. Resistance 0.6: the arrangement has met sustained organized resistance for half a century (the fixed-meaning movement, appointment politics, academic counter-program). All three tracked series run on one shared grid (t=0 approximates the 1937 New Deal settlement that discredited the prior fixed economic reading and cleared the path for the modern regime; each unit approximates one year; t=90 is the contemporary Court). base_extractiveness rises to a Warren-era peak as the regime's override of legislative majorities widens, then oscillates with Court composition — the driver is external (appointment cycles), not intermittent reinforcement — ending at the authored scalar. suppression_requirement is authored because this story specifically tracks enforcement-capacity change: consolidation of judicial supremacy, hardening of stare decisis, socialization of the bar through law schools — rising through mid-interval, then decaying as enforcement normalized into precedent and legitimacy rather than active suppression. theater_ratio creeps upward as the regime matures. The base_properties scalars are the contemporary endpoint values. No mandatrophy — see mandatrophy_analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the bench, the arrangement is the Constitution working as designed: principles written broadly, applied to circumstances the drafters could not see. From the democratic-majority seat, the same structure is governance by unelected interpreters who overturn what elections produced. The academy experiences it as an open research program whose subject matter renews itself; the fixed-meaning advocate experiences the same profession as a structure that exiles their method. Same arrangement, four incompatible experiences — the engine computes this per-seat divergence from power, exit, and role; the authored claim does not adjudicate it. The sharpest same-level lateral contrast is constitutional_law_academy versus fixed_meaning_advocates: comparable power atoms at national scope, identical identity-locked exits, opposite sides of the transfer — which shows the divergence is structural position, not power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: rights_claimants_in_changed_contexts (trapped exit — the court is the only forum that can grant the right, so they sit near the full-beneficiary end), federal_judiciary (collects interpretive authority and administers the regime; its effective extraction is damped toward subsidy because the arrangement is what constitutes its office), constitutional_law_academy (collects career capital from the regime's openness). Targets: democratic_majorities and state_legislatures (constrained exits — no opt-out from judicial supremacy, Article V prohibitively hard — high d, amplified chi), fixed_meaning_advocates (identity_locked — their subordination persists as long as they hold their commitment, high d). ratifying_generation is a non-agent entry and feeds no derivation. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an old, short text with a nearly unreachable amendment threshold governing unforeseen circumstances — is live, and its status is corroborated from outside the benefiting parties: originalist scholars concede the problem while disputing the remedy. founding_problem_status=live with disappearance_verdict=world_rearranges produces no dead-mandate mismatch flag. The tangled_rope claim is what prevents both standard mislabels: calling the arrangement a pure coordination device hides the counter-majoritarian transfer its own tradition concedes (Bickel's difficulty is internal to it); calling it pure extraction hides the obsolescence-coordination function even its opponents rely on when they propose their own answer to the same problem. It is not a piton: the function is not atrophied, theater is low, and the agenda-setting seat actively maintains the regime because the regime concentrates authority in that seat — a concentrated collector, not diffuse neglect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel us_constitution_text — the living_constitutionalist_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in the time-index of constitutional meaning: this reading holds meaning evolves with society (making post-ratification practice and social change authoritative, and rights claimants in changed contexts the beneficiary seat); the originalist_reading holds meaning fixed at ratification (making the ratifying public understanding authoritative and democratic constraint the protected seat); the positivist_reading relocates validity to enactment procedure and is orthogonal to the meaning question. No in-story data resolves this — it resolves only by which reading a party adopts, i.e. by which constraint file is loaded.',
    'Under the originalist sibling, the beneficiary and victim sets effectively swap (democratic majorities become the protected seat; rights claimants in changed contexts lose their forum) and that reading authors high epsilon for this arrangement. Under the positivist sibling, the question is reframed as validity rather than meaning, dissolving this story''s transfer structure. This file''s classification is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and different beneficiary/victim sets.').

omega_variable(
    counter_majoritarian_cost_necessity,
    'Is the transfer from democratic processes to courts — the empowerment to override enactments on adapted meanings — a necessary cost of keeping an 18th-century text governing, or does it exceed what adaptation requires?',
    'Comparative constitutional evidence: jurisdictions with formal adaptation mechanisms (easy amendment, weak or absent judicial review) and jurisdictions with constrained-adaptation doctrines (presumption of constitutionality, Thayerian deference); measure whether rights protection and governability survive with less judicial override of legislative outcomes.',
    'If the override exceeds what adaptation requires, the transfer side of the arrangement dominates and the structure drifts toward pure extraction; if it tracks necessity, the structure sits nearer pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_cost_necessity, empirical, 'Whether the counter-majoritarian cost is intrinsic to the coordination function or excess layered on top of it.').

omega_variable(
    regime_capture_by_interpreter_class,
    'Who durably collects the regime''s gains — do rights claimants in changed contexts capture the transferred authority, or does it accrue to the federal judiciary as a standing interpreter class?',
    'Track the distribution of adaptive-doctrine wins across decades: episodic rights outcomes for successive claimant cohorts versus the continuous accumulation of docket control, doctrinal authorship, and institutional prerogative in a life-tenured interpreter class with low turnover.',
    'If the judiciary is the durable collector, the receipt of gains names that seat and the arrangement is capture-flavored; if gains genuinely flow through to successive claimant cohorts, the receipt is diffuse across beneficiaries and the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_capture_by_interpreter_class, empirical, 'Whether the arrangement''s gains concentrate in the interpreter class or pass through to rights claimants.').

omega_variable(
    dead_hand_consent_status,
    'Does the ratifying generation''s consent bind — is adaptation a taking from a real constraint-owner, or is intergenerational consent a fiction with no living bearer?',
    'Not resolvable by data: it depends on a theory of intergenerational obligation (contractarian versus living-tradition conceptions). The fixed-meaning movement treats the consent as real and overridden; the regime treats it as non-binding; both positions are internally coherent.',
    'If the consent is real, the transfer is a taking from a present constraint-owner and effective extraction rises for that constituency; if fictional, the victim set shrinks to living democratic majorities and the arrangement''s cost is purely counter-majoritarian.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_consent_status, conceptual, 'Whether the dead-hand objection identifies a genuine paying seat or a rhetorical device.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__living_constitutionalist_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_text__living_constitutionalist_reading, theater_ratio, 45, 0.23).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_text__living_constitutionalist_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t75, observed).
narrative_ontology:measurement(us_c_tr_t90, us_constitution_text__living_constitutionalist_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement_basis(us_c_be_t45, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t75, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 75, 0.54).
narrative_ontology:measurement_basis(us_c_be_t75, observed).
narrative_ontology:measurement(us_c_be_t90, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 90, 0.5).
narrative_ontology:measurement_basis(us_c_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(us_c_su_t45, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t75, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(us_c_su_t75, observed).
narrative_ontology:measurement(us_c_su_t90, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 90, 0.35).
narrative_ontology:measurement_basis(us_c_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional interpretation' decomposes under the epsilon-invariance principle into three structurally distinct claims sharing one kernel (us_constitution_text). This file authors the living-constitutionalist reading (moderate epsilon as the reading's own lights assess it; beneficiaries: rights claimants, judiciary, academy; targets: democratic majorities, state legislatures, fixed-meaning claimants). us_constitution_text__originalist_reading authors the fixed-meaning arrangement with its own epsilon and a swapped beneficiary/victim structure (democratic constraint protected; rights claimants in changed contexts left to Article V). us_constitution_text__positivist_reading authors the enactment-procedure arrangement, orthogonal to the meaning question. Pressure runs between the readings through appointment politics and doctrinal precedent; edges are declared so contamination analysis can trace legitimacy spillover between family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
