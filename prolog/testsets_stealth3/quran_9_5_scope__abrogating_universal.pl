% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Verse 9:5 Universal Offensive Jihad Obligation (Abrogating-Universal Reading)
 *   domain: religious jurisprudence/hermeneutics/political theology
 *
 * SUMMARY:
 *   This story instantiates ONE reading — abrogating_universal — of the
 *   contested kernel quran_9_5_scope: the claim that Quran 9:5 abrogates all
 *   previously revealed peaceful verses and installs war against
 *   non-submitted non-Muslims as a standing, universal legal obligation
 *   terminating only at conversion or formal submission. Per Rule 1, the
 *   story is a clean, epsilon-invariant constraint: the standing arrangement
 *   under assessment is the obligation itself as this reading holds it,
 *   traced across its operational history (revelation-era application,
 *   imperial codification and execution, post-caliphal dormancy, violent
 *   revival). The referent of every metric is that standing obligation, never
 *   the sibling readings' endorsed alternatives. The claim/metric
 *   independence discipline applies: claimed_type records my structural
 *   judgment; the metrics record the arrangement's operation independently.
 *   KEY AGENTS (by structural relationship): - non_submitted_polytheists:
 *   Primary target (powerless/trapped) — bears the terminal
 *   convert-submit-or-die choice - non_muslim_treaty_counterparts: Secondary
 *   target (moderate/trapped) — agreements voided without consent, no
 *   negotiating seat - contextualist_scholars: Internal target
 *   (moderate/identity_locked) — rival exegetes suppressed under enforcement
 *   - reluctant_muslim_subjects: Mixed seat (moderate/constrained) — bears
 *   conscription and duty-burden costs, incidental benefit - conquest_elites:
 *   Primary beneficiary (institutional/arbitrage) — directs campaigns,
 *   captures spoils and tribute - ghazi_frontier_warriors: Secondary
 *   beneficiary (organized/mobile) — collects plunder shares and status -
 *   expansionist_jihad_movements: Beneficiary and enforcer
 *   (organized/identity_locked) — inherits the reading as organizational
 *   charter - abrogationist_ulama_establishment: Agenda setter
 *   (institutional/identity_locked) — codifies, transmits, and guards the
 *   doctrine - academic_hermeneutic_observers: Analytical observer
 *   (analytical/analytical) — documents chronology and abrogation debates
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.84).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.78).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.84).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Verse 9:5 Universal Offensive Jihad Obligation (Abrogating-Universal Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious jurisprudence/hermeneutics/political theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'aa0aeb4a-034f-4754-bd6a-809eca07da74').
narrative_ontology:cs_kernel_codification('aa0aeb4a-034f-4754-bd6a-809eca07da74', fixed_text).
narrative_ontology:cs_authority_grounding('aa0aeb4a-034f-4754-bd6a-809eca07da74', lineage).
narrative_ontology:cs_interpretation_layer_present('aa0aeb4a-034f-4754-bd6a-809eca07da74').
narrative_ontology:cs_reading_relation('aa0aeb4a-034f-4754-bd6a-809eca07da74', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('aa0aeb4a-034f-4754-bd6a-809eca07da74', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('aa0aeb4a-034f-4754-bd6a-809eca07da74', foundational, sword_verse_abrogates_peaceful_verses).
narrative_ontology:cs_axiom_status(sword_verse_abrogates_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('aa0aeb4a-034f-4754-bd6a-809eca07da74', sword_verse_abrogates_peaceful_verses, conventional).
narrative_ontology:cs_axiom('aa0aeb4a-034f-4754-bd6a-809eca07da74', foundational, offensive_jihad_standing_obligation).
narrative_ontology:cs_axiom_status(offensive_jihad_standing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('aa0aeb4a-034f-4754-bd6a-809eca07da74', offensive_jihad_standing_obligation, deontological).
narrative_ontology:cs_axiom('aa0aeb4a-034f-4754-bd6a-809eca07da74', secondary, no_standing_coexistence_absent_submission).
narrative_ontology:cs_axiom_status(no_standing_coexistence_absent_submission, holdable).
narrative_ontology:cs_axiom_grounding('aa0aeb4a-034f-4754-bd6a-809eca07da74', no_standing_coexistence_absent_submission, deontological).
narrative_ontology:cs_reference_frame('aa0aeb4a-034f-4754-bd6a-809eca07da74', companion_era_offensive_baseline).
narrative_ontology:cs_drift_state('aa0aeb4a-034f-4754-bd6a-809eca07da74', contemporary_post_caliphate, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('aa0aeb4a-034f-4754-bd6a-809eca07da74', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, abrogationist_ulama_establishment).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, conquest_elites).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, ghazi_frontier_warriors).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihad_movements).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_submitted_polytheists).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_treaty_counterparts).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, contextualist_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, reluctant_muslim_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and transmits the ruling that the later verse stands over the earlier peaceful ones, trains jurists in the methodology that yields it, and certifies when the collective military duty may be discharged or suspended. Collects scholarly standing, students, endowments, and adjudication authority from guarding the doctrine against revision. Jurists who denied the abrogation historically faced charges of undermining the law; leaving the framework means forfeiting the scholarly office and community standing that constitute the career.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, abrogationist_ulama_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, abrogationist_ulama_establishment, beneficiary).

% Caliphs, governors, and army commanders who direct campaigns and allocate the proceeds: the commander's reserved fifth of portable spoils, annexed land revenues, tribute schedules, and enslaved persons. They gave the doctrine its administrative form and could suspend it when diplomacy paid better — truces and tributary peace with rival empires recur throughout the record — maneuvering between the doctrine's demand and strategic interest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, conquest_elites, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, conquest_elites, agenda_setter).

% Frontier fighters and volunteers whose compensation is the distributed share of portable plunder, land allocations, social honor, and the promised martyr's portion. They move along contested frontiers seasonally; a fighter who leaves the frontier loses the shares and standing but faces no penalty for going.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, ghazi_frontier_warriors, beneficiary,
    organized, immediate, mobile, continental).

% Modern organizations that inherit the reading as charter: Qutbist study circles, transnational volunteer networks, and the movement that declared a territorial caliphate in 2014. They recruit on the obligation, govern captured populations by its terms, and treat members who abandon the program as traitors. Membership and doctrine are fused, so leaving equals ideological dissolution rather than relocation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihad_movements, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_jihad_movements, agenda_setter).

% Populations classified as idolaters who have neither converted nor accepted subordinate submission terms. Where the reading holds power they face a terminal choice — conversion, acceptance of second-class submission status carrying special levies, or the sword — and children inherit the choice-set at birth. Flight beyond the reading's reach is the only exit and is available mainly to the wealthy and mobile.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_submitted_polytheists, payer,
    powerless, generational, trapped, continental).

% Tribes and polities whose existing peace agreements the doctrine declares void once its deadline passes. They negotiated in good faith under earlier verses whose force the abrogation cancels without their consent, and their protests have no seat in the framework because the doctrine denies their treaties continuing validity. Many were armed and fought; few could renegotiate on comparable terms.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_treaty_counterparts, excluded,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, non_muslim_treaty_counterparts, payer).

% Muslim exegetes and jurists who hold that the verse addressed the treaty-breaking tribes of its own season and that the earlier peaceful verses retain force. Under enforcement periods they are tried for weakening the law, barred from teaching, exiled, or executed. Their professional existence runs through institutions the ruling establishment controls; abandoning the vocation ends the persecution but also the scholarly life that formed them.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, contextualist_scholars, payer,
    moderate, generational, identity_locked, global).

% Believers subject to the collective military duty who bear conscription, war taxation, garrison rotation, and death risk while sharing incidentally in the order the campaigns produce. Some served eagerly; many paid substitutes or evaded recruitment where they could. Exit runs through desertion accusations at minimum and apostasy charges where enforcement is strict.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, reluctant_muslim_subjects, payer,
    moderate, biographical, constrained, continental).

% Philologists and historians of exegesis, early biography, and Islamic law who document the verse's occasion of revelation, the competing revelation chronologies, and the wide range of abrogation counts in the classical literature. They take no side in the contest and bear none of its costs.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, academic_hermeneutic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, conquest_elites).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a dispersed tribal coalition under one divinely sanctioned war aim: it standardizes the grounds for hostilities, the rules for treaty termination, the division of spoils, and the boundary between combatant and protected, solving the mobilization and command-cohesion problems of an expansionary polity. Stated without evaluation.
% TRANSFER_FUNCTION: Moves sovereignty over territory, movable wealth, land title, and persons (enslavement and ransom) from non-submitted non-Muslim populations to the commanding polity and its fighters; afterward moves recurring tribute and labor services from submitted populations to the treasury; and moves obedience and military service from believers up to the command hierarchy.
% ABSENT_VOICES: Non-Muslim treaty counterparts whose agreements the doctrine voids without their consent have no negotiating seat; contextualist and rationalist exegetes inside Islam are silenced precisely when enforcement is strongest — which is also when unanimity behind the reading is cited as evidence for it. The absence of both seats is load-bearing for the reading's claim to speak for the tradition.
% DISAPPEARANCE_RATIONALE: If the standing obligation vanished overnight, conquest polities lose their charter and expansion coalitions their unifying aim; the terminal choice imposed on non-submitted populations dissolves into ordinary treaty relations; tribute, enslavement, and submission-status structures collapse; the sibling readings become the default account of the verse; and the movements organized around the obligation would fragment or rename their mission.
% FOUNDING_PROBLEM: A seventh-century Medinan coalition facing treaty-breaking Arab tribes after a decade of fragile truce needed a unified command decision: how to answer broken agreements, secure the polity's material base, and define who stood inside and outside the alliance. The verse's own season answered it with a four-month ultimatum followed by authorized war against the recalcitrant parties.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: occasion-of-revelation reports in the classical compilations of al-Wahidi and al-Tabari tie the verse to specific fourth-quarter treaty terminations of 9 AH; classical and modern contextualist jurists attest the occasion-bound application; academic historians of early Islam corroborate the geopolitical specificity of the crisis. The abrogationist movements themselves deny the problem is dead — universalizing the remedy is their charter — and no abrogationist source corroborates the founding-problem-is-over account; none is expected to, which is itself the signal.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the reading's terminal condition — conversion, formal submission with subordinate levies, or death — transfers sovereignty, wealth, land, and persons wholesale from the target class, and the 2014 territorial episode demonstrates the demand is executable, not merely declarative. Suppression is high (0.78) because persistence requires actively voiding treaties, criminalizing the rival exegetical seats, and punishing internal desertion; the suppression series traces enforcement buildups (mihna-era coercive orthodoxy, Ibn Taymiyyan escalation, 2014 total enforcement) and the post-1924 collapse. Theater is low-moderate (0.22) because when capacity exists the activity is executed rather than performed; the 1924 theater spike (0.58) marks declaratory maintenance without enforcing capacity — a dormancy symptom, not the constraint's normal register. Accessibility collapse (0.67) sits below mountain range because alternatives (sibling readings, ordinary treaty relations) survive wherever enforcement is weak, while collapsing nearly completely inside operated jurisdictions. Resistance (0.74) reflects fourteen centuries of armed external resistance and continuous internal scholarly contestation. All three metric series run on one shared nine-point grid so no row is backfilled. The trajectory is cyclical rather than monotonic: capacity-driven rise, collapse, revival — driven by the presence or absence of an enforcing sovereign, not by intermittent reinforcement, though revival-period propaganda does use intermittent reward tactically. The base_property scalars encode the standing arrangement's intrinsic demand profile (peak-realized); the series encodes realized operation including the dormancy trough.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structure. From the non_submitted_polytheists seat the arrangement is a terminal choice with no exit — maximal-target classification. From the conquest_elites seat it is a divinely warranted mandate paying spoils, land, and tribute — near-full-beneficiary classification. From the ulama establishment seat it is a custodial juridical order whose guarding constitutes their office. From the reluctant_muslim_subjects seat it is a heavy civic burden wrapped in sacral necessity. From the observer seat it is a hermeneutical contest with a measurable body count. None of these is the constraint; the engine computes each seat's classification from the structural data, and the divergence between them is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Full-beneficiary end: conquest_elites (direct receipt of spoils, tribute, and land, with arbitrage-grade maneuver — they historically suspended the doctrine when truce paid better), ghazi_frontier_warriors (plunder shares, status, martyrdom premium), expansionist_jihad_movements (organizational charter, recruitment engine, governing prerogative), and the ulama establishment (authority rents from doctrinal custody — collected as standing, students, and endowments rather than treasure). Full-target end: non_submitted_polytheists (powerless, trapped, generational exposure to the terminal choice — d approaches 1.0), non_muslim_treaty_counterparts (consent voided, no seat), contextualist_scholars (suppressed internal dissenters whose livelihood and safety the enforcement machinery attacks directly). Mixed seat: reluctant_muslim_subjects hold role payer (conscription, war taxation, death risk) but share incidental benefit in the order produced; their true directionality is mid-range, and the derivation may overshoot toward the target end — I decline an override because overrides key on power_atom, and three distinct moderate-power seats (treaty counterparts, contextualist scholars, reluctant subjects) need three different directionalities the single-atom override surface cannot express. Identity-lock cuts differently by seat: for beneficiary movements it deepens commitment to the mandate; for the ulama and contextualist scholars it traps opponents and custodians alike inside the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the specific seventh-century Medinan crisis of treaty-breaking tribes and coalition cohesion — is dead: its parties, season, and stakes are fifteen centuries gone. Yet the arrangement persists as a standing obligation, producing exactly the R5 mismatch (founding_problem_status=dead x disappearance_verdict=world_rearranges) that flags zombie/capture dynamics: the obligation now serves expansionist movements' organizational and recruiting needs, not the original crisis. The snare classification prevents three mislabelings. Against rope: the divine-unity framing presents genuine-looking coordination (mobilization, spoils division, boundary maintenance), but the coordinated activity is itself the harm to the target class — coordination clothing on an extraction machine, which is the snare signature, not a coordination dividend. Against piton: the post-1924 theater spike mimics inertial decay, but the 2014 re-execution proves the function revivable, so theatricality is a capacity symptom rather than the definition. Against mountain: the reading presents itself as eternal divine law, but emerges_naturally is false — it is authored jurisprudence meeting sustained resistance (0.74) with live rival readings, the opposite of a natural-law profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Which reading of the quran_9_5_scope kernel does a given community actually instantiate, and what follows for the victim set and extraction profile?',
    'Cross-reference each community''s operative rulings: whether treaty obligations survive, whether war aims are bounded and defensive, whether the obligation is standing and universal. This file authors only the abrogating_universal reading; the contextual_defensive and progressive_synthesis files instantiate their own constraints.',
    'Adopting contextual_defensive shrinks the victim set to the treaty-breaking parties of a closed seventh-century season and drops extractiveness toward bounded defensive-coordination levels; adopting progressive_synthesis converts the arrangement into a historical directive without standing force, collapsing the victim set entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'This constraint is one of three competing readings of one verse-scope kernel; classification is reading-relative by design.').

omega_variable(
    abrogation_chain_validity,
    'Does the abrogation verdict survive philological scrutiny — is there a stable revelation chronology, and do the verses declared abrogated reliably postdate 9:5 rather than precede it?',
    'Critical collation of revelation-chronology traditions (the Egyptian standard sequence, Nöldeke ordering, lectionary variants), the classical abrogation-list corpus (whose verse counts range from a handful to over two hundred), and manuscript evidence for variant orderings.',
    'If the chain fails, the reading loses its textual warrant and collapses into contextual_defensive, cutting the victim set and extractiveness drastically; if it holds within the tradition''s own interpretive methodology, the reading stands on conventional grounds and the contest shifts to whether that methodology binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_chain_validity, empirical, 'Textual sustainability of the naskh verdict underlying the universal obligation.').

omega_variable(
    victim_set_boundary,
    'Where does the terminal condition terminate: at polytheists offered a submission-and-tribute path, at scriptuary religions folded into submission terms, or at every non-submitter without exception?',
    'Compare the reading''s actual application across regimes and movements operating it: treatment of Jewish, Christian, Zoroastrian, Hindu, and Yazidi populations under tribute-and-subordination versus extermination tracks.',
    'A polytheist-bounded reading narrows the victim class and lowers extractiveness substantially; the universal boundary witnessed in the 2014 enslavement and killing of Yazidis maximizes it — the boundary choice moves roughly half the measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Scope of the victim class under the standing obligation.').

omega_variable(
    enforcement_sovereign_dependence,
    'Does the standing obligation require an enforcing sovereign to operate, or does it persist as a latent mandate that revives whenever organizational capacity appears?',
    'Comparative analysis of the 1924-1979 dormancy against the post-1979 revivals: whether doctrine, recruitment networks, and targeting doctrine continued developing without state enforcement, indicating latency rather than extinction.',
    'Latent persistence supports classifying the standing arrangement continuously across the dormancy trough; sovereign dependence would argue for decomposing the post-1924 phase into a separately-authored dormant constraint with its own lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sovereign_dependence, empirical, 'Whether the post-caliphate dormancy is an interruption of the constraint or a hibernation of it.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression is structural (courts, capital enforcement, destruction of rival institutions) versus internalized (self-censorship by believers who fear accusation without any court being involved)?',
    'Measure dissent-survival rates across jurisdictions matched for doctrine but differing in enforcement intensity; assess internalized reluctance where no enforcement machinery operates.',
    'A large internalized share raises effective suppression above the structural measure and predicts slower decay of the reading after enforcement removal; a small share means dismantling the enforcement machinery suffices to release the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the suppression load.').

omega_variable(
    identity_lock_reversibility,
    'If senior clerical authorities collectively repudiated the abrogation verdict, would the movements holding this reading deflate into the contextual reading, or re-derive the obligation from parallel sources (the hadith corpus, conquest-era precedent) and persist?',
    'Track movement responses to the major clerical repudiations already issued against the 2014 territorial movement: whether recruitment and doctrine shifted toward the contextual frame or re-anchored in non-kernel sources.',
    'Irreversibility implies the constraint outlives its textual kernel and should be tracked as a distinct persistent structure; reversibility ties its fate to the kernel contest and to the sibling readings'' fortunes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Durability of the reading against loss of its textual warrant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 632, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95au_tr_t632, quran_9_5_scope__abrogating_universal, theater_ratio, 632, 0.1).
narrative_ontology:measurement(q95au_tr_t750, quran_9_5_scope__abrogating_universal, theater_ratio, 750, 0.1).
narrative_ontology:measurement(q95au_tr_t900, quran_9_5_scope__abrogating_universal, theater_ratio, 900, 0.14).
narrative_ontology:measurement(q95au_tr_t1258, quran_9_5_scope__abrogating_universal, theater_ratio, 1258, 0.18).
narrative_ontology:measurement(q95au_tr_t1550, quran_9_5_scope__abrogating_universal, theater_ratio, 1550, 0.24).
narrative_ontology:measurement(q95au_tr_t1924, quran_9_5_scope__abrogating_universal, theater_ratio, 1924, 0.58).
narrative_ontology:measurement(q95au_tr_t1979, quran_9_5_scope__abrogating_universal, theater_ratio, 1979, 0.34).
narrative_ontology:measurement(q95au_tr_t2014, quran_9_5_scope__abrogating_universal, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(q95au_tr_t2024, quran_9_5_scope__abrogating_universal, theater_ratio, 2024, 0.21).

% Extraction over time
narrative_ontology:measurement(q95au_be_t632, quran_9_5_scope__abrogating_universal, base_extractiveness, 632, 0.72).
narrative_ontology:measurement(q95au_be_t750, quran_9_5_scope__abrogating_universal, base_extractiveness, 750, 0.8).
narrative_ontology:measurement(q95au_be_t900, quran_9_5_scope__abrogating_universal, base_extractiveness, 900, 0.83).
narrative_ontology:measurement(q95au_be_t1258, quran_9_5_scope__abrogating_universal, base_extractiveness, 1258, 0.81).
narrative_ontology:measurement(q95au_be_t1550, quran_9_5_scope__abrogating_universal, base_extractiveness, 1550, 0.76).
narrative_ontology:measurement(q95au_be_t1924, quran_9_5_scope__abrogating_universal, base_extractiveness, 1924, 0.48).
narrative_ontology:measurement(q95au_be_t1979, quran_9_5_scope__abrogating_universal, base_extractiveness, 1979, 0.63).
narrative_ontology:measurement(q95au_be_t2014, quran_9_5_scope__abrogating_universal, base_extractiveness, 2014, 0.89).
narrative_ontology:measurement(q95au_be_t2024, quran_9_5_scope__abrogating_universal, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(q95au_su_t632, quran_9_5_scope__abrogating_universal, suppression_requirement, 632, 0.5).
narrative_ontology:measurement(q95au_su_t750, quran_9_5_scope__abrogating_universal, suppression_requirement, 750, 0.64).
narrative_ontology:measurement(q95au_su_t900, quran_9_5_scope__abrogating_universal, suppression_requirement, 900, 0.73).
narrative_ontology:measurement(q95au_su_t1258, quran_9_5_scope__abrogating_universal, suppression_requirement, 1258, 0.79).
narrative_ontology:measurement(q95au_su_t1550, quran_9_5_scope__abrogating_universal, suppression_requirement, 1550, 0.68).
narrative_ontology:measurement(q95au_su_t1924, quran_9_5_scope__abrogating_universal, suppression_requirement, 1924, 0.38).
narrative_ontology:measurement(q95au_su_t1979, quran_9_5_scope__abrogating_universal, suppression_requirement, 1979, 0.57).
narrative_ontology:measurement(q95au_su_t2014, quran_9_5_scope__abrogating_universal, suppression_requirement, 2014, 0.92).
narrative_ontology:measurement(q95au_su_t2024, quran_9_5_scope__abrogating_universal, suppression_requirement, 2024, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, resource_allocation).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The colloquial label 'the ruling of the Verse of the Sword' conflates three structurally distinct claims: (1) an occasion-bound ruling on treaty-breakers (contextual_defensive), (2) an abrogation installing a universal standing offensive obligation (this file, abrogating_universal), and (3) a time-bound political directive superseded by the ethical trajectory (progressive_synthesis). Per the epsilon-invariance principle each claim gets its own story, epsilon, victim set, and type; the three form a constraint family linked through affects_constraints. This member is the maximally extractive pole: its success in the contest raises the stakes and reshapes the legitimacy conditions under which the siblings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
