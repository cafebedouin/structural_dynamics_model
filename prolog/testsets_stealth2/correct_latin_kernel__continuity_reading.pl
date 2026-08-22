% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Continuity Reading of the Correct-Latin Standard (Medieval Latin as Naturally Evolved Classical Latin)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the continuity_reading of the
 *   correct_latin_kernel: the standing arrangement under contest is the
 *   medieval Latin written standard together with its correction regime,
 *   assessed by this reading's own lights — medieval usage as classical Latin
 *   carried forward by natural linguistic evolution, and reconstruction as
 *   internal correction within the tradition. On this reading the arrangement
 *   is a legitimate, self-repairing coordination achievement whose costs are
 *   the ordinary price of maintaining a learned standard; the humanist demand
 *   to reoccupy the classical texts reads as prescriptive interference rather
 *   than recovery. The claim and the metrics are authored independently: the
 *   claimed type states the structure I take to be true of the arrangement
 *   (genuine coordination carrying asymmetric extraction under active
 *   enforcement), while the metrics describe its operation as this reading
 *   can honestly assess it. Sibling readings — discontinuity_reading and
 *   hybrid_reading — are separate stories linked through the network section;
 *   they assess the same historical arrangement against a different ground
 *   and will author different epsilon over the shared referent. KEY AGENTS
 *   (by structural relationship): - curial_chancery_authorities:
 *   agenda-setting seat (institutional/identity_locked) — administers the
 *   standard, collects fees and deference, cannot abandon the continuity
 *   identification without dissolving its own office -
 *   grammar_school_masters: beneficiary seat (organized/constrained) —
 *   collects fees and standing from the teaching economy - monastic_copyists:
 *   beneficiary seat (organized/constrained) — staffs the transmission and
 *   correction economy - clerical_students: primary target
 *   (powerless/trapped) — bears the training and correction burden -
 *   vernacular_authors: target at the boundary (moderate/constrained) —
 *   capped below the learned registers - unlettered_parish_laity: diffuse
 *   target (powerless/trapped) — governed by texts it cannot read -
 *   women_excluded_from_schools: diffuse target (powerless/trapped) — barred
 *   from the credentialing path - humanist_reformers: excluded challenger
 *   (powerful/mobile) — carries the rival readings' demand for textual
 *   reoccupation - comparative_philologists: analytical observer
 *   (analytical/analytical) — tests the continuity claim against the full
 *   corpus
 *
 * KEY AGENTS:
 *   - curial_chancery_authorities: agenda-setting seat (institutional/identity_locked) — administers the standard, collects fees and deference, cannot abandon the continuity identification without dissolving its own office
 *   - grammar_school_masters: beneficiary seat (organized/constrained) — collects fees and standing from the teaching economy
 *   - monastic_copyists: beneficiary seat (organized/constrained) — staffs the transmission and correction economy
 *   - clerical_students: primary target (powerless/trapped) — bears the training and correction burden
 *   - vernacular_authors: target at the boundary (moderate/constrained) — capped below the learned registers
 *   - unlettered_parish_laity: diffuse target (powerless/trapped) — governed by texts it cannot read
 *   - women_excluded_from_schools: diffuse target (powerless/trapped) — barred from the credentialing path
 *   - humanist_reformers: excluded challenger (powerful/mobile) — carries the rival readings' demand for textual reoccupation
 *   - comparative_philologists: analytical observer (analytical/analytical) — tests the continuity claim against the full corpus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.44).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.6).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Continuity Reading of the Correct-Latin Standard (Medieval Latin as Naturally Evolved Classical Latin)").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '7d474406-398b-46e1-9e65-e402dc3629c4').
narrative_ontology:cs_kernel_codification('7d474406-398b-46e1-9e65-e402dc3629c4', formalized).
narrative_ontology:cs_authority_grounding('7d474406-398b-46e1-9e65-e402dc3629c4', lineage).
narrative_ontology:cs_interpretation_layer_present('7d474406-398b-46e1-9e65-e402dc3629c4').
narrative_ontology:cs_reading_relation('7d474406-398b-46e1-9e65-e402dc3629c4', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7d474406-398b-46e1-9e65-e402dc3629c4', correct_latin_kernel__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('7d474406-398b-46e1-9e65-e402dc3629c4', foundational, tradition_self_sufficient_for_correction).
narrative_ontology:cs_axiom_status(tradition_self_sufficient_for_correction, holdable).
narrative_ontology:cs_axiom_grounding('7d474406-398b-46e1-9e65-e402dc3629c4', tradition_self_sufficient_for_correction, empirically_contingent).
narrative_ontology:cs_axiom('7d474406-398b-46e1-9e65-e402dc3629c4', foundational, language_change_is_drift_not_decay).
narrative_ontology:cs_axiom_status(language_change_is_drift_not_decay, holdable).
narrative_ontology:cs_axiom_grounding('7d474406-398b-46e1-9e65-e402dc3629c4', language_change_is_drift_not_decay, empirically_contingent).
narrative_ontology:cs_axiom('7d474406-398b-46e1-9e65-e402dc3629c4', secondary, humanist_recovery_is_prescriptive_interference).
narrative_ontology:cs_axiom_status(humanist_recovery_is_prescriptive_interference, holdable).
narrative_ontology:cs_axiom_grounding('7d474406-398b-46e1-9e65-e402dc3629c4', humanist_recovery_is_prescriptive_interference, conventional).
narrative_ontology:cs_reference_frame('7d474406-398b-46e1-9e65-e402dc3629c4', unbroken_transmission_framework).
narrative_ontology:cs_drift_state('7d474406-398b-46e1-9e65-e402dc3629c4', post_humanist_philological_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d474406-398b-46e1-9e65-e402dc3629c4', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, curial_chancery_authorities).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, grammar_school_masters).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, monastic_copyists).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, clerical_students).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, vernacular_authors).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, unlettered_parish_laity).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, women_excluded_from_schools).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, natural_language_evolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Papal and episcopal chanceries, together with university faculties, license teachers, prescribe the grammars taught, and order the correction of books and men. Their office rests on identifying the living written tradition with the language of Rome itself; an incumbent who renounced that identification would dissolve the ground of his own authority, so the identification is not something they can step away from while holding office. Fees, appointments, and doctrinal oversight flow through their hands.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, curial_chancery_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, curial_chancery_authorities, beneficiary).

% Cathedral and monastery schoolmasters teach Donatus and Priscian, hear lessons, and administer correction to pupils. Teaching fees, board, and standing in the town flow to them from the demand the standard creates. Their skill is specific to this curriculum; a master who abandoned it would start over without trade or title.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, grammar_school_masters, beneficiary,
    organized, biographical, constrained, regional).

% Scriptorium monks copy, emend, and compare manuscripts against the house exemplars, sustaining the transmission economy. The work gives them provision, rank within the cloister, and a recognized office; leaving it means leaving the community that houses them. They also bear the labor of the correction rounds when new exemplars arrive.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, monastic_copyists, beneficiary,
    organized, biographical, constrained, regional).

% Boys and young men committed to clerical careers spend years memorizing grammar under corporal discipline, submitting their compositions to public correction. The investment is sunk once vows approach; a student who abandons the track forfeits the years and carries the shame of a broken vocation.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, clerical_students, payer,
    powerless, biographical, trapped, regional).

% Writers working in the emerging vernaculars produce chronicles, romance, and devotion outside the learned registers. Theology, law, and high office require the Latin standard, so their ambitions are capped at a second-tier cultural rank unless they convert to Latin composition, shedding their audience.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, vernacular_authors, payer,
    moderate, biographical, constrained, national).

% Parishioners encounter scripture, charters, and judgments only through clerical mediation. They cannot read the texts that govern their marriages, tithes, and land, and no local alternative register exists for those functions.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, unlettered_parish_laity, payer,
    powerless, biographical, trapped, local).

% Girls are generally barred from the grammar schools and from the orders that confer the standard's credentials; convent scriptrices are the narrow exception. Exclusion from the learned language compounds exclusion from office, property litigation, and doctrinal speech.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, women_excluded_from_schools, payer,
    powerless, biographical, trapped, local).

% Italian circles of text-hunters recover classical manuscripts and argue that usage must answer to Cicero rather than to school practice. They sit outside the licensing conversation that administers the standard, publishing prefaces and seeking princely patronage to build rival channels of credential.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_reformers, excluded,
    powerful, biographical, mobile, continental).

% Later analysts compare corpora across the whole span of the language, reconstructing sound change and usage statistically. They collect nothing from the arrangement and bear none of its costs; their seat exists to test claims like continuity against the full record.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, comparative_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, curial_chancery_authorities).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one supraregional written standard so that law, theology, diplomacy, and scholarship remain mutually intelligible across a linguistically fragmented continent and across centuries; the correction regime repairs divergences before they break mutual intelligibility.
% TRANSFER_FUNCTION: Moves years of training labor, fees, and deference from learners and peripheral users to the licensed master class and the central chanceries; moves authority over texts, offices, and doctrine to those certified in the standard.
% ABSENT_VOICES: Vernacular authors, unlettered laity, and women barred from schooling would object that the standard prices them out of scripture, law, and learned office; humanist reformers stand outside the licensing conversation insisting the standard answer to recovered classical texts rather than to its own practice.
% DISAPPEARANCE_RATIONALE: Overnight loss of the standard would sever cross-border correspondence, freeze courts and chanceries that conduct business in Latin, strand university teaching, and force immediate vernacular improvisation in law and theology — the learned world rearranges within a generation.
% FOUNDING_PROBLEM: After the western empire's administrative collapse, mutually unintelligible vernaculars emerged while the authoritative inheritance (scripture, Roman law, patristics) remained in Latin; the arrangement was built to keep that inheritance usable and learned communication possible.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: municipal and mercantile records show contracts and correspondence crossing dialect boundaries conducted in Latin, attesting the coordination problem was real, while vernacular chronicles and lay petitions record the price of exclusion; comparative philologists corroborate both faces from an uninterested seat. No corroborating voice attests the problem was already solved before the interval's end.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.44 by this reading's lights: the coordination achievement is real and load-bearing, but the acquisition burden grows as the taught standard recedes from living speech, and deference flows upward to the licensed center. Suppression (0.60) is a raw structural figure, unscaled by power or scope: it reflects licensing gates, correction discipline, and the policing of vernacular scripture, not any computed quantity. Theater (0.38) marks the share of grammatical activity that performs inherited authority rather than repairing intelligibility — pedagogy repeating Priscian long after the norms' original function shifted. Accessibility_collapse (0.42) is moderate: vernaculars and Greek persist as alternatives, but both are priced out of theology, law, and office, so the alternative set narrows sharply exactly where the standard's stakes are highest. Resistance (0.50) is episodic — vernacular literary movements, occasional lay complaint, and, at the interval's end, the humanist challenge. The three measurement series share one grid (0, 200, 400, 600, 700, 800, approximating 600-1400 CE); suppression_requirement is tracked because enforcement capacity visibly builds over the interval — Carolingian standardization, then university licensing, then measures against vernacular scripture — rather than holding static. Coalition potential among the payer seats was structurally blocked: the laity and women were dispersed and unlettered, students were transient and hierarchically supervised, and vernacular authors lacked a shared grievance channel, so no payer coalition formed against the standard inside the interval. Continental scope raises verification difficulty and thereby amplifies effective extraction at the target seats; the engine owns that arithmetic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat experiences the arrangement as the natural shape of learned life: it built the schools, hears the correction rounds, and identifies its own office with the tradition's continuity, so its computed extraction is damped toward subsidy. The trapped payer seats — students, laity, women — experience the same structure as discipline and price: years taken, texts unreadable, offices unreachable. Between them, the beneficiary seats (masters, copyists) collect steadily while bearing real labor, computing somewhere between. Nothing in the authored claim adjudicates these differences; the engine derives them from role, power, exit, and scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (the curial authorities as collectors, the masters, the copyists) derive low directionality — the arrangement subsidizes them — with the curia pushed toward the beneficiary end despite its administrative burden because its gains are concentrated and its exit is closed by identity. Payers derive high directionality, and their trapped exits push them toward the full-target end: a student who has sunk years, a laity with no alternative register, women with no path in at all. Vernacular authors sit slightly less extreme: constrained rather than trapped, with a mobile fringe that converts to Latin composition. No directionality overrides are declared — the beneficiary/victim declarations plus the exit atoms already yield the relationships the story asserts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a shared learned language usable across a fragmented linguistic landscape — remains live throughout the interval, so no mandatrophy is declared. The tangled-rope classification is what prevents mislabeling in both directions: reading the arrangement as a pure snare erases the coordination function whose overnight loss would rearrange the learned world; reading it as a pure rope erases the identifiable payers whose exclusion is enforced, not incidental. The humanist challenge at the interval's edge is the stress test to watch: if the discontinuity reading wins the ground, the founding problem's original formulation dies and the arrangement's persistence becomes inertia — the transition this corpus's lifecycle machinery is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the correct_latin_kernel. What would adopting a sibling reading — discontinuity_reading or hybrid_reading — change structurally in the classification of the same historical arrangement?',
    'Generate and classify the sibling stories against the shared referent (the medieval standard and its correction regime) and compare per-seat outputs; the disagreement is located in whether the transmitted tradition is self-sufficient as the standard.',
    'Under the discontinuity reading the same arrangement computes as enforcing a standard it no longer embodies — extraction and theater rise sharply and the victim set gains every user corrected against a lost referent; under the hybrid reading intermediate values obtain. This file''s numbers are valid only for the continuity seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the correct-Latin kernel; sibling adoption shifts epsilon, victim sets, and theater for the identical arrangement.').

omega_variable(
    continuity_claim_epistemic_status,
    'Does the continuity thesis function as a description of how the language developed, or as a legitimacy charter whose truth is secondary to its office in licensing the tradition''s authorities?',
    'Independent corpus comparison of medieval usage against classical benchmarks, read separately from the tradition''s own grammatical doctrine; if the doctrine''s claims outrun what the corpora support, the charter reading is favored.',
    'If charter-like, the theater ratio understates performative maintenance and the arrangement drifts toward inertial persistence; if descriptive, the coordination framing stands and the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_epistemic_status, empirical, 'Whether the continuity claim is linguistic fact or legitimating instrument.').

omega_variable(
    exclusion_or_price,
    'Are the vernacular authors, unlettered laity, and women barred from schooling victims of the standard''s enforcement, or merely priced out of a costly good — training — that no one owed them?',
    'Compare access policy across regions and centuries: whether exclusion tracks enforceable gating choices (school licensing, orders'' admission rules, language ordinances) or resource scarcity alone; examine regions that widened access as natural experiments.',
    'If victims, pressure toward the snare end of the hybrid range and the payer seats'' directionality saturates; if priced-out, the rope component dominates and the arrangement approaches a heavy-but-honest coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_or_price, conceptual, 'Status of the excluded populations: enforced victims or voluntary non-purchasers.').

omega_variable(
    carolingian_correction_function,
    'Was the Carolingian correction campaign restoring mutual intelligibility across a degraded manuscript tradition, or asserting central authority over regional script communities?',
    'Measure regional textual variance and intelligibility before and after the standardized exemplars spread; separate the effect of emendation from the effect of liturgical and administrative uniformity.',
    'If functional, the early-interval suppression requirement is overstated and the coordination floor is firmer; if extractive, extraction accumulates from the start and the rising series understates the trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carolingian_correction_function, empirical, 'Function versus authority-display in the interval''s largest correction campaign.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_continuity_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(clk_continuity_tr_t0, observed).
narrative_ontology:measurement(clk_continuity_tr_t200, correct_latin_kernel__continuity_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(clk_continuity_tr_t200, observed).
narrative_ontology:measurement(clk_continuity_tr_t400, correct_latin_kernel__continuity_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(clk_continuity_tr_t400, observed).
narrative_ontology:measurement(clk_continuity_tr_t600, correct_latin_kernel__continuity_reading, theater_ratio, 600, 0.27).
narrative_ontology:measurement_basis(clk_continuity_tr_t600, observed).
narrative_ontology:measurement(clk_continuity_tr_t700, correct_latin_kernel__continuity_reading, theater_ratio, 700, 0.33).
narrative_ontology:measurement_basis(clk_continuity_tr_t700, observed).
narrative_ontology:measurement(clk_continuity_tr_t800, correct_latin_kernel__continuity_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement_basis(clk_continuity_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(clk_continuity_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(clk_continuity_be_t0, observed).
narrative_ontology:measurement(clk_continuity_be_t200, correct_latin_kernel__continuity_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement_basis(clk_continuity_be_t200, observed).
narrative_ontology:measurement(clk_continuity_be_t400, correct_latin_kernel__continuity_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement_basis(clk_continuity_be_t400, observed).
narrative_ontology:measurement(clk_continuity_be_t600, correct_latin_kernel__continuity_reading, base_extractiveness, 600, 0.41).
narrative_ontology:measurement_basis(clk_continuity_be_t600, observed).
narrative_ontology:measurement(clk_continuity_be_t700, correct_latin_kernel__continuity_reading, base_extractiveness, 700, 0.43).
narrative_ontology:measurement_basis(clk_continuity_be_t700, observed).
narrative_ontology:measurement(clk_continuity_be_t800, correct_latin_kernel__continuity_reading, base_extractiveness, 800, 0.44).
narrative_ontology:measurement_basis(clk_continuity_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(clk_continuity_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(clk_continuity_su_t0, observed).
narrative_ontology:measurement(clk_continuity_su_t200, correct_latin_kernel__continuity_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement_basis(clk_continuity_su_t200, observed).
narrative_ontology:measurement(clk_continuity_su_t400, correct_latin_kernel__continuity_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement_basis(clk_continuity_su_t400, observed).
narrative_ontology:measurement(clk_continuity_su_t600, correct_latin_kernel__continuity_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement_basis(clk_continuity_su_t600, observed).
narrative_ontology:measurement(clk_continuity_su_t700, correct_latin_kernel__continuity_reading, suppression_requirement, 700, 0.56).
narrative_ontology:measurement_basis(clk_continuity_su_t700, observed).
narrative_ontology:measurement(clk_continuity_su_t800, correct_latin_kernel__continuity_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement_basis(clk_continuity_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' conflates three structurally distinct claims about the same historical arrangement, per the epsilon-invariance principle: continuity (this file — the tradition is self-sufficient; lowest contested extraction), discontinuity (the systems are distinct; reconstruction required reoccupation from texts — highest extraction, since every user is corrected against a lost referent), and hybrid (morphology continuous, syntax and lexicon recovered — intermediate). Each is authored as its own story with its own epsilon, stakeholders, and type. The upstream continuity claim influenced the others because centuries of institutional legitimacy rested on it; the sibling stories attack or amend exactly that ground. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
