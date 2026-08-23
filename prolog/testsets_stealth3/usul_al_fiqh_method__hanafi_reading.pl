% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Usul al-Fiqh Derivation Method — Hanafi Reading (Expansive Qiyas, Ra'y, Istihsan)
 *   domain: legal/jurisprudential/religious
 *
 * SUMMARY:
 *   This story authors ONE reading of the usul_al_fiqh_method kernel: the
 *   Hanafi regime in which analogical reasoning (qiyas) is expansively
 *   applicable wherever the texts are silent, reasoned opinion (ra'y)
 *   supplements analogy at its limits, and juristic preference (istihsan)
 *   licenses departure from strict analogy for public interest. The standing
 *   arrangement under contest — the referent of epsilon — is this
 *   methodological regime as it actually operated from the formative Iraqi
 *   period through madhhab institutionalization, state adoption,
 *   codification, and the modern era, assessed by this reading's own lights.
 *   The sibling readings (maliki_reading, shafii_reading, hanbali_reading)
 *   are separate constraints with their own epsilon values and seat
 *   structures; nothing here averages over them. The claim/metric gap is
 *   deliberate: the school CLAIMS the method as faithful extension of
 *   revelation (its own framing is coordination-faithful), while the authored
 *   metrics describe a substantially extractive, actively enforced
 *   arrangement whose gains concentrate in the jurist class — the engine
 *   measures that divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - hanafi_jurist_class: agenda-setter and principal collector (institutional / identity_locked) — administers the derivation method, decides valid analogy and justified departure, and accrues its authority, offices, and material support
 *   - ruling_administrative_establishment: secondary beneficiary (institutional / mobile) — obtains an adaptable legal instrument by staffing judgeships from the school
 *   - textualist_hadith_specialists: primary structured opponent among the learned (organized / constrained) — their authentication-based authority is demoted by the method's priorities
 *   - lay_subjects_of_juristic_discretion: diffuse bearer of the method's outputs (powerless / trapped) — bound by rulings they had no hand in shaping
 *   - rival_madhhab_jurists: excluded challengers (organized / mobile) — locked out of the posts the method staffs, sustaining parallel schools
 *   - comparative_legal_historians: analytical observer (analytical / analytical) — external check on the school's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.5).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Usul al-Fiqh Derivation Method — Hanafi Reading (Expansive Qiyas, Ra'y, Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/jurisprudential/religious").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5').
narrative_ontology:cs_kernel_codification('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', formalized).
narrative_ontology:cs_authority_grounding('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', lineage).
narrative_ontology:cs_interpretation_layer_present('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5').
narrative_ontology:cs_reading_relation('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', usul_al_fiqh_method__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', foundational, istihsan_valid_public_interest_override).
narrative_ontology:cs_axiom_status(istihsan_valid_public_interest_override, holdable).
narrative_ontology:cs_axiom_grounding('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', istihsan_valid_public_interest_override, instrumental).
narrative_ontology:cs_axiom('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', foundational, expansive_qiyas_default_on_silence).
narrative_ontology:cs_axiom_status(expansive_qiyas_default_on_silence, holdable).
narrative_ontology:cs_axiom_grounding('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', expansive_qiyas_default_on_silence, conventional).
narrative_ontology:cs_reference_frame('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', rationalist_juristic_supremacy).
narrative_ontology:cs_drift_state('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', contemporary_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e7d3a97-39a9-4dfe-84f0-a4e5d670b1e5', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, ruling_administrative_establishment).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_hadith_specialists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_subjects_of_juristic_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains for decades in the derivation method, applies qiyas, ra'y, and istihsan to produce rulings, staffs judge and mufti posts, teaches in madrasas, and decides what counts as a valid analogy or a justified departure from one. Stipends, consultation fees, and endowment support flow to its members, and adjudicative authority concentrates in its hands precisely because expansive analogy makes trained mastery indispensable. Leaving the school means discarding a lifetime of methodological formation, professional standing, and the salvation-framed duty to follow a recognized school — exit is identity death, not relocation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, beneficiary).

% Appoints chief judges from the school (beginning with Abu Yusuf under the Abbasids) and thereby obtains a legal apparatus flexible enough to handle fiscal innovation, new contract forms, and administrative novelty without waiting for textual warrant. Patronage can be shifted between schools when politically convenient, so its attachment to this particular method is strategic rather than fused.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, ruling_administrative_establishment, beneficiary,
    institutional, generational, mobile, continental).

% Their authority rests on transmitting, memorizing, and authenticating reports from the Prophet and Companions. Under a regime where expansive analogy outranks weak reports and juristic preference can override strict analogy, custody of the texts is demoted to an input among others, and rulings arrive that their authentication craft would never license. Their exit runs toward anti-analogy currents (the Zahiri school) or rival regions at the cost of state access and institutional livelihood.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_hadith_specialists, payer,
    organized, biographical, constrained, continental).

% Ordinary Muslims are bound by marriage, inheritance, commercial, and penal rulings derived through analogy and juristic preference rather than explicit revelation. They carry the consequences of jurist error or captured judgment with no seat in the methodological councils that decide what binds them, and no practical exit from the law that governs their families and property.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_subjects_of_juristic_discretion, payer,
    powerless, biographical, trapped, continental).

% Shafi'i, Maliki, and Hanbali colleagues are locked out of judgeships and teaching posts in territories administered by the school, and their methodological objections (most forcefully al-Shafi'i's attack on istihsan) are answered from inside institutions they do not control. They sustain parallel schools and would contest the derivation premises directly if seated in the appointments and academies this method staffs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rival_madhhab_jurists, excluded,
    organized, generational, mobile, continental).

% Reconstruct the method's formation and drift from outside the tradition, reading all four schools as variant answers to one coverage problem. They neither collect from nor bear the method's operation, and their accounts are the main external check on the school's self-description.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the case-coverage problem: a finite corpus of revealed texts must yield rulings for an unbounded stream of novel situations — new contracts, transactions, penalties, rituals — with no living prophet to consult. The method standardizes how inference extends the texts, so judges across regions and generations reach mutually intelligible, roughly consistent decisions instead of ad hoc personal verdicts.
% TRANSFER_FUNCTION: Moves adjudicative authority and interpretive jurisdiction from the transmitted texts (and their specialist custodians) to the trained jurist class; moves binding rulings to the lay population derived from jurist judgment rather than explicit revelation; and moves material support — judge stipends, consultation fees, endowment income, state patronage — to the jurists who operate the method.
% ABSENT_VOICES: Textualist hadith specialists object from outside the school's institutions, rival-school jurists are excluded from the posts and academies the method staffs, and the lay population bound by the resulting rulings has no seat at all in the methodological councils — the people bound by istihsan-derived decisions never participate in deciding the method that produces them.
% DISAPPEARANCE_RATIONALE: If the method vanished overnight, legal answers to every novel case would halt or fragment into uncoordinated personal opinion: judges would have no shared procedure, commercial and family life would lose predictable rulings, the jurist class would lose its office and income structure, and the state would lose the adaptive legal instrument it appointed the school to provide. Arrangements across the entire legal-output apparatus depend on it.
% FOUNDING_PROBLEM: After the Prophet's death, the community faced unprecedented questions with no living recipient of revelation. Abu Hanifa's circle in Kufa built systematic reasoning procedures — analogy from textual cases, reasoned opinion where analogy ran out, and controlled departure from analogy where public interest demanded — to extend the revealed corpus to cases the texts never addressed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the rival schools attest the coverage problem is real even while disputing the Hanafi solution — al-Shafi'i's own Risala addresses the same problem with a different source hierarchy, and Hanbali and Maliki literatures concede the novelty-of-cases difficulty they answer differently. Academic legal history (the Schacht-lineage studies and their successors) independently documents the problem as the driver of the method's formation. No serious participant, inside or outside the school, claims the founding problem is solved.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.68 because the method's gains concentrate measurably: expansive analogy manufactures permanent demand for trained jurists (every novel case requires mastery only they certify), istihsan hands them a licensed override of the very constraints textualists appeal to, and the school's institutionalization converted earned authority into inherited authority under taqlid. Suppression is 0.50 — real but bounded: madhhab boundaries, state appointment of Hanafi judges, and curricular control enforced conformity, yet rival schools survived and flourished, so alternatives were disadvantaged, not eliminated. Theater_ratio is 0.36 and rising across the interval: the formative generations produced rulings under visible methodological strain, while later taqlid-era practice increasingly performed fidelity to the founders while mechanically recycling compiled precedent. Accessibility_collapse is low (0.25) because the three sibling readings remain fully live, practiced alternatives — understanding this method does not close off the others. Resistance is 0.60: al-Shafi'i's polemic against istihsan, Hanbali minimization of qiyas, and the Zahiri secession from analogy altogether are sustained, organized resistance from inside the learned class. The measurement series run on one shared time grid (points 0–30 at steps of 5) so every tracked metric is authored at every examined point; the suppression series traces the enforcement arc — informal master-disciple networks, madhhab consolidation, Abbasid state appointment, peak conformity, Ottoman codification absorbing discretion, modern-state and Salafi erosion — which is why suppression_requirement is tracked here rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   From the jurist seat, the method is liberation: revelation rescued from textual poverty, divine law extended to every human circumstance, the jurist as trustee of the Lawgiver's purposes. From the textualist seat, the same structure is usurpation: reports the Prophet actually spoke subordinated to a craftsman's analogies, preference elevated over transmission. From the lay seat, neither debate registers — what arrives is a binding ruling whose pedigree is a chain of inferences no layperson can audit. The engine computes these divergent per-seat classifications from the structural data (role, power, exit, scope); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The jurist class sits nearest the beneficiary end: it wrote the method, administers it, and collects its outputs (offices, fees, authority), with identity_locked exit amplifying its investment in the arrangement. The administrative establishment collects second-order gains (a pliable legal apparatus) with mobile exit — it can redirect patronage between schools, damping its directionality below the jurists'. Textualist specialists sit near the target end: the method's priorities directly demote their asset (authenticated transmission), and their exit is costly migration to anti-analogy currents. Lay subjects sit at the full-target end: powerless, trapped, bearing every ruling the method emits with zero participation in its governance. Rival-school jurists are excluded rather than coordinated — their exclusion from Hanafi-staffed posts is part of what the enforcement machinery maintains. No directionality overrides are used: the beneficiary/victim declarations plus exit options already yield the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — extending a finite revealed corpus to an unbounded stream of novel cases after the Prophet's death — remains live: every generation produces questions the texts never addressed, and contemporary fiqh academies still convene over financial instruments, transplantation, and digital assets. The constraint therefore does not present as mandate-outlived-function, and the mismatch consumer finds status=live paired with verdict=world_rearranges, yielding no zombie flag. The genuine mandatrophy risk is partial and localized: the taqlid centuries show the theater_ratio climbing as derivation receded into precedent-recitation, and had that trajectory completed, the arrangement would have drifted toward inertial performance maintained by administrators who could change it but bore little cost from leaving it. The omega taqlid_theater_trajectory keeps that question open rather than resolving it by fiat. Classification discipline here prevents two errors: reading the jurist class's capture story as pure coordination (which would erase the textualist and lay costs), and reading the Shafi'i polemic as proof of pure extraction (which would erase the real coverage function every school, including the critics', concedes it needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the hanafi_reading of the usul_al_fiqh_method kernel — do its epsilon, beneficiary structure, and classification hold for the kernel as a whole?',
    'They do not and must not be averaged: each sibling reading (maliki_reading, shafii_reading, hanbali_reading) is a separate constraint file with its own epsilon, its own beneficiary/victim seats, and its own classification. Cross-reading comparison happens only through the network edges, never inside this story.',
    'If readings were merged into one story, epsilon would become observable-dependent (violating epsilon-invariance) and the hanafi-specific jurist-capture structure would be diluted by the hanbali textualist structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading, one constraint, one epsilon.').

omega_variable(
    istihsan_discipline_vs_arbitrariness,
    'Is istihsan a disciplined procedure (departure from strict analogy grounded in documented master-level precedent and implicit school consensus, as Hanafi authorities claim) or discretionary jurist preference dressed as method (as al-Shafi''i charged when he called it a man''s arbitrary opinion made law)?',
    'Compare istihsan-based departures across the school''s corpus against recorded positions of Abu Hanifa, Abu Yusuf, and al-Shaybani: departures that track identifiable master precedents indicate discipline; departures with no traceable anchor indicate discretion. Modern fiqh academy proceedings supply additional testable cases.',
    'If disciplined, part of the measured extraction is the price of a working derivation method and the coordination component strengthens; if arbitrary, the method is closer to pure jurist-capture and effective extraction rises sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_discipline_vs_arbitrariness, conceptual, 'The central intra-traditional contest over whether the method''s signature tool is rule-governed or rent-seeking.').

omega_variable(
    text_inference_priority_axis,
    'Where exactly do the four readings disagree structurally — and would flipping the priority ordering between transmitted text and juristic inference change who occupies the beneficiary and payer seats?',
    'Locate the disagreement on the text-versus-inference priority axis: the hanafi reading places trained inference as full partner (and occasional override) to text; the hanbali and shafii readings place authenticated text first and minimize inference; the maliki reading adds communal practice as a third input. Re-deriving each reading''s seats from its position on this axis tests whether the seat structure follows the axis.',
    'Under a flipped ordering, the beneficiary seat migrates from the rationalist jurist class to the hadith-custodian class, and the payer seat migrates correspondingly — the same kernel yields opposite capture structures depending on the axis position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_inference_priority_axis, conceptual, 'The specific structural element on which sibling readings diverge.').

omega_variable(
    authority_framing_lineage_vs_expertise,
    'Is the Hanafi method''s authority structure better framed as lineage (continuity of transmission from Abu Hanifa through the sahibayn, with taqlid anchoring legitimacy) or as credentialed expertise (a professional body whose authority rests on demonstrated mastery of ''illa identification and analogy)?',
    'Examine what the school itself cites when defending a contested ruling: appeals to founder-attribution and transmitter chains indicate lineage; appeals to methodological competence and peer scrutiny indicate expertise. Both framings are internally coherent; the school uses both opportunistically.',
    'The lineage framing (authored here) routes drift through transmission-integrity channels; an expertise framing would change the drift computation toward competence-challenge channels and could alter foreclosure results computed from axiom grounding combined with drift state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_lineage_vs_expertise, conceptual, 'CS-framing under-determination: two defensible authority framings with different downstream classifications.').

omega_variable(
    taqlid_theater_trajectory,
    'Does the rising theater_ratio reflect genuine functional atrophy (the method decaying into recitation of inherited precedent under taqlid) or an artifact of institutional maturity (ritual forms accreting around a still-working core)?',
    'Track whether post-taqlid institutions still generate novel rulings for unprecedented cases (modern finance, organ transplantation, digital assets) versus merely restating compiled school positions; count the share of contemporary fatwas that cite fresh analogical derivation rather than precedent alone.',
    'If atrophy is real and accelerating, the constraint drifts toward inertial persistence with performative maintenance; if the core still functions, the theater is overhead on a live method and the classification stays hybrid coordination-plus-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_theater_trajectory, empirical, 'Whether observed ritualization signals lifecycle decay or ordinary institutional overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_tr_t5, usul_al_fiqh_method__hanafi_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanafi_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(usul_tr_t15, usul_al_fiqh_method__hanafi_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__hanafi_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanafi_reading, theater_ratio, 30, 0.36).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_be_t5, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(usul_be_t15, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_su_t5, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(usul_su_t15, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'usul al-fiqh' (Islamic legal methodology) decomposes into four structurally distinct derivation regimes — one per Sunni school — per the epsilon-invariance principle. Each reading assigns a different epsilon over the same coverage problem: the hanafi reading (this file) maximizes jurist-inference scope and concentrates gains in the rationalist jurist class; the hanbali reading minimizes inference and shifts the benefit seat to hadith custodians; the shafii reading conditions all derivation on authenticated reports and systematizes the meta-discipline itself; the maliki reading admits communal practice and unrestricted public interest as inputs. The upstream/downstream structure runs through the shafii systematization, which was formulated partly against existing Hanafi practice and in turn pressured the Hanafis into writing their oral method down. This file links all three siblings; each sibling file links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
