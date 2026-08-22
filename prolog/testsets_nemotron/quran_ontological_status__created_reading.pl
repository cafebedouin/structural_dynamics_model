% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Rationalist Mu'tazilite Reading)
 *   domain: theological/philosophical/political
 *
 * SUMMARY:
 *   The Mu'tazilite doctrine that the Qur'an is created (makhlūq) divine
 *   speech — articulated against the view that the Qur'an is uncreated and
 *   coeternal with God's essence — functions as a coordination artifact
 *   (rope) rather than an ontic mountain. By locating God's essence above all
 *   temporal artifacts including revelation, this reading preserves divine
 *   transcendence while making textual meaning interpretively flexible. The
 *   constraint solves a genuine coordination problem: how to maintain divine
 *   unity (tawḥīd) and transcendence while accounting for revelation's
 *   temporal occurrence. Rationalist theologians, later reform movements, and
 *   philosophical schools benefit from the hermeneutic authority this opens;
 *   traditionalist jurists whose authority rests on textual fixity and
 *   literalist communities whose identity depends on unmediated divine speech
 *   bear the cost of interpretive destabilization. The constraint's low
 *   extractiveness and suppression in its pure theological form distinguish
 *   it from the state-enforced variant (mihna), which is a separate
 *   constraint story.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: Primary beneficiary (intellectual/organized) — gains hermeneutic authority and rational theology's legitimacy
 *   - traditionalist_jurists: Primary victim (institutional/identity_locked) — loses authority derived from textual fixity and unmediated divine speech
 *   - literalist_communities: Victim (organized/identity_locked) — identity fused with uncreated speech doctrine; exit is existentially costly
 *   - philosophical_schools: Beneficiary (organized/mobile) — gains space for rational inquiry into revelation's nature
 *   - reform_movements: Beneficiary (moderate/constrained) — gains doctrinal resources for reinterpretation
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.25).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.15).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Rationalist Mu'tazilite Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/philosophical/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '8c92121d-fd00-4574-bf0d-2ec2a5273eca').
narrative_ontology:cs_kernel_codification('8c92121d-fd00-4574-bf0d-2ec2a5273eca', formalized).
narrative_ontology:cs_authority_grounding('8c92121d-fd00-4574-bf0d-2ec2a5273eca', lineage).
narrative_ontology:cs_interpretation_layer_present('8c92121d-fd00-4574-bf0d-2ec2a5273eca').
narrative_ontology:cs_reading_relation('8c92121d-fd00-4574-bf0d-2ec2a5273eca', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c92121d-fd00-4574-bf0d-2ec2a5273eca', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('8c92121d-fd00-4574-bf0d-2ec2a5273eca', foundational, divine_transcendence_requires_created_speech).
narrative_ontology:cs_axiom_status(divine_transcendence_requires_created_speech, holdable).
narrative_ontology:cs_axiom_grounding('8c92121d-fd00-4574-bf0d-2ec2a5273eca', divine_transcendence_requires_created_speech, deontological).
narrative_ontology:cs_axiom('8c92121d-fd00-4574-bf0d-2ec2a5273eca', secondary, reason_authorized_as_theological_arbiter).
narrative_ontology:cs_axiom_status(reason_authorized_as_theological_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8c92121d-fd00-4574-bf0d-2ec2a5273eca', reason_authorized_as_theological_arbiter, instrumental).
narrative_ontology:cs_reference_frame('8c92121d-fd00-4574-bf0d-2ec2a5273eca', classical_mutazilite_tauhid_framework).
narrative_ontology:cs_drift_state('8c92121d-fd00-4574-bf0d-2ec2a5273eca', post_mihna_ashari_synthesis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c92121d-fd00-4574-bf0d-2ec2a5273eca', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mu'tazilite and allied rationalist theologians (8th–10th century) who articulate and defend the createdness doctrine. They gain hermeneutic authority: reason becomes the arbiter of theological truth, revelation is subject to rational criteria. Their institutional position (kalām circles, court patronage under early Abbasids) lets them shape discourse. Exit is mobile — they can shift intellectual frameworks without existential loss.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, continental).

% Traditionalist scholars (Ahl al-Ḥadīth, early Ash'arites) whose juristic authority derives from the Qur'an's uncreated fixity. The created reading makes textual meaning contingent on interpretation, undermining the fixed textual anchor their authority rests on. Exit is identity_locked: their professional identity, institutional role, and self-concept are fused with textual fixity; abandoning it dissolves their authority structure.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    institutional, generational, identity_locked, continental).

% Communities whose religious identity is constituted by direct, unmediated access to divine speech. The created reading interposes interpretation between believer and revelation, threatening the immediacy that grounds their practice and self-understanding. Exit is identity_locked: the uncreated Qur'an is not a belief they hold but a world they inhabit; leaving it is existential dislocation, not opinion change.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    organized, generational, identity_locked, continental).

% Falsafa (Islamic philosophy) traditions and later hikma schools that treat revelation as a symbolic/imaginative faculty output rather than literal divine speech. The created reading legitimizes their hermeneutic: philosophy gains equal or superior epistemic standing to revelation. Exit is mobile — intellectual frameworks are their native medium.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, continental).

% Modernist and reformist movements (19th–21st century) that invoke createdness to authorize reinterpretation of seemingly fixed texts. They gain doctrinal resources but operate within traditions where the uncreated reading dominates, making their position contingent and contested. Exit is constrained — they can adopt the reading but cannot easily escape the dominant framework's social weight.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, biographical, constrained, regional).

% The analytical seat that sees the full kernel structure: three readings (created, uncreated, state-enforced) contesting the same ontological commitment. This seat neither collects nor pays; it maps the structural relationships across the constraint family.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the theological problem of divine transcendence (tawḥīd) vs. temporal revelation: how can God be absolutely one and beyond time if His speech occurs in time? The created reading coordinates Muslim theology around a single answer — God creates speech as an act, preserving divine unity and transcendence.
% TRANSFER_FUNCTION: Moves hermeneutic authority from fixed textual literalism (traditionalist jurists, literalist communities) to rational interpretation (rationalist theologians, philosophical schools, reform movements). The transfer is intellectual authority and interpretive control, not material wealth.
% ABSENT_VOICES: Pre-theological believers for whom the Qur'an's ontological status is not a question — the text simply *is* divine speech, unmediated by doctrine. Also marginalized communities (women, non-elite believers) whose relationship to revelation is mediated by both traditionalist and rationalist male elites. They are structurally excluded from the theological debate that shapes how the constraint operates on them.
% DISAPPEARANCE_RATIONALE: If the created reading vanished overnight, the coordination it provides — a theologically articulated solution to transcendence vs. revelation — would disappear. Rationalist theology would lose its foundational doctrinal anchor; reform movements would lose a key hermeneutic resource; traditionalist and literalist positions would face less contestation but also lose the dialectical pressure that shaped their own articulations. The theological landscape would rearrange around the remaining readings (uncreated, state-enforced).
% FOUNDING_PROBLEM: How to preserve absolute divine transcendence and unity (tawḥīd) while accounting for the Qur'an's temporal occurrence as revealed speech in history. The created reading was built to solve this by making revelation an act of God (created) rather than an attribute coeternal with God's essence.
% FOUNDING_PROBLEM_CORROBORATION: Mu'tazilite texts (e.g., al-Naẓẓām, al-Jāḥiẓ, ʿAbd al-Jabbār) explicitly articulate transcendence as the motive. Ash'arite opponents (al-Ashʿarī, al-Bāqillānī) attest the problem is live but argue the created solution undermines divine speech's reality. Modern scholars (Wolfson, van Ess, Rudolph) corroborate from outside the benefiting parties: the transcendence/revelation tension is a genuine structural problem in Islamic theology, not a Mu'tazilite invention.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.25) reflects the reading's primary function as coordination: it solves the theological problem of divine transcendence vs. temporal revelation without extracting significant rents. Suppression (0.15) is low because the reading relies on argumentative persuasion, not coercion — though it rises during the mihna period (t=100), captured in measurements as a separate enforcement spike. Theater ratio (0.1) is low; the coordination function is genuine. Accessibility collapse (0.35) is moderate: alternatives (uncreated reading) remain structurally available and historically persistent. Resistance (0.55) is substantial from traditionalist and literalist seats whose authority and identity are threatened. The claimed type 'rope' reflects the structural truth: genuine coordination with minimal coercive overhead, participants as net beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist theologian seat (beneficiary, organized, mobile exit), the constraint is experienced as liberating coordination — divine transcendence secured, reason authorized. From the traditionalist jurist seat (victim, institutional, identity_locked exit), the same constraint is experienced as extractive destabilization — authority undermined, the fixed text that grounds their role made fluid. From the literalist community seat (victim, organized, identity_locked), it is an existential threat to self-understanding. The engine computes this divergence from the structural data: beneficiaries have low directionality (d ≈ 0.1–0.2), victims have high directionality (d ≈ 0.8–0.9) due to identity_locked exit and institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rationalist_theologians, reform_movements, philosophical_schools) gain hermeneutic authority, intellectual space, and doctrinal resources without running the constraint — they collect the coordination dividend. Victims (traditionalist_jurists, literalist_communities) bear the cost: their authority structures and identity formations depend on the uncreated reading's fixity. The created reading does not actively suppress them (low suppression), but it structurally undermines their position by making the alternative (uncreated) theologically contestable. Directionality is derived from beneficiary/victim declarations + exit options: identity_locked victims (literalist_communities) sit near d=1.0; institutional victims (traditionalist_jurists) sit near d=0.85; organized beneficiaries with mobile exit sit near d=0.15.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — preserving divine transcendence while accounting for temporal revelation — remains live (founding_problem_status: contested). The created reading has not atrophied into piton; its coordination function persists in contemporary reform movements. No mandatrophy resolution declared because the founding problem is still structurally active across the kernel's readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint one reading of a contested kernel (quran_ontological_status) rather than a standalone constraint?',
    'Authoring discipline: this reading instantiates one specific constraint with one stable ε. The kernel and sibling readings are tracked via omega, not folded into this constraint''s classification.',
    'Confirms DP-001 (ε-invariance) compliance: one reading, one constraint, one ε. Sibling readings (uncreated_reading, state_enforced_creation_reading) are separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is a kernel reading instantiation, not a standalone constraint.').

omega_variable(
    divine_transcendence_vs_textual_fixity,
    'Does classifying revelation as a coordination artifact (rope) preserve divine transcendence more faithfully than classifying it as an ontic constraint (mountain)?',
    'Theological debate across Ash''arite, Maturidi, and Mu''tazilite traditions; philosophical analysis of divine simplicity and speech acts.',
    'If transcendence is preserved by the created reading, its rope classification is theologically motivated, not reductive. If transcendence requires uncreated speech, the created reading is structurally extractive against divine attributes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_transcendence_vs_textual_fixity, conceptual, 'Whether the created reading''s rope status serves transcendence or undermines it.').

omega_variable(
    state_power_capture_risk,
    'Does the created reading''s interpretive flexibility create an opening for state enforcement (mihna) that the reading itself does not entail?',
    'Historical analysis of the Abbasid mihna (833–848 CE): did the created doctrine structurally enable inquisition, or was state enforcement an independent political capture?',
    'If the doctrine enabled state enforcement, its effective extraction rises and classification shifts toward tangled_rope/snare. If enforcement was contingent capture, the reading remains a low-extraction rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_power_capture_risk, empirical, 'Whether interpretive flexibility structurally enables coercive state power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.15).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__created_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(qura_tr_t250, quran_ontological_status__created_reading, theater_ratio, 250, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__created_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement(qura_be_t250, quran_ontological_status__created_reading, base_extractiveness, 250, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(qura_su_t150, quran_ontological_status__created_reading, suppression_requirement, 150, 0.2).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__created_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement(qura_su_t250, quran_ontological_status__created_reading, suppression_requirement, 250, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.08).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This reading (created_reading) and uncreated_reading are dual formulations of the same kernel: one preserves transcendence by locating God above text (rope), the other by locating text within God's essence (mountain). The state_enforced_creation_reading adds coercive enforcement to the created doctrine, shifting it toward tangled_rope/snare. All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__created_reading, institutional, 0.85).
constraint_indexing:directionality_override(quran_ontological_status__created_reading, organized, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
