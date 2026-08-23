% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Naskh (Divine Pedagogy Hermeneutic)
 *   domain: religious/hermeneutic/legal-theory
 *
 * SUMMARY:
 *   Within Islamic legal theory (usul al-fiqh), the progressive_restriction
 *   reading organizes revelation as intentional pedagogy: early permissive
 *   rulings are transitional accommodations, later restrictive rulings carry
 *   final divine intent, and no verse is declared textually void — the
 *   trajectory itself does the work classical abrogation does by
 *   cancellation. This story authors ONE reading of the naskh_principle
 *   kernel as a clean, epsilon-invariant constraint. The referent of epsilon
 *   is the standing arrangement under contest: the pedagogy-certification
 *   regime as actually operated (juristic control of revelation chronology,
 *   certification of transitional-versus-final status, redirection of
 *   practitioner citation), assessed by this reading's own lights — never the
 *   harmonization arrangement reformers would install. The claim and the
 *   metrics are independent authored facts: tangled_rope is asserted from the
 *   structure (genuine hermeneutic coordination plus asymmetric extraction
 *   plus active enforcement); the metric values describe observed operation
 *   without being tuned to any predicted engine verdict. Family note: the
 *   colloquial 'naskh debate' conflates three structurally distinct claims
 *   about the same verse-pairs; this file is one member of a three-story
 *   family linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - usuli_jurist_class: Agenda setter and principal collector (institutional/identity_locked) — administers the pedagogy frame and collects interpretive authority
 *   - juridical_enforcement_institutions: Secondary beneficiary (institutional/constrained) — applies final rulings pre-legitimated by the frame
 *   - lay_practitioners_citing_early_permissives: Primary target (powerless/identity_locked) — warrants for early-permissive practice neutralized without compensation
 *   - modernist_contextual_reformers: Excluded contestant (moderate/mobile) — holds the rival harmonization account, no administrative seat
 *   - traditional_abrogationist_scholars: Excluded rival-reading holder (institutional/identity_locked) — contests the frame from the classical-abrogation school
 *   - comparative_fiqqh_analysts: Analytical observer — maps the whole distribution of authority and permission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Naskh (Divine Pedagogy Hermeneutic)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/hermeneutic/legal-theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'fe6085aa-5109-443e-a299-fadebdcd96b4').
narrative_ontology:cs_kernel_codification('fe6085aa-5109-443e-a299-fadebdcd96b4', fixed_text).
narrative_ontology:cs_authority_grounding('fe6085aa-5109-443e-a299-fadebdcd96b4', lineage).
narrative_ontology:cs_interpretation_layer_present('fe6085aa-5109-443e-a299-fadebdcd96b4').
narrative_ontology:cs_reading_relation('fe6085aa-5109-443e-a299-fadebdcd96b4', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('fe6085aa-5109-443e-a299-fadebdcd96b4', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('fe6085aa-5109-443e-a299-fadebdcd96b4', foundational, restriction_progression_is_divine_pedagogy).
narrative_ontology:cs_axiom_status(restriction_progression_is_divine_pedagogy, holdable).
narrative_ontology:cs_axiom_grounding('fe6085aa-5109-443e-a299-fadebdcd96b4', restriction_progression_is_divine_pedagogy, theological).
narrative_ontology:cs_axiom('fe6085aa-5109-443e-a299-fadebdcd96b4', foundational, no_textual_invalidation_in_revelation).
narrative_ontology:cs_axiom_status(no_textual_invalidation_in_revelation, holdable).
narrative_ontology:cs_axiom_grounding('fe6085aa-5109-443e-a299-fadebdcd96b4', no_textual_invalidation_in_revelation, theological).
narrative_ontology:cs_axiom('fe6085aa-5109-443e-a299-fadebdcd96b4', secondary, restriction_culmination_defines_final_intent).
narrative_ontology:cs_axiom_status(restriction_culmination_defines_final_intent, holdable).
narrative_ontology:cs_axiom_grounding('fe6085aa-5109-443e-a299-fadebdcd96b4', restriction_culmination_defines_final_intent, instrumental).
narrative_ontology:cs_reference_frame('fe6085aa-5109-443e-a299-fadebdcd96b4', pedagogical_permissive_to_restrictive_arc).
narrative_ontology:cs_drift_state('fe6085aa-5109-443e-a299-fadebdcd96b4', mass_literacy_translation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe6085aa-5109-443e-a299-fadebdcd96b4', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, usuli_jurist_class).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, juridical_enforcement_institutions).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, lay_practitioners_citing_early_permissives).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_gradualism).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, chronology_determines_operative_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and certify the chronological reading of revelation: which early rulings counted as temporary accommodations and which later rulings bind permanently. Staff the madrasa curricula, write the manuals of legal theory, and adjudicate whether a cited verse still carries operative force. Their standing depends on command of revelation-order knowledge lay readers generally lack; stepping outside the pedagogy frame would reduce them to readers of the same text anyone can now open.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, usuli_jurist_class, agenda_setter,
    institutional, generational, identity_locked, global).

% Operate courts, mufti councils, and doctrinal boards applying rulings certified as final. They receive the practical payoff: restrictive norms arrive pre-legitimated as the culmination of revealed instruction, so enforcement reads as completing divine intent rather than imposing human preference. They spend real resources defending the frame against direct-citation arguments and rival hermeneutics.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, juridical_enforcement_institutions, beneficiary,
    institutional, generational, constrained, national).

% Ordinary believers who find a permissive ruling in an early verse — on drink, marriage, or conduct of war — and take it as sufficient warrant for practice. Under the chronological-pedagogy frame their citation is re-described as invoking a temporary accommodation, and the practice they meant to ground is redirected to the later restrictive ruling. Their relationship to scripture is constitutive of identity, so dropping the citation is not a simple choice, and pressing it invites charges of doubting divine wisdom.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, lay_practitioners_citing_early_permissives, payer,
    powerless, biographical, identity_locked, global).

% Reform-minded scholars and preachers who hold that every verse keeps its force within its revelatory situation and resolve tensions by specifying contexts rather than sequencing them. They publish competing commentaries and argue on the same platforms the jurist class occupies, but hold no seat in the curricular and certifying bodies that administer the pedagogy frame.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, modernist_contextual_reformers, excluded,
    moderate, generational, mobile, global).

% Scholars of the rival classical school who hold that qualifying later verses legally cancel earlier ones outright. From their seat the pedagogy reading is a softening error that leaves cancelled rulings ambiguously alive; they contest it in treatises and seminary disputes but do not participate in the pedagogy frame's administration.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditional_abrogationist_scholars, excluded,
    institutional, generational, identity_locked, global).

% Academic students of Islamic legal theory, inside and outside the tradition, who map how the rival readings distribute interpretive authority and practical permission differently. They watch the whole structure without administering or funding any part of it.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, comparative_fiqqh_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, usuli_jurist_class).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent conflicts between rulings revealed at different times into a single stable legal corpus, preserving the standing of every verse as revealed speech while sequencing which rulings bind practice.
% TRANSFER_FUNCTION: Moves interpretive authority and behavioral permission from lay practitioners who cite early permissive verses to the jurist class that certifies which rulings were temporary accommodations; the legitimacy of contested practices transfers from the text-citer to the chronology-certifier.
% ABSENT_VOICES: Modernist contextual reformers and rival abrogationist scholars sit outside the frame's certifying bodies; ordinary practitioners whose warrants get neutralized rarely appear before curriculum committees; historically, women subject to restrictive marital and inheritance rulings certified as final intent had no seat in the usul debates that sealed the sequence.
% DISAPPEARANCE_RATIONALE: Fatwa bodies, curricula, and certification chains built on the pedagogy frame would lose their organizing account; practitioners citing early permissive verses would regain contested-but-operative textual standing; restrictive rulings would need fresh legitimation from one of the rival readings; the interpretive economy would reorganize around whichever reading absorbed the load.
% FOUNDING_PROBLEM: Apparent conflicts between earlier permissive revelations and later restrictive rulings threatened both the coherence of divine speech and the stability of communal practice; the arrangement was built to sequence revelation into intentional pedagogy so law could settle on the final rulings without declaring scripture self-cancelling.
% FOUNDING_PROBLEM_CORROBORATION: Rival classical abrogationists attest the underlying textual tension (they resolve it by invalidation); modernist contextual reformers acknowledge the same apparent conflicts their reading exists to dissolve; academic historians of usul al-fiqh outside both camps document the formative debates. None of these corroborators sits inside the pedagogy frame's beneficiary set.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial but bounded (0.62): the frame neutralizes practitioners' textual warrants and concentrates certification in the jurist class without confiscating the text or imposing physical penalty — the loss is interpretive standing and behavioral latitude, not life or property. Suppression (0.58, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine) reflects institutional sanction: curricular control, certification gatekeeping, and rhetorical condemnation of direct citation, short of coercive violence. Theater (0.36) captures the widening gap between the rhetoric of honoring every verse and a shrinking operative set — gradualism apologetics performs reverence for early permissives precisely while rendering them inert. Accessibility collapse (0.58): inside the frame, deriving practice from an early permissive verse collapses almost completely once the re-description lands, yet exit to rival readings keeps the value well below natural-law range. Resistance (0.55): quiet citation persistence, reformist publishing, and rival-school contest are real and continuous. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the trajectories are monotonic, not cyclical — institutional consolidation ratchets rather than oscillates, and the rising suppression_requirement series traces enforcement capacity maturing (seminary systems, fatwa bureaucracies, translation policing) as mass literacy and translation began threatening juristic mediation, not an intermittent-reinforcement cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the usuli_jurist_class position the arrangement is faithful transmission of divine pedagogy — low felt burden, high vocational meaning. From the lay_practitioner seat the same structure operates as neutralized warrant: a verse they can read with their own eyes is ruled inoperative for them by people they cannot check. From the traditional_abrogationist seat it is a softening error; from the reformer seat a gatekeeping device. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. usuli_jurist_class (agenda_setter, beneficiary, identity_locked exit) derives near the beneficiary end — effective burden damped toward subsidy of its position, and the identity lock amplifies its investment: the fusion is professional-plus-ideological, since the jurist's authority IS the chronology-mediation; if the frame broke, the seat becomes an ordinary text-reader with a large sunk credential. juridical_enforcement_institutions (beneficiary, constrained) derives low d with mild damping. lay_practitioners_citing_early_permissives (payer, powerless, identity_locked) derives near full-target — amplified by identity lock of the relational-constitutive kind: scripture-as-divine-speech makes selective citation identity-charged, so exit is not a menu option. Suppression mechanism splits roughly 70% structural (curricular control, certification gatekeeping, social sanction) and 30% internalized (deference framed as piety); omega internalized_pedagogical_deference tracks the split. No directionality_overrides are authored: the derivation chain reproduces the true relationships from declarations alone, and an override keyed by power atom would smear across three institutional seats that hold genuinely different structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is resolved here and mandatrophy_resolved is left undeclared: founding_problem_status is live — the arrangement was built to sequence genuinely conflicting rulings, and the conflict recurs with every full reading of the corpus, so the function persists. The classification prevents mislabeling in both directions: read without structural data, the hermeneutic presents as pure coordination service (a rope that keeps scripture coherent) or, read cynically, as pure warrant-destruction (a snare against inconvenient verses); the tangled_rope claim with named beneficiaries, named victims, and active-enforcement declaration preserves both halves. The lifecycle risk visible in the data is accretion, not obsolescence: base_extractiveness climbs monotonically as certification consolidates into rent, and theater rises as 'every verse honored' rhetoric expands over a shrinking operative set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_naskh_kernel,
    'This story is one reading (progressive_restriction) of the naskh_principle kernel; how would classification and epsilon shift if the sibling readings were instantiated instead?',
    'Compile naskh_principle__classical_abrogation and naskh_principle__contextual_harmonization as separate stories; compare victim sets, epsilon, and per-seat types across the triplet against the fixed underlying text.',
    'Under classical_abrogation the same verse-pairs yield holders of outright-invalidated rulings as victims (loss of the verse''s legal force, not just its warrant-status); under contextual_harmonization victims approach zero since every ruling stays contextually live — epsilon diverges sharply across the family while the text stays fixed, confirming the label ''naskh debate'' conflates distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_naskh_kernel, conceptual, 'Committer structure: this constraint is one of three rival readings of the abrogation kernel; sibling instantiation changes the victim set and epsilon.').

omega_variable(
    pedagogy_vs_retrofit_rationalization,
    'Is the permissive-to-restrictive arc an intentional divine pedagogy articulated within the tradition''s own sources, or a post-hoc rationalization fitted onto revelation order after restrictive norms had already settled?',
    'Date the articulation: compare the earliest usul discussions of gradation and final-intent claims against the political settlement of the restrictive rulings they legitimate; test whether pedagogy language precedes or follows the enforcement needs it serves.',
    'If retrofit, the coordination story is cover and the arrangement shifts toward pure enforcement of pre-set norms (snare-flavored); if internally articulated, a larger share of the measured burden counts as the price of the hermeneutic service itself (rope-flavored).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_vs_retrofit_rationalization, empirical, 'Whether the pedagogy arc is designed theology or retrospective justification.').

omega_variable(
    chronology_access_gatekeeping,
    'How much of the practitioner''s interpretive dependence rests on genuine epistemic scarcity (revelation chronology is intrinsically hard) versus maintained gatekeeping (chronology kept scarce to preserve the jurist class''s mediating role)?',
    'Audit access: publication and translation of asbab al-nuzul and chronology materials; measure whether curricula and fatwa channels restrict lay access beyond what intrinsic difficulty requires.',
    'If gatekeeping dominates, suppression is monopoly defense and the effective burden on practitioners rises; if scarcity is genuine, part of the burden is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronology_access_gatekeeping, empirical, 'Genuine epistemic scarcity versus manufactured interpretive monopoly.').

omega_variable(
    internalized_pedagogical_deference,
    'Is practitioner compliance with the final-restrictive account driven by structural sanction (social and institutional cost of citation) or internalized conviction (sincere belief that restriction is manifest wisdom)?',
    'Compare citation behavior across environments differing in sanction intensity — anonymous digital spaces versus embedded communities; persistence of deference where sanction is absent indicates internalization.',
    'If internalized, effective suppression exceeds the structural measure and travels with practitioners across institutional boundaries; if structural, removing sanction would rapidly restore direct citation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_pedagogical_deference, empirical, 'Structural versus internalized component of practitioner deference to the restrictive-final account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_prog_rest_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t0, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t200, naskh_principle__progressive_restriction, theater_ratio, 200, 0.21).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t200, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t400, naskh_principle__progressive_restriction, theater_ratio, 400, 0.24).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t400, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t600, naskh_principle__progressive_restriction, theater_ratio, 600, 0.27).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t600, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t800, naskh_principle__progressive_restriction, theater_ratio, 800, 0.3).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t800, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t1000, naskh_principle__progressive_restriction, theater_ratio, 1000, 0.33).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t1000, observed).
narrative_ontology:measurement(naskh_prog_rest_tr_t1200, naskh_principle__progressive_restriction, theater_ratio, 1200, 0.36).
narrative_ontology:measurement_basis(naskh_prog_rest_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(naskh_prog_rest_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t0, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t200, naskh_principle__progressive_restriction, base_extractiveness, 200, 0.46).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t200, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t400, naskh_principle__progressive_restriction, base_extractiveness, 400, 0.5).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t400, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t600, naskh_principle__progressive_restriction, base_extractiveness, 600, 0.53).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t600, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t800, naskh_principle__progressive_restriction, base_extractiveness, 800, 0.56).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t800, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t1000, naskh_principle__progressive_restriction, base_extractiveness, 1000, 0.59).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t1000, observed).
narrative_ontology:measurement(naskh_prog_rest_be_t1200, naskh_principle__progressive_restriction, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(naskh_prog_rest_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(naskh_prog_rest_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t0, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t200, naskh_principle__progressive_restriction, suppression_requirement, 200, 0.37).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t200, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t400, naskh_principle__progressive_restriction, suppression_requirement, 400, 0.41).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t400, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t600, naskh_principle__progressive_restriction, suppression_requirement, 600, 0.44).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t600, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t800, naskh_principle__progressive_restriction, suppression_requirement, 800, 0.48).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t800, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t1000, naskh_principle__progressive_restriction, suppression_requirement, 1000, 0.53).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t1000, observed).
narrative_ontology:measurement(naskh_prog_rest_su_t1200, naskh_principle__progressive_restriction, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement_basis(naskh_prog_rest_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial 'naskh debate' is one label over three structurally distinct claims about the same verse-pairs, decomposed per the epsilon-invariance principle. classical_abrogation (invalidation regime; victims lose the ruling outright) is historically upstream — the dominant classical account whose settled status lent the pedagogy reading its chronological scaffolding. This story (progressive_restriction; victims lose warrant-status while the text stays nominally intact) sits mid-family. contextual_harmonization (all rulings contextually live; victims near zero) is the modernist counter-account downstream. Each member carries its own epsilon, beneficiaries, and victims; all three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
