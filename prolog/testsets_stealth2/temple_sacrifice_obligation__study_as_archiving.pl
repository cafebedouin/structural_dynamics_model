% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Study-as-Archiving Reading of the Post-Temple Sacrificial Obligation
 *   domain: religious/halakhic/commitment-systems
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The kernel —
 *   the status of the sacrificial commandments after the Temple's destruction
 *   — is read three ways within the tradition, and each reading is a separate
 *   constraint with its own ε, its own victim set, and its own
 *   classification. This file authors study_as_archiving: the command remains
 *   binding and unfulfilled; study preserves the execution-knowledge for a
 *   future restoration without discharging anything. The sibling files author
 *   study_as_occupation (study legitimately occupies the obligation in the
 *   Temple's absence) and messianic_suspension (the obligation is suspended,
 *   neither fulfilled nor violated). The ε referent is the standing
 *   arrangement this reading is about — a community bound by an unperformable
 *   command, maintaining its binding status and archiving its
 *   execution-knowledge — assessed by this reading's own lights; the endorsed
 *   restoration is not the referent. The claimed type (tangled_rope) and the
 *   metrics are authored independently: the claim states what this reading
 *   structurally is from its own seat; the metrics describe how the
 *   arrangement actually operates. Assumptions: the interval is CE years; the
 *   divine command is treated as a non-agent structural victim per the
 *   expected delta (the party that goes unmet), with experiential costs
 *   carried by the community; the enforcement history is drawn from
 *   documented challenge-response episodes (Karaite rejection, Sabbatean
 *   crisis, Reform liturgical excision, Zionist sovereignty). The structural
 *   signature the delta predicts — moderate extraction, the unfulfilled
 *   command as victim, an authority whose standing depends on keeping the law
 *   binding and unperformed — is what the declarations and metrics record.
 *
 * KEY AGENTS:
 *   - halakhic_authority_structure: agenda-setter and principal collecting seat (institutional / identity_locked) — maintains the command's binding status and collects standing and continuity from its unmet state
 *   - unfulfilled_divine_command: primary target — the command corpus itself goes unexecuted for the entire post-Temple period (non-agent structural victim; powerless / trapped)
 *   - post_temple_generations: bearers of the standing debt (organized / identity_locked) — carry the unmet command and supply the archive's study labor while receiving the preserved identity
 *   - priestly_line_kohanim: prospective performers (moderate / constrained) — designated executors of the archived knowledge, carrying anticipatory costs
 *   - rabbinic_academy_network: secondary collecting seat (institutional / identity_locked) — curriculum and purpose sustained by the archiving mandate
 *   - non_orthodox_denominations: excluded voice (organized / mobile) — repudiated the binding status and sits outside the adjudication
 *   - comparative_religion_scholars: analytical observer (analytical / analytical) — sees the full structure from outside the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.5).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Study-as-Archiving Reading of the Post-Temple Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic/commitment-systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '170add2d-e1ae-4d61-b78a-c1445f490fd0').
narrative_ontology:cs_kernel_codification('170add2d-e1ae-4d61-b78a-c1445f490fd0', fixed_text).
narrative_ontology:cs_authority_grounding('170add2d-e1ae-4d61-b78a-c1445f490fd0', lineage).
narrative_ontology:cs_interpretation_layer_present('170add2d-e1ae-4d61-b78a-c1445f490fd0').
narrative_ontology:cs_reading_relation('170add2d-e1ae-4d61-b78a-c1445f490fd0', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('170add2d-e1ae-4d61-b78a-c1445f490fd0', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('170add2d-e1ae-4d61-b78a-c1445f490fd0', foundational, sacrificial_command_remains_binding_unperformed).
narrative_ontology:cs_axiom_status(sacrificial_command_remains_binding_unperformed, holdable).
narrative_ontology:cs_axiom_grounding('170add2d-e1ae-4d61-b78a-c1445f490fd0', sacrificial_command_remains_binding_unperformed, theological).
narrative_ontology:cs_axiom('170add2d-e1ae-4d61-b78a-c1445f490fd0', foundational, study_preserves_knowledge_without_discharging_obligation).
narrative_ontology:cs_axiom_status(study_preserves_knowledge_without_discharging_obligation, holdable).
narrative_ontology:cs_axiom_grounding('170add2d-e1ae-4d61-b78a-c1445f490fd0', study_preserves_knowledge_without_discharging_obligation, theological).
narrative_ontology:cs_reference_frame('170add2d-e1ae-4d61-b78a-c1445f490fd0', binding_unperformed_command_awaiting_restoration).
narrative_ontology:cs_drift_state('170add2d-e1ae-4d61-b78a-c1445f490fd0', contemporary_post_deferral_institutionalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('170add2d-e1ae-4d61-b78a-c1445f490fd0', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_academy_network).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, priestly_line_kohanim).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, post_temple_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, post_temple_generations).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, priestly_line_kohanim).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, unlapsed_commandment_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, restoration_executability_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, study_nonsubstitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and maintains the ruling that the sacrificial commandments remain in force after the destruction of the Temple, and that study of their laws preserves the knowledge needed for a future restoration without counting as performance. Issues rulings, shapes the liturgy that petitions daily for restoration, and trains the decisor class that transmits the arrangement. Its standing as custodian of the command's unmet state is inseparable from its authority; it cannot set the command aside without dissolving the role it occupies.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_structure, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_structure, beneficiary).

% The corpus of sacrificial commandments as the framework understands it: binding, addressed to the community, and unexecuted for the entire post-Temple period. It cannot act, speak, or enforce itself; every effect it has is mediated by the authority structure that maintains its binding status and by the liturgy that acknowledges its non-performance. Within this reading, no act available to the community discharges it; only the restoration of the Temple service would.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% Live under the standing unmet command: the daily liturgy names the sacrifices that cannot be brought and asks for their restoration, and the community's self-understanding includes being addressed by a command it cannot carry out. They supply the study labor that maintains the archive — curriculum, recitation, commentary — and receive from it a continuous covenantal identity and the preserved law. Leaving the framework means leaving the community's identity altogether; remaining means carrying the unmet command forward.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, post_temple_generations, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, post_temple_generations, beneficiary).

% Families descended from the priestly line maintain genealogical records and marriage restrictions so that qualified priests will exist if the service is restored. They are the designated performers of the archived knowledge and the reason the archive is kept executable. The anticipatory discipline — endogamy, lineage documentation, purity awareness — is a cost they carry now for a role they cannot currently occupy.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, priestly_line_kohanim, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, priestly_line_kohanim, payer).

% Academies and study institutions whose curricula include the order of sacrifices and the Temple service. The archiving mandate gives these institutions a standing subject, a reason for the material's place in the curriculum, and a continuity of purpose across generations of students. Their self-concept includes being the place where this knowledge is kept alive.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_academy_network, beneficiary,
    institutional, generational, identity_locked, global).

% Movements that removed the restoration-of-sacrifice petitions from their liturgies and repudiated the binding status of the sacrificial commandments. They would contest the arrangement's premise at its root — that the command still binds — but they sit outside the halakhic conversation in which the arrangement is maintained, and their objection does not enter its adjudication.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, non_orthodox_denominations, excluded,
    organized, generational, mobile, continental).

% Study the arrangement from outside the framework: how communities maintain binding law whose performance is impossible, how procedural knowledge is archived across centuries of non-practice, and how custodial authority and restoration-expectation interact. They take no position inside the adjudication.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real preservation problem: a detailed procedural corpus (animal selection, slaughter, dismemberment, altar procedure, priestly duties) must remain executable across centuries in which it cannot be practiced. Distributed study, fixed textual transmission, and liturgical rehearsal keep the corpus intact and transmissible without a practicing institution, so that a restored Temple would find the knowledge ready.
% TRANSFER_FUNCTION: Moves study labor, liturgical attention, and institutional continuity from each generation of the community into the knowledge archive and into the authority structure that curates it; holds the sacrificial obligation open and undischarged, transmitting it intact to the next generation rather than crediting any substitute for it.
% ABSENT_VOICES: The command itself cannot speak: its interests are represented only by the authority structure that claims custodianship of it — the arrangement's principal collector is also its only authorized voice. Outside the framework, the non-Orthodox denominations that excised restoration petitions from their liturgy would contest the binding status at its root; historically the Karaites rejected the oral apparatus through which the binding status is maintained. Neither voice enters the halakhic adjudication.
% DISAPPEARANCE_RATIONALE: If the binding-unfulfilled status and its archiving apparatus vanished overnight — if the command were held suspended or discharged — the daily liturgy would lose its restoration petitions and its recitation of the sacrificial order, the order of Kodashim would lose its place as a living curriculum, the priestly line's genealogical maintenance would lose its point, and the authority structure would lose a central custodial role. The community's self-understanding as addressed by an unmet command would dissolve; the apparatus would not quietly persist.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), a community bound by sacrificial commandments possessed no venue in which to perform them. The founding problem: how to remain in the command's grip — neither suspending nor discharging it — while keeping the service's knowledge executable until performance becomes possible again.
% FOUNDING_PROBLEM_CORROBORATION: The rival readings attest the founding problem's reality: study_as_occupation and messianic_suspension are alternative answers to the same post-destruction crisis — they dispute the answer, not the problem. Academic historians of the post-70 period corroborate the crisis and the community's response from outside the benefiting parties. What no external source attests is the problem's continuing liveness: that the command still binds is the framework's own assertion, maintained by its authority structure; outsiders can attest only that the community maintains it.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end): the arrangement holds the community in permanent non-compliance with a command it cannot perform, and channels study labor and liturgical attention into an apparatus whose custodian collects standing and continuity from the command's unmet state — but the same apparatus solves a genuine preservation problem and collects no material rent. Suppression (0.5) is the raw, unscaled structural work of keeping the binding status intact against rival readings that would discharge it: the occupation reading would credit study as performance and close the debt; the suspension reading would void it outright. Only extractiveness is scaled downstream (by directionality and scope); suppression is authored as the structure's own coercive force. Theater (0.25) is low-moderate: the study is real work with a real function — the corpus is genuinely preserved and executable-in-principle — with a stable performative component in the daily recitation of a service that cannot occur. Accessibility collapse is low (0.3): the rival readings remain live inside the tradition's own texts, and exit, while costly, is real. Resistance (0.55) is the standing internal contest plus the historical discharge attempts the enforcement series records. All three tracked metrics share one time grid (CE years); the 1666 point marks the Sabbatean crisis, when a false restoration nearly discharged the debt and enforcement of binding status peaked. Coordination type is information_standard with a floor override to 0.10, justified by domain knowledge: keeping a non-practiced ritual corpus transmissible for nineteen centuries requires dedicated institutions — academies, liturgical rehearsal, codification — an inherent cost far above a lightweight naming standard's default floor; burden below that level is transmission cost, not extractive overhead. Claim and metrics are independent: the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently. From the authority structure's seat, the arrangement is faithful custodianship: the command's binding status is its dignity, the unfulfilled state is the community's honest condition, and study is the duty of preservation — the debt's persistence is the point, not a cost. From the community's seat, the same structure is a permanent unpayable obligation: every day accrues non-compliance that no available act discharges, acknowledged three times daily in a liturgy that names the missing service. The sharpest structural oddity sits between them: the command — the party that goes unmet — has no voice of its own; its interests are represented exclusively by the authority structure, which is also the seat that collects from maintaining the unmet state. The victim's representation is monopolized by the arrangement's principal collector, and the engine's per-seat computation should surface that asymmetry from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the authority structure (agenda-setter and collector) and the academy network are subsidized by the arrangement — its operation flows standing, curriculum, and continuity to them. The priestly line is a declared beneficiary with a genuine secondary burden (anticipatory discipline), so its derived directionality sits low but not at the beneficiary pole. Victim declarations drive high directionality: the unfulfilled command (powerless, trapped, universal) sits at the full-target end — the arrangement operates entirely at its expense, withholding its execution indefinitely; the post-Temple generations (organized, identity_locked) sit near the target end as the experiential bearers of the standing debt. No directionality overrides are authored: the derivation from declarations, power, and exit produces the right relationships for every seat, and the one dual-positioned agent (the community) is genuinely near the target end within this reading's own lights — the reading itself insists their study does not pay the debt down.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — remaining bound to an unperformable command while keeping its knowledge executable — is live within this reading's own lights: the obligation is unfulfilled today by the framework's own accounting, so founding_problem_status = live against disappearance_verdict = world_rearranges, and the mismatch check finds no zombie. The mandatrophy structure has an unusual twist: the arrangement cannot be resolved by its own success. A realized restoration would consume the archive and dissolve the custodial role the authority structure occupies — the arrangement's terminal event is also its collector's obsolescence. Persistence therefore carries a built-in anti-mandatrophy incentive: only the framework's eschatological event can retire it, and the seat that administers it has no interest in hastening that event. The drift to watch is toward the occupation reading: if study becomes self-justifying rather than archival, the preservation function atrophies into performance — the theater_ratio series and the drift_state record track exactly this. The classification keeps the coordination function (real preservation) and the burden (real standing non-compliance) on the books together, which is what prevents mislabeling this either as pure extraction (the archive is real) or as pure coordination (the debt is real and never credited).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_status_assignment_underdetermination,
    'Is the obligation''s current status — outstanding and violated, legitimately occupied, or suspended — a fact the framework''s texts determine, or a constitutive choice of whichever reading controls the apparatus?',
    'Observe which reading operative practice presupposes: the daily liturgy''s petitionary structure presupposes outstanding status; the study-credit homily (study as if offering) supports occupation; the impossibility-exemption principles support suspension. Trace which presupposition controls rulings when the readings conflict.',
    'If status is text-determined, this reading''s ε and victim set are stable; if constitutive, the arrangement''s classification follows the controlling seat''s choice, and the sibling constraints would show materially different ε (suspension: no non-compliance accrues; occupation: no standing debt remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_status_assignment_underdetermination, conceptual, 'Whether the obligation''s status is text-determined or reading-constituted.').

omega_variable(
    custodial_deferral_interest,
    'Does the authority structure''s persistence-interest depend on the restoration not arriving — and if so, can its restoration-orientation be distinguished from custodial self-preservation?',
    'Observe the authority''s behavior at restoration-adjacent junctures (Temple Mount access changes, red-heifer candidates, political sovereignty moments): does it act to enable preparatory performance or to defer it? Compare with its rulings when restoration-anticipation threatened disorder (post-1666).',
    'If deferral-interest dominates, the arrangement''s coordination story covers custodial self-preservation and the classification shifts toward pure extraction; if enabling behavior dominates, the coordination-plus-burden structure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodial_deferral_interest, empirical, 'Whether the custodian''s interest runs with or against restoration.').

omega_variable(
    nonagent_victim_reach,
    'The declared victim — the unfulfilled divine command — is a non-agent: does the burden analysis hold when the party that goes unmet cannot bear costs experientially, with experiential costs carried by the community and structural cost by the command?',
    'Re-run the classification with the victim set restricted to experiential bearers (post_temple_generations) and separately to the structural bearer (the command); compare the resulting profiles.',
    'With only the community as victim, the arrangement reads as a conventional community-versus-authority structure; with only the command, it reads as a compliance-failure archive; the dual victim set is what produces the moderate mixed profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonagent_victim_reach, conceptual, 'Whether a non-agent victim carries the burden analysis.').

omega_variable(
    binding_status_internalization,
    'Is the binding status maintained by active enforcement or by internalized covenantal identity — and how much suppression survives if the enforcement infrastructure withdraws?',
    'Compare communities where enforcement infrastructure weakened (secularized or geographically detached communities) with those where it is strong: does binding-status practice and restoration-petition persist without enforcement?',
    'If internalized, the arrangement''s persistence is more self-sustaining than the suppression series suggests and the authority''s enforcement role is smaller than modeled; if structural, the enforcement record is the load-bearing fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_status_internalization, empirical, 'Structural vs internalized maintenance of binding status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 200, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsa_archiving_tr_t200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 200, 0.15).
narrative_ontology:measurement(tsa_archiving_tr_t600, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 600, 0.18).
narrative_ontology:measurement(tsa_archiving_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(tsa_archiving_tr_t1666, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1666, 0.35).
narrative_ontology:measurement(tsa_archiving_tr_t1800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(tsa_archiving_tr_t1948, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(tsa_archiving_tr_t2025, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(tsa_archiving_be_t200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(tsa_archiving_be_t600, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 600, 0.52).
narrative_ontology:measurement(tsa_archiving_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(tsa_archiving_be_t1666, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1666, 0.62).
narrative_ontology:measurement(tsa_archiving_be_t1800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(tsa_archiving_be_t1948, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(tsa_archiving_be_t2025, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tsa_archiving_su_t200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(tsa_archiving_su_t600, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 600, 0.4).
narrative_ontology:measurement(tsa_archiving_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(tsa_archiving_su_t1666, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1666, 0.68).
narrative_ontology:measurement(tsa_archiving_su_t1800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1800, 0.62).
narrative_ontology:measurement(tsa_archiving_su_t1948, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(tsa_archiving_su_t2025, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, information_standard).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The colloquial question 'what happened to the sacrifice obligation after 70 CE' decomposes into three structurally distinct constraints with different ε, victim sets, and enforcement profiles: this file (study_as_archiving: binding and unfulfilled; moderate extraction; the unmet command as victim), study_as_occupation (study occupies the obligation; the standing debt dissolves into credited performance), and messianic_suspension (the obligation is suspended; no non-compliance accrues). They are linked here as one constraint family; each is a separate story with a single stable ε, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
