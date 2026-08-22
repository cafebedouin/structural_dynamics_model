% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study Mandate — Archive Reading (Defunct Sacrificial Law as Communal Heritage)
 *   domain: religious/legal-educational
 *
 * SUMMARY:
 *   This story instantiates the archive reading of the Kodashim study
 *   arrangement: the sacrificial order of the Mishnah and Talmud documents a
 *   cult that ceased operating in 70 CE, and the standing arrangement under
 *   assessment is the curricular mandate that keeps every ordination track
 *   routed through it. Read as archive, the mandate's live output is
 *   historical preservation and identity-maintenance; its costs fall on
 *   students and practitioners whose years and attention go to law with no
 *   ritual application, while revenue and legitimacy accrue to the
 *   institutions administering the canon. The epsilon authored here is over
 *   THAT standing arrangement as this reading sees it — moderate, because the
 *   identity function is real — and it is invariant within this file. The
 *   colloquial label 'studying Kodashim' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   constraints (this archive reading; a performance reading claiming cosmic
 *   efficacy; a preparation reading claiming suspended binding obligation),
 *   each authored as its own story with its own epsilon and linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   rabbinic_academies: Agenda-setter (institutional/identity_locked) — sets
 *   curriculum, collects tuition and legitimacy -
 *   observant_jewish_communities: Beneficiary with costs
 *   (organized/constrained) — funds the system, receives identity maintenance
 *   - yeshiva_students: Primary target (powerless/constrained) — formative
 *   years spent on unperformable law - halakhic_practitioners: Secondary
 *   target (moderate/identity_locked) — professional standing fused with
 *   whole-corpus mastery - academic_jewish_studies_scholars: Analytical
 *   observer (moderate/analytical) — performs preservation without the
 *   mandate - temple_restoration_advocates: Excluded voice
 *   (organized/constrained) — rejects the defunctness premise from outside
 *
 * KEY AGENTS:
 *   - rabbinic_academies: Agenda-setter (institutional/identity_locked) — sets curriculum, collects tuition and legitimacy
 *   - observant_jewish_communities: Beneficiary with costs (organized/constrained) — funds the system, receives identity maintenance
 *   - yeshiva_students: Primary target (powerless/constrained) — formative years spent on unperformable law
 *   - halakhic_practitioners: Secondary target (moderate/identity_locked) — professional standing fused with whole-corpus mastery
 *   - academic_jewish_studies_scholars: Analytical observer (moderate/analytical) — performs preservation without the mandate
 *   - temple_restoration_advocates: Excluded voice (organized/constrained) — rejects the defunctness premise from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.48).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.38).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.48).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study Mandate — Archive Reading (Defunct Sacrificial Law as Communal Heritage)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/legal-educational").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '8a68463a-75ac-461e-97ee-a7a35df4f2bc').
narrative_ontology:cs_kernel_codification('8a68463a-75ac-461e-97ee-a7a35df4f2bc', fixed_text).
narrative_ontology:cs_authority_grounding('8a68463a-75ac-461e-97ee-a7a35df4f2bc', lineage).
narrative_ontology:cs_interpretation_layer_present('8a68463a-75ac-461e-97ee-a7a35df4f2bc').
narrative_ontology:cs_reading_relation('8a68463a-75ac-461e-97ee-a7a35df4f2bc', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('8a68463a-75ac-461e-97ee-a7a35df4f2bc', kodashim_obligation__study_as_preparation, forecloses).
narrative_ontology:cs_axiom('8a68463a-75ac-461e-97ee-a7a35df4f2bc', foundational, sacrificial_obligation_lapsed_with_temple).
narrative_ontology:cs_axiom_status(sacrificial_obligation_lapsed_with_temple, holdable).
narrative_ontology:cs_axiom_grounding('8a68463a-75ac-461e-97ee-a7a35df4f2bc', sacrificial_obligation_lapsed_with_temple, empirically_contingent).
narrative_ontology:cs_axiom('8a68463a-75ac-461e-97ee-a7a35df4f2bc', foundational, study_value_exhausted_in_preservation).
narrative_ontology:cs_axiom_status(study_value_exhausted_in_preservation, holdable).
narrative_ontology:cs_axiom_grounding('8a68463a-75ac-461e-97ee-a7a35df4f2bc', study_value_exhausted_in_preservation, instrumental).
narrative_ontology:cs_reference_frame('8a68463a-75ac-461e-97ee-a7a35df4f2bc', defunct_system_archive).
narrative_ontology:cs_drift_state('8a68463a-75ac-461e-97ee-a7a35df4f2bc', contemporary_yeshiva_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a68463a-75ac-461e-97ee-a7a35df4f2bc', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, observant_jewish_communities).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, halakhic_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, observant_jewish_communities).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, mesorah_continuity_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, canon_wholeness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the curriculum, examine candidates, and grant ordination across the full six-order canon, including the sacrificial order whose rites no altar currently receives. Tuition, endowments, and enrollment flow in; institutional authority flows from the claim to transmit the whole received corpus intact. Setting the defunct order aside would undercut the completeness on which that authority rests, so the curriculum is not something these institutions can lightly revise — over generations they have become the thing they administer.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_academies, agenda_setter,
    institutional, generational, identity_locked, global).

% Fund the academies, enroll their children, and organize communal life around the study calendar. They receive a shared memory of the Temple rite, a common textual repertoire, and a continuity narrative that marks belonging. They also carry the bill: tuition, donations, and the years their members devote to material with no current ritual application. Stepping outside the system carries heavy social meaning, so most stay within it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, observant_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, observant_jewish_communities, payer).

% Spend their formative decades mastering tractates that govern offerings no one can bring, alongside tractates that govern daily life. Opting out of the sacrificial order means forgoing ordination and the communal standing attached to it; in insular settings it effectively means leaving the community's respect economy altogether.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_students, payer,
    powerless, biographical, constrained, global).

% Ordained decisors whose day-to-day work concerns slaughterhouse supervision, Sabbath, marriage, and mourning — none of it requiring the sacrificial order. Their professional standing and peer recognition nonetheless presuppose fluency in it, and their self-concept as carriers of the whole tradition is bound up with that mastery; setting it aside would feel like ceasing to be what they are.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, halakhic_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Work in universities, editing the sacrificial tractates critically and writing their history. They demonstrate that the preservation function can be performed without ordination gates or curricular mandates, and they publish findings the academies cite selectively.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_jewish_studies_scholars, observer,
    moderate, biographical, analytical, global).

% Maintain vessel reconstructions, priestly genealogies, and architectural plans for a renewed service. They deny the premise that the system is defunct, hold the laws binding here and now, and would object to any curriculum that frames the material as closed history — but they sit outside the academies' deliberative circles entirely.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, temple_restoration_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, rabbinic_academies).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of a complete canonical curriculum across generations: a shared sequence of texts, ordination standards keyed to whole-corpus mastery, and a common memory of the Temple rite that marks communal boundaries and membership claims.
% TRANSFER_FUNCTION: Moves years of student study-time, tuition, and communal funding toward the academies, and moves legitimacy, continuity-authority, and identity-assurance back toward the institutions and the community; the defunct subject matter is the token in which the transfer is denominated.
% ABSENT_VOICES: Curriculum-reform voices inside the community who would reallocate sacrificial-order hours to applicable civil and festival law are marginalized as shallow; Temple-restoration advocates outside it, who deny the defunctness premise altogether, are not part of these institutions' conversations at all. Neither seat appears in curricular deliberations.
% DISAPPEARANCE_RATIONALE: Ordination standards, academy schedules, donor narratives, and communal self-description all reference the complete six-order corpus; overnight removal would force curricula to reorganize, redefine rabbinic erudition, and rewrite the continuity story — disruptive rearrangement, though not communal collapse.
% FOUNDING_PROBLEM: Training priests, judges, and Temple administrators to operate a running sacrificial system: which offering, in what order, with what disqualifications — knowledge needed daily while the Temple stood.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians and archaeologists of the Second Temple period attest the system ceased in 70 CE and never resumed; the site's present political status independently corroborates non-operation. The academies themselves now justify study on continuity and identity grounds rather than operational ones — a tacit concession, from outside the founding problem's original user base, that the problem is gone.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) rather than high because the identity function is genuine under this reading: communities demonstrably value the shared memory and boundary-marking the curriculum provides, so part of the resource flow purchases something real. Suppression (0.38) is the lowest of the three headline metrics and falling: ordination gates and funding dependence remain, but universities, digital libraries, and secular careers have widened exit over the interval, and the load has shifted to internalized norms. Theater (0.42) reflects completion-ceremony culture and pace-over-depth cycles layered onto serious scholarship. Accessibility_collapse (0.32) is low for an enforced arrangement: the alternative — reallocating hours to applicable law — is visible, periodically proposed, and occasionally enacted by smaller seminaries, yet it is systematically devalued rather than unthinkable. Resistance (0.40) runs through student challenge sessions, reform-community abandonment of the order, and quiet disengagement rather than open revolt. The temporal series share one grid (points 0-75 at 15-year steps): base_extractiveness climbs as the yeshiva sector expands faster than any new ritual application appears, theater_ratio climbs with completion-ceremony culture, and suppression_requirement falls as external alternatives mature — enforcement decay co-occurring with extraction accumulation, i.e., the arrangement needs less force because internalization has deepened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently from the same structure. From the academies' position the mandate is custodianship: they transmit, they do not take. From the students' position it is a toll: years priced against a rite that will never resume. The practitioners' seat is the sharpest divergence — their identity lock means the same curriculum that costs them time also constitutes their standing, so their computed extraction should land between the academy's near-benefit and the student's near-full-target. The engine derives these divergences from the declared roles, exits, and locks; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: rabbinic_academies (d near the beneficiary end — they collect revenue and legitimacy and control the rules) and observant_jewish_communities (near symmetric — genuine identity return against diffuse funding cost). Victims: yeshiva_students (near the target end — constrained exit, biographical stakes) and halakhic_practitioners (high but not maximal — identity lock pushes them toward full target even though status gains partially offset the diversion). Spatial scope is global across seats, which scales effective extraction modestly upward for the target seats; suppression is left unscaled as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — operating a running sacrificial cult — died with the Temple, and this reading declares the mandate outlived: the arrangement persists by conversion, not by function. The classification discipline prevents two opposite mislabels. Reading the arrangement as pure extraction fails because the coordination function is real: remove the mandate and the community's shared textual memory frays, which is why enforcement decay has not produced mass exit. Reading it as pure coordination fails because the gains concentrate — tuition and legitimacy pool at the academies — while the costs are borne diffusely by students and practitioners. The hybrid structure is what both halves of the evidence support. The R5 interview records the consequence plainly: a dead founding problem paired with a world-that-rearranges verdict is the zombie signature, and this reading owns it rather than laundering it. Fixing is classed prohibitive: the academies could administratively revise the curriculum at trivial cost, but the legitimacy price of abandoning the completeness claim exceeds any benefit they would recognize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kodashim_obligation kernel; what structural changes would adoption of the sibling readings (study_as_performance, study_as_preparation) produce?',
    'Track which motivational frame dominates actual study uptake across communities (attitudinal surveys plus curriculum analysis); the sibling stories author their own epsilon over the same practice in their own files.',
    'If the preparation reading dominates, study becomes necessary investment for a resumed service — extraction falls sharply and the victim set thins toward volunteers; if the performance reading dominates, study claims a live cosmic output, invalidating the null-output premise on which this reading''s moderate extraction rests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the kernel governs, and what the siblings would change.').

omega_variable(
    temple_restoration_contingency,
    'Is Temple restoration genuinely structurally impossible and undesired, as this reading holds, or merely contingently blocked?',
    'Political and archaeological developments at the Temple Mount site; halakhic rulings on altar location and purity qualification; longitudinal surveys of communal desire for restoration.',
    'A live restoration prospect collapses the archive reading into the preparation reading: study converts from preservation into necessary training, and epsilon drops well below the authored 0.48.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_contingency, empirical, 'Whether the defunctness premise is permanent or contingent.').

omega_variable(
    identity_function_genuineness,
    'Is the identity-maintenance attributed to Kodashim study a genuine coordination good, or a cover story for institutional rent collection?',
    'Compare identity-cohesion, retention, and boundary-marker salience between communities whose curricula de-emphasize the sacrificial order and those mandating it, controlling for overall study intensity.',
    'If cover, the constraint loses its coordination gate and reclassifies toward pure extraction; if genuine, the hybrid reading stands with extraction measured net of the identity value delivered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_function_genuineness, empirical, 'Gaming-risk check on the identity-coordination typing of this constraint.').

omega_variable(
    opportunity_cost_attribution,
    'Is student time spent on the sacrificial order genuinely diverted from higher-value applicable law, or would the marginal hours otherwise go unstudied?',
    'Curriculum natural experiments: track total halakhic competence and engagement where sacrificial-order hours are reallocated to applicable law versus simply removed.',
    'If substitution is real, the victim seats are strong and epsilon is understated; if the hours would lapse unused, the victim claim weakens and the arrangement moves toward benign coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_attribution, empirical, 'Strength of the diverted-intellectual-resources victim claim.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (ordination gates, funding dependence, communal standing) or internalized (the belief that whole-corpus mastery defines erudition)?',
    'Post-exit trajectory: graduates who leave the academy track — does the felt obligation to master the defunct order persist once no institution enforces it?',
    'If internalized, effective suppression exceeds the structural measure and travels with agents after exit; if structural, widening alternative credentials would rapidly lower it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_archive_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(kodashim_archive_tr_t0, observed).
narrative_ontology:measurement(kodashim_archive_tr_t15, kodashim_obligation__study_as_archive, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(kodashim_archive_tr_t15, observed).
narrative_ontology:measurement(kodashim_archive_tr_t30, kodashim_obligation__study_as_archive, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(kodashim_archive_tr_t30, observed).
narrative_ontology:measurement(kodashim_archive_tr_t45, kodashim_obligation__study_as_archive, theater_ratio, 45, 0.37).
narrative_ontology:measurement_basis(kodashim_archive_tr_t45, observed).
narrative_ontology:measurement(kodashim_archive_tr_t60, kodashim_obligation__study_as_archive, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(kodashim_archive_tr_t60, observed).
narrative_ontology:measurement(kodashim_archive_tr_t75, kodashim_obligation__study_as_archive, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(kodashim_archive_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(kodashim_archive_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(kodashim_archive_be_t0, observed).
narrative_ontology:measurement(kodashim_archive_be_t15, kodashim_obligation__study_as_archive, base_extractiveness, 15, 0.39).
narrative_ontology:measurement_basis(kodashim_archive_be_t15, observed).
narrative_ontology:measurement(kodashim_archive_be_t30, kodashim_obligation__study_as_archive, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(kodashim_archive_be_t30, observed).
narrative_ontology:measurement(kodashim_archive_be_t45, kodashim_obligation__study_as_archive, base_extractiveness, 45, 0.44).
narrative_ontology:measurement_basis(kodashim_archive_be_t45, observed).
narrative_ontology:measurement(kodashim_archive_be_t60, kodashim_obligation__study_as_archive, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(kodashim_archive_be_t60, observed).
narrative_ontology:measurement(kodashim_archive_be_t75, kodashim_obligation__study_as_archive, base_extractiveness, 75, 0.48).
narrative_ontology:measurement_basis(kodashim_archive_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_archive_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(kodashim_archive_su_t0, observed).
narrative_ontology:measurement(kodashim_archive_su_t15, kodashim_obligation__study_as_archive, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(kodashim_archive_su_t15, observed).
narrative_ontology:measurement(kodashim_archive_su_t30, kodashim_obligation__study_as_archive, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(kodashim_archive_su_t30, observed).
narrative_ontology:measurement(kodashim_archive_su_t45, kodashim_obligation__study_as_archive, suppression_requirement, 45, 0.44).
narrative_ontology:measurement_basis(kodashim_archive_su_t45, observed).
narrative_ontology:measurement(kodashim_archive_su_t60, kodashim_obligation__study_as_archive, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(kodashim_archive_su_t60, observed).
narrative_ontology:measurement(kodashim_archive_su_t75, kodashim_obligation__study_as_archive, suppression_requirement, 75, 0.38).
narrative_ontology:measurement_basis(kodashim_archive_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'studying Kodashim.' One label covered three structurally distinct claims: whether the sacrificial obligation persists (archive: no; preparation: yes, suspended; performance: moot — efficacy is independent of performance), and what study outputs (preservation; cosmic enactment; restoration-ready technique). Because epsilon differs across those claims, each is authored as its own story with its own stakeholder surface; this file links both siblings. Historical influence runs upstream from the preparation reading (the classical post-destruction justification) toward the archive reading (the modern historicizing move), while the performance reading draws on shared mystical sources with the other two without depending on them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
