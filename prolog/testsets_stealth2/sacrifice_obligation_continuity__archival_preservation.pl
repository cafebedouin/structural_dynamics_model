% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrificial Law as Archived Heritage: Concluded Obligation, Cultural Study Practice
 *   domain: religious/ritual/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the archival_preservation reading of the
 *   sacrifice_obligation_continuity kernel: the sacrificial legislation is
 *   treated as concluded — its normative force ended with the cult it
 *   governed — and the continuing study of the corpus (Leviticus, the
 *   Mishnah's sacrificial tractates, the Talmudic orders of Qodashim and
 *   Seraim and their commentaries) is cultural practice: transmission of
 *   memory and text, not fulfillment of any command. The standing arrangement
 *   under contest, and therefore the epsilon referent, is that voluntary
 *   study practice, assessed by this reading's own lights: it asserts no
 *   normative claim, compels no one, and sanctions no exit, so base
 *   extractiveness sits near zero (0.05). Constraint family: the colloquial
 *   label 'the sacrifice obligation' covers four structurally distinct
 *   settlements of one kernel, and this file is one of four linked stories.
 *   The sibling readings author higher epsilon over the same corpus because
 *   their arrangements attach normative force where this one detaches it:
 *   study_as_performance obligates the study itself, performance_only
 *   obligates an act that cannot currently be performed (the family's highest
 *   extraction), and messianic_suspension obligates readiness. This story's
 *   epsilon is the family floor; each sibling file carries its own
 *   beneficiary/victim structure and classification. The claim/metric gap is
 *   minimal here by design: the claimed type and the authored metrics
 *   independently describe a low-cost voluntary coordination practice.
 *
 * KEY AGENTS:
 *   - heritage_educational_institutions: Agenda-setter (institutional/constrained) — sets and administers the curricula that keep the corpus in study; collects tuition and endowments; could restructure the practice at institutional cost
 *   - diaspora_jewish_communities: Primary beneficiary (organized/mobile) — receives cultural continuity, liturgical comprehension, and memory; participation voluntary
 *   - rabbinic_textual_scholars: Beneficiary (moderate/mobile) — careers and standing built on explicating the corpus with no normative stakes
 *   - secular_jewish_heritage_learners: Beneficiary (moderate/mobile) — voluntary heritage engagement; exit is non-re-enrollment
 *   - priestly_lineage_families: Excluded (moderate/identity_locked) — ascribed priestly vocation left without function by the settlement; not consulted
 *   - restorationist_temple_movements: Excluded (powerless/identity_locked) — hold the obligation live; outside the institutions that run the practice
 *   - comparative_ritual_studies_scholars: Analytical observer (analytical/analytical) — sees the settlement, the contest, and the family of readings from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.08).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrificial Law as Archived Heritage: Concluded Obligation, Cultural Study Practice").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/ritual/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '26575e05-6a6f-4b4f-ba26-422049a6ce0d').
narrative_ontology:cs_kernel_codification('26575e05-6a6f-4b4f-ba26-422049a6ce0d', fixed_text).
narrative_ontology:cs_authority_grounding('26575e05-6a6f-4b4f-ba26-422049a6ce0d', distributed).
narrative_ontology:cs_reading_relation('26575e05-6a6f-4b4f-ba26-422049a6ce0d', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('26575e05-6a6f-4b4f-ba26-422049a6ce0d', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('26575e05-6a6f-4b4f-ba26-422049a6ce0d', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('26575e05-6a6f-4b4f-ba26-422049a6ce0d', foundational, sacrificial_obligation_concluded).
narrative_ontology:cs_axiom_status(sacrificial_obligation_concluded, holdable).
narrative_ontology:cs_axiom_grounding('26575e05-6a6f-4b4f-ba26-422049a6ce0d', sacrificial_obligation_concluded, conventional).
narrative_ontology:cs_axiom('26575e05-6a6f-4b4f-ba26-422049a6ce0d', foundational, study_as_cultural_transmission).
narrative_ontology:cs_axiom_status(study_as_cultural_transmission, holdable).
narrative_ontology:cs_axiom_grounding('26575e05-6a6f-4b4f-ba26-422049a6ce0d', study_as_cultural_transmission, conventional).
narrative_ontology:cs_axiom('26575e05-6a6f-4b4f-ba26-422049a6ce0d', secondary, no_readiness_obligation).
narrative_ontology:cs_axiom_status(no_readiness_obligation, holdable).
narrative_ontology:cs_axiom_grounding('26575e05-6a6f-4b4f-ba26-422049a6ce0d', no_readiness_obligation, conventional).
narrative_ontology:cs_reference_frame('26575e05-6a6f-4b4f-ba26-422049a6ce0d', post_temple_textual_heritage).
narrative_ontology:cs_drift_state('26575e05-6a6f-4b4f-ba26-422049a6ce0d', contemporary_denominational_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('26575e05-6a6f-4b4f-ba26-422049a6ce0d', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, rabbinic_textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, secular_jewish_heritage_learners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, heritage_educational_institutions).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, study_without_practice_transmission_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot, rabbinic seminaries, and university Jewish-studies programs decide which sacrificial texts are studied, in what order, and by whom; they schedule the cycles, train the teachers, and publish the curricula. Tuition, endowments, and communal donations flow to them for running the practice, and the corpus anchors their institutional identity. They could drop or restructure the material at any time — the settlement enforces nothing — but doing so would carry accreditation, donor, and identity costs.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, heritage_educational_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, heritage_educational_institutions, beneficiary).

% Communities receive the practice's output: a shared textual heritage, comprehension of the sacrificial passages embedded in the liturgy, and continuity of memory across generations. Participation is voluntary and unenforced — a community or family that stopped studying these texts would face no penalty, only the gradual loss of the material and the connection it carries.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Commentators, rabbinic teachers, and academic scholars build careers and reputations explicating the sacrificial codes. Because the settlement attaches no obligation to the corpus, they work on it as analysts and transmitters with no halakhic stakes in the content; leaving the field would cost a career change and nothing else.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, rabbinic_textual_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Learners in community classes, online courses, museums, and heritage programs engage the sacrificial texts as history, literature, and ancestry. They take what they want, owe nothing, and exit by not re-enrolling; the practice asks no commitment and records no lapse.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, secular_jewish_heritage_learners, beneficiary,
    moderate, biographical, mobile, national).

% Families of priestly descent carry the ascribed status that the sacrificial system once gave function. Under the settlement that status has no sacrificial outlet — it survives as genealogy and a few synagogue honors — and the families were not consulted when the settlement took its current form. Their objection, that the obligation and their vocation remain live, is marginal to the institutions that run the study practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, priestly_lineage_families, excluded,
    moderate, generational, identity_locked, global).

% Groups preparing vessels, vestments, and priestly candidates for a restored cult hold that the obligation is in force and that declaring it concluded is dereliction. They operate entirely outside the study institutions, have no seat in curricular decisions, and their growth or decline is the observable test of whether the settlement is terminal.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, restorationist_temple_movements, excluded,
    powerless, generational, identity_locked, regional).

% Academic observers of ritual, law, and textual tradition track how a legal corpus survives the end of the practice it governed. They take no side in the dispute over the corpus's standing, bear no costs from the practice, and can see the whole structure — settlement, contest, and the family of readings — from outside.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, comparative_ritual_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a large, technically demanding legal corpus — the Pentateuchal sacrificial legislation, the Mishnah's sacrificial tractates, the Talmudic orders of Qodashim and Seraim, and their commentary chains — in continuous communal transmission after the practice it governed ended, coordinating curricula, teachers, study cycles, and liturgical memory so each generation can read the texts.
% TRANSFER_FUNCTION: Moves time, attention, and funding (tuition, donations, endowment income) from learners, families, and philanthropists to educational institutions and scholars; moves back textual literacy, heritage continuity, liturgical comprehension, and scholarly standing. Nothing moves under a normative claim — the settlement asserts that none exists.
% ABSENT_VOICES: Priestly-lineage families, whose ascribed vocation the settlement leaves without function, and restorationist movements, who hold the obligation live, would object that archiving the law is itself a normative act taken without them; traditionalist readers would object that stripping normative force misdescribes the study they sustain as fulfillment. All three sit outside the curricular and communal bodies that run the practice — their objections register in public dispute but not in the institutions' own deliberations.
% DISAPPEARANCE_RATIONALE: If the study practice vanished overnight, curricula would shed the sacrificial orders, scholarly fields built on those tractates would contract within a career-span, the liturgical and calendar scaffolding that assumes the corpus would lose its referent, and communal fluency in the texts would decay within a generation or two — the corpus would survive in critical editions, but the living transmission network would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE) ended the sacrificial cult, the tradition faced a corpus of detailed, binding-looking law whose practice had become impossible: preserve it, study it, or let it lapse — and if study it, with what standing? The archival settlement is the answer that keeps the texts central while declaring the obligation concluded.
% FOUNDING_PROBLEM_CORROBORATION: Academic rabbinics, manuscript-preservation scholarship, and liturgical history corroborate from outside the beneficiary set that the corpus is transmitted chiefly through study institutions and that its sacrificial passages persist in the rite as memory rather than scheduled practice. No source outside the beneficiary set attests that the settlement's 'no normative force' claim is settled — that is precisely the kernel contest, and the sibling-reading communities explicitly dispute it.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.05: the practice asserts no obligation, and the only friction is the mild dependence of schools and programs on continued enrollment — below the identity_coordination floor (0.08), i.e., within the cost of the coordination itself. Suppression is 0.08 and is a raw structural property, unscaled by scope or power: it records mild communal expectation to participate, with no sanction machinery at any point in the interval; because the enforcement picture is static (there has never been enforcement capacity to build or erode), no suppression_requirement series is authored — the scalar carries the whole story. Theater_ratio is 0.15: the transmission is real (the corpus feeds commentary, liturgical comprehension, and historical understanding), with a small ritualized share — calendar-driven study terminating in completion ceremonies — that has grown slowly across the interval (0.05 to 0.15). Accessibility_collapse is 0.35: once the settlement is understood, alternatives remain fully workable — secular academic study, heritage and museum engagement, or traditionalist study under a sibling reading — so nothing collapses. Resistance is 0.25: the practice itself meets almost no resistance, but the reading's core claim (that the obligation is concluded) is actively contested by holders of the sibling readings, and that contest is resistance to the arrangement's governing claim. The measurement series run on one shared grid (interval 0-140; 0 is approximately 1885, the era of formal abrogation declarations and the rise of academic Jewish studies; 140 is approximately 2025) with both tracked metrics authored at all eight points.
 *
 * PERSPECTIVAL GAP:
 *   From the institutions' seat the practice is a mission they administer and fund; from the communities' seat it is a voluntary heritage they could drop without sanction; from the scholars' seat it is a professional field with no halakhic stakes. All seated positions should compute as the same low-cost coordination. The divergence the engine should find runs at the boundary: an excluded traditionalist or restorationist seat, holding that the obligation is live, experiences the same corpus-study as a misdescription that suppresses their reading — for them the settlement is not a benign arrangement but one imposed without their consent. That divergence belongs to the sibling files' seats; this story authors only the archival settlement's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated agent is a beneficiary: communities receive continuity, scholars receive careers and standing, learners receive literacy and connection, institutions receive purpose and funding — so the derivation places each near the beneficiary end (low d, near-zero effective extraction). No victims are declared because the arrangement extracts from no one; there is no payer seat, and the asymmetry that usually separates seats runs instead between seated participants and the excluded claimants (priestly lineages, restorationist movements), who are not parties to the arrangement and derive no extraction from it — their grievance is with the reading, not a cost the practice imposes. Spatial scope is global (a diaspora-wide transmission network), which amplifies effective extraction modestly, but amplifying 0.05 leaves it inside the identity_coordination floor. The coordination type is genuine, not a cover story: the practice coordinates communal identity and boundary maintenance through shared textual engagement, and there is no extraction for an identity framing to disguise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting an unperformable legal corpus without re-imposing an impossible obligation or letting the material lapse — is live, and the practice still solves it, so no mandatrophy is declared and the R5 mismatch check (live status paired with a world_rearranges verdict) raises no zombie flag. The classification guards against two mislabels. Read as 'studying law that will never be practiced,' the arrangement can look like an inertial remnant — vestigial performance kept alive by institutional habit; the low theater ratio and the real downstream uses of the studied material are the evidence against that reading, and the vestigial_function_share omega tracks the slow drift (theater 0.05 to 0.15) that would eventually support it. Read from a traditionalist seat, the settlement can look like a stripping of a binding obligation relabeled as culture; that reading is the kernel contest itself and is carried by the sibling files, not resolved here. If the founding problem ever died — the corpus fully absorbed into academic editions and secular curricula — the practice would drift toward genuinely inertial territory; the terminal_or_transitional_reading omega records that contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the sacrifice_obligation_continuity kernel governs a community''s relationship to the corpus — archival preservation, study-as-fulfillment, performance-requirement, or messianic suspension?',
    'Denominational adjudication, longitudinal survey of communal practice and stated belief, or theological development within the traditions holding each reading.',
    'If a sibling reading prevails in a community, the arrangement acquires normative force: study becomes obligatory or fulfilling, extractiveness rises above the identity_coordination floor, and the classification shifts from a low-cost coordination arrangement toward a normatively enforced one. This story''s classification holds only for seats where the archival reading actually governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of a four-reading kernel; sibling readings are separate constraints with their own extractiveness and structure.').

omega_variable(
    residual_social_obligation,
    'Does the practice carry informal social enforcement — communal expectation, family pressure, identity cost of non-participation — that the ''no normative force'' framing understates?',
    'Ethnographic and survey data on participation motives and the social cost of disengagement in communities running the practice.',
    'If informal obligation is substantial, effective suppression and extraction rise above the authored scalars and the arrangement acquires a hybrid coordination-plus-pressure character; if genuinely absent, the low-cost coordination classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_social_obligation, empirical, 'Whether ''voluntary'' cultural study carries hidden social enforcement.').

omega_variable(
    vestigial_function_share,
    'What share of the study practice is live cultural transmission versus ritualized completion — calendar-driven study whose content no longer feeds commentary, liturgy, or historical understanding?',
    'Curriculum and outcome analysis: whether studied material is used in teaching, liturgical explanation, and scholarship, or terminates in completion ceremonies.',
    'A rising vestigial share would lift the theater ratio toward inertial-drift territory; the current gentle rise (0.05 to 0.15 over the interval) is the early signal this omega tracks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vestigial_function_share, empirical, 'Live transmission versus ritualized completion share of the study practice.').

omega_variable(
    terminal_or_transitional_reading,
    'Is the archival settlement a terminal resolution of the kernel or a transitional stage — a waypoint toward full secularization of the corpus or toward re-normativization under a sibling reading?',
    'Longitudinal tracking of enrollment, denominational alignment, and restorationist movement growth across generations.',
    'If transitional, the arrangement is waypoint-like in trajectory (a support that will eventually be dismantled) even without a declared sunset clause; if terminal, the low-cost coordination classification is the steady state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_or_transitional_reading, conceptual, 'Whether the archival settlement is stable or a waypoint between secularization and re-normativization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 80, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t80, observed).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 100, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t100, observed).
narrative_ontology:measurement(sacr_tr_t120, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 120, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t120, observed).
narrative_ontology:measurement(sacr_tr_t140, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 140, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 20, 0.03).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 40, 0.04).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 60, 0.04).
narrative_ontology:measurement_basis(sacr_be_t60, observed).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 80, 0.05).
narrative_ontology:measurement_basis(sacr_be_t80, observed).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 100, 0.05).
narrative_ontology:measurement_basis(sacr_be_t100, observed).
narrative_ontology:measurement(sacr_be_t120, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 120, 0.05).
narrative_ontology:measurement_basis(sacr_be_t120, observed).
narrative_ontology:measurement(sacr_be_t140, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 140, 0.05).
narrative_ontology:measurement_basis(sacr_be_t140, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel sacrifice_obligation_continuity under the epsilon-invariance principle: the colloquial label 'the sacrifice obligation' conflates four structurally distinct settlements — concluded (this file, extractiveness near zero, no normative claim), study-as-fulfillment (the obligation persists through study), performance-required (the obligation binds but cannot currently be performed — the family's highest extraction), and messianic suspension (the obligation dormant but intact). Each is authored as its own story with its own epsilon, beneficiaries, and victims; this file links all three siblings via affects_constraints. The common ancestor — the binding Temple-cult obligation itself — is upstream of all four readings and is not separately authored here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
