% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (Cultural-Historical Reading)
 *   domain: religious law / cultural identity / commitment-system dynamics
 *
 * SUMMARY:
 *   The kernel is the standing body of sacrificial law — Leviticus and the
 *   Mishnah/Talmud order of Qodashim — whose status after the cessation of
 *   the cult is contested across four readings. This file instantiates
 *   exactly one of them, the symbolic_archive_reading, as a clean
 *   epsilon-invariant constraint (Rule 1): the standing arrangement under
 *   contest is the contemporary practice in which dispersed Jewish
 *   communities transmit detailed knowledge of the sacrificial order through
 *   study framed as heritage and memory. Assessed BY THIS READING'S OWN
 *   LIGHTS, that arrangement is a voluntary cultural archive: no binding
 *   obligation exists to be violated, no enforcement machinery operates, no
 *   victim set can be named, and the epsilon referent is this standing
 *   arrangement — never the rights-respecting or obligation-restoring
 *   alternative any reading would endorse. Holders of the three sibling
 *   readings assess the SAME referent and author different epsilon values in
 *   their own files (OQ-26: epsilon is a property of a reading, not a topic);
 *   those files are separate constraints, linked here via
 *   network.affects_constraints. The claim and the metrics are independent
 *   authored facts: claimed_type rope states what this reading holds
 *   structurally true; the metrics state what is descriptively true of the
 *   arrangement's operation, including its small residual costs and slow
 *   drift toward heritage performativity.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: primary beneficiary (organized/mobile) — sustains identity through voluntary archive study; bears no involuntary cost
 *   - torah_scholars_and_students: beneficiary (moderate/mobile) — converts study into meaning, craft, and standing
 *   - jewish_heritage_educators: beneficiary (moderate/mobile) — teaches the archive without normative burden
 *   - heritage_curriculum_designers: agenda-setter (moderate/mobile) — shapes the archive's contours by persuasion, not enforcement
 *   - traditionalist_halakhic_authorities: excluded dissenter (powerful/locked out of the framing conversation) — holds sibling readings; no seat in this arrangement
 *   - academic_judaica_scholars: analytical observer (institutional/analytical) — documents the memory-preservation project
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.04).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (Cultural-Historical Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious law / cultural identity / commitment-system dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'df2c2233-9a98-4d12-b45d-a6f682e4f63e').
narrative_ontology:cs_kernel_codification('df2c2233-9a98-4d12-b45d-a6f682e4f63e', fixed_text).
narrative_ontology:cs_authority_grounding('df2c2233-9a98-4d12-b45d-a6f682e4f63e', lineage).
narrative_ontology:cs_interpretation_layer_present('df2c2233-9a98-4d12-b45d-a6f682e4f63e').
narrative_ontology:cs_reading_relation('df2c2233-9a98-4d12-b45d-a6f682e4f63e', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('df2c2233-9a98-4d12-b45d-a6f682e4f63e', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('df2c2233-9a98-4d12-b45d-a6f682e4f63e', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_axiom('df2c2233-9a98-4d12-b45d-a6f682e4f63e', foundational, sacrifice_law_carries_no_operative_normative_force).
narrative_ontology:cs_axiom_status(sacrifice_law_carries_no_operative_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('df2c2233-9a98-4d12-b45d-a6f682e4f63e', sacrifice_law_carries_no_operative_normative_force, conventional).
narrative_ontology:cs_axiom('df2c2233-9a98-4d12-b45d-a6f682e4f63e', secondary, textual_study_preserves_collective_continuity).
narrative_ontology:cs_axiom_status(textual_study_preserves_collective_continuity, holdable).
narrative_ontology:cs_axiom_grounding('df2c2233-9a98-4d12-b45d-a6f682e4f63e', textual_study_preserves_collective_continuity, instrumental).
narrative_ontology:cs_reference_frame('df2c2233-9a98-4d12-b45d-a6f682e4f63e', sealed_postcult_archive).
narrative_ontology:cs_drift_state('df2c2233-9a98-4d12-b45d-a6f682e4f63e', contemporary_temple_revivalist_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('df2c2233-9a98-4d12-b45d-a6f682e4f63e', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, torah_scholars_and_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_heritage_educators).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_memory_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sustain connection to the Temple-era past through cyclical study of the sacrificial tractates, synagogue and museum programming, and lifecycle teaching. Participation is chosen; a community that dropped the practice would face no sanction and would continue its identity work through the many other available threads of language, calendar, and text.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Read Zevahim, Menahot, and related material as intellectual inheritance and analytic craft-training. They receive meaning, scholarly standing, and communal embeddedness from the engagement, and they can redirect their study hours to other corpora at any time without penalty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, torah_scholars_and_students, beneficiary,
    moderate, biographical, mobile, global).

% Teach the sacrificial material in schools, adult education, and public-history settings. The archive framing gives them pedagogical latitude to present the ancient cult historically, without carrying or transmitting a normative demand; alternative curricula remain open to them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_heritage_educators, beneficiary,
    moderate, biographical, mobile, regional).

% Decide which sacrificial texts enter syllabi, exhibitions, and study cycles, and in what register they appear. Their choices shape the archive's contours, but they bind no one: their authority rests on persuasion and institutional position, and there is no enforcement machinery behind their decisions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, heritage_curriculum_designers, agenda_setter,
    moderate, biographical, mobile, national).

% Hold that the sacrificial laws retain normative force — to be performed when the cult is possible, suspended until restoration, or discharged through study — and regard the archive framing as erasing a live divine claim. They stand outside the curricular and cultural conversation this reading organizes, with no seat in it from which to press their position; their dissent lives in sermons, responsa, and seminary halls adjacent to, but not inside, the heritage frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, traditionalist_halakhic_authorities, excluded,
    powerful, generational, trapped, global).

% Document and analyze how post-destruction communities preserved cultic memory, tracing the transmission of Qodashim material through manuscripts, curricula, and communal practice. They neither collect nor pay under the arrangement; their analyses feed the archive's own self-understanding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, academic_judaica_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains transgenerational, trans-diasporic continuity of memory of the sacrificial order and its legal imagination: dispersed communities solve the problem of keeping formative cultic knowledge alive across generations by sharing a canonical study corpus, so no community must reconstruct the material independently.
% TRANSFER_FUNCTION: Moves voluntary attention, study time, and teaching effort from individuals into communal memory-work, and moves scholarly standing and pedagogical authority among participants. No money, labor, or status moves compulsorily from anyone to anyone.
% ABSENT_VOICES: Traditionalist halakhic authorities holding the performance, suspension, or study-as-exercise readings would object that the archive framing strips the material of normative force it still carries. They are present in the same communities — in seminaries, responsa, and liturgical life — but absent from the curricular and heritage conversation this reading frames, where the material is presented as settled cultural inheritance rather than contested obligation.
% DISAPPEARANCE_RATIONALE: If the archival study practice vanished overnight, curricula, study cycles, museum programming, and a scholarly field would lose their object; communities would lose one thread of identity transmission and would reorganize continuity work around the remaining threads — language, calendar, lifecycle, other textual corpora. The rearrangement is real but modest: nothing coercive collapses, and no one is freed from a burden because no burden exists.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the community faced the problem of retaining knowledge and memory of the sacrificial order — the operating system of its formative era — once the cult itself was impossible. The archive reading's version of that problem: how does a post-cultic, dispersed community keep continuity with its cultic past without a functioning cult?
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of ancient Judaism and comparative-ritual scholars — parties outside the beneficiary set — corroborate that post-destruction rabbinic communities undertook deliberate projects of cultic-memory preservation and that continuity-under-dispersion remains an active problem for diaspora communities facing assimilation pressure. Attestation from within the tradition comes from the beneficiaries themselves and is therefore discounted; the external historiography is the named corroborating source.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.08 sits at the identity_coordination Boltzmann floor (0.08): the residual cost is the inherent price of identity work — time and attention voluntarily spent — not extractive overhead. Suppression 0.04 is authored as a raw structural property, unscaled by power or scope (only extractiveness is engine-scaled): encouragement and habit operate, but no enforcement machinery exists. Theater_ratio 0.12 is low because the activity genuinely performs its declared function — continuity of memory — though the series shows slow growth as heritage display (reenactments, symbolic exhibits, commodified programming) takes a modest share alongside substantive engagement. Accessibility_collapse 0.18: alternatives do not collapse at all — language, calendar, cuisine, lifecycle, and other textual corpora remain fully open as identity carriers, which is precisely what keeps the arrangement voluntary. Resistance 0.22 reflects mostly intra-communal contestation from holders of sibling readings rather than resistance to the practice itself. Rope is the structurally true claim: a genuine collective-action problem (transgenerational memory across dispersion) solved with minimal overhead, net beneficiaries throughout, and unsuppressed alternatives. Both measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the gentle upward drift in both tracks the growth of the paid heritage industry, not degradation of the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The participant seats — communities, scholars, educators, curriculum designers — should compute nearly identically: all sit near the beneficiary end of directionality and experience the arrangement as benign coordination. The divergent seat is the excluded traditionalist authority: from that position the same corpus is not an archive but a live claim awaiting performance, suspension, or discharge — a different constraint altogether, authored in the sibling files. The engine registers this divergence through the excluded seat's role, power, and exit data; this file's claim does not adjudicate it, and per-seat computation is expected to show the participant seats converging on rope while the excluded seat's computed relationship reflects contest rather than capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map directly onto structural reality: diaspora communities, scholars, and students all receive identity, meaning, and standing from the arrangement and bear only chosen costs, placing them near d = 0. Curriculum designers sit marginally above pure beneficiary — they shape the archive and derive influence from it, but serve it rather than feeding on anyone's contribution. No victims exist under this reading, so no seat derives a high d. One override is declared: the sole powerful seat, traditionalist_halakhic_authorities, would inherit a power-keyed derivation fallback that reads powerful actors as extractor-adjacent; structurally they neither collect nor pay here — they are dissenters locked out of the conversation — so d is overridden to 0.5 (symmetric/unaffected). Global scope for the community and scholar seats raises verification difficulty marginally, but with epsilon at the coordination floor the scope amplification is negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabeling risks are guarded against. First, the piton temptation: the corpus is literally a remnant of a destroyed cult, inviting vestigial or theatrical coding. The piton test is cost-asymmetry and function-liveness, and both fail for piton here — the function (identity continuity under dispersion) is live and genuinely served, theater_ratio is low, and there is no administrator who could change the arrangement more cheaply than bearing it, because there is nothing burdensome to bear. Correspondingly, the founding problem (preserving cultic memory after 70 CE, in its modern form continuity under assimilation pressure) is live, and status=live crossed with verdict=world_rearranges produces no zombie flag. Second, the snare temptation from the sibling side: holders of the performance and suspension readings see the archive framing as evasion of a binding duty, and from their seats the arrangement may indeed look like suppressed obligation. That judgment belongs to the sibling files, where binding force creates enforceable structure and identifiable losers; folding it into this file would break epsilon-invariance by averaging two constraints into one label. The rope classification here prevents the reverse error as well — misreading voluntary, net-beneficial coordination as covert extraction simply because its subject matter is an ancient obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the symbolic_archive_reading of the sacrifice_obligation_kernel: is the standing arrangement best described as a closed cultural archive making no halakhic claim, or does one of the sibling ontologies (discharged-by-study, performance-demanding, divinely suspended) correctly describe a live obligation?',
    'Intra-communal halakhic deliberation and reception history: track whether authoritative communal practice treats the corpus as normatively inert heritage or as carrying latent obligation, and which venues of authority (responsa, curricula, liturgy) absorb which framing.',
    'Adopting any sibling reading replaces this near-zero-extraction rope with a constraint carrying binding force, enforcement expectations, and potentially identifiable victims (those unable to perform or excused from readiness); the family''s classifications must be read per-reading and never averaged across the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer content: which reading of the sacrifice-obligation kernel this constraint instantiates, and what the siblings would change structurally.').

omega_variable(
    existential_disagreement_location,
    'Where exactly do the four readings disagree — on the justification of a shared obligation, or on the existence of any presently-binding normative fact?',
    'Conceptual analysis of each reading''s core premise: archive (no claim exists), exercise (a claim exists and is discharged by study), performance (a claim exists and demands action), suspension (a claim exists, paused by divine decree).',
    'Because the contest is existential, the readings mutually foreclose within any single framework — unlike justification contests, which coexist across parties; the foreclosure edges in cs_structure encode this and drive engine-computed displacement, so mislocating the disagreement (as mere opinion difference) would corrupt the family''s relation graph.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(existential_disagreement_location, conceptual, 'Location of the kernel disagreement: existence of the normative fact, not its warrant.').

omega_variable(
    soft_competence_voluntariness,
    'Is participation in archive study fully voluntary, or does communal belonging exert soft compulsion that the near-zero-suppression picture understates?',
    'Exit-cost comparison: contrast identity outcomes, social standing, and family pressure for community members who decline participation versus those who engage, across communities of differing intensity.',
    'If soft compulsion is material, effective extraction rises above the identity_coordination floor and the type drifts from rope toward tangled_rope with diffuse payers; if participation is genuinely costless to decline, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_competence_voluntariness, empirical, 'Boundary of the voluntariness claim: structural openness versus internalized communal expectation.').

omega_variable(
    revivalist_conversion_risk,
    'Will Temple-revivalist movements convert the archived corpus back into operative expectation, overriding this reading within parts of the community?',
    'Track the institutional growth of restorationist projects, liturgical and educational innovation around the sacrificial order, and the political salience of the Temple site over coming decades.',
    'If conversion proceeds, this reading becomes locally overridden and the affected communities'' constraint migrates toward performance or suspension structures with real enforcement stakes; the drift_state''s repudiation_pressure vector would deepen from minor toward substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revivalist_conversion_risk, empirical, 'Persistence question: whether the archive frame holds or is displaced by revivalist repudiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(sacr_tr_t10, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t50, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement_basis(sacr_be_t30, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement_basis(sacr_be_t50, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement_basis(sacr_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrifice obligation' covers four structurally distinct claims that share one referent (the post-cultic standing arrangement) and diverge on whether a presently-binding normative fact exists and in what mode. This file (symbolic_archive_reading) authors epsilon near zero with no victim set; the exercise reading authors a modestly loaded arrangement; the performance and suspension readings author arrangements with binding force, enforcement expectations, and potential victim sets (those unable to perform or exempted from readiness). The archive reading is downstream of the historical fact of the cult's cessation and upstream of nothing coercive; sibling files carry their own stories, and cross-reading comparison is valid only per-file, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__symbolic_archive_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
