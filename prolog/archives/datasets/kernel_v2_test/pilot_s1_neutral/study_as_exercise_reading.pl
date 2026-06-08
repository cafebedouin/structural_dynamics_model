% ============================================================================
% CONSTRAINT STORY: study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_exercise_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_exercise_reading
 *   human_readable: Study as Exercise of Sacrifice Obligation (Halakhic Reading)
 *   domain: religious_law/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   The study-as-exercise reading of the sacrifice obligation kernel is the
 *   dominant interpretation in classical and contemporary rabbinic Judaism.
 *   When the Second Temple was destroyed in 70 CE, the obligation to bring
 *   sacrifices became impossible as a matter of fact. The reading holds that
 *   the obligation persists in the Torah and demands interpretation under
 *   these conditions. The halakhic solution: study of the sacrifice laws
 *   constitutes the legitimate exercise of the obligation. This is not mere
 *   symbolism or spiritual replacement — in this reading, intellectual
 *   engagement with the legal texts is the authentic form of the mitzvah
 *   under the current (post-Temple) conditions. The obligation is transformed
 *   but not eliminated. Competing readings interpret the same kernel (the
 *   sacrifice obligation) differently: the performance-only reading holds
 *   that without actual Temple sacrifice, the obligation is inert and no
 *   substitute exists; the messianic-suspension reading holds that the
 *   obligation is suspended until the Temple is rebuilt; the symbolic-archive
 *   reading holds that the obligation is preserved in narrative form without
 *   legal force. The study-as-exercise reading has become institutionally
 *   dominant through rabbinic interpretive authority, Talmudic grounding, and
 *   halakhic consensus. The constraint represents the ongoing work of
 *   interpretation — the obligation to keep the sacrifice law alive through
 *   study, commentary, and teaching.
 *
 * KEY AGENTS:
 *   - Torah observant individual: Obligated party seeking legitimate fulfillment (powerless/constrained) — benefits from interpretive clarity that makes the obligation actionable
 *   - Halakhic community of practice: Organized parties affirming the study reading as binding (organized/mobile) — coordinate around this interpretation; alternatives remain live but are not adopted
 *   - Rabbinic interpretive authority: Institutional beneficiary (institutional/arbitrage) — maintains monopoly on determining what counts as adequate study-as-exercise; benefits from the centrality of hermeneutic work to obligation fulfillment
 *   - Jewish legal tradition: Authority structure (institutional/constrained) — sustains the reading through textual chains of interpretation; constrained by need to maintain coherence with source material
 *   - Alternative reading adherents: Minority positions (moderate/constrained) — performance-only, messianic, and symbolic-archive readings remain live but non-dominant; constrained by burden of alternative interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_exercise_reading, 0.15).
domain_priors:suppression_score(study_as_exercise_reading, 0.08).
domain_priors:theater_ratio(study_as_exercise_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_exercise_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(study_as_exercise_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(study_as_exercise_reading, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_exercise_reading, rope).
narrative_ontology:human_readable(study_as_exercise_reading, "Study as Exercise of Sacrifice Obligation (Halakhic Reading)").
narrative_ontology:topic_domain(study_as_exercise_reading, "religious_law/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_exercise_reading, 'f9f87e07-5f83-4f4f-b37d-ba335793f5bb').
narrative_ontology:cs_kernel_codification('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', fixed_text).
narrative_ontology:cs_authority_grounding('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', lineage).
narrative_ontology:cs_interpretation_layer_present('f9f87e07-5f83-4f4f-b37d-ba335793f5bb').
narrative_ontology:cs_reading_relation('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', study_as_exercise_reading__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', study_as_exercise_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', study_as_exercise_reading__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', foundational, obligation_persists_post_temple).
narrative_ontology:cs_axiom_status(obligation_persists_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', obligation_persists_post_temple, deontological).
narrative_ontology:cs_axiom('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', foundational, study_constitutes_legitimate_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_legitimate_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', study_constitutes_legitimate_fulfillment, deontological).
narrative_ontology:cs_axiom('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', secondary, transformation_not_suspension).
narrative_ontology:cs_axiom_status(transformation_not_suspension, holdable).
narrative_ontology:cs_axiom_grounding('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', transformation_not_suspension, conventional).
narrative_ontology:cs_reference_frame('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', obligation_fulfilled_through_study_of_sacrifice_law).
narrative_ontology:cs_drift_state('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', contemporary_communal_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f9f87e07-5f83-4f4f-b37d-ba335793f5bb', '2026-06-07T14:32:18Z').
narrative_ontology:cs_kernel_id(study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, halakhic_community_of_practice).
narrative_ontology:constraint_victim(study_as_exercise_reading, torah_obligated_individual).
narrative_ontology:constraint_vindicates(study_as_exercise_reading, suspension_doctrine_talmudic_legitimacy).
narrative_ontology:constraint_vindicates(study_as_exercise_reading, intellectual_engagement_as_mitzvah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the obligation to engage with sacrifice law through study; fulfills the obligation through intellectual engagement with texts and interpretations. Constrained by their knowledge level (deeper study requires education), available time (study demands consistent engagement), and social position (must affirm the community's accepted reading to maintain standing). Can exit by adopting alternative readings, but at cost of social marginalization or community rupture.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, torah_obligated_individual, payer,
    powerless, biographical, constrained, local).

% Controls the interpretation of what constitutes adequate study-as-exercise. Sets standards for fulfillment, trains the next generation of interpreters, generates the authoritative texts and rulings that define the obligation's requirements. Benefits from institutional centrality to the obligation's fulfillment — the more deeply study is emphasized, the more authority falls to those who interpret it. Can exit by ceding authority to alternative institutional sites (Temple restoration movements, fundamentalist readings) but retains current power through the dominance of the study reading.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(study_as_exercise_reading, rabbinic_interpretive_authority, beneficiary).

% Affirms and perpetuates the study reading through communal practice, education, ritual integration (Torah reading in services, Talmud study circles, teaching). Coordinates around shared interpretation; maintains the reading across geographical and temporal scatter through institutional structures (synagogues, yeshivas, publications). Can adopt alternative readings but chooses not to; can migrate to alternative religious frameworks entirely.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, halakhic_community_of_practice, beneficiary,
    organized, generational, mobile, global).

% The accumulated body of Talmudic discussion, medieval commentaries, and contemporary rulings that constitutes the framework within which interpretations are validated. The tradition sustains the study reading through textual authority (Menachot 110a and parallel discussions of substitution, authority of interpretive chains across centuries). Is itself 'agentified' in the sense that it exerts pressure on interpreters through its own internal coherence requirements.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, jewish_legal_tradition, beneficiary,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_non_agent(study_as_exercise_reading, jewish_legal_tradition).

% Would reject the study reading in favor of messianic or literal performance readings. Prepares for actual Temple sacrifice; treats study as preliminary or symbolic rather than as fulfillment. Excluded from the dominant halakhic conversation but maintains minority interpretive position. Not suppressed but sidelined within mainstream Jewish institutional structures.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, temple_restoration_movement, excluded,
    moderate, generational, constrained, regional).

% The Talmudic principle that obligations can be suspended or transformed when conditions for performance change. Not an agent in its own right but a structural principle that grounds the reading's exegetical legitimacy. The doctrine operates as an authority structure underneath the specific reading.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, suspension_doctrine_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(study_as_exercise_reading, suspension_doctrine_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How can a binding textual obligation (sacrifice) remain authoritative when its literal performance is impossible? The obligation must be interpreted or abandoned; the study reading interprets it as transformed rather than terminated.
% TRANSFER_FUNCTION: The reading transfers authority from Temple-based priestly practice to scholarly interpretation. The obligation moves from ritual performance to textual engagement. What flows is interpretive authority — from those who could bring sacrifices to those who can correctly expound the law.
% ABSENT_VOICES: Voices absent from the dominant study reading include: Temple restoration movements (who would demand actual sacrifice); secular Jews (who reject the obligation entirely as binding); philosophical critics (who question whether study can fulfill a sacrifice obligation); minority fundamentalist positions (who treat the obligation as suspended, not transformed). These voices exist but are not in the room of mainstream halakhic authority.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise reading disappeared and were replaced by performance-only or messianic readings, Jewish practice would rearrange substantially. The obligation's role in community identity, education, and institutional structure would change. However, the underlying obligation (sacrifice in Torah) would remain and would demand interpretation. The world would not return to a state without the constraint — it would reorganize around alternative readings of the same constraint.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE removed the physical and institutional capacity to fulfill the sacrifice obligation through literal performance. Yet the obligation remains in the Torah's text as a binding command. Jewish law requires both (a) that the obligation be taken seriously and (b) that Judaism remain viable as a lived religious practice. The founding problem is the tension between these two requirements.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Temple destroyed, obligation remains) is attested by historical fact (Josephus, archaeological evidence, rabbinic sources acknowledging the reality of exile). The rabbinic response (substitution through study) is documented in Talmudic discussion (Menachot 110a, Eruvin 21b) where the principle of substitution is debated. However, the founding problem's 'status' is interpreted differently by different readings: study reading treats it as 'dead' (solved by transformation); messianic reading treats it as 'live' (awaiting restoration); performance-only treats it as 'definitively closed' (no solution possible). The corroboration is historical; the interpretation is contested.
narrative_ontology:disappearance_verdict(study_as_exercise_reading, contested).
narrative_ontology:founding_problem_status(study_as_exercise_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TORAH OBSERVANT INDIVIDUAL (ROPE) — Genuine coordination problem solved: the sacrifice obligation persists in the legal corpus and demands interpretation. Study resolves the binding question (how to fulfill when Temple is destroyed) without external coercion. The individual benefits from interpretive clarity; the framework provides legitimate path. Suppression minimal — the reading is held by mainstream rabbinic authority and accepted within halakhic communities. No victim apparent under this reading: suspension is authorized transformation of the obligation, not suppression of it.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: HALAKHIC COMMUNITY OF PRACTICE (ROPE) — Organized commitment to the study reading as the operative form of the obligation. The reading enables coordination across dispersed Jewish communities that lack Temple infrastructure. Mobile exit: communities that reject the suspension doctrine (messianic communities, some fundamentalist groups) do maintain sacrificial preparations or claims; they are not coerced into study-as-fulfillment. The community recognizes alternatives exist but affirms this reading as the binding one. Pure coordination: the reading stabilizes the obligation across geographical and temporal scatter.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Benefits from the study reading through institutional monopoly on what counts as proper fulfillment. The reading requires constant interpretive work (determining which texts constitute sufficient study, what constitutes adequate engagement, how the obligation maps onto contemporary mitzvah practice). Arbitrage exit: alternative readings (performance-only, messianic suspension) would shift authority to different institutional sites (Temple re-establishment, messianics, fundamentalist movements). This reading maintains rabbinic hermeneutic control. Low extractiveness (0.15) because the coordination function is genuine and substantial: the Torah's text demands interpretation, and the suspension doctrine is a coherent solution, not a pretext. Beneficiary status is real but not extractive in the snare sense.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JEWISH LEGAL TRADITION AS AUTHORITY STRUCTURE (ROPE) — The Talmudic discussion of substitution and suspension grounds the study reading in centuries of precedent. The tradition itself sustains the reading through textual interpretation chains (rishonim, acharonim, contemporary poskim). Constrained exit: the reading is bound to maintain coherence with earlier sources; radical departure requires demonstrating internal contradiction in prior authorities. This constraint is the ongoing act of interpretation itself — not enforcement, but active hermeneutic work maintaining the reading's legitimacy.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LOGICAL STRUCTURE (ANALYTICAL/MOUNTAIN) — From a universal logical perspective, any binding textual obligation must generate an interpretation when conditions for performance are absent. The obligation persists in the text; the obligation logically demands fulfillment or legitimate suspension. Study-as-exercise is one coherent answer to the structural problem. However, this mountain perspective risks naturalizing what is actually a contested reading chosen among alternatives. The logical structure itself does not dictate which interpretation is correct — performance-only, messianic, and symbolic-archive readings are equally coherent responses to the same logical situation. The mountain view is aspirational; the engine will identify this as a false summit if beneficiary presence is declared.
constraint_indexing:constraint_classification(study_as_exercise_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MINIMAL ENGAGEMENT OBSERVER (PITON) — A significant portion of observant practice treats the study obligation as largely performative: reciting Torah, attending services where sacrifice texts are chanted, symbolic engagement with the concept rather than deep intellectual work. Theater_ratio high (0.65) at this level — the studied obligation becomes ritualized practice, invocatory rather than transformative. The reading persists through institutional momentum and community participation without deep engagement. This perspective reveals degradation: the original reading (intellectual transformation through study) has atrophied into formal compliance.
constraint_indexing:constraint_classification(study_as_exercise_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_exercise_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(study_as_exercise_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(study_as_exercise_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(study_as_exercise_reading, TR),
    TR >= 0.70.

:- end_tests(study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The study reading solves a genuine coordination problem — the Torah's sacrifice obligation must be interpreted when sacrifice is impossible. The reading is exegetically coherent (grounded in Talmudic discussion of substitution). The primary beneficiary (rabbinic interpretive authority) benefits from maintaining centralized control over fulfillment criteria, but this benefit flows from a real institutional function (hermeneutic work), not from artificial scarcity or suppression. The obligation would require interpretation under any reading, so the extraction is minimal. Suppression (0.08): Very low. The reading is institutionally dominant and widely accepted within halakhic communities. Alternatives exist and are not forcibly suppressed — they are intellectually engaged (minority Talmudic commentaries, contemporary fundamentalist and messianic positions). Exit from the study framework is constrained but not trapped: individuals or communities can adopt alternative readings, though at social cost. Theater ratio (0.22): Low-moderate. The core intellectual work of studying sacrifice laws is genuine — interpretive engagement with texts, legal analysis, commentary traditions. However, degradation is evident (Perspective 6): much communal engagement with the obligation has become ritualized invocation (Torah chanting in services, formal mention in prayer) rather than deep study. The reading persists partly through ongoing interpretive work and partly through performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The study reading exhibits minimal perspectival gap because the structural benefit aligns with the interpretive grounding. All perspectives from within halakhic framework see coordination or pure benefit, not extraction. The analytical (mountain) perspective risks falsely naturalizing the reading as necessary, when it is actually one contestable option among coherent alternatives. The piton perspective (minimal engagement observer) reveals degradation: what began as intellectual transformation has atrophied into formal compliance. The gap is not between perspectives on whether this is rope (they agree) but between the rope reading and the counterfactual alternative readings that would shift authority structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is low because the structural relationships are genuinely coordinative, not extractive. The obligated individual (powerless/constrained) experiences the study reading as legitimate fulfillment without coercion — the suppression of alternatives is intellectual (other interpretations are less compelling) rather than institutional (suppressed by force). Rabbinic authority (institutional/arbitrage) benefits from the reading's dominance but does not achieve this benefit through capturing or closing alternatives — the benefit flows from the interpretive work the obligation requires. There are no masked victims: the obligation's transformation through the reading is an authorized reinterpretation, not a suppressed class bearing costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy through authorized transformation: the obligation is not abandoned, but the mandate (bring sacrifices) is no longer applicable. The mandate has died as a literal command, but the obligation persists through reinterpretation. This is a genuine case of mandate obsolescence met by authentic (rather than performative) institutional response. The reading does not deny the original mandate or claim it was never binding; it asserts that the obligation binds differently under changed conditions. The minimal extractiveness (0.15) reflects this — there is no class of victims because the transformation is theoretically justified and institutionally transparent. If extractiveness were higher (as it might be under alternative readings where rabbinic authority suppresses non-study interpretations), mandatrophy would become active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_doctrine_authority,
    'What grounds the halakhic authority to interpret the sacrifice obligation as substitutable by study? Is the suspension doctrine itself a divine principle or a human interpretive choice?',
    'Textual analysis of Talmudic derivation of the suspension doctrine (Menachot 110a and parallels); examination of whether the doctrine is presented as exegetical discovery or legal creativity; comparison with other cases where obligations are suspended or substituted',
    'If divine principle: the study reading is mountain-like in its necessity — any competent interpreter reaches the same conclusion. If human choice: the reading is one contestable option among alternatives, and extractiveness scales upward if alternative readings are suppressed rather than merely superseded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_doctrine_authority, conceptual, 'Authority grounding of the suspension doctrine').

omega_variable(
    study_threshold_ambiguity,
    'What constitutes sufficient intellectual engagement to fulfill the obligation through study? Is there a minimum threshold, or is any study engagement legitimate?',
    'Survey of contemporary halakhic rulings on study obligation sufficiency; examination of rabbinic debate over depth requirements (systematic study vs. casual engagement); analysis of whether threshold is tied to scholarly attainment or intention',
    'If threshold exists and is enforceable: extractiveness rises because enforcement requires gatekeeping (who determines adequacy). If no threshold: extractiveness stays low because engagement is genuinely substitutable. If threshold is knowledge-dependent: organized groups with interpretive authority control certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_threshold_ambiguity, empirical, 'Sufficiency criteria for study-as-fulfillment').

omega_variable(
    alternative_reading_suppression,
    'Are sibling readings (performance-only, messianic, symbolic-archive) actively suppressed, tolerated as minority positions, or foreclosed as incoherent?',
    'Historical and contemporary analysis of how non-study readings are treated in halakhic discourse; examination of whether heretical designation is applied; assessment of institutional pressure against alternative readings',
    'If suppressed: extractiveness rises due to enforced monopoly. If tolerated: extractiveness drops because alternatives remain live. If foreclosed: reading is genuinely foundational, not a contingent choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression, empirical, 'Treatment of alternative readings of the sacrifice obligation').

omega_variable(
    reading_and_beneficiary_confounding,
    'Does the study reading persist because it is exegetically correct, or because it benefits rabbinic institutional authority? Can these be separated?',
    'Counterfactual analysis: if alternative readings (e.g., messianic) would grant authority to different institutional structures, what pressure exists toward the study reading? Cross-reading comparison of exegetical strength vs. institutional incentive alignment',
    'If the reading is exegetically strongest: classification holds. If institutional incentive is a confounding variable: classification may be tangled_rope rather than rope (asymmetric extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_and_beneficiary_confounding, conceptual, 'Separation of exegetical vs. institutional drivers of the reading').

omega_variable(
    suspension_doctrine_as_reading_pivot,
    'Is the study-as-exercise reading genuinely distinct from the performance-only reading on the ground of textual interpretation, or are they the same constraint read through different institutional frames?',
    'Analysis of the kernel: the sacrifice obligation. Both readings accept the obligation; both accept that Temple sacrifice is impossible. The readings diverge on whether the obligation persists (study reading) or is suspended (performance-only). This is a genuine textual divergence — the question is whether it is a kernel reading (different readings of one obligation) or two separate constraints (one about the obligation''s status, one about how it is fulfilled).',
    'If genuine kernel reading: epsilon-invariance holds and the omegas correctly document irreducible interpretive divergence. If two separate constraints: the epsilon-invariance principle requires decomposition into distinct stories with independent epsilon values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_doctrine_as_reading_pivot, conceptual, 'Whether study-as-exercise is a kernel reading or a separate constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_ex_theater_t0, study_as_exercise_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(study_ex_theater_t500, study_as_exercise_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement(study_ex_theater_t1000, study_as_exercise_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(study_ex_theater_t1500, study_as_exercise_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(study_ex_theater_t2000, study_as_exercise_reading, theater_ratio, 2000, 0.25).

% Extraction over time
narrative_ontology:measurement(study_ex_extract_t0, study_as_exercise_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(study_ex_extract_t500, study_as_exercise_reading, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(study_ex_extract_t1000, study_as_exercise_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(study_ex_extract_t1500, study_as_exercise_reading, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement(study_ex_extract_t2000, study_as_exercise_reading, base_extractiveness, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(study_as_exercise_reading, 0.1).
narrative_ontology:affects_constraint(study_as_exercise_reading, performance_only_reading).
narrative_ontology:affects_constraint(study_as_exercise_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(study_as_exercise_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice obligation kernel has four structurally distinct readings, each with different epsilon values and institutional implications. study_as_exercise_reading (this file) has low epsilon because the coordination function is genuine and the reinterpretation is authorized. performance_only_reading would have zero epsilon (no obligation active). messianic_suspension_reading would have low-to-moderate epsilon (obligation alive but dormant; rabbinic authority defers to future restoration). symbolic_archive_reading would have moderate epsilon (obligation transformed to narrative form; ambiguity about whether legal force persists). These are not the same constraint viewed differently — they are different constraints arising from different readings of the same kernel. Each reading has its own beneficiary/victim structure, its own interpretive authority, and its own institutional implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
