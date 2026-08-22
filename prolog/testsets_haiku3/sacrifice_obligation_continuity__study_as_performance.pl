% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity via Textual Study
 *   domain: religious_law/ritual_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the sacrifice-obligation
 *   kernel: the reading that textual study itself constitutes
 *   obligation-fulfillment. After the Temple's destruction, the diaspora
 *   community faced a coordination crisis: the obligation persisted in
 *   textual sources but could not be performed in its original form. The
 *   study-as-performance reading solved this by redefining fulfillment as
 *   intellectual engagement with the texts. This reading emphasizes
 *   continuity of obligation through transformation of practice, and treats
 *   the interpretive community as the legitimate seat of the obligation. The
 *   constraint is low-extraction (study is widely accessible, no scarce
 *   resource), low-suppression (the reading is continuously affirmed, not
 *   defended against external coercion), and benefits the interpretive
 *   community and textual tradition by keeping both alive and normatively
 *   bound. Importantly, this is NOT the only possible reading—sibling
 *   readings contest whether study is fulfillment (performance-only),
 *   suspension (messianic), or mere preservation (archival).
 *
 * KEY AGENTS:
 *   - Interpretive community: scholars, students, religious practitioners who engage with sacrifice texts; benefit from reading study as normatively binding
 *   - Halakhic authority: institutional adjudicators of obligation fulfillment; agenda-setters who maintain the reading through continuous citation and refinement
 *   - Textual tradition: the body of sacrifice law sources; kept alive by the constraint's requirement of continuous engagement
 *   - Performance-only adherents: excluded; hold that only physical sacrifice or Temple performance counts as fulfillment
 *   - Messianic-suspension holders: excluded; read obligation as suspended pending restoration, not fulfilled by study
 *   - Archival preservationists: absent; treat sacrifice law as historical content, rejecting normative obligation entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.18).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity via Textual Study").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '39f97aaf-98e9-4f6e-90da-31f4a44fff1c').
narrative_ontology:cs_kernel_codification('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', fixed_text).
narrative_ontology:cs_authority_grounding('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', lineage).
narrative_ontology:cs_interpretation_layer_present('39f97aaf-98e9-4f6e-90da-31f4a44fff1c').
narrative_ontology:cs_reading_relation('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', study_constitutes_fulfillment, conventional).
narrative_ontology:cs_axiom('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', foundational, obligation_persists_post_temple).
narrative_ontology:cs_axiom_status(obligation_persists_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', obligation_persists_post_temple, deontological).
narrative_ontology:cs_reference_frame('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', textual_engagement_as_obligation_fulfillment).
narrative_ontology:cs_drift_state('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', contemporary_secular_jewish_modernity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('39f97aaf-98e9-4f6e-90da-31f4a44fff1c', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, interpretive_community).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, textual_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, students, and religious practitioners who engage with sacrifice law texts. They benefit from the reading that frames study itself as obligation-fulfillment: this legitimates their intellectual work as religious practice rather than mere historical or academic exercise. The constraint grants their study normative weight and integrates their intellectual labor into the religious economy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, interpretive_community, beneficiary,
    organized, civilizational, mobile, global).

% The body of halakhic and biblical texts on sacrifice. This reading keeps the texts alive as binding sources rather than historical artifacts. The constraint ensures continuous engagement and reinterpretation, preventing the texts from sliding into archival status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__study_as_performance, textual_tradition).

% Those who hold that sacrifice obligation requires literal animal sacrifice or physical Temple performance, not merely textual study. They would argue that study is preparation at best, not fulfillment. Their position is structurally incompatible with the study-as-performance reading within a single halakhic framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, performance_only_adherents, excluded,
    moderate, generational, constrained, regional).

% Those who read the obligation as suspended—neither fulfilled nor violated—pending messianic restoration. They see study as maintaining readiness, not as actual fulfillment. Their reading coexists with study-as-performance in the broader tradition but represents a different normative claim about the obligation's current status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_suspension_holders, excluded,
    moderate, civilizational, constrained, regional).

% Secular scholars and some liberal religious practitioners who treat sacrifice law as historical content to be preserved and studied, not as binding obligation. They reject the normative framing entirely. Their absence from the conversation means the constraint's legitimacy depends on the interpretive community's internal consensus.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, archival_preservationists, excluded,
    moderate, biographical, mobile, global).

% The bodies of recognized halakhic interpretation (rabbinic courts, textual authorities, scholarly consensus) that adjudicate what counts as obligation fulfillment. They maintain the reading that study qualifies as performance by continuously citing, interpreting, and refining the textual and precedential grounds for this position. Their authority is grounded in lineage and the tradition's own interpretive practices.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, halakhic_authority, agenda_setter,
    institutional, civilizational, trapped, regional).

% The comparative-religious and historical-textual analysis seat. Documents how different readings of the kernel compete, which texts and precedents support or undermine each reading, and how shifts in living practice correlate with shifts in textual interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-Temple coordination problem: how does a dispersed diaspora community maintain religious obligation when the only site of its original performance (the Temple) no longer exists? The reading coordinates all members around a common, accessible practice (textual study) that counts as legitimate obligation-fulfillment, binding scholars, students, and practitioners into a unified interpretive community across geography and time.
% TRANSFER_FUNCTION: Transfers authority and normative weight from institutional Temple sacrifice to individual intellectual and textual engagement. The obligation no longer depends on centralized priestly performance but on distributed study. What moves is legitimacy: from physical ritual to mental engagement; from professional priests to any community member; from one site to infinite sites of interpretation.
% ABSENT_VOICES: Performance-only adherents and messianic-suspension holders are excluded from the conversation that adjudicates whether study counts as fulfillment. Archival preservationists (secular scholars) are absent because they reject the normative frame entirely. Their absence means the constraint's persistence rests on consensus among those who accept the study-as-performance reading—a narrower party set than the full range of Jewish textual engagement.
% DISAPPEARANCE_RATIONALE: If the reading that study-as-performance fulfills the obligation disappeared, the diaspora religious community would face a coordination crisis: either the obligation would be read as dormant/suspended (messianic reading), or it would be reduced to a memory practice (archival reading), or adherents would have to sustain expensive preparation for eventual Temple restoration (performance-only reading). The constraint's disappearance would force a reorganization around a different reading, not a collapse of the textual tradition itself.
% FOUNDING_PROBLEM: After the Roman destruction of the Second Temple (70 CE), the institutional site and authorized personnel for sacrifice offerings ceased to exist. The religious obligation to engage with sacrifice law persisted in textual sources, but its performance became impossible. The community needed to maintain both the obligation's binding force and the possibility of its continuance without Temple or priesthood.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (Menachot 110a and parallels) document the explicit halakhic reasoning that study of the laws of sacrifice counts as if the sacrifice were performed. Medieval and modern halakhic authorities (Maimonides, Shulchan Aruch, contemporary responsa) consistently affirm and refine this interpretation. The reading is attested by the tradition's own internal witnesses across centuries, not by external observers, because the founding problem is internal to the tradition's self-understanding.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at endpoint) because the reading grants normative status to study itself, which is accessible to anyone with textual literacy—no scarce resource is extracted. The temporal series shows slight decline over the interval: as the reading became more institutionalized and widely affirmed across centuries, the need for active enforcement decreased, making extraction marginally less extractive. Suppression is correspondingly low (0.12) because the reading is continuously reaffirmed within the tradition's own authority structure—it does not persist by coercion of dissenters but by ongoing interpretive consensus. Theater ratio is minimal (0.08) because the performance of study is functionally continuous with the coordination problem it solves: reading and interpreting sacrifice texts genuinely engages the community and maintains the textual tradition, not merely theatrically. The measurement series track one shared time grid (t=0 at Temple destruction, t=2000 at present, representing roughly 2000 years of rabbinic tradition). The shallow negative slope across all three metrics reflects the reading's increasing legitimacy and stability within the tradition—less need for performative assertion as the interpretive consensus solidified.
 *
 * PERSPECTIVAL GAP:
 *   From the interpretive community's seat, this is genuine coordination: the reading binds them together in a shared obligation while solving the post-Temple problem. From the performance-only seat (excluded), this is a watering-down of the obligation—study is preparation at best, not fulfillment. From the messianic-suspension seat, this is premature closure—the obligation persists unresolved, not fulfilled. From the archival-preservation seat, this is residual normativity imposed on historical texts. The engine computes these divergent readings from the structural data: the interpretive community benefits (d low), the textual tradition is kept alive (beneficiary non-agent), and excluded seats have incompatible foundational premises. The study-as-performance reading is NOT universally held; its persistence depends on the halakhic authority's continued affirmation.
 *
 * DIRECTIONALITY LOGIC:
 *   The interpretive community is a beneficiary (d near 0.0-0.2): their study is legitimated as obligation-fulfillment rather than optional engagement or mere scholarship. They bear no extraction cost—study is freely undertaken. The textual tradition is also a beneficiary (non-agent): it remains normatively binding and continuously engaged rather than becoming historical artifact. Halakhic authority is the agenda-setter: they adjudicate what counts as obligation-fulfillment and maintain the reading through interpretation. They bear low cost and gain institutional authority. The performance-only and messianic-suspension holders are excluded, not payers: their incompatible premises mean they cannot be integrated into the coordination without abandoning their core claims. This is structural exclusion, not economic extraction. The absence of a victim set is diagnostic: the reading solves the coordination problem without requiring sacrifice from any party, which is why extractiveness is so low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Temple coordination) is LIVE but has TRANSFORMED: the original crisis (how to maintain obligation without Temple) has been solved by the reading, but new crises have emerged (how to maintain the reading against modern secular contexts, how to integrate archival-preservation scholarship). The constraint does not exhibit mandatrophy in the classic sense because the coordination function remains real and continuously affirmed. However, there is a secondary question: whether the reading's normative force is weakening as secular Jewish identity becomes dominant and fewer practitioners affirm the obligation to study sacrifice law as binding. This is tracked in the resistance metric (0.35): the reading meets resistance from those who reject the normative frame, but this resistance is low because those parties are structurally excluded rather than actively opposing. The theater ratio's low value indicates minimal performative maintenance—the reading is reinforced through continuous real interpretation, not through theatrical assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_force_of_study,
    'Is the reading that study-as-performance fulfills the obligation grounded in textual necessity (the texts genuinely entail this interpretation) or in pragmatic adaptation (the community adopted this reading to solve the post-Temple crisis)?',
    'Comparative analysis of pre-Temple and post-Temple textual sources to assess whether the texts themselves support equivalence of study and sacrifice, or whether the equivalence was imposed by interpretive necessity after 70 CE.',
    'If textual grounding is strong, the reading is constitutive of the tradition''s meaning; if pragmatic, the reading is contingent and could be revised if Temple restoration became possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_force_of_study, empirical, 'Whether study-as-performance fulfillment is textually grounded or pragmatically adapted.').

omega_variable(
    reading_coexistence_vs_logical_conflict,
    'Can the study-as-performance reading and the performance-only reading coexist within a single halakhic framework, or does accepting one require logically foreclosing the other?',
    'Analysis of whether halakhic sources permit simultaneous affirmation (study is fulfillment; physical performance would be more complete; both are binding in different senses) or whether the readings are strictly incompatible (study is fulfillment; therefore physical performance is not required; therefore performance-only is false).',
    'If coexistable, both readings remain live within the tradition; if foreclosing, the study-as-performance reading''s dominance depends on institutional authority silencing the performance-only reading. This determines whether the constraint is genuinely coordinating or suppressing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_logical_conflict, conceptual, 'Whether sibling readings are logically foreclosed or merely competing.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the low suppression metric (0.12) because the reading is genuinely accepted by participants, or because dissenting views have been internalized as marginal and no longer voiced?',
    'Ethnographic or historical documentation of whether performance-only or messianic-suspension adherents exist and speak, or whether they have been silenced by institutional pressure or self-censorship.',
    'If suppression is internalized (dissenters have adopted the consensus position), the constraint is more stable than the low metric suggests. If dissenters exist but are silent, the suppression is structural despite appearing consensual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether low suppression reflects genuine consensus or internalized dissent.').

omega_variable(
    accessibility_and_barrier_collapse,
    'How accessible is study of sacrifice law to dispersed diaspora community members without formal training? Does the constraint''s coordination actually depend on wide accessibility, or does it depend on a specialized interpretive class?',
    'Historical documentation of literacy rates, availability of texts, and participation in sacrifice-law study across different Jewish communities and time periods.',
    'If study is widely accessible, extractiveness stays low and the constraint is genuinely coordinating. If study requires specialized training or text access, extractiveness is higher (only trained specialists fulfill the obligation) and the constraint may concentrate authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_and_barrier_collapse, empirical, 'Whether obligation-fulfillment through study is genuinely accessible or requires specialized resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 250, 0.12).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1500, 0.07).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 2000, 0.08).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 250, 0.2).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 500, 0.19).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 2000, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(sacr_su_t250, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 250, 0.15).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 500, 0.13).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The sacrifice-obligation kernel admits four structurally distinct readings, each with different ε values and beneficiary/victim structures. This file instantiates the study-as-performance reading (low extraction, interpretive community as beneficiary). The performance-only reading (preparation framing) has moderate extraction (only Temple-restoration believers benefit). The messianic-suspension reading (obligation suspended) has near-zero extraction (no fulfillment claims). The archival-preservation reading (historical study) has near-zero extraction (no normative obligation). All four readings compete for authority within the same tradition; the persistence of any one depends on halakhic authority's continued affirmation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__study_as_performance, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
