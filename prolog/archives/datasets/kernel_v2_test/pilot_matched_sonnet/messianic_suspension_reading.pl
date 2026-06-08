% ============================================================================
% CONSTRAINT STORY: messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_messianic_suspension_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: messianic_suspension_reading
 *   human_readable: Messianic Suspension Reading of Sacrifice Obligation
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The messianic suspension reading of sacrifice obligation holds that the
 *   commandments governing Temple sacrifice are divinely suspended (not
 *   abolished, not transformed) until messianic restoration, and that study
 *   of sacrifice law during the suspension period maintains operational
 *   readiness for that restoration. This reading is one of four structurally
 *   distinct interpretations of the same kernel (the post-Temple status of
 *   sacrifice obligation). The suspension is explicit and constitutive: the
 *   obligation is in abeyance, not violated. Study is instrumental
 *   (preserving technical capacity) rather than substitutive (replacing the
 *   obligation). The sunset clause (messianic restoration) is a genuine
 *   structural feature of the reading, not a symbolic placeholder. The
 *   constraint exhibits low extraction because no victim set exists during
 *   the suspension — the obligation is suspended, so non-performance is not
 *   violation. The beneficiaries are future generations (who inherit
 *   operational knowledge at restoration) and the contemporary study
 *   community (who benefit from engagement with a living intellectual
 *   tradition). The theater ratio is low because study under this reading is
 *   functional (capacity maintenance) rather than performative (ritual
 *   substitution). The constraint has drifted modestly over the interval: as
 *   the suspension period lengthened, some study practices became more
 *   symbolic and less operationally focused, raising both theater_ratio and
 *   extractiveness slightly. But the core structure remains scaffold:
 *   temporary suspension with explicit sunset and functional transitional
 *   purpose.
 *
 * KEY AGENTS:
 *   - Halakhic Authority Structure: Institutional steward (institutional/constrained) — maintains the suspension framework and the study tradition; sees itself as preserving rather than extracting
 *   - Contemporary Study Practitioner: Voluntary participant (moderate/mobile) — engages with sacrifice law study as intellectual and spiritual practice; net beneficiary of the tradition
 *   - Identity-Committed Layperson: Identity-locked participant (powerless/identity_locked) — Jewish identity constituted through halakhic commitment; experiences suspension as relief rather than burden, but cannot exit the identity frame
 *   - Rabbinic Study Institutions: Organized preservers (organized/constrained) — yeshivot and study communities that maintain technical mastery; net beneficiaries of the knowledge base
 *   - Future Generations at Restoration: Deferred beneficiaries (powerless/trapped in time) — inherit operational capacity if restoration occurs; cannot exit the temporal constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(messianic_suspension_reading, 0.18).
domain_priors:suppression_score(messianic_suspension_reading, 0.25).
domain_priors:theater_ratio(messianic_suspension_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(messianic_suspension_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(messianic_suspension_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(messianic_suspension_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(messianic_suspension_reading, "Messianic Suspension Reading of Sacrifice Obligation").
narrative_ontology:topic_domain(messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system").

narrative_ontology:has_sunset_clause(messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(messianic_suspension_reading, '0c2ba15a-3e11-4016-bd7f-a404198896e5').
narrative_ontology:cs_kernel_codification('0c2ba15a-3e11-4016-bd7f-a404198896e5', fixed_text).
narrative_ontology:cs_authority_grounding('0c2ba15a-3e11-4016-bd7f-a404198896e5', lineage).
narrative_ontology:cs_interpretation_layer_present('0c2ba15a-3e11-4016-bd7f-a404198896e5').
narrative_ontology:cs_reading_relation('0c2ba15a-3e11-4016-bd7f-a404198896e5', messianic_suspension_reading__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c2ba15a-3e11-4016-bd7f-a404198896e5', messianic_suspension_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c2ba15a-3e11-4016-bd7f-a404198896e5', messianic_suspension_reading__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('0c2ba15a-3e11-4016-bd7f-a404198896e5', foundational, divine_suspension_until_restoration).
narrative_ontology:cs_axiom_status(divine_suspension_until_restoration, holdable).
narrative_ontology:cs_axiom_grounding('0c2ba15a-3e11-4016-bd7f-a404198896e5', divine_suspension_until_restoration, theological).
narrative_ontology:cs_axiom('0c2ba15a-3e11-4016-bd7f-a404198896e5', secondary, study_as_operational_maintenance).
narrative_ontology:cs_axiom_status(study_as_operational_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('0c2ba15a-3e11-4016-bd7f-a404198896e5', study_as_operational_maintenance, instrumental).
narrative_ontology:cs_reference_frame('0c2ba15a-3e11-4016-bd7f-a404198896e5', sinaitic_transmission_continuity).
narrative_ontology:cs_drift_state('0c2ba15a-3e11-4016-bd7f-a404198896e5', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c2ba15a-3e11-4016-bd7f-a404198896e5', '').
narrative_ontology:cs_kernel_id(messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, future_generations_at_restoration).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, contemporary_study_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, contemporary_study_practitioner).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, identity_committed_layperson).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, rabbinic_study_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic authority structure maintains the suspension framework and the study tradition. It sets the agenda for what constitutes adequate study (operational readiness) and adjudicates disputes about the suspension's scope. Constrained exit because institutional identity is bound to the tradition. The authority structure sees itself as steward rather than extractor: it preserves the framework for future restoration rather than collecting rents from it.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, constrained, global).

% Individual scholars who engage with sacrifice law study as intellectual and spiritual practice. They benefit from access to a living intellectual tradition and from the community of study. Exit is available (study is voluntary), and the constraint imposes no burden — the suspension relieves the obligation, and study is chosen engagement.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, contemporary_study_practitioner, beneficiary,
    moderate, biographical, mobile, regional).

% A layperson whose Jewish identity is constituted through halakhic commitment. The suspension relieves the burden of an unperformable obligation, and the study tradition maintains connection to the deferred practice. Identity-locked because exit would require abandoning the identity frame, but the constraint itself is not extractive — the suspension is experienced as relief, not imposition.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, identity_committed_layperson, beneficiary,
    powerless, generational, identity_locked, national).

% Yeshivot and study institutions that maintain technical mastery of sacrifice law. They benefit from the preserved knowledge base and the study community it sustains. Constrained exit because institutional identity is bound to the tradition, but they are net beneficiaries — the study tradition is their core function and source of legitimacy.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, rabbinic_study_institutions, beneficiary,
    organized, generational, constrained, global).

% Future generations who would inherit operational knowledge if messianic restoration occurs. They are trapped in time (cannot exit the temporal constraint) and powerless (cannot influence whether restoration occurs or whether the study tradition is maintained). They are beneficiaries if the sunset clause is genuine (they inherit capacity) but are entirely dependent on the present generation's preservation efforts.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, future_generations_at_restoration, beneficiary,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving technical mastery of sacrifice law during the suspension period so that operational capacity is available if messianic restoration occurs. The coordination problem is maintaining specialized knowledge across a potentially indefinite interval when the knowledge cannot be practiced.
% TRANSFER_FUNCTION: Study time and institutional resources flow from the contemporary community to the preservation of sacrifice law knowledge. The transfer is voluntary (study is chosen) and the beneficiaries are both present (the study community gains from engagement) and future (future generations inherit capacity if restoration occurs).
% ABSENT_VOICES: Those outside the halakhic tradition who view the suspension as permanent transformation or symbolic archive rather than operational maintenance. Also absent: those within the tradition who see the study requirement as burdensome rather than voluntary, if such a group exists. The unanimity around low extraction may reflect that dissenting voices (those who experience study as obligatory burden) are not in the conversation.
% DISAPPEARANCE_RATIONALE: If the suspension framework disappeared, the halakhic tradition would need to reclassify the status of sacrifice obligation: either as permanently abolished (requiring theological justification), as immediately obligatory (requiring practical operationalization without the Temple), or as transformed into a different practice (requiring new interpretive framework). The study institutions would lose their core function (preserving operational readiness), and the identity-committed layperson would face a different relationship to the unperformable obligation. The world rearranges because the suspension framework organizes how the tradition relates to the deferred practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE made sacrifice physically impossible. The founding problem was: how does a tradition whose central ritual practice is now unperformable maintain continuity with that practice without either abandoning it (theological crisis) or pretending it can still be performed (practical impossibility)? The suspension reading solves this by treating the obligation as divinely deferred rather than abolished, and study as operational maintenance rather than substitution.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Temple destruction making sacrifice impossible) is historically uncontested and corroborated by all parties. The STATUS of the problem is contested: this reading holds that the problem is still live (the obligation is suspended, not resolved), while sibling readings hold that the problem has been resolved through transformation (study_as_exercise), permanent change (performance_only), or archival preservation (symbolic_archive). Corroboration for the 'still live' status comes from within the messianic suspension tradition itself (rabbinic authorities who maintain the framework), but external corroboration is absent — those outside the reading do not treat restoration as a live structural possibility.
narrative_ontology:disappearance_verdict(messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(messianic_suspension_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HALAKHIC AUTHORITY (SCAFFOLD) — The institutional reading sees the suspension as temporary coordination: obligation is in abeyance, not abolished. Study maintains operational readiness for restoration. The sunset clause is explicit (messianic restoration), and the constraint's function is transitional maintenance of capacity. Low extraction because the arrangement preserves rather than extracts, and the authority structure sees itself as steward rather than beneficiary.
constraint_indexing:constraint_classification(messianic_suspension_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: STUDY PRACTITIONER (ROPE) — Individual scholars experience the constraint as pure coordination: study of sacrifice laws is voluntary engagement with a preserved tradition. No extraction (they choose to participate), no suppression (exit is available), and the coordination function is genuine (collective preservation of technical knowledge). The practitioner benefits from access to a living intellectual tradition.
constraint_indexing:constraint_classification(messianic_suspension_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: IDENTITY-LOCKED LAYPERSON (SCAFFOLD) — A layperson whose Jewish identity is constituted through halakhic commitment experiences the suspension as scaffold: the obligation is real but deferred, and study maintains the connection during the interval. Identity-locked because exit would require abandoning the identity frame, but the constraint itself is not extractive — the suspension relieves rather than imposes burden. The sunset (messianic restoration) is part of the identity commitment.
constraint_indexing:constraint_classification(messianic_suspension_reading, scaffold,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: RABBINIC INSTITUTIONS (ROPE) — Yeshivot and study institutions see the constraint as coordination: preserving technical mastery of sacrifice law is a collective good that benefits the tradition. Constrained exit (institutional identity is bound to the tradition) but low extraction — the institutions are net beneficiaries of the preserved knowledge base and the study community it sustains.
constraint_indexing:constraint_classification(messianic_suspension_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SCAFFOLD) — From the analytical position, this reading instantiates a genuine scaffold: the obligation is suspended (not transformed or abolished), study maintains operational capacity, and the sunset clause (messianic restoration) is explicit and constitutive of the reading. The constraint's function is transitional preservation. Low extraction because no victim set exists during the suspension period — the obligation is in abeyance, not violated.
constraint_indexing:constraint_classification(messianic_suspension_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(messianic_suspension_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(messianic_suspension_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(messianic_suspension_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The suspension relieves obligation rather than imposing it, and study is voluntary engagement with a preserved tradition. The modest extraction reflects the opportunity cost of study time and the institutional resources devoted to maintaining the tradition, but there is no victim set during the suspension period — the obligation is in abeyance, not violated. The value has drifted upward slightly (from 0.12 to 0.18) as some study practices became more institutionally embedded and less purely instrumental. Suppression (0.25): Low. Exit options are available for individuals (study is voluntary), though institutional actors face identity constraints. The suppression reflects the social and identity costs of abandoning the tradition, not coercive enforcement. Theater ratio (0.15): Low. Study under this reading is functional (maintaining operational capacity) rather than performative (ritual substitution). The value has drifted upward modestly (from 0.10 to 0.18) as the suspension period lengthened and some study practices became more symbolic, but the core function remains instrumental. The low theater ratio distinguishes this reading from study_as_exercise_reading, where study is substitutive performance (high theater ratio).
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify as either scaffold or rope, with no snare or tangled_rope perspectives. This uniformity reflects the structural fact that the suspension relieves obligation rather than imposing it. The halakhic authority sees scaffold (temporary suspension with sunset). The study practitioner sees rope (voluntary coordination). The identity-locked layperson sees scaffold (deferred obligation that relieves present burden). The rabbinic institutions see rope (collective preservation as coordination). The analytical observer sees scaffold (genuine transitional structure with explicit sunset). The gap between scaffold and rope perspectives reflects the difference between seeing the sunset clause as constitutive (scaffold) versus seeing the present study practice as stable coordination (rope). The absence of snare or tangled_rope perspectives distinguishes this reading from its siblings: if study_as_exercise_reading treats study as obligatory substitution, it would produce higher extraction and a victim set (those who cannot study). If performance_only_reading treats the obligation as permanently transformed, it would produce a different beneficiary structure (those who benefit from the transformation's finality).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure is unusual: the primary beneficiaries are future generations at restoration (who inherit operational knowledge) and the contemporary study community (who benefit from engagement with the tradition). No victim set exists during the suspension period because the obligation is in abeyance — non-performance is not violation. The halakhic authority structure is a steward rather than an extractor: it maintains the framework but does not collect rents from it. Individual practitioners are net beneficiaries (they choose to engage and gain from the tradition). The identity-locked layperson experiences the constraint as scaffold rather than snare because the suspension relieves rather than imposes burden — the identity lock binds them to the tradition, but the tradition's suspension structure is not extractive. The directionality derivation produces low d values for all agents (all are beneficiaries or neutral), which yields low or negative chi (experienced extraction). The modest base extractiveness (0.18) reflects institutional overhead and opportunity costs, not asymmetric extraction from a victim set.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that scaffold classification is compatible with civilizational time horizons when the sunset clause is explicit and constitutive. The messianic restoration is not a symbolic placeholder but a structural feature of the reading: the obligation is suspended UNTIL restoration, not transformed or abolished. The study practice is instrumental (maintaining capacity for restoration) rather than substitutive (replacing the obligation). The low extraction and absence of a victim set during the suspension period confirm that this is not a snare disguised as coordination. The modest drift in theater_ratio (0.10 to 0.18) and extractiveness (0.12 to 0.18) over 1950 years reflects that some study practices became more symbolic as the suspension lengthened, but the core structure remains functional. The constraint is a genuine scaffold: temporary suspension with explicit sunset and transitional purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the sacrifice_obligation_kernel, or is the suspension itself a natural structural feature of post-Temple Judaism?',
    'Cross-reading analysis: if sibling readings (study_as_exercise, performance_only, symbolic_archive) produce structurally different victim sets or extractiveness profiles, the kernel is contested. If all readings converge on the same structural facts, the suspension is not a reading but a shared premise.',
    'If contested kernel: this constraint is one reading among several, and the committer axis is active. If shared premise: the suspension is not a reading but a structural fact, and the kernel framing is incorrect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the suspension is a contested reading or a shared structural premise').

omega_variable(
    operational_readiness_threshold,
    'What level of study detail constitutes ''operational readiness'' versus symbolic preservation?',
    'Comparison with sibling readings: if study_as_exercise_reading treats study as substitutive performance (high theater_ratio), and this reading treats study as instrumental capacity-maintenance (low theater_ratio), the threshold distinguishes them. Empirical test: could the study community actually operationalize sacrifice protocols if the Temple were rebuilt tomorrow?',
    'If threshold is high (detailed technical mastery required): this reading''s low theater_ratio is justified. If threshold is low (symbolic familiarity sufficient): this reading collapses toward symbolic_archive_reading, and the operational readiness claim is cover for a different function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_readiness_threshold, empirical, 'What study depth constitutes operational readiness for restoration').

omega_variable(
    sunset_clause_operationalization,
    'Is the messianic restoration sunset clause a genuine structural feature (the obligation resumes when conditions change) or a theological placeholder (the sunset is infinitely deferred)?',
    'Historical analysis: do other halakhic suspensions with messianic sunset clauses ever resume? Comparison with sibling readings: if performance_only_reading treats the obligation as permanently transformed (no sunset), the difference is structural. Theological test: does the reading''s own framework treat restoration as a real future event or as an eschatological symbol?',
    'If genuine sunset: scaffold classification is correct. If infinitely deferred: the sunset clause is theatrical, and the constraint is closer to piton (a suspended obligation maintained as performance) or rope (study is the new steady state, not a transitional arrangement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_operationalization, conceptual, 'Whether the messianic sunset is structurally operative or symbolically deferred').

omega_variable(
    framing_underdetermination_lineage_vs_practice,
    'Is the authority grounding for this reading best understood as lineage (chain of transmission from Sinai through rabbinic tradition) or practice (the study community''s ongoing engagement constitutes the authority)?',
    'Comparison with sibling readings: if study_as_exercise_reading grounds authority in practice (study IS the obligation), and this reading grounds authority in lineage (study maintains the transmitted obligation), the framings produce different cs_pattern classifications. Signal: does the reading cite textual transmission (lineage) or communal consensus (practice) as the source of the suspension''s legitimacy?',
    'If lineage: cs_structure.authority_grounding = lineage, and the reading''s legitimacy depends on continuity with the received tradition. If practice: cs_structure.authority_grounding = practice, and the reading''s legitimacy depends on the study community''s ongoing ratification. The choice affects whether drift (changing study practices) threatens the reading''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_lineage_vs_practice, conceptual, 'Whether authority is grounded in transmitted lineage or constituted through practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(messianic_suspension_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mssr_theater_initial, messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mssr_theater_medieval, messianic_suspension_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(mssr_theater_modern, messianic_suspension_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(mssr_theater_contemporary, messianic_suspension_reading, theater_ratio, 1500, 0.18).

% Extraction over time
narrative_ontology:measurement(mssr_extract_initial, messianic_suspension_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(mssr_extract_medieval, messianic_suspension_reading, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(mssr_extract_modern, messianic_suspension_reading, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement(mssr_extract_contemporary, messianic_suspension_reading, base_extractiveness, 1500, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(messianic_suspension_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel. The sibling readings (study_as_exercise, performance_only, symbolic_archive) are separate constraint stories with different extractiveness values, beneficiary structures, and sunset logic. The kernel decomposition follows the epsilon-invariance principle: each reading has a stable epsilon value, and the structural differences between readings are captured by separate stories linked via the kernel_id, not by a single story with measurement-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
