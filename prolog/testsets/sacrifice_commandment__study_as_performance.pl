% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of Divine Commandment
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested halakhic kernel:
 *   the sacrifice_commandment. The reading is study_as_performance — the
 *   doctrine that intellectual study of sacrifice law IS the exercise of the
 *   commandment; engagement with the legal and theological texts fulfills the
 *   divine obligation that once required physical sacrifice. This reading
 *   emerged after the Second Temple's destruction (70 CE) and now functions
 *   as a core principle in rabbinic Judaism, validating continuous Torah
 *   study as a primary form of worship. The kernel contest includes two
 *   sibling readings: performance_only (physical sacrifice is required and
 *   study cannot substitute), and archive_maintenance (study preserves
 *   technical knowledge for future restoration but does not itself fulfill
 *   the obligation). This story models only the study_as_performance reading
 *   as a structurally clean constraint: zero extractiveness (the scholar
 *   genuinely fulfills the obligation through study); no victim set (all who
 *   engage benefit); a beneficiary set consisting of those who study. The
 *   claim and metrics are independent authored facts — the reading is CLAIMED
 *   as a mountain (natural to halakhic logic), and the metrics show zero
 *   extraction, zero suppression, and near-complete accessibility collapse
 *   (once the principle is understood, no alternatives remain; resistance is
 *   negligible because the principle is normatively binding on scholars).
 *
 * KEY AGENTS:
 *   - torah_scholar: practitioner engaged in the study and interpretation of sacrifice law; fulfills the commandment through intellectual engagement
 *   - halakhic_authority: interpretive community that establishes and maintains the doctrine that study equals performance
 *   - lay_community: members whose relationship to the obligation is mediated through the scholars' interpretation
 *   - analytical_observer: studies the halakhic structure and its institutional implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of Divine Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '55f290ca-5d51-4428-bf18-2ab87b32844b').
narrative_ontology:cs_kernel_codification('55f290ca-5d51-4428-bf18-2ab87b32844b', fixed_text).
narrative_ontology:cs_authority_grounding('55f290ca-5d51-4428-bf18-2ab87b32844b', lineage).
narrative_ontology:cs_interpretation_layer_present('55f290ca-5d51-4428-bf18-2ab87b32844b').
narrative_ontology:cs_reading_relation('55f290ca-5d51-4428-bf18-2ab87b32844b', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('55f290ca-5d51-4428-bf18-2ab87b32844b', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('55f290ca-5d51-4428-bf18-2ab87b32844b', foundational, study_fulfills_commandment).
narrative_ontology:cs_axiom_status(study_fulfills_commandment, holdable).
narrative_ontology:cs_axiom_grounding('55f290ca-5d51-4428-bf18-2ab87b32844b', study_fulfills_commandment, deontological).
narrative_ontology:cs_axiom('55f290ca-5d51-4428-bf18-2ab87b32844b', foundational, intellectual_engagement_is_sacred_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_sacred_performance, holdable).
narrative_ontology:cs_axiom_grounding('55f290ca-5d51-4428-bf18-2ab87b32844b', intellectual_engagement_is_sacred_performance, deontological).
narrative_ontology:cs_reference_frame('55f290ca-5d51-4428-bf18-2ab87b32844b', sacrificial_worship_through_study).
narrative_ontology:cs_drift_state('55f290ca-5d51-4428-bf18-2ab87b32844b', contemporary_diaspora_judaism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55f290ca-5d51-4428-bf18-2ab87b32844b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, torah_scholar).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, lay_community).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, intellectual_engagement_is_worship).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, study_fulfills_commandment_without_temple).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in intensive study of sacrifice law and related halakhic texts. The study itself is treated as fulfillment of the biblical commandment. The scholar benefits directly — their intellectual and spiritual engagement satisfies the divine obligation. Exit from the obligation is not available to a committed member of the halakhic tradition, but the scholar can choose the depth and form of engagement. The study is treated as intrinsically valuable, not instrumental to another goal.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, torah_scholar, beneficiary,
    moderate, biographical, arbitrage, universal).

% The interpretive community (rabbinical tradition, yeshiva leadership, textual authorities) that established and maintains the doctrine that study-as-performance fulfills the sacrifice commandment. They set the framework within which the obligation is understood and interpreted. They do not enforce the obligation against resisters — the obligation is normatively binding on those who accept the halakhic system. The authority is exercised through textual interpretation, precedent, and communal norm-setting, not through external coercion.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Members of the Jewish community for whom the sacrifice commandment would be binding but who do not engage in intensive halakhic study. They benefit from the scholars' interpretation and the legitimacy of the doctrine (it validates their religious life without requiring Temple sacrifice). However, they are structurally excluded from the fulfillment mechanism itself — they do not study at the level that constitutes performance of the obligation. Their relationship to the constraint is mediated; they depend on scholars for interpretation and validation.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, lay_community, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, lay_community, excluded).

% Studies the halakhic structure from outside the commitment system. Analyzes how the kernel (sacrifice commandment) is read differently across the three readings and what institutional and logical implications each reading carries. Neither fulfills nor bears the obligation; observes the system's structural properties.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous communal practice of sacred intellectual engagement with textual tradition in place of physical sacrifice. The constraint enables the fulfillment of the divine commandment within the conditions of diaspora (no Temple, no sacrifice possible) by treating study as the form that fulfillment takes. This allows the community to remain bound to an ancient obligation while adapting to new material conditions.
% TRANSFER_FUNCTION: No transfer occurs in this reading. The study flows from the scholar to the scholar's own fulfillment of the obligation and to the community's received tradition. There is no value extraction from one party to another; the constraint structures the form of fulfillment, not a distribution of gains.
% ABSENT_VOICES: Critics who hold the performance_only reading (those who argue study cannot substitute for sacrifice and the commandment is suspended) are not in this story's conversation; they hold a different reading of the same kernel. Within this reading's authority structure, dissenters are those who deny that study has intrinsic sacred value — but such voices are structurally outside the halakhic framework this constraint operates within.
% DISAPPEARANCE_RATIONALE: If the doctrine that study-as-performance fulfills the sacrifice commandment disappeared, the halakhic system would reclassify the obligation as suspended (the performance_only reading) or as archive maintenance only. The communal religious life would not cease, but the normative status of study as fulfillment would shift. Whether the world would genuinely rearrange depends on whether the principle is intrinsic to halakhic logic (it would have to reappear) or constructed for institutional benefit (it could be abandoned). This is the natural-law-vs-constructed omega question.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the Jewish community faced a theological problem: the Torah contains commandments regarding sacrifice and the Temple service that can no longer be physically performed. How can the community remain faithful to these commandments without the Temple? The solution developed over centuries of interpretation: the study of sacrifice law, the detailed intellectual engagement with the texts describing sacrificial practice, IS the fulfillment of the commandment. The study preserves and perpetuates the knowledge, honors the original practice, and constitutes a form of worship that requires no Temple.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources (Talmud, Midrash, medieval halakhic codes) attest that study serves as a replacement for sacrifice and is valued as such. However, the status of whether the founding problem is 'live' or 'dead' is contested: some sources and contemporary scholars argue that the principle remains essential (live), while others argue that if the Temple were restored, the original commandment to perform sacrifice would supersede study (dead). The principle_only reading holds the problem is dead; the archive_maintenance reading holds the problem is contested. No corroboration from outside the benefiting parties (scholars) exists; secular historians and non-halakhic observers do not attest to the existence or solution of a 'divine obligation problem.'
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_commandment__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because study-as-performance is modeled as intrinsically valuable — no transfer of value occurs; the scholar's engagement benefits the scholar (fulfilling the obligation) and potentially the community (through transmitted knowledge), but no extraction from one party to another is present. Suppression is zero because the principle is presented as internally consistent and logically binding — once accepted within the halakhic framework, it does not require external coercion. Theater ratio is zero because the intellectual engagement is the substance of the practice, not a proxy for it. Accessibility collapse is near-complete (0.95) because once the principle is established in halakhic tradition, alternative performances of the obligation (for those who study) effectively vanish — the obligation IS study within this reading. Resistance is near-zero (0.05) because the principle is normatively binding within its authority structure; it generates virtually no organized resistance from those who accept the halakhic framework. The temporal measurements are flat across the interval because this reading models a structural principle whose force does not decay or intensify over time — it is either operative or not, and when operative, its metrics remain constant.
 *
 * DIRECTIONALITY LOGIC:
 *   The sole beneficiary is the torah_scholar — the agent who engages in study and thereby fulfills the obligation. There are no victims in this reading because the obligation is fulfilled through the scholar's own action (study), not through extraction from another party. The halakhic_authority (the interpretive community) is the agenda-setter, establishing and maintaining the doctrine that study counts as performance. The lay_community's relationship to the constraint is mediated — they benefit from the scholars' transmission of tradition and interpretation, but they are not direct targets of extraction. Within this reading, no asymmetric transfer occurs; the constraint is modeled as a natural structural principle of halakhic logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The study_as_performance reading faces a potential mandatrophy question: Was the doctrine developed to solve the founding problem (how to maintain the sacrifice commandment after the Temple's destruction), or has it outlived that problem through institutional and textual evolution? The FSM check flags this constraint as a candidate: it declares a beneficiary (torah_scholar) on a mountain claim (emerges_naturally). The natural-law-vs-constructed omega directly addresses this: is the principle intrinsic to halakhic logic, or is it a compensatory doctrine that benefits scholars by making study obligatory? The mandatrophy would be resolved if the tradition itself acknowledged that the principle was innovative and no longer necessary (e.g., if mainstream halakhic sources stated that Temple reconstruction would invalidate study-as-performance). Currently, the principle shows no mandatrophy signature because it remains functionally central to the religious life it governs. The measurement series are flat, indicating no temporal drift in the constraint's force — no theater creep, no extraction accumulation — which is consistent with a genuine mountain (principle) rather than an atrophied one (piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is this constraint a structural property of halakhic logic itself (natural law to the halakhic system), or a constructed reading that benefits scholars by treating cognitive engagement as fulfilling obligations that originally required physical action?',
    'Historical textual analysis: does the identification of study with sacrifice predate the Temple''s destruction, or emerge as a compensatory doctrine after 70 CE? Does the halakhic tradition''s own authorities claim the principle is logical necessity or innovative interpretation?',
    'If natural to halakhic logic: mountain classification is justified; beneficiary declaration is FSM-level signal of how beneficiaries cluster around natural-law claims. If constructed post-hoc: the constraint may be tangled_rope (scholars benefit from the doctrine; laypeople bear the cost of intensified study obligation that substitutes for a simpler historical practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether study-as-performance is intrinsic to halakhic structure or a doctrine that benefits a specific stakeholder group.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one of three competing readings of the sacrifice_commandment kernel. How does this reading''s core premise (study fulfills the obligation) relate logically to the sibling readings (performance_only: physical sacrifice alone counts; archive_maintenance: study preserves knowledge but does not fulfill)?',
    'Formal analysis of the logical structure: Does study-as-performance logically foreclose performance_only (i.e., if study counts as fulfillment, can physical sacrifice simultaneously be required)? Or do the readings coexist as different parties'' live positions? What structural conditions enable or suppress each reading?',
    'If study-as-performance forecloses performance_only within a single halakhic framework, the relationship is foreclosure (rare). If both remain live options held by different Orthodox communities and their authorities, the relation is coexistence. The relation type feeds the cs_structure.reading_relations array and the engine''s commitment-system drift analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The logical and institutional relationship between this reading and its siblings in the sacrifice_commandment kernel contest.').

omega_variable(
    suppression_internalization_mechanism,
    'The measured suppression is zero (this reading treats study as intrinsically fulfilling). Is this an accurate description of how the obligation is experienced by Torah scholars, or does the reading obscure a suppression of lay dissent—i.e., non-scholars who cannot engage in advanced study are positioned as failing the obligation unless they defer to scholars?',
    'Ethnographic and textual evidence: Do halakhic sources acknowledge legitimate alternative performances of the sacrifice obligation for those without study capacity? Or does the scholar-centered reading suppress non-intellectual paths to fulfillment?',
    'If suppression is genuinely zero: mountain classification holds. If suppression is present (non-scholars are trapped in dependency on scholars'' interpretation or excluded from fulfilling the obligation themselves), the constraint reclassifies toward tangled_rope or snare depending on whether coordination (collective study as communal worship) or pure extraction (scholars monopolize the fulfillment function) dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether study-as-performance suppresses non-intellectual paths to fulfilling the sacrifice obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__study_as_performance, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__study_as_performance, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__study_as_performance, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__study_as_performance, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__study_as_performance, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.0).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints, each instantiating a different reading of what counts as fulfilling the biblical sacrifice commandment after the Second Temple's destruction. study_as_performance (this story) models the intellectual-engagement reading: zero extractiveness, beneficiary is the scholar. performance_only models the physical-performance reading: would show higher resistance and a contested distinction between obligation and suspension. archive_maintenance models the knowledge-preservation reading: would show lower stakes for fulfillment and a future-oriented time horizon. All three readings share the kernel (the biblical commandment) but diverge in their ε values, beneficiary structures, and temporal horizons. The network links these as a family so that corpus analysis can track how a single kernel generates competing constraint types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
