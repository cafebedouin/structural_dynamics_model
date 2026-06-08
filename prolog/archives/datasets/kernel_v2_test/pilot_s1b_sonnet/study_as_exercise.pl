% ============================================================================
% CONSTRAINT STORY: study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_exercise, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: study_as_exercise
 *   human_readable: Study of Sacrifice Law as Exercise of Divine Command
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   The rabbinic doctrine that study of sacrifice law constitutes exercise of
 *   the divine command addresses a structural problem: how does a covenant
 *   community maintain fidelity to a ritual obligation suspended by material
 *   circumstances (Temple destruction, 70 CE)? This constraint is the
 *   'study-as-exercise' READING of the contested temple_sacrifice_commitment
 *   kernel — one interpretive framework among three within the halakhic
 *   tradition. Under this reading, intellectual engagement with the
 *   sacrificial code (Leviticus, Talmudic tractate Zevachim, Maimonidean
 *   codification) is not preparation for future resumption but intrinsically
 *   fulfills the commandment in its current form. The practice coordinates
 *   community devotion around texts rather than altars, preserving the
 *   obligation's claim on practitioners without demanding impossible material
 *   compliance. Sibling readings frame study differently: 'performance_only'
 *   holds that only material sacrifice counts (study is merely preservative),
 *   while 'hybrid_preparatory' treats study as dual-function (preserves
 *   memory AND prepares for messianic restoration). This story generates the
 *   study-as-exercise reading ONLY, per Rule 1.
 *
 * KEY AGENTS:
 *   - Studying Community: Primary beneficiary (organized/mobile) — yeshiva students, chavruta partnerships, lay study groups maintaining covenant relationship through intellectual devotion
 *   - Rabbinic Authority: Institutional beneficiary (institutional/constrained) — transmits study-equivalence doctrine as lineage-grounded coordination; sustains interpretive function
 *   - Individual Practitioner: Moderate beneficiary (moderate/mobile) — engages study as devotional practice; net beneficiary of coordination infrastructure
 *   - Covenant Fidelity: Abstract beneficiary (non-agent) — the community's relationship to divine command is maintained through study in absence of Temple
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees study-as-exercise as coordination mechanism solving suspended-obligation problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_exercise, 0.03).
domain_priors:suppression_score(study_as_exercise, 0.08).
domain_priors:theater_ratio(study_as_exercise, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(study_as_exercise, theater_ratio, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_exercise, rope).
narrative_ontology:human_readable(study_as_exercise, "Study of Sacrifice Law as Exercise of Divine Command").
narrative_ontology:topic_domain(study_as_exercise, "religious_law/halakhic_tradition/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_exercise, '60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84').
narrative_ontology:cs_kernel_codification('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', fixed_text).
narrative_ontology:cs_authority_grounding('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', lineage).
narrative_ontology:cs_interpretation_layer_present('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84').
narrative_ontology:cs_reading_relation('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', study_as_exercise__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', study_as_exercise__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', foundational, study_intrinsically_fulfills_commandment).
narrative_ontology:cs_axiom_status(study_intrinsically_fulfills_commandment, holdable).
narrative_ontology:cs_axiom_grounding('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', study_intrinsically_fulfills_commandment, deontological).
narrative_ontology:cs_axiom('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', secondary, temple_absence_is_operative_context).
narrative_ontology:cs_axiom_status(temple_absence_is_operative_context, holdable).
narrative_ontology:cs_axiom_grounding('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', temple_absence_is_operative_context, conventional).
narrative_ontology:cs_reference_frame('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', sinaitic_transmission_continuity).
narrative_ontology:cs_drift_state('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', post_temple_destruction_stabilization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('60cb5d06-1c72-4d59-ae9b-85e2dcf7bb84', '').
narrative_ontology:cs_kernel_id(study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(study_as_exercise, covenant_fidelity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(study_as_exercise, rabbinic_authority).
narrative_ontology:constraint_beneficiary(study_as_exercise, individual_practitioner).
narrative_ontology:constraint_vindicates(study_as_exercise, intellectual_devotion_doctrine).
narrative_ontology:constraint_vindicates(study_as_exercise, study_equivalence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshiva students, chavruta study partnerships, and lay study groups engage with sacrificial law texts (Leviticus, Zevachim, Maimonidean Hilchot Korbanot) as devotional practice. Study sessions are coordinated through institutional infrastructure (yeshivot, synagogue study programs, online chavruta matching) and interpretive lineages (rabbinic commentaries providing structured pathways through complex legal material). Exit is mobile — practitioners can disengage from study without material penalty, though they lose access to community and interpretive support. Net beneficiaries: the practice maintains covenant relationship and community identity.
narrative_ontology:constraint_stakeholder(study_as_exercise, studying_community, beneficiary,
    organized, biographical, mobile, regional).

% Rabbinic authorities transmit the study-equivalence doctrine through halakhic rulings, yeshiva curricula, and responsa literature. They set the interpretive framework (study of sacrifice law = exercise of divine command) and administer its application (which texts are authoritative, what study methods are valid, how the obligation is fulfilled). Authority grounds itself in lineage (chain of transmission from Sinai) but experiences the constraint as coordination — the doctrine solves the real problem of occupying a suspended ritual obligation. Constrained exit: institutional role carries obligations and rabbinic ordination creates identity lock, but no coercive enforcement prevents departure. Net beneficiary: the practice sustains the authority's interpretive function and institutional continuity.
narrative_ontology:constraint_stakeholder(study_as_exercise, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(study_as_exercise, rabbinic_authority, beneficiary).

% Individual Jews engage study of sacrificial law as personal devotional practice, often in partnership (chavruta) or through synagogue study groups. Access structured texts and commentaries provided by rabbinic tradition. Study ranges from daily Talmud page (daf yomi) including sacrificial tractates to focused engagement with Maimonidean sacrifice code. Exit is fully mobile — can disengage without penalty beyond loss of community participation and devotional structure. Net beneficiary: gains devotional practice, intellectual engagement, and community connection through coordination infrastructure.
narrative_ontology:constraint_stakeholder(study_as_exercise, individual_practitioner, beneficiary,
    moderate, biographical, mobile, local).

% Abstract good: the community's fidelity to divine command is maintained through intellectual engagement when material performance is impossible. This is a non-agent beneficiary (vindicated proposition) — covenant relationship is preserved, but it collects no rents and has no agency. Included for narrative completeness but excluded from extraction metrics per agent=false flag.
narrative_ontology:constraint_stakeholder(study_as_exercise, covenant_fidelity, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(study_as_exercise, covenant_fidelity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Study-as-exercise solves the problem of how a covenant community maintains fidelity to a ritual obligation (sacrifice) that is materially suspended (no Temple). Without this coordination, the obligation either lapses entirely (covenant breakdown) or generates impossible demands (practitioners cannot perform what is commanded). Study substitutes intellectual engagement for material performance, preserving the obligation's claim on practitioners in a form they can actually fulfill.
% TRANSFER_FUNCTION: No material transfer. Attention and intellectual labor flow FROM practitioners TO textual engagement (time spent studying sacrificial law). Community infrastructure (yeshivot, commentaries, study partnerships) flows FROM rabbinic institutions TO practitioners, enabling coordinated engagement. No money, status, or work extracted from any party — all participants are net beneficiaries of the coordination structure.
% ABSENT_VOICES: Karaite Jews and secular/Reform Jewish communities that reject rabbinic authority are structurally excluded from this framework. They would object that the rabbinic reading (study-as-exercise) is not authoritative — Karaites because they reject rabbinic interpretation altogether, secular/Reform Jews because they do not hold sacrifice (or its study) as obligatory. Their absence from the conversation is geographical and ideological, not coercive: rabbinic Judaism is one tradition among several, and alternative frameworks exist and are accessible. The unanimity among rabbinic authorities about study-equivalence partly reflects that dissenting voices are in different traditions, not that dissent was suppressed within the rabbinic lineage.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise doctrine disappeared, observant Jewish communities would face an unresolved structural tension: how to maintain covenant fidelity to a suspended obligation? Study sessions, yeshiva curricula, and commentary traditions organized around sacrificial law would lose their justification. Practitioners might revert to treating sacrifice as purely historical (Reform position) or purely messianic (Temple rebuilding expectation without present obligation). The rabbinic coordination infrastructure depends on this doctrine — world rearranges.
% FOUNDING_PROBLEM: Temple destruction (70 CE) suspended the material possibility of sacrifice while leaving the biblical commandment textually intact. The founding problem: how does a textually-grounded covenant community maintain fidelity to an obligation it cannot perform? The problem is structural, not merely psychological — the Torah commands sacrifice, the covenant depends on obedience to command, but the Temple (the only legitimate sacrifice site) is gone. Without a resolution, the community faces either covenant rupture (abandon the obligation) or paralysis (obligated to the impossible).
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live because the Temple has not been rebuilt and the obligation has not been textually rescinded. Status is corroborated by: (1) continued production of halakhic literature on sacrifice law across all major rabbinic authorities (Ashkenazi, Sephardi, Mizrahi traditions maintain study as active obligation, not historical curiosity); (2) institutional infrastructure — yeshivot allocate curriculum time to Talmudic tractate Zevachim and Maimonidean Hilchot Korbanot as living halakha, not mere archaeology; (3) the problem's liveness is attested by parties OUTSIDE the beneficiary set (academic scholars of Judaism, historians of halakha) who identify Temple absence as ongoing structural tension in rabbinic Judaism. The founding problem would shift to 'dead' only if the Temple were rebuilt (ending material suspension) or if rabbinic consensus emerged that the obligation itself has lapsed (no major tradition holds this).
narrative_ontology:disappearance_verdict(study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(study_as_exercise, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDYING COMMUNITY (ROPE) — Voluntary coordination around intellectual devotion. Study sessions, yeshiva curricula, and rabbinic commentary networks coordinate engagement with sacrifice law in absence of Temple. Net beneficiaries: the practice maintains covenant relationship and community identity. Exit is mobile — practitioners can leave without penalty beyond loss of community participation. Minimal extraction.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — Transmits the study-as-exercise doctrine as coordination mechanism for maintaining halakhic continuity. Authority grounds itself in lineage (chain of transmission from Sinai) but experiences the constraint as coordination: the doctrine solves the real problem of how to occupy a suspended ritual obligation. Constrained exit (institutional role carries obligations) but still net beneficiary — the practice sustains the authority's interpretive function.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL PRACTITIONER (ROPE) — Engages study as devotional practice. Experiences the constraint as coordination: the community provides structured texts, study partnerships (chavruta), and interpretive frameworks that make solitary engagement with complex ritual law meaningful. Mobile exit — can disengage from study without material penalty. Net beneficiary of the coordination infrastructure.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — Study-as-exercise is a coordination mechanism that solves a genuine structural problem: how does a covenant community maintain fidelity to a ritual obligation that cannot be materially performed? The intellectual engagement substitutes for the suspended practice, preserving the obligation's claim on practitioners without demanding impossible material compliance. Negligible extraction, minimal suppression, low theater. Pure coordination.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_exercise_tests).
:- end_tests(study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.03): Near-zero. Study-as-exercise is a voluntary coordination practice with negligible rent collection. The minimal extraction reflects mild in-group/out-group asymmetry (rabbinic authority vs Karaite or secular Jewish positions that reject the framework) but no identifiable victim bearing costs. The practice benefits all participants through coordination infrastructure (texts, study partnerships, interpretive lineages) without extracting from any party. Suppression (0.08): Very low. Exit is mobile for individual practitioners (can disengage without material penalty) and constrained but not blocked for institutional actors (rabbinic authorities are bound by role but not coerced). The minimal suppression reflects that alternative frameworks (Karaite, Reform, secular) exist and are accessible, though the rabbinic lineage presents its reading as authoritative. Theater ratio (0.05): Negligible. Study sessions and halakhic discourse have minimal performative overhead — the intellectual engagement IS the devotional practice, not a simulation of it. Some ritualization exists (structured study times, blessing recitations before study) but the core activity is functional, not theatrical. Measurements show stable low values across the 1000-year interval (rabbinic period through modernity), with slight initial suppression decay as alternative interpretive communities stabilized.
 *
 * PERSPECTIVAL GAP:
 *   NO SIGNIFICANT GAP — all perspectives classify as rope. This is intentional and structurally accurate: study-as-exercise is a genuine coordination mechanism that benefits all participants without extracting from any. The uniform classification reflects that the constraint solves a real collective-action problem (how to maintain suspended ritual obligation) with minimal coercive overhead and no suppressed alternatives. The absence of a perspectival gap is itself diagnostic: pure coordination constraints do not generate extraction asymmetry, so different structural positions do not produce divergent classifications. The studying community, rabbinic authority, individual practitioner, and analytical observer all experience the same structure because the structure is symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this story are beneficiaries or neutral — no victim set exists. The studying community, rabbinic authority, and individual practitioners all benefit from the coordination infrastructure that study-as-exercise provides. Directionality values derived from beneficiary status + mobile or constrained exit options yield low d for all agents, producing low or negative chi (experienced extractiveness is negligible or inverted into subsidy). Covenant fidelity is an abstract beneficiary (non-agent vindicated proposition) and does not feed directionality. The analytical observer holds the analytical seat, which derives d from the structural relationship rather than from agent-specific context. This is a constraint where the engine's beneficiary-driven derivation produces uniformly low chi across all perspectives, consistent with the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Study-as-exercise demonstrates that not all commitment-system constraints are extractive. The mandatrophy resolution here is straightforward: this IS coordination, not naturalized extraction. The rabbinic doctrine solves the genuine problem of occupying a suspended obligation without creating victim classes or suppressing alternatives. The minimal extraction (0.03) likely represents the irreducible coordination cost of maintaining any shared interpretive framework, not rent collection. The constraint vindicates propositions (intellectual devotion doctrine, study equivalence principle) but collects no rents from their vindication — vindicated propositions are non-agent beneficiaries that appear in the narrative but do not feed directionality or extraction metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is study-as-exercise the SOLE legitimate response to Temple absence, or one reading among several within the halakhic tradition?',
    'Comparative analysis of rabbinic opinions (Rambam vs Ramban on study sufficiency; disputes over whether study REPLACES or PREPARES for renewed sacrifice). Historical distribution of each reading''s institutional support.',
    'If sole legitimate reading: sibling readings (performance_only, hybrid_preparatory) are foreclosed — no coherent framework holds them. If one reading among several: readings coexist across different rabbinic lineages, and the kernel is genuinely contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether study-as-exercise forecloses or coexists with sibling readings').

omega_variable(
    study_equivalence_axiom_status,
    'Is the study-equivalence principle (study of sacrifice law = performance of sacrifice) a holdable axiom or an overridden position within contemporary halakhic discourse?',
    'Survey of contemporary responsa and yeshiva curricula: what proportion of authorities treat study as fully equivalent vs preparatory? Has messianic Zionism (Temple rebuilding expectation) shifted the axiom''s status?',
    'If holdable: study-as-exercise remains a live halakhic position. If overridden: the reading has been superseded by hybrid or restorationist positions, and the constraint''s minimal extraction reflects a historical configuration rather than contemporary practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_equivalence_axiom_status, empirical, 'Contemporary status of study-equivalence principle').

omega_variable(
    extraction_floor_uncertainty,
    'Does ANY constraint involving intellectual devotion and community identity maintenance have an irreducible extraction floor above zero, or is near-zero extraction structurally achievable?',
    'Cross-domain comparison: study-as-exercise vs other intellectual-devotional practices (Sufi dhikr, Christian lectio divina, Buddhist sutra recitation). Do all such practices show measurable in-group/out-group asymmetry, or can some achieve true zero-extraction coordination?',
    'If irreducible floor exists: extractiveness = 0.03 is the coordination cost, not extraction. If zero is achievable: 0.03 reflects mild suppression of alternative interpretive frameworks (e.g., Karaite rejection of rabbinic authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_floor_uncertainty, conceptual, 'Whether intellectual-devotional practices have irreducible extraction floor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_exercise, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_ex_theater_rabbinic_period, study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(study_ex_theater_medieval, study_as_exercise, theater_ratio, 500, 0.04).
narrative_ontology:measurement(study_ex_theater_modern, study_as_exercise, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(study_ex_extract_rabbinic_period, study_as_exercise, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(study_ex_extract_medieval, study_as_exercise, base_extractiveness, 500, 0.03).
narrative_ontology:measurement(study_ex_extract_modern, study_as_exercise, base_extractiveness, 1000, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(study_ex_suppress_rabbinic_period, study_as_exercise, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(study_ex_suppress_medieval, study_as_exercise, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(study_ex_suppress_modern, study_as_exercise, suppression_requirement, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_exercise, identity_coordination).

% DUAL FORMULATION NOTE:
% Part of temple_sacrifice_commitment kernel family. Sibling constraints (performance_only, hybrid_preparatory) are structurally distinct — different ε values reflecting different extraction from the same suspended obligation. This story models study-as-exercise reading only; siblings are separate files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
