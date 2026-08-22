% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Study as Future-Oriented Archive (Archive Maintenance Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Within Jewish halakhic tradition, the obligation to study the sacrificial
 *   laws is presented as preparation for the messiah's coming and the
 *   Temple's restoration. This is the 'archive maintenance' reading: study
 *   preserves technical knowledge (exact measurements, priestly procedures,
 *   animal species, seasonal variations) for a future generation that will
 *   need to restore Temple service. The constraint operates as a Scaffold—a
 *   temporary arrangement justified by its transitional function
 *   (preservation for a messianic future). The present cost is borne by study
 *   participants; the benefit accrues to a future generation whose existence
 *   and needs are contingent on a theological event (messiah's arrival and
 *   Temple rebuilding) that may never occur. This reading is contested by two
 *   siblings: performance_only (the commandments are suspended, not
 *   fulfilled, without a Temple) and study_as_performance (studying the law
 *   IS the fulfillment of the commandment in the present). The ε-invariance
 *   principle requires that each reading be authored as a separate, ε-stable
 *   constraint. This story captures the archive-maintenance reading's
 *   structural view: the standing obligation to study sacrifice law, assessed
 *   by the archive-maintenance reading's own lights, is moderately extractive
 *   because it imposes present burden justified by deferred benefit. The
 *   sibling readings would produce different ε values (study_as_performance
 *   would show lower extractiveness because it frames study as
 *   present-focused fulfillment; performance_only would show near-zero ε
 *   because it treats study as optional, not obligatory). Each constraint
 *   story is linked via network.affects_constraints to show how the reading
 *   choice determines the classification.
 *
 * KEY AGENTS:
 *   - contemporary_study_participants: obligated to maintain expertise in unexecutable commandments (moderate power, identity_locked exit — target seat bearing present cost)
 *   - future_jewish_community: would inherit preserved knowledge but existence/needs contingent on messianic restoration (powerless, trapped exit — deferred beneficiary)
 *   - halakhic_authorities: set and enforce study curriculum, gain prestige and institutional power from leadership (institutional power, mobile exit — agenda-setter and gain_flow recipient)
 *   - messiah: theological event representing condition whose arrival would validate study obligation (non-agent placeholder for the messianic frame)
 *   - alternative_religious_movements: excluded from consensus, would argue obligation is theater not preparation (moderate power, constrained exit — excluded seat with contestatory reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.58).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Study as Future-Oriented Archive (Archive Maintenance Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '140e7285-f4c4-45fa-a5be-5a30c430ac80').
narrative_ontology:cs_kernel_codification('140e7285-f4c4-45fa-a5be-5a30c430ac80', formalized).
narrative_ontology:cs_authority_grounding('140e7285-f4c4-45fa-a5be-5a30c430ac80', lineage).
narrative_ontology:cs_interpretation_layer_present('140e7285-f4c4-45fa-a5be-5a30c430ac80').
narrative_ontology:cs_reading_relation('140e7285-f4c4-45fa-a5be-5a30c430ac80', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('140e7285-f4c4-45fa-a5be-5a30c430ac80', sacrifice_commandment__study_as_performance, influences).
narrative_ontology:cs_axiom('140e7285-f4c4-45fa-a5be-5a30c430ac80', foundational, messianic_temple_restoration_obligatory).
narrative_ontology:cs_axiom_status(messianic_temple_restoration_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('140e7285-f4c4-45fa-a5be-5a30c430ac80', messianic_temple_restoration_obligatory, deontological).
narrative_ontology:cs_axiom('140e7285-f4c4-45fa-a5be-5a30c430ac80', foundational, knowledge_preservation_duty).
narrative_ontology:cs_axiom_status(knowledge_preservation_duty, holdable).
narrative_ontology:cs_axiom_grounding('140e7285-f4c4-45fa-a5be-5a30c430ac80', knowledge_preservation_duty, instrumental).
narrative_ontology:cs_reference_frame('140e7285-f4c4-45fa-a5be-5a30c430ac80', diaspora_preparation_framework).
narrative_ontology:cs_drift_state('140e7285-f4c4-45fa-a5be-5a30c430ac80', contemporary_post_secular_jewish_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('140e7285-f4c4-45fa-a5be-5a30c430ac80', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_jewish_community).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, contemporary_study_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated to study and teach sacrifice law with full technical detail (measurements, procedures, materials, priestly roles) despite the commandments being unexecutable in the present. They bear the cognitive and time cost of maintaining expertise in a practice whose utility is deferred to an indefinite future. Exit would constitute abandonment of the halakhic obligation and rejection of core Jewish identity; constrained because the duty is woven into the self-concept of a religious Jew and into the continuity of the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, contemporary_study_participants, payer,
    moderate, biographical, identity_locked, national).

% Receives the preserved technical knowledge required to restore and perform the Temple sacrifices upon the messiah's coming and Temple reconstruction. They inherit a fully transmissible system of law rather than fragmentary records. Their benefit is contingent on the messiah's arrival and Temple rebuilding; they cannot opt out of needing this knowledge if the condition is met.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_jewish_community, beneficiary,
    powerless, generational, trapped, national).

% Decide which sacrifice laws must be studied, how deeply, in what order, and enforce compliance through status mechanisms (recognition of scholarship, leadership roles, institutional positions). They justify the requirement as preserving divine commandment and preparing the Jewish people for messiah's coming. They have discretion over curriculum emphasis but are constrained by received halakhic tradition and the need to maintain consensus.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_authorities, agenda_setter,
    institutional, generational, mobile, national).

% A non-agent entity (theological event/concept, not a person or group with agency). Represents the condition whose arrival would activate the deferred benefit and validate the study requirement as preparation rather than theater.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messiah, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__archive_maintenance, messiah).

% Are shut out of authoritative interpretation of sacrifice law and cannot claim the legitimacy that detailed study and mastery confers. They are excluded because their theology rejects or radically reinterprets the messianic restoration frame (e.g., Classical Reform Judaism, some strands of modern Jewish philosophy). Their presence in the conversation would challenge the foundational axiom (messianic_temple_restoration_obligatory) by rejecting its deontological grounding.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, alternative_religious_movements, excluded,
    moderate, biographical, constrained, national).

% Examines the structural arrangement: present cost (study obligation), deferred uncertain benefit (knowledge for future restoration), enforcement mechanism (halakhic authority via status), and the contestation about whether this is genuine preparation or theological theater masking a present-day identity function.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, halakhic_authorities).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits the technical and halakhic knowledge necessary to perform the Temple sacrifices correctly and completely, should the Temple be rebuilt and sacrificial service restored. Solves the problem of intergenerational knowledge loss across centuries of diaspora and suspension of Temple practice. The coordination problem is: how does a religious community maintain mastery of a complex legal system for a practice that is currently impossible but might become possible again?
% TRANSFER_FUNCTION: Moves the obligation to study sacrifice law from its original context (actual Temple practice and performance) to a deferred, contingent future context (messianic era restoration and rebuilding). The transfer is temporal and conditional: present study burden → preserved knowledge availability → potential future performance capability. The arrangement transfers the cost of knowledge maintenance from a hypothetical future generation (which would have to learn from fragmentary sources) to the present generation (which pays the cost of detailed study now).
% ABSENT_VOICES: Rival halakhic movements and religious philosophers that have rejected the messianic restoration framework (e.g., Classical Reform Judaism, Spinoza, modern Jewish existentialists) are excluded from the conversation about whether this study obligation is legitimate. They would argue the messianic frame is obsolete or metaphorical, and the study obligation is theater masking a present identity function rather than genuine preparation. Their corroboration would come from textual analysis of Jewish philosophy and from the lived experience of communities that have abandoned the obligation without suffering spiritual loss. They are excluded because their theology undermines the foundational axiom on which the archive-maintenance reading rests.
% DISAPPEARANCE_RATIONALE: From the archive-maintenance reading: if the study obligation disappeared, the Temple's restoration would be impossible because the technical knowledge would be lost—upon the messiah's arrival, the Jewish people would be incapable of restoring sacrifice properly, violating divine law. The world would rearrange in a catastrophic way (spiritual incompleteness, failure to fulfill restored commandments). From the alternative readings (study_as_performance): the obligation disappearing would remove an extractive pseudo-duty that masks present theological meaning in false future language; the world would reorganize around present-moment observance and spiritual fulfillment rather than deferred messianic hope. The readings dispute what constitutes rearrangement: failure to prepare (archive view) vs. authentic present engagement (performance view).
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments became impossible to perform. The foundational question: how does the Jewish people maintain its covenant with God and uphold divine law when the commandments that sealed that covenant are suspended? Two answers emerged: (1) study of the laws preserves them for the messiah's coming, such that when the Temple is rebuilt, sacrifice can resume [archive_maintenance reading], or (2) the study itself becomes the performance, fulfilling the obligation in the present through intellectual engagement [study_as_performance reading]. A third position: the commandments are simply suspended, not fulfilled, and the Jewish people awaits restoration [performance_only reading].
% FOUNDING_PROBLEM_CORROBORATION: Halakhic tradition (Mishnah Avot, Talmud, Maimonides, subsequent rabbinic literature) attests that the founding problem—how to relate to impossible commandments—is still live and central to Jewish theology. Maimonides (12th century) and later authorities continue to mandate detailed study of sacrifice, explicitly citing the framework of preparation for messiah's coming. However, corroboration for the archive-maintenance reading specifically comes from Maimonides' Mishneh Torah and from Yeshiva culture, which treat knowledge preservation as primary. Alternative theological movements (Jewish Enlightenment philosophers, Classical Reform, modern Jewish existentialism) corroborate that the archive-maintenance reading is contested—they dispute whether the messianic frame is operative or whether present-moment theological meaning has superseded it. Texts from outside the benefiting halakhic authorities (Jewish philosophy, modern Jewish thought, sociology of contemporary Jewish practice) provide external corroboration that the reading is under challenge.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the study obligation imposes a real present cost (time, cognitive load, institutional commitment) whose future benefit is uncertain and contingent. The benefit nominally accrues to a generation that may never exist (if the messiah does not come) or may exist in a radically transformed context where the preserved knowledge is inapplicable or superseded. The constraint is claimed as a Scaffold because its stated justification is explicitly transitional: it exists to prepare for a future state (Temple rebuilding) at which point it would sunset (because the commandments would be performable again instead of studied). Theater ratio is high (0.67) because a substantial portion of contemporary learning activity is devoted to rehearsal and preservation of law rather than actual performance—the ceremonial structure, the pedagogical apparatus, the institutional frameworks all serve the archival function rather than present-day ritual. Suppression is moderate (0.42) because the obligation is enforced primarily through identity mechanisms (status, recognition, role assignment in the community) rather than external coercion, yet it is also deeply constraining because exit would mean leaving the tradition itself. The measurement series model a historical trajectory: extractiveness rises from t=0 (early medieval period, when messiah seemed proximate) to t=15 (early modern period, as historical time elapsed) and then plateaus at t=25 (contemporary era), because as time passes without messiah's arrival, the extractiveness of the obligation increases (the deferred benefit becomes increasingly remote and hypothetical) but eventually stabilizes at a certain institutional level because the obligation becomes so normalized and identity-fused that it stops feeling like a burden and starts feeling like an identity. Theater ratio rises correspondingly over the same interval: as the connection to an actual future Temple becomes more distant and historically implausible, more of the activity appears ceremonial, performative, and identity-maintaining rather than pragmatically preparatory. Suppression_requirement shows parallel rise but more modest magnitude, because the internalization of the obligation through identity reduces the need for active external enforcement—the constraint becomes self-policing.
 *
 * PERSPECTIVAL GAP:
 *   The halakhic_authorities seat and the contemporary_study_participants seat should compute to different types from the engine's per-seat analysis. Authorities would compute as beneficiary or symmetric (they set the rules, enforce compliance through status mechanisms, gain prestige and institutional position from leading scholarship, and have mobile exit options—they can reinterpret the requirement if institutional context demands it). Study participants would compute as target or payer (they bear the present cost, have identity_locked exit that makes departure impossible without self-concept dissolution, receive deferred and uncertain benefit). The future_jewish_community would compute as beneficiary but is structurally powerless and exists only contingently. This divergence is the engine's per-seat computation from the structural data: the same constraint (study obligation) operates differently from different seats because the directionality (d) toward the constraint differs by power level and exit mechanism. The archive-maintenance reading itself claims that the arrangement is genuine preparation, not extraction—that the study obligation is legitimate because it solves the real problem of knowledge loss. But the metrics describe extractiveness and theater, which the engine measures and reports. This is the claim/metric independence rule in action: the reading's own framing (claimed-as-scaffold: transitional, justified by future restoration) is independent of the empirical pattern (extractive-and-theatrical: present burden, deferred uncertain benefit, growing performativity as the future recedes). The engine computes which is structurally true given the measurement data.
 *
 * DIRECTIONALITY LOGIC:
 *   The contemporary study participants carry high directionality toward target (d near 1.0): they bear the cost (time, cognitive load, the obligation to maintain expertise in unexecutable law) without receiving a present benefit. Their exit is identity_locked because the study obligation is fused with Jewish identity and halakhic continuity—leaving would mean apostasy or rejection of the tradition, which is not merely a choice but an identity death. The structural derivation gives high d (victim in base_properties, identity_locked exit, moderate power). The future Jewish community carries nominal-beneficiary directionality (d near 0.0) but is paradoxically powerless and contingent—they are the beneficiary of the preserved knowledge but cannot influence whether the arrangement persists because they don't exist. Halakhic authorities carry high beneficiary directionality (d near 0.0): they set the rules (agenda_setter role), gain prestige and institutional position (concentrated benefit), and have mobile exit (they can reinterpret the requirement if needed). They are not victimized by the constraint but rather empowered. Alternative_religious_movements carry excluded-party directionality: they are structurally outside the coordination function because their theological framework rejects the foundational axiom. The 'messiah' entry carries contingent-beneficiary directionality: it is the nominal beneficiary (the knowledge is preserved for its arrival) but it is a non-agent and its contingency makes the benefit structure uncertain. No directionality_overrides are needed here because the structural derivation from beneficiary/victim declarations and exit_options produces the accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The archive-maintenance reading faces a mandatrophy risk: the founding problem (how to relate to impossible commandments after Temple destruction) was solved by one of three mechanisms: (1) study becomes the performance (study_as_performance reading), or (2) study preserves knowledge for messianic future (archive_maintenance reading), or (3) commandments are suspended (performance_only reading). The evidence suggests the tradition has de facto migrated toward reading (1) even while publicly maintaining reading (2). The measurements show theater_ratio rising from 0.58 to 0.67 over 25 time units—a 15% increase in the proportion of activity that is performative rather than functional. This is consistent with mandatrophy: the obligation persists but its stated justification (preparation for messiah) has been displaced by an unstated justification (present identity, theological meaning, continuity). Maimonides explicitly justified study as preparation; contemporary yeshivas justify it as the exercise of the commandment itself. The divergence between founding_problem_status=live and the actual institutional practice (which treats study as fulfillment rather than preparation) is the mandatrophy flag. However, the halakhic consensus has not yet formally overridden the archive-maintenance justification—it remains authoritative in Maimonidean tradition and is still taught alongside the study_as_performance reading. The constraint persists in an institutional state of denial about its own function: the mandate (preparation) has atrophied but the arrangement (obligation to study) remains and is now sustained by a different logic (present fulfillment). A formal mandatrophy verdict would require the tradition to acknowledge that the founding problem is solved and the obligation now serves a different purpose. Until that acknowledgment occurs, the constraint is in transition between scaffold (temporary, preparation-focused) and piton (inertial, identity-focused).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_arrival_contingency,
    'What is the actual institutional and theological commitment to the messiah''s arrival and Temple rebuilding? Is it treated as an eschatological certainty or as a low-probability theological ideal?',
    'Textual and ethnographic analysis: examine halakhic authorities'' treatment of messianic timing, probability, and conditions. If the sources treat messiah''s coming as imminent and inevitable, the archive-maintenance framing is credible; if they treat it as distant and conditional on divine will beyond human influence, the archive-maintenance reading''s justification becomes weaker because the deferred benefit is so remote as to be effectively non-existent.',
    'If messiah''s arrival is treated as low-probability or infinitely deferred, the archive-maintenance reading''s scaffold justification (temporary, preparation-focused) collapses into a piton justification (inertial, theater-focused). The constraint would reclassify from scaffold to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_arrival_contingency, empirical, 'Whether the deferred benefit (messiah''s arrival) is treated as credible justification for present obligation.').

omega_variable(
    knowledge_preservation_necessity,
    'Is detailed, intensive study of sacrifice law the necessary and sufficient mechanism to preserve technical knowledge across generations, or could archival, textual transmission be adequate without the living obligation to study?',
    'Historical comparison: examine communities that have not maintained intensive study obligations (secular Jewish communities, reform movements, diaspora communities far from centers of learning) and assess whether knowledge of sacrifice has survived in them through texts alone. Can knowledge be preserved through written sources and occasional scholarly consultation without a living obligation to master the material?',
    'If detailed study is unnecessary for preservation, the constraint is not solving a genuine coordination problem (knowledge transmission across generations) but rather imposing a burden in the name of a problem that could be solved more cheaply through texts. This would lower the coordination-function credibility and raise the extractiveness assessment significantly (ε could rise to 0.70+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_preservation_necessity, empirical, 'Whether intensive study obligation is the only viable knowledge preservation mechanism.').

omega_variable(
    study_as_performance_displacement,
    'Has the halakhic tradition de facto adopted the study_as_performance reading, such that the archive-maintenance reading''s mandate has atrophied and the obligation now serves present identity and theological meaning rather than future preparation?',
    'Textual and institutional analysis: examine halakhic authorities'' stated justifications for the obligation over historical time. If early authorities justify study as preparation for messiah, and later authorities justify it as fulfillment of the commandment in the present, a reading displacement has occurred. If the learning culture treats knowledge mastery as the primary goal rather than knowledge preservation, the displacement is confirmed.',
    'If mandate displacement has occurred, the constraint has undergone mandatrophy: its stated function (preparation) has been functionally replaced by an unstated function (present identity). The constraint should reclassify from scaffold (temporary, future-focused) to piton (inertial, performance-focused). The rising theater_ratio in the measurements supports this diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_performance_displacement, empirical, 'Whether the constraint''s founding justification has been functionally replaced by a present-day justification within the tradition.').

omega_variable(
    internal_vs_external_suppression,
    'Is the constraint''s persistence and compliance achieved through identity internalization (the obligation feels intrinsic to Jewish selfhood) or through external institutional enforcement (status mechanisms, community pressure, role assignment)?',
    'Post-exit ethnography: interview individuals who have left the study obligation and assess whether they report internalized guilt and constraint (identity suppression) or report that institutional status mechanisms no longer apply (structural suppression). If post-exit individuals continue to experience the obligation as binding on their identity, suppression is internalized; if they report relief upon institutional exit, suppression is structural.',
    'If suppression is substantially internalized, the effective suppression is higher than the scalar 0.42 suggests, and the constraint is more extractive and binding than the base metric indicates. The agent carries the suppression with them after exit, which is a signature of deep identity-fusion. If suppression is mostly structural, it is maintained by institutional status mechanisms and could be lifted by institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_suppression, empirical, 'Whether suppression of the study obligation is internalized (identity-fused) or structural (institutional).').

omega_variable(
    reading_underdetermination_at_kernel,
    'Is the archive-maintenance reading the only coherent halakhic framing of the standing obligation to study sacrifice, or are study_as_performance and performance_only equally defensible within the authoritative tradition?',
    'Textual archaeology and scholastic history: trace the earliest and most authoritative rabbinic sources (Mishnah, Talmud, Maimonides, early Amoraim) for their explicit justifications of the study obligation. If all early sources cite future preparation (archive-maintenance), the reading is historically dominant. If early sources cite multiple justifications (preparation and present fulfillment both present), the kernel is genuinely underdetermined.',
    'If all three readings are equally defensible from the canonical sources, the kernel is genuinely contestatory and the archive-maintenance reading is one live position among others with equivalent textual warrant. If archive-maintenance is the historic consensus but is now being displaced, mandatrophy is occurring and reclassification from scaffold to piton is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_underdetermination_at_kernel, conceptual, 'Whether the sacrifice_commandment kernel admits multiple defensible readings or whether one reading holds textual priority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.58).
narrative_ontology:measurement(sacr_tr_t5, sacrifice_commandment__archive_maintenance, theater_ratio, 5, 0.61).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_commandment__archive_maintenance, theater_ratio, 10, 0.64).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_commandment__archive_maintenance, theater_ratio, 15, 0.67).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.68).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_commandment__archive_maintenance, theater_ratio, 25, 0.67).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sacr_be_t5, sacrifice_commandment__archive_maintenance, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(sacr_be_t10, sacrifice_commandment__archive_maintenance, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sacr_be_t15, sacrifice_commandment__archive_maintenance, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(sacr_be_t25, sacrifice_commandment__archive_maintenance, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sacr_su_t5, sacrifice_commandment__archive_maintenance, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(sacr_su_t10, sacrifice_commandment__archive_maintenance, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(sacr_su_t15, sacrifice_commandment__archive_maintenance, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(sacr_su_t25, sacrifice_commandment__archive_maintenance, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.12).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three constraint stories, one per reading. archive_maintenance: study preserves knowledge for messianic Temple restoration (this story); performance_only: commandments are suspended without Temple, not fulfilled by study; study_as_performance: studying the law is itself the fulfillment of the obligation in the present. Each reading instantiates a different constraint with different ε values, beneficiary structures, and types. The three stories form a constraint family linked by network.affects_constraints. Archive-maintenance influences both siblings by claiming that the reading dispute is about future orientation vs. present meaning—it establishes the terms on which the siblings define themselves as alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
