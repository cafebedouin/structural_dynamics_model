% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Legitimate Mitzvah Exercise (Study-as-Exercise Reading)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   In this reading of the sacrifice-obligation kernel, the mitzvah
 *   (religious obligation) to bring sacrifices to the Temple is understood as
 *   being fulfilled through rigorous intellectual study and mastery of
 *   sacrificial law, rather than through physical performance. This reading
 *   emerged in Talmudic reasoning as a response to the destruction of the
 *   Second Temple (70 CE) and the impossibility of physical sacrifice under
 *   Diaspora conditions and Roman sovereignty. The rabbinic authorities who
 *   authored and sustained this reading claim that study of the halakhic
 *   (legal) details of sacrifice constitutes genuine exercise of the
 *   obligation, preserving both the obligation's force and the Jewish
 *   people's covenantal relationship to it. The reading is grounded in the
 *   principle that intellectual engagement with Torah is the highest form of
 *   religious action and that the sacrifice laws encode divine wisdom that
 *   must be continuously transmitted and studied. From this reading's
 *   internal perspective, there is no victims set — the suspension of
 *   physical performance is not extraction but rather an authorized
 *   transformation of the obligation's form under changed historical
 *   conditions. The beneficiary is rabbinic authority: the interpretive body
 *   that maintains exclusive power to define what counts as fulfillment and
 *   to adjudicate disputes about proper study methodology.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority — agenda-setter and beneficiary; defines what counts as legitimate study and fulfillment; maintains the monopoly on determining adequacy of intellectual engagement
 *   - jewish_community_practitioners — primary obligated parties; engage in study-based practice; accept rabbinic definitions of fulfillment; medium power, biographical horizon, constrained exit
 *   - alternative_interpretive_communities — Karaite, Samaritan, early Christian Jewish movements; reject the study-as-sufficient reading; maintain performance-obligation framing; excluded from rabbinic authority structure
 *   - historical_temple_institution — ceased operation after 70 CE; authorized conditions under which the original obligation was framed; no longer present to adjudicate competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Legitimate Mitzvah Exercise (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'faf6ac93-10ab-4e4d-bf6e-664ca66eea8a').
narrative_ontology:cs_kernel_codification('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', fixed_text).
narrative_ontology:cs_authority_grounding('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', lineage).
narrative_ontology:cs_interpretation_layer_present('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a').
narrative_ontology:cs_reading_relation('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', foundational, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', intellectual_engagement_fulfills_obligation, deontological).
narrative_ontology:cs_axiom('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', foundational, suspension_is_divinely_authorized_transformation).
narrative_ontology:cs_axiom_status(suspension_is_divinely_authorized_transformation, holdable).
narrative_ontology:cs_axiom_grounding('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', suspension_is_divinely_authorized_transformation, deontological).
narrative_ontology:cs_reference_frame('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', rabbinic_halakhic_authority_lineage).
narrative_ontology:cs_drift_state('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', contemporary_post_enlightenment_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('faf6ac93-10ab-4e4d-bf6e-664ca66eea8a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the authoritative interpretation of halakhic obligations, including the definition of what counts as legitimate fulfillment of the sacrifice obligation. Through study of Talmudic precedent and responsa (formal legal opinions), the rabbinic authority establishes that intellectual engagement with sacrificial law constitutes genuine exercise of the mitzvah. Derives legitimacy from a continuous lineage of transmitted teaching and from the principle that Torah study is the highest form of religious action. Controls adjudication of disputes about proper methodology and adequacy of study. Administers the obligation without undertaking it personally — benefits from monopoly on authoritative interpretation without bearing the obligational burden.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, analytical, global).

% Accept and participate in the study-based form of the sacrifice obligation. Engage in rigorous textual study (Talmud, halakhic codes, responsa) to fulfill the mitzvah. Benefit from the transformation of an impossible obligation (physical Temple sacrifice) into a feasible one (intellectual engagement). Organized through synagogues, study groups (chaburot), and yeshivas. Constrained exit because the study-based understanding has achieved near-universal acceptance within rabbinic Judaism; leaving the framework means abandoning halakhic identity itself. Benefit from the rabbinic authority's maintenance of the obligation's meaningfulness and from the continuity of Jewish religious practice across generations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community_practitioners, beneficiary,
    organized, generational, constrained, global).

% Karaite, Samaritan, early Christian Jewish, and other non-rabbinic interpretive communities reject the study-as-sufficient reading and maintain that the sacrifice obligation requires physical performance or remains impossible and therefore suspended. Their exclusion from the rabbinic framework is not through coercive suppression but through jurisdictional definition: the study-as-exercise reading is authorized within the rabbinic authority structure; alternative readings operate in distinct (and historically marginalized) interpretive communities. Would argue, if present in the rabbinic conversation, that study-only represents a spiritual diminishment and that the obligation's true form remains physical performance. Trapped because the rabbinic framework has become institutionally dominant in Jewish legal practice; alternative traditions survive but lack the institutional power to contest the rabbinic reading's authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, alternative_interpretive_traditions, excluded,
    powerful, generational, trapped, global).

% The historical Second Temple (destroyed 70 CE) was the site and institution through which the physical sacrifice obligation was originally fulfilled. Its destruction created the halakhic problem to which the study-as-exercise reading is a response. Non-agent status: the Temple ceased operation and cannot adjudicate contemporary readings of the obligation, though it remains the historical referent for the obligation's original form.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, temple_institution_historical, observer,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__study_as_exercise_reading, temple_institution_historical).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the sacrifice obligation's meaningfulness, force, and halakhic binding power across generations of Jewish diaspora life when physical sacrifice is structurally impossible. Solves the problem: 'How does a commanded obligation remain binding when its original form is no longer feasible?' Enables continuous transmission and refinement of sacrificial law as essential Torah knowledge, maintaining Jewish religious identity and covenantal relationship to the obligation across centuries and across geographies where Temple sacrifice cannot occur.
% TRANSFER_FUNCTION: Transfers interpretive authority from Temple priests (who performed sacrifices and adjudicated halakhic details) to rabbinic scholars (who study and adjudicate halakhic details). Moves the locus of obligation-fulfillment from Temple (a place) to study-hall and yeshiva (any site where rigorous textual engagement occurs). No material transfer of value or goods; the transfer is institutional and epistemological.
% ABSENT_VOICES: Alternative interpretive communities (Karaite, Samaritan, early Christian Jewish traditions) would argue — if present in the rabbinic conversation — that study-only represents a spiritual compromise and that the true obligation remains performance or suspension. They are excluded from the rabbinic authority structure not through active suppression but through competing jurisdiction: the rabbinic framework is institutionally dominant in Jewish legal practice; alternative readings survive in distinct communities but lack the institutional power to contest the study-as-exercise reading's authority within rabbinic Judaism. Later secular Jewish movements would also argue — if consulted — that the obligation has become historically obsolete rather than transformed, and that study of sacrificial law serves cultural preservation rather than halakhic obligation-fulfillment.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise reading disappeared and was replaced by the performance-only reading (or by complete abandonment of the obligation), Jewish religious practice would reorganize substantially: the obligation would re-emerge as impossible (no Temple, no legitimate alternative form) or would be understood as permanently suspended, requiring either messianic resurrection of Temple sacrifice or acceptance of the obligation's obsolescence. Yeshiva study of sacrifice law would shift from fulfillment-practice to historical-cultural study. The continuity of Jewish legal interpretation across 2,000 years of diaspora life would be fractured. Alternatively, if the symbolic-archive reading replaced it, study would continue but without the claim of obligation-fulfillment, shifting its meaning from religious obligation to cultural memory.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE and the dispersal of Jewish people into Diaspora under Roman sovereignty, the sacrifice obligation became structurally impossible to fulfill in its original form (physical performance at the Temple). Yet the obligation remains a central feature of Torah law, commanded with absolute force. How can an impossible obligation remain binding on the Jewish people? How do subsequent generations maintain covenantal relationship to this obligation across centuries and geographies where physical sacrifice cannot occur?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities from the early post-Temple period through the medieval period and into the present consistently affirm that the founding problem is live: the Talmud (Menahot 110a, et al.) debates how study and intention can substitute for physical performance; Maimonides (12th century) codes the study-as-fulfillment understanding as established halakhic principle; contemporary responsa and halakhic codes (Chofetz Chaim, Mishnah Berurah, and modern Orthodox halakhic literature) continue to adjudicate questions about adequate study methodology and fulfillment conditions. The Amoraim (Talmudic sages, 200-500 CE) who first worked out the study-as-exercise principle explicitly frame it as a response to the Temple's absence: study is what is possible now; performance is what would be ideal if the Temple existed. The problem is live as a halakhic matter for anyone who accepts that Torah obligations remain binding. Corroboration from outside the benefiting parties: secular historians and scholars of Judaism (not invested in rabbinic authority) confirm that this reading emerged as the authoritative response to Temple destruction and has been continuously maintained and refined across Jewish legal tradition. Even scholars critical of rabbinic authority acknowledge that the study-as-exercise reading became the normative understanding in diaspora Judaism.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.0 reflects the reading's own internal structure: under this reading, the obligation has been legitimately transformed (not suspended, not abandoned), and the new form (study) does not extract from participants — it constitutes their fulfillment of the mitzvah itself. Suppression is 0.0 because there is no coercive mechanism preventing alternatives; the reading claims that study-as-fulfillment is the correct understanding of the obligation's essence, not an enforced substitute. Theater is 0.0 because the study is not performative maintenance of a defunct function — it is the genuine form of the obligation under current halakhic understanding. Accessibility collapse is very high (0.95) because once the reading is understood, the alternatives (performance-only, purely symbolic study, abandonment of obligation) collapse into incoherence from within this framework's logic: study-as-exercise is presented as the natural and inevitable understanding of what a mitzvah IS when the physical form becomes impossible. Resistance is very low (0.05) because the reading has achieved nearly universal acceptance across rabbinic Judaism for nearly 2,000 years; active resistance to it is marginal and comes primarily from outside the rabbinic framework (Karaites, other non-rabbinic traditions). The claimed type is mountain: this reading treats the sufficiency of study as a structural feature of halakhic logic itself, not as a constructed policy choice. The beneficiary declaration (rabbinic authority) and vindicated propositions are present because FSM evaluation is warranted: the reading benefits a specific identifiable group (rabbinic interpreters who control fulfillment criteria), and the ambiguity about whether this is natural law or constructed interpretation is captured in omega variables.
 *
 * PERSPECTIVAL GAP:
 *   This reading instantiates only ONE seat's perspective, not a divergent seat-pair. Within rabbinic Judaism, the study-as-exercise reading achieves consensus among practitioners, authorities, and the interpretive lineage — there is no internal perspectival gap. The gap that EXISTS is between this reading and sibling readings (performance-only, messianic-suspension, symbolic-archive), but those are different constraints, not different seats within this one. The only potential gap would surface if one discovered that the reading benefits rabbinic authority in ways that practitioners do not recognize — i.e., if the authority's monopoly on fulfillment-criteria definition is extractive even though it is not experienced as coercive suppression. This is captured in the omega variables, not in per-seat divergence within this story.
 *
 * DIRECTIONALITY LOGIC:
 *   There is one declared beneficiary: rabbinic_interpretive_authority. This sits at d near the beneficiary end (low d, negative effective extraction) because the authority benefits from the monopoly on defining fulfillment criteria without bearing costs — it administers rather than undertakes the obligation itself. Practitioners (the actual obligated parties) are not declared as a stakeholder because they are not part of the constraint's beneficiary/victim structure under this reading — they benefit from the obligation's transformation (made feasible), they are not victimized by it, and they do not face suppression. The reading treats practitioners as participants in a coordinated solution, not as targets. Alternative interpretive communities are excluded from the framework rather than victimized by it — their exclusion is a structural fact of the authority's jurisdiction, not an extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the impossibility of physical sacrifice after 70 CE and under Diaspora conditions) is LIVE — it has not been solved or transcended. The reading's response is not to declare the obligation obsolete (which would resolve mandatrophy) but to redefine its form so that it remains binding and meaningful under new conditions. This is mandate preservation, not mandate death. However, the constraint is vulnerable to a specific mandatrophy challenge: if contemporary Jewish practice (particularly in secular movements and in non-Orthodox communities) increasingly treats the sacrifice obligation as historically superseded rather than as an ongoing halakhic claim, then the founding problem has effectively shifted from 'how to fulfill the obligation in absence of a Temple' to 'whether the obligation remains binding at all.' A story mapping that later condition would classify differently (likely as piton: the obligation persists through institutional inertia and textual authority, but the founding problem has atrophied). This story captures the reading at the moment of coherence — when the study-as-exercise transformation was the live solution to a live problem. Measurement drift over 2,000 years would show whether that coherence has held or whether authority erosion and mandate death have occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the sufficiency of study-without-performance a feature of halakhic logic itself (natural law of the Torah''s structure), or a constructed reading grounded in rabbinic interpretive authority that benefits from monopolizing fulfillment-criteria?',
    'Historical-linguistic analysis of Talmudic textual genealogy: do earlier strata (Mishna, early Amoraic layers) claim study alone suffices, or does the claim emerge from later interpretive layers? Cross-tradition comparison with non-rabbinic Jewish movements: do they accept study-as-sufficient, or do they maintain performance-obligation? Textual stress-testing: are there internal contradictions in Talmudic passages that suggest the claim was imposed rather than derived?',
    'If the claim is a natural feature of Torah logic, the constraint is correctly classified as mountain (zero extraction). If it is a constructed reading that emerged from later interpretive layers, it is a false-summit candidate: rabbinic authority benefits from monopolizing the definition of fulfillment, and the claim carries hidden extraction. The FSM signature would fire, reclassifying to tangled_rope (coordination function: preserving continuity and readiness; extraction: rabbinic control over fulfillment criteria).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether study-as-sufficient is a natural feature of halakhic logic or a constructed reading that benefits identifiable parties.').

omega_variable(
    kernel_framing_ambiguity,
    'What is the contested kernel? Is it (a) ''the nature of the sacrifice obligation itself'' (four readings of one underlying legal category), or (b) ''what counts as proper maintenance of Torah during Temple absence'' (four distinct mitzvot with different referents)?',
    'Textual genealogy of Talmudic passages: do all four readings frame themselves as interpretations of a SINGLE obligation (m. Menahot 110a et al.), or do they frame themselves as distinct obligations? Authority-structure analysis: do rabbinic, Karaite, Samaritan, and Christian Jewish-law traditions agree they are debating ONE kernel, or do they treat the debate as incommensurable?',
    'If the readings share ONE kernel (a single obligation, differently understood), then the study-as-exercise reading is a coherent commitment-system reading and belongs in the family. If the readings are incommensurable (debating different obligations), then this story does not have siblings — it is an orphan constraint, and the kernel_id framing is incorrect. The constraint would need re-authored under a different structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the four readings dispute one kernel or instantiate incommensurable obligations.').

omega_variable(
    authority_legitimacy_grounding,
    'Does the rabbinic authority that defines study-as-fulfillment ground its legitimacy in text-interpretation fidelity (lineage: faithful transmission of revealed teaching), or in institutional-maintenance efficacy (extraction: the authority sustains itself by controlling fulfillment criteria)?',
    'Close reading of rabbinic self-justification for the study-as-sufficient claim: do responsa and halakhic codes justify it via rigorous textual derivation (lineage signal), or via institutional continuity and communal welfare arguments (maintenance signal)? Institutional history: did the claim emerge when rabbinic authority was consolidating power over Jewish practice? Did competing authority structures (Karaite, Samaritan, other lineages) reject the claim?',
    'If lineage grounds the authority (textual fidelity), the mountain classification is stable. If institutional maintenance grounds it (the authority benefits from controlling fulfillment), the false-summit signature fires and reclassification to tangled_rope follows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_legitimacy_grounding, conceptual, 'Whether rabbinic authority legitimacy rests on text-interpretation fidelity or institutional-maintenance capture.').

omega_variable(
    kernel_drift_in_contemporary_judaism,
    'In contemporary Jewish movements (Orthodox, Conservative, Reform, Reconstructionist, secular Jewish identity), does the study-as-exercise reading remain the authoritative understanding of sacrifice-obligation fulfillment, or has it drifted toward symbolic-archive or performance-only readings?',
    'Survey of contemporary halakhic codes, movement positions, and lay practice: which readings do different Jewish communities affirm? Has the performance-only reading (messianic-suspension framing) weakened as messianic expectation itself has attenuated? Has the symbolic-archive reading gained ground as secular Jewish identity has decoupled from halakhic obligation?',
    'If the study-as-exercise reading has maintained authority across movements, it is a stable commitment. If it has been superseded by other readings in most communities (exception: Orthodox), the drift-state measurement should show authority erosion and the reading''s reference frame (rabbinic-halakhic monopoly on fulfillment definition) should be marked as undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_drift_in_contemporary_judaism, empirical, 'Whether the study-as-exercise reading has maintained authority or drifted toward alternative readings in contemporary Judaism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 2000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel is a contested textual kernel with four structurally distinct constraint instantiations (study-as-exercise, performance-only, messianic-suspension, symbolic-archive). Each reading has a different ε (this one: 0.0; performance-only: likely 0.5-0.7 if extractive; messianic-suspension: likely 0.1-0.3 if seen as forestalling performance; symbolic-archive: likely 0.0 if purely archival). Each reading has a different beneficiary structure (this one: rabbinic authority; performance-only: possibly Temple institution or Zealot movements; messianic-suspension: possibly rabbinic authority + messianic hope; symbolic-archive: possibly secular Jewish identity or diaspora communities). The four stories are linked as a constraint family via network.affects_constraints; no single ε-value or beneficiary set captures all four readings simultaneously. The kernel-framing ambiguity omega documents the possibility that the readings are incommensurable (four different obligations rather than four readings of one obligation), which would dissolve the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
