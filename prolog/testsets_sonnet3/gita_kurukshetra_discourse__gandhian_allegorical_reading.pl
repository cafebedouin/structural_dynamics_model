% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This story generates one reading of the contested Kurukshetra-discourse
 *   kernel: the Gandhian allegorical reading, which holds that the
 *   battlefield of the Bhagavad Gita and Arjuna's crisis are figures for an
 *   internal moral struggle (against ego, attachment, and fear), and that
 *   Krishna's counsel is spiritual instruction rather than a literal sanction
 *   for armed conflict or caste-bound martial duty. This reading emerged and
 *   hardened in the specific historical context of early-twentieth-century
 *   Indian anti-colonial politics, where it did real coordinating work —
 *   supplying scriptural legitimacy to a mass nonviolent movement — while
 *   simultaneously displacing rival readings that had themselves undergirded
 *   caste hierarchy and literalist justifications for violence. The reading
 *   is authored here as ε-invariant on its own terms: extraction is assessed
 *   against the standing arrangement of scriptural authority the reading
 *   contests (who gets to say what the text means, and what social
 *   arrangements that determination underwrites), not against the reading's
 *   own preferred end state of universal ahimsa. Sibling readings (orthodox
 *   literal, universalist devotional) are separate constraints, not alternate
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - satyagraha_movement_leadership: primary agenda_setter, organized/mobile — advances and mobilizes the reading
 *   - orthodox_ritual_authorities_displaced_by_lay_reading: primary payer, institutional/constrained — loses interpretive monopoly
 *   - literalist_martial_traditions_delegitimized: payer, powerful/constrained — loses scriptural warrant for armed action
 *   - caste_hierarchy_beneficiaries_losing_scriptural_warrant: payer, powerful/constrained — loses svadharma-as-caste-duty warrant
 *   - subjects_of_literal_caste_and_war_violence: excluded, powerless/trapped — invoked as evidence, absent from the interpretive contest
 *   - comparative_religion_scholars: analytical observer — traces the reading's historical emergence and selective textual emphasis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.58).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.62).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'b1fabd04-6737-4f4b-94bb-5f05044b13b7').
narrative_ontology:cs_kernel_codification('b1fabd04-6737-4f4b-94bb-5f05044b13b7', fixed_text).
narrative_ontology:cs_authority_grounding('b1fabd04-6737-4f4b-94bb-5f05044b13b7', practice).
narrative_ontology:cs_interpretation_layer_present('b1fabd04-6737-4f4b-94bb-5f05044b13b7').
narrative_ontology:cs_reading_relation('b1fabd04-6737-4f4b-94bb-5f05044b13b7', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('b1fabd04-6737-4f4b-94bb-5f05044b13b7', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('b1fabd04-6737-4f4b-94bb-5f05044b13b7', foundational, ahimsa_supersedes_svadharma_as_caste_duty).
narrative_ontology:cs_axiom_status(ahimsa_supersedes_svadharma_as_caste_duty, holdable).
narrative_ontology:cs_axiom_grounding('b1fabd04-6737-4f4b-94bb-5f05044b13b7', ahimsa_supersedes_svadharma_as_caste_duty, deontological).
narrative_ontology:cs_axiom('b1fabd04-6737-4f4b-94bb-5f05044b13b7', foundational, individual_conscience_supersedes_lineage_interpretive_authority).
narrative_ontology:cs_axiom_status(individual_conscience_supersedes_lineage_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('b1fabd04-6737-4f4b-94bb-5f05044b13b7', individual_conscience_supersedes_lineage_interpretive_authority, conventional).
narrative_ontology:cs_axiom('b1fabd04-6737-4f4b-94bb-5f05044b13b7', secondary, battlefield_violence_is_figurative_not_literal_sanction).
narrative_ontology:cs_axiom_status(battlefield_violence_is_figurative_not_literal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('b1fabd04-6737-4f4b-94bb-5f05044b13b7', battlefield_violence_is_figurative_not_literal_sanction, conventional).
narrative_ontology:cs_reference_frame('b1fabd04-6737-4f4b-94bb-5f05044b13b7', precolonial_brahminical_commentarial_authority).
narrative_ontology:cs_drift_state('b1fabd04-6737-4f4b-94bb-5f05044b13b7', anticolonial_nationalist_movement_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b1fabd04-6737-4f4b-94bb-5f05044b13b7', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_hindu_laity).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, cross_communal_nonviolence_advocates).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_ritual_authorities_displaced_by_lay_reading).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_martial_traditions_delegitimized).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_beneficiaries_losing_scriptural_warrant).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_as_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advances the allegorical reading as scriptural warrant for organized nonviolent resistance, reinterpreting Krishna's counsel to Arjuna as inner moral struggle rather than sanction for war. Sets the terms of the reading, publishes commentary, and mobilizes the interpretation into political practice. Benefits from the legitimacy the reinterpretation lends to a mass movement grounded in indigenous scripture rather than borrowed Western ethics.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership, agenda_setter,
    organized, generational, mobile, national).

% Gains access to the text without Brahminical mediation; can read the Gita as a manual for personal ethical struggle rather than a caste-bound military-religious code. Their exit from orthodox interpretive dependency is real but incomplete — they remain embedded in social structures that still enforce caste practice regardless of how they read scripture.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_hindu_laity, beneficiary,
    moderate, biographical, constrained, national).

% Draws on the allegorical reading to build a universalizable ethic of nonviolence usable outside Hindu contexts (Tolstoy, King, and successor movements). They receive a portable moral vocabulary decoupled from the text's literal warfare and caste content.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, cross_communal_nonviolence_advocates, beneficiary,
    moderate, civilizational, mobile, global).

% Traditionally held interpretive monopoly over the text through Sanskrit training, guru lineages, and temple authority. The allegorical reading routes interpretive legitimacy to individual conscience, eroding their gatekeeping function and the social capital tied to being the sanctioned exegetes. They cannot simply exit their institutional role without losing the basis of their authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_ritual_authorities_displaced_by_lay_reading, payer,
    institutional, generational, constrained, national).

% Kshatriya-identified and militant nationalist readings that cite the Gita's literal sanction of righteous war (as Krishna's command to Arjuna to fight) lose scriptural cover under the allegorical frame. Political actors who invoked the literal battlefield to justify armed resistance or communal violence are denied that textual ground; they experience the reading as an attack on a legitimate martial-dharma tradition, not merely as a difference of opinion.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_martial_traditions_delegitimized, payer,
    powerful, biographical, constrained, national).

% Those whose social position depended on the Gita's apparent endorsement of svadharma-as-caste-duty (Arjuna's duty as a warrior born to his caste) lose a load-bearing scriptural justification when the battlefield is read as internal, not social. They cannot simply relocate their claim to legitimacy elsewhere without conceding the point at issue.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_beneficiaries_losing_scriptural_warrant, payer,
    powerful, generational, constrained, national).

% Lower-caste communities and populations historically subjected to literal violence justified by dharmic-war and caste-duty readings of the text are not themselves party to the elite hermeneutic contest between allegorists, literalists, and devotionalists, even though the outcome of that contest determines whether scripture is invoked against them. Their historical suffering is the retrospective evidence the allegorical reading cites, but they are not seated at the interpretive table.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, subjects_of_literal_caste_and_war_violence, excluded,
    powerless, civilizational, trapped, national).

% Study the allegorical reading as one of several historically situated receptions of the text, tracing its emergence in the colonial and anti-colonial period and its selective use of textual and extratextual material. They neither benefit from nor pay the costs of the reading's political deployment.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral vocabulary that lets a mass nonviolent movement recruit religious legitimacy from its own tradition's central scripture, coordinating diverse participants around a single ethical frame (inner struggle against ego and attachment) without requiring literal endorsement of caste duty or war.
% TRANSFER_FUNCTION: Moves interpretive authority away from Brahminical scholarly lineages and orthodox militarist readings toward individual conscience and movement leadership; moves scriptural legitimacy away from caste-duty and literal-war justifications toward a universalizable nonviolence ethic; the social costs of that authority transfer land on displaced orthodox authorities and on political actors who relied on literal readings for martial or caste legitimation.
% ABSENT_VOICES: Communities historically subject to caste violence and to violence justified by literal dharmic-war readings are invoked as the ultimate stakes of the interpretive contest but are not participants in the scholarly or leadership-level debate over how to read the text; their historical experience functions as evidence rather than as a seat in the discourse.
% DISAPPEARANCE_RATIONALE: If the allegorical reading disappeared, the satyagraha movement's scriptural self-justification would need reconstruction (arrangements clearly depend on it), but orthodox and devotional readings of the Gita would persist largely unchanged since they do not depend on the allegorical frame for their own coherence — hence contested rather than uniformly world_rearranges.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century reformers needed to reconcile a scripture that appears to command violence and caste-bound duty with an emerging political commitment to nonviolent resistance and social equality; the allegorical reading was built to resolve that apparent contradiction without discarding the text's authority.
% FOUNDING_PROBLEM_CORROBORATION: Gandhi and successor movement leaders (the reading's primary beneficiaries) attest the founding problem remains live — that literal readings continue to be invoked to justify caste hierarchy and communal violence in contemporary politics. Independent historians of religion and postcolonial scholars, outside the movement's leadership, corroborate that the allegorical reading emerged specifically as a strategic response to colonial-era Orientalist and Hindu-nationalist literalist readings, but many also observe that the reading itself selectively de-emphasizes textual material (the caste-duty verses of chapter 18, the theophany of chapter 11) rather than resolving the tension it claims to resolve — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — substantial but not extreme — because the allegorical reading, while displacing orthodox interpretive authority and delegitimizing literalist-martial and caste-warrant readings (a real cost to those parties), does so in service of a genuine coordination function: supplying a scripturally grounded ethic for a mass nonviolent movement. Suppression (0.62) reflects that maintaining the allegorical frame against literalist counter-readings has required active argumentative and institutional work — sustained commentary production, movement discipline, and rhetorical contestation against orthodox and militarist rivals — not passive acceptance. Theater ratio is moderate-low (0.30): the interpretive labor is substantially functional (it did organize real political action), though some of the exegetical apparatus (elaborate allegorical mappings of every character to a psychological faculty) drifts toward performative systematization once the movement's practical need for scriptural cover receded. Accessibility collapse is moderate (0.40) because rival readings remain fully available and contested in scholarship and lived practice — this is not a case where alternatives have vanished. Resistance is moderate-high (0.55), consistent with ongoing orthodox and Hindu-nationalist pushback against the allegorical frame across the twentieth century.
 *
 * DIRECTIONALITY LOGIC:
 *   Movement leadership and reform-minded laity sit near the beneficiary end: they gain interpretive access and political legitimacy without bearing the cost of the authority transfer. Orthodox ritual authorities, literalist-martial traditions, and caste-hierarchy beneficiaries sit near the target end: each loses a scriptural warrant load-bearing for their social position, and their exit options are constrained because their authority is specifically tied to the literal or caste-duty reading being displaced — they cannot simply relocate to another textual basis without conceding the substantive point. Subjects of literal caste and war violence are declared victims of the STANDING arrangement (the literal/caste readings this reading contests), not victims of the allegorical reading itself; they are listed as excluded stakeholders because the interpretive contest that determines their treatment proceeds without their direct participation, per the ε-referent rule for kernel-reading stories.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling a scripture containing apparent literal-violence and caste-duty content with an emerging nonviolent political commitment) is authored as contested rather than dead, precisely because literalist and caste-warrant invocations of the text persist into the present, meaning the allegorical reading's coordinating function has not become vestigial even where its political urgency has receded. This prevents the tangled_rope classification from collapsing into either pure extraction (the reading is not merely a power grab against orthodox authorities — it enabled real, large-scale nonviolent coordination) or pure rope (the reading did impose real costs on displaced authorities and delegitimized traditions, and required active enforcement through sustained argument to hold against contestation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_vs_literal_textual_warrant,
    'Does the Gita''s own internal textual evidence (Krishna''s explicit theophany in chapter 11, the literal battlefield setting, the caste-duty language of chapter 18) support a purely allegorical reading, or does the allegorical reading require setting aside substantial portions of the text''s plain sense?',
    'Close philological and historical-critical analysis of the Sanskrit text against the interpretive traditions (Shankara''s advaitin commentary, Ramanuja''s theistic commentary, and colonial-era reception history) to establish whether allegorical reading is a recovered ancient strand or a modern strategic innovation.',
    'If the allegorical reading is shown to be a modern strategic construction with weak textual warrant, its extraction from displaced orthodox authority looks less like correcting a misreading and more like an innovation claiming ancient authority it does not textually possess — this would raise the effective extraction attributed to the reading''s claim to interpretive legitimacy. If it is shown to have deep roots in prior commentarial traditions, the reading''s claim to legitimacy strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_vs_literal_textual_warrant, conceptual, 'Whether the allegorical reading recovers or invents its textual warrant.').

omega_variable(
    movement_instrumentalization_vs_genuine_belief,
    'Was the allegorical reading adopted primarily as sincere theological conviction, or substantially as a strategic instrument to secure indigenous scriptural legitimacy for an already-formed political commitment to nonviolent resistance?',
    'Biographical and archival analysis of the reading''s principal authors'' private writings versus public political statements, and comparison with the timeline of political strategy development relative to theological development.',
    'If substantially instrumental, the coordination function is real but secondary to a political project, and the reading''s persistence past the movement''s active political phase would indicate theater/inertia rather than continued genuine coordination need. If substantially sincere, the coordination function is closer to the reading''s own self-understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_instrumentalization_vs_genuine_belief, empirical, 'Whether the reading''s origin is primarily strategic or primarily devotional/theological.').

omega_variable(
    committer_framing_kernel_location,
    'Where exactly is the interpretive disagreement located across the three kernel readings — is it a disagreement about what the text SAYS (semantic), about what authority may say what it means (interpretive-institutional), or about which social arrangements the text should be read as underwriting (political-consequentialist)?',
    'This is the committer-frame ambiguity routed here per Rule 2: a full resolution would require decomposing each reading''s disagreement point-by-point against the other two, which this single-reading story deliberately does not attempt (per Rule 1, ε-invariance).',
    'If the disagreement is primarily interpretive-institutional (who may authoritatively read), the allegorical reading''s extraction is concentrated on displaced authorities. If primarily political-consequentialist (what social order the reading licenses), the extraction is concentrated on caste-hierarchy and literalist-martial beneficiaries. The story as authored treats both as co-present victims, which is itself a substantive choice documented here rather than smuggled into the metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_kernel_location, conceptual, 'Locating where across semantic/institutional/political axes the kernel readings actually diverge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(gita_tr_t1917, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(gita_tr_t1934, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1934, 0.24).
narrative_ontology:measurement(gita_tr_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(gita_tr_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1975, 0.29).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2000, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(gita_be_t1917, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1917, 0.42).
narrative_ontology:measurement(gita_be_t1934, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1934, 0.5).
narrative_ontology:measurement(gita_be_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(gita_be_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1975, 0.57).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(gita_su_t1917, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1917, 0.5).
narrative_ontology:measurement(gita_su_t1934, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1934, 0.58).
narrative_ontology:measurement(gita_su_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(gita_su_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1975, 0.61).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gita_kurukshetra_discourse kernel: gandhian_allegorical_reading (this story, tangled_rope), orthodox_literal_reading (sibling, expected higher extraction concentrated on caste-subordinated and martial-conscripted populations), and universalist_devotional_reading (sibling, expected lower extraction, closer to rope, coordinating around devotional access rather than displacing institutional authority as sharply). Each carries its own ε assessed against the standing arrangement the reading contests, per the ε-referent rule for kernel-reading stories. Do not average or reconcile ε across the three; they are structurally distinct constraints sharing a textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
