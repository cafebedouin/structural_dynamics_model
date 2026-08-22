% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7_allegorical_displacement_reading, []).

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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Internal Spiritual Discipline (Allegorical Reading)
 *   domain: religious/hermeneutical/ethical
 *
 * SUMMARY:
 *   This constraint story instantiates the allegorical_displacement_reading
 *   of the herem command (Deuteronomy 7 and related passages). Under this
 *   reading, the biblical command to herem ('devote to destruction') is
 *   interpreted as a typological directive: the 'nations' Israel is commanded
 *   to destroy are typological placeholders for spiritual enemies (sin,
 *   temptation, moral compromise), and the conquest narrative encodes
 *   internal moral warfare rather than historical territorial conquest. This
 *   reading relocates extractiveness entirely from the interethnic/political
 *   domain to the internal psychological/spiritual domain of individual moral
 *   agents. The constraint's extractiveness is minimal (0.08) because no
 *   group is systematically extracted from; instead, beneficiary groups
 *   (individual moral agents, practitioner communities) gain a hermeneutical
 *   framework that preserves scriptural authority while disavowing ethnic
 *   violence. The suppression is low (0.12) because adherence to this reading
 *   is voluntary and mobile—agents can adopt or abandon the allegorical
 *   framework without material penalty. The theater ratio (0.22) reflects a
 *   modest performative component: maintaining the reading requires
 *   interpretive labor (teaching, commentary, justification) that defends the
 *   framework against literal historical challenge.
 *
 * KEY AGENTS:
 *   - Individual moral agents adopting allegorical reading (beneficiary, moderate power, mobile exit)
 *   - Spiritual practitioner and theological communities (beneficiary, organized power, mobile exit)
 *   - Literal historical reading proponents (excluded, organized power, constrained exit)
 *   - Indigenous descendant communities (observer, moderate power, mobile exit)
 *   - Secular critical scholarship (observer, institutional power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Internal Spiritual Discipline (Allegorical Reading)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/hermeneutical/ethical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2').
narrative_ontology:cs_kernel_codification('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', fixed_text).
narrative_ontology:cs_authority_grounding('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', lineage).
narrative_ontology:cs_interpretation_layer_present('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2').
narrative_ontology:cs_reading_relation('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', foundational, scriptural_typology_discloses_true_meaning).
narrative_ontology:cs_axiom_status(scriptural_typology_discloses_true_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', scriptural_typology_discloses_true_meaning, conventional).
narrative_ontology:cs_axiom('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', foundational, internal_moral_struggle_is_referent_of_conquest).
narrative_ontology:cs_axiom_status(internal_moral_struggle_is_referent_of_conquest, holdable).
narrative_ontology:cs_axiom_grounding('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', internal_moral_struggle_is_referent_of_conquest, deontological).
narrative_ontology:cs_reference_frame('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', scriptural_canonical_authority).
narrative_ontology:cs_drift_state('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', contemporary_secular_ethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7676fd9-4d7f-4fd0-908e-31ffaac7d6b2', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, individual_moral_agent).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioner_community).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_scriptural_interpretation).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, internalized_moral_warfare_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads herem as a directive to internal struggle: conquering sin (the 'nations' as temptations, vices, spiritual enemies within oneself). The reading frames the moral effort as voluntary spiritual discipline. The agent benefits from a coherent self-understanding that reframes potentially disturbing ancient warfare commands as a template for personal sanctification rather than as historical blueprints for ethnic conflict.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, individual_moral_agent, beneficiary,
    moderate, biographical, mobile, universal).

% Theologically-trained and layreading communities that adopt allegorical interpretation maintain a coherent narrative: the Bible's moral authority is preserved, ethnic violence is disavowed, and the text remains spiritually relevant to modern believers. The reading protects the canonical authority of scripture by relocating its moral content from the political/ethnic domain to the inner spiritual one.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioner_community, beneficiary,
    organized, generational, mobile, global).

% Scholars and theologians who argue herem records actual ancient warfare directives (durable_separation_reading) or historically-bounded commands (contextual_supersession_reading) are structurally absent from this reading's conversation. They would contest the typological move as eisegesis (reading meaning into the text rather than out of it) and argue it abandons fidelity to the text's apparent historical referent. Their exclusion from the reading's frame is structural.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literal_historical_reading_proponents, excluded,
    organized, generational, constrained, global).

% Communities whose ancestors inhabited the land described in herem narratives may hold complex relationships to interpretations—some find allegorical reading less harmful than literal, historical readings; others see both as forms of textual violence that obscure or reframe their own historical experiences. They are analytically external to the constraint itself but affected by its social consequences.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, indigenous_descendant_communities, observer,
    moderate, generational, mobile, regional).

% Academic biblical studies assesses the reading's historical and textual merit. Secular scholars typically locate herem in Iron Age settlement conflict and treat allegorical readings as hermeneutical moves rather than recoveries of authorial intent. They occupy an analytical position, neither benefiting nor bearing cost from the reading itself, but providing interpretive assessment.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, secular_critical_scholarship, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioner_community).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, ethically coherent reading strategy for communities that hold biblical authority while rejecting ethnic genocide as divinely mandated. Coordinates the preservation of scriptural canonical standing with modern ethical convictions against interethnic violence.
% TRANSFER_FUNCTION: Moves the referent of 'conquest' from interethnic/political domain (literal territorial settlement, expulsion of historic populations) to the internal psychological/spiritual domain (conquest of internal vices, sins, temptations within the believer's own soul). The 'nations' transfer from ethnic groups to typological placeholders for spiritual enemies.
% ABSENT_VOICES: Historians and archaeologists who document the material record of Iron Age settlement and conflict—they would argue the text refers to actual conquest, not metaphorical internal struggle. Literal historical reading proponents who treat herem as normative divine instruction for ethnic bounded membership would contest the allegorical displacement. Indigenous communities whose ancestors inhabited the contested territory may experience both literal and allegorical readings as forms of textual usurpation of their own historical agency.
% DISAPPEARANCE_RATIONALE: If this allegorical reading disappeared, believers adopting it would face a hermeneutical crisis: the text would appear to mandate ethnic violence, forcing either rejection of scriptural authority or acceptance of genocide as divinely mandated. The reading's disappearance would not alter the material text but would remove a cognitive buffer that allows believers to hold both biblical authority and ethical non-violence. The world does not rearrange materially, but the interpretive landscape for believers fundamentally shifts.
% FOUNDING_PROBLEM: The problem of reconciling scriptural authority (the Bible's canonical status in religious communities) with modern ethical convictions against genocide and ethnic cleansing. How can a community treat ancient herem commands as divinely inspired while rejecting their apparent historical referent (the annihilation or expulsion of ethnic populations)?
% FOUNDING_PROBLEM_CORROBORATION: Theological and biblical studies scholarship outside the allegorical reading tradition attests the problem is live: mainstream Christian and Jewish commentaries document the recurring tension between treating herem as morally binding (leading to ethnic-cleansing interpretations) and treating it as historically irrelevant (leading to scriptural authority erosion). Philosophers of religion, ethicists, and comparative theology scholars external to any single reading attest that modern believers do experience this hermeneutical pressure.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near-zero (0.08) because the reading generates no systematic asymmetry of benefit and cost: individual agents voluntarily adopt the allegorical framework to resolve a cognitive tension between scriptural authority and modern ethics. No beneficiary class extracts from a victim class; both beneficiary classes (individual agents and practitioner communities) gain the same hermeneutical benefit. Suppression (0.12) is low because the reading's persistence depends on voluntary intellectual adoption, not coercion—a reader who finds the allegorical move unconvincing can abandon it and adopt a different reading without facing material barriers (unlike economic or political constraints where exit is materially costly). Theater (0.22) is modest because while the reading requires interpretive work (commentary, teaching, theological justification), that work is not primarily performative—the constraint's function is genuinely to coordinate a hermeneutical solution, not to maintain appearance. The accessibility_collapse (0.31) is moderate-low: the allegorical reading is intelligible and available to any educated reader, but literal historical readings remain equally accessible alternatives; moving between them is cognitively possible without material barrier. The resistance (0.58) is moderate-to-high because the reading encounters substantial push from literal historical scholars, fundamentalist communities, and critics who argue the allegorical move abandons textual fidelity and imposes modern ethical categories on ancient religious texts.
 *
 * PERSPECTIVAL GAP:
 *   See above under 'perspectival_gap'.
 *
 * DIRECTIONALITY LOGIC:
 *   See above under 'directionality_logic'.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not exhibit mandatrophy in the classical sense (a founding problem that has died while the constraint persists). The founding problem—reconciling scriptural authority with modern ethical convictions against genocide—remains live. However, a weaker mandatrophy symptom is present: the allegorical reading's authority depends partly on rejecting alternative readings (the literal, contextual-supersession readings) as inadequate or dangerous. If a beneficiary community came to accept that scriptural authority itself was the problem (rather than the interpretation of scripture), the entire allegorical reading infrastructure would collapse. The constraint persists not because the founding problem is genuinely solved but because the beneficiary communities maintain the hermeneutical framework as an article of faith. This is not classical mandatrophy but a fragile coordination that depends on continuous reaffirmation of the canonical status of scripture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    typological_vs_historical_referent,
    'Does the herem text''s apparent historical referent (actual conquest of Canaanite populations) constitute the ''true'' meaning, or is typological interpretation (spiritual enemies as vices) a legitimate reading that displaces the historical referent without denying it?',
    'Textual and historical analysis: does the text contain internal signals of typological intent (metaphorical language, genealogical implausibility, theological framings)? Do ancient Jewish and Christian interpreters adopt typological readings, or is allegory a modern innovation? What standard of ''legitimate meaning'' is applied—authorial intent, community tradition, contemporary ethical coherence?',
    'If typological reading is legitimate, the allegorical displacement stands as a coherent reading that preserves scriptural authority while disavowing ethnic violence. If typological reading is dismissed as eisegesis, the constraint collapses because its core move is invalidated—the text would refer straightforwardly to ethnic conquest, and the allegorical framework would be recognized as an overlay rather than a recovery of meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typological_vs_historical_referent, conceptual, 'Whether typological/allegorical interpretation is a defensible reading strategy or a hermeneutical illusion imposed on the text.').

omega_variable(
    scriptural_authority_stability,
    'How fragile is the allegorical reading''s dependence on the presupposition that scripture holds canonical authority for believers? If that presupposition erodes, does the reading collapse entirely?',
    'Long-term measurement of scriptural authority in Western Christian and Jewish communities. If authority declines (fewer believers treating the Bible as divinely inspired), does the allegorical reading persist as an interpretive tradition, or does it become a merely historical artifact?',
    'If scriptural authority is essential to the reading''s coherence, the constraint is fragile—its persistence depends on a broader cultural commitment that the reading itself does not maintain but presupposes. If the reading can survive loss of scriptural authority (becoming a humanistic or literary interpretation), it is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_authority_stability, empirical, 'Whether the allegorical reading is self-sustaining or dependent on broader scriptural-authority commitments.').

omega_variable(
    literal_reading_suppression,
    'Does the rise of allegorical reading in mainstream theology actively suppress literal historical reading, or does it merely offer an alternative that readers can accept or reject? Is the constraint''s low suppression metric accurate, or does it undercount institutional pressure on literal readings?',
    'Survey of theological education and commentary production: what proportion of seminaries teach allegorical reading as standard? What proportion of major biblical commentaries adopt typological interpretation? Do literal readers face social or professional barriers in academic theological discourse?',
    'If institutional pressure on literal readings is substantial, the constraint''s suppression metric understates the cost of non-adoption. The reading would exhibit higher suppression than authored (0.12), and the classification could shift from rope toward tangled_rope or snare if institutional barriers are severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_reading_suppression, empirical, 'Whether the allegorical reading''s institutional success involves suppression of alternative readings.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the allegorical reading logically foreclose the durable_separation and contextual_supersession readings, or do all three readings remain live options held by different communities? Can a framework coherently hold all three, or are they genuinely incompatible?',
    'Logical analysis: if the allegorical reading is true (herem refers to internal vices, not ethnic groups), does that make the literal historical reading false? Or is it possible that herem refers to both internal vices (typologically) and historical conquest (historically) simultaneously? Do any communities hold hybrid readings that combine elements of multiple approaches?',
    'If the readings are mutually exclusive, the kernel is genuinely contested and no single authority can resolve it. If hybrid readings are possible, the kernel''s structure is more fluid than the three-reading decomposition suggests, and the allegorical reading does not mark a clean break from its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the three kernel readings logically foreclose each other or remain compatible alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(here_tr_t10, observed).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(here_tr_t20, observed).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(here_tr_t30, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(here_tr_t40, observed).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(here_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement_basis(here_be_t10, observed).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(here_be_t20, observed).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(here_be_t30, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(here_be_t40, observed).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(here_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(here_su_t10, observed).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(here_su_t20, observed).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(here_su_t30, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(here_su_t40, observed).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(here_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.05).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the herem command kernel. The durable_separation_reading interprets herem as timeless divine mandate for identity preservation through bounded membership; the contextual_supersession_reading treats herem as historically bounded and morally superseded. This allegorical_displacement_reading relocates the victim set to abstract vices and the conquest domain to internal spiritual struggle. All three readings contest the same textual kernel; their structural data (beneficiary/victim, extractiveness, suppression) differ substantially because they make different claims about what herem refers to and who benefits from its interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
