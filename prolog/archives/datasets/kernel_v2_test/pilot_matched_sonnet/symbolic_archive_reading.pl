% ============================================================================
% CONSTRAINT STORY: symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbolic_archive_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (Cultural Preservation Reading)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The symbolic archive reading treats sacrifice law (korbanot) as a
 *   cultural-historical repository rather than a binding halakhic obligation.
 *   This reading emerged prominently in 19th-century Reform Judaism but has
 *   roots in earlier diaspora rationalization of Temple ritual's suspension.
 *   The constraint coordinates voluntary study of sacrifice texts to preserve
 *   collective memory and textual literacy without claiming that the laws
 *   impose current obligations. No victim set exists because no one is bound
 *   by an unperformable duty; beneficiaries are the Jewish collective memory,
 *   contemporary Jewish identity (especially non-Orthodox movements), and
 *   historical scholarship. The reading solves a genuine coordination
 *   problem: how to maintain 2000+ years of complex legal-ritual knowledge
 *   when the ritual itself cannot be performed and many Jews reject the
 *   theological premise that it ever will be again. Extractiveness is
 *   negligible (~0.02) because participation is voluntary and intrinsically
 *   motivated; suppression is minimal (~0.05) because exit is costless — one
 *   can simply not study sacrifice texts without social or institutional
 *   penalty in communities holding this reading. Theater ratio is very low
 *   (~0.08) because the study activity is functionally what it claims to be:
 *   historical-cultural engagement with texts, not performative compliance
 *   with a binding law.
 *
 * KEY AGENTS:
 *   - Individual Scholar: Moderate power, mobile exit — voluntary engagement with sacrifice texts as cultural material; no obligation, no penalty for non-study
 *   - Educational Institution: Organized power, mobile exit — yeshivot and Jewish studies programs coordinate curriculum around sacrifice texts as historical archive without enforcement
 *   - Reform/Reconstructionist Movement: Institutional power, arbitrage exit — primary beneficiary; this reading aligns with non-Orthodox theology and enables textual engagement without theological crisis
 *   - Jewish Collective Memory: Powerless (abstract collective good), trapped exit — benefits from preservation of textual tradition but has no agency; not a victim because no extraction occurs
 *   - Historical Scholarship Community: Moderate power, mobile exit — benefits from access to living tradition of sacrifice text study; voluntary participation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbolic_archive_reading, 0.02).
domain_priors:suppression_score(symbolic_archive_reading, 0.05).
domain_priors:theater_ratio(symbolic_archive_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbolic_archive_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(symbolic_archive_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(symbolic_archive_reading, theater_ratio, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbolic_archive_reading, rope).
narrative_ontology:human_readable(symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (Cultural Preservation Reading)").
narrative_ontology:topic_domain(symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbolic_archive_reading, '2418609c-b092-4948-8aa6-c559c20b6cf1').
narrative_ontology:cs_kernel_codification('2418609c-b092-4948-8aa6-c559c20b6cf1', fixed_text).
narrative_ontology:cs_authority_grounding('2418609c-b092-4948-8aa6-c559c20b6cf1', distributed).
narrative_ontology:cs_reading_relation('2418609c-b092-4948-8aa6-c559c20b6cf1', symbolic_archive_reading__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('2418609c-b092-4948-8aa6-c559c20b6cf1', symbolic_archive_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('2418609c-b092-4948-8aa6-c559c20b6cf1', symbolic_archive_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('2418609c-b092-4948-8aa6-c559c20b6cf1', foundational, sacrifice_law_non_binding_post_temple).
narrative_ontology:cs_axiom_status(sacrifice_law_non_binding_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('2418609c-b092-4948-8aa6-c559c20b6cf1', sacrifice_law_non_binding_post_temple, conventional).
narrative_ontology:cs_axiom('2418609c-b092-4948-8aa6-c559c20b6cf1', foundational, textual_preservation_intrinsic_value).
narrative_ontology:cs_axiom_status(textual_preservation_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('2418609c-b092-4948-8aa6-c559c20b6cf1', textual_preservation_intrinsic_value, deontological).
narrative_ontology:cs_reference_frame('2418609c-b092-4948-8aa6-c559c20b6cf1', cultural_heritage_framework).
narrative_ontology:cs_drift_state('2418609c-b092-4948-8aa6-c559c20b6cf1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2418609c-b092-4948-8aa6-c559c20b6cf1', '').
narrative_ontology:cs_kernel_id(symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, contemporary_jewish_identity).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, historical_scholarship_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, individual_scholar).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, reform_reconstructionist_movement).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, cultural_continuity_through_study).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, non_coercive_identity_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages sacrifice texts voluntarily as cultural-historical material. No obligation, no penalty for non-study. Benefits from access to textual tradition and shared literacy with community. Exit is costless — can stop studying at any time without social or institutional consequence.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, individual_scholar, beneficiary,
    moderate, biographical, mobile, local).

% Yeshivot and Jewish studies programs coordinate curriculum around sacrifice texts as historical archive. Sets agenda for what texts are studied and how they are framed (cultural vs halakhic). No enforcement mechanism — students choose engagement level. Benefits from coordinating transmission of complex textual tradition across generations.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, educational_institution, agenda_setter,
    organized, generational, mobile, national).

% Primary institutional beneficiary. This reading aligns with non-Orthodox theology: sacrifice study as cultural heritage rather than halakhic obligation. Arbitrage exit: movement can engage or disengage with sacrifice texts based on pedagogical value without theological crisis. Preserves textual tradition while releasing binding claim.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, reform_reconstructionist_movement, beneficiary,
    institutional, generational, arbitrage, global).

% Abstract collective good. Benefits from preservation of 2000+ years of textual tradition and ritual knowledge. Has no agency — cannot choose to study or not study. Not a victim because no extraction occurs; the constraint preserves rather than depletes this collective resource.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, jewish_collective_memory, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(symbolic_archive_reading, jewish_collective_memory).

% Academic scholars of Jewish history and rabbinic literature benefit from living tradition of sacrifice text study. Access to communities that maintain textual literacy enables historical research. Participation is voluntary — scholars can engage or not based on research needs.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, historical_scholarship_community, beneficiary,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complex legal-ritual textual tradition across 2000+ years of diaspora when the ritual itself is suspended. Coordinates voluntary study to maintain collective memory and shared textual literacy without claiming binding obligation.
% TRANSFER_FUNCTION: Minimal transfer. Educational institutions allocate curriculum time and resources to sacrifice text study; students allocate attention. No money, no coercion, no status extraction. The 'transfer' is voluntary participation in cultural transmission.
% ABSENT_VOICES: Orthodox communities holding study_as_exercise or performance_only readings are not absent but hold different readings. No systematically excluded voice — the reading is one option in a pluralistic landscape. The 'absent voice' would be Jews who want to study sacrifice texts but are prevented from doing so, but no such group exists under this reading (study is voluntary and accessible).
% DISAPPEARANCE_RATIONALE: If this reading disappeared, communities holding it would face a choice: adopt a sibling reading (study_as_exercise, performance_only, messianic_suspension) or abandon sacrifice text study entirely. The first option reintroduces obligation and raises epsilon; the second option loses textual tradition. The constraint's disappearance would rearrange how non-Orthodox Judaism relates to sacrifice law — either reintroducing binding claims or severing connection to a major textual corpus.
% FOUNDING_PROBLEM: Post-Temple Judaism faced a structural problem: how to preserve detailed knowledge of sacrifice law when the Temple was destroyed and sacrifice could not be performed. Early rabbinic responses (study as substitute, messianic deferral) maintained binding obligation. The symbolic archive reading emerged later (prominently in 19th-century Reform) to solve a different problem: how to preserve textual tradition for Jews who reject the theological premise that sacrifice will ever resume.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preserve sacrifice knowledge in diaspora) remains live for communities that value textual tradition but reject binding obligation. Corroboration: Reform and Reconstructionist liturgy removes sacrifice restoration prayers while retaining historical references; Jewish studies curricula in secular universities teach sacrifice law as cultural history; ethnographic studies of non-Orthodox communities show continued engagement with sacrifice texts framed as heritage rather than halakha. The problem is live because the tension between preservation and non-obligation persists.
narrative_ontology:disappearance_verdict(symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(symbolic_archive_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SCHOLAR (ROPE) — Voluntary engagement with sacrifice texts as cultural-historical material. No obligation, no penalty for non-study. Coordination function: shared textual literacy enables collective memory transmission. Zero extraction — participation is elective and intrinsically motivated.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (ROPE) — Yeshivot and Jewish studies programs coordinate curriculum around sacrifice texts as historical archive. No enforcement mechanism — students choose engagement level. Coordination solves genuine problem: how to transmit complex legal-historical material across generations without claiming binding authority.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM/RECONSTRUCTIONIST MOVEMENT (ROPE) — Institutional beneficiary of this reading. Sacrifice study as cultural heritage rather than halakhic obligation aligns with non-Orthodox theology. Arbitrage exit: movement can engage or disengage with sacrifice texts based on pedagogical value without theological crisis. Pure coordination — preserves textual tradition while releasing binding claim.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From civilizational perspective, this reading solves a real coordination problem: how does a diaspora community preserve complex legal-ritual knowledge when the ritual itself is suspended for 2000+ years? The symbolic archive reading coordinates voluntary study without coercion. No extraction detected — the constraint operates as pure information standard with negligible overhead.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbolic_archive_reading_tests).
:- end_tests(symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.02): Near-zero. The symbolic archive reading imposes no obligation and extracts nothing from participants. The minimal non-zero value reflects only the coordination overhead of maintaining curriculum and textual transmission infrastructure — the same overhead any voluntary educational tradition carries. No one is bound, no one is penalized for non-participation, no one collects rents from the constraint's operation. Suppression (0.05): Negligible. Exit is costless in communities holding this reading. A Jew who chooses not to study sacrifice texts faces no social sanction, no institutional penalty, no identity cost. The minimal non-zero value reflects only that some educational institutions include sacrifice texts in standard curriculum, creating mild path-dependency (easier to study if the curriculum is already structured around it), but this is not coercive. Theater ratio (0.08): Very low. The study activity is functionally what it claims to be: engagement with texts as cultural-historical material. There is no performative gap between claimed purpose (preserve knowledge, transmit tradition) and actual function (preserve knowledge, transmit tradition). The minimal non-zero value reflects only that some study occurs in formal educational settings with ritual framing (blessings before study, etc.), but this ritual framing is transparent and does not mask extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as Rope. The individual scholar, the educational institution, the Reform movement, and the analytical observer all see the same structure: voluntary coordination around textual preservation with negligible extraction and negligible suppression. The uniformity is diagnostic: when a constraint solves a genuine coordination problem without imposing costs on non-participants, all perspectives converge on Rope. The contrast with sibling readings is stark: if study_as_exercise_reading introduces obligation (study is a mitzvah with binding force), epsilon rises and victim sets appear (those obligated but unable to study). If performance_only_reading claims sacrifice laws remain binding but suspended, suppression rises (obligation exists but cannot be fulfilled). The symbolic archive reading avoids both traps by releasing the binding claim entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries or neutral. No victim set exists because the symbolic archive reading makes no binding claim — there is no obligation to violate, no duty to bear. The Reform/Reconstructionist movement is the primary institutional beneficiary (d ≈ 0.0): this reading enables them to engage sacrifice texts without theological crisis, preserving textual tradition while releasing the binding claim. Individual scholars and educational institutions are also beneficiaries (d ≈ 0.1-0.2): they gain access to a rich textual tradition and coordinate its transmission, bearing only the minimal overhead of voluntary study. Jewish collective memory is a beneficiary (d ≈ 0.0) but is an abstract collective good, not an agent with directionality in the standard sense. The constraint operates as pure coordination with negligible extraction — a genuine Rope from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The symbolic archive reading resolves mandatrophy by eliminating the mandate. There is no binding obligation, so there is no gap between mandate and function. The constraint's purpose (preserve textual tradition, coordinate collective memory) is exactly what it does, and participation is voluntary. This is the opposite of mandatrophy: the constraint's function has not outlived its justification because the justification (cultural preservation) is intrinsic to the function (study as preservation). The reading does not claim that sacrifice laws are binding-but-suspended or binding-but-unperformable; it claims they are not binding at all, only historically significant. This eliminates the structural tension that produces mandatrophy in other readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the sacrifice_obligation_kernel one constraint with multiple readings, or are the readings structurally distinct constraints that happen to reference the same texts?',
    'Epsilon-invariance test: if changing from symbolic_archive to study_as_exercise or performance_only changes epsilon substantially, they are different constraints. If epsilon remains stable across readings, they are perspectives on one constraint.',
    'If distinct constraints: network decomposition required, each reading gets separate constraint_id. If one constraint: perspectival classification applies, readings are observer-indexed views.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether kernel readings are distinct constraints or indexed perspectives').

omega_variable(
    sibling_reading_structural_delta,
    'What structural elements change when moving from symbolic_archive_reading to study_as_exercise_reading or performance_only_reading?',
    'Comparative analysis: Does study_as_exercise_reading introduce obligation (raising epsilon)? Does performance_only_reading introduce victim set (those obligated but unable to perform)? Does messianic_suspension_reading introduce temporal conditionality that changes beneficiary structure?',
    'If study_as_exercise introduces obligation: epsilon rises from ~0.02 to ~0.25-0.40 (tangled_rope territory). If performance_only creates victim set: classification shifts to snare for trapped agents. If messianic_suspension is purely temporal: epsilon may remain low but suppression rises (obligation exists but is suspended).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'Structural differences between sibling readings of sacrifice kernel').

omega_variable(
    cultural_preservation_vs_halakhic_claim,
    'Does treating sacrifice law as symbolic archive actually eliminate halakhic obligation, or does it reframe obligation as cultural rather than ritual?',
    'Ethnographic study of communities holding this reading: Do members experience social pressure to study sacrifice texts? Is non-study sanctioned informally? If yes, the ''voluntary'' framing masks soft coercion and epsilon is higher than claimed.',
    'If genuinely voluntary: rope classification confirmed, epsilon ~0.02. If soft coercion exists: tangled_rope, epsilon rises to ~0.20-0.30. The distinction matters for whether this reading actually resolves the obligation or merely displaces it from ritual to cultural domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_preservation_vs_halakhic_claim, empirical, 'Whether symbolic archive reading eliminates or displaces obligation').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''sacrifice law texts'' (the written corpus) or ''sacrifice obligation'' (the normative claim the texts encode)? Different framings produce different cs_pattern classifications.',
    'If kernel = texts: authority_grounding is lineage (chain of textual transmission), interpretation_layer_present = true (Talmudic/rabbinic commentary absorbs drift). If kernel = obligation: authority_grounding is extraction or practice (who benefits from maintaining the obligation claim), interpretation_layer may be absent (obligation is direct, not mediated). The symbolic_archive reading treats kernel as texts; sibling readings may treat it as obligation.',
    'Framing choice determines whether this is a fixed_text CS (texts are stable, interpretation varies) or a distributed CS (obligation claim is contested, no single authority adjudicates). Affects cs_structure.kernel_codification and authority_grounding values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Kernel identity ambiguity: texts vs obligation claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbolic_archive_reading, 0, 186).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1840, symbolic_archive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(theater_1890, symbolic_archive_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(theater_1940, symbolic_archive_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement(theater_1990, symbolic_archive_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(theater_2026, symbolic_archive_reading, theater_ratio, 186, 0.08).

% Extraction over time
narrative_ontology:measurement(extract_1840, symbolic_archive_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(extract_1890, symbolic_archive_reading, base_extractiveness, 50, 0.02).
narrative_ontology:measurement(extract_1940, symbolic_archive_reading, base_extractiveness, 100, 0.02).
narrative_ontology:measurement(extract_1990, symbolic_archive_reading, base_extractiveness, 150, 0.02).
narrative_ontology:measurement(extract_2026, symbolic_archive_reading, base_extractiveness, 186, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbolic_archive_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel may decompose into multiple constraints rather than multiple readings of one constraint, pending epsilon-invariance test. If study_as_exercise_reading has substantially higher epsilon (obligation introduces extraction), it should be a separate constraint_id linked via network.affects_constraints. If epsilon remains stable across readings (~0.02-0.05), they are perspectival views of one constraint and should remain as readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
