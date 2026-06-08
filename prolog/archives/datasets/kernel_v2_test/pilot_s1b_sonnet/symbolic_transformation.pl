% ============================================================================
% CONSTRAINT STORY: symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbolic_transformation, []).

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
 *   constraint_id: symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   The destruction of the Second Temple in 70 CE created a crisis for Jewish
 *   covenantal practice: the divine commandments for animal sacrifice could
 *   no longer be materially performed. The rabbinic response was not to
 *   suspend practice pending restoration, but to authorize transformation:
 *   prayer (tefillah) became 'service of the heart' replacing 'service of the
 *   altar,' and Torah study was declared equivalent to offering sacrifices.
 *   This constraint story examines the SYMBOLIC TRANSFORMATION reading of
 *   this event — the claim that prayer and study are not temporary
 *   substitutes but the new, authorized instantiation of the sacrifice
 *   commitment itself. This reading faces three sibling alternatives: (1)
 *   STUDY AS EXERCISE: prayer and study are preparatory practice for future
 *   material sacrifice, not substitutes; (2) PERFORMANCE ONLY: the material
 *   commandment is dormant but unchanged, awaiting restoration; (3) HYBRID
 *   PREPARATORY: study has independent value but sacrifice remains the
 *   primary obligation when possible. The symbolic transformation reading
 *   consolidates rabbinic interpretive authority while creating victims among
 *   those who hold material performance as non-negotiable. The constraint's
 *   moderate theater ratio (0.35) reflects that the transformation was not
 *   purely performative — prayer and study have real spiritual and communal
 *   function — but also that the claim of equivalence to animal sacrifice
 *   carries performative elements (the original divine language was material,
 *   and the symbolic equation requires interpretive work to sustain).
 *
 * KEY AGENTS:
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — captures authority to redefine divine command; consolidates power through adaptive response to crisis
 *   - Post-Destruction Jewish Communities: Secondary beneficiary (organized/constrained) — gain actionable practice in Temple's absence; benefit from diasporic portability of prayer/study
 *   - Materially Committed Practitioners: Primary victim (powerless/identity_locked) — identity fused with original material command; experience transformation as unauthorized extraction
 *   - Priestly Lineage (Kohanim): Mixed victim (moderate/constrained) — lose functional role but retain symbolic status markers; constrained exit due to inherited identity
 *   - Messianic Restoration Movement: Organized constituency (organized/constrained) — frames transformation as temporary; maintains agency through eschatological sunset claim
 *   - Analytical Observer: Sees tangled rope structure (analytical/analytical) — genuine coordination (diaspora survival) layered with asymmetric extraction (authority capture, victim creation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbolic_transformation, 0.62).
domain_priors:suppression_score(symbolic_transformation, 0.68).
domain_priors:theater_ratio(symbolic_transformation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbolic_transformation, extractiveness, 0.62).
narrative_ontology:constraint_metric(symbolic_transformation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(symbolic_transformation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(symbolic_transformation, "religious_law/halakhic_tradition/commitment_system").

domain_priors:requires_active_enforcement(symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbolic_transformation, 'e23a6e1d-3d6a-4001-b4ca-b8396db4ef05').
narrative_ontology:cs_kernel_codification('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', fixed_text).
narrative_ontology:cs_authority_grounding('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', lineage).
narrative_ontology:cs_interpretation_layer_present('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05').
narrative_ontology:cs_reading_relation('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', symbolic_transformation__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', symbolic_transformation__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', symbolic_transformation__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', foundational, prayer_study_equivalence_to_sacrifice).
narrative_ontology:cs_axiom_status(prayer_study_equivalence_to_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', prayer_study_equivalence_to_sacrifice, deontological).
narrative_ontology:cs_axiom('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', foundational, rabbinic_transformative_authority).
narrative_ontology:cs_axiom_status(rabbinic_transformative_authority, holdable).
narrative_ontology:cs_axiom_grounding('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', rabbinic_transformative_authority, conventional).
narrative_ontology:cs_axiom('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', secondary, material_form_secondary_to_intent).
narrative_ontology:cs_axiom_status(material_form_secondary_to_intent, holdable).
narrative_ontology:cs_axiom_grounding('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', material_form_secondary_to_intent, deontological).
narrative_ontology:cs_reference_frame('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', temple_service_material_performance).
narrative_ontology:cs_drift_state('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', post_second_temple_destruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e23a6e1d-3d6a-4001-b4ca-b8396db4ef05', '').
narrative_ontology:cs_kernel_id(symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbolic_transformation, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(symbolic_transformation, post_destruction_jewish_communities).
narrative_ontology:constraint_victim(symbolic_transformation, materially_committed_practitioners).
narrative_ontology:constraint_victim(symbolic_transformation, priestly_lineage_status).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATERIALLY COMMITTED PRACTITIONER (SNARE) — Identity fused with the original divine command for animal sacrifice. Views symbolic transformation as unauthorized drift that extracts obedience to human authority while claiming divine warrant. Experiences maximum extraction: the authority structure tells them their core commitment has been redefined, and dissent means expulsion from the community. Identity-locked because exit would require abandoning not just practice but the foundational premise that divine commands are materially binding and non-negotiable.
constraint_indexing:constraint_classification(symbolic_transformation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC STUDENT (TANGLED ROPE) — Constrained by educational investment and community embeddedness, but also benefits from access to the transformed tradition's intellectual structure. Experiences both coordination (prayer and study provide actionable spiritual practice in the Temple's absence) and extraction (must accept rabbinic redefinition of divine command or lose community standing). Moderate extraction because the agent has some mobility and gains real coordination value.
constraint_indexing:constraint_classification(symbolic_transformation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Primary beneficiary. Experiences the transformation as legitimate coordination: the destruction of the Temple created a crisis requiring adaptive authority, and the symbolic transformation preserved Jewish practice across diaspora. The authority structure sees itself as solving a genuine coordination problem (how to maintain covenant relationship without sacrificial infrastructure) while also consolidating institutional power. Low effective extraction because this agent captures the benefit flow.
constraint_indexing:constraint_classification(symbolic_transformation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MESSIANIC RESTORATION MOVEMENT (SCAFFOLD) — Organized groups holding that prayer and study are temporary measures pending Temple restoration. Sees the transformation as transitional coordination with an explicit sunset: when the Third Temple is built, material sacrifice will resume. Moderate extraction because the movement maintains agency through its eschatological framing, but constrained by current institutional dominance of the symbolic reading.
constraint_indexing:constraint_classification(symbolic_transformation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PRIESTLY LINEAGE (TANGLED ROPE) — Mixed experience. Benefits from preserved ritual status markers (aliyah priority, pidyon haben) and genealogical identity, but also bears the cost of functional obsolescence. The symbolic transformation coordinates memory of priestly role while extracting the material practice that originally constituted that role. Constrained exit because priestly status is inherited and identity-constituting, but also because the transformation preserves enough symbolic capital to make complete abandonment costly.
constraint_indexing:constraint_classification(symbolic_transformation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, this is a hybrid structure. Genuine coordination function: the transformation solved a real collective-action problem (maintaining Jewish practice after 70 CE without Temple infrastructure). Asymmetric extraction: the rabbinic authority structure captured interpretive power and redefined divine obligation, creating victims among those who held material performance as non-negotiable. The transformation is not a mountain (it required sustained institutional enforcement and suppression of dissent) and not pure rope (identifiable victims exist). Tangled rope is the structural classification.
constraint_indexing:constraint_classification(symbolic_transformation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbolic_transformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbolic_transformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbolic_transformation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The symbolic transformation solves a genuine coordination problem (maintaining covenant practice without Temple infrastructure) but also consolidates rabbinic authority to redefine divine obligation. The extraction is substantial because it operates on the highest-stakes domain (divine command) and creates victims whose identity is constituted through material obedience. However, extraction is not maximal because the transformation also provides real coordination value — post-destruction communities gained portable, sustainable practice. The value rose from 0.45 (immediate post-destruction period, when transformation was emergency response) to 0.62 (medieval stabilization, when transformation became entrenched institutional claim). Suppression (0.68): High. Dissent from the symbolic transformation is costly: rejection of rabbinic authority means exclusion from Jewish communal life, loss of access to ritual infrastructure (Torah scrolls, marriage/burial services, educational institutions), and marginalization as heretic. Suppression increased sharply during the Mishnaic/Talmudic period (0.50 → 0.70) as the transformation was codified and alternatives were systematically delegitimized, then stabilized at 0.68 as the rabbinic consensus became normative. Theater ratio (0.35): Moderate-low. The transformation has real functional content — prayer and study structure daily practice, transmit tradition, and build community. But the claim of equivalence to animal sacrifice is partly performative: the original commandment was material and specific, and declaring study 'as if you offered a sacrifice' requires sustained interpretive performance to maintain the equation. Theater has increased modestly over the interval (0.20 → 0.35) as the living memory of actual sacrifice faded and the symbolic equation became more abstract.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled rope structure from the analytical position: genuine coordination (the transformation preserved Jewish practice across two millennia of diaspora) layered with asymmetric extraction (rabbinic authority captured interpretive power and redefined obligation, creating victims among materially committed practitioners). The rabbinic authority sees pure coordination (Rope) — they solved a crisis. The materially committed practitioner sees pure extraction (Snare) — their core commitment was redefined without their consent, and dissent is suppressed. The priestly lineage and rabbinic students see the mixed structure (Tangled Rope) — they gain coordination value but also bear extraction. The messianic restoration movement sees a scaffold — temporary coordination with a sunset. The perspectival gap is diagnostic: the beneficiary's rope experience and the victim's snare experience both accurately reflect the same structural data, viewed from different positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic interpretive authority is the primary beneficiary — they captured authority to redefine divine command, consolidated institutional power, and gained control over Jewish practice for two millennia. Their perspective derives low directionality (d → 0.0-0.2 range) from beneficiary status + arbitrage exit, producing low or negative effective extraction (χ). Post-destruction Jewish communities are secondary beneficiaries — they gained actionable practice and diasporic portability — but with less mobility than the authority structure itself. Materially committed practitioners are primary victims — the transformation extracts obedience while claiming divine warrant, and their identity-locked exit (leaving would mean abandoning the foundational premise that divine commands are materially binding) produces maximum directionality (d → 0.9-1.0 range) and maximum effective extraction (χ). The priestly lineage is a mixed case — victim of functional obsolescence but beneficiary of preserved status markers — producing moderate directionality. The messianic restoration movement's organized power and eschatological framing (the transformation is temporary) provides them agency and moderate directionality despite their disagreement with the symbolic reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The symbolic transformation reading resolves its potential mandatrophy by maintaining that the original mandate (covenant relationship through sacrifice) has been FULFILLED through transformation, not suspended or violated. The rabbinic claim is that the Temple's destruction was not an obstacle but a divinely orchestrated transition — the material form was always secondary to the covenantal relationship, and prayer/study actualize that relationship more directly. However, this resolution depends entirely on accepting rabbinic authority to make that determination. For practitioners who hold material performance as non-negotiable, the mandatrophy is UNRESOLVED: the transformation is unauthorized drift, and the original mandate remains unfulfilled. The constraint thus instantiates a mandatrophy contest: whether adaptive authority can legitimately transform divine command, or whether such transformation is by definition extractive capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this reading (symbolic transformation) the only defensible framing of the temple sacrifice commitment kernel, or do the sibling readings (study as preparatory exercise, performance-only dormancy, hybrid preparatory-symbolic) represent equally coherent positions within halakhic tradition?',
    'Historical analysis of rabbinic texts, responsa literature, and contemporary halakhic disputes about sacrifice; comparison of textual warrants and institutional backing for each reading.',
    'If symbolic transformation is the only coherent reading: extraction is coordination cost (all alternatives are foreclosed by the tradition''s own logic). If siblings remain live: extraction is institutional capture (one reading among several claimed exclusive authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading forecloses or coexists with sibling readings of the sacrifice kernel').

omega_variable(
    authorization_source,
    'What is the source of rabbinic authority to transform a divine command? Is it derived from the original covenant (making transformation authorized), or is it a post-destruction institutional innovation (making transformation extractive drift)?',
    'Textual analysis of claimed warrants (oral Torah doctrine, rabbinic legislative power, precedent in prophetic reinterpretation); comparison to other instances of halakhic transformation; examination of dissent suppression mechanisms.',
    'If authority is covenantally grounded: transformation is coordination. If authority is institutional innovation: transformation is extraction disguised as religious necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorization_source, conceptual, 'Whether rabbinic authority to transform sacrifice commitment is itself authorized or captured').

omega_variable(
    material_necessity_threshold,
    'Does divine command contain an irreducible material component, or is symbolic performance sufficient when material performance is impossible? At what threshold of impossibility does symbolic substitution become legitimate?',
    'Cross-tradition comparison (Christian Eucharist transformation, Islamic hajj substitution rules, Hindu puja adaptations); philosophical analysis of divine command theory and material vs intentional worship.',
    'If material component is negotiable: symbolic transformation is coordination. If material component is non-negotiable: transformation is extraction from materially committed practitioners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(material_necessity_threshold, preference, 'Whether divine commands permit symbolic transformation under impossibility conditions').

omega_variable(
    victim_count_ambiguity,
    'How many practitioners in the post-destruction period genuinely held material sacrifice as non-negotiable versus accepted symbolic transformation? Were dissenters a substantial minority or fringe outliers?',
    'Historical demographic analysis; examination of suppressed or marginal traditions (Karaite rejection of rabbinic transformation, Samaritan preservation of animal sacrifice, Christian-Jewish polemics about sacrifice).',
    'If dissenters were substantial: high victim count increases extractiveness. If dissenters were minimal: low victim count suggests genuine coordination consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_count_ambiguity, empirical, 'Whether victim set is substantial or marginal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_70ce, symbolic_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(theater_mishnaic, symbolic_transformation, theater_ratio, 200, 0.25).
narrative_ontology:measurement(theater_talmudic, symbolic_transformation, theater_ratio, 500, 0.3).
narrative_ontology:measurement(theater_medieval, symbolic_transformation, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(theater_early_modern, symbolic_transformation, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(theater_contemporary, symbolic_transformation, theater_ratio, 2000, 0.35).

% Extraction over time
narrative_ontology:measurement(extract_70ce, symbolic_transformation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(extract_mishnaic, symbolic_transformation, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(extract_talmudic, symbolic_transformation, base_extractiveness, 500, 0.6).
narrative_ontology:measurement(extract_medieval, symbolic_transformation, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(extract_early_modern, symbolic_transformation, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(extract_contemporary, symbolic_transformation, base_extractiveness, 2000, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(suppress_70ce, symbolic_transformation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(suppress_mishnaic, symbolic_transformation, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(suppress_talmudic, symbolic_transformation, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(suppress_medieval, symbolic_transformation, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(suppress_early_modern, symbolic_transformation, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(suppress_contemporary, symbolic_transformation, suppression_requirement, 2000, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbolic_transformation, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of four structurally distinct readings of the temple sacrifice commitment kernel. Each reading has its own beneficiary/victim structure, its own ε value, and its own classification profile. The symbolic transformation reading is linked to its siblings through the kernel_id but represents a separate constraint in the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
