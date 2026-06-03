% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Latin Correctness: Hybrid Reading (Classical/Medieval Bifurcation)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of Latin correctness represents a bifurcated
 *   legitimacy claim: classical norms govern literary and theological domains
 *   where prestige and authority are concentrated, while medieval forms are
 *   recognized as legitimate (if subordinate) in technical and practical
 *   domains where clarity and precision take precedence. This constraint
 *   emerges from medieval institutional realities where the same Latin-using
 *   establishments (monasteries, universities, church bureaucracy)
 *   simultaneously produced Ciceronian prose for formal theology and Vulgar
 *   Latin neologisms for practical instruction. The constraint is not merely
 *   descriptive (medieval Latin evolved differently) but normative and
 *   enforced: authorities police the boundary between domains, maintaining
 *   classical standards as the prestige form and medieval usage as the
 *   tolerated practical dialect. The extractiveness (0.42) reflects moderate
 *   asymmetry: the classical establishment benefits from monopoly on
 *   high-status language, while technical writers and medieval usage bear the
 *   cost of delegitimization. The suppression (0.58) reflects real barriers
 *   to alternative forms of legitimacy — medieval writers cannot claim their
 *   usage as equally valid without losing ecclesiastical authority. The
 *   theater ratio (0.65) reflects rising performativity: by the high medieval
 *   period, the distinction between classical correctness and medieval
 *   practice is increasingly ritualized rather than functionally necessary,
 *   as the church itself operates in evolved Latin regardless of official
 *   standards.
 *
 * KEY AGENTS:
 *   - Classical Literary Establishment: Institutional beneficiary (institutional/arbitrage) — maintains monopoly on legitimate 'correct' Latin through gatekeeping; benefits from scarcity and prestige of classical standards
 *   - Ecclesiastical Authority: Primary enforcer (institutional/arbitrage) — establishes and polices the classical/medieval boundary; uses classical standards as legitimacy marker for theological authority
 *   - Technical Writers: Primary victim (powerless/trapped) — forced to choose between classical purity (incomprehensible for technical subjects) and boundary violation (loss of authority); no exit from institutional mandate
 *   - Monastic Scribes: Secondary victim (moderate/constrained) — experience dual mandate: preserve classical standards for theology while writing practical texts requiring medieval vocabulary; constrained by monastic authority
 *   - University Grammarians: Organized secondary actors (organized/mobile) — define and enforce the boundary between classical prescriptive grammar and medieval descriptive grammar; benefit from dual teaching domain but incentivized to maintain hierarchy
 *   - Medieval Usage: Collective victim (abstract, no power/trapped) — delegitimized despite being the living language; bears symbolic cost of subordination to 'correct' forms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.42).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.58).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Latin Correctness: Hybrid Reading (Classical/Medieval Bifurcation)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '37e3a93b-cad5-4f11-b785-bf0fe944d1ce').
narrative_ontology:cs_kernel_codification('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', fixed_text).
narrative_ontology:cs_authority_grounding('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', lineage).
narrative_ontology:cs_interpretation_layer_present('37e3a93b-cad5-4f11-b785-bf0fe944d1ce').
narrative_ontology:cs_reading_relation('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', foundational, domain_partitioned_legitimacy).
narrative_ontology:cs_axiom_status(domain_partitioned_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', domain_partitioned_legitimacy, conventional).
narrative_ontology:cs_axiom('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', foundational, technical_necessity_override).
narrative_ontology:cs_axiom_status(technical_necessity_override, holdable).
narrative_ontology:cs_axiom_grounding('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', technical_necessity_override, instrumental).
narrative_ontology:cs_reference_frame('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', classical_authority_with_functional_boundaries).
narrative_ontology:cs_drift_state('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', late_medieval_institutional_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('37e3a93b-cad5-4f11-b785-bf0fe944d1ce', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_literary_establishment).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medieval_usage_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TECHNICAL WRITER (SNARE) — Trapped by institutional mandate to write in classical forms despite the medieval vocabulary and syntax required for technical precision. Cannot exit: church/manuscript authority forbids medieval legitimacy in formal texts; cannot comply: technical subjects lack classical terminology. Bears extraction: forced choice between classical purity (incomprehensibility) or violation (loss of authority/correctness). No exit option.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: MONASTIC SCRIBE (TANGLED ROPE) — Constrained by dual mandate: preserve classical standards for liturgy/theology while writing practical technical texts (farming manuals, medical recipes, administrative documents) that require medieval vocabulary and syntax. Benefits from legitimacy ceiling for classical texts; extracted from by pressure to maintain hybrid correctness in practical domains. Constrained exit: career, social position, and spiritual authority depend on manuscript authority; cannot walk away from either domain.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CLASSICAL LITERARY ESTABLISHMENT (ROPE) — Institutional beneficiary (humanists, church theologians, legal authorities). Benefits from monopoly on legitimacy for 'correct' Latin; the bifurcation keeps classical standards elevated and scarce, preserving the establishment's gatekeeping power. Experiences the constraint as coordination: maintaining standards enables the transmission of high-status texts. Can arbitrage their classical mastery into ecclesiastical authority, teaching, jurisprudence. Net beneficiary — extraction runs toward this group.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: UNIVERSITY GRAMMARIANS (TANGLED ROPE) — Organized institutional actors with mobile exit options (can migrate universities, shift to vernacular teaching, write grammar treatises defining medieval Latin as legitimate). But also beneficiaries: the bifurcation gives them teaching domain (both classical prescriptive grammar AND medieval descriptive grammar), and technical text compilation. Coordination function: defining which texts exemplify which standard. Extraction function: enforcing the boundary that keeps medieval texts subordinate. Mixed structure — agency to reshape the boundary, but incentives to maintain it.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ECCLESIASTICAL AUTHORITY (PITON) — At civilizational scale, the enforcement of classical standards becomes increasingly performative: by the 11th-13th centuries, the church's own administrative and liturgical texts use medieval forms (subjunctive agreement drift, ablative absolute decay, new prepositions). The institutional insistence on classical purity for 'formal' texts coexists with the organization's own evolved practice. The constraint persists through tradition and authority-maintenance theater rather than through functional necessity — the church uses both standards flexibly while officially maintaining classical supremacy. Theater ratio high because the boundary enforcement is about status signaling more than linguistic coherence.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL EVOLUTION VIEW (MOUNTAIN) — From a universal/civilizational perspective, the hybrid reading appears as recognition of a natural linguistic law: living languages always diverge from their classical standards over time; specialized technical vocabularies always emerge; written standards always lag spoken evolution. The bifurcation (classical for literary, medieval for technical) looks like rational accommodation to linguistic reality. However, the structural data reveals this as a false summit: the constraint's extractiveness (0.42) and suppression (0.58) indicate that the 'natural divergence' is being actively enforced and policed for status purposes, not passively observed.
constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(latin_correctness__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(latin_correctness__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate-high. The hybrid reading creates asymmetric benefit distribution. Classical literary establishment captures prestige, ecclesiastical authority, and gatekeeping power; technical writers and medieval usage bear delegitimization cost. However, the extractiveness is moderate rather than high (not snare-level 0.66+) because the bifurcation serves a genuine coordination function: classical standards maintain theological coherence and ecclesiastical authority, medieval legitimacy enables practical communication. The constraint is not pure extraction — it solves real problems of domain differentiation. But the solution privileges one domain over another, creating extractive pressure on the technical domain. The rising trajectory (0.28→0.42) reflects increasing institutional enforcement of the boundary as the medieval language diverges further from classical norms, requiring more active policing. Suppression (0.58): Moderate-high. Technical writers face real barriers: institutional mandate for classical forms, loss of authority for medieval usage, no alternative prestige hierarchy that recognizes medieval legitimacy. But suppression is not total (not snare-level 0.60+) because the boundary is permeable — medieval usage is explicitly allowed in technical domains, not forbidden absolutely. Suppression takes the form of subordination and delegitimization rather than outright prohibition. Theater ratio (0.65): Moderate-high. The enforcement of the classical/medieval boundary becomes increasingly performative over the measurement interval. Early period (theater 0.52): boundary reflects real linguistic divergence and functional necessity (classical forms genuinely do provide theological precision that early-stage medieval forms lack). Mid period (theater 0.60): boundary becomes ritualized; the church's own administrative texts use medieval forms while officially maintaining classical standards. Late period (theater 0.65): enforcement is largely symbolic — the distinction is maintained through institutional custom and authority signaling rather than through functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence across power levels. The classical literary establishment sees pure coordination (Rope) — maintaining standards enables authority and textual transmission. The ecclesiastical authority sees institutional preservation through boundary maintenance (Rope to Piton, depending on honesty about actual practice). The monastic scribe sees genuine hybrid pressure (Tangled Rope) — real coordination function alongside real extraction pressure. The university grammarian sees organized power to reshape the boundary (Tangled Rope with mobile exit) — agency to define what counts as legitimate. The technical writer sees entrapment (Snare) — forced choice between incomprehensibility and illegitimacy, no exit. The analytical observer risks seeing natural linguistic law (Mountain) — the boundary looks like rational recognition of domain-specific variation. But the structural data reveals false summit: the boundary is enforced for status/authority reasons, not merely recognized as natural fact. The gap between beneficiary perception (coordination) and victim perception (extraction) is the diagnostic signal that this is Tangled Rope, not pure Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are derived from structural position: classical establishment (beneficiary, institutional, arbitrage exit) gets low d → negative effective extraction (they benefit). Technical writer (victim, powerless, trapped) gets high d → high f(d) → high experienced extraction. Monastic scribe (mixed victim/beneficiary, moderate power, constrained exit) gets moderate d → moderate experienced extraction. University grammarian (organized, mobile) gets lower d than victims despite partial victim status because their exit options and agency reduce their structural vulnerability. Analytical observer (analytical perspective) gets canonical d ≈ 0.73 → f(d) ≈ 1.15. The piton perspective uses low d (beneficiary status, arbitrage exit) despite the piton classification — the piton class derives from theater ratio gate, not from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by documenting how the hybrid reading manages the tension between coordination and extraction. The coordination function is genuine: maintaining classical standards for formal/theological texts does provide coherence and authority. The extraction function is also genuine: the bifurcation creates status hierarchy that benefits the classical establishment while burdening technical writers with impossible standards. The Tangled Rope classification captures both: the constraint cannot be reduced to either pure coordination (Rope) or pure extraction (Snare). The tension is structural, not resolvable by choosing one type. The piton perspective reveals that the boundary enforcement becomes increasingly performative over time — the coordination function weakens while the authority-maintenance theater persists. This is the lifecycle signature of a constraint moving toward degradation: as the functional necessity for the distinction diminishes, institutional inertia keeps it in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_maintenance_cost,
    'How much of the institutional effort spent enforcing the classical/medieval boundary is driven by linguistic clarity vs. status hierarchy maintenance?',
    'Historical analysis of institutional flexibility: do authorities relax classical requirements when practical texts fail (medical crisis, administrative breakdown) or maintain them regardless of functional cost? Correlation between boundary enforcement strictness and institutional power consolidation periods.',
    'If predominantly status maintenance: the constraint is more extractive than the current 0.42 assessment; hybrid reading is cover story for hierarchy. If mixed clarity/status: current assessment holds; bifurcation serves real coordination function. If predominantly clarity: constraint reclassifies toward Rope; the boundary is functional, not extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_cost, empirical, 'Proportion of boundary enforcement driven by status vs. functional necessity').

omega_variable(
    technical_comprehensibility_prerequisite,
    'Do technical texts written in strict classical Latin achieve the same comprehensibility and precision as those using medieval vocabulary and syntax?',
    'Comparative analysis of parallel technical texts (medical recipes, agricultural manuals, architectural treatises) in classical vs. hybrid vs. purely medieval Latin. Error rates, ambiguity indices, copy fidelity across transmission.',
    'If classical insufficient: technical victimization is real (trapped between incomprehensibility and boundary violation). If classical sufficient: the medieval vocabulary is preference/convenience, not necessity; extraction is less coercive. If medieval necessary: the constraint becomes actively counterproductive — enforcing classical norms actively harms the technical domain''s function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_comprehensibility_prerequisite, empirical, 'Whether classical Latin provides adequate technical precision').

omega_variable(
    sibling_reading_empirical_boundary,
    'At what scale and in what contexts would the hybrid reading''s bifurcation collapse, merging toward the continuity reading (all medieval Latin legitimate) vs. the rupture reading (medieval Latin is corruption, not alternative)?',
    'Historical corpus analysis: tracking which authorities recognize medieval forms as legitimate vs. corrupted across centuries and institutions. Mapping institutional power consolidation to tightening of classical standards. Identifying moments when the hybrid boundary shifts.',
    'If boundary consistently collapses toward continuity: hybrid reading is unstable; contingent on specific institutional configurations. If boundary collapses toward rupture: indicates power shift toward classical supremacy; hybrid reading becomes untenable. If boundary persists: hybrid reading is structurally robust across time scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_boundary, empirical, 'Historical stability of the classical/medieval boundary across institutions').

omega_variable(
    reading_vs_sibling_foreclosure_test,
    'Does this hybrid reading''s core axiom (classical AND medieval legitimacy, domain-partitioned) logically foreclose either sibling reading''s core premise within a single institutional framework?',
    'Formal logical analysis of axiom commitments: can an institution simultaneously hold (a) classical forms are normative in literary domains AND (b) medieval forms are legitimate alternative in technical domains AND (c) medieval Latin is organic continuation of classical (continuity), or must it reject this? Similarly for rupture reading: can an institution hold both the hybrid bifurcation AND the claim that medieval Latin is textual corruption? Where do institutional commitments break?',
    'If hybrid forecloses one sibling: update reading_relations to ''forecloses'' for that sibling. If no foreclosure: relation remains ''coexists_with'' or ''influences''. If mutual foreclosure: reading_relations may need revision for consistency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure_test, conceptual, 'Whether hybrid reading logically forecloses sibling readings'' core premises').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latinhy_theater_early, latin_correctness__hybrid_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(latinhy_theater_mid, latin_correctness__hybrid_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(latinhy_theater_late, latin_correctness__hybrid_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(latinhy_extract_early, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(latinhy_extract_mid, latin_correctness__hybrid_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(latinhy_extract_late, latin_correctness__hybrid_reading, base_extractiveness, 10, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(latinhy_suppress_early, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(latinhy_suppress_mid, latin_correctness__hybrid_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(latinhy_suppress_late, latin_correctness__hybrid_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The kernel 'latin_correctness' decomposes into three structurally distinct constraint stories, one for each reading. Each reading asserts a different structural relationship between classical and medieval Latin, yielding different ε values and different beneficiary/victim structures. The hybrid reading (this story) positions classical and medieval as domain-partitioned legitimate standards; the continuity reading positions them as points on a continuum of organic evolution; the rupture reading positions them as authentic vs. corrupted dichotomy. These are not three perspectives on one constraint — they are three different constraints, each from an incommensurable institutional commitments standpoint. They are linked by network.affects_constraints because adoption of one reading by an authority directly constrains which other readings remain viable in overlapping institutional space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
