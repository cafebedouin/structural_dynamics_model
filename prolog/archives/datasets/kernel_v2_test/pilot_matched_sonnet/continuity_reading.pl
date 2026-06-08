% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
 *   constraint_id: continuity_reading
 *   human_readable: Continuity Reading: Medieval Latin as Legitimate Evolution
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading holds that correct Latin is the form transmitted
 *   through continuous living practice from classical antiquity through the
 *   medieval period. Medieval Latin is not corruption but legitimate
 *   evolution — the language remained alive in institutional use (Church,
 *   universities, legal systems) and evolved naturally as living languages
 *   do. This reading coordinates the relationship between classical and
 *   medieval forms: both are authentic Latin, distinguished by period rather
 *   than correctness. The reading solves a genuine coordination problem: how
 *   to maintain a working scholarly language across a millennium and a
 *   continent while allowing natural linguistic change. The alternative
 *   readings (discontinuity: medieval Latin is corruption requiring classical
 *   restoration; hybrid: medieval and classical are distinct registers)
 *   represent different structural positions on the same kernel question:
 *   what makes Latin correct?
 *
 * KEY AGENTS:
 *   - Medieval Ecclesiastical Institutions: Primary beneficiary (institutional/mobile) — the continuity reading legitimates their Latin usage and validates institutional transmission as authoritative
 *   - Medieval Scribes and Scholars: Practitioners (moderate/constrained) — their daily Latin practice is validated as correct, not degraded; coordination costs are real but extraction is low
 *   - Vernacular Language Communities: Secondary beneficiary (organized/mobile) — if medieval Latin is legitimate evolution, Romance vernaculars are legitimate descendants, not corruptions
 *   - Renaissance Humanists: Mixed position (powerful/constrained) — benefit from institutional continuity and manuscript access, but constrained by having their classicizing reforms framed as preference rather than restoration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination mechanism with low extraction; living language evolution is a real phenomenon, not a cover story
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.28).
domain_priors:suppression_score(continuity_reading, 0.35).
domain_priors:theater_ratio(continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, rope).
narrative_ontology:human_readable(continuity_reading, "Continuity Reading: Medieval Latin as Legitimate Evolution").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, 'cfec5966-85bf-4c29-a37a-cb5cd135b78e').
narrative_ontology:cs_kernel_codification('cfec5966-85bf-4c29-a37a-cb5cd135b78e', distributed).
narrative_ontology:cs_authority_grounding('cfec5966-85bf-4c29-a37a-cb5cd135b78e', lineage).
narrative_ontology:cs_interpretation_layer_present('cfec5966-85bf-4c29-a37a-cb5cd135b78e').
narrative_ontology:cs_reading_relation('cfec5966-85bf-4c29-a37a-cb5cd135b78e', continuity_reading__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfec5966-85bf-4c29-a37a-cb5cd135b78e', continuity_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cfec5966-85bf-4c29-a37a-cb5cd135b78e', foundational, living_transmission_preserves_legitimacy).
narrative_ontology:cs_axiom_status(living_transmission_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cfec5966-85bf-4c29-a37a-cb5cd135b78e', living_transmission_preserves_legitimacy, conventional).
narrative_ontology:cs_axiom('cfec5966-85bf-4c29-a37a-cb5cd135b78e', foundational, evolution_within_continuity_is_legitimate).
narrative_ontology:cs_axiom_status(evolution_within_continuity_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cfec5966-85bf-4c29-a37a-cb5cd135b78e', evolution_within_continuity_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('cfec5966-85bf-4c29-a37a-cb5cd135b78e', classical_latin_as_transmitted).
narrative_ontology:cs_drift_state('cfec5966-85bf-4c29-a37a-cb5cd135b78e', late_medieval, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cfec5966-85bf-4c29-a37a-cb5cd135b78e', '').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_scholars).
narrative_ontology:constraint_beneficiary(continuity_reading, church_institutional_continuity).
narrative_ontology:constraint_beneficiary(continuity_reading, vernacular_language_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL ECCLESIASTICAL INSTITUTIONS (ROPE) — The continuity reading solves a genuine coordination problem: maintaining intelligible communication across centuries and regions while allowing natural linguistic evolution. Medieval forms are legitimate developments, not corruptions. The Church benefits from this framing but the coordination function is real — Latin remained a working lingua franca precisely because it was allowed to evolve with living practice.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 2: MEDIEVAL SCRIBES AND SCHOLARS (ROPE) — Practitioners experience the continuity reading as coordination: their Latin is legitimate because it descends continuously from Classical usage through unbroken transmission. They face constraints (must maintain intelligibility with earlier texts, must train in grammatical tradition) but these are coordination costs, not extraction. The reading validates their practice as authentic Latin, not degraded imitation.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VERNACULAR LANGUAGE COMMUNITIES (ROPE) — The continuity reading legitimizes the Romance languages as natural descendants rather than corruptions. If medieval Latin is legitimate evolution, then Italian, French, Spanish are legitimate evolutions of Latin, not degenerations. This reading coordinates the relationship between learned Latin and vernacular practice: both are valid developments from the same root. Low extraction — the reading enables rather than constrains vernacular development.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: RENAISSANCE HUMANISTS (TANGLED ROPE) — Humanists experience the continuity reading as mixed. On one hand, it coordinates access to medieval texts and validates the unbroken transmission they depend on. On the other hand, it constrains their reform project: if medieval Latin is legitimate evolution, their classicizing reforms are arbitrary preference, not restoration of correctness. They benefit from institutional continuity (access to manuscripts, university positions) while bearing the cost of having their purism framed as innovation rather than recovery. Moderate extraction.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, the continuity reading describes a genuine coordination mechanism: living languages evolve, and correctness is determined by continuous practice rather than frozen classical standards. The reading has low extraction because it does not suppress alternatives (classicizing Latin coexisted with medieval forms) and solves a real problem (maintaining a working scholarly language across a millennium). The coordination function is primary; any extraction (institutional authority derived from transmission claims) is secondary.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).
:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The continuity reading does generate some extraction — institutional authority is derived from claims of unbroken transmission, and this authority can be leveraged for other purposes (ecclesiastical power, university gatekeeping). However, the extraction is substantially lower than the discontinuity reading would produce: the continuity reading does not require expensive classical education to participate in legitimate Latin, does not invalidate existing medieval texts, and does not create a sharp insider/outsider boundary. The modest extraction reflects that institutional transmission claims do carry some rent-seeking potential, but the coordination function is primary. Suppression (0.35): Low-moderate. The continuity reading does not strongly suppress alternatives — classicizing Latin coexisted with medieval forms throughout the period, and vernacular languages developed alongside learned Latin. Some suppression exists (institutional preference for Latin over vernaculars in certain domains, grammatical training requirements) but it is not severe. The suppression increased toward the Renaissance as the discontinuity reading gained strength and began to delegitimate medieval forms. Theater ratio (0.42): Moderate. Some performative elements exist — appeals to 'continuous transmission' can be theatrical when the transmission was actually fragmented or when forms are justified by lineage rather than function. The theater ratio increased over the interval as institutional claims of unbroken transmission became more important for legitimacy (especially as the discontinuity reading challenged medieval forms). Early medieval practice had lower theater (forms were justified by current usage); late medieval and Renaissance practice had higher theater (forms were increasingly justified by transmission claims).
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading produces relatively uniform classification across perspectives — most agents see rope (genuine coordination with low extraction). This uniformity is itself diagnostic: a reading that solves a real coordination problem without severe extraction or suppression will appear as rope from most structural positions. The one exception is the Renaissance humanists, who experience tangled_rope: they benefit from the institutional continuity the reading provides (access to manuscripts, university positions, scholarly community) while bearing the cost of having their reform project framed as arbitrary preference rather than restoration of correctness. The perspectival gap between humanists (tangled_rope) and other agents (rope) reveals the structural tension: the continuity reading coordinates medieval practice well but constrains classical purism.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval ecclesiastical institutions are primary beneficiaries — the continuity reading legitimates their Latin and validates their institutional authority as transmitters of correct usage. They have high power (institutional) and high exit options (mobile — could adopt vernacular or classicizing Latin if needed), so their directionality is low (beneficiary position). Medieval scribes and scholars are also beneficiaries but with more constraints — their Latin is validated, but they must maintain intelligibility with tradition. Moderate power, constrained exit, beneficiary status yields low-moderate directionality. Vernacular language communities benefit indirectly — if medieval Latin is legitimate evolution, their languages are legitimate descendants. Organized power, mobile exit, beneficiary status yields low directionality. Renaissance humanists have mixed directionality — they benefit from institutional continuity but bear costs from having their reforms delegitimated. Powerful, constrained exit, mixed beneficiary/victim status yields moderate directionality. The analytical observer has analytical directionality (neither beneficiary nor victim in the structural sense).
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resolves mandatrophy by distinguishing genuine coordination (maintaining working scholarly language across time and space) from extraction (institutional authority derived from transmission claims). The coordination function is primary and real — Latin did function as a lingua franca for a millennium, and the continuity reading describes the mechanism that made this possible (allowing natural evolution while maintaining intelligibility). The extraction is secondary and modest — some institutional rent-seeking occurs, but it does not dominate the structure. The reading is rope rather than mountain because it is not a natural law (languages do not inherently evolve continuously — they can also fragment, be artificially frozen, or be deliberately reformed) and it is not a snare because the coordination function is genuine and the extraction is not severe. The mandatrophy is resolved by recognizing that the same structure can be primarily coordinative (rope) while containing some extractive elements, without collapsing into tangled_rope (which requires both functions to be substantial and active enforcement to maintain the hybrid).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the continuity reading one interpretation of a contested kernel (correct_latin), with sibling readings (discontinuity_reading: medieval Latin is corruption requiring classical restoration; hybrid_reading: medieval and classical Latin are distinct registers with different legitimacy domains)?',
    'Historical analysis of when and where each reading was held; identification of which institutional actors defended which reading; examination of whether the readings coexisted or foreclosed each other within single frameworks.',
    'If readings coexist: the kernel is genuinely contested and each reading represents a live structural position. If one reading forecloses others: the kernel has a dominant interpretation and sibling readings are historical artifacts or minority positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether this constraint is one reading of a contested kernel with identifiable sibling readings').

omega_variable(
    transmission_legitimacy_grounding,
    'Does the continuity reading ground legitimacy in unbroken institutional transmission (lineage), or in the functional success of medieval Latin as a working scholarly language (practice)?',
    'Examination of medieval grammatical treatises and institutional documents: do they justify medieval forms by appeal to continuous transmission from classical sources, or by appeal to current scholarly usage and communicative success?',
    'If lineage: the reading is a commitment system with authority_grounding=lineage. If practice: authority_grounding=practice. The distinction affects how drift is absorbed — lineage systems develop interpretation layers, practice systems evolve the kernel directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_legitimacy_grounding, empirical, 'Whether legitimacy is grounded in transmission lineage or functional practice').

omega_variable(
    evolution_threshold,
    'At what point does evolutionary change become discontinuity? The continuity reading must draw a boundary between legitimate evolution (medieval forms) and illegitimate rupture (hypothetical future forms that break intelligibility).',
    'Analysis of medieval grammatical debates about acceptable variation; identification of forms that were contested as ''too far'' from classical usage; examination of when intelligibility actually broke down (e.g., between Latin and early Romance vernaculars).',
    'If the threshold is vague: the continuity reading has an internal instability — it cannot distinguish evolution from corruption without importing the discontinuity reading''s criteria. If the threshold is clear: the reading has a principled boundary (e.g., mutual intelligibility with classical texts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolution_threshold, conceptual, 'How the continuity reading distinguishes legitimate evolution from rupture').

omega_variable(
    vernacular_legitimacy_coupling,
    'Does the continuity reading''s legitimation of medieval Latin necessarily legitimate the Romance vernaculars, or can the readings be decoupled?',
    'Historical examination of whether defenders of medieval Latin also defended vernacular legitimacy, or whether some held that Latin could evolve while vernaculars were corruptions. Analysis of 16th-17th century debates about vernacular literary status.',
    'If coupled: the continuity reading has broader structural implications (legitimates all evolutionary language change). If decoupled: the reading can be held selectively (Latin evolution is legitimate, vernacular ''corruption'' is not), revealing potential extraction (institutional Latin benefits from evolution claim while vernaculars do not).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_legitimacy_coupling, empirical, 'Whether medieval Latin legitimacy necessarily extends to vernacular legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_medieval, continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_high_medieval, continuity_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(theater_late_medieval, continuity_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(theater_renaissance, continuity_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(extract_early_medieval, continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(extract_high_medieval, continuity_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(extract_late_medieval, continuity_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(extract_renaissance, continuity_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppress_early_medieval, continuity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(suppress_late_medieval, continuity_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(suppress_renaissance, continuity_reading, suppression_requirement, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, information_standard).
narrative_ontology:affects_constraint(continuity_reading, discontinuity_reading).
narrative_ontology:affects_constraint(continuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three readings with different structural properties. The continuity_reading has the lowest extraction (0.28) because it does not invalidate existing practice or require expensive retraining. The discontinuity_reading (not modeled here) would have higher extraction because it delegitimates medieval forms and requires classical education for participation. The hybrid_reading (not modeled here) would have intermediate extraction because it partitions the legitimacy space but does not fully delegitimate either register. The three readings are linked via network.affects_constraints because they compete for the same institutional and scholarly territory — adoption of one reading changes the structural environment for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
