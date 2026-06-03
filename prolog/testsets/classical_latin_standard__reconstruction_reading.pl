% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Classical Latin Reconstruction Standard (Philological Archaeology Reading)
 *   domain: historical_linguistics/philology/humanist_authority
 *
 * SUMMARY:
 *   The Classical Latin Reconstruction Standard (Philological Archaeology
 *   Reading) instantiates one specific reading of a contested kernel
 *   governing what 'correct' or 'legitimate' Latin is. This reading stakes
 *   its authority claim on the possibility of recovering pristine Classical
 *   forms through textual archaeology and philological method — rejecting
 *   medieval drift as corruption and degradation. The kernel itself (what
 *   Latin correctness means) admits multiple readings: the continuity_reading
 *   holds that living transmitted practice IS correct, incorporating drift as
 *   legitimate development; the hybrid_reading accepts both Classical
 *   fidelity and post-Classical technical/ecclesiastical developments; this
 *   reconstruction_reading insists on discontinuous return to sources,
 *   treating intervening practice as invalid. This is a reading that benefits
 *   an identifiable elite (humanist philologists), suppresses alternatives
 *   (medieval forms delegitimized), and creates a new gatekeeping class
 *   (those trained in textual methods). It exhibits genuine coordination
 *   function (establishing shared standards for intellectual commerce)
 *   alongside asymmetric extraction (delegitimizing existing authority
 *   structures and creating dependency on humanist expertise).
 *
 * KEY AGENTS:
 *   - Humanist Philological Elite: Beneficiary (institutional/arbitrage) — exclusive access to Classical reconstruction methods, new prestige, intellectual gatekeeping power
 *   - Institutional Medieval Latin Users: Primary victim (powerless/trapped) — monks, scribes, administrators whose existing practice becomes 'incorrect'; cannot exit without abandoning their roles
 *   - Ecclesiastical Latin Community: Secondary victim (moderate/constrained) — church institutions lose linguistic authority to humanist scholars; can adapt but at organizational cost
 *   - Non-Elite Latin Communities: Tertiary victim (powerless/trapped) — scribal schools, notarial traditions, merchant guilds whose Latin practice is delegitimized; no access to Classical reconstruction training
 *   - Vernacular Language Movement: Organized exit path (organized/mobile) — emergence of national languages and print vernacular creates alternative to Classical gatekeeping
 *   - Analytical Observer: Civilizational identity-locked position (analytical/identity_locked) — humanist scholarly framework itself constituted through Classical standard acceptance; cannot see alternative without abandoning disciplinary identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.62).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.68).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Reconstruction Standard (Philological Archaeology Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/humanist_authority").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '2132d17d-87c8-49ae-a2ff-8c0fa554016a').
narrative_ontology:cs_kernel_codification('2132d17d-87c8-49ae-a2ff-8c0fa554016a', fixed_text).
narrative_ontology:cs_authority_grounding('2132d17d-87c8-49ae-a2ff-8c0fa554016a', lineage).
narrative_ontology:cs_interpretation_layer_present('2132d17d-87c8-49ae-a2ff-8c0fa554016a').
narrative_ontology:cs_reading_relation('2132d17d-87c8-49ae-a2ff-8c0fa554016a', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2132d17d-87c8-49ae-a2ff-8c0fa554016a', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('2132d17d-87c8-49ae-a2ff-8c0fa554016a', foundational, medieval_drift_is_corruption).
narrative_ontology:cs_axiom_status(medieval_drift_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('2132d17d-87c8-49ae-a2ff-8c0fa554016a', medieval_drift_is_corruption, empirically_contingent).
narrative_ontology:cs_axiom('2132d17d-87c8-49ae-a2ff-8c0fa554016a', foundational, classical_sources_contain_authentic_standard).
narrative_ontology:cs_axiom_status(classical_sources_contain_authentic_standard, holdable).
narrative_ontology:cs_axiom_grounding('2132d17d-87c8-49ae-a2ff-8c0fa554016a', classical_sources_contain_authentic_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('2132d17d-87c8-49ae-a2ff-8c0fa554016a', classical_textual_purity).
narrative_ontology:cs_drift_state('2132d17d-87c8-49ae-a2ff-8c0fa554016a', post_medieval_manuscript_recovery, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2132d17d-87c8-49ae-a2ff-8c0fa554016a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philological_elite).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, institutional_medieval_latin_users).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, non_elite_latin_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL MEDIEVAL LATIN USER (SNARE) — Monks, scribes, ecclesiastical administrators, and institutional users whose Latin practice was formed through living transmission now face systematic delegitimization. Their practice is reclassified as 'corrupt' or 'degraded.' Exit is blocked: abandoning their Latin entirely is structurally impossible (their role depends on Latin literacy), but their existing competence is now invalid. Maximum experienced extraction with no exit path.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ECCLESIASTICAL LATIN COMMUNITY (TANGLED ROPE) — Church institutions and ecclesiastical practitioners genuinely benefit from standardization (clarity in liturgy, theological precision) but face extraction through loss of institutional authority over Latin standards. They can resist or adapt, but at significant organizational and doctrinal cost. Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANIST PHILOLOGICAL ELITE (ROPE) — Beneficiaries who experience the constraint as pure coordination: recovering and standardizing Classical Latin enables intellectual commerce, scholarly prestige, and elite community membership. They have full exit through arbitrage (can switch scholarly communities, languages, disciplines). The constraint solves their coordination problem without coercion from their perspective.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VERNACULAR LANGUAGE MOVEMENT (SCAFFOLD) — Organized actors (early print culture, vernacular scholars, nation-state language standardization efforts) view the Classical Latin reconstruction as a temporary bottleneck that will sunset as vernacular standards mature. The constraint has low experienced extraction for this group because they see an exit path through linguistic diversification. Sunset logic: as national languages standardize and acquire prestige, the gatekeeping function of Classical Latin diminishes.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: HUMANIST TEXTUAL RITUAL (PITON) — From a civilizational view, the elaborate apparatus of recovering 'pure' Classical Latin through manuscript comparison is substantially performative. The ritual generates prestige and institutional authority (the humanist scholar as sage), but the functional gains beyond medieval Latin are often modest. Theater dominates function as the apparatus persists and elaborates despite diminishing returns.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL CAPTURE (TANGLED ROPE) — From civilizational/universal scope, this constraint exhibits genuine coordination function (establishing a shared standard for intellectual commerce across fragmented Late Medieval institutions) coupled with extractive institutionalization (gatekeeping authority, delegitimization of alternative practice traditions). The analytical position is identity-locked: the observer's scholarly framework itself depends on accepting the Classical standard as legitimate — rejecting it would require abandoning the entire humanist interpretive tradition. This instantiates the oracle gap: the analytical view cannot see beyond the standard it is constituted through.
constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(classical_latin_standard__reconstruction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, TR),
    TR >= 0.70.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. This reading systematically delegitimizes existing practice-based authority (medieval institutional Latin) and creates dependency on scarce expertise (philological training accessible only to elite humanists). The constraint extracts significant value through prestige redistribution and gatekeeping. However, it retains genuine coordination function — establishing shared standards for cross-institutional Latin scholarship — which prevents full snare classification. Measured at t=50 (mature institutionalization). Suppression (0.68): High. The reading explicitly suppresses medieval forms as 'corrupt' or 'degraded,' delegitimizing centuries of institutional practice. Exit barriers are severe: institutional users cannot adopt Classical standards without massive retraining; they cannot abandon Latin without abandoning their roles. Suppression increases over time (0.42 → 0.68) as humanist authority consolidates and medieval practices are progressively excluded from legitimate contexts. Theater ratio (0.55): Moderate. The elaborate apparatus of manuscript comparison, textual emendation, and reconstruction method has genuine functional component (establishing shared standards) but also substantial performative content (the humanist scholar as sage with special access to textual truth; the display of erudition and exclusivity). Theater remains below 0.70 because real coordination benefit persists, but it is not negligible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival gap. The humanist elite sees rope (coordination without coercion — they experience the standard as solving their scholarly communication problem). The institutional medieval user sees snare (trapped in an invalid practice with no exit). The ecclesiastical institution sees tangled rope (mixed benefit from standardization + extraction through loss of authority). The vernacular movement sees scaffold (temporary bottleneck with sunset as national languages mature). The humanist ritual, viewed civilizationally, is piton (performative apparatus maintained by institutional inertia). The analytical observer is trapped in identity_locked position — cannot reject the Classical standard without abandoning the humanist interpretive framework that constitutes their scholarly identity. This last perspective instantiates the oracle gap: the analysis is cognitively captured by the very standard it observes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary vs. victim, power level, and exit options. Humanist elite: beneficiary + institutional power + arbitrage exit = low d (around 0.15) = negative/low effective extraction from their position. Institutional medieval user: victim + powerless + trapped = high d (around 0.92) = high effective extraction (experienced as coercion). Ecclesiastical institution: victim + institutional power + constrained exit = moderate-high d (around 0.60) = moderate effective extraction with some agency. Vernacular movement: neither pure beneficiary nor pure victim + organized power + mobile exit = moderate d (around 0.50) = moderate extractiveness but high escape velocity. The engine derives these automatically from the beneficiary/victim declarations and exit options; no override needed. The perspectival gap emerges from these differing d values applied to the same base extractiveness (0.62): the beneficiary's chi is low/negative, the victim's chi is high (≥0.75), and organized actors' chi is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy through kernel contest: there is no single correct classification because the kernel itself (what 'correct Latin' means) is contested across three live readings with genuinely different premises. The reconstruction_reading tangled_rope classification is stable because the constraint exhibits both coordination (establishing shared standards) and extraction (delegitimizing alternatives, creating gatekeeping). However, the classification is CONDITIONAL on accepting the reading's core axioms: (1) that medieval drift is corruption rather than legitimate development, and (2) that Classical textual sources provide recoverable authenticity. If the axiom is overridden (evidence accumulates that medieval drift was inevitable/legitimate), the reading collapses toward snare (pure extraction without coordination benefit). The analytics are sound; the instability is at the kernel level, not the classification level. The engine correctly identifies this as a kernel reading with high axiom contest risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authenticity_criterion,
    'What makes a Latin form ''authentically Classical'' vs. ''corrupted medieval''? Is the criterion textual (attestation in surviving manuscripts), chronological (pre-Christian empire), functional (mutual intelligibility), or social (elite prestige)?',
    'Systematic comparison of philological selection criteria across different reconstructionist scholars; examination of which medieval forms would be admitted if criterion shifted from textual attestation to functional intelligibility or manuscript age; analysis of selection bias (which classical authors'' usage patterns are treated as authoritative?)',
    'If criterion is genuinely objective (textual attestation in known manuscripts): reading is minimally extractive, medieval forms are genuinely ''corrupt.'' If criterion is socially constructed (elite prestige as authenticity marker): extractiveness and suppression are substantially higher; medieval forms are pragmatically valid alternatives. This shifts classification toward higher snare component across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authenticity_criterion, conceptual, 'Criterion for distinguishing authentic Classical form from medieval corruption').

omega_variable(
    medieval_drift_inevitability,
    'Is medieval Latin drift a contingent artifact of institutional fragmentation and manuscript loss, or an inevitable linguistic consequence of living language use separated from prestige sources?',
    'Comparative analysis of drift patterns in other languages after textual standardization breaks down (post-classical Greek, post-classical Arabic, vulgar Latin itself post-empire); reconstruction of medieval scribal practices to distinguish intentional innovation from transmission error; analysis of whether medieval users perceived their practice as drift or as legitimate development',
    'If drift is inevitable/legitimate: medieval Latin is a natural linguistic development, not corruption; suppression of it is unjust; reading reclassifies toward snare. If drift is contingent/erroneously accumulated: reading is justified in treating recovery as restoration; classification remains tangled_rope but with lower victim impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_drift_inevitability, empirical, 'Whether medieval Latin drift is inevitable linguistic change or contingent corruption').

omega_variable(
    gatekeeping_hierarchy_emergence,
    'Does the reconstruction reading necessarily create a new gatekeeping hierarchy (philologically trained humanist elite as sole legitimate judges of correct Latin), or could Classical standards be recovered and diffused without privileging a new authority class?',
    'Historical analysis of how Classical Latin education was actually distributed: was it restricted to humanist elite or broadly taught? Comparison with societies that adopted Classical standards without creating new gatekeeping structures; analysis of whether high extractiveness is intrinsic to the reading or contingent on institutional capture by humanist circles',
    'If gatekeeping is intrinsic: reading''s extractiveness (0.62) is justified and unavoidable cost of standardization. If gatekeeping is contingent: extractiveness could be substantially lower if knowledge distribution changed; suppression is unjustly high; reading should be reclassified toward lower snare component or mapped onto hybrid_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_hierarchy_emergence, empirical, 'Whether Classical standard gatekeeping is intrinsic to this reading or contingent on institutional capture').

omega_variable(
    reading_kernel_contest,
    'This reading is one instantiation of the classical_latin_standard kernel. How does this reading''s core premise (correct Latin is recoverable only through discontinuous philological archaeology, rejecting medieval drift) relate structurally to the sibling continuity_reading (correct Latin is living practice) and hybrid_reading (both Classical fidelity and post-Classical developments are legitimate)?',
    'Examined via cs_structure.reading_relations and cs_structure.axioms fields (see below). This omega documents the kernel contest and the structural ambiguity it generates.',
    'If reconstruction_reading forecloses continuity_reading: no single institution can hold both commitments. If coexists_with: both remain live options across different scholarly communities. If influences: reconstruction creates pressure toward hybrid_reading but doesn''t eliminate continuity option. Classification stability depends on which relation obtains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural relationship between this reconstruction reading and sibling readings of the classical_latin_standard kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clsr_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(clsr_tr_t25, classical_latin_standard__reconstruction_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(clsr_tr_t50, classical_latin_standard__reconstruction_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(clsr_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clsr_be_t25, classical_latin_standard__reconstruction_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(clsr_be_t50, classical_latin_standard__reconstruction_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clsr_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clsr_su_t25, classical_latin_standard__reconstruction_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(clsr_su_t50, classical_latin_standard__reconstruction_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_authority_erosion).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, humanist_elite_gatekeeping).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three reading-specific constraints with distinct ε values and beneficiary/victim structures. reconstruction_reading (ε=0.62, Tangled Rope) emphasizes discontinuous return and medieval delegitimization. continuity_reading (separate story) emphasizes unbroken practice and natural drift. hybrid_reading (separate story) bridges both, accepting post-Classical developments in specific domains. All three are linked via network.affects_constraints because the three readings exist in real-time contest — adoption of one reading creates pressure (influences) or logical barriers (forecloses or coexists_with) for the others. Downstream constraints like ecclesiastical_latin_authority_erosion and humanist_elite_gatekeeping are shaped by which reading dominates institutional adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
