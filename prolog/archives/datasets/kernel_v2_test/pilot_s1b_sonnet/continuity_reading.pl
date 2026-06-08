% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Correct Latin as Continuity Through Guided Correction (Continuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading treats medieval Latin as a recoverable degradation
 *   of the Classical standard. Under this reading, 'correct Latin' is the
 *   Classical form (Cicero, Vergil, Augustan prose), and medieval drift (loss
 *   of case distinctions, phonetic spellings, syntactic simplification)
 *   represents correctable error rather than organic language change. The
 *   Carolingian reforms (late 8th–9th century) instantiate this reading:
 *   Alcuin's standardization program treated vulgar medieval forms as
 *   mistakes to be corrected through education, textual emendation, and
 *   institutional discipline. The constraint coordinates the pan-European
 *   clerical network around a shared written standard, solving the genuine
 *   problem of administrative and liturgical communication across Romance
 *   vernacular zones where spoken mutual intelligibility had collapsed.
 *   Within this reading, all agents are net beneficiaries: scribes gain
 *   access to a prestigious and stable orthographic system, monastic
 *   scriptoria gain administrative capacity, the Church gains cross-regional
 *   coherence. Extraction is low because adoption is (claimed to be)
 *   voluntary and mutually beneficial. Theater ratio is low because the
 *   coordination function is genuine — the textual corrections and
 *   grammatical instruction produce real increases in mutual intelligibility.
 *   This is ONE reading of the contested kernel 'correct Latin'; sibling
 *   readings (rupture_reading, hybrid_reading) instantiate different
 *   constraints with different epsilon values and different
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Medieval Scholarly Community: Primary beneficiary (moderate/mobile, organized/constrained) — gains access to shared written standard enabling cross-regional communication and textual transmission
 *   - Classical Textual Transmission Infrastructure: Beneficiary (institutional/arbitrage) — scriptoria, cathedral schools, palace schools that invest in correction programs and reap administrative coordination benefits
 *   - Pan-European Clerical Network: Beneficiary (institutional/arbitrage) — ecclesiastical hierarchy gains administrative coherence through standardized Latin for legal, liturgical, and diplomatic texts
 *   - Analytical Observer (Continuity Frame): Sees the constraint as pure coordination (rope) — Classical Latin is recoverable, drift is correctable error, all parties benefit from shared standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.18).
domain_priors:suppression_score(continuity_reading, 0.15).
domain_priors:theater_ratio(continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, rope).
narrative_ontology:human_readable(continuity_reading, "Correct Latin as Continuity Through Guided Correction (Continuity Reading)").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, '64fd6365-9d0c-42b4-9ac1-415c429efd52').
narrative_ontology:cs_kernel_codification('64fd6365-9d0c-42b4-9ac1-415c429efd52', fixed_text).
narrative_ontology:cs_authority_grounding('64fd6365-9d0c-42b4-9ac1-415c429efd52', lineage).
narrative_ontology:cs_interpretation_layer_present('64fd6365-9d0c-42b4-9ac1-415c429efd52').
narrative_ontology:cs_reading_relation('64fd6365-9d0c-42b4-9ac1-415c429efd52', continuity_reading__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('64fd6365-9d0c-42b4-9ac1-415c429efd52', continuity_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('64fd6365-9d0c-42b4-9ac1-415c429efd52', foundational, linguistic_continuity_across_vulgarization).
narrative_ontology:cs_axiom_status(linguistic_continuity_across_vulgarization, holdable).
narrative_ontology:cs_axiom_grounding('64fd6365-9d0c-42b4-9ac1-415c429efd52', linguistic_continuity_across_vulgarization, empirically_contingent).
narrative_ontology:cs_axiom('64fd6365-9d0c-42b4-9ac1-415c429efd52', foundational, classical_standard_recoverable_via_correction).
narrative_ontology:cs_axiom_status(classical_standard_recoverable_via_correction, holdable).
narrative_ontology:cs_axiom_grounding('64fd6365-9d0c-42b4-9ac1-415c429efd52', classical_standard_recoverable_via_correction, conventional).
narrative_ontology:cs_reference_frame('64fd6365-9d0c-42b4-9ac1-415c429efd52', augustan_prose_standard).
narrative_ontology:cs_drift_state('64fd6365-9d0c-42b4-9ac1-415c429efd52', carolingian_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64fd6365-9d0c-42b4-9ac1-415c429efd52', '').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_scholarly_community).
narrative_ontology:constraint_beneficiary(continuity_reading, classical_textual_transmission_infrastructure).
narrative_ontology:constraint_beneficiary(continuity_reading, pan_european_clerical_network).
narrative_ontology:constraint_vindicates(continuity_reading, linguistic_continuity_hypothesis).
narrative_ontology:constraint_vindicates(continuity_reading, recoverable_classical_standard).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBE (ROPE) — Views Classical Latin as an accessible ideal reachable through study and correction. The gap between vulgar and classical forms is bridgeable via monastic education and textual exemplars. Mobile exit options: scribes move between scriptoria, adopt regional standards, or code-switch between registers. Low extraction: coordination solves the genuine problem of cross-regional communication.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: CAROLINGIAN REFORM (ROPE) — Institutional agents (Alcuin, Palace School, monastic scriptoria network) experience the standardization program as pure coordination: establishing shared orthographic and grammatical norms enables administrative coherence across the empire. Arbitrage exit: institutional actors can adopt or reject reform standards based on political alignment. Net beneficiary: reforms consolidate administrative capacity.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: CATHEDRAL SCHOOLS (ROPE) — Organized educational institutions see Classical Latin as a coordination standard that solves the problem of training clerics for administrative and liturgical roles. Constrained exit: schools are embedded in ecclesiastical hierarchy and cannot fully abandon Latin, but they can vary the degree of classicization. Moderate extraction: some resource investment required for textual correction and pedagogical infrastructure, but the coordination benefit outweighs the cost.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / CONTINUITY FRAME (ROPE) — From a civilizational perspective within the continuity reading's axioms, medieval Latin is a degraded but recoverable form of Classical Latin. The constraint (adopt Classical norms through guided correction) coordinates the scholarly community around a shared standard without extracting from any party. All agents are net beneficiaries of mutual intelligibility. Low suppression: alternatives (regional vernaculars, uncorrected vulgar Latin) remain available; the Classical standard is adopted voluntarily for its coordination value.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.18): Low. The continuity reading treats Classical Latin adoption as a coordination solution to the problem of cross-regional communication among populations whose spoken Romance vernaculars had diverged. Modest extraction reflects resource investment in textual correction and pedagogical infrastructure, but the coordination benefit (mutual intelligibility, administrative capacity) is claimed to outweigh the cost for all parties. Extraction increases slightly over the interval (0.12 → 0.18) as Carolingian reforms institutionalize the standard and embed it in ecclesiastical career pathways, creating mild gatekeeping pressure. Suppression (0.15): Low. The continuity reading claims that Classical Latin adoption was substantially voluntary: scribes and students chose to learn the classical standard because it provided genuine value (prestige, access to textual tradition, administrative utility). Alternatives (regional vulgar Latin, vernacular literacy) remained available but were not suppressed — they were simply less useful for the coordination problems the clerical network faced. Suppression rises modestly during the Carolingian period (0.10 → 0.15) as reforms embed the standard in institutional credentialing, but remains low overall. Theater ratio (0.22): Low. Within the continuity reading, the grammatical instruction, textual emendation, and orthographic standardization have genuine functional content — they produce measurable increases in cross-regional textual intelligibility and administrative capacity. Theater rises modestly (0.15 → 0.22) as the institutional infrastructure matures and some performative credentialing emerges (knowing Donatus and Priscian becomes a status marker), but the functional coordination core remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   Within the continuity reading, there is minimal perspectival gap because all agents experience the constraint as coordination. The medieval scribe (moderate/mobile) sees Classical Latin as an accessible ideal reachable through study. The Carolingian reform movement (institutional/arbitrage) sees standardization as administrative problem-solving. The cathedral school network (organized/constrained) sees the classical standard as a pedagogical coordination mechanism. The analytical observer within the continuity frame sees rope from a civilizational perspective. The gap that DOES exist is between this reading and its siblings: if rupture_reading shows substantially higher extraction (enforced standard, suppressed alternatives, identifiable victims), the kernel decomposes into multiple constraints per the epsilon-invariance principle. The perspectival gap is not within this constraint but between the readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries in this reading are net beneficiaries with no victim counterparts. The medieval scholarly community (moderate/mobile and organized/constrained) experiences low directionality toward extraction because they gain coordination value from the shared standard — the constraint subsidizes their work rather than extracting from it. The classical textual transmission infrastructure (institutional/arbitrage) benefits from administrative coordination gains and reputational capital. The pan-European clerical network (institutional/arbitrage) benefits from standardized legal, liturgical, and diplomatic communication. No victims are declared because the continuity reading treats Classical Latin adoption as a voluntary coordination mechanism where all parties gain from mutual intelligibility. The low base extractiveness combined with beneficiary-only structure produces low or negative effective extraction (chi) for all perspectives, yielding rope classification across all contexts. If the rupture reading or hybrid reading identifies victims (vernacular-literate populations excluded from clerical careers, peripheral scribes denied access to elite textual resources), those victims belong to those readings' constraints, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading avoids mandatrophy by treating Classical Latin as a living coordination standard rather than a dead mandate. Medieval Latin is not 'post-functional' — it is functional-but-degraded, and the coordination problem (cross-regional intelligibility) remains live throughout the medieval period. The modest rise in extractiveness and theater_ratio over the interval (extraction 0.12→0.18, theater 0.15→0.22) reflects mild institutional ossification (the standard becomes embedded in credentialing, some performative knowledge-signaling emerges) but does not cross into mandatrophy territory. The constraint remains scaffold-like in structure (temporary correction program with implicit sunset: once the standard is re-established, active correction becomes unnecessary) but is classified as rope because the coordination function dominates the extraction mechanism throughout. If theater_ratio exceeded 0.5 or extractiveness exceeded 0.4, mandatrophy detection would flag the constraint as having outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_frame_dependency,
    'Is this constraint one reading of a contested kernel (correct_latin), where sibling readings (rupture_reading, hybrid_reading) instantiate structurally different constraints with different epsilon values?',
    'Cross-reading comparison: if rupture_reading shows substantially higher extraction (identifiable victims, enforced standard with suppressed alternatives) while continuity_reading shows low extraction (mutual benefit, voluntary adoption), the kernel decomposes into multiple constraints per the epsilon-invariance principle.',
    'If confirmed: three distinct constraint stories, not three perspectives on one constraint. Each reading gets its own beneficiary/victim structure, its own epsilon, its own classification. Network edges via affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_dependency, conceptual, 'Whether this is one reading of a multi-reading kernel').

omega_variable(
    voluntariness_vs_institutional_coercion,
    'To what extent was Classical Latin adoption voluntary (coordination) vs coerced (extraction) through ecclesiastical institutional power?',
    'Historical analysis: (1) penalties for non-adoption (career exclusion, denial of ecclesiastical office), (2) availability of alternatives (vernacular literacy, regional Latin standards), (3) distribution of adoption across power levels (elite-driven vs broad-based).',
    'If substantially coerced: extraction rises, victims appear (vernacular-literate populations excluded from clerical careers), classification shifts toward tangled_rope or snare. If voluntary: remains rope as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_vs_institutional_coercion, empirical, 'Degree of voluntariness in Classical Latin adoption').

omega_variable(
    exemplar_accessibility,
    'Were Classical Latin exemplars (Vergil, Cicero, grammatical treatises) genuinely accessible to medieval scribes and students, or concentrated in elite monastic libraries?',
    'Manuscript census: (1) distribution of classical texts across scriptoria by wealth/prestige, (2) evidence of circulation (loan records, copying networks), (3) correlation between access to exemplars and quality of Latinity in produced texts.',
    'If exemplars were widely accessible: coordination function confirmed. If concentrated in elite centers: extraction mechanism emerges (elite gatekeeping over the ''correct'' standard), beneficiaries narrow to wealthy scriptoria, victims include peripheral scribes denied access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemplar_accessibility, empirical, 'Accessibility of Classical Latin textual exemplars').

omega_variable(
    sibling_relation_uncertainty,
    'Does continuity_reading FORECLOSE rupture_reading (claim that no rupture occurred, making the rupture reading logically impossible within a single framework), or do both readings COEXIST as competing historical interpretations held by different scholarly traditions?',
    'Historiographic analysis: if continuity-reading scholars acknowledge that a rupture interpretation is defensible but incorrect, the relation is coexists_with. If continuity-reading scholars claim the rupture never happened and the rupture reading is incoherent, the relation is forecloses.',
    'Determines the reading_relations declaration in cs_structure. Forecloses is the stronger claim and rarer — most historiographic disputes coexist rather than foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_relation_uncertainty, conceptual, 'Structural relation between continuity and rupture readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_lat_theater_early_medieval, continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cont_lat_theater_carolingian, continuity_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(cont_lat_theater_post_carolingian, continuity_reading, theater_ratio, 6, 0.22).

% Extraction over time
narrative_ontology:measurement(cont_lat_extract_early_medieval, continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cont_lat_extract_carolingian, continuity_reading, base_extractiveness, 3, 0.16).
narrative_ontology:measurement(cont_lat_extract_post_carolingian, continuity_reading, base_extractiveness, 6, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cont_lat_suppress_early_medieval, continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cont_lat_suppress_carolingian, continuity_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement(cont_lat_suppress_post_carolingian, continuity_reading, suppression_requirement, 6, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, information_standard).
narrative_ontology:affects_constraint(continuity_reading, rupture_reading).
narrative_ontology:affects_constraint(continuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The continuity reading is one of three readings of the 'correct Latin' kernel. The three readings instantiate structurally distinct constraints with different epsilon values, beneficiary/victim structures, and enforcement mechanisms. They are not three perspectives on one constraint but three separate constraints linked by their shared kernel. The continuity reading treats medieval Latin as recoverable Classical Latin (low epsilon, rope). The rupture reading treats 'Classical Latin' as an imposed standard suppressing organic medieval forms (higher epsilon, expected tangled_rope or snare). The hybrid reading treats 'correct Latin' as context-dependent (moderate epsilon, expected scaffold or tangled_rope). Network edges model structural influence: continuity_reading influences rupture_reading and hybrid_reading by establishing the baseline philological claim that shapes later historiographic disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
