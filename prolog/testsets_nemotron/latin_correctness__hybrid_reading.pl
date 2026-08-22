% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Correctness Standard (Hybrid Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of Latin correctness dominated European intellectual
 *   life from the late 15th to mid-17th century. It partitioned Latin into
 *   two legitimate registers: classical Latin for literature, rhetoric, and
 *   belles-lettres (where Ciceronian elegance signaled cultural capital), and
 *   medieval Latin for medicine, law, science, and administration (where
 *   evolved vocabulary and syntax served precision). What began as a
 *   pragmatic compromise — humanists conceded technical domains because
 *   classical Latin lacked the vocabulary — gradually hardened into a status
 *   hierarchy. Literary Latin became the exclusive marker of elite education;
 *   technical Latin was stigmatized as 'barbarous' even while remaining
 *   functionally indispensable. Technical writers faced a double bind:
 *   classicize and lose precision, or remain medieval and lose legitimacy.
 *   The constraint's extraction is the transfer of status from producers of
 *   useful knowledge to custodians of cultural prestige.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.52).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.45).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Correctness Standard (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'ac3b5807-9f96-4cc2-b85d-13d435b0e965').
narrative_ontology:cs_kernel_codification('ac3b5807-9f96-4cc2-b85d-13d435b0e965', distributed).
narrative_ontology:cs_authority_grounding('ac3b5807-9f96-4cc2-b85d-13d435b0e965', lineage).
narrative_ontology:cs_interpretation_layer_present('ac3b5807-9f96-4cc2-b85d-13d435b0e965').
narrative_ontology:cs_reading_relation('ac3b5807-9f96-4cc2-b85d-13d435b0e965', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac3b5807-9f96-4cc2-b85d-13d435b0e965', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('ac3b5807-9f96-4cc2-b85d-13d435b0e965', foundational, domain_specific_linguistic_legitimacy).
narrative_ontology:cs_axiom_status(domain_specific_linguistic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ac3b5807-9f96-4cc2-b85d-13d435b0e965', domain_specific_linguistic_legitimacy, conventional).
narrative_ontology:cs_axiom('ac3b5807-9f96-4cc2-b85d-13d435b0e965', foundational, classical_superiority_in_rhetoric).
narrative_ontology:cs_axiom_status(classical_superiority_in_rhetoric, holdable).
narrative_ontology:cs_axiom_grounding('ac3b5807-9f96-4cc2-b85d-13d435b0e965', classical_superiority_in_rhetoric, deontological).
narrative_ontology:cs_reference_frame('ac3b5807-9f96-4cc2-b85d-13d435b0e965', humanist_restoration_compromise).
narrative_ontology:cs_drift_state('ac3b5807-9f96-4cc2-b85d-13d435b0e965', post_ciceronianism_peak, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac3b5807-9f96-4cc2-b85d-13d435b0e965', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_literati).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_scholars).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medical_practitioners).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, legal_scribes).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scientific_authors).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_specific_linguistic_legitimacy).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, classical_superiority_in_rhetoric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control literary taste, patronage, and educational curricula; their status depends on classical Latin as the exclusive mark of cultural legitimacy. They set the norms for 'proper' Latin in rhetoric, poetry, and belles-lettres while conceding technical domains to medieval usage — a concession that protects their monopoly on prestige without requiring them to master technical vocabularies.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_literati, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, humanist_literati, agenda_setter).

% Professional philologists and editors whose authority rests on adjudicating classical correctness. They benefit from the bifurcation: their expertise is indispensable for literary texts while technical texts are treated as beneath their purview, creating a protected professional niche.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_scholars, beneficiary,
    organized, biographical, constrained, continental).

% Authors of medical, legal, scientific, and administrative texts who must write in medieval Latin for precision and currency, yet face persistent pressure to classicize their language for legitimacy. Their work is treated as linguistically inferior by definition; adopting classical forms degrades technical precision, but refusing marks them as uneducated.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_writers, payer,
    moderate, biographical, constrained, continental).

% Physicians and pharmacologists writing in medieval Latin because classical Latin lacks vocabulary for anatomy, pharmacology, and clinical observation. They are pressured by humanist critics to 'purify' their language, which would render their texts less precise and potentially dangerous for practice.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medical_practitioners, payer,
    moderate, biographical, constrained, continental).

% Notaries, jurists, and chancery officials whose documentary Latin has evolved medieval forms for legal precision. They cannot adopt classical forms without breaking legal continuity and formulaic precision, yet their documents are treated as linguistically corrupt by humanist standards.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, legal_scribes, payer,
    organized, generational, trapped, continental).

% Early scientific writers (botany, astronomy, mathematics) who begin shifting to vernacular to escape the classical/medieval trap entirely. Their exit option is real but costly: vernacular publication fragments the international Republic of Letters and reduces audience.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scientific_authors, payer,
    moderate, biographical, mobile, continental).

% Printers and publishers producing technical works in vernacular languages. They would argue that the bifurcated Latin standard is itself a barrier to knowledge diffusion, but they are excluded from the Latin correctness debate because they operate outside the Latin system entirely.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_printers, excluded,
    moderate, biographical, mobile, regional).

% Contemporary scholars analyzing the historical constraint. They see the full bifurcation structure — the coordination function (domain-appropriate language) and the extraction function (literary prestige extracting status from technical utility) — without being subject to it.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, modern_philologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows different domains to use the Latin variety best suited to their communicative needs: classical forms for rhetorical and literary purposes where aesthetic tradition matters; medieval forms for technical domains where precision, neologism, and living usage matter. Solves the genuine problem of a single language serving both conservative prestige functions and innovative descriptive functions.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and cultural capital from technical writers (who produce useful knowledge in medieval Latin) to humanist literati (who monopolize classical Latin as the badge of elite education). Technical writers pay the cost of either adopting inappropriate classical forms (degrading precision) or accepting subordinate linguistic status.
% ABSENT_VOICES: Vernacular printers and early vernacular authors are structurally excluded — they would argue the entire Latin framework (classical or medieval) is an obstacle to knowledge diffusion, but the debate occurs entirely within Latin. Women technical practitioners (midwives, herbalists, manuscript illuminators) are doubly excluded: from Latin authorship and from the humanist definition of the learned.
% DISAPPEARANCE_RATIONALE: If the bifurcated standard vanished overnight, technical writers would immediately adopt whatever Latin forms served precision (likely accelerating medieval/vernacular development), humanist literati would lose their monopoly on linguistic legitimacy, and the Republic of Letters would reorganize around either a unified vernacular standard or a reformed Latin without the prestige/utility split. The status hierarchy (literary > technical) would collapse.
% FOUNDING_PROBLEM: Late medieval Latin had diverged significantly from classical models while becoming the universal language of learning. Humanists sought to restore classical elegance in literary domains, but technical writers needed the evolved medieval vocabulary for precision. The hybrid reading emerged as a compromise: restore classical norms where they function as cultural capital (literature, rhetoric), permit medieval forms where they function as technical tools (medicine, law, science).
% FOUNDING_PROBLEM_CORROBORATION: Humanist prefaces (Erasmus, Vives) attest the literary restoration motive. Medical and legal manuscript colophons attest the technical necessity of medieval forms. Modern historians of science (e.g., Park, Pomata) corroborate from outside the beneficiary set that the bifurcation enabled technical progress in medicine while preserving humanist cultural capital — the arrangement was not merely a humanist imposition but a functional compromise that later hardened into status extraction.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) reflects moderate but real transfer: technical writers pay legitimacy costs without receiving commensurate benefits. Suppression (0.45) is structural — the bifurcation is enforced through educational gatekeeping, patronage, and the prestige economy, not primarily through coercion. Theater ratio (0.38) rises over the interval as the 'pragmatic compromise' framing increasingly masks status extraction. Accessibility collapse (0.58) is partial: technical writers can and do use medieval forms, but at a status penalty; vernacular exit exists but fragments the Republic of Letters. Resistance (0.42) is real but dispersed: scientific authors lead the vernacular shift, medical writers defend medieval precision, legal scribes entrench formulaic medieval Latin.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist seat, the arrangement is a rope: genuine coordination solving the classical/medieval tension. From the technical writer seats, it is a snare: the coordination story (domain-appropriate language) is cover for a status hierarchy that extracts legitimacy from useful knowledge production. The engine computes this divergence from the declared beneficiaries/victims and exit options. The claimed_type (tangled_rope) reflects the authoring seat's judgment that both functions are real and structurally fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist literati are structural beneficiaries (d ≈ 0.15): they collect cultural capital, control the prestige standard, and have arbitrage-grade exit (they can write in vernacular for prestige without losing status). Classical scholars are secondary beneficiaries (d ≈ 0.25): professional niche protection. Technical writers, medical practitioners, and legal scribes are targets (d ≈ 0.75–0.85): they bear the legitimacy penalty, have constrained or trapped exit, and the constraint's persistence depends on their continued subordination. Scientific authors have mobile exit (vernacular) but pay high audience costs. Vernacular printers are excluded — their structural position is outside the Latin system entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical elegance vs. technical precision) was live in 1450. By 1600, the technical domains had largely migrated to vernacular or developed their own Latin conventions independent of humanist norms. The literary prestige hierarchy persisted after its coordination function atrophied — humanist Latin became a pure status marker with no technical necessity. This is mandatrophy: the mandate (domain-appropriate language) outlived its function and became extraction. The hybrid reading does not resolve the mandatrophy; it crystallizes it by legitimating the bifurcation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'At what point does the domain bifurcation cease to be a genuine coordination compromise and become a status extraction mechanism?',
    'Track the trajectory of technical writers'' exit behavior: when vernacular adoption accelerates despite audience costs, the coordination function has failed and the status hierarchy is self-sustaining. Correlate with humanist discourse: when ''barbarous'' rhetoric intensifies against technical Latin that is functionally adequate, extraction dominates.',
    'If the boundary is early (1480s), the hybrid reading was extraction from the start. If late (1570s+), it was a genuine coordination that degraded into extraction — a mandatrophy case. Classification shifts from rope→tangled_rope or tangled_rope→snare depending on timing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The temporal boundary between genuine domain-appropriate coordination and prestige extraction in the hybrid reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of technical Latin primarily structural (patronage, education, publication gatekeeping) or internalized (technical writers accepting their linguistic inferiority)?',
    'Examine technical writers'' prefaces and private correspondence: do they defend medieval Latin as functionally superior, or apologize for it? Track whether vernacular adoption is framed as pragmatic escape or shameful concession.',
    'If primarily internalized, effective suppression is higher than structural measures suggest — the constraint persists even after structural enforcement relaxes (explaining the 1600–1650 suppression decline without extractiveness collapse). If structural, the decline in suppression tracks the decline in extractiveness more closely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Latin correctness hierarchy.').

omega_variable(
    reading_relations_underdetermination,
    'Does the hybrid reading foreclose the continuity reading, or do they coexist as competing but compatible frameworks within different institutional domains?',
    'Analyze institutional texts: do universities, chanceries, and medical faculties adopt one reading explicitly, or do they operate different readings in different contexts without acknowledging contradiction?',
    'If forecloses, the hybrid reading''s domain partition logically excludes the continuity reading''s universal medieval legitimacy. If coexists_with, both readings operate simultaneously in different institutional niches — the kernel is genuinely underdetermined. If influences, the hybrid reading''s prestige hierarchy creates downstream pressure on continuity-reading institutions to classicize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_underdetermination, conceptual, 'Structural relationship between hybrid and continuity readings of the latin_correctness kernel.').

omega_variable(
    cs_framing_underdetermination,
    'Is the authority structure of the hybrid reading best understood as lineage (humanist tradition transmitting classical norms) or extraction (humanist literati extracting status from technical domains)?',
    'Compare the hybrid reading''s institutional instantiation: where humanist norms are enforced through educational curricula and patronage (lineage), vs. where technical writers are actively penalized for medieval forms despite functional adequacy (extraction). The framing determines whether interpretation_layer_present applies.',
    'Lineage framing → interpretation_layer_present = true (humanist commentary tradition absorbs drift). Extraction framing → interpretation_layer_present = false (no interpretive buffer; direct status enforcement). Changes cs_structure classification and drift_state interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings of the hybrid reading''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1450, latin_correctness__hybrid_reading, theater_ratio, 1450, 0.15).
narrative_ontology:measurement(lati_tr_t1480, latin_correctness__hybrid_reading, theater_ratio, 1480, 0.22).
narrative_ontology:measurement(lati_tr_t1510, latin_correctness__hybrid_reading, theater_ratio, 1510, 0.28).
narrative_ontology:measurement(lati_tr_t1540, latin_correctness__hybrid_reading, theater_ratio, 1540, 0.35).
narrative_ontology:measurement(lati_tr_t1570, latin_correctness__hybrid_reading, theater_ratio, 1570, 0.38).
narrative_ontology:measurement(lati_tr_t1600, latin_correctness__hybrid_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement(lati_tr_t1630, latin_correctness__hybrid_reading, theater_ratio, 1630, 0.4).
narrative_ontology:measurement(lati_tr_t1650, latin_correctness__hybrid_reading, theater_ratio, 1650, 0.38).

% Extraction over time
narrative_ontology:measurement(lati_be_t1450, latin_correctness__hybrid_reading, base_extractiveness, 1450, 0.35).
narrative_ontology:measurement(lati_be_t1480, latin_correctness__hybrid_reading, base_extractiveness, 1480, 0.42).
narrative_ontology:measurement(lati_be_t1510, latin_correctness__hybrid_reading, base_extractiveness, 1510, 0.48).
narrative_ontology:measurement(lati_be_t1540, latin_correctness__hybrid_reading, base_extractiveness, 1540, 0.52).
narrative_ontology:measurement(lati_be_t1570, latin_correctness__hybrid_reading, base_extractiveness, 1570, 0.55).
narrative_ontology:measurement(lati_be_t1600, latin_correctness__hybrid_reading, base_extractiveness, 1600, 0.53).
narrative_ontology:measurement(lati_be_t1630, latin_correctness__hybrid_reading, base_extractiveness, 1630, 0.49).
narrative_ontology:measurement(lati_be_t1650, latin_correctness__hybrid_reading, base_extractiveness, 1650, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1450, latin_correctness__hybrid_reading, suppression_requirement, 1450, 0.25).
narrative_ontology:measurement(lati_su_t1480, latin_correctness__hybrid_reading, suppression_requirement, 1480, 0.35).
narrative_ontology:measurement(lati_su_t1510, latin_correctness__hybrid_reading, suppression_requirement, 1510, 0.42).
narrative_ontology:measurement(lati_su_t1540, latin_correctness__hybrid_reading, suppression_requirement, 1540, 0.48).
narrative_ontology:measurement(lati_su_t1570, latin_correctness__hybrid_reading, suppression_requirement, 1570, 0.5).
narrative_ontology:measurement(lati_su_t1600, latin_correctness__hybrid_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement(lati_su_t1630, latin_correctness__hybrid_reading, suppression_requirement, 1630, 0.42).
narrative_ontology:measurement(lati_su_t1650, latin_correctness__hybrid_reading, suppression_requirement, 1650, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, vernacular_rise_scientific_communication).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, humanist_educational_reform).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, medical_latin_standardization).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three readings: continuity_reading (universal medieval legitimacy), hybrid_reading (this constraint — bifurcated domain legitimacy), rupture_reading (universal classical standard). The hybrid reading is the historical compromise that enabled both literary humanism and technical Latin to coexist; its degradation into status extraction is the constraint story here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__hybrid_reading, organized, 0.8).
constraint_indexing:directionality_override(latin_correctness__hybrid_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
