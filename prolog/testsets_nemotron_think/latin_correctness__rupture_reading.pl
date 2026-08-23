% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purity Standard (Rupture Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The rupture_reading asserts that Classical Latin is a fixed textual
 *   standard recoverable only by reconstructing ancient sources, and that all
 *   medieval Latin usage constitutes corruption. This constraint operates
 *   through editorial norms, academic curricula, and publishing gatekeeping
 *   to delegitimize medieval Latin. It presents itself as a natural law of
 *   philology (a mountain) but is a constructed normative standard that
 *   extracts epistemic authority and institutional resources from
 *   medievalists and vernacular-adjacent domains. The constraint has
 *   intensified over the last two centuries as classical philology
 *   professionalized and medieval studies were marginalized.
 *
 * KEY AGENTS:
 *   - classical_philologists: Primary agenda_setter (institutional/arbitrage) — defines and enforces the standard
 *   - critical_edition_editors: Primary beneficiary (organized/constrained) — their expertise is validated by the constraint
 *   - medieval_latin_scholars: Primary payer (moderate/identity_locked) — their work is delegitimized, exit requires identity rupture
 *   - vernacular_technical_domains: Payer (powerless/trapped) — forced to adopt anachronistic classical forms
 *   - ecclesiastical_latin_practitioners: Payer (organized/identity_locked) — living liturgical usage branded as corrupt
 *   - linguistic_historians: Observer (analytical/analytical) — sees the constraint as prescriptive imposition on natural language change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.82).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purity Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '595a24b0-2db9-49c2-8ef3-a4297e0a3b6e').
narrative_ontology:cs_kernel_codification('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', fixed_text).
narrative_ontology:cs_authority_grounding('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', lineage).
narrative_ontology:cs_interpretation_layer_present('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e').
narrative_ontology:cs_reading_relation('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', latin_correctness__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', foundational, medieval_latin_is_corrupt).
narrative_ontology:cs_axiom_status(medieval_latin_is_corrupt, holdable).
narrative_ontology:cs_axiom_grounding('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', medieval_latin_is_corrupt, deontological).
narrative_ontology:cs_axiom('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', foundational, classical_standard_requires_reconstruction).
narrative_ontology:cs_axiom_status(classical_standard_requires_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', classical_standard_requires_reconstruction, empirically_contingent).
narrative_ontology:cs_reference_frame('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', ancient_classical_corpus).
narrative_ontology:cs_drift_state('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', modern_philology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('595a24b0-2db9-49c2-8ef3-a4297e0a3b6e', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, critical_edition_editors).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classics_departments).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_technical_domains).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, ecclesiastical_latin_practitioners).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, classical_latin_purity_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, textual_reconstruction_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the classical Latin standard through editorial boards, hiring committees, curricula, and grant review. Their authority derives from controlling the canon; they benefit from the constraint by monopolizing the definition of 'correct' Latin and the resources that follow.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, global).

% Produce critical editions adhering to the classical standard. Their specialized expertise is validated and remunerated because the constraint makes classical textual reconstruction the gold standard. Exit means leaving the field or accepting marginalization.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, critical_edition_editors, beneficiary,
    organized, biographical, constrained, global).

% Study medieval Latin texts that the constraint declares corrupt. They must constantly justify their object of study, publish in separate venues, and compete for fewer positions. Their professional identity is fused with medieval Latin; exiting requires abandoning their research identity.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_latin_scholars, payer,
    moderate, biographical, identity_locked, global).

% Scholars of medieval law, medicine, theology, and administration whose source texts are in medieval Latin. The constraint forces them to either adopt anachronistic classical forms (distorting their sources) or accept delegitimization. They have no institutional leverage to change the standard.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_technical_domains, payer,
    powerless, immediate, trapped, regional).

% The Catholic Church and other Christian traditions use medieval/ecclesiastical Latin as a living liturgical and administrative language. The rupture_reading brands their living usage as corrupt. Their identity is constituted through this usage; exit is not an option without schism.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ecclesiastical_latin_practitioners, payer,
    organized, generational, identity_locked, global).

% Study language change across the Latin continuum. They observe the constraint as a prescriptive imposition that contradicts the empirical reality of linguistic continuity. They neither collect rents nor pay them, but their work is marginalized by the constraint's dominance in classical departments.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed textual standard for classical Latin, enabling consistent editing, translation, and teaching across generations and institutions.
% TRANSFER_FUNCTION: Moves epistemic authority, academic positions, publication venues, and funding from medieval Latin practitioners to classical philologists; delegitimizes medieval forms as 'corrupt' and redirects resources to classical reconstruction.
% ABSENT_VOICES: Medieval Latin practitioners (especially in ecclesiastical and technical domains) who would argue for the legitimacy of their living tradition; vernacular scholars whose source texts are medieval and who are excluded from classical departments; students who would benefit from a unified Latin curriculum but are forced to choose between classical and medieval tracks.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, medieval Latin would be rehabilitated as a legitimate stage of the language; editorial practices would incorporate medieval forms; academic structures would merge classical and medieval Latin positions; funding and publishing would redistribute. The entire field of Latin studies would reorganize.
% FOUNDING_PROBLEM: The need for a stable textual standard for classical Latin after the fragmentation of medieval textual traditions made authoritative texts unreliable.
% FOUNDING_PROBLEM_CORROBORATION: Textual historians (e.g., Reynolds & Wilson, 'Scribes and Scholars') document that by the late 19th century critical editions had established stable texts for virtually all classical authors. The constraint persists not because textual instability remains a problem, but because it now functions as a boundary-maintenance mechanism that allocates prestige and resources. No corroborating source outside the classical philology beneficiary set attests that the founding problem is still live.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) because the constraint redirects authority, positions, and funding from medievalists to classicists while providing a coordination function (stable classical texts) that could be achieved without delegitimizing medieval Latin. Suppression (0.82) is high because the constraint's persistence depends on active exclusion: editorial boards reject medieval forms, hiring committees privilege classical training, funding bodies follow the classical canon. Theater ratio (0.42) reflects that the purity performance (recitation contests, 'correctness' examinations) exceeds the functional need for a stable textual standard. Accessibility collapse (0.85) is high because once the standard is internalized, medieval forms are not merely alternative but 'wrong'. Resistance (0.55) is moderate: medievalists have created parallel institutions (Medieval Latin journals, conferences) but remain structurally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a rope: it solves the coordination problem of textual stability. From the payer seats, it is a snare: the coordination story is cover for extracting authority. The engine computes this divergence from the structural data; the authored claimed_type (tangled_rope) reflects the generating model's assessment that both functions are real but extraction dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and editors are structural beneficiaries (d near 0.0): they collect the rents of gatekeeping. Medieval Latin scholars are targets (d near 1.0): they pay through marginalization and must justify their object of study. Vernacular technical domains are trapped payers (d=1.0): they cannot exit because their source texts are medieval. Ecclesiastical practitioners are identity-locked payers: their liturgical identity fuses with medieval Latin, making exit existentially costly. Linguistic historians are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual instability of classical Latin) is largely solved — critical editions have stabilized the corpus. The constraint persists as a boundary-maintenance mechanism: it defines who is a 'real' Latinist and allocates prestige accordingly. This is mandatrophy: the mandate (provide a stable standard) has atrophied, but the constraint remains because it now serves to extract resources from medievalists. The classification as tangled_rope (not snare) prevents mislabeling the genuine coordination function as pure extraction, while the high extractiveness metrics prevent mislabeling the extraction as mere coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is the rupture_reading one framing of a contested kernel (latin_correctness) rather than an independent constraint?',
    'Comparative analysis of sibling readings (continuity_reading, hybrid_reading) to determine whether they share a common kernel commitment or are structurally independent constraints.',
    'If confirmed as a kernel reading, the constraint''s ε and victim structure are reading-indexed; the engine must treat sibling readings as separate constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Committee frame: this constraint is a reading of the latin_correctness kernel.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the classical standard provide a genuine coordination function (stable text for scholarship) or is the coordination story a cover for extracting epistemic authority from medievalists?',
    'Counterfactual: if medieval Latin were rehabilitated tomorrow, would classical scholarship lose its ability to coordinate on texts, or would it simply lose its gatekeeping power?',
    'If coordination is genuine, the constraint is tangled_rope; if cover, it is snare. The engine''s classification will reflect the authored metrics and structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the constraint''s coordination function is structurally real or a legitimating narrative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of medieval Latin structural (academic hiring, editorial boards, funding) or internalized (medievalists accepting the ''corruption'' label)?',
    'Post-exit trajectory: if medievalists who leave classical departments still frame their work in terms of ''deviation from classical norm'', suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint travels with the agent after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the marginalization of medieval Latin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(lati_tr_t0, observed).
narrative_ontology:measurement(lati_tr_t25, latin_correctness__rupture_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(lati_tr_t25, observed).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__rupture_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement_basis(lati_tr_t50, observed).
narrative_ontology:measurement(lati_tr_t75, latin_correctness__rupture_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement_basis(lati_tr_t75, observed).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__rupture_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(lati_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(lati_be_t0, observed).
narrative_ontology:measurement(lati_be_t25, latin_correctness__rupture_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(lati_be_t25, observed).
narrative_ontology:measurement(lati_be_t50, latin_correctness__rupture_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(lati_be_t50, observed).
narrative_ontology:measurement(lati_be_t75, latin_correctness__rupture_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement_basis(lati_be_t75, observed).
narrative_ontology:measurement(lati_be_t100, latin_correctness__rupture_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement_basis(lati_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(lati_su_t0, observed).
narrative_ontology:measurement(lati_su_t25, latin_correctness__rupture_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(lati_su_t25, observed).
narrative_ontology:measurement(lati_su_t50, latin_correctness__rupture_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement_basis(lati_su_t50, observed).
narrative_ontology:measurement(lati_su_t75, latin_correctness__rupture_reading, suppression_requirement, 75, 0.79).
narrative_ontology:measurement_basis(lati_su_t75, observed).
narrative_ontology:measurement(lati_su_t100, latin_correctness__rupture_reading, suppression_requirement, 100, 0.82).
narrative_ontology:measurement_basis(lati_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the latin_correctness kernel. The rupture_reading asserts medieval Latin is corruption; continuity_reading asserts organic continuation; hybrid_reading asserts domain-specific legitimacy. The three readings share the kernel (the Latin textual tradition) but instantiate different constraints with different ε, victim sets, and classifications. They are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__rupture_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
