% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Reading of Usul al-Fiqh: Medinan Practice and Custom Integration
 *   domain: legal/religious/jurisprudential
 *
 * SUMMARY:
 *   The Maliki reading of usul al-fiqh treats the lived practice of Medina
 *   ('amal ahl al-Madina) as carrying independent evidentiary weight
 *   alongside hadith, permits maslaha mursala (public interest unrestricted
 *   by specific text), and integrates customary law ('urf) where it does not
 *   contradict textual sources. This reading is one of four major classical
 *   readings of the same kernel (the sources of Islamic law). It structurally
 *   privileges regional continuity and custom over universalist textual
 *   derivation, creating a tension between coordination (legal stability
 *   across diverse regions) and extraction (the subordination of text-centric
 *   reformist or universalist claims to local practice). The reading is
 *   claimed as a rope (genuine coordination methodology) but operates with
 *   measurable extraction toward textualist seats.
 *
 * KEY AGENTS:
 *   - maliki_jurists: Primary agenda-setter (institutional/constrained) â administer the usul framework and authorize custom integration.
 *   - medinan_community: Primary beneficiary (moderate/identity_locked) â customary practice carries independent legal weight.
 *   - regional_customary_communities: Secondary beneficiary (moderate/constrained) â local 'urf preserved against textual standardization.
 *   - textualist_jurists: Primary payer (powerful/mobile) â textual arguments subordinated to practice and custom within Maliki discourse.
 *   - non_maliki_litigants: Secondary payer (moderate/constrained) â in Maliki jurisdictions, their text-based claims are overridden.
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) â maps the structural divergence across Sunni usul readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.55).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Reading of Usul al-Fiqh: Medinan Practice and Custom Integration").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "legal/religious/jurisprudential").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'f96ab770-5e46-490a-9a7a-e3270ba7df2d').
narrative_ontology:cs_kernel_codification('f96ab770-5e46-490a-9a7a-e3270ba7df2d', fixed_text).
narrative_ontology:cs_authority_grounding('f96ab770-5e46-490a-9a7a-e3270ba7df2d', lineage).
narrative_ontology:cs_interpretation_layer_present('f96ab770-5e46-490a-9a7a-e3270ba7df2d').
narrative_ontology:cs_reading_relation('f96ab770-5e46-490a-9a7a-e3270ba7df2d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('f96ab770-5e46-490a-9a7a-e3270ba7df2d', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('f96ab770-5e46-490a-9a7a-e3270ba7df2d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('f96ab770-5e46-490a-9a7a-e3270ba7df2d', foundational, medinan_practice_independent_weight).
narrative_ontology:cs_axiom_status(medinan_practice_independent_weight, holdable).
narrative_ontology:cs_axiom_grounding('f96ab770-5e46-490a-9a7a-e3270ba7df2d', medinan_practice_independent_weight, conventional).
narrative_ontology:cs_axiom('f96ab770-5e46-490a-9a7a-e3270ba7df2d', foundational, maslaha_mursala_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('f96ab770-5e46-490a-9a7a-e3270ba7df2d', maslaha_mursala_valid_source, instrumental).
narrative_ontology:cs_axiom('f96ab770-5e46-490a-9a7a-e3270ba7df2d', secondary, urf_integration_where_non_contradictory).
narrative_ontology:cs_axiom_status(urf_integration_where_non_contradictory, holdable).
narrative_ontology:cs_axiom_grounding('f96ab770-5e46-490a-9a7a-e3270ba7df2d', urf_integration_where_non_contradictory, conventional).
narrative_ontology:cs_reference_frame('f96ab770-5e46-490a-9a7a-e3270ba7df2d', classical_medinan_continuity).
narrative_ontology:cs_drift_state('f96ab770-5e46-490a-9a7a-e3270ba7df2d', contemporary_modernist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f96ab770-5e46-490a-9a7a-e3270ba7df2d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_maliki_litigants).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, amal_ahl_al_madina_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_principle).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_integration_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Maliki usul framework, determine which Medinan practices and regional customs carry legal weight, and derive rulings for their communities. They are bound by the school's methodological commitments but exercise discretion in admitting custom and unrestricted public interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Their customary worship and social practices are treated as independently probative in Maliki law, giving their local traditions normative force across the Muslim world without requiring individual textual proof for each practice.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_community, beneficiary,
    moderate, generational, identity_locked, regional).

% Local customs that do not contradict textual sources are integrated into Maliki legal rulings, preserving their social and economic arrangements against legal standardization that would override them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_communities, beneficiary,
    moderate, generational, constrained, regional).

% Advocate for legal derivation strictly from authenticated hadith and the Quran. Within Maliki methodological discourse, their textual arguments are systematically weighed below Medinan practice and custom, limiting their influence in Maliki-majority institutions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_jurists, payer,
    powerful, generational, mobile, global).

% Muslims in Maliki-majority jurisdictions who seek rulings based on strict textual derivation find that courts and muftis applying Maliki usul privilege continuous practice and recognized custom over their textual claims.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_maliki_litigants, payer,
    moderate, biographical, constrained, regional).

% Study the structural differences between the four major Sunni usul readings, analyzing how each distributes authority among text, prophetic practice, analogy, and custom.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, trans-generational legal methodology for deriving rulings across diverse Muslim communities, integrating regional custom and the continuous practice of Medina with textual sources to prevent interpretive fragmentation.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy from strict textual derivation and universal analogical reasoning toward the continuous practice of Medina and recognized regional custom, concentrating methodological gatekeeping in the Maliki scholarly tradition.
% ABSENT_VOICES: Hanbali and modernist Salafi textualists who reject independent evidentiary weight for practice and unrestricted maslaha; they exist in the broader Islamic legal discourse but are methodologically marginalized within Maliki usul.
% DISAPPEARANCE_RATIONALE: If the Maliki reading vanished, Maliki-majority legal systems would lose the methodological basis for privileging Medinan practice and regional custom; courts and muftis would shift toward textual derivation or other schools' methods, and regional customs would face direct textual scrutiny rather than enjoying presumptive validity.
% FOUNDING_PROBLEM: How to maintain legal continuity and regional legitimacy for Muslim communities outside Arabia when textual sources are silent, contradictory, or insufficiently specific to local conditions, while preserving a normative link to the practice of the Prophet's city.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians outside the Maliki school attest to the genuine coordination problem of early Islamic imperial expansion across diverse legal cultures; Hanbali and modernist critics contest that the specific solutionâelevating Medinan practice and unrestricted customâwas necessary, arguing textual sources plus restricted qiyas suffice. Corroboration is thus split across schools.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high: the methodology structurally transfers authority from textual proof to custom and practice. Suppression (0.55) is moderate: textualist alternatives are institutionally subordinated but not eliminated. Theater ratio (0.28) is low-moderate: the methodology is largely functional, though some defensive maintenance of Medinan-practice claims occurs under modernist pressure. Accessibility collapse (0.50) reflects that once inside the Maliki framework, strict textual derivation ceases to be a viable methodological move. Resistance (0.45) captures the live opposition from Hanbali, Shafi'i, and modernist textualist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the Maliki jurist seat and the regional beneficiary seats, the reading is experienced as necessary coordination: it preserves regional legal cultures and prevents chaotic textual literalism. From the textualist jurist seat, the same structure is experienced as extraction: it confiscates the epistemic priority of authenticated text and transfers authority to historically contingent practice and jurist discretion. The engine computes this divergence from the same structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan communities and regional customary communities benefit: the constraint subsidizes their practices by granting them independent legal weight, lowering their directionality toward the beneficiary end. Maliki jurists, as agenda-setters administering the framework, derive institutional authority from its maintenance; without being declared beneficiaries, their structural position is intermediate but leans toward low extraction. Textualist jurists bear the costs: their preferred methodology is subordinated, giving them high directionality and amplified effective extraction. Non-Maliki litigants in Maliki jurisdictions experience a similar target position because their claims are overridden by custom-privileging rulings. The divergence between the custom-holding seats and the textualist seats is the engine's primary asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a genuine coordination function and asymmetric extraction. The Maliki reading is not pure extraction because it genuinely coordinates legal life across diverse communities; it is not pure coordination because it systematically disadvantages textualist methodology. The Tangled Rope classification captures this hybridity. Were the coordination function to atrophy (e.g., if custom integration became purely performative), the rising theater_ratio and declining coordination would signal Piton or Snare migration; the temporal measurements are authored to detect such drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinctness,
    'Is the Maliki reading a structurally distinct constraint from the Hanafi reading of the same usul kernel, or merely a parameter modulation within a single methodology?',
    'Cross-reading comparison of epsilon, beneficiary/victim sets, and directionality distributions; if structurally distinct, the kernel decomposes into four separate constraints.',
    'If parameter modulation, the corpus should merge the four stories into one constraint with reading-indexed metrics; if distinct, the current decomposition is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether the Maliki reading constitutes a distinct constraint or a parameter shift.').

omega_variable(
    custom_textual_contradiction_boundary,
    'Who authoritatively determines when ''urf contradicts text, and does this discretion itself constitute an extractive mechanism?',
    'Historical case-study analysis of Maliki rulings where custom was rejected versus admitted; measure inter-jurist consistency.',
    'If discretion is unconstrained and inconsistent, the custom-integration mechanism functions as jurist-authority extraction; if constrained by coherent principles, it remains coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_textual_contradiction_boundary, conceptual, 'Uncertainty about the boundary between valid custom and contradicting text.').

omega_variable(
    authority_erosion_acknowledgment,
    'Does the Maliki scholarly establishment explicitly recognize the contemporary erosion of its methodological authority by textualist and modernist movements?',
    'Survey of contemporary Maliki jurists'' meta-usul writings for acknowledgment of epistemic challenge.',
    'If unacknowledged, drift_state.acknowledged=false is confirmed; if acknowledged, the reading''s drift_state may require revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_erosion_acknowledgment, empirical, 'Whether authority erosion is acknowledged by the interpretive body.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__maliki_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__maliki_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(usul_tr_t75, usul_al_fiqh_method__maliki_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__maliki_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__maliki_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__maliki_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(usul_be_t75, usul_al_fiqh_method__maliki_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__maliki_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__maliki_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(usul_su_t75, usul_al_fiqh_method__maliki_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__maliki_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four structurally distinct constraints (the four Sunni school readings) because each reading assigns different epsilon, beneficiary/victim structures, and directionalities to the same textual kernel. This story instantiates the Maliki reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
