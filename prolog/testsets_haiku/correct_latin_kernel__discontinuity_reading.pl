% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical Latin Kernel (Discontinuity Reading): Reconstruction as Symbolic Reoccupation
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   The Classical Latin kernel — 'what is correct Latin?' — divides into
 *   three reading communities. This story instantiates the discontinuity
 *   reading: Medieval Latin is treated as a linguistically distinct system,
 *   not a corrupted continuation of Classical Latin. Reconstruction of lost
 *   Classical forms is framed as recovery of true structure obscured by
 *   medieval scribal and linguistic change. This reading coordinates the
 *   humanist curriculum and philological scholarship around a single
 *   Classical standard while extracting prestige and resources from Medieval
 *   Latin scholarship. The constraint operates as tangled_rope: it solves a
 *   real coordination problem (enabling consistent Classical text recovery
 *   across centuries and institutions) while asymmetrically extracting from
 *   Medieval specialists who must operate within a framework that treats
 *   their materials as secondary corruptions.
 *
 * KEY AGENTS:
 *   - Classical philologists: institutional agenda-setters who define what counts as correct Latin and control curriculum standards
 *   - Humanist curriculum authorities: institutional beneficiaries who profit from the prestige of Classical education and Classical manuscript recovery
 *   - Medieval Latin scholars: moderate-power payers constrained to operate in a field that treats their primary sources as evidence of error
 *   - Vernacular language advocates: moderate-power payers who face pressure to learn Classical Latin as the marker of educated literacy
 *   - Manuscript conservators: organized beneficiaries receiving resources prioritized toward Classical manuscript recovery
 *   - Printing and publishing establishment: institutional beneficiary profiting from Classical text production and commentary
 *   - Linguistic reconstructionists: analytical observers testing whether Medieval forms are corruptions or evidence of systematic language change
 *   - Textual critics: excluded from setting standards; would argue for treating medieval variants as witnesses to language change rather than errors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.67).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical Latin Kernel (Discontinuity Reading): Reconstruction as Symbolic Reoccupation").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, 'e91cf321-1450-46d1-84ad-1bced10c6240').
narrative_ontology:cs_kernel_codification('e91cf321-1450-46d1-84ad-1bced10c6240', fixed_text).
narrative_ontology:cs_authority_grounding('e91cf321-1450-46d1-84ad-1bced10c6240', lineage).
narrative_ontology:cs_interpretation_layer_present('e91cf321-1450-46d1-84ad-1bced10c6240').
narrative_ontology:cs_reading_relation('e91cf321-1450-46d1-84ad-1bced10c6240', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e91cf321-1450-46d1-84ad-1bced10c6240', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('e91cf321-1450-46d1-84ad-1bced10c6240', foundational, classical_medieval_linguistic_discontinuity).
narrative_ontology:cs_axiom_status(classical_medieval_linguistic_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('e91cf321-1450-46d1-84ad-1bced10c6240', classical_medieval_linguistic_discontinuity, empirically_contingent).
narrative_ontology:cs_axiom('e91cf321-1450-46d1-84ad-1bced10c6240', foundational, reconstruction_via_textual_authority).
narrative_ontology:cs_axiom_status(reconstruction_via_textual_authority, holdable).
narrative_ontology:cs_axiom_grounding('e91cf321-1450-46d1-84ad-1bced10c6240', reconstruction_via_textual_authority, conventional).
narrative_ontology:cs_reference_frame('e91cf321-1450-46d1-84ad-1bced10c6240', classical_linguistic_authenticity).
narrative_ontology:cs_drift_state('e91cf321-1450-46d1-84ad-1bced10c6240', contemporary_modern_linguistics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e91cf321-1450-46d1-84ad-1bced10c6240', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, humanist_curriculum_authorities).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, vernacular_language_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the constraint routes prestige and resources toward Classical materials and away from Medieval sources without any compensation or acknowledgment that Medieval scholars are operating in a degraded epistemic position. Suppression is high (0.67) because the constraint persists through institutional enforcement — curriculum requirements, publishing filters, conservation priorities — that actively suppress alternative readings of Medieval Latin as a legitimate linguistic system. Theater ratio is moderate-high (0.41) because increasing shares of the enforcement machinery go to maintaining the purity narrative rather than to actual Classical reconstruction: once core texts are recovered, much scholarship becomes commentary and re-edition of established texts, yet the prestige framework requires treating this secondary work as the true scholarship. The measurement series show extraction and suppression accumulating from the Renaissance through the 19th-20th centuries as institutional infrastructure hardens around the Classical standard and Medieval materials become progressively devalued in curriculum and publishing. Theater ratio rises because enforcement shifts from discovery (finding Classical texts) to gatekeeping (preventing Medieval readings from displacing Classical authority). Resistance is moderate (0.52) because Medieval Latin scholarship persists and modern linguistic evidence increasingly challenges the corruption framing, but institutional power remains concentrated in Classical philology.
 *
 * PERSPECTIVAL GAP:
 *   From the classical philologist's seat, the constraint solves the urgent problem of Classical textual recovery and enables coherent humanist education — a genuine coordination function. From the Medieval scholar's seat, the same constraint operates as institutionalized devaluation of their materials and expertise — they can publish, but only as correction or commentary on Classical texts, never as the primary object of linguistic inquiry. The engine computes this asymmetry from the beneficiary/victim declarations and the power/exit differentials: institutional power + arbitrage exit (Classical scholars can publish broadly) produces low directionality; moderate power + constrained exit (Medieval scholars trapped in a field that devalues their materials) produces high directionality. The two seats experience structurally different constraints from the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists enter as institutional beneficiaries: they define standards, receive prestige and publishing prominence, control curriculum, direct conservation resources. Their directionality is near the beneficiary end (d ≈ 0.15–0.25) because the constraint routes goods toward them without extracting from them in return. Medieval scholars enter as victims: they bear the cost of operating in a framework that treats their materials as secondary, constrains their publishing options, receives lower conservation funding, and demands they prove Medieval innovations are rule-governed before they can claim legitimacy. Their directionality is near the target end (d ≈ 0.75–0.85) because extraction is substantial and their alternatives are constrained by institutional structure. Manuscript conservators sit as beneficiaries with constrained exit (d ≈ 0.20–0.30): they receive resource priority but cannot exit the institutional framework. Printing establishments are institutional beneficiaries with arbitrage (d ≈ 0.10–0.20): they can publish medieval texts if they choose, but institutional demand flows Classical, so they follow the prestige. No directionality overrides are needed; the structural data produces appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint fails mandatrophy resolution if Medieval Latin is treated as a distinct system with its own internal grammar, not as corrupt Classical Latin. In that case, the founding problem (distinguishing authentic Classical from medieval corruption) dissolves — there is no corruption to distinguish, only language change to describe. The constraint then persists purely through institutional inertia and prestige, making it piton-candidate territory. However, the discontinuity reading can maintain its mandatrophy by asserting that reconstruction of lost Classical structure is still the primary goal and Medieval forms are still evidence of loss rather than change. The reading's survival depends on this normative framing persisting as the institutional default. Linguistic evidence increasingly challenges this framing: if Modern Romance languages can be traced through Medieval Latin, then Medieval forms are not losses but continuations. The constraint handles this evidence by treating it as outside the domain (Romance linguistics, not Latin philology), which is itself a suppression mechanism — excluding empirical counter-evidence from the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_language_change,
    'Are Medieval Latin innovations genuinely corruptions of a stable Classical system, or evidence of systematic language change following internal grammatical rules?',
    'Comparative historical-linguistic analysis of Medieval Latin phonology, morphology, and syntax; comparison with Romance language evolution; testing whether medieval variations follow rule-governed patterns versus random error; study of Medieval Latin scribal practices and regional variation.',
    'If Medieval forms follow rule-governed patterns and trace to Romance successors, Medieval Latin is a linguistic system, not a corruption, and the discontinuity reading loses its central empirical warrant. The constraint would then be exposed as enforcing a prestige hierarchy rather than recovering objective truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_language_change, empirical, 'Whether Medieval forms are corruptions or linguistic change.').

omega_variable(
    classical_purity_as_value_judgment,
    'Is the preference for ''correct'' Classical Latin grounded in objective linguistic superiority, or is it a normative choice embedded in Renaissance humanist ideology?',
    'Genealogical analysis of how the Classical purity standard emerged in 15th–16th century humanism; examination of whether linguistic arguments for Classical superiority rely on empirical claims or aesthetic/ideological preferences; comparison with how other languages treat historical variation (e.g., English toward Old English, German toward Middle High German).',
    'If Classical purity is a normative choice rather than an empirical finding, the entire enforcement structure becomes suspect — it is institutional power masquerading as linguistic fact. Medieval Latin scholarship would be reclassified from secondary/corrupted to historically and linguistically primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classical_purity_as_value_judgment, conceptual, 'Whether Classical purity is empirically grounded or ideologically chosen.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.67) entirely structural — institutional barriers to Medieval publishing, funding, curriculum presence — or is part of it internalized by Medieval scholars who accept the corruption framing as legitimate?',
    'Study of Medieval Latin scholars'' professional narratives, publishing choices, and career decisions; exit analysis: do they leave the field, specialize within Medieval constraints, or reproduce the corruption framing in their own work? Post-exit trajectory: do Medieval-trained scholars who move to adjacent fields (Romance linguistics, historical linguistics, paleography) retain the corruption framing or adopt systems-description framing?',
    'If significant internalization exists, the constraint''s effective suppression is higher than structural measures suggest — Medieval scholars carry the constraint''s framing into exit spaces and cannot fully leave its jurisdiction. Removing structural barriers alone would not resolve the constraint; cognitive reframing would be necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree of internalization of the corruption framing by Medieval specialists.').

omega_variable(
    legitimacy_grounding_shift,
    'This reading''s authority is grounded in lineage (the Renaissance humanist tradition of Classical recovery). Has that grounding shifted toward extraction (institutional power defending its own prestige) as the original founding problem (access to authentic Classical texts) became solved?',
    'Historical trace of which authority justifications are invoked across the interval: at t=0 (early humanists), restoration of lost texts is the justification; at t=200–300 (18th–19th century), purity of form and defense against ''barbarism'' dominate; at t=500 (modern), enforcement focuses on maintaining the standard despite mounting linguistic evidence. Shift in justifications signals authority migration from lineage-grounded ("we recover what the ancients wrote") to extraction-grounded ("we maintain the standard because we always have and institutional power depends on it").',
    'If authority has migrated from lineage to extraction, the constraint should be reclassified from tangled_rope (genuine coordination + extraction) toward piton (atrophied coordination + institutional inertia maintaining prestige). Theater ratio rising (0.15 to 0.41) is consistent with this shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_grounding_shift, empirical, 'Whether the constraint''s legitimacy grounding has shifted from lineage to extraction as the founding coordination problem was solved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__discontinuity_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__discontinuity_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement(corr_tr_t250, correct_latin_kernel__discontinuity_reading, theater_ratio, 250, 0.35).
narrative_ontology:measurement(corr_tr_t350, correct_latin_kernel__discontinuity_reading, theater_ratio, 350, 0.4).
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__discontinuity_reading, theater_ratio, 500, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__discontinuity_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__discontinuity_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(corr_be_t250, correct_latin_kernel__discontinuity_reading, base_extractiveness, 250, 0.55).
narrative_ontology:measurement(corr_be_t350, correct_latin_kernel__discontinuity_reading, base_extractiveness, 350, 0.58).
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__discontinuity_reading, base_extractiveness, 500, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(corr_su_t50, correct_latin_kernel__discontinuity_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__discontinuity_reading, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(corr_su_t250, correct_latin_kernel__discontinuity_reading, suppression_requirement, 250, 0.65).
narrative_ontology:measurement(corr_su_t350, correct_latin_kernel__discontinuity_reading, suppression_requirement, 350, 0.67).
narrative_ontology:measurement(corr_su_t500, correct_latin_kernel__discontinuity_reading, suppression_requirement, 500, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the correct_latin_kernel, which decomposes into three structurally distinct constraints: discontinuity_reading (Medieval Latin as distinct system, reconstruction as recovery), continuity_reading (Medieval Latin as natural evolution, reconstruction as internal correction), and hybrid_reading (mixed: morphology continuous, syntax/lexicon require recovery). The three readings compete for institutional authority but are not reducible to a single constraint. Each has distinct beneficiaries, victims, and ε values. The discontinuity reading is the most extractive (0.58) and most theatrically performative (0.41) because its founding coordination problem (recovering lost Classical texts) is largely solved, but its enforcement machinery persists. The continuity reading treats Medieval materials as primary linguistic evidence (lower extraction). The hybrid reading splits the difference. All three link via network.affects_constraints to acknowledge their mutual institutional competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
