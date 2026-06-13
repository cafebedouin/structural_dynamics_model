% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin: Hybrid Transmission-Plus-Textual-Correction Reading
 *   domain: intellectual_history/philology/education
 *
 * SUMMARY:
 *   The hybrid reading of Correct Latin positions the Classical form as the
 *   normative standard but acknowledges that medieval practice preserved
 *   legitimate grammatical inheritance and that divergences are correctable
 *   via textual evidence rather than terminal corruptions. This reading
 *   emerged during the Renaissance as scholars gained access to more ancient
 *   manuscripts and could compare medieval usage against older texts. It
 *   differs from pure continuity (medieval evolution is fully legitimate) and
 *   pure discontinuity (medieval forms are simply corrupt) by declaring a
 *   dual-sourced legitimacy: the medieval transmission is real and preserves
 *   Classical elements, but textual reconstruction can and should guide
 *   correction. The constraint's operation coordinates pedagogical authority
 *   around this middle position while extracting epistemic authority from
 *   living practitioners, assigning correctness-judgment to trained
 *   manuscript scholars and textual critics. The reading grounds itself in
 *   evidence (older manuscripts are better access to the original form) and
 *   in continuity (medieval forms are not wholly alien departures). It also
 *   rides on institutional authority (universities, churches, humanist
 *   networks) to enforce the standard and to suppress alternative readings
 *   that would grant equal legitimacy to medieval development or full access
 *   to practitioners' intuitions.
 *
 * KEY AGENTS:
 *   - manuscript_scholars: institutional agenda-setters; control the textual-criticism apparatus and adjudicate correctness via paleographic comparison
 *   - educational_institutions: beneficiaries and secondary agenda-setters; adopt and enforce the hybrid standard in curricula and credentialing
 *   - medieval_tradition_inheritors: identity-locked payers; their embedded practices are reclassified as correctable rather than legitimate, requiring constant adaptation
 *   - living_practice_communities: constrained payers and excluded from authority; their innovations are treated as needing correction rather than development
 *   - humanist_scholars: institutional agenda-setters; championed Classical recovery and textual correction; benefit from prestige of scholarly authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin: Hybrid Transmission-Plus-Textual-Correction Reading").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "intellectual_history/philology/education").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'e40a721f-2e45-47db-9577-cee250b08ac9').
narrative_ontology:cs_kernel_codification('e40a721f-2e45-47db-9577-cee250b08ac9', fixed_text).
narrative_ontology:cs_authority_grounding('e40a721f-2e45-47db-9577-cee250b08ac9', lineage).
narrative_ontology:cs_interpretation_layer_present('e40a721f-2e45-47db-9577-cee250b08ac9').
narrative_ontology:cs_reading_relation('e40a721f-2e45-47db-9577-cee250b08ac9', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e40a721f-2e45-47db-9577-cee250b08ac9', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('e40a721f-2e45-47db-9577-cee250b08ac9', foundational, classical_primacy_with_continuity_concessions).
narrative_ontology:cs_axiom_status(classical_primacy_with_continuity_concessions, holdable).
narrative_ontology:cs_axiom_grounding('e40a721f-2e45-47db-9577-cee250b08ac9', classical_primacy_with_continuity_concessions, deontological).
narrative_ontology:cs_axiom('e40a721f-2e45-47db-9577-cee250b08ac9', secondary, textual_evidence_adjudicates_disputed_forms).
narrative_ontology:cs_axiom_status(textual_evidence_adjudicates_disputed_forms, holdable).
narrative_ontology:cs_axiom_grounding('e40a721f-2e45-47db-9577-cee250b08ac9', textual_evidence_adjudicates_disputed_forms, empirically_contingent).
narrative_ontology:cs_reference_frame('e40a721f-2e45-47db-9577-cee250b08ac9', medieval_transmission_with_classical_ancestry).
narrative_ontology:cs_drift_state('e40a721f-2e45-47db-9577-cee250b08ac9', early_modern_humanist_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e40a721f-2e45-47db-9577-cee250b08ac9', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, manuscript_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, textual_authority_keepers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, living_practice_communities).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_tradition_inheritors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.42 (moderate-high) at interval end because the constraint extracts epistemic authority from practitioners and assigns it to manuscript scholars, even though the fiction of partial continuity is maintained. Suppression measures at 0.38 because the constraint's persistence depends on actively suppressing the alternative reading that would grant full legitimacy to medieval development — scribal traditions must be corrected, not celebrated as evolution. Theater_ratio at 0.28 reflects that the apparatus of textual correction is real scholarly work, but an increasing share of the constraint's enforcement energy defends textual primacy against practitioners' intuitions rather than resolving genuine ambiguity. Measurements show mild rise in extractiveness from t=0 to t=10 (as textual resources accumulate and the correction apparatus strengthens), then plateau (the institutional standard becomes entrenched, no further extraction capacity accrues). The temporal pattern tracks the Renaissance through early modern period: initial rise as humanists consolidate authority, then stabilization as the hybrid reading becomes institutional orthodoxy. Accessibility_collapse at 0.72 because once the hybrid reading is institutionalized and manuscript scholarship becomes the only recognized authority, practitioners cannot credibly appeal to their own intuitions or evolutionary legitimacy. Resistance at 0.55 reflects ongoing monastic and regional scribal traditions that continue medieval practices despite institutional pressure, never fully capitulating to textual standards.
 *
 * PERSPECTIVAL GAP:
 *   The manuscript scholars and educational institutions experience the constraint as legitimate coordination: they are solving a real pedagogical problem (how to teach a dead language when transmission is broken) and using the best available evidence (ancient texts). From their seat, the constraint is rope — genuine coordination with modest overhead. The medieval-tradition inheritors and living-practice communities experience the same structure as asymmetric extraction: their authority is continuously undermined, their innovations are perpetually subject to correction, and they have no standing to authenticate their own usage. From their seat, the constraint is tangled_rope or snare — coordination function exists but rides on enforced subordination. The engine computes both readings from the structural data: beneficiary/victim declarations, power differentials, exit-option constraints. The temporal measurements show mild extraction-accumulation as the hybrid reading consolidates institutional power: early on, when multiple readings compete, practitioners retain some negotiating room; once the hybrid reading becomes administrative orthodoxy (t=10 onward), the correction apparatus no longer faces meaningful challenge and practitioners' options narrow to compliance or silence.
 *
 * DIRECTIONALITY LOGIC:
 *   Manuscript scholars sit at d near 0.1 (beneficiaries): they set the agenda, collect prestige and authority, and face no enforced correction of their own practices. Educational institutions sit near d=0.2 (beneficiaries with minor constraint): they adopt the standard they prefer and enforce it downward; their exit is mobile (they could choose continuity or discontinuity readings instead). Medieval-tradition inheritors sit at d near 0.85 (targets): their practices are constantly subject to external correction, their identity is locked into the tradition they defend, and their exit is blocked by professional and cultural identity fusion. Living-practice communities sit at d near 0.8 (targets): organized enough to mount resistance but constrained by institutional authority and lack of standing in the textual-criticism conversation. The override logic here is straightforward structural derivation: beneficiary/victim declarations + identity-lock for practitioners + institutional power for scholars + mobile exit for educated institutions → directionality that diverges sharply. No overrides needed; the derivation chain captures the actual structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification (false coordination claim covering pure extraction) because it genuinely coordinates a pedagogical function: teaching a dead language requires some standard, and the hybrid reading does reconcile textual primacy with historical continuity. However, the constraint sits at the boundary: if manuscript discoveries dried up and new textual evidence stopped flowing, the correction apparatus would become performative theater, practitioners would recognize the standard as arbitrary, and the constraint would migrate toward piton (enforced by inertia, not by real function). Currently the genealogy holds because new manuscripts are still discovered and textual reconstruction remains intellectually vital. The founding problem (how to teach Latin when medieval practice and Classical texts conflict) is live and contested — different parties genuinely disagree on whether medieval forms are evolved legitimate forms or corruptions needing correction. The constraint's persistence depends on continuous textual work and institutional enforcement; if textual authority eroded (e.g., if manuscript discovery became implausible or if critical theory deconstructed the notion of textual primacy), the constraint would shift toward piton very quickly. The mandatrophy risk is real but not currently realized: the coordination function is genuine, even if asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_grounding,
    'Is the authority of ancient texts over medieval practice grounded in genuine epistemological access to the original form, or in a constructed preference for older witnesses?',
    'Examine manuscript discovery patterns: if newly discovered texts systematically revise prior standards, the authority is provisional and observer-dependent. If new texts converge on stable forms, the authority is more robust.',
    'If observer-dependent, the constraint''s classification shifts from rope toward snare: the textual standard becomes arbitrary institutional choice rather than evidence-grounded correction. If robust, the constraint retains legitimacy as corrective coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_grounding, empirical, 'Whether textual primacy rests on epistemological access or institutional preference.').

omega_variable(
    medieval_form_status,
    'Are medieval Latin divergences legitimate evolutionary development of Classical forms (continuous transmission) or corruptions requiring correction (discontinuous deviation)?',
    'Linguistic analysis comparing medieval forms to Classical predecessors: do they follow regular sound changes, grammatical extensions, and lexical innovation patterns consistent with language evolution, or do they appear random and degenerate?',
    'If legitimate evolution, practitioners gain standing to authenticate their usage and the constraint loses suppression capacity. If corruption, textual correction gains legitimacy and extraction is less costly to the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_form_status, empirical, 'Whether medieval divergences are evolved forms or corruptions.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of medieval-practice intuitions structural (enforced by institutional authority and textual apparatus) or internalized (practitioners have accepted the correction standard as legitimate)?',
    'Examine whether suppression persists after institutional enforcement weakens: if practitioners spontaneously revert to medieval forms, suppression is structural; if they maintain textual forms even without enforcement, suppression is internalized.',
    'If structural, the constraint is more extractive than measured: targets bear the cost of institutional enforcement plus opportunity cost of deviation. If internalized, targets have partially adopted the reading and the constraint is less purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is enforced or internalized among practitioners.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the hybrid reading logically foreclose either the continuity or discontinuity reading, or do all three remain live positions that different parties can coherently hold?',
    'Examine the core premises: hybrid = Classical normative + medieval partially legitimate + textual correction. Continuity = medieval fully legitimate. Discontinuity = Classical exclusive authority. Do any two premises directly contradict such that no framework could hold both?',
    'If foreclosure exists, classify the reading_relations as forecloses rather than coexists_with. If no foreclosure, the relations are coexistential and the kernel remains genuinely contested across different institutional communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether sibling readings logically foreclose each other or coexist as live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t5, correct_latin__hybrid_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(corr_tr_t10, correct_latin__hybrid_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(corr_tr_t20, correct_latin__hybrid_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(corr_tr_t30, correct_latin__hybrid_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(corr_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(corr_be_t5, correct_latin__hybrid_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(corr_be_t10, correct_latin__hybrid_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(corr_be_t20, correct_latin__hybrid_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(corr_be_t30, correct_latin__hybrid_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(corr_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(corr_su_t5, correct_latin__hybrid_reading, suppression_requirement, 5, 0.29).
narrative_ontology:measurement(corr_su_t10, correct_latin__hybrid_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(corr_su_t20, correct_latin__hybrid_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(corr_su_t30, correct_latin__hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(corr_su_t40, correct_latin__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three structurally distinct constraints, each instantiating a different reading. The hybrid_reading (this story) declares Classical forms normative but medieval transmission partially legitimate, subject to textual correction. The continuity_reading treats medieval transmission as fully legitimate evolution. The discontinuity_reading treats Classical texts as exclusive authority with medieval forms as corruption. Each reading has distinct ε (extractiveness of the epistemic authority assignment), distinct stakeholder structures (whose intuitions count as authoritative), and distinct classification. All three readings share the kernel (what counts as correct Latin?) but diverge in authority epistemology. The hybrid reading influences both siblings: it borrows the discontinuity reading's textual-criticism method while constraining it with continuity-reading concessions to medieval legitimacy. None of the three logically forecloses the others within medieval institutional frameworks — they coexist as competing readings held by different factions (continuity in monastic traditions, discontinuity in humanist circles, hybrid in educational institutions). The constraint family's network is fully connected: hybrid affects both siblings; siblings would affect hybrid reciprocally (if continuity gained institutional ground, hybrid's textual standards would weaken; if discontinuity dominated, hybrid's medieval concessions would vanish).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
