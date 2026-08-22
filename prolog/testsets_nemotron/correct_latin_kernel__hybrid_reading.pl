% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Hybrid Reading of Correct Latin: Morphology Continuous, Syntax/Lexicon Reconstructed
 *   domain: intellectual/historical/philological
 *
 * SUMMARY:
 *   The hybrid reading of 'correct Latin' holds that core morphology
 *   (inflectional systems, case endings, verbal paradigms) shows genuine
 *   continuity from Classical through Medieval Latin, while syntax (word
 *   order, clause structure, prepositional governance) and lexicon
 *   (vocabulary replacement, semantic shift, neologism) required active
 *   textual recovery from Classical models during the Carolingian and
 *   Renaissance reforms. Reconstruction was therefore 'layered':
 *   morphological continuity provided a scaffold, but syntactic and lexical
 *   norms were reimposed from texts. This reading instantiates one position
 *   in the contested kernel 'correct_latin_kernel' alongside
 *   continuity_reading (all evolution is natural) and discontinuity_reading
 *   (Classical and Medieval are distinct systems; reconstruction is symbolic
 *   reoccupation). The constraint operates in philological practice: critical
 *   editions normalize orthography and morphology silently but flag syntactic
 *   'corrections'; dictionaries mark medieval forms as 'corrupt' or 'late'
 *   while treating Classical forms as standard; pedagogical grammars teach
 *   reconstructed syntax as 'correct Latin'.
 *
 * KEY AGENTS:
 *   - philologists_reconstructing_syntax: Primary agenda_setter (institutional/biographical) — defines reconstruction norms, controls critical apparatus
 *   - editors_establishing_critical_texts: Primary beneficiary (organized/biographical) — gains editorial authority from reconstruction mandate
 *   - scholars_claiming_continuity_through_morphology: Secondary beneficiary (organized/biographical) — uses morphological continuity to legitimize syntactic reconstruction
 *   - medieval_manuscript_forms_discarded_as_corrupt: Primary victim (powerless/civilizational) — manuscript authority overridden by textual recovery
 *   - vernacular_developments_treated_as_deviations: Secondary victim (powerless/civilizational) — Romance emergence framed as degeneration from Latin norm
 *   - scholars_reading_medieval_latin_on_its_own_terms: Excluded observer (moderate/biographical) — medieval Latin specialists marginalized by correctness constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.35).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Hybrid Reading of Correct Latin: Morphology Continuous, Syntax/Lexicon Reconstructed").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual/historical/philological").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '7f1a8483-823b-484a-bdc0-8ed701dc17dc').
narrative_ontology:cs_kernel_codification('7f1a8483-823b-484a-bdc0-8ed701dc17dc', distributed).
narrative_ontology:cs_authority_grounding('7f1a8483-823b-484a-bdc0-8ed701dc17dc', practice).
narrative_ontology:cs_interpretation_layer_present('7f1a8483-823b-484a-bdc0-8ed701dc17dc').
narrative_ontology:cs_reading_relation('7f1a8483-823b-484a-bdc0-8ed701dc17dc', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f1a8483-823b-484a-bdc0-8ed701dc17dc', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('7f1a8483-823b-484a-bdc0-8ed701dc17dc', foundational, morphology_provides_legitimate_scaffold_for_reconstruction).
narrative_ontology:cs_axiom_status(morphology_provides_legitimate_scaffold_for_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('7f1a8483-823b-484a-bdc0-8ed701dc17dc', morphology_provides_legitimate_scaffold_for_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('7f1a8483-823b-484a-bdc0-8ed701dc17dc', foundational, syntax_lexicon_require_textual_recovery_from_classical_models).
narrative_ontology:cs_axiom_status(syntax_lexicon_require_textual_recovery_from_classical_models, holdable).
narrative_ontology:cs_axiom_grounding('7f1a8483-823b-484a-bdc0-8ed701dc17dc', syntax_lexicon_require_textual_recovery_from_classical_models, conventional).
narrative_ontology:cs_reference_frame('7f1a8483-823b-484a-bdc0-8ed701dc17dc', carolingian_renaissance_reform_mandate).
narrative_ontology:cs_drift_state('7f1a8483-823b-484a-bdc0-8ed701dc17dc', contemporary_philological_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f1a8483-823b-484a-bdc0-8ed701dc17dc', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, philologists_reconstructing_syntax).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, editors_establishing_critical_texts).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, scholars_claiming_continuity_through_morphology).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_manuscript_forms_discarded_as_corrupt).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, vernacular_developments_treated_as_deviations).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, scholars_reading_medieval_latin_on_its_own_terms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, scholars_claiming_continuity_through_morphology).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, morphological_continuity_thesis).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, textual_recovery_as_legitimate_reconstruction_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the norms of 'correct' Latin syntax and lexicon through critical editions, grammars, and pedagogical materials. Control the apparatus criticus that marks medieval forms as 'corrupt' or 'restored'. Gain professional authority, editorial control, and disciplinary coherence from the reconstruction mandate. Can move between subfields (Classical, Medieval, Romance) — exit is arbitrage-grade because their expertise transfers across the constraint's boundary.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, philologists_reconstructing_syntax, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, philologists_reconstructing_syntax, beneficiary).

% Produce critical editions that normalize morphology silently but flag syntactic/lexical 'corrections' in apparatus. Their editorial authority derives from the hybrid reading's mandate: they are the legitimate reconstructors. Exit is constrained — leaving the reconstruction framework means abandoning the critical edition paradigm that structures their field, but they could shift to diplomatic editions or manuscript-focused work.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, editors_establishing_critical_texts, beneficiary,
    organized, biographical, constrained, global).

% Use morphological continuity as evidence that Latin 'remained Latin' — this legitimizes their work on medieval texts within Classical frameworks. But they pay when their syntactic analyses of medieval texts deviate from reconstructed norms; their work is marginalized if it treats medieval syntax as coherent rather than 'degenerate'. Exit is constrained: they can pivot to Romance linguistics or medieval Latin studies, but the hybrid reading's authority shapes funding, publication, and hiring across all Latin-adjacent fields.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholars_claiming_continuity_through_morphology, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, scholars_claiming_continuity_through_morphology, payer).

% The actual manuscript witnesses — their spellings, word orders, vocabulary choices, syntactic constructions — are systematically normalized in critical editions, marked as 'corrupt' in grammars, and excluded from 'correct Latin' corpora. They have no voice; their authority is overridden by the reconstruction norm. Exit is trapped: the manuscripts cannot speak, and the scholars who might defend them are disciplined by the same constraint.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_manuscript_forms_discarded_as_corrupt, payer,
    powerless, civilizational, trapped, global).

% Early Romance forms emerging in medieval Latin texts (prepositional case replacement, analytic verb forms, lexical innovations) are framed as 'degeneration' or 'corruption' of Latin rather than as the birth of new languages. The hybrid reading's syntax/lexicon reconstruction treats these as errors to be corrected. Exit is trapped: the vernaculars cannot retroactively change how their Latin-stage forms were classified, and Romance linguistics emerged partly in reaction to this framing.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_developments_treated_as_deviations, payer,
    powerless, civilizational, trapped, continental).

% Medieval Latin specialists who treat medieval syntax, vocabulary, and orthography as coherent systems rather than deviations from a Classical norm. They are structurally excluded from 'mainstream' Classical philology, critical edition series, and Latin pedagogical canon. Their exit is identity_locked: their professional identity is constituted through reading medieval Latin as a legitimate system; leaving the field means abandoning the intellectual project that defines them, but staying means permanent marginalization by the hybrid reading's authority.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholars_reading_medieval_latin_on_its_own_terms, excluded,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared scaffold for Latin textual work: morphological continuity allows philologists across periods to communicate, edit, and teach using a stable inflectional framework. The 'layered' premise coordinates effort by distinguishing what can be taken as given (morphology) from what requires active reconstruction (syntax/lexicon).
% TRANSFER_FUNCTION: Moves editorial authority and definitional power over Latin syntax and lexicon from medieval manuscript witnesses (and the vernacular developments they instantiate) to modern philologists and editors who reconstruct Classical norms. The transfer is legitimized by the morphological anchor: 'we keep what is continuous, we restore what is not'.
% ABSENT_VOICES: Medieval scribes and authors (cannot speak); early Romance speakers (their Latin-stage productions classified as errors); contemporary scholars in non-European Latin traditions (Neo-Latin, missionary Latin, scientific Latin) whose textual practices don't fit the Classical/medieval binary — all would object to the 'corrupt' label but are not in the conversation.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, critical editions would shift to diplomatic or multi-layered presentation; medieval Latin syntax would be studied as a coherent system; Romance emergence would be narrated as innovation rather than degeneration; the philological discipline would lose its central organizing distinction between 'continuous' and 'reconstructed' domains.
% FOUNDING_PROBLEM: The Carolingian and Renaissance reforms needed to restore a uniform Latin for administration, liturgy, and education across fragmented territories. Manuscripts showed massive variation in spelling, grammar, and vocabulary. The founding problem was: how to produce a single authoritative Latin from divergent witnesses?
% FOUNDING_PROBLEM_CORROBORATION: Historians of education (not philologists) attest that Latin's administrative/liturgical uniformity mandate ended by 1700 as vernaculars took over. The hybrid reading's persistence in 19th-21st century critical editions and pedagogical grammars serves disciplinary coherence, not the founding administrative problem. No non-beneficiary source attests the founding problem as live; the beneficiaries themselves (philologists) now frame the mandate as 'textual scholarship' rather than 'administrative uniformity'.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).
:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the hybrid reading's partial extraction: it legitimizes morphological continuity (coordination function) while extracting authority from medieval syntactic/lexical forms (extraction function). Suppression (0.35) is moderate — the constraint operates through editorial norms and pedagogical canon formation rather than direct coercion, but manuscript variants are systematically normalized away in critical editions. Theater ratio (0.28) captures the performative dimension: the 'layered' narrative presents as scholarly nuance but the boundary between continuous and reconstructed domains is often drawn to protect the reconstruction project's authority. Accessibility collapse (0.45) and resistance (0.55) are middling: alternatives exist (medieval Latin studies, vernacular philology) but are institutionally marginalized. The claimed_type 'tangled_rope' reflects the genuine coordination function (morphological continuity as shared scaffold) combined with asymmetric extraction (syntax/lexicon reconstruction as authority over medieval forms).
 *
 * PERSPECTIVAL GAP:
 *   From the philologist/editor seat (agenda_setter/beneficiary), the constraint appears as genuine coordination: morphological continuity provides a real anchor, and textual recovery solves the genuine problem of establishing authoritative texts. From the medieval manuscript/vernacular seat (victim), the same structure appears as extraction: their forms are labeled 'corrupt' not because they fail communication but because they deviate from a reconstructed norm. The scholar reading medieval Latin on its own terms (excluded) sees a constraint that claims to describe continuity but actually enforces a Classical ideal. The engine computes this divergence from the structural data — the hybrid reading's 'layered' premise creates the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Philologists and editors are structural beneficiaries: they control the reconstruction apparatus, gain editorial authority, and define the 'correct' norm — directionality near beneficiary end (d ~ 0.2). Medieval manuscript forms and vernacular developments are structural targets: their authority is overridden by the reconstruction norm, with no exit from the 'corrupt' label — directionality near target end (d ~ 0.8). Scholars claiming continuity through morphology occupy a dual position: they benefit from the morphological anchor but pay when their syntactic analyses deviate from reconstructed norms — directionality near symmetric (d ~ 0.5). Scholars reading medieval Latin on its own terms are excluded: their exit is identity_locked (professional identity constituted through the very constraint they contest) — directionality modulated toward target despite moderate power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing authoritative Latin texts for education, liturgy, and administration) was live in 800-1500 but substantially resolved by 1800 as national vernaculars displaced Latin's administrative function. The constraint persists through institutional inertia (philological disciplines, critical edition practices, pedagogical canons) rather than solving its original problem. The 'layered' narrative prevents mislabeling: pure extraction would ignore the genuine morphological coordination; pure coordination would ignore the systematic discarding of medieval syntactic/lexical authority. The hybrid reading's mandatrophy is resolved in the sense that its original administrative/educational mandate is dead, but the constraint persists as a disciplinary boundary marker — a piton-like residue with tangled_rope extraction profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested ''correct_latin_kernel'' rather than a standalone constraint on Latin correctness?',
    'Compare structural predictions of this reading against continuity_reading and discontinuity_reading; if they produce divergent ε values, beneficiary/victim structures, or seat classifications for the same empirical domain, the kernel decomposition is validated.',
    'If validated, this story must link to sibling readings via network.affects_constraints and cs_structure.reading_relations; if not, the constraint_id should be renamed to reflect a standalone claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid_reading is properly a kernel reading of correct_latin_kernel').

omega_variable(
    morphology_syntax_boundary,
    'Where exactly does the boundary fall between ''continuous morphology'' and ''reconstructed syntax/lexicon'' — is it a clean structural divide or a gradient of reconstruction intensity?',
    'Analyze specific cases: (1) case system erosion → prepositional syntax (morphology or syntax?), (2) verbal system reduction → periphrastic forms (continuous innovation or textual recovery?), (3) lexical replacement rates across semantic fields. Measure whether practitioners agree on the classification of borderline phenomena.',
    'A clean divide supports the hybrid reading''s structural coherence; a gradient suggests the reading imposes a false dichotomy on a continuous reconstruction spectrum, potentially inflating extraction claims against medieval forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_syntax_boundary, empirical, 'Boundary precision between continuous and reconstructed domains in the hybrid reading').

omega_variable(
    reconstruction_legitimacy_criteria,
    'What makes a reconstruction ''layered reoccupation'' rather than ''symbolic imposition'' or ''internal correction'' — are there criteria internal to the hybrid reading, or does the distinction collapse under scrutiny?',
    'Trace the historical debate: when did philologists begin distinguishing ''legitimate recovery'' from ''over-restoration''? Examine critical editions'' apparatus criticus for explicit markers of reconstruction confidence. Compare with discontinuity_reading''s ''symbolic reoccupation'' and continuity_reading''s ''internal correction'' — do they use the same textual evidence differently?',
    'If criteria are internal and coherent, the hybrid reading has a defensible coordination function; if criteria are borrowed from sibling readings or post-hoc, the coordination story may be cover for extraction against medieval manuscript authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_legitimacy_criteria, conceptual, 'Whether layered reoccupation has independent legitimacy criteria or borrows from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_hr_tr_t1800, correct_latin_kernel__hybrid_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(clk_hr_tr_t1850, correct_latin_kernel__hybrid_reading, theater_ratio, 1850, 0.22).
narrative_ontology:measurement(clk_hr_tr_t1900, correct_latin_kernel__hybrid_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(clk_hr_tr_t1950, correct_latin_kernel__hybrid_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(clk_hr_tr_t2000, correct_latin_kernel__hybrid_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(clk_hr_tr_t2020, correct_latin_kernel__hybrid_reading, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(clk_hr_be_t1800, correct_latin_kernel__hybrid_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(clk_hr_be_t1850, correct_latin_kernel__hybrid_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(clk_hr_be_t1900, correct_latin_kernel__hybrid_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(clk_hr_be_t1950, correct_latin_kernel__hybrid_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(clk_hr_be_t2000, correct_latin_kernel__hybrid_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(clk_hr_be_t2020, correct_latin_kernel__hybrid_reading, base_extractiveness, 2020, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clk_hr_su_t1800, correct_latin_kernel__hybrid_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(clk_hr_su_t1850, correct_latin_kernel__hybrid_reading, suppression_requirement, 1850, 0.3).
narrative_ontology:measurement(clk_hr_su_t1900, correct_latin_kernel__hybrid_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(clk_hr_su_t1950, correct_latin_kernel__hybrid_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(clk_hr_su_t2000, correct_latin_kernel__hybrid_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(clk_hr_su_t2020, correct_latin_kernel__hybrid_reading, suppression_requirement, 2020, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.03).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'correct Latin' concept into three structurally distinct readings sharing the kernel 'correct_latin_kernel'. The continuity_reading claims near-zero extraction (mountain-like); the discontinuity_reading claims high extraction but frames it as necessary reoccupation (snare-like); the hybrid_reading claims mixed coordination/extraction (tangled_rope). The ε values differ substantially because the referent (what counts as 'correctness') is operationalized differently in each reading. They are linked via affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
