% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Reading: Source Language Structure as Primary Authority
 *   domain: religious/textual/hermeneutical
 *
 * SUMMARY:
 *   The formal equivalence reading of biblical translation theory asserts
 *   that fidelity to the source language's structure—not communicative
 *   effectiveness in the target language—is the primary measure of a
 *   translation's integrity. Under this reading, a translator's
 *   responsibility is to preserve the formal features of the original Hebrew,
 *   Aramaic, and Greek texts, even when those features create awkwardness or
 *   opacity in the target language. The lay reader's task is to be educated
 *   into understanding the source structures; intelligibility is the reader's
 *   and community's responsibility, not the translator's or the institutional
 *   authority's burden. This reading concentrates hermeneutical authority in
 *   source-language specialists and conservative theological institutions
 *   that control access to the original texts and their 'proper'
 *   interpretation. It structures extraction: lay readers pay an access cost
 *   (education, dependency, cognitive friction); specialists and conservative
 *   communities collect the authority premium. The constraint is CLAIMED as
 *   tangled rope (a genuine coordination function: maintaining textual
 *   stability for doctrinal coherence) while the authored metrics show
 *   substantial extractiveness and enforced suppression of alternative
 *   readings. The gap between claim and metrics is the evidence the engine
 *   measures.
 *
 * KEY AGENTS:
 *   - Hermeneutically conservative communities: institutional beneficiaries; gatekeepers of the textual authority; identity-locked to source-language primacy
 *   - Scholarly specialists: institutional beneficiaries; professional gatekeepers whose authority and funding depend on source-language expertise; mobile exit (can switch to dynamic equivalence but at career cost)
 *   - Non-specialist lay readers: powerless payers; structurally dependent on specialist mediation; constrained exit (can only absorb cost, adopt passivity, or leave the reading tradition)
 *   - Dynamic equivalence advocates: excluded from the conversation; would argue that communicative power in the target language is the proper measure; their exclusion is enforced by authority control
 *   - Critical reconstructive scholars: analytical observers; step outside the formal equivalence frame to examine the historical text itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.61).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Reading: Source Language Structure as Primary Authority").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/textual/hermeneutical").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'a928de14-869b-4de2-b112-bcfdb5e26440').
narrative_ontology:cs_kernel_codification('a928de14-869b-4de2-b112-bcfdb5e26440', fixed_text).
narrative_ontology:cs_authority_grounding('a928de14-869b-4de2-b112-bcfdb5e26440', extraction).
narrative_ontology:cs_interpretation_layer_present('a928de14-869b-4de2-b112-bcfdb5e26440').
narrative_ontology:cs_reading_relation('a928de14-869b-4de2-b112-bcfdb5e26440', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a928de14-869b-4de2-b112-bcfdb5e26440', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('a928de14-869b-4de2-b112-bcfdb5e26440', foundational, source_language_structure_is_semantically_constitutive).
narrative_ontology:cs_axiom_status(source_language_structure_is_semantically_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('a928de14-869b-4de2-b112-bcfdb5e26440', source_language_structure_is_semantically_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('a928de14-869b-4de2-b112-bcfdb5e26440', foundational, lay_reader_education_is_legitimate_cost_of_fidelity).
narrative_ontology:cs_axiom_status(lay_reader_education_is_legitimate_cost_of_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('a928de14-869b-4de2-b112-bcfdb5e26440', lay_reader_education_is_legitimate_cost_of_fidelity, deontological).
narrative_ontology:cs_reference_frame('a928de14-869b-4de2-b112-bcfdb5e26440', source_text_structural_fidelity_as_authority_ground).
narrative_ontology:cs_drift_state('a928de14-869b-4de2-b112-bcfdb5e26440', contemporary_accessibility_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a928de14-869b-4de2-b112-bcfdb5e26440', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, scholarly_specialists).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_lay_readers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint systematically creates access barriers for lay readers and privileges specialist mediation without clear proportionate benefit. The barrier is structural: formal equivalence translations (e.g., NASB, ESV, formal translations of Hebrew biblical texts) require either sophisticated source-language knowledge or extensive interpretive apparatus to yield meaning. Lay readers pay this cost without control over it. Suppression is substantial (0.61) because alternative readings (dynamic equivalence, critical-reconstructive frames) are actively marginalized through institutional authority claims, funding concentration, and pulpit gatekeeping. Theater ratio (0.42) reflects that the enforcement machinery increasingly defends institutional authority rather than a material coordination function: the founding problem (uncontrolled paraphrastic divergence) is substantially mitigated by modern translation discipline, but the formal-equivalence standard persists theatrically as the authoritative measure. The measurement series tracks extraction accumulation (base_extractiveness rising from 0.48 to 0.68 across the 40-unit interval), indicating that as specialization deepened and academic gatekeeping hardened in the 20th and 21st centuries, the constraint became progressively more extractive. Theater ratio also rises, suggesting that the theatrical defense of formal equivalence's authority increased as its material coordination function diminished. Accessibility collapse (0.72) is high because once the formal equivalence standard is understood, lay readers see no alternative: they are excluded from the conversation that sets the standard, and they believe the constraint is a matter of textual principle rather than institutional authority. Resistance (0.58) is moderate: significant resistance from dynamic-equivalence movements and critical scholars, but the institutional apparatus is strong enough to suppress most lay-reader pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the hermeneutically conservative institutional seat, the formal equivalence constraint is a genuine coordination mechanism: it preserves the text's original meaning, prevents interpretive drift, and maintains the anchor for doctrinal authority. From the scholarly specialist seat, it is a system of legitimate expertise: source-language mastery is genuinely difficult and valuable, and the privilege of controlling interpretation is a justified reward. From the lay reader's seat, the same constraint is an access barrier: they want to understand scripture but are told that intelligibility is not the translator's responsibility, and they must acquire expensive expertise or depend on specialist mediation. From the dynamic equivalence advocate's seat, it is a false standard imposed by institutional power: communicative effectiveness IS the measure of fidelity, and formal structure is a red herring that serves specialist gatekeeping. The engine computes these per-seat divergences from the structural data (power, exit_options, beneficiary/victim declarations). The constraint's type will differ from seat to seat because their structural relationships are fundamentally asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   The hermeneutically conservative communities sit at the beneficiary end (d near 0.0): they set the agenda, they control the interpretive apparatus, they benefit from the stability and authority that formal equivalence confers. Their identity is locked to source-language primacy, so exit is cognitive death; but they do not experience that as exit-blocking because the lock is identity-constituting. Scholarly specialists are also beneficiaries (d at 0.2–0.3): they profit from expertise gatekeeping, though they retain mobile exit (switch disciplines or adopt dynamic equivalence at career cost). Non-specialist lay readers are targets (d near 1.0): they bear the access cost and the suppression of alternatives, with constrained exit—they can only absorb the cost, depend on mediation, or leave the reading tradition. Dynamic equivalence advocates are excluded rather than coordinated, so their d is not computed by the standard chain; they would be targets if they were included, but the constraint's persistence depends on keeping them out of the frame. Critical reconstructive scholars are observers with arbitrage exit; they can adopt or reject the formal equivalence frame depending on the question. The claim/metric independence: I claim tangled rope (real coordination function—textual stability for doctrinal coherence—plus real asymmetric extraction) and I author metrics showing substantial extraction and enforcement. The engine will compute per-seat types and measure the gap between my claim and the computed types. That gap is the signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The formal equivalence reading's founding problem was the historical threat of interpretive divergence through loose paraphrastic translation (Septuagint's liberties, Old Latin variants, Targum expansionism). The founding problem was live when translation discipline was weak. Modern translation methodology—standardized philological practice, peer-reviewed critical apparatus, controlled equivalence taxonomies—has substantially mitigated the problem. Yet the formal equivalence standard persists and has intensified (enforcement of exclusivity of scholarly interpretation, resistance to dynamic-equivalence alternatives, gatekeeping through language requirements). This is mandatrophy: the constraint's founding problem is substantially dead, but the constraint is maintained theatrically as institutional authority apparatus. The false-summit omega addresses whether the constraint is grounded in genuine textual necessity or in institutional benefit. The measurement series' rising theater_ratio (0.25 → 0.42) and rising base_extractiveness (0.48 → 0.68) track the constraint's transformation from coordination mechanism to institutional extraction. The constraint persists because hermeneutically conservative communities and scholarly institutions benefit from it; no party bears enough cost to forcibly remove it, and the interpretive community is too ideologically committed to self-correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence_empirical,
    'Does the historical problem of interpretive divergence through loose translation remain a material threat to doctrinal stability, or has modern translation discipline (standardized methodology, peer review, critical apparatus, translation theory) substantially mitigated the risk?',
    'Empirical measurement: (1) Compare doctrinal consistency across reading communities using formal-equivalence vs. dynamic-equivalence translations of theologically significant passages (Christology passages, soteriology, eschatology). (2) Measure whether divergence correlates with translation approach or with exegetical methodology and theological tradition. (3) Examine whether critical scholars'' historical reconstructions produce comparable doctrinal divergence regardless of translation approach.',
    'If modern translation science shows that doctrinal divergence is not significantly higher under dynamic equivalence than formal equivalence, the founding problem is substantially dead and the constraint persists as institutional authority maintenance (piton or snare) rather than coordination. If doctrinal divergence under dynamic equivalence is materially higher, the constraint''s coordination function is vindicated and the classification remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_empirical, empirical, 'Whether the founding problem justifies ongoing constraint enforcement or is substantially resolved.').

omega_variable(
    source_structure_exegetical_informativeness,
    'How much substantive theological or exegetical insight is actually gained by preserving source-language formal structure, relative to the access cost imposed on lay readers? Do the formal-structure features that formal-equivalence translations preserve actually drive different theological conclusions, or is the constraint largely theatrical?',
    'Controlled exegetical analysis: (1) Identify formal-structural features in source texts that formal-equivalence translations preserve (word order patterns, particle sequences, morphological structures, parallelisms). (2) For each feature, measure whether changing it (as dynamic-equivalence translations do) produces a different theological interpretation when examined by trained exegetes blinded to translation source. (3) Aggregate to compute the proportion of preserved structures that actually drive theological divergence.',
    'If preserved structures rarely produce different theological conclusions (low informativeness), the constraint''s extraction cost is not proportionate to its coordination benefit and the classification shifts toward pure snare. If preserved structures frequently produce substantively different theological readings, the constraint is a genuine tangled rope with real coordination function and warranted (though still extractive) enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(source_structure_exegetical_informativeness, empirical, 'Whether formal-structure preservation produces materially different theological conclusions or is primarily institutional gatekeeping dressed as principle.').

omega_variable(
    lay_reader_suppression_mechanism,
    'What proportion of the constraint''s suppression of lay readers is structural (lack of linguistic education, economic barriers to acquiring expertise, cognitive demands) versus internalized (readers believe they deserve limited access, have internalized specialist gatekeeping as legitimate, or have fused their identity with hermeneutical conservatism)?',
    'Post-instruction intervention: (1) Provide lay readers with explicit source-language training (morphology, syntax, semantic range of key terms) alongside access to both formal-equivalence and dynamic-equivalence translations. (2) Measure whether suppression—hesitation, sense of incompetence, deference to specialist authority—dissolves with education (indicating structural suppression) or persists (indicating internalized lock). (3) Track whether trained lay readers shift toward dynamic equivalence or remain committed to formal equivalence despite now having tools to read formally equivalently.',
    'If suppression dissolves with education, it is structural (educable, remediable). If suppression persists despite education, it indicates internalized identity lock: readers have incorporated the gatekeeping narrative into their self-concept (''I am not a biblical scholar, so I cannot understand the text''). Internalized suppression suggests the constraint has become self-enforcing and represents high identity-lock extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_suppression_mechanism, empirical, 'Whether suppression of lay readers is external (structural barriers) or internal (identity fusion with specialist dependence).').

omega_variable(
    formal_equivalence_kernel_reading_origin,
    'Is formal equivalence a reading that emerges from the biblical texts'' own epistemological commitments and authority claims, or is it a constructed reading layered atop the kernel for the benefit of institutional gatekeepers?',
    'Genealogical analysis: (1) Trace the historical emergence of formal equivalence as an authoritative interpretive principle (Reformation emphasis on ad fontes and original languages, post-Enlightenment philology, 19th-century evangelical hermeneutics, 20th-century scholarly consensus). (2) Examine whether formal equivalence is advocated as something the texts themselves demand, or as a methodological commitment imposed by institutions. (3) Assess whether the texts contain meta-textual claims about what constitutes proper interpretation (is there a biblical epistemology that privileges formal structure?).',
    'If formal equivalence is revealed as a historically constructed reading (not demanded by the texts themselves, but adopted for institutional benefit), it loses the status of a natural law and becomes more clearly institutional extraction. The false-summit detection machinery would activate, and the classification would shift decisively toward snare. If formal equivalence can be grounded in the texts'' own authority-claims or epistemological structure, it remains a defensible authoritative reading despite its extractive characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equivalence_kernel_reading_origin, conceptual, 'Whether formal equivalence is a natural-law reading grounded in textual nature or a constructed institutional reading.').

omega_variable(
    kernel_reading_committer_contest,
    'Which reading of the biblical_source_text kernel—formal equivalence, dynamic equivalence, or critical-reconstructive—represents the text''s actual epistemological commitment, and which readings are imposed institutional readings?',
    'Textual hermeneutics: Examine the biblical texts themselves for meta-textual claims about authority, interpretation, intelligibility, and textual stability. Do the texts advocate for formal-structure preservation (supporting formal equivalence)? For communicative clarity (supporting dynamic equivalence)? For historical reconstruction (supporting critical-reconstructive reading)? Or do the texts remain silent, leaving space for multiple readings?',
    'This is a conceptual/preference omega: the answer determines which reading is grounded in textual nature and which are institutional constructions. If the texts are silent or ambiguous (high probability), all three readings are constructed and the classification becomes a matter of institutional power rather than textual necessity. If the texts advocate for one reading, that reading becomes the ground truth and the others become deviations from textual authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_contest, conceptual, 'Which reading of the kernel is grounded in the texts'' actual epistemic commitments vs. which are institutional constructions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__formal_equivalence_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__formal_equivalence_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__formal_equivalence_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(bibl_tr_t32, biblical_source_text__formal_equivalence_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__formal_equivalence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__formal_equivalence_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__formal_equivalence_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__formal_equivalence_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(bibl_be_t32, biblical_source_text__formal_equivalence_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__formal_equivalence_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__formal_equivalence_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__formal_equivalence_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(bibl_su_t24, biblical_source_text__formal_equivalence_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(bibl_su_t32, biblical_source_text__formal_equivalence_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__formal_equivalence_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into three structurally distinct constraints instantiating three readings. Each reading prioritizes a different element (source structure, target intelligibility, historical reconstruction) and generates a different constraint with different extractiveness, different beneficiary sets, and different classifications. Formal equivalence exhibits the highest extractiveness on non-specialist readers because it requires the most mediation. Dynamic equivalence is lowest-extractiveness because it privileges lay-reader comprehension. Critical-reconstructive is highest-extractiveness on generalists but operates in a different frame (historical rather than prescriptive). The three constraints are linked because they compete for institutional authority and academic resources; changes in one reading's legitimacy (e.g., a major textual discovery, a shift in translation practice) downstream affect the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
