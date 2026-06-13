% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections (Restrictive Reading): Individual Worship Only
 *   domain: international/religious/legal
 *
 * SUMMARY:
 *   The Lausanne Treaty of 1923 concluded the Greco-Turkish War and
 *   established Turkey as a nation-state. Article 39 committed Turkey to
 *   protect Christian and Jewish minorities, guaranteeing them freedom of
 *   worship and autonomy in religious matters. The restrictive reading
 *   interprets this as protecting only individual religious conscience—the
 *   right to worship privately. Institutional questions—property ownership,
 *   ecclesiastical governance, theological education, legal personality of
 *   religious bodies—are read as falling outside Lausanne's scope and subject
 *   to Turkish domestic law. Under this reading, minority religious
 *   institutions can be dissolved, their property confiscated or converted to
 *   state ownership, their educational autonomy foreclosed, and their legal
 *   standing in Turkish courts denied. This is not a natural law or
 *   coordination mechanism; it is a reading of a treaty that serves the
 *   beneficiary (the Turkish state) by narrowing minority protections while
 *   maintaining a veneer of legality. The restrictive reading itself is the
 *   constraint—it is contested, actively enforced against minority
 *   institutions, and extractive of their institutional capacity.
 *
 * KEY AGENTS:
 *   - Turkish state apparatus (Directorate of Religious Affairs, Justice Ministry): agenda-setter; interprets Lausanne, administers property confiscation, controls theological education policy.
 *   - Minority religious institutions (Orthodox, Armenian Apostolic, Evangelical): victims; lose institutional autonomy, property control, educational authority, legal personality.
 *   - Minority clergy: victims; identity-locked, cannot be trained within their communities, lack legal recognition.
 *   - Minority theological education systems: trapped victims; cannot operate under domestic law, cannot credential their own educators.
 *   - Lausanne guarantor states (France, Italy, Greece, Romania, Japan): observers; theoretically supervisory but constrained by diplomatic protocol and reluctance to confront NATO ally.
 *   - European human rights mechanisms (ECHR, Council of Europe): excluded; can invoke ECHR Article 9 but not Lausanne directly under this reading.
 *   - International human rights NGOs: excluded; document violations but have no standing in Turkish law.
 *   - Legal scholars and treaty interpreters: observers; contest whether Article 39 covers institutional autonomy or only individual worship.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.82).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.79).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections (Restrictive Reading): Individual Worship Only").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international/religious/legal").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '5a026fbf-bc0c-4d80-8b3d-1468f22de948').
narrative_ontology:cs_kernel_codification('5a026fbf-bc0c-4d80-8b3d-1468f22de948', fixed_text).
narrative_ontology:cs_authority_grounding('5a026fbf-bc0c-4d80-8b3d-1468f22de948', extraction).
narrative_ontology:cs_interpretation_layer_present('5a026fbf-bc0c-4d80-8b3d-1468f22de948').
narrative_ontology:cs_reading_relation('5a026fbf-bc0c-4d80-8b3d-1468f22de948', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('5a026fbf-bc0c-4d80-8b3d-1468f22de948', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('5a026fbf-bc0c-4d80-8b3d-1468f22de948', foundational, lausanne_individual_worship_only).
narrative_ontology:cs_axiom_status(lausanne_individual_worship_only, holdable).
narrative_ontology:cs_axiom_grounding('5a026fbf-bc0c-4d80-8b3d-1468f22de948', lausanne_individual_worship_only, conventional).
narrative_ontology:cs_axiom('5a026fbf-bc0c-4d80-8b3d-1468f22de948', foundational, domestic_law_supremacy_over_treaty_institutional_clauses).
narrative_ontology:cs_axiom_status(domestic_law_supremacy_over_treaty_institutional_clauses, holdable).
narrative_ontology:cs_axiom_grounding('5a026fbf-bc0c-4d80-8b3d-1468f22de948', domestic_law_supremacy_over_treaty_institutional_clauses, deontological).
narrative_ontology:cs_reference_frame('5a026fbf-bc0c-4d80-8b3d-1468f22de948', turkish_sovereign_interpretation_of_lausanne).
narrative_ontology:cs_drift_state('5a026fbf-bc0c-4d80-8b3d-1468f22de948', contemporary_european_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a026fbf-bc0c-4d80-8b3d-1468f22de948', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_clergy).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_theological_education_systems).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.35→0.82 over 101 years). At 1923, the restrictive reading was nascent—minority institutions were still functioning with substantial autonomy inherited from the Ottoman millet system, and Turkish administrative capacity to enforce institutional dissolution was limited. From 1950 onward, as the Turkish state apparatus consolidated (Directorate of Religious Affairs formalized in 1924, strengthened throughout the century), the ability to confiscate property, close theological schools, and deny legal personality to minority bodies increased. The measurement series reflects this: suppression requirement rises as the state's enforcement machinery becomes more sophisticated and applied more systematically. Theater ratio also rises: by 2024, much of the enforcement activity is ritualistic ('protecting national unity,' 'preventing institutional fragmentation') rather than actual security necessity. The restriction on property and education serves rent extraction, not public safety. Accessibility collapse is high (0.72): once a minority institution understands that the restrictive reading has been adopted by the state, their alternatives collapse—they cannot legally defend their property, cannot credibly operate a school, cannot challenge state interpretation. Resistance is substantial (0.64) because minority communities, diaspora networks, and international advocates continue to challenge the reading through European human rights mechanisms and public diplomacy, even though Turkish domestic law provides no remedy.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state apparatus experiences this as coordination—establishing uniform legal treatment of all religious institutions under a single framework. The minority institutions experience it as extraction—their institutional capacity is transferred to the state without compensation or consent. The guarantor states should experience supervisory authority, but the restrictive reading denies them that seat. The European human rights bodies experience it as a violation of ECHR Article 9, but the restrictive reading asserts their authority does not extend to Lausanne interpretation. The engine's per-seat computation will show divergent classifications: from the state's institutional seat, this may appear as legitimate governance; from the minority institutions' powerless seats, it appears as a snare with high identity-locking of clergy and theological educators.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the beneficiary (d near 0.0): it consolidates control, collects property, controls theological education policy, and faces no cost from the arrangement—it designed and enforces it. Minority institutions are the victims (d near 1.0): they lose institutional autonomy, property, educational authority, and legal capacity. Minority clergy are trapped targets (d = 1.0 or near it): they are identity-locked—their entire professional and spiritual identity is bound to their religious community, making exit devastating. The clergy's only alternatives are either accepting state-controlled ordination (not a real exit, a surrender) or leaving the country. Guarantor states have constrained exit (d moderate): they could invoke supervisory authority, but doing so risks Turkey's position in NATO and Europe, so they stay silent. No directionality override is needed; the structural derivation from beneficiary/victim + power + exit + identity-locking captures the full asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: Turkey needed a framework for managing religious minorities in a new nation-state. But by 2024, the founding problem has shifted. The restrictive reading asserts the problem is 'protecting national unity by preventing religious institutional fragmentation.' The expansive and guarantor readings assert the problem is now 'protecting minority institutional rights as human rights, not threats to unity.' The measurement series shows extraction rising even as the founding-problem justification (security, national unity) remains constant in state rhetoric. This is mandatrophy: the restriction began as a response to a real coordination problem (managing post-Ottoman religious heterogeneity) but has atrophied into pure extraction—consolidating state control over religious institutions and collecting their property. The theater ratio rising from 0.25 to 0.58 documents this: the functional activity (managing security, preventing fragmentation) is dwarfed by performative activity (defending sovereignty, maintaining the interpretive fiction that Lausanne only covers individual worship). The mandatrophy resolves by recognizing that under this reading, the arrangement is a snare, not coordination, and the state's mandate to protect minorities has been inverted into a mandate to extract their institutional capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_institutional_separability,
    'Can individual worship rights be meaningfully exercised without institutional autonomy? Can a person practice their faith if they cannot train clergy, own or control a place of worship, or have institutional representation in secular law?',
    'Comparative analysis: examine whether minorities with institutional autonomy (under expansive or guarantor readings, or in other countries) demonstrate higher religious practice rates, higher institutional participation, and stronger intergenerational transmission of faith than minorities without institutional autonomy. Examine whether individuals forced to worship without institutional backing report their rights as fully exercised or constrained.',
    'If individual worship proves inseparable from institutional autonomy in practice, the restrictive reading''s core premise (that protections can be separated) is false, and the constraint reclassifies toward the expansive reading. If individuals can practice faith without institutional backing (through diaspora networks, underground communities, or private devotion), the restrictive reading''s premise holds and the constraint remains a snare (high extraction from institutional targets, not from individual worshippers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_institutional_separability, empirical, 'Whether institutional autonomy is functionally inseparable from individual worship rights.').

omega_variable(
    treaty_interpretation_authority,
    'Who has legitimate authority to interpret Lausanne: Turkey alone (as a sovereign state bound by the treaty), the guarantor states collectively (as signatories with supervisory responsibility), or international human rights bodies applying general principles of religious freedom to the treaty''s language?',
    'This is not empirically resolvable within Turkish law. Resolution requires either: (a) international consensus on the Vienna Convention''s rules of treaty interpretation applied to Lausanne, (b) political agreement among guarantor states to enforce a joint interpretation, or (c) a supranational court (ECHR, ICJ) issuing a binding decision. Each resolution path would favor one reading over the others.',
    'If Turkey retains sole authority, the restrictive reading persists. If guarantor states or international bodies gain authority, the expansive or guarantor readings could prevail, reversing the extraction and restoring institutional autonomy to minorities. This is a pure allocation-of-authority question with direct consequences for whether the constraint operates as a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_interpretation_authority, conceptual, 'Who legitimately interprets Lausanne—Turkey, guarantor states, or international human rights bodies?').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.79) primarily structural (state coercive apparatus preventing property recovery and theological education) or internalized (minority communities have internalized the restrictive reading and do not resist its enforcement)?',
    'Post-exit trajectory analysis: if Turkey were to formally recognize the expansive or guarantor reading, would minorities immediately recover institutional capacity, or would psychological/cultural factors (internalized acceptance, loss of institutional memory, dependency on state provision) prevent rapid institutional reorganization? Comparative analysis: do minority communities in diaspora or in countries recognizing institutional autonomy show higher institutional activity, suggesting the suppression is structural rather than internalized?',
    'If suppression is primarily structural, the constraint''s effective suppression is correctly measured at 0.79—the state apparatus enforces the restriction actively. If suppression is partially internalized, the measured suppression understates the constraint''s grip (minorities carry the suppression with them even after formal policy changes), and institutional recovery would be slower than the formal change would suggest. This affects prognosis for remedial policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Is the measured suppression structural (state apparatus enforced) or internalized (minorities have accepted the restrictive reading)?').

omega_variable(
    property_confiscation_reclassification_test,
    'Turkish law treats minority religious property as subject to general civil and administrative law (confiscation, conversion, bureaucratic dissolution). Is this treatment a neutral application of general law (favoring neither majority nor minority religion) or a discriminatory application that exempts majority Islamic institutions from the same treatment?',
    'Comparative analysis of how Turkish law treats property ownership by Islamic waqfs (endowments), Hindu temples, Buddhist monasteries, and secular civil organizations. If Islamic religious property is exempted from the confiscation and conversion mechanisms applied to minority religious property, the restrictive reading is discriminatory (it applies only to minorities, not to the majority religion), which would elevate the extraction and snare classification.',
    'If the treatment is discriminatory, the restrictive reading moves from a reading-of-a-treaty to a reading-that-targets-minorities, which strengthens the victim classification and makes the snare category more robust. If the treatment is genuinely neutral (all religious institutions face the same restrictions), the reading is internally consistent, though still extractive of institutional capacity from all religions equally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_confiscation_reclassification_test, empirical, 'Is the restrictive reading applied equally to majority and minority religions, or discriminatory?').

omega_variable(
    kernel_reading_displacement_via_european_law,
    'Does Turkey''s membership in the ECHR and Council of Europe create an alternative enforcement mechanism for minority institutional rights that operates outside the Lausanne framework, such that the guarantor reading becomes effective despite Turkey''s rejection of it?',
    'Monitor ECHR judgments against Turkey regarding religious institutional rights (property, education, legal personality). If the ECHR establishes that ECHR Article 9 requires institutional autonomy for minorities, and Turkey is compelled to comply, the guarantor reading becomes de facto operative (minorities gain institutional rights through ECHR, not through Lausanne reinterpretation, but the outcome is the same).',
    'If European mechanisms effectively displace the restrictive reading (by providing minorities a path to institutional recognition outside Turkish law), the snare''s persistence becomes contested—it persists under Turkish domestic law but is being hollowed out by supranational enforcement. The constraint would remain formally a snare within Turkey, but functionally degrading toward a piton (maintained through theater and procedural obstruction rather than active enforcement of a coherent extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_displacement_via_european_law, empirical, 'Can the guarantor reading be effectively enforced through ECHR mechanisms, displacing the restrictive reading?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.25).
narrative_ontology:measurement(laus_tr_t1950, lausanne_minority_protections__restrictive_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(laus_tr_t1980, lausanne_minority_protections__restrictive_reading, theater_ratio, 1980, 0.48).
narrative_ontology:measurement(laus_tr_t2005, lausanne_minority_protections__restrictive_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(laus_tr_t2015, lausanne_minority_protections__restrictive_reading, theater_ratio, 2015, 0.57).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(laus_be_t1950, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(laus_be_t1980, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1980, 0.71).
narrative_ontology:measurement(laus_be_t2005, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(laus_be_t2015, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.42).
narrative_ontology:measurement(laus_su_t1950, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(laus_su_t1980, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(laus_su_t2005, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2005, 0.77).
narrative_ontology:measurement(laus_su_t2015, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Lausanne minority protections kernel family. Three structurally distinct constraints instantiate three readings of Article 39 of the Lausanne Treaty: (1) expansive_reading—institutional autonomy is protected; (2) guarantor_reading—international supervision and enforcement apply; (3) restrictive_reading (THIS constraint)—only individual worship is protected, institutional matters are domestic. The ε values differ substantially: the expansive reading treats Lausanne as a genuine coordination mechanism (low extraction, ~0.2-0.3); the restrictive reading treats it as an enforcement mechanism that extracts minority institutional capacity (high extraction, ~0.8). The readings are not observational variations on one constraint; they are three separate constraints with three separate ε values, arising from a single contested legal text. The restrictive reading forecloses the expansive reading within Turkish domestic legal interpretation but coexists with the guarantor reading in international diplomacy and European forums.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
