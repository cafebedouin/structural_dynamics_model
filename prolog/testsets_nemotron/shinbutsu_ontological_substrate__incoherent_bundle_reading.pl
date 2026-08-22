% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as Enforced Institutional Bundle (Incoherent Bundle Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The Shinbutsu Bunri (separation of kami and buddhas, 1868) is
 *   conventionally narrated as a Meiji restoration of 'pure' Shinto from
 *   Buddhist corruption. This reading inverts that frame: the pre-Meiji field
 *   was a coherent, functional syncretism (honji suijaku) where kami and
 *   buddhas operated as an integrated ontological substrate. The Meiji state
 *   forcibly decomposed this substrate into two state-managed categories
 *   ('Shinto' and 'Buddhism') to construct a national religion legitimizing
 *   imperial sovereignty. The constraint is the ongoing enforcement of this
 *   decomposition — shrines must be 'Shinto', temples 'Buddhist', and their
 *   fusion is legally and ideologically suppressed. The incoherent bundle
 *   reading holds that no unified commitment (kernel) ever existed; the
 *   'Shinto' category is a state construction maintained by extracting
 *   autonomy from shrine/temple networks and imposing contradictory doctrinal
 *   demands on practitioners. The syncretic_fusion_reading and
 *   domain_partition_reading are sibling constraints from the same kernel
 *   (shinbutsu_ontological_substrate) but instantiate different structural
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as Enforced Institutional Bundle (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'f413df56-9fa2-4cef-8f2c-046bb36835c0').
narrative_ontology:cs_kernel_codification('f413df56-9fa2-4cef-8f2c-046bb36835c0', implicit).
narrative_ontology:cs_authority_grounding('f413df56-9fa2-4cef-8f2c-046bb36835c0', extraction).
narrative_ontology:cs_reading_relation('f413df56-9fa2-4cef-8f2c-046bb36835c0', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('f413df56-9fa2-4cef-8f2c-046bb36835c0', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('f413df56-9fa2-4cef-8f2c-046bb36835c0', foundational, no_premeiji_unified_kernel).
narrative_ontology:cs_axiom_status(no_premeiji_unified_kernel, holdable).
narrative_ontology:cs_axiom_grounding('f413df56-9fa2-4cef-8f2c-046bb36835c0', no_premeiji_unified_kernel, empirically_contingent).
narrative_ontology:cs_axiom('f413df56-9fa2-4cef-8f2c-046bb36835c0', foundational, state_constructed_shinto_category).
narrative_ontology:cs_axiom_status(state_constructed_shinto_category, holdable).
narrative_ontology:cs_axiom_grounding('f413df56-9fa2-4cef-8f2c-046bb36835c0', state_constructed_shinto_category, conventional).
narrative_ontology:cs_reference_frame('f413df56-9fa2-4cef-8f2c-046bb36835c0', pre_meiji_fluid_syncretism).
narrative_ontology:cs_drift_state('f413df56-9fa2-4cef-8f2c-046bb36835c0', contemporary, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f413df56-9fa2-4cef-8f2c-046bb36835c0', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, meiji_state).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_ideology_apparatus).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_monks).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_shinto_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_lineage_centrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1868 Shinbutsu Bunri order separating kami and buddhas, then consolidates shrines under state Shinto while absorbing Buddhist infrastructure into the imperial bureaucracy. Gains unified religious authority aligned with modern nation-state sovereignty and the emperor's divine legitimacy. Can redirect enforcement resources at will.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, meiji_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Produces and enforces the doctrinal framework that treats kami as ancestral spirits of the imperial line and Buddhism as a foreign import to be subordinated. Collects prestige, state funding, and control over ritual calendar. Their authority depends on the enforced coherence of the 'Shinto' category the state constructs.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_ideology_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Forced to purge Buddhist elements from shrine complexes, destroy honji suijaku icons, and perform state-mandated rituals. Some gain state stipends and elevated status as 'national ritualists' but lose autonomous institutional memory and must teach contradictory doctrines (kami as both Buddhist manifestations and pure native spirits). Exit means defrocking or underground practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests, beneficiary).

% Subjected to haibutsu kishaku (abolish Buddhism, destroy Shakyamuni): temple lands confiscated, monks laicized or forced into Shinto priesthood, statues melted for cannon bronze. Some lineages survive by rebranding as 'funeral Buddhism' for state-approved ancestor rites. Cannot exit without abandoning vocation and community; the doctrinal contradiction (Buddhism as both suppressed and co-opted) is baked into daily practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_monks, payer,
    organized, biographical, constrained, regional).

% Required to register at a state shrine (ujiko) while maintaining household Buddhist altars for ancestor rites. Taught that kami and buddhas are separate (state doctrine) while inherited practice treats them as fused (honji suijaku). The contradiction is lived, not chosen: funerals are Buddhist, New Year is Shinto, ancestral tablets are both. No exit without severing family and community ties.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Mountain ascetics whose practice explicitly fuses kami, buddhas, and Taoist deities — the living embodiment of the syncretism the state declares incoherent. Banned outright in 1872; forced to choose Shinto priesthood, Buddhist monkhood, or secular life. Their exclusion is the proof that the enforced separation is artificial: they cannot be categorized without destroying their practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shugendo_practitioners, excluded,
    moderate, biographical, trapped, regional).

% Analyze the Shinbutsu Bunri as a modern construction of 'religion' and 'Shinto' imposed on a pre-modern field where no such categories existed. Their analysis does not change the constraint's operation but documents the category violence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, modern_religious_studies_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function exists. The arrangement solves no collective-action problem; it imposes a state-defined taxonomy on a field that previously operated through fluid, context-dependent kami-buddha interaction. The 'coordination' claimed (unified national ritual) is the extraction mechanism itself.
% TRANSFER_FUNCTION: Moves institutional autonomy, land, ritual authority, and doctrinal coherence from shrine/temple networks and lay communities to the Meiji state and its imperial ideology apparatus. The state extracts legitimacy (emperor as divine descendant) and control (unified ritual calendar, conscription-era moral education) by forcibly disentangling a fused religious field.
% ABSENT_VOICES: Shugendo practitioners (banned), crypto-Buddhist lineages (underground), folk practitioners whose domestic rites never fit the shrine/temple binary, and pre-Meiji doctrinal traditions that treated honji suijaku as metaphysics not policy. They are absent because the state's classification made their positions unspeakable in official discourse.
% DISAPPEARANCE_RATIONALE: If the enforced separation vanished, shrine and temple networks would re-fuse organically (as they did in folk practice despite state prohibition), honji suijaku iconography would return to shrine complexes, and the state Shinto / sect Shinto / Buddhism tripartite structure would collapse. The modern 'Shinto' category would lose its state-enforced coherence.
% FOUNDING_PROBLEM: The Meiji state needed a unified national religion to legitimize the emperor's restored sovereignty and compete with Western Christianity as a civilizational peer. The pre-existing kami-buddha fusion was treated as 'superstition' incompatible with modern nation-statehood.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (legitimizing imperial sovereignty through a state religion) is attested as dead by: (1) post-1945 Occupation directives abolishing State Shinto (Shinto Directive, 1945), (2) the 1947 Constitution's Article 20 separating religion from state, (3) the imperial household's own renunciation of divinity (Humanity Declaration, 1946). None of these sources are beneficiaries of the Meiji arrangement; all are external corroborators of the founding problem's obsolescence. The arrangement persists as institutional inertia (shrine registration, ujiko system, Yasukuni controversy) without its founding justification.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because the state extracts institutional autonomy, land, and doctrinal coherence from religious networks while giving back a fabricated category ('Shinto') that serves state legitimacy. Suppression (0.68) is structural: the 1868-1872 violence (haibutsu kishaku, shugendo ban) created the constraint; post-1945 suppression shifted to ideological (Yasukuni enshrinement controversies, textbook disputes, ujiko registration pressure). Theater ratio (0.58) is high because the constraint's enforcement increasingly performs 'tradition' (restored shrine architecture, invented rituals) while the actual coordination function (if any) atrophied in 1945. Accessibility collapse (0.42) is moderate: folk syncretism persists despite state categories (household altars, festival practice), but official discourse cannot represent it. Resistance (0.55) is sustained: shugendo survival, crypto lineages, and scholarly deconstruction all contest the constraint without toppling it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Meiji state) experiences the constraint as nation-building coordination; the payers (shrine priests, temple monks, laity) experience it as enforced doctrinal schizophrenia. The engine computes this divergence from the structural data — the state's 'coordination' is the payers' extraction. The incoherent bundle reading makes this divergence the central claim: the constraint has no kernel, only state enforcement of an artificial taxonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji state and imperial ideology apparatus are structural beneficiaries (d near 0.0): they collect legitimacy, control, and resources from the enforced decomposition. Shrine priests are dual-positioned: they gain state stipends but lose autonomy and must teach contradictions (d ~ 0.45). Temple monks and lay practitioners are targets (d > 0.7): monks lose land and institutional memory; laity live the contradiction daily with identity-locked exit (family/community ties prevent exit). Shugendo practitioners are excluded (trapped): their very existence falsifies the constraint's categories. Observers see the full structure analytically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing imperial sovereignty through state religion) died in 1945-1946, but the constraint persists through institutional inertia (shrine registration system, ujiko parishioner obligations, Yasukuni as unofficial state shrine) and periodic revival pressure (textbook revisions, prime ministerial visits). This is mandatrophy: the mandate (state Shinto) outlived its function (sovereignty legitimization) and now extracts via theatrical maintenance of 'tradition'. The high theater ratio (0.58) and rising extractiveness post-1980 (0.58→0.72) track this zombie persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_existence_ambiguity,
    'Did a pre-Meiji unified commitment (kernel) exist, or is the ''ontological substrate'' a retrospective projection onto fluid practice?',
    'Comparative analysis of pre-1868 shrine-temple complex records, honji suijaku iconography programs, and practitioner self-descriptions vs. Meiji state categorization. If pre-Meiji actors consistently describe their practice as ''honji suijaku'' as metaphysics, fusion reading gains ground; if they describe it as pragmatic domain-specialization, domain reading gains; if no consistent self-description exists, incoherent bundle reading is supported.',
    'If a kernel existed (fusion or domain), the Meiji intervention is a deformation of a live commitment system. If no kernel existed, the Meiji intervention is the originary violence that created the categories ''Shinto'' and ''Buddhism'' as we know them — the constraint is the enforcement of its own invention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_existence_ambiguity, conceptual, 'Whether the contested kernel is a discovered structure or a constructed category.').

omega_variable(
    state_benefit_continuity,
    'Does the post-1945 Japanese state continue to benefit from the Shinbutsu Bunri''s category structure, or is current extraction purely institutional inertia?',
    'Trace budgetary flows, legal privileges, and ceremonial roles of the Association of Shinto Shrines (Jinja Honcho) and Buddhist sects; measure correlation between state patronage and adherence to the Shinto/Buddhist binary. If state resources preferentially flow to actors maintaining the binary, extraction continues; if resources are category-neutral, persistence is inertial.',
    'If the state still benefits, the constraint is an active snare with living agenda-setters. If purely inertial, it drifts toward piton (theatrical maintenance without concentrated beneficiaries). The current extractiveness rise (0.58→0.72 since 1980) suggests active benefit, but the beneficiaries may have shifted from ''imperial state'' to ''conservative political apparatus''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_benefit_continuity, empirical, 'Whether post-war extraction has active beneficiaries or is zombie inertia.').

omega_variable(
    lay_practitioner_internalization,
    'Is the doctrinal contradiction (kami separate from buddhas officially; fused in household practice) internalized by lay practitioners as cognitive dissonance, or maintained as comfortable compartmentalization?',
    'Ethnographic study of household ritual practice, interview data on self-described belief, and measurement of distress markers when the contradiction is made explicit. If internalized as dissonance, suppression is partly internalized (omega: suppression_mechanism_ambiguity); if compartmentalized, the constraint''s effective suppression is lower than structural measures suggest.',
    'If internalized, the constraint''s effective suppression on lay_practitioners is higher than the structural measure (0.68) — the target carries the suppression internally. If compartmentalized, the constraint''s extraction is partially evaded at the phenomenological level, reducing effective extraction for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_practitioner_internalization, empirical, 'Structural vs. internalized suppression mechanism for identity-locked lay practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 1868, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.25).
narrative_ontology:measurement(shin_tr_t1890, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(shin_tr_t1915, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1915, 0.48).
narrative_ontology:measurement(shin_tr_t1945, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1945, 0.75).
narrative_ontology:measurement(shin_tr_t1952, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1952, 0.68).
narrative_ontology:measurement(shin_tr_t1980, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1980, 0.62).
narrative_ontology:measurement(shin_tr_t2000, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(shin_tr_t2025, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.85).
narrative_ontology:measurement(shin_be_t1890, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1890, 0.78).
narrative_ontology:measurement(shin_be_t1915, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1915, 0.72).
narrative_ontology:measurement(shin_be_t1945, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(shin_be_t1952, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1952, 0.52).
narrative_ontology:measurement(shin_be_t1980, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(shin_be_t2000, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(shin_be_t2025, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.92).
narrative_ontology:measurement(shin_su_t1890, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement(shin_su_t1915, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1915, 0.82).
narrative_ontology:measurement(shin_su_t1945, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(shin_su_t1952, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1952, 0.42).
narrative_ontology:measurement(shin_su_t1980, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(shin_su_t2000, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(shin_su_t2025, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_shinto_institutional_persistence).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, yasukuni_enshrinement_controversy).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, ujiko_registration_system).

% DUAL FORMULATION NOTE:
% This reading decomposes the colloquial 'Shinbutsu syncretism' into three structurally distinct constraints from one kernel. The syncretic_fusion_reading claims ontological unity (low extraction, mountain-like if true); the domain_partition_reading claims functional separation (rope-like coordination); this reading claims the kernel is incoherent and the constraint is state enforcement of an artificial binary (snare). Their ε values differ by >0.4: fusion ε≈0.15, domain ε≈0.35, incoherent ε≈0.72. They are linked via affects_constraints because the state's enforcement history references all three framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, organized, 0.45).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
