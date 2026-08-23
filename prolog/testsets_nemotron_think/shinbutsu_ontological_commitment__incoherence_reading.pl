% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Institutionally Tolerated Ontological Incoherence in Shinbutsu-Shugo
 *   domain: religious/historical/japanese
 *
 * SUMMARY:
 *   The incoherence_reading of the shinbutsu_ontological_commitment kernel
 *   holds that pre-Meiji shinbutsu-shugo (Shinto-Buddhist syncretism) was not
 *   a stable syncretic theology nor a clean domain partition, but an
 *   institutionally maintained state of tolerated ontological contradiction.
 *   Shrine-temple complexes (jingūji), major Buddhist sects, and local elites
 *   benefited from the arrangement's resource-pooling and social-stabilizing
 *   functions, while parishioners bore dual-support costs and doctrinal
 *   purists were marginalized. The Meiji state's shinbutsu bunri (separation)
 *   proceeded rapidly because the constraint lacked deep ontological
 *   embedding — its coordination function was institutional, not doctrinal.
 *   This reading claims the constraint was a tangled_rope: genuine
 *   coordination (shared infrastructure, social peace) combined with
 *   asymmetric extraction (institutional capture of parishioner resources,
 *   suppression of purist voices), held by active institutional enforcement
 *   against ontological clarification.
 *
 * KEY AGENTS:
 *   - shrine_temple_complexes: Primary agenda_setter (institutional/constrained) — managed the incoherence, controlled ritual calendars, collected dual revenue
 *   - major_buddhist_sects: Primary beneficiary (institutional/constrained) — gained shrine patronage, land, and state recognition through honji-suijaku framing
 *   - local_elites: Beneficiary (organized/constrained) — mediated parishioner obligations, controlled temple-shrine economies
 *   - parishioners: Primary payer (moderate/trapped) — funded both institutions, navigated contradictory demands, no exit
 *   - doctrinal_purists: Victim/excluded (moderate/identity_locked) — demanded ontological consistency, suppressed by institutional pressure
 *   - minority_lineages: Victim/excluded (moderate/identity_locked) — non-conforming practices erased by dominant complexes
 *   - meiji_state_builders: Observer/beneficiary of collapse (institutional/analytical) — exploited kernel instability for state-building
 *   - modern_scholars: Observer (analytical/analytical) — analytical seat, no material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.45).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.35).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Institutionally Tolerated Ontological Incoherence in Shinbutsu-Shugo").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/historical/japanese").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '77fc7ca3-8bd5-49a9-b7e7-79185d061fe2').
narrative_ontology:cs_kernel_codification('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', distributed).
narrative_ontology:cs_authority_grounding('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', practice).
narrative_ontology:cs_interpretation_layer_present('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2').
narrative_ontology:cs_reading_relation('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', foundational, ontological_incoherence_tolerated_institutionally).
narrative_ontology:cs_axiom_status(ontological_incoherence_tolerated_institutionally, holdable).
narrative_ontology:cs_axiom_grounding('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', ontological_incoherence_tolerated_institutionally, empirically_contingent).
narrative_ontology:cs_axiom('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', secondary, ritual_practice_precedes_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_practice_precedes_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', ritual_practice_precedes_doctrinal_coherence, conventional).
narrative_ontology:cs_reference_frame('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', pre_meiji_institutional_pluralism).
narrative_ontology:cs_drift_state('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', meiji_restoration, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('77fc7ca3-8bd5-49a9-b7e7-79185d061fe2', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, major_buddhist_sects).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_elites).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, parishioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_purists).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, minority_lineages).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, institutional_pragmatism_over_doctrinal_coherence).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, ritual_efficacy_independent_of_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Managed combined shrine-temple institutions (jingūji, chinjusha), controlled ritual calendars, collected offerings from parishioners for both kami and buddha rites, appointed bettō (monk-administrators). Their authority depended on maintaining the incoherence — clarifying ontology would force a choice between Shinto or Buddhist institutional identity, losing half their revenue base. Exit meant institutional dissolution.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_complexes, agenda_setter,
    institutional, generational, constrained, regional).

% Tendai, Shingon, Jōdo, Zen sects operated through shrine-temple complexes, receiving land, labor, and state recognition via honji-suijaku (original ground / manifest trace) theology that identified kami as manifestations of buddhas. They benefited from the arrangement's legitimacy and resource flows. Exit meant losing shrine networks and reverting to purely monastic economics — possible but costly.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, major_buddhist_sects, beneficiary,
    institutional, generational, constrained, national).

% Village headmen, regional lords, and merchant patrons funded and governed shrine-temple complexes. They mediated parishioner obligations, resolved disputes, and extracted economic surplus from the dual-affiliation system. The incoherence gave them leverage: they could invoke kami or buddha authority as suited the moment. Exit meant losing a flexible tool of local governance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_elites, beneficiary,
    organized, biographical, constrained, local).

% Peasant households and urban commoners were registered (danka system) to both a family temple (bodaiji) and a local shrine (ujigami). They paid for funerals, memorials, festivals, and repairs to both institutions. They navigated contradictory soteriologies (kami pollution vs. buddha compassion) with no doctrinal guidance. Leaving the village meant losing land, community, and ancestral rites — effectively impossible.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, parishioners, payer,
    moderate, biographical, trapped, local).

% Shinto nativists (Kokugaku), Nichiren Buddhists, Jōdo Shinshū hardliners, and Yoshida Shinto ritualists who insisted on ontological clarity. They were denied institutional positions, their texts censored, their lineages marginalized. Their identity fused with doctrinal purity — abandoning the critique would dissolve their self-concept. They paid with exclusion and persecution but could not exit the commitment.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_purists, excluded,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_purists, payer).

% Folk practitioners, yamabushi, onmyōji, and heterodox lineages (e.g., hidden Christians, Fujufuse Nichiren) whose practices didn't fit the shrine-temple binary. They were absorbed, suppressed, or driven underground by the dominant complexes. Their identity was constituted through non-conformity — exit meant becoming the very thing they opposed (mainstream).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, minority_lineages, payer,
    moderate, biographical, identity_locked, local).

% Meiji oligarchs, Shinto ideologues (Hirata Atsutane lineage), and bureaucrats who implemented shinbutsu bunri (1868-1874). They were external to the constraint but benefited enormously from its collapse — the separation gave them State Shinto as a unifying ideology, temple lands as revenue, and Buddhist institutions as controllable entities. They experienced the constraint as an analytical object to be dismantled.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, observer,
    institutional, generational, analytical, national).

% Historians of Japanese religion (Kuroda Toshio, Fabio Rambelli, Mark Teeuwen, etc.) who analyze shinbutsu-shugo as a historical formation. They hold no material stake in the constraint's operation but produce the readings that constitute the kernel contest. Their exit is costless; their role is to map the structural options.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed Shinto and Buddhist institutions to share physical space, parishioner bases, ritual calendars, and economic resources without resolving their contradictory ontologies — providing social stability, disaster response coordination, and conflict avoidance in a politically fragmented archipelago.
% TRANSFER_FUNCTION: Moved material resources (land, labor, donations, tax exemptions) from parishioners to shrine-temple complexes and major sects; moved doctrinal authority from purist lineages to institutional managers who policed the boundaries of acceptable practice; moved legitimacy from local autonomy to centralized sect hierarchies.
% ABSENT_VOICES: Women's religious practices (miko, lay nun communities, folk healers) were subsumed under male institutional hierarchies; Ainu and Ryukyuan ritual systems were erased by the expanding shrine-temple order; burakumin (outcaste) communities were excluded from both shrine and temple affiliation yet forced to fund them. These voices appear only in the gaps of institutional records.
% DISAPPEARANCE_RATIONALE: When the constraint vanished (Meiji shinbutsu bunri, 1868-1874), the world rearranged violently: 40,000+ Buddhist temples destroyed or converted, shrine-temple complexes dissolved, Buddhist clergy defrocked, Shinto priesthood nationalized, parishioner registration reorganized, State Shinto instituted. The rearrangement was not inevitable — it required massive state violence — but the kernel's instability (low accessibility_collapse) meant the rearrangement faced little institutional resistance from within.
% FOUNDING_PROBLEM: How to maintain social cohesion, institutional viability, and ritual efficacy in a polity where kami worship and Buddhism were both deeply entrenched, politically necessary, and ontologically incompatible — without triggering civil war or foreign intervention.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is corroborated by Meiji oligarchs (Iwakura Tomomi, Ōkubo Toshimichi) who explicitly declared the 'evil customs of the past' abolished; by nativist scholars (Hirata Atsutane, Motoori Norinaga) who documented the incoherence as a cause of national weakness; and by modern historians (Kuroda Toshio, Hardacre Helen) who show the problem was specific to the pre-modern polity. The constraint's own beneficiaries (shrine-temple complexes) never claimed the problem was solved — they claimed it was unsolvable and therefore must be tolerated, which is itself corroboration that the problem persisted until external force ended it.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the arrangement extracted parishioner resources for institutional maintenance beyond what doctrinal commitment would justify, but provided real coordination (disaster response, calendrical unity, conflict avoidance). Suppression (0.35) is low-moderate: 'tolerated' incoherence means purists were marginalized not persecuted; enforcement was institutional inertia and social pressure, not violence. Theater_ratio (0.62) is high: honji-suijaku metaphysics and dual-ritual performance became increasingly performative — the ontological cover story thickened as the coordination function became routine. Accessibility_collapse (0.28) is low: pure Shinto (Yoshida, Ise) and pure Buddhist (Nichiren, Jōdo Shinshū) alternatives persisted conceptually and institutionally throughout. Resistance (0.42) is moderate: purist movements (Yoshida Shinto, Fukko Shinto, nativist studies) existed but remained minority until Meiji. The claim/metric independence is respected: claimed_type tangled_rope asserts coordination+extraction+enforcement; metrics describe the degree of each without being tuned to force the classification.
 *
 * PERSPECTIVAL GAP:
 *   From shrine_temple_complexes (agenda_setter), the constraint appears as rope: they built and maintained a working coordination system. From parishioners (payer), it appears as snare: they paid twice for contradictory salvific promises with no exit. From doctrinal_purists (excluded), it appears as mountain of institutional power: an immutable fact they could not challenge. From meiji_state_builders (observer), it appears as piton: a theatrical shell easily shattered. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (shrine_temple_complexes, major_buddhist_sects, local_elites) collect resources and authority from the arrangement — directionality d near 0.1-0.2 (beneficiary end). Victims (parishioners, doctrinal_purists, minority_lineages) bear costs and exclusion — d near 0.8-0.9 (target end). Parishioners are trapped (exit_options: trapped) — no alternative religious economy exists locally. Purists are identity_locked — their self-concept requires doctrinal coherence, making exit from the incoherence existentially costly. Meiji_state_builders are analytical (d=0.5) but become de facto beneficiaries of collapse (directionality_override may apply). The derivation chain: beneficiary/victim declarations + power + exit → d → χ computed by engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — maintaining social cohesion and institutional viability amid competing religious claims in a fragmented polity — was live for centuries. By the late Edo period, the problem was contested: nativist scholars argued the incoherence itself caused spiritual and political decay. The Meiji state declared it dead and imposed separation. The mandatrophy is resolved (mandatrophy_resolved: true in base_properties would be appropriate but is not a field — the six_questions founding_problem_status=dead captures this). The tangled_rope classification prevents mislabeling: a pure rope reading would miss the extraction from parishioners and suppression of purists; a pure snare reading would miss the genuine coordination function that made the arrangement stable for centuries. The scaffold classification would be wrong — no sunset clause was declared; the arrangement ended by external conquest, not internal design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the constraint ''institutionally tolerated incoherence'' a single reading of the shinbutsu_ontological_commitment kernel, or does it describe the kernel itself?',
    'Compare structural predictions of incoherence_reading against syncretic_reading and partition_reading on the same historical referent; if they produce divergent ε and beneficiary/victim sets, they are distinct constraints sharing a kernel.',
    'If distinct, each reading gets its own constraint story with independent ε; the kernel becomes a family linked by network.affects_constraints. If unified, the natural-language label ''shinbutsu-shugo'' masks one constraint with measurement-dependent classification — forbidden by ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this JSON instantiates exactly one reading (incoherence_reading) of the contested kernel shinbutsu_ontological_commitment, not the kernel itself.').

omega_variable(
    tolerance_vs_enforcement_mechanism,
    'Was the ''tolerated incoherence'' maintained by soft institutional inertia (low suppression) or by active disciplinary pressure against ontological clarification (high suppression)?',
    'Examine temple-shrine administrative records for sanctions against monks/priests who preached doctrinal purity; measure frequency of honji-suijaku disputations suppressed vs. ignored.',
    'If active suppression, suppression metric rises toward 0.6+ and constraint moves toward snare; if passive tolerance, suppression stays ~0.35 and tangled_rope holds with coordination dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tolerance_vs_enforcement_mechanism, empirical, 'Whether ''tolerated'' describes absence of enforcement or a managed equilibrium with hidden coercion.').

omega_variable(
    meiji_separation_ease_ambiguity,
    'Did the Meiji shinbutsu bunri (separation) proceed easily because the prior incoherence lacked deep roots, or because state violence overrode embedded institutions?',
    'Quantify temple destruction, priest defrocking, and land confiscation during 1868-1874; compare to pre-Meiji institutional density.',
    'If violence was high, the ''ease of separation'' is a retrospective myth — the constraint had high structural embeddedness (low accessibility_collapse was misread). If violence was low, the incoherence_reading''s low accessibility_collapse (0.28) is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_ease_ambiguity, empirical, 'Whether the Meiji separation''s speed reflects kernel instability or state coercion.').

omega_variable(
    parishioner_extraction_vs_coordination,
    'Did parishioners experience the dual-support obligation (donations to both shrine and temple) as extractive cost or as coordinated spiritual insurance?',
    'Analyze village expense records, petition frequency, and folk narratives for resentment vs. pragmatic acceptance of double affiliation.',
    'If extractive, victim set ''parishioners'' is validated and extraction rises; if coordinative, ''parishioners'' shifts toward beneficiary and constraint approaches rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parishioner_extraction_vs_coordination, preference, 'Valence of parishioner experience — the same material flow can be read as cost or benefit depending on frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_incoherence_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shinbutsu_incoherence_tr_t250, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 250, 0.48).
narrative_ontology:measurement(shinbutsu_incoherence_tr_t500, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 500, 0.55).
narrative_ontology:measurement(shinbutsu_incoherence_tr_t750, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 750, 0.6).
narrative_ontology:measurement(shinbutsu_incoherence_tr_t1000, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1000, 0.62).

% Extraction over time
narrative_ontology:measurement(shinbutsu_incoherence_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(shinbutsu_incoherence_be_t250, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 250, 0.41).
narrative_ontology:measurement(shinbutsu_incoherence_be_t500, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 500, 0.43).
narrative_ontology:measurement(shinbutsu_incoherence_be_t750, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 750, 0.45).
narrative_ontology:measurement(shinbutsu_incoherence_be_t1000, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_incoherence_su_t0, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(shinbutsu_incoherence_su_t250, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 250, 0.3).
narrative_ontology:measurement(shinbutsu_incoherence_su_t500, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 500, 0.33).
narrative_ontology:measurement(shinbutsu_incoherence_su_t750, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 750, 0.35).
narrative_ontology:measurement(shinbutsu_incoherence_su_t1000, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1000, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, state_shinto_formation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, buddhist_secularization_meiji).

% DUAL FORMULATION NOTE:
% Kernel shinbutsu_ontological_commitment decomposes into three constraint stories: incoherence_reading (this file), syncretic_reading, partition_reading. All three share the historical referent (pre-Meiji shrine-temple relations) but posit different ε, beneficiary/victim structures, and types. incoherence_reading ε=0.45 (tangled_rope); syncretic_reading likely ε≈0.2 (rope — genuine coordination via shared metaphysics); partition_reading likely ε≈0.15 (rope — clean domain separation). The upstream constraint (incoherence_reading) influences downstream: the kernel instability it posits explains why Meiji separation could proceed rapidly, affecting meiji_shinbutsu_bunri's initial conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, moderate, 0.85).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
