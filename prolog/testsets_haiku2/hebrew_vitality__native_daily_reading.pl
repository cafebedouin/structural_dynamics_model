% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native Daily Vitality Standard for Hebrew Language Revitalization
 *   domain: sociolinguistics/cultural/political
 *
 * SUMMARY:
 *   The Hebrew language revitalization of the late 19th and 20th centuries
 *   operated under an implicit but powerful constraint: only daily native
 *   speaker use by new generations constitutes linguistic vitality. This
 *   reading—native daily use as the sole valid marker of life—became
 *   institutionalized in Israeli education, media, and state policy. It
 *   simultaneously enabled Hebrew's transition from a diaspora liturgical
 *   language to a vernacular state language AND delegitimized the unbroken
 *   liturgical tradition as merely preservational rather than vital. This is
 *   one reading of the contested kernel 'hebrew_vitality'; sibling readings
 *   (liturgical_reading, hybrid_continuity_reading) locate vitality
 *   differently and attribute different legitimacy to tradition.
 *
 * KEY AGENTS:
 *   - zionist_state_builders: institutional agenda-setter, designed and enforced the native-daily standard
 *   - secular_hebrew_speakers: organized beneficiary, inhabit the revitalized vernacular ecosystem
 *   - liturgical_hebrew_tradition: powerful payer, displaced from authority over Hebrew's definition
 *   - ultra_orthodox_communities: moderate identity-locked payer, excluded from legitimate speech authority
 *   - diaspora_hebrew_learners: moderate constrained beneficiary, access Hebrew through the revitalized curriculum
 *   - academic_hebraists: institutional observer, measure the constraint's empirical and conceptual coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.62).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.71).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native Daily Vitality Standard for Hebrew Language Revitalization").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/cultural/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '50ae4e4d-7ab5-471d-8b5a-7d0e1070e084').
narrative_ontology:cs_kernel_codification('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', formalized).
narrative_ontology:cs_authority_grounding('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', extraction).
narrative_ontology:cs_interpretation_layer_present('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084').
narrative_ontology:cs_reading_relation('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', foundational, vitality_requires_native_generation).
narrative_ontology:cs_axiom_status(vitality_requires_native_generation, holdable).
narrative_ontology:cs_axiom_grounding('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', vitality_requires_native_generation, empirically_contingent).
narrative_ontology:cs_axiom('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', foundational, ritual_preservation_excludes_vitality).
narrative_ontology:cs_axiom_status(ritual_preservation_excludes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', ritual_preservation_excludes_vitality, deontological).
narrative_ontology:cs_axiom('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', secondary, secular_modernization_necessary_for_language_survival).
narrative_ontology:cs_axiom_status(secular_modernization_necessary_for_language_survival, holdable).
narrative_ontology:cs_axiom_grounding('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', secular_modernization_necessary_for_language_survival, instrumental).
narrative_ontology:cs_reference_frame('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', native_generational_vitality).
narrative_ontology:cs_drift_state('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', contemporary_liturgical_resurgence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50ae4e4d-7ab5-471d-8b5a-7d0e1070e084', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, ultra_orthodox_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, diaspora_hebrew_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed and drove the Hebrew revival project from the late 19th century onward, establishing native-speaker Hebrew as the linguistic substrate of Jewish sovereignty. They created schools, standardized orthography, and suppressed liturgical-only speakers from positions of cultural authority. Benefited from the revitalization by anchoring national identity in a vernacular rather than a diaspora-ritual language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Inhabit a living Hebrew language ecosystem where everyday speech, newspapers, commerce, and school instruction operate in modern Hebrew. They inherit a language they speak natively without liturgical mediation. Their cultural vitality is directly constituted by daily use; the standard affirms their speech as the legitimate form.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% The unbroken chain of Hebrew-in-prayer, Talmudic study, and rabbinical interpretation spanning 2000+ years. By the native-daily reading, this constitutes preservation, not vitality—a kept museum rather than a living language. The standard's enforcement delegitimizes liturgical use as a sufficient claim to language stewardship, displacing Hebrew from sacred to heritage status.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition, payer,
    powerful, civilizational, identity_locked, global).

% Maintain Hebrew primarily through liturgy, Talmudic argument, and religious instruction. They are constructed as non-vital speakers under the native-daily standard, despite unbroken continuity. The constraint excludes them from authority over Hebrew's definition; their liturgical fluency is reframed as antiquarian rather than authoritative. Exit would require abandoning religious identity itself.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, ultra_orthodox_communities, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, ultra_orthodox_communities, excluded).

% Acquire Hebrew through modern standardized curriculum rooted in the native-daily standard. They inherit a language designed as living speech rather than liturgical artifact. Their access to Hebrew is premised on the vitality claim—if liturgical preservation were deemed sufficient, the institutional infrastructure for modern learning might not exist at the scale it does.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_hebrew_learners, beneficiary,
    moderate, biographical, constrained, global).

% Study Hebrew's historical structure, revival mechanisms, and sociolinguistic outcomes. They document the constraint's operation and test whether native-generation vitality is empirically true (native speakers do use the language daily), conceptually coherent (what counts as native generation?), or prescriptive (what should count).
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, academic_hebraists, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for what counts as a living language: daily vernacular use by native speakers, requiring continuous lexical innovation, colloquial fluency, and intergenerational transmission through informal speech and formal education. Solves the coordination problem of defining Hebrew's status post-diaspora when the language could have been treated as historical artifact rather than state language.
% TRANSFER_FUNCTION: Moves cultural authority from liturgical authorities and diaspora scholars to secular speakers and state institutions; transfers the legitimacy of 'Hebrew' from ritual specialists to anyone who speaks it natively in daily contexts. Transfers the sacred status of Hebrew-in-prayer to the profane status of Hebrew-in-commerce and Hebrew-in-childhood.
% ABSENT_VOICES: The historical liturgical tradition itself cannot speak in the modern native-daily framework—it is the entity being redefined. Diaspora communities who maintained Hebrew primarily through prayer and study are structurally excluded from the conversation about vitality; their continued practice is framed as preservation by the standard-setters, not as a competing claim to vitality.
% DISAPPEARANCE_RATIONALE: If the native-daily standard vanished, Hebrew would not disappear—the spoken language would persist—but the institutional assertion that only living speech constitutes vitality would collapse. The liturgical tradition would reclaim legitimacy as a vital form of Hebrew practice. Educational emphasis would shift; the boundary between living language and heritage artifact would dissolve or re-center on continuity rather than generation.
% FOUNDING_PROBLEM: How could Hebrew transition from a diaspora liturgical language spoken fluently only by ritual specialists to a vernacular state language accessible to ordinary children and shopkeepers? The constraint operationalizes one answer: native daily use is the criterion for linguistic vitality, which justifies massive institutional investment in secular Hebrew education and desacralizes the language to make it available for everyday use.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and language-planning authorities attest the founding problem was acute and the native-daily solution was necessary. Academic sociolinguists and historians of the revival attest the problem was real. Ultra-Orthodox and diaspora communities attest the problem was artificially constructed—Hebrew was already living and vital through liturgical practice; the constraint was imposed to consolidate political power, not to solve a genuine coordination gap. Linguistic evidence shows Hebrew was functionally specialized (sacred/study registers) rather than dead.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62) and rises over the interval because the constraint requires continuous institutional reinforcement to suppress the counter-claim (liturgical vitality). Early extractiveness is lower (0.38) when the constraint competes with living liturgical practice; final extractiveness plateaus (0.62) when the native-daily standard is culturally sedimented but still must be actively defended against fundamentalist and diaspora revitalization movements. Suppression is high (0.71) because the constraint's enforcement depends on delegitimizing an alternative that IS empirically continuous (the liturgical tradition really never broke). Theater rises (0.12 to 0.28) as the vitality criterion becomes increasingly performative—declaring Hebrew vital through daily use becomes an identity ritual even as the language's actual status shifts. Accessibility collapse is moderate (0.48): alternatives (liturgical vitality, code-switching, diaspora Hebrew) remain conceptually available even if institutionally suppressed. Resistance is high (0.74) initially and moderates (0.65 at end) as orthodox communities adapt; the constraint meets real opposition from those it delegates to non-vital status.
 *
 * PERSPECTIVAL GAP:
 *   The zionist agenda-setter and secular speakers perceive this as genuine coordination—the solution to the real problem of How Hebrew becomes a language of everyday life. The payer seats (liturgical tradition, ultra-Orthodox) perceive it as imposed extraction: their continuity is reframed as museum-keeping, their authority stripped, their identity-constituting practice delegitimized as non-vital. The gap arises because the constraint does BOTH simultaneously: it solves a coordination problem (Hebrew revitalization) AND it extracts legitimacy from one tradition to confer it on another.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist state builders: d ≈ 0.1 (full beneficiary, they set and capture the arrangement). Secular Hebrew speakers: d ≈ 0.25 (beneficiary, they speak the language the standard protects as vital, but they don't set policy; they inherit the standard). Liturgical tradition: d ≈ 0.95 (near full target, their practice is explicitly declared non-vital by the standard; they bear the cost of desacralization and delegitimization). Ultra-Orthodox communities: d ≈ 0.88 (high target, identity-locked, excluded from authority, their children educated into the secular Hebrew standard). The derivation chains from these victim/beneficiary declarations without requiring overrides; directionality is sharp because the identity-locking of the payer seats prevents arbitrage-style exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How does Hebrew become everyday vernacular?) was real and substantive through roughly year 30 of the interval. By year 50-100, Hebrew IS living vernacular speech; the founding problem is solved. Yet the constraint (only native generation constitutes vitality) persists and even hardens. This is a live mandatrophy candidate: the arrangement that solved the founding problem now persists as extraction from the tradition it displaced. The native-daily standard became less about solving coordination and more about maintaining state-builder authority over Hebrew's definition. The six-questions mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) flags this: the problem is arguably dead but the world would rearrange if the standard dissolved, which is the signature of zombie extraction wearing a coordination mask.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_contestation,
    'Is ''daily native use by new generations'' the correct criterion for linguistic vitality, or is continuity-of-practice (including liturgical transmission) equally valid?',
    'Comparative sociolinguistics: survey whether liturgical-only languages (e.g., Coptic, Classical Armenian in church contexts) meet the functional definition of living language. Test whether functional vitality requires daily secular use or whether ritual-specialist fluency suffices for intergenerational transmission.',
    'If liturgical-only transmission CAN constitute vitality, the native-daily reading''s core premise is overridden; reclassify to hybrid_continuity_reading or liturgical_reading. If native-daily use is empirically necessary for the language to innovate and survive generational shift, the native-daily reading is strengthened and the liturgical-reading becomes historical preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_contestation, empirical, 'Whether the vitality criterion is empirically grounded or reading-dependent.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Was the native-daily standard a necessary coordination solution to Hebrew revitalization, or was it a constructed imposition that suppressed an already-living liturgical tradition to consolidate Zionist state authority?',
    'Genealogical reconstruction of Hebrew''s status pre-revival: measure fluency distribution, generational transmission, and innovation rates in liturgical contexts (1700–1890). Test whether the coordination problem (fragmented multilingual diaspora, lack of Hebrew-based institutions) required suppressing liturgical authority or whether liturgical and secular streams could have coexisted.',
    'If the founding problem required suppression of liturgical authority, the constraint is tangled-rope (mixed coordination and extraction). If the problem was solved structurally without the suppression, the constraint is primarily snare (suppression is the core, coordination is the cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether the native-daily standard''s extraction component is necessary to its coordination function or parasitic on it.').

omega_variable(
    identity_lock_mechanism,
    'For ultra-Orthodox communities, is the identity-lock that prevents exit structural (loss of community, religious identity, educational coherence) or internalized (the community has been taught to accept the native-daily standard as legitimate)?',
    'Longitudinal study of religious Jewish communities that adopted secular Hebrew without losing religious practice; post-exit psychological surveys of individuals who left ultra-Orthodox communities; comparative analysis of language identity in communities with access to alternative authority structures for Hebrew.',
    'If identity-lock is structural, suppression requires constant enforcement. If it is internalized, post-exit suppression persists (the community carries the standard with them). If it is partially each, the constraint''s suppression is higher than the scalar metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the identity-fusion preventing ultra-Orthodox exit is structural or internalized.').

omega_variable(
    kernel_reading_under_determination,
    'The native-daily reading treats ''Hebrew vitality'' as a kernel that can support multiple readings; is this framing legitimate, or is vitality itself reading-constituted (i.e., the kernel is not a shared commitment but a label over fundamentally incommensurable claims)?',
    'Examine whether zionist, liturgical, and hybrid-continuity parties could articulate a single, shared commitment (the kernel) and disagree only on its interpretation (the reading), or whether they are making incompatible claims about what Hebrew IS (liturgical artifact vs. living language) that admit no common kernel.',
    'If a single kernel exists, the three readings are legitimate siblings. If vitality is reading-constituted (different claims about ontology, not interpretation), the kernel frame is misapplied and the constraint should be split into three independent constraints rather than three readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether Hebrew vitality admits a kernel reading framework or is fundamentally incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t15, hebrew_vitality__native_daily_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(hebr_tr_t30, hebrew_vitality__native_daily_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(hebr_tr_t50, hebrew_vitality__native_daily_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(hebr_tr_t75, hebrew_vitality__native_daily_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__native_daily_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_be_t15, hebrew_vitality__native_daily_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(hebr_be_t30, hebrew_vitality__native_daily_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(hebr_be_t50, hebrew_vitality__native_daily_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(hebr_be_t75, hebrew_vitality__native_daily_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__native_daily_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hebr_su_t15, hebrew_vitality__native_daily_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(hebr_su_t30, hebrew_vitality__native_daily_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(hebr_su_t50, hebrew_vitality__native_daily_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(hebr_su_t75, hebrew_vitality__native_daily_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__native_daily_reading, suppression_requirement, 100, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(hebr_grid_01, hebrew_vitality__native_daily_reading, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(hebr_grid_02, hebrew_vitality__native_daily_reading, accessibility_collapse(class), 100, 0.5).
narrative_ontology:measurement(hebr_grid_03, hebrew_vitality__native_daily_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(hebr_grid_04, hebrew_vitality__native_daily_reading, accessibility_collapse(individual), 100, 0.48).
narrative_ontology:measurement(hebr_grid_05, hebrew_vitality__native_daily_reading, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(hebr_grid_06, hebrew_vitality__native_daily_reading, accessibility_collapse(organizational), 100, 0.55).
narrative_ontology:measurement(hebr_grid_07, hebrew_vitality__native_daily_reading, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(hebr_grid_08, hebrew_vitality__native_daily_reading, accessibility_collapse(structural), 100, 0.48).
narrative_ontology:measurement(hebr_grid_09, hebrew_vitality__native_daily_reading, resistance(class), 0, 0.82).
narrative_ontology:measurement(hebr_grid_10, hebrew_vitality__native_daily_reading, resistance(class), 100, 0.55).
narrative_ontology:measurement(hebr_grid_11, hebrew_vitality__native_daily_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(hebr_grid_12, hebrew_vitality__native_daily_reading, resistance(individual), 100, 0.65).
narrative_ontology:measurement(hebr_grid_13, hebrew_vitality__native_daily_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(hebr_grid_14, hebrew_vitality__native_daily_reading, resistance(organizational), 100, 0.48).
narrative_ontology:measurement(hebr_grid_15, hebrew_vitality__native_daily_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(hebr_grid_16, hebrew_vitality__native_daily_reading, resistance(structural), 100, 0.42).
narrative_ontology:measurement(hebr_grid_17, hebrew_vitality__native_daily_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(hebr_grid_18, hebrew_vitality__native_daily_reading, stakes_inflation(class), 100, 0.74).
narrative_ontology:measurement(hebr_grid_19, hebrew_vitality__native_daily_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(hebr_grid_20, hebrew_vitality__native_daily_reading, stakes_inflation(individual), 100, 0.55).
narrative_ontology:measurement(hebr_grid_21, hebrew_vitality__native_daily_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(hebr_grid_22, hebrew_vitality__native_daily_reading, stakes_inflation(organizational), 100, 0.71).
narrative_ontology:measurement(hebr_grid_23, hebrew_vitality__native_daily_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(hebr_grid_24, hebrew_vitality__native_daily_reading, stakes_inflation(structural), 100, 0.68).
narrative_ontology:measurement(hebr_grid_25, hebrew_vitality__native_daily_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(hebr_grid_26, hebrew_vitality__native_daily_reading, suppression(class), 100, 0.78).
narrative_ontology:measurement(hebr_grid_27, hebrew_vitality__native_daily_reading, suppression(individual), 0, 0.35).
narrative_ontology:measurement(hebr_grid_28, hebrew_vitality__native_daily_reading, suppression(individual), 100, 0.62).
narrative_ontology:measurement(hebr_grid_29, hebrew_vitality__native_daily_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(hebr_grid_30, hebrew_vitality__native_daily_reading, suppression(organizational), 100, 0.76).
narrative_ontology:measurement(hebr_grid_31, hebrew_vitality__native_daily_reading, suppression(structural), 0, 0.38).
narrative_ontology:measurement(hebr_grid_32, hebrew_vitality__native_daily_reading, suppression(structural), 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel decomposes into three structurally distinct constraint stories corresponding to three readings of what constitutes Hebrew's vitality. This story (native_daily_reading) instantiates the Zionist-secular reading: vitality requires daily native-speaker use, intergenerational transmission through vernacular, and continuous lexical innovation. The sibling constraints instantiate alternative readings: liturgical_reading (vitality is unbroken ritual use by specialists) and hybrid_continuity_reading (vitality requires both liturgical substrate and vernacular reconstruction). Each reading assigns different ε, different beneficiaries/victims, and different classifications. They are linked by network.affects_constraints to show their structural codependence: the native-daily reading's success depends on suppressing the liturgical reading's legitimacy claims; the hybrid reading attempts to mediate between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
