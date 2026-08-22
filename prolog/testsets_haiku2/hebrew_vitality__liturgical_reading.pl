% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality via Liturgical Reading (Kernel: hebrew_vitality)
 *   domain: sociolinguistic/religious
 *
 * SUMMARY:
 *   The liturgical_reading of Hebrew vitality claims that unbroken ritual
 *   recitation of classical texts—prayer, Torah study, rabbinic
 *   commentary—constitutes the essential vitality of the language. This
 *   reading emerged during the medieval diaspora when Hebrew ceased to be
 *   native and was sustained through institutional practice in yeshivas,
 *   synagogues, and textual study. The reading frames rabbinic and liturgical
 *   authorities as the custodians of vitality because they maintain the
 *   transmission contexts and interpretive traditions that keep the language
 *   recognizable and learnable. This is ONE reading of the contested kernel
 *   'hebrew_vitality.' Two sibling readings exist: native_daily_reading
 *   (which claims only native generation constitutes vitality, treating
 *   ritual as preservation, not life) and hybrid_continuity_reading (which
 *   claims both liturgical substrate AND native acquisition were necessary
 *   for modern vitality). The three readings share a kernel—what constitutes
 *   Hebrew vitality?—but offer incompatible answers grounded in different
 *   core premises about language essence, transmission mechanisms, and
 *   institutional authority.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: set and defend the liturgical reading; position themselves as custodians of vitality through institutional control of liturgical practice
 *   - liturgical_continuity_preservers: organized communities sustaining daily prayer, text study, and classical transmission; their institutional forms are validated by this reading as vitality-preserving
 *   - native_speaker_advocates: excluded from the rabbinic institutional frame; contest the claim that ritual alone constitutes vitality; argue native generation is essential
 *   - secular_hebrew_speakers: inherit language shaped by liturgical substrate but generate it natively; sit between beneficiary and payer positions—they benefit from the preserved substrate but their lived vitality comes from native use, not ritual recitation
 *   - hybrid_continuity_theorists: external observers who analyze historical interaction between liturgical preservation and vernacular revival; provide evidence that both mechanisms contributed to modern vitality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.12).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality via Liturgical Reading (Kernel: hebrew_vitality)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistic/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '82f42f75-40ac-478f-a8fb-5bf21ab6f3e4').
narrative_ontology:cs_kernel_codification('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', fixed_text).
narrative_ontology:cs_authority_grounding('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', lineage).
narrative_ontology:cs_interpretation_layer_present('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4').
narrative_ontology:cs_reading_relation('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', foundational, ritual_recitation_constitutes_vitality).
narrative_ontology:cs_axiom_status(ritual_recitation_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', ritual_recitation_constitutes_vitality, deontological).
narrative_ontology:cs_axiom('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', foundational, classical_text_substrate_is_essence).
narrative_ontology:cs_axiom_status(classical_text_substrate_is_essence, holdable).
narrative_ontology:cs_axiom_grounding('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', classical_text_substrate_is_essence, instrumental).
narrative_ontology:cs_reference_frame('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', contemporary_native_speaker_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82f42f75-40ac-478f-a8fb-5bf21ab6f3e4', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_continuity_preservers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, secular_hebrew_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and defend the claim that liturgical Hebrew, maintained through unbroken ritual recitation across 2000+ years, constitutes the true vitality of the language. They interpret sacred texts, guide liturgical practice, and frame ritual as the essential continuity that kept Hebrew alive when no native speakers existed. They benefit by establishing their institutional reading as the authoritative one.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, mobile, global).

% Communities and institutions organized around maintaining unbroken liturgical practice (yeshiva networks, diaspora congregations, ritual specialists). They sustain daily prayer, study of classical texts, and ceremonial practice. The liturgical_reading validates their work as vitality-preserving and positions their institutional forms as the core of Hebrew continuity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_continuity_preservers, beneficiary,
    organized, civilizational, constrained, global).

% Proponents of native Hebrew acquisition and daily vernacular use (educational innovators, secular Zionists, language planners) who argue vitality requires native generation of speakers, not ritual recitation. They contest the liturgical_reading's claim that ritual alone constitutes vitality. Excluded from the rabbinic institutional adjudication of what 'vitality' means.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, native_speaker_advocates, excluded,
    powerful, biographical, mobile, national).

% Scholars and theorists (linguistic historians, revival specialists) who analyze the interaction between liturgical preservation and vernacular revival. They observe that both liturgical maintenance and native acquisition contributed to modern Hebrew vitality, neither alone sufficient. They sit outside the institutional dispute but provide external evidence on the constitutive question.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, hybrid_continuity_theorists, observer,
    analytical, biographical, analytical, global).

% Modern native speakers who learned Hebrew in schools and speak it daily. They inherit the language in a form shaped by liturgical literacy standards (vocabulary, grammar, text-anchoring) but generate it natively. The liturgical_reading credits liturgical continuity with preserving the linguistic core that made their native acquisition possible; native speakers may credit liturgical institutions with this or dispute it, creating a perspectival split.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_hebrew_speakers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, secular_hebrew_speakers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains linguistic coherence and continuity across non-native populations through ritual recitation of fixed texts. Unbroken liturgical practice solves the coordination problem of 'how does a language stay recognizable and accessible to future generations when no native speakers exist to transmit it naturally?' Ritual recitation provides a stable substrate of vocabulary, grammar, and pronunciation rules that survives diaspora fragmentation and generational discontinuity.
% TRANSFER_FUNCTION: Moves institutional authority and definitional power to rabbinic and liturgical authorities: they decide what constitutes 'Hebrew vitality,' frame ritual practice as the essential mechanism, and position themselves as the custodians of that vitality. This transfer is not coercive—it operates through persuasion and institutional positioning—but it privileges their reading over competing framings (e.g., native-speaker vitality, hybrid continuity).
% ABSENT_VOICES: Native speaker advocates and secular Hebrew planners are systematically excluded from the institutional site where 'vitality' is defined. Their empirical observations about what actually revitalized Hebrew as a spoken language (education systems, nation-building projects, daily use contexts) are not admitted to the rabbinic adjudication. They would contest the claim that ritual alone constitutes vitality and would point to the necessity of native generation.
% DISAPPEARANCE_RATIONALE: If the claim 'liturgical reading constitutes Hebrew vitality' disappeared—if the rabbinic reading were rejected and native-speaker vitality became the authoritative frame—institutional priorities would shift: resources would redirect to native education and secular linguistic development; the status of liturgical knowledge would become a specialized scholarly domain rather than the core of vitality; rabbinic institutions would lose the authority to define what counts as Hebrew's essential survival. The institutional reorganization would be substantial.
% FOUNDING_PROBLEM: During the medieval and early modern diaspora (roughly 5th–18th centuries CE), Hebrew ceased to be anyone's native language. The founding problem was: how does a language persist, remain recognizable, and stay available to learners when its native-speaker community has dissolved? Liturgical practice—daily prayer, Torah study, rabbinic commentary in classical languages—provided an answer: unbroken ritual use maintains the language in fixed texts and creates transmission contexts (heder, yeshiva) where non-native learners continuously encounter and reproduce the classical forms.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historical reality is uncontested: Hebrew was indeed non-native for centuries. The STATUS of the founding problem is contested. Rabbinic authorities (and the institutional reading they represent) attest that liturgical preservation SOLVED the problem—that ritual use kept Hebrew alive and vitalized in the classical forms. Secular linguists and historians of the 20th-century revival (e.g., Fishman, Spolsky, Nahir) attest that the founding problem was substantially transformed, not solved by ritual alone: native acquisition via school systems and state institutions required BOTH the substrate preserved by liturgical continuity AND massive modernization and secular development. The revival did not happen through ritual extension; it happened through native generation using a substrate the ritual domain had kept alive but could not, by itself, revitalize.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18 at interval end) because this reading claims a coordination function (preserving language coherence across non-native populations) rather than asserting asymmetric wealth or power transfer. The beneficiaries (rabbinic authorities, liturgical institutions) do accrue institutional authority and definitional power—they position themselves as custodians of vitality—but this authority is persuasive and institutional, not coercively enforced. Suppression is minimal (0.12) because the constraint's persistence depends on continued institutional practice and persuasion, not on blocking alternatives. Theater_ratio is very low (0.08) because liturgical practice genuinely does maintain linguistic coherence; the performative component is modest. Accessibility_collapse is very high (0.92): once the liturgical reading is understood as a claim about what constitutes vitality, it becomes nearly impossible to engage the classical language outside that framework without accepting the liturgical substrate it preserved. Resistance is low (0.15) because the liturgical institutions face no organized internal opposition to maintaining their reading; the native-speaker camp and secular planners resist externally but are excluded from the institutional adjudication site. The measurement series show slight creep over the 100-year interval (roughly 1926–2026, the modern Hebrew revival era): extractiveness and suppression both rise modestly as the competitive pressure from native-speaker vitality claims increases and rabbinic institutions invest more institutional effort in defending their reading against the 'hybrid' and 'native' readings. This is not dramatic drift—the constraint's core structure remains stable—but it reflects growing institutional tension as native Hebrew speakers outnumber liturgical specialists and question whether ritual alone constitutes vitality.
 *
 * PERSPECTIVAL GAP:
 *   The gap between rabbinic-institutional and native-speaker seats should compute distinctly. From the rabbinic-authority position, the constraint is genuine coordination: unbroken ritual use did solve a medieval coordination problem and does preserve language coherence across diaspora. They author the reading from an institutional seat that has invested centuries in defending it. From the native-speaker position (particularly 20th-century revival planners), the same constraint appears as a definitional claim that, while historically true, is incomplete: liturgical preservation was necessary but not sufficient, and it is misleading to call ritual recitation alone 'vitality' when vitality actually emerged from massive secular native-acquisition projects that liturgical institutions did not drive. The engine should compute different type assignments from these seats because the structural relationships diverge: the reading is coordination from the institutional seat, and (something more extractive) from the native-speaker seat that contests its sufficiency. The authored metrics describe the constraint as LOW extraction (the reading genuinely did solve a coordination problem), but the perspectival split should remain visible in the per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic_authorities are the agenda-setter: they control which reading is transmitted as authoritative, decide what counts as vitality, and frame ritual preservation as the essential mechanism. Their power is institutional and their directionality is toward beneficiary (d near 0.0)—they collect authority and institutional prestige without bearing extraction costs. Liturgical_continuity_preservers are secondary beneficiaries: their institutional forms and daily practices are validated as vitality-preserving; their situation is stable and aligns with the reading's core claims; their directionality is also beneficiary-leaning (d near 0.1). Secular_hebrew_speakers sit near symmetric (d ≈ 0.5): they benefit from the preserved linguistic substrate (inherited vocabulary, grammar grounded in classical texts) but generate language natively, not liturgically; the reading credits liturgical preservation for their foundation but does not describe their lived practice as vitality. Native_speaker_advocates have no formal position in the reading's institutional structure (excluded, not seated)—they would experience the reading as imposing an alternative definition of vitality that devalues their own work. The excluded position means they experience high directionality as targets (d near 1.0) of the reading's definitional authority, though they are not coercively suppressed. Hybrid_continuity_theorists are observers: their position is analytical, they bear no costs and collect no benefits from the reading itself, though they provide external evidence that contests the reading's sufficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does Hebrew persist without native speakers?) was genuinely solved by liturgical preservation during the medieval diaspora. By the 20th-century revival, the founding problem's STATUS had changed: native acquisition became possible and happened at massive scale through state education, modern textbooks, and secular Hebrew culture. The liturgical_reading persists in asserting that ritual alone constitutes vitality, even as the actual revitalization of Hebrew shifted to secular, native, institutional contexts (schools, literature, daily media). This is a potential mandatrophy: the arrangement that solved the medieval coordination problem no longer addresses the contemporary problem (revitalizing Hebrew as a spoken language for millions of new native speakers). Rabbinic institutions continue to frame their liturgical practice as vitality-preserving and position themselves as custodians, but the institutional reorganization around Hebrew revitalization happened outside and independently of the rabbinic reading. The mandatrophy is not fully resolved—liturgical knowledge remains part of Hebrew literacy and rabbinic institutions continue to exert definitional authority—but the reading's claim to constitute vitality is increasingly contested by the empirical fact that native speakers, not ritual specialists, are the majority of Hebrew language users. An observer-seat computation might flag mandatrophy_likelihood as moderate-to-high.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutiveness_vs_necessity,
    'Does ''liturgical preservation constituted vitality'' mean the ritual practice IS the vitality, or that it was NECESSARY for vitality but not sufficient by itself?',
    'Distinguish ''constitutes'' (identity claim: ritual = vitality) from ''enables'' (causal claim: ritual was necessary precondition). The liturgical reading asserts the former; hybrid and native readings assert the latter. Textual analysis of authoritative rabbinic sources and modern revivalist writings, separated by reading affiliation, would show which framing each side commits to.',
    'If constitutes is affirmed, the reading stands as authored (low extraction, genuine coordination). If the actual claim is ''enabled,'' the reading conflates necessary condition with definition and should be reclassified as a hybrid reading that understands vitality as requiring both ritual substrate and native generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutiveness_vs_necessity, conceptual, 'Whether the liturgical reading asserts identity or necessary causation.').

omega_variable(
    institutional_authority_capture,
    'To what degree does the rabbinic institutional reading benefit from monopoly over the definition of ''vitality,'' such that maintaining the reading preserves institutional authority even as the empirical basis shifts to native speakers?',
    'Comparative institutional analysis: measure how much rabbinic institutional authority, prestige, and resource allocation depends on the vitality claim. If institutional interests drive persistence despite native-speaker evidence to the contrary, the reading contains an extractive component masked by coordination framing.',
    'If institutional capture is substantial, the reading should be reclassified upward on extractiveness (from 0.18 toward 0.35–0.45) and the beneficiary-victim split should be reconsidered: rabbinic authorities benefit from the definitional monopoly, while native-speaker advocates and secular planners are victimized by exclusion from the institutional site that defines vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_capture, empirical, 'Degree to which institutional interests sustain the reading despite competing empirical evidence.').

omega_variable(
    mandatrophy_staging,
    'At what point (historically) did the founding problem the liturgical reading solves cease to be live, such that the reading''s persistence becomes a zombie function sustained by institutional inertia rather than active coordination need?',
    'Historical markers: (1) the emergence of mass secular Hebrew education (1920s–1950s Mandate/statehood era); (2) the reach of native speakers surpassing liturgical specialists (mid-20th century); (3) the production of native Hebrew literature, law, media independent of rabbinic institutions (ongoing since 1948). The reading was live when liturgical preservation was the primary vitality mechanism; it becomes zombie-like when native generation is the primary mechanism and rabbinic institutions defend the reading retroactively.',
    'If mandatrophy is confirmed with a clear staging date (e.g., 1950s), the constraint''s status changes from live rope to piton (attenuated function, institutional theater, persistent but not dynamically justified). Theater_ratio may underestimate the theatrical component if the reading is performed as vitality-preserving while the actual vitality work happens in native contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_staging, empirical, 'Historical point at which the reading''s founding problem ceased to be the active vitality mechanism.').

omega_variable(
    kernel_vs_reading_distinction,
    'Is the boundary between the contested kernel (what constitutes vitality?) and the competing readings (liturgical vs. native vs. hybrid) stable, or do the readings actually dispute what the kernel IS?',
    'Textual and institutional analysis of each reading''s account of the kernel: do all three readings agree on the question (''what constitutes vitality?'') and diverge on the answer, or does each reading reframe the question itself? If native_daily_reading redefines vitality as ''native generation only,'' it may be disputing the kernel, not offering a reading of it.',
    'If the readings dispute the kernel itself, the three constraint stories are not true siblings but rather distinct constraints with overlapping vocabulary. The network structure would change from ''three readings of one kernel'' to ''three distinct constraint families with a shared domain label.'' Reclassification could require decomposition: separate stories for ''liturgical continuity as a coordination mechanism'' (genuine rope) vs. ''native vitality as the definition'' (different constraint entirely).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Whether the three readings dispute the kernel question or just the answer to a shared question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__liturgical_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__liturgical_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__liturgical_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__liturgical_reading, suppression_requirement, 80, 0.12).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel hebrew_vitality decomposes into three reading-specific constraints: liturgical_reading (this story, low ε, institutional coordination via ritual), native_daily_reading (high ε, native generation required, ritual is preservation not life), and hybrid_continuity_reading (moderate ε, both mechanisms necessary, interaction constitutes vitality). The three readings coexist as live institutional positions held by different factions within Hebrew-speaking and Hebrew-studying communities. They share a kernel (what constitutes vitality?) but offer incompatible answers grounded in different premises about language essence. No single reading forecloses another; they influence each other's resource allocation and institutional positioning. Each story is authored independently per Rule 1 (ε-invariance); the shared kernel and network edges link them per the constraint-family pattern. The sibling stories are authored from distinct committer frames and will show different ε, different beneficiary/victim structures, and different classification endpoints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
