% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: biblical_studies/textual_criticism/religious_authority
 *
 * SUMMARY:
 *   The critical reconstructive reading of biblical source text claims that
 *   historical recovery of the original autograph (the text closest to the
 *   author's compositional intent) is the primary foundation for all
 *   legitimate interpretation. This reading privileges philological,
 *   linguistic, and manuscript-historical evidence over traditional
 *   hermeneutical frameworks or modern theological agendas. The constraint
 *   arises because the reconstructive process is inherently incomplete and
 *   probabilistic — no original text can be recovered with certainty — yet
 *   institutional frameworks (academic careers, publishing authority,
 *   seminary curricula) treat the reconstructed text as though it were
 *   certain. This creates an extraction mechanism: confessional communities
 *   that depend on the received text's interpretive stability bear the cost
 *   of perpetual textual uncertainty, while academic biblical scholarship
 *   benefits from the methodological primacy of reconstruction. The critical
 *   reading instantiates ONE hermeneutical stance within a larger kernel
 *   (biblical source text authority) that admits multiple readings. Sibling
 *   readings — formal equivalence (prioritizing exact word correspondence
 *   across translations) and dynamic equivalence (prioritizing
 *   meaning-for-meaning translation over word-for-word equivalence) —
 *   represent different institutional and theological stakes. The
 *   constraint's extraction profile depends on which reading's legitimacy
 *   framework is operative: low extraction for academic beneficiaries, high
 *   extraction for confessional victims.
 *
 * KEY AGENTS:
 *   - Academic Biblical Scholarship: Primary beneficiary (institutional/arbitrage) — controls publishing venues, university employment, translation committees; benefits from reconstructive methodology's prestige
 *   - Confessional Faith Communities: Primary victim (powerless/trapped) — depend on textual stability for doctrinal coherence; no alternative authority framework fully stabilizes meaning if text is unstable
 *   - Ecclesiastical Interpretive Authority: Secondary victim (moderate/constrained) — must navigate between academic methodological demands and traditional hermeneutical authority; constrained by loss of textual ground
 *   - Progressive Theological Movements: Organized secondary beneficiary (organized/constrained) — use reconstructive methods to retrieve suppressed readings but do not control reconstruction process
 *   - Traditional Manuscript Transmission Authority: Institutional theater-keeper (institutional/arbitrage) — Masoretic text, Byzantine tradition persist through inertia despite methodological challenge
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing historical-critical reconstruction as the only legitimate methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.68).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "biblical_studies/textual_criticism/religious_authority").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '933fef3c-8342-4b29-b88c-5a4d91d1184c').
narrative_ontology:cs_kernel_codification('933fef3c-8342-4b29-b88c-5a4d91d1184c', fixed_text).
narrative_ontology:cs_authority_grounding('933fef3c-8342-4b29-b88c-5a4d91d1184c', extraction).
narrative_ontology:cs_interpretation_layer_present('933fef3c-8342-4b29-b88c-5a4d91d1184c').
narrative_ontology:cs_reading_relation('933fef3c-8342-4b29-b88c-5a4d91d1184c', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('933fef3c-8342-4b29-b88c-5a4d91d1184c', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('933fef3c-8342-4b29-b88c-5a4d91d1184c', foundational, historical_original_primacy).
narrative_ontology:cs_axiom_status(historical_original_primacy, holdable).
narrative_ontology:cs_axiom_grounding('933fef3c-8342-4b29-b88c-5a4d91d1184c', historical_original_primacy, empirically_contingent).
narrative_ontology:cs_axiom('933fef3c-8342-4b29-b88c-5a4d91d1184c', foundational, reconstruction_methodological_necessity).
narrative_ontology:cs_axiom_status(reconstruction_methodological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('933fef3c-8342-4b29-b88c-5a4d91d1184c', reconstruction_methodological_necessity, deontological).
narrative_ontology:cs_reference_frame('933fef3c-8342-4b29-b88c-5a4d91d1184c', authorial_intent_recovery).
narrative_ontology:cs_drift_state('933fef3c-8342-4b29-b88c-5a4d91d1184c', contemporary_textual_criticism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('933fef3c-8342-4b29-b88c-5a4d91d1184c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, historical_critical_establishment).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_faith_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, hermeneutical_tradition_stability).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, ecclesiastical_interpretive_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONFESSIONAL FAITH COMMUNITY (SNARE) — Trapped by institutional dependence on received text authority. The critical reconstructive reading destabilizes the hermeneutical foundation without offering an alternative legitimacy structure that preserves faith coherence. Maximum extraction: the community bears full cost of textual instability (loss of interpretive authority, fragmentation of meaning-making) with minimal coordination benefit. No exit option available without abandoning religious identity.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ECCLESIASTICAL INTERPRETIVE AUTHORITY (TANGLED ROPE) — Constrained by loss of canonical textual ground but also partially benefits from critical scholarship's methodological rigor and resource access (university positions, publishing infrastructure, research funding). The constraint generates both genuine coordination problems (how do we maintain interpretive consistency given textual variability?) and asymmetric extraction (academic methods privilege certain readings over traditional exegesis). Mixed experience: some benefit from critical apparatus, significant cost from authority destabilization.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACADEMIC BIBLICAL SCHOLARSHIP (ROPE) — Primary beneficiary. The critical reconstructive reading is their foundational methodology. They experience the constraint as pure coordination: establishing a shared philological and textual-critical apparatus enables collaborative knowledge production. Net beneficiary — access to university employment, publishing venues, research funding, and hermeneutical authority flow toward this community. The reconstructive reading legitimates their disciplinary authority and methods.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE THEOLOGICAL MOVEMENTS (TANGLED ROPE) — Organized agents (feminist theology, liberation theology, postcolonial biblical hermeneutics) use critical reconstruction methods to retrieve suppressed textual evidence and challenge traditional patriarchal/colonial readings. They benefit from the methodology (access to uncovered alternative readings) but are constrained by the same textual uncertainty that destabilizes confessional authority. Asymmetric coordination: critical methods serve their agenda, but they do not control the reconstruction process itself.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRADITIONAL MANUSCRIPT TRANSMISSION AUTHORITY (PITON) — The Masoretic Text, Byzantine manuscript tradition, and established canons of textual criticism persist through institutional inertia despite the critical reconstructive reading's methodological challenge to their authority. These systems are now performative: maintained because alternatives haven't fully displaced them and because coordinating a global shift would be costly, not because the reconstruction process has validated their stability. Theater ratio high: ceremonial affirmation of text-as-received persists in liturgical and devotional contexts even as scholarship openly acknowledges its contingency.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a logical/epistemic perspective, textual corruption and manuscript variation are inherent features of any orally-transmitted then textualized tradition. No original can be recovered with certainty — this is an immutable constraint of historical knowledge. Reconstruction is always partial and probabilistic. However, this naturalizes what is actually a contingent historical choice: whether to privilege historical-critical recovery over other hermeneutical frameworks. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_source_text__critical_reconstructive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the critical reconstructive reading creates asymmetric benefit distribution. Academic scholarship captures disproportionate career and authority benefits from methodological primacy, while confessional communities bear costs of perpetual textual uncertainty without corresponding authority gains. The value has risen over the interval (0.28 → 0.58) as reconstructive methodology has consolidated institutional power in universities, seminaries, and translation committees. Suppression (0.68): Moderate-high. Barriers to alternative readings include: institutional gatekeeping in academic publishing, training requirements that privilege critical methodology in graduate programs, inclusion of critical apparatus in standard editions (making alternative textual bases invisible), and discursive framing that treats non-critical readings as less rigorous. These barriers are not total — some confessional communities maintain alternative hermeneutical frameworks — but they create significant costs for those who resist. Theater ratio (0.61): Moderate-high, reflecting that much critical-reconstructive discourse involves performative certainty about textual recovery despite epistemological uncertainty about achievability. Scholarly consensus affirms reconstructed texts as stable products while acknowledging manuscript variations and methodological disagreements. The theater has increased over the interval (0.35 → 0.61) as the reconstructive apparatus has become more elaborate and more ceremonially deployed in institutional contexts.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (critical reconstruction of biblical source text) classifies as Rope for its primary beneficiaries (academic scholarship), Snare for its primary victims (confessional communities), Tangled Rope for agents who benefit from some aspects but are constrained by others, Piton for institutions maintaining traditional alternatives through inertia, and Mountain from an analytical view that risks naturalizing one particular hermeneutical framework as inevitable. The gap reveals that the constraint's type depends on whether the observer is embedded in the institutional framework that privileges critical reconstruction or embedded in alternative hermeneutical traditions. For academic readers, reconstruction is a coordinating mechanism (Rope). For confessional readers, it is an extraction mechanism (Snare). For those navigating both, it is hybrid (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directional computation (d) derives from structural position: beneficiaries (academic scholarship) with arbitrage options (can publish, teach, redirect careers) experience low d → negative/low effective extraction; victims (confessional communities) with trapped options (depend on text's interpretive stability for religious identity) experience high d → high effective extraction. Ecclesiastical authorities experience mixed directionality: they are beneficiaries of critical methodology's rigor but victims of the methodological challenge to their interpretive authority. Progressive theological agents have access to critical methods (arbitrage into new readings) but do not control the reconstruction apparatus itself (constrained by the academy's methodological primacy). The false summit emerges at the analytical perspective: the claim that textual reconstruction is an immutable feature of historical knowledge naturalizes what is actually a contested hermeneutical choice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_text_recoverability,
    'Is recovery of a historically-original text epistemically possible given the manuscript evidence, or does the reconstructive reading presume an unattainable ideal?',
    'Meta-analysis of textual-critical methodologies; assessment of whether reconstruction converges on a stable hypothesis or diverges across different methodological schools; empirical test whether two independent reconstruction teams produce identical texts',
    'If recoverable: reconstruction is a legitimate coordinating mechanism (Rope becomes more defensible). If unrecoverable: the constraint is an epistemically-grounded extraction mechanism (Snare becomes more defensible for confessional communities).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_text_recoverability, conceptual, 'Whether historical-original text recovery is epistemically achievable').

omega_variable(
    reading_primacy_assumption,
    'Is the critical reconstructive reading''s privilege of historical-critical reconstruction over other hermeneutical frameworks (canonical, liturgical, traditionary) a methodological necessity or an institutional preference?',
    'Comparative analysis of hermeneutical frameworks; identification of whether non-critical frameworks produce incoherent readings or merely different readings; documentation of institutional gatekeeping that privileges critical methodology in academic and publishing venues',
    'If methodological necessity: the framework''s asymmetric extraction is justified by superior epistemic access. If institutional preference: the extractiveness value should be reclassified as institutional extraction masquerading as methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_primacy_assumption, conceptual, 'Whether critical reconstruction is methodologically necessary or institutionally preferred').

omega_variable(
    meaning_stability_paradox,
    'Can textual meaning remain stable across reconstructed variants, or does text instability necessarily entail meaning instability?',
    'Philosophical analysis of what constitutes textual identity and meaning identity; case studies of confessional communities maintaining doctrinal stability despite engaging with critical textual evidence; assessment of whether meaning-making is more dependent on stable text or stable interpretive tradition',
    'If meaning can decouple from text: confessional communities have exit option (tradition-grounded interpretation survives textual instability; Snare reclassifies to Tangled Rope). If meaning requires text stability: confessional communities remain trapped in epistemic dependency on textual recovery success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_stability_paradox, conceptual, 'Whether textual instability entails meaning instability in interpretation').

omega_variable(
    beneficiary_circularity,
    'Does the critical reconstructive reading constitute an extraction from confessional communities, or does it constitute a legitimate challenge to false certainty claims that confessional frameworks previously made?',
    'Genealogical analysis of confessional authority claims before critical reconstruction became widespread; assessment of whether pre-critical confessions required belief in textual stability/inerrancy or merely in the text''s spiritual authority; documentation of whether confessional communities could adjust hermeneutical frameworks without abandoning faith coherence',
    'If extraction: the constraint remains Tangled Rope/Snare with clear victim status. If legitimate challenge: the extraction is justified by superior epistemic access and confessional damage was collateral to correcting false claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_circularity, conceptual, 'Whether critical reconstruction extracts from or challenges false certainty in confessional frameworks').

omega_variable(
    kernel_reading_contest,
    'Which reading of the biblical source text kernel is structurally defensible: critical reconstruction, formal equivalence, or dynamic equivalence?',
    'This omega documents that this constraint is ONE reading of a contested kernel. Sibling readings (formal_equivalence_reading, dynamic_equivalence_reading) instantiate different structural axioms and produce different beneficiary/victim profiles. No single reading logically forecloses the others — they coexist as live positions held by different institutional actors.',
    'The critical reconstructive reading''s extraction profile depends on which reading''s authority framework you are measuring from. From within academic biblical scholarship, it is Rope (coordination). From within confessional frameworks, it is Snare (extraction). The omega documents that this perspectival gap is not a measurement error but a structural feature of the kernel''s contested status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: critical reconstruction vs sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibsrc_theater_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bibsrc_theater_t25, biblical_source_text__critical_reconstructive_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(bibsrc_theater_t50, biblical_source_text__critical_reconstructive_reading, theater_ratio, 50, 0.61).

% Extraction over time
narrative_ontology:measurement(bibsrc_extract_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bibsrc_extract_t25, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(bibsrc_extract_t50, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibsrc_suppress_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bibsrc_suppress_t25, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(bibsrc_suppress_t50, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, ecclesiastical_interpretive_authority).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, confessional_textual_stability).

% DUAL FORMULATION NOTE:
% The critical reconstructive reading is one member of a constraint family centered on the kernel 'biblical source text authority.' The family contains three core reading instantiations (critical reconstructive, formal equivalence, dynamic equivalence) which are structurally related through their shared kernel but distinct in their axioms, beneficiary profiles, and extractiveness values. Each reading produces different effects downstream: on ecclesiastical authority structures, on confessional identity coherence, and on interpretation methodologies. The family should be traversed together to understand the full structure of textual authority in biblical studies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
