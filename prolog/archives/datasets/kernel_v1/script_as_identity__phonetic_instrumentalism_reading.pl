% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Script as Neutral Technology: Latin Phonetic Optimization for Turkish Vowel Harmony
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   The script reform for Turkish represents a contested kernel where three
 *   distinct readings instantiate structurally different constraints. This
 *   constraint embodies the PHONETIC INSTRUMENTALISM reading: script choice
 *   is a neutral technical optimization problem. Latin script provides
 *   superior phonetic transparency for Turkish vowel harmony. The state
 *   apparatus adopts the most efficient writing system. Under this reading,
 *   the constraint has low extractiveness (0.35) because the decision is
 *   framed as politically neutral, driven by linguistic-technical criteria
 *   rather than sovereignty claims. However, the structural data reveals
 *   Tangled Rope dynamics: the state benefits from depicting script choice as
 *   technical optimization (denies identity-encoding function, conceals
 *   sovereignty claim); Turkish continuity communities and Ottoman-era
 *   interpreters bear extraction costs (alienation, literacy struggle,
 *   professional knowledge devaluation); the bilingual generation experiences
 *   both coordination (modernization, technical access) and extraction
 *   (identity bifurcation, alienation). Theater increases over time (0.45 →
 *   0.68) as the phonetic instrumentalism framing becomes entrenched
 *   institutional doctrine, while suppression also increases (0.40 → 0.52) as
 *   the cost of questioning the technical neutrality frame rises. The
 *   analytical observer risks naturalizing this reading as a universal truth
 *   (Mountain perspective) — that script choice is inherently a technical
 *   optimization problem — when the reading is actually one interpretation of
 *   a politically contested kernel. The phonetic instrumentalism reading is
 *   held by the state apparatus and technical standardizers; the
 *   ottoman_continuity_reading (not this constraint) is held by continuity
 *   advocates; the kemalist_rupture_reading (not this constraint) is held by
 *   those who see the script reform as purposeful identity rupture, not
 *   accidental technical consequence.
 *
 * KEY AGENTS:
 *   - Alphabet Reform State Apparatus: Primary beneficiary (institutional/arbitrage) — captures sovereignty authority to define national literacy identity while framing decision as technical; obscures political claim through phonetic instrumentalism
 *   - Ottoman Continuity Interpreters: Primary victim (powerless/trapped) — bears extraction through script alienation and professional devaluation; cannot contest the decision within the technical-instrumental frame
 *   - Bilingual Generation: Secondary victim (moderate/constrained) — experiences both coordination benefit (modernization, technical literacy) and extraction cost (identity bifurcation, childhood literacy struggle, alienation from historical texts)
 *   - Technical Standardization Advocates: Organized beneficiary (organized/mobile) — linguists and technical professionals who frame script as efficiency optimization; retain agency and see sunset horizon as technical standards stabilize
 *   - International Standardization Framework: Institutional actor (institutional/arbitrage) — maintains phonetic instrumentalism as degraded doctrine through ISO scripts and UNESCO transliteration standards; persists through bureaucratic inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the reading's depoliticizing frame as universal truth about how writing systems work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.35).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.52).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Script as Neutral Technology: Latin Phonetic Optimization for Turkish Vowel Harmony").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '8e32ffc6-3a1b-4b2c-aa06-b627257bab83').
narrative_ontology:cs_kernel_codification('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', formalized).
narrative_ontology:cs_authority_grounding('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', extraction).
narrative_ontology:cs_interpretation_layer_present('8e32ffc6-3a1b-4b2c-aa06-b627257bab83').
narrative_ontology:cs_reading_relation('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', script_as_identity__kemalist_rupture_reading, influences).
narrative_ontology:cs_axiom('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', foundational, script_is_neutral_technology).
narrative_ontology:cs_axiom_status(script_is_neutral_technology, holdable).
narrative_ontology:cs_axiom_grounding('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', script_is_neutral_technology, instrumental).
narrative_ontology:cs_axiom('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', secondary, phonetic_transparency_determines_literacy_efficiency).
narrative_ontology:cs_axiom_status(phonetic_transparency_determines_literacy_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', phonetic_transparency_determines_literacy_efficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', technical_phonetic_optimization_neutral).
narrative_ontology:cs_drift_state('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', contemporary_identity_politics_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e32ffc6-3a1b-4b2c-aa06-b627257bab83', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, technical_standardization_advocates).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, alphabet_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, modernist_intellectual_class).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_interpreters).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, script_identity_coalitions).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, historical_literacy_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN CONTINUITY INTERPRETERS (SNARE) — Trapped within the new script regime; cannot exit literacy practices without remaking their identity and professional knowledge. The phonetic instrumentalism framing forecloses recognition of their structurally legitimate claim: that script choice encodes identity and continuity, not merely phonetic efficiency. Experiences extraction as erasure. Zero degrees of freedom within the national literacy system.
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BILINGUAL GENERATION (TANGLED ROPE) — Faces the coordination benefit (access to modernized technical literature, state employment) alongside extraction costs (childhood literacy struggle, identity bifurcation, alienation from Ottoman-era texts). Constrained by education system requirements and career incentives. Both coordination and extraction are structurally real.
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALPHABET REFORM STATE APPARATUS (ROPE) — Benefits from depicting script choice as technical optimization problem. The phonetic instrumentalism framing enables the state to coordinate literacy standardization while obscuring that the constraint is also a sovereignty claim (authority to redefine the nation's written identity). Experiences the constraint as pure coordination: solving the nation's literacy efficiency. High arbitrage optionality — can shift the framing if political costs rise.
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNICAL STANDARDIZATION ADVOCATES (SCAFFOLD) — Organized agents (linguists, printing engineers, UNESCO standardizers) frame script choice as a solvable technical problem with sunset logic: once phonetic optimization is complete and literacy norms stabilize, script choice becomes neutral infrastructure. Low extraction because organized advocates retain agency and see a time horizon for transition completion. Theater is moderate (technical committees, comparative phonetics studies).
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL STANDARDIZATION FRAMEWORK (PITON) — Maintains the phonetic instrumentalism as degraded institutional doctrine. The global framework persists through bureaucratic inertia (ISO scripts, UNESCO transliteration standards) despite low functional verification that phonetic transparency actually optimizes literacy acquisition across different writing systems and L1 contexts. The framework sees script choice as neutralized but performs the neutralization theatrically, not functionally.
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, phonetic transparency is a universal property of writing systems: all scripts must represent the sound structure of their language. From this view, Latin script's phonetic fit to Turkish is an immutable optimization that transcends political choice. This perspective risks naturalizing what is actually a reading-specific framing: that scripts are tools with measurable phonetic properties, not identity-bearing institutions. Engine false summit detection reveals the naturalization.
constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(script_as_identity__phonetic_instrumentalism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, TR),
    TR >= 0.70.

:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low-moderate. This reading frames script choice as a technical-optimization problem with measurable phonetic properties. Under this frame, the state's authority to adopt Latin script flows from linguistic efficiency, not political sovereignty. The reading depoliticizes the decision and obscures that script choice is always identity-encoding. However, extractiveness is not zero because the beneficiaries (state apparatus, modernist class) gain asymmetric advantage: they control which technical metrics 'matter' (phonetic transparency privileged; historical continuity devalued), and they benefit from denying the identity dimension. The constraint exhibits extraction primarily through suppression — not coercive barriers to exit, but denial of legitimacy for contesting the technical frame. Suppression (0.52): Moderate-high. The phonetic instrumentalism frame suppresses competing readings of the kernel: it prevents ottoman_continuity_reading from being heard as technically legitimate (continuity advocates are heard as 'resisting progress' rather than defending a structural claim about script-as-identity). It suppresses the kemalist_rupture_reading by denying intentionality (the reform was 'necessary for literacy efficiency,' not purposeful rupture). Suppression increases over time (0.40 → 0.52) as the technical frame becomes institutionalized doctrine — questioning it becomes 'unscientific,' not legitimate political disagreement. Theater (0.68): Moderate-high and rising. The phonetic instrumentalism reading performs technical analysis (comparative phonetics studies, literacy rate statistics, phoneme-grapheme mappings) that is real but also performative — the technical apparatus exists partly to justify a decision made on other grounds (sovereignty, modernization, national identity). The performance increases over time as the institutional commitment deepens. The constraint is not pure theater (some technical work is genuine) but the balance shifts toward performance as political commitment hardens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (Turkish script reform) appears as pure technical optimization (Rope from state apparatus perspective) or as extractive sovereignty claim disguised as technical necessity (Snare from ottoman_continuity perspective) or as a coordinated-but-problematic transition (Tangled Rope from bilingual generation) or as a solvable technical problem with sunset (Scaffold from organized technical advocates) or as degraded institutional ritual (Piton from international standardization framework) or as an immutable property of writing systems (Mountain from analytical observer — but engine false summit detection reveals naturalization). The perspectival gap reveals that the phonetic_instrumentalism_reading achieves low extractiveness precisely by obscuring the constraint's political-identity dimensions. From the state apparatus perspective, extractiveness is low because the decision appears technical. From the ottoman_continuity perspective, extractiveness is high because the decision is a sovereignty claim denying their legitimacy. The gap is not measurement disagreement but framing disagreement: what counts as the relevant dimension (phonetics vs identity) and who controls which dimensions matter.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality reveals how different agents experience the same constraint based on their structural position. This reading (phonetic instrumentalism) depoliticizes the decision, which lowers the experienced extraction for beneficiaries (state apparatus sees coordination, not sovereignty claim) and increases suppression for targets (ottoman_continuity interpreters cannot contest the decision within the technical frame, so the constraint appears immutable rather than negotiable). If the ottoman_continuity_reading were the operative framing, the same constraint would show higher extractiveness (explicit sovereignty extraction) but lower suppression (legitimacy of contesting the reading is recognized). The directionality values capture this: beneficiaries have low d (easy exit via reframing); targets have high d (trapped within the framing). The analytical observer's mountain classification risks naturalizing the phonetic_instrumentalism_reading as universal truth about script-technology, preventing recognition that all three readings are legitimate interpretations of a politically contested kernel.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonetic_transparency_measurement_basis,
    'What constitutes ''superior phonetic transparency''? Is it a measurable property of the script system, or a normative judgment about which linguistic properties matter most?',
    'Cross-linguistic phonetic transparency metrics (diacritical requirements, grapheme-to-phoneme ambiguity ratios, orthographic depth) applied to Ottoman script vs Latin script for Turkish. Comparison with literacy acquisition rates and error patterns in bilingual populations.',
    'If transparency is measurable and Latin outperforms Ottoman: phonetic instrumentalism is empirically grounded; constraint may downgrade to Rope (pure coordination). If transparency is normative (privileging certain features over others): phonetic instrumentalism is a reading choice, not a technical fact; constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonetic_transparency_measurement_basis, empirical, 'Whether phonetic transparency is a measurable property or normative judgment').

omega_variable(
    reading_kernel_ambiguity,
    'Is the kernel (script selection for Turkish) fundamentally about technical optimization, or about defining national identity through writing authority?',
    'Historical textual analysis: examine official justifications for the reform across time periods. Discourse analysis: compare technical linguistics journals with political speeches and constitutional documents. Interview/archival evidence: did Ottoman continuity advocates frame their resistance in technical terms (disputing the phonetic claim) or in identity/continuity terms (script encodes Ottoman heritage)?',
    'If kernel is genuinely technical: phonetic_instrumentalism_reading is the correct framing; Ottoman_continuity_reading is ideological misreading. If kernel is identity-encoding authority: phonetic_instrumentalism_reading is a depoliticizing cover story; Ottoman_continuity_reading correctly identifies the kernel as political. This omega cannot be resolved within this reading''s framework — it requires cross-reading comparison.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the kernel is technical optimization or identity-encoding authority').

omega_variable(
    vowel_harmony_phonetic_advantage,
    'Does Latin script with diacriticals actually provide phonetically superior representation of Turkish vowel harmony compared to Ottoman script variants?',
    'Contrastive phonetic analysis: clarity of vowel harmony markings in both scripts; typography and legibility studies; cognitive load comparison in processing vowel harmony distinctions between scripts; historical literacy error rates if data available.',
    'If Latin is demonstrably superior: the phonetic instrumentalism reading has empirical grounding independent of political authority. If Ottoman script variants are comparably transparent (or superior in some dimensions): the phonetic claim is normative preference disguised as technical fact; suppression value may rise (engine is forced to model the reading as more extractive than claimed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vowel_harmony_phonetic_advantage, empirical, 'Phonetic transparency advantage of Latin vs Ottoman for Turkish vowel harmony').

omega_variable(
    script_identity_decoupling_possibility,
    'Can a script ever be truly neutral technology, or does any script choice constitute an identity-encoding act because scripts are always embedded in institutional authority structures?',
    'Theoretical: examine whether ''neutral script'' is a coherent concept or a category error. Empirical: track whether agents who adopt Latin script for instrumental reasons still experience script choice as identity-relevant (through cultural attachment, historical alienation, or institutional power dynamics). Compare to other ''neutral'' technology adoptions (metric system, electrical standards) — do they produce similar identity-encoding effects?',
    'If scripts cannot be neutral: phonetic_instrumentalism_reading is inherently a depoliticizing cover story; Tangled Rope classification is diagnostically correct because suppression works through denying the identity dimension. If scripts can be neutral under certain institutional conditions: the reading may be valid and extractiveness genuinely lower than feared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(script_identity_decoupling_possibility, conceptual, 'Whether scripts can be neutral technology or are always identity-encoding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_phon_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(script_phon_tr_t5, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(script_phon_tr_t10, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(script_phon_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(script_phon_be_t5, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement(script_phon_be_t10, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(script_phon_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(script_phon_su_t5, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(script_phon_su_t10, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).

% DUAL FORMULATION NOTE:
% The script_as_identity kernel decomposes into three constraint stories, each representing a distinct reading of the same stabilized commitment. The phonetic_instrumentalism_reading (this constraint) has extractiveness 0.35 and claims technical neutrality. The ottoman_continuity_reading has higher extractiveness and emphasizes identity-encoding and continuity rupture. The kemalist_rupture_reading has lowest suppression (the rupture is acknowledged) but highest extractiveness (the rupture is intentional sovereignty exercise). All three stories link via network.affects_constraints because the three readings compete in the same institutional and political space. The constraining mechanism (Turkish script choice) is identical; the extractiveness values differ because each reading emphasizes different structural dimensions (technical vs political, continuity vs rupture, neutral vs intentional). No single ε value captures the constraint across all readings — the ε-invariance principle requires decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, institutional, 0.12).
constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, analytical, 0.71).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
