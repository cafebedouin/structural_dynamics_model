% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Formal Equivalence Translation: Source Language Authority and Reader Access
 *   domain: religious_authority/translation_theory/hermeneutics
 *
 * KEY AGENTS:
 *   - Non-specialist readers: Primary victims (powerless/trapped) — face complete comprehension barriers and must defer to specialist interpretation
 *   - Educated lay readers: Secondary victims (moderate/constrained) — can partially access the text but must invest significant hermeneutical labor and face social suppression if deviating from approved interpretation
 *   - Hermeneutically conservative traditions (e.g., traditional liturgical communities, fundamentalist exegetical schools): Primary beneficiaries (institutional/arbitrage) — maintain textual authority through source-language fidelity and specialist gate-keeping
 *   - Specialist translators and biblical scholars: Organized beneficiaries (organized/constrained) — maintain professional authority and interpretive hierarchy through formal-equivalence methodology
 *   - Historical translation apparatus (King James Version, scholarly critical editions): Institutional inertia (institutional/arbitrage) — perpetuates formal-equivalence as standard through momentum and prestige despite degraded functional role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.62).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.58).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Translation: Source Language Authority and Reader Access").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious_authority/translation_theory/hermeneutics").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c').
narrative_ontology:cs_kernel_codification('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', fixed_text).
narrative_ontology:cs_authority_grounding('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', lineage).
narrative_ontology:cs_interpretation_layer_present('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c').
narrative_ontology:cs_reading_relation('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', foundational, form_preserves_meaning).
narrative_ontology:cs_axiom_status(form_preserves_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', form_preserves_meaning, conventional).
narrative_ontology:cs_axiom('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', foundational, reader_responsibility_for_access).
narrative_ontology:cs_axiom_status(reader_responsibility_for_access, holdable).
narrative_ontology:cs_axiom_grounding('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', reader_responsibility_for_access, deontological).
narrative_ontology:cs_reference_frame('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', source_language_structural_fidelity).
narrative_ontology:cs_drift_state('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', contemporary_digital_translation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7e5f9a2-1b3d-4e8f-9c2a-7d6e5f4a3b2c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, specialist_translators).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, textual_authority_maintainers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_readers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_congregations).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, linguistic_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SPECIALIST READER (SNARE) — Trapped by educational barriers and linguistic access requirements. Formal equivalence prioritizes source-language structure preservation over reader comprehension, creating maximum extractiveness for those without specialized hermeneutical training. No exit option: cannot understand the text without submitting to the translation's structural priorities. Cannot choose alternative reading frameworks within their own community without violating doctrinal authority.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EDUCATED LAY READER (TANGLED ROPE) — Constrained but not trapped. Benefits from the precision and authority-grounding that formal equivalence provides (coordination function: stable textual meaning across interpretive communities). But bears extraction: must invest significant cognitive effort to parse source-language structures embedded in English, and faces suppression if attempting to reinterpret via more accessible readings. Moderate exit option: can consult dynamic-equivalence translations or commentaries, but doing so risks community disapproval and doctrinal instability.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HERMENEUTICALLY CONSERVATIVE TRADITION (ROPE) — Primary beneficiary. Experiences formal equivalence as pure coordination: preserves source-language structure, maintains textual stability across centuries, prevents unauthorized re-interpretation, enables authoritative hermeneutical control. Zero experienced extraction because the constraint's operation IS the coordination goal. Full arbitrage: can maintain authority by insisting on the source-language reading while framing accessibility barriers as legitimate requirements for serious engagement.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SPECIALIST TRANSLATION COMMUNITY (TANGLED ROPE) — Organized beneficiaries (scholars, professional translators, exegetes) who maintain the formal-equivalence standard. Benefit from the constraint: their specialized knowledge becomes valuable; their interpretive authority is protected by the gate-keeping function of source-language mastery. But also constrained: must defend the translation choice against public accessibility demands, must continuously justify why reader comprehension is subordinate to formal structure, must suppress alternative methodologies. Active enforcement required to sustain the hierarchy.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: HISTORICAL TRANSLATION APPARATUS (PITON) — The formal-equivalence methodology itself as institutional inertia. Once served crucial coordination function (stabilizing text across scribal traditions, preventing doctrinal drift). But in modern context with print stability and critical apparatus widely available, the primary function has degraded. Maintained through institutional momentum (Authorized Version, King James Version legacy, scholarly prestige) and theater (formal equivalence continues to be performed as THE scholarly standard even when its functional role is no longer primary). Theater ratio high because much of the formal-equivalence apparatus now serves certification and boundary-maintenance rather than actual meaning-preservation.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some gap between source language and target language is inherent to translation: languages have different structures, idioms, conceptual frameworks. Formal equivalence appears to be responding to an irreducible constraint of linguistic transmission itself — you cannot preserve meaning without preserving structure. This perspective risks naturalizing what is actually a contingent methodological choice. The engine will identify this as a false summit: the choice to prioritize source-language fidelity over reader access is not a law of linguistics but a hermeneutical commitment with identifiable beneficiaries.
constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_source_text__formal_equivalence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The formal-equivalence reading creates substantial extraction from non-specialist readers through incomprehension barriers and enforced dependence on specialist interpretation. However, the extraction is not maximal (0.72+) because the constraint also provides genuine coordination value — stabilizing textual meaning across centuries and interpretive communities, preventing unlimited re-interpretation, maintaining doctrinal coherence. The measured value reflects the hybrid: real coordination function overlaid with asymmetric access and power-maintenance. Measurement trajectory (0.48 → 0.62 over 300 years) shows extraction increasing as print standardization removes the functional need for formal equivalence as text-stabilization mechanism, yet the constraint persists and intensifies through institutional inertia and prestige-maintenance. Suppression (0.58): Moderate-high and rising (0.45 → 0.58). Barriers include educational gatekeeping (specialist training requirements), cultural suppression (lay members risk community disapproval when consulting alternative translations), institutional enforcement (churches and denominations enforce formal-equivalence as doctrinal norm), and linguistic complexity (target-language structures preserved from source that create parsing difficulty). Theater ratio (0.65, rising 0.40 → 0.65): High and increasing. In earlier period (medieval to early modern), formal equivalence served essential function: scribal copying required fidelity to preserve text before print; specialist translators were genuinely necessary because languages were less standardized. In modern context with digital texts, critical apparatus, comparative translations readily available, much of formal equivalence becomes performative — it continues as scholarly standard and spiritual practice rather than functional requirement for text stability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The non-specialist reader sees pure extraction (Snare) — comprehension barriers with no coordination benefit from their structural position. The educated lay reader sees mixed coordination-extraction (Tangled Rope) — they benefit from textual stability but pay heavy cognitive access costs. The conservative tradition sees pure coordination (Rope) — formal equivalence IS their goal, experienced as solving the problem of textual authority and preventing hermeneutical drift. Specialist translators see mixed coordination-extraction with benefits (Tangled Rope) — they maintain expertise value through gate-keeping while genuinely solving coordination problems for the tradition. The historical apparatus itself appears degraded (Piton) — the function it served (text stabilization before print) has been mechanically automated; it persists through institutional momentum. The civilizational observer risks seeing natural law (Mountain) — the appearance that some gap between languages is inherent and formal equivalence is the natural response — but the structural data reveals this as false summit: the choice to prioritize structure over comprehension is a contingent hermeneutical commitment, not linguistic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from power level, exit options, and structural position relative to extraction flow. Non-specialists (powerless/trapped/no arbitrage) experience maximum d ≈ 0.95, producing high f(d) ≈ 1.42, high experienced extractiveness. Educated lay (moderate/constrained/no arbitrage) experience medium-high d ≈ 0.75, producing f(d) ≈ 1.15. Conservative tradition beneficiaries (institutional/arbitrage) experience low d ≈ 0.05, producing f(d) ≈ -0.12, negative experienced extraction (they gain from the constraint). Specialist community (organized/constrained/partial arbitrage) experience medium d ≈ 0.45, producing f(d) ≈ 0.45. The constraint's canonical chi at national scope (σ=1.0) is approximately χ ≈ 0.62 × 0.75 × 1.0 ≈ 0.465 for the victim-median perspective, placing the constraint in tangled-rope territory. For beneficiary perspectives, χ turns negative, indicating pure coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy not by collapsing the six types into one but by showing why each type is correct for its perspective. The conservative tradition experiences Rope (pure coordination). The non-specialist experiences Snare (pure extraction). Both observations are structurally accurate — they are measuring from different positions in the constraint's directionality field. The analytical observer who tries to declare ONE true type commits the false summit error: naturalizing the beneficiary's coordination goal as if it were an invariant property of translation itself. The formal-equivalence reading's mandatrophy is resolved by recognizing that the constraint simultaneously coordinates for one set of agents (specialist community, conservative tradition) and extracts from another set (non-specialists). This is the definition of Tangled Rope. The claim of pure coordination (Rope from beneficiary position) is legitimate local experience. The claim of pure extraction (Snare from victim position) is legitimate local experience. The Tangled Rope classification is the global structure that makes both local experiences true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaning_conservation_mechanism,
    'Does formal equivalence actually preserve meaning better than dynamic equivalence, or does it merely preserve form while meaning shifts through historical semantic drift?',
    'Comparative analysis of meaning-shift patterns: semantic stability test comparing formal-equivalence translations at 50-year intervals vs. dynamic-equivalence translations measuring conceptual fidelity to original intent; post-hoc reconstruction of original meaning from fragmentary evidence and correlation with both translation approaches',
    'If form preservation = meaning preservation: formal equivalence is functionally justified and extraction cost is legitimate educational overhead. If form preservation ≠ meaning preservation: the constraint''s claimed coordination function (stable meaning) is illusory and extraction is pure, not tangled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meaning_conservation_mechanism, empirical, 'Whether formal equivalence preserves meaning or only form').

omega_variable(
    specialization_necessity_claim,
    'Is reader incomprehension a feature (preventing misinterpretation) or a bug (preventing access)? Does the interpretive hierarchy serve meaning-protection or power-maintenance?',
    'Historical reconstruction: compare error rates and doctrinal disputes in high-literacy vs. low-literacy reading communities; measure alignment between specialist interpretations and lay reinterpretations; track which interpretive innovations originate from specialist vs. lay readings',
    'If incomprehension protects meaning: suppression is legitimate overhead and coordination claim is valid. If incomprehension enables hierarchy: suppression is extraction and coordination is secondary to power-maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialization_necessity_claim, conceptual, 'Whether reading barriers serve meaning-protection or hermeneutical authority').

omega_variable(
    sibling_reading_empirical_underdetermination,
    'Can empirical historical evidence distinguish between formal-equivalence and dynamic-equivalence readings, or do the three sibling readings form a hermeneutical underdetermination where different methodologies produce incompatible but internally coherent interpretive frameworks?',
    'Comparative textual analysis: identify claims where formal-equivalence, dynamic-equivalence, and critical-reconstructive readings produce contradictory results; assess whether empirical evidence from ancient manuscripts, linguistic parallels, or historical context can adjudicate between them or whether each reading remains consistent with available evidence while diverging at the axiom level',
    'If empirically decidable: one reading has better fit to evidence. If underdetermined: the three readings are genuine alternatives rooted in axiom choice, not in evidence. This determines whether the constraint''s falsifiability is epistemic or preference-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_underdetermination, empirical, 'Whether sibling readings are empirically distinguishable or axiomatically underdetermined').

omega_variable(
    reading_as_kernel_identity,
    'Is the formal-equivalence reading a distinct constraint, or is it one instantiation of a larger commitment system where the kernel IS the contested notion of textual authority itself?',
    'Structural decomposition: test whether formal equivalence can be abandoned while maintaining the hermeneutically conservative tradition''s core commitments (textual stability, authoritative interpretation, doctrinal continuity). If yes: formal equivalence is one reading of authority. If no: formal equivalence IS the reading and the constraint is the kernel.',
    'If modular: the constraint''s classification can shift if alternative readings achieve equal institutional standing. If constitutive: the constraint is locked to the conservative tradition''s identity and cannot be dislodged without institutional dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_identity, conceptual, 'Whether formal equivalence is modular reading choice or constitutive to conservative authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bst_fe_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bst_fe_tr_t150, biblical_source_text__formal_equivalence_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement(bst_fe_tr_t300, biblical_source_text__formal_equivalence_reading, theater_ratio, 300, 0.65).

% Extraction over time
narrative_ontology:measurement(bst_fe_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bst_fe_be_t150, biblical_source_text__formal_equivalence_reading, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(bst_fe_be_t300, biblical_source_text__formal_equivalence_reading, base_extractiveness, 300, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bst_fe_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bst_fe_su_t150, biblical_source_text__formal_equivalence_reading, suppression_requirement, 150, 0.52).
narrative_ontology:measurement(bst_fe_su_t300, biblical_source_text__formal_equivalence_reading, suppression_requirement, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, hermeneutical_authority_gate_keeping).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, specialist_knowledge_asymmetry).

% DUAL FORMULATION NOTE:
% The biblical source text kernel decomposes into three distinct constraint stories, each representing a different reading. The formal-equivalence reading (this constraint) has ε=0.62 and structures around source-language priority. The dynamic-equivalence sibling has lower extraction on lay readers (estimated ε=0.38) because it prioritizes comprehension, but may have higher extraction on specialist communities who see authority diffusion. The critical-reconstructive sibling involves substantially higher epistemological extraction (estimated ε=0.58) because it demands acceptance of historical-critical methodology that many conservative readers reject. Each reading has different beneficiary and victim sets. All three are linked via network.affects_constraints to show they are alternative instantiations of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
