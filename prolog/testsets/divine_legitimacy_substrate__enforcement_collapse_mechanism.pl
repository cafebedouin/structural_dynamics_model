% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__enforcement_collapse_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__enforcement_collapse_mechanism, []).

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
 *   constraint_id: divine_legitimacy_substrate__enforcement_collapse_mechanism
 *   human_readable: Divine Legitimacy Substrate: Enforcement Collapse Mechanism
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   The divine legitimacy substrate represents the institutional apparatus by
 *   which premodern rulers and priesthoods coordinate subject compliance
 *   through transcendent authority claims rather than direct enforcement.
 *   This constraint is ONE READING of the contested kernel
 *   divine_legitimacy_substrate, specifically the
 *   enforcement_collapse_mechanism reading. This reading frames the
 *   constraint as a coordination-extraction hybrid that degrades over time as
 *   enforcement mechanisms improve and alternative legitimacy frames
 *   proliferate. The kernel itself is ambiguous: different historical actors
 *   and modern analysts read divine legitimacy systems differently — some as
 *   eternally necessary (natural law reading), some as contingent
 *   institutional arrangements (enforcement-collapse reading), some as pure
 *   extraction masks (snare reading). This story instantiates the
 *   enforcement-collapse reading, which holds that divine legitimacy systems
 *   function as genuine coordination mechanisms in societies lacking secular
 *   bureaucratic capacity, but gradually degrade into extractive inertia as
 *   state capacity and alternative authority frames emerge. The measurement
 *   trajectory shows the diagnostic signature: extractiveness rises (0.32 →
 *   0.58) while suppression falls slightly (0.72 → 0.65), with theater ratio
 *   rising sharply (0.42 → 0.68). This pattern indicates the system's
 *   functional necessity is declining while its performative content
 *   increases — a textbook piton trajectory. The constraint exhibits all
 *   three lower-extraction types (rope, tangled_rope, scaffold) from
 *   institutional or organized perspectives, but snare from the trapped
 *   subject population, revealing the perspectival gap between those who
 *   experience the system as coordination and those locked into its
 *   extraction.
 *
 * KEY AGENTS:
 *   - Ruling Hierarchy: Institutional beneficiary (institutional/arbitrage) — captures legitimacy authority and resource extraction via divine mandate framing
 *   - Priestly Authority: Institutional beneficiary (institutional/arbitrage) — intermediates the divine mandate system, extracts rents from ritual and interpretation monopolies
 *   - Subject Populations: Primary victims (powerless/identity_locked) — identity constituted through compliance; frame-dependent exit (trapped at identity level, mobile at structural level)
 *   - Economic Compliance Bearers (Merchants/Producers): Secondary victims (moderate/constrained) — bear extraction costs (tithes, fees, monopolies) but benefit from temple coordination functions
 *   - Dissident Reform Coalitions: Organized agents (organized/constrained) — perceive system as degraded; propose alternative legitimacy frames with generational sunset
 *   - Historical Analyst: Civilizational observer (analytical/analytical) — risks naturalizing contingent institutional arrangement; detects piton degradation pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__enforcement_collapse_mechanism, 0.58).
domain_priors:suppression_score(divine_legitimacy_substrate__enforcement_collapse_mechanism, 0.65).
domain_priors:theater_ratio(divine_legitimacy_substrate__enforcement_collapse_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__enforcement_collapse_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__enforcement_collapse_mechanism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__enforcement_collapse_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__enforcement_collapse_mechanism, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__enforcement_collapse_mechanism, "Divine Legitimacy Substrate: Enforcement Collapse Mechanism").
narrative_ontology:topic_domain(divine_legitimacy_substrate__enforcement_collapse_mechanism, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__enforcement_collapse_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__enforcement_collapse_mechanism, '1d94daa6-46f5-464a-9506-b54ad8269e0d').
narrative_ontology:cs_kernel_codification('1d94daa6-46f5-464a-9506-b54ad8269e0d', formalized).
narrative_ontology:cs_authority_grounding('1d94daa6-46f5-464a-9506-b54ad8269e0d', extraction).
narrative_ontology:cs_interpretation_layer_present('1d94daa6-46f5-464a-9506-b54ad8269e0d').
narrative_ontology:cs_axiom('1d94daa6-46f5-464a-9506-b54ad8269e0d', foundational, divine_mandate_requires_priestly_intermediation).
narrative_ontology:cs_axiom_status(divine_mandate_requires_priestly_intermediation, holdable).
narrative_ontology:cs_axiom_grounding('1d94daa6-46f5-464a-9506-b54ad8269e0d', divine_mandate_requires_priestly_intermediation, conventional).
narrative_ontology:cs_axiom('1d94daa6-46f5-464a-9506-b54ad8269e0d', foundational, enforcement_mechanism_replacement_trajectory).
narrative_ontology:cs_axiom_status(enforcement_mechanism_replacement_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('1d94daa6-46f5-464a-9506-b54ad8269e0d', enforcement_mechanism_replacement_trajectory, empirically_contingent).
narrative_ontology:cs_reference_frame('1d94daa6-46f5-464a-9506-b54ad8269e0d', functional_legitimacy_necessity).
narrative_ontology:cs_drift_state('1d94daa6-46f5-464a-9506-b54ad8269e0d', post_bureaucratic_state_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d94daa6-46f5-464a-9506-b54ad8269e0d', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__enforcement_collapse_mechanism, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__enforcement_collapse_mechanism, ruling_hierarchy).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__enforcement_collapse_mechanism, priestly_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__enforcement_collapse_mechanism, subject_populations).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__enforcement_collapse_mechanism, dissident_sects).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__enforcement_collapse_mechanism, economic_compliance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — The believer's identity is constituted through adherence to the divine mandate system. Exit would require abandoning not merely compliance but the identity frame itself — becoming apostate means becoming a different person within the society's ontology. The system presents compliance as cosmically necessary, not structurally imposed. From this position, the constraint appears immutable (mountain to powerless/identity_locked), yet the engine derives rope classification at biographical time because the identity frame itself is theoretically shiftable. The suppression is maximal because the binding mechanism operates at the cognitive level, making the barrier to exit existential rather than merely material.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: ECONOMIC COMPLIANCE BEARER (TANGLED ROPE) — Merchants and producers face tangible extraction: temple tithes, ritual fees, mandatory offerings, religious monopolies on certain trades (e.g., temple grain distribution, religious craft guilds). These create genuine costs. But the system also provides coordination benefits — temple storage during famine, standardized weights/measures for commerce, predictable law. The constraint is neither pure extraction (snare) nor pure coordination (rope) — it genuinely coordinates resource allocation alongside asymmetric extraction. Exit is constrained by both material costs (loss of patronage networks, temple credit) and social costs (suspicion, reduced marriageability), but not trapped.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIESTLY AUTHORITY (ROPE) — From the institutional perspective, the divine legitimacy system solves a genuine collective action problem: coordinating subject compliance without standing armies or police infrastructure. The priesthood experiences the constraint as coordination — they are solving the problem of how to govern a dispersed population. Their arbitrage consists in the ability to reinterpret divine will, leverage temple resources, and move between religious and political authority. They perceive extraction as modest because they view the system's primary function as the coordination it achieves.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HISTORICAL ANALYST / PITON VIEW (CIVILIZATIONAL) — From the long view, divine legitimacy systems are degraded — maintained through institutional inertia and theater long after their primary functional purpose (coordination of dispersed populations) has been displaced by other mechanisms (state bureaucracy, secular law, market infrastructure). The system persists because rulers found it expedient to preserve the legitimacy apparatus even after direct enforcement became possible. Theater ratio (0.68) reflects that much priestly activity by the late-antique period was performative: maintaining the appearance of divine engagement in succession, harvest outcomes, and military victory despite secular rulers already controlling these outcomes.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized dissident groups (heterodox sects, philosophical movements, rival priesthoods) perceive the divine legitimacy substrate as a temporary structure with a built-in sunset. Their perspective: as education spreads, alternative legitimacy frames compete, and economic complexity outpaces temple coordination capacity, the system's functional necessity erodes. The coalition has constrained but real agency — they can propose alternative frames, build parallel institutions, and accelerate the transition. This perspective is scaffold because it sees the constraint as solvable through institutional redesign with a generational timeline.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — The analytical observer at maximum scope risks seeing divine legitimacy as a natural law: all premodern societies require transcendent legitimacy claims; human societies cannot organize without supernatural authority narratives; the constraint is immutable to historical action. This perspective classifies as mountain but triggers the false summit detector — the structural data reveals beneficiaries (ruling hierarchy, priestly authority) and asymmetric extraction (suppression ≥ 0.65, extractiveness ≥ 0.46), contradicting the mountain gates. The naturalization masks a contingent institutional arrangement.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__enforcement_collapse_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__enforcement_collapse_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__enforcement_collapse_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_legitimacy_substrate__enforcement_collapse_mechanism, TR),
    TR >= 0.70.

:- end_tests(divine_legitimacy_substrate__enforcement_collapse_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The constraint begins as mixed coordination (ε ≈ 0.32) when divine legitimacy genuinely solves the coordination problem of governing dispersed populations without bureaucratic capacity. As state capacity and alternative frames emerge, the extractive component rises (ε ≈ 0.58) while the coordination function becomes redundant — the same institutional apparatus persists, now extracting rents without providing commensurate coordination benefits. Suppression (0.65): High but stable and slightly declining. The system's binding mechanism is identity-lock (cognitive/existential) rather than merely material coercion. As education spreads and alternative frames become legible, the suppression requirement to maintain compliance falls slightly — more energy is devoted to theater (ritual maintenance, priestly performance) to compensate for weakening cognitive grip. Theater ratio (0.68): High and rising. As the system's functional coordination purpose declines, performative content increases: more elaborate rituals, more theological elaboration, more priestly ceremony to maintain the appearance of divine engagement in outcomes the system no longer actually coordinates. This is the diagnostic signature of a piton (degraded constraint maintained by theater).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival disagreement between structural positions. The priestly authority sees rope (coordination function is real; they are solving a genuine problem). The economic bearer sees tangled_rope (genuine coordination but also genuine extraction). The subject population locked into identity fusion sees snare or even mountain (immutable, cosmic necessity). The reform coalition sees scaffold (temporary, solvable by institutional redesign). The historical analyst sees piton (degraded, maintained by inertia). The maximum-scope analytical observer risks mountain (naturalizing the system as universal human necessity) until the structural data reveals false summit: beneficiaries exist, asymmetric extraction is documented, suppression is high. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural relationship to it and their freedom to recognize alternative frames.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's relationship to the extraction flow and their exit options. Priesthood with arbitrage exit (d ≈ 0.15): low d → negative f(d) → net beneficiary experience. Subject with identity_locked exit (d ≈ 0.89): high d → high f(d) ≈ 1.28 → maximum experienced extraction. Moderate economic bearer with constrained exit (d ≈ 0.65): mid-range d → f(d) ≈ 1.0 → moderate experienced extraction. The identity_locked status for subjects is critical: they are structurally mobile (could theoretically exit to other communities or adopt secular frames) but cognitively trapped (their identity frame makes exit unthinkable from within). This produces the rope classification at biographical time for the identity_locked agent — the engine recognizes that identity frames are theoretically shiftable — but functionally they experience snare-level extraction because the cognitive barrier is as effective as physical imprisonment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by documenting how the same institutional apparatus can function as genuine coordination (rope/tangled_rope) in one historical phase and degraded extraction (snare/piton) in another. The temporal measurements show the transition: extractiveness rising while suppression falls, with theater compensating. The constraint avoids the mandatrophy trap — claiming the system is both pure coordination and pure extraction — by anchoring classification in the agent's structural position AND the historical moment. Early divine legitimacy systems (state capacity low, alternatives nonexistent): snare → tangled_rope depending on agent power. Late divine legitimacy systems (state capacity high, alternatives available): piton or snare as the coordination function becomes redundant and extraction persists through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_collapse_trigger,
    'What observable threshold marks the transition from ''divine legitimacy as functional coordination'' to ''divine legitimacy as extractive inertia''?',
    'Historical correlation analysis: track extractiveness vs suppression as literacy spreads, alternative authority frames emerge, and state capacity for direct enforcement increases. Identify the inflection point where suppression remains constant while extractiveness rises (indicative of enforcement mechanism degradation).',
    'If threshold is crossed: the constraint moves from tangled_rope (mixed coordination + extraction) toward snare (extraction with minimal coordination function). The shift indicates the constraint''s primary function has been displaced; it persists through inertia and theater, not genuine need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_collapse_trigger, empirical, 'Observable threshold distinguishing functional legitimacy coordination from degraded extractive inertia').

omega_variable(
    alternative_legitimacy_framework_viability,
    'Can secular bureaucratic authority or philosophical legitimacy provide equivalent coordination function without the divine apparatus?',
    'Comparative institutional analysis: examine cases where divine legitimacy was displaced (Enlightenment Europe, post-colonial states, secular revolutions); measure coordination outcomes (stability, economic function, population compliance) pre- and post-displacement.',
    'If viability confirmed: divine legitimacy system reclassifies as pure extraction (snare, χ > 0.66) — the coordination function it claimed is achievable without it, meaning persistence is purely extractive. If viability denied: system reclassifies as necessary coordination (rope, χ ≤ 0.35) — the divine apparatus is structurally required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_legitimacy_framework_viability, empirical, 'Whether secular legitimacy frameworks can provide equivalent coordination function').

omega_variable(
    identity_lock_mechanism_irreversibility,
    'Is the cognitive binding (identity fusion with divine mandate system) reversible through education/exposure, or does it constitute a structural irreversibility?',
    'Psychological/anthropological study of apostasy, conversion, and belief shift across literacy gradients. Track whether exposure to alternative frames causes framework abandonment or deepened commitment to original frame (cognitive resistance patterns).',
    'If reversible: identity_locked classification is accurate; the binding is perceptual and shifts with frame exposure. If irreversible: the binding is structural (not merely cognitive); subjects with high identity fusion have materially trapped exit capacity regardless of frame exposure. Reclassify to trapped exit for high-fusion subpopulations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_irreversibility, empirical, 'Whether identity fusion with divine legitimacy system is cognitively reversible').

omega_variable(
    kernel_reading_distinguishability,
    'Is this reading (enforcement collapse via mechanism degradation) distinguishable from alternative readings of divine legitimacy substrate as a natural law, or does the same empirical evidence sustain both readings simultaneously?',
    'Logical and structural analysis: trace which axioms and reference frames this reading commits to, and which alternative readings would require contradictory axioms. Identify what observable evidence would falsify this reading while confirming an alternative (or vice versa).',
    'If distinguishable: this reading instantiates a genuine alternative to natural-law or eternal-necessity framings. If indistinguishable: the kernel is genuinely under-determined; the readings are incommensurable. This omega documents the indeterminacy built into the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinguishability, conceptual, 'Whether enforcement-collapse reading is logically distinguishable from sibling readings of divine legitimacy substrate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__enforcement_collapse_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dles_tr_t0, divine_legitimacy_substrate__enforcement_collapse_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dles_tr_t3, divine_legitimacy_substrate__enforcement_collapse_mechanism, theater_ratio, 3, 0.55).
narrative_ontology:measurement(dles_tr_t6, divine_legitimacy_substrate__enforcement_collapse_mechanism, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(dles_be_t0, divine_legitimacy_substrate__enforcement_collapse_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dles_be_t3, divine_legitimacy_substrate__enforcement_collapse_mechanism, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(dles_be_t6, divine_legitimacy_substrate__enforcement_collapse_mechanism, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dles_su_t0, divine_legitimacy_substrate__enforcement_collapse_mechanism, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(dles_su_t3, divine_legitimacy_substrate__enforcement_collapse_mechanism, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(dles_su_t6, divine_legitimacy_substrate__enforcement_collapse_mechanism, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__enforcement_collapse_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__enforcement_collapse_mechanism, legitimacy_claim_system).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__enforcement_collapse_mechanism, priestly_monopoly_rent_extraction).

% DUAL FORMULATION NOTE:
% Divine legitimacy substrate decomposes into three constraint families: (1) legitimacy_claim_system (the epistemic claim that divine will is knowable and guides outcomes); (2) priestly_monopoly_rent_extraction (the economic extraction via ritual monopolies); (3) enforcement_collapse_mechanism (this story — the structural degradation as alternatives emerge). Each has distinct ε values and measurements. This story focuses on the enforcement-mechanism degradation; siblings handle epistemic claims and economic rents separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__enforcement_collapse_mechanism, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
