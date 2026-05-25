% ============================================================================
% CONSTRAINT STORY: coordination_lock_in
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_lock_in, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coordination_lock_in
 *   human_readable: Coordination Lock-In: QWERTY Keyboard Layout Persistence
 *   domain: technology_history/path_dependence/institutional_economics
 *
 * SUMMARY:
 *   The QWERTY keyboard layout has persisted as the dominant global standard
 *   for over 150 years, despite documented evidence that alternative layouts
 *   (Dvorak, Colemak, Bépo) reduce finger motion and could improve typing
 *   speed and reduce repetitive strain injury. This constraint exemplifies
 *   the tension between coordination lock-in and extractive maintenance. The
 *   story pivots on a foundational empirical question flagged in the
 *   UKE_SCOPE manifest: are manufacturers and industry bodies active
 *   beneficiaries maintaining QWERTY dominance against the efficiency
 *   frontier, or does QWERTY persist through neutral network effects and
 *   coordination drift with no identifiable extractors? This distinction
 *   determines whether the constraint is a Tangled Rope (genuine coordination
 *   plus extraction) or a Rope (pure coordination with lock-in as a natural
 *   byproduct). The constraint generates a false-summit risk: the mountain
 *   perspective (PERSPECTIVE 6) naturalizes the lock-in as an inescapable law
 *   of coordination physics, but empirical evidence of active enforcement
 *   would reveal it as a contingent institutional arrangement benefiting
 *   specific actors.
 *
 * KEY AGENTS:
 *   - Incumbent Keyboard Manufacturers: Potentially beneficiary (institutional/arbitrage) — standardized layout eliminated SKU proliferation and enabled global supply chains. Active maintenance status contested per omega-1.
 *   - Alternative Layout Adopters: Primary victim (powerless/trapped) — bear full switching costs and network isolation. Cannot feasibly adopt Dvorak without retraining and losing muscle memory value.
 *   - Typing Efficiency Community: Secondary victim (moderate/constrained) — constrained by lock-in but also benefit from standardized training and global interoperability.
 *   - Typewriter Industry Associations: Potentially institutional beneficiary (institutional/arbitrage) — cross-licensing and standards enforcement bodies. Function has degraded in digital era (piton status).
 *   - Niche Technical Communities: Powerful actors with exit (powerful/mobile) — can fund custom hardware and programmable firmware. See constraint as temporary with sunset.
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing contingent institutional arrangements as physical laws of coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_lock_in, 0.38).
domain_priors:suppression_score(coordination_lock_in, 0.42).
domain_priors:theater_ratio(coordination_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_lock_in, extractiveness, 0.38).
narrative_ontology:constraint_metric(coordination_lock_in, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(coordination_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_lock_in, tangled_rope).
narrative_ontology:human_readable(coordination_lock_in, "Coordination Lock-In: QWERTY Keyboard Layout Persistence").
narrative_ontology:topic_domain(coordination_lock_in, "technology_history/path_dependence/institutional_economics").

domain_priors:requires_active_enforcement(coordination_lock_in).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(coordination_lock_in, distributed).
narrative_ontology:cs_authority_grounding(coordination_lock_in, practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_lock_in, incumbent_manufacturers).
narrative_ontology:constraint_beneficiary(coordination_lock_in, typewriter_industry).
narrative_ontology:constraint_victim(coordination_lock_in, alternative_layout_adopters).
narrative_ontology:constraint_victim(coordination_lock_in, typing_efficiency_frontier).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE LAYOUT ADOPTER (SNARE) — Trapped by network effects and software ecosystem lock-in. Switching to Dvorak or Colemak imposes massive costs: retraining time, incompatibility with shared computers, loss of muscle memory value, inability to use public terminals. No exit without bearing full cost. Maximum experienced extraction.
constraint_indexing:constraint_classification(coordination_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TYPING EFFICIENCY COMMUNITY (TANGLED ROPE) — Constrained by the network lock-in but also benefits from standardized training materials, widespread keyboard hardware, and software keyboard driver support. Some coordination function exists (QWERTY enables interoperability), but the constraint also extracts by preventing adoption of layouts that would reduce repetitive strain injury. Mixed experience: genuine coordination benefit shadowed by extractive prevention of improvement.
constraint_indexing:constraint_classification(coordination_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT MANUFACTURERS (ROPE) — Benefits from standardized layout, elimination of SKU proliferation, and network effects that entrench their market position. Experiences the constraint as pure coordination: QWERTY standardization solves the multi-variant problem and ensures their products work globally. Arbitrage position allows them to capture switching costs if they choose to fund new layouts — they don't, revealing the extraction beneath the coordination framing.
constraint_indexing:constraint_classification(coordination_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY TYPEWRITER ASSOCIATIONS (PITON) — Maintains QWERTY standardization through industry bodies and cross-licensing, but the function has degraded. By the digital era, QWERTY enforcement was purely performative — the actual coordination mechanism shifted to software and cultural convention, but industry associations continued enforcing the standard through inertia. Institutional inertia maintains a constraint whose primary function no longer operates.
constraint_indexing:constraint_classification(coordination_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NICHE TECHNICAL COMMUNITIES (SCAFFOLD) — Powerful enough (programmers, enthusiasts) to fund custom hardware and maintain alternative layout software drivers. See the constraint as a temporary coordination problem with a sunset: as smart keyboards and programmable firmware mature, alternative layouts become implementable without retraining populations. Some suppression remains (global software compatibility), but exit is visible and achievable. Theater low because commitment to alternatives is authentic.
constraint_indexing:constraint_classification(coordination_lock_in, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational view, QWERTY persistence could be framed as an inevitable consequence of coordination physics: once millions of people invest in QWERTY muscle memory, no amount of efficiency gain justifies defection, regardless of layout merits. Lock-in is thus a natural law of coordination — the imbalance between switching costs and marginal efficiency gains is inescapable. However, this perspective naturalizes what empirical history reveals as contingent: active enforcement by manufacturers and industry bodies maintained QWERTY dominance when neutral drift alone would not.
constraint_indexing:constraint_classification(coordination_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(coordination_lock_in, TR),
    TR >= 0.70.

:- end_tests(coordination_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The measurement trajectory shows extractiveness rising from 0.12 in the mechanical era (where QWERTY had genuine coordination benefits) to 0.38 in mature digital era (where unit production costs are negligible but ecosystem lock-in persists). The rise suggests extraction increasing as the original coordination justification decayed. However, extractiveness remains moderate rather than high because: (1) the beneficiary identification is contested — if manufacturers did not actively enforce QWERTY, the constraint is coordination-only and extractiveness should be lower; (2) alternative layouts remain technically accessible to motivated users, even if costly. Suppression (0.42): Moderate-high. Barriers to alternative layout adoption include: retraining cost (significant but surmountable), incompatibility with public/shared computers (real but shrinking as personal devices proliferate), software ecosystem bias (real; most OSes ship QWERTY-first), and social friction (high but declining among technical communities). Suppression is not total — niche communities successfully maintain alternatives. Theater ratio (0.55): Moderate-high. Industry standardization bodies continue to enforce QWERTY through formal standards and cross-licensing, but the performative content is rising: by the digital era, standardization serves more to prevent fragmentation (a coordination function) than to solve a genuine coordination problem (different layout hardware is trivially implementable). The rise from 0.25 to 0.55 tracks the shift from mechanical era (where enforcing a single layout had real coordination benefits) to digital era (where enforcement is increasingly theatrical).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Incumbent manufacturers see Rope (pure coordination solving the SKU problem). Alternative adopters see Snare (trapped by network effects with no exit). The typing community sees Tangled Rope (mixed coordination and constraint). Technical communities see Scaffold (temporary lock-in solvable by firmware programmability). Industry associations see Piton (enforcement of a standard whose coordination function has degraded). The civilizational analyst risks seeing Mountain (inherent law of coordination physics). The perspectival gap exposes the methodological risk: if the researcher prioritizes the beneficiary perspective, they find a Rope story of necessary standardization. If they prioritize the victim perspective, they find a Snare story of predatory lock-in. Both are structurally defensible readings of the same empirical data. The engine resolves this not by choosing sides but by computing all perspectives simultaneously and flagging the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are contested pending resolution of omega-1 (active vs passive maintenance). Under the beneficiary reading: manufacturers derive d ≈ 0.20 (beneficiary with arbitrage options), alternative adopters derive d ≈ 0.92 (victims trapped by network effects), typing community derives d ≈ 0.60 (mixed: benefits from standardization, constrained by lock-in). Under the no-beneficiary reading (if active enforcement cannot be documented): manufacturers should be removed from beneficiaries, the constraint reclassifies toward Rope, and d values for remaining agents drop (no identifiable extractor, so directionality becomes symmetric). The beneficiary/victim declarations are explicitly provisional pending the empirical resolution flagged in omega-1. This is a D3 ARTIFACT test case: the methodology of 'finding beneficiaries' can construct them retroactively by asking 'who benefits from the status quo?' Applied uncritically, this methodology finds beneficiaries everywhere. Genuine beneficiary status requires evidence of active maintenance against alternatives, not merely post-hoc correlation with market dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED CONSTRAINT: This story instantiates the D3 ARTIFACT diagnostic — the boundary between genuine coordination and retrofitted extraction depends entirely on whether active beneficiary maintenance can be documented. The mandatrophy is not 'which type is correct' but 'what counts as evidence of beneficiary agency?' If manufacturers can be shown to have: (a) deliberately suppressed alternative layout hardware (patent litigation, cross-licensing restrictions, refusal to produce alternatives), (b) invested capital in QWERTY standardization when neutral drift would have sufficed, (c) explicitly protected QWERTY dominance as a strategic asset — then beneficiary status is genuine, the constraint is Tangled Rope, and extractiveness is justified. If these evidences are absent and QWERTY persists through coordination physics alone, then beneficiary identification is analyst-constructed, beneficiaries should be removed, the constraint reclassifies to Rope, and the 'lock-in' is a natural coordination outcome with no extraction. The D3 ARTIFACT methodology tests whether analyst-level beneficiary inference maps to structural beneficiary agency. This story hangs in suspended judgment pending omega-1 resolution. The high confidence level on omega-1 reflects that historical archives can resolve the question definitively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_maintenance,
    'Did incumbent manufacturers and typewriter industry associations actively enforce QWERTY dominance, or did QWERTY persist through neutral network effects and coordination drift?',
    'Historical archive analysis: patent litigation history, cross-licensing agreements, industry association meeting minutes, keyboard manufacturer SKU data showing suppression of alternative layout hardware. Comparison of alternative layout adoption rates in contexts where manufacturers supported them (early computing, specialized communities) vs where they didn''t.',
    'If active enforcement: constraint is Tangled Rope (coordination + extraction). If passive drift: constraint is Rope (pure coordination). Beneficiary declaration hinges on this distinction. If active enforcement cannot be documented, beneficiaries should be removed, reclassifying toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_vs_passive_maintenance, empirical, 'Whether QWERTY dominance was actively maintained or passively drifted').

omega_variable(
    manufacturing_cost_differential,
    'What were the actual manufacturing cost differentials between QWERTY and alternative layout production? Were these costs material to the lock-in, or were they negligible?',
    'Historical engineering data: tooling costs, unit production costs for QWERTY vs Dvorak vs Colemak keyboards across different eras (mechanical, electric, digital). Economic analysis of whether cost savings from standardization exceeded R&D costs for layout variety.',
    'If costs were material: beneficiary claim is strong (manufacturers saved significant costs from standardization). If costs were negligible: coordination function was dominant, beneficiary claim is weak. Extractiveness value should be adjusted downward if costs reveal neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_cost_differential, empirical, 'Manufacturing cost differences between QWERTY and alternatives').

omega_variable(
    counterfactual_adoption_timeline,
    'If manufacturers had supported Dvorak or Colemak layouts with the same capital investment they gave QWERTY, what would adoption timelines have looked like?',
    'Comparative analysis of adoption curves for technologies where manufacturers DID support alternatives (e.g., multiple smartphone operating systems, gaming console architectures). Historical simulation using empirical network-effect models calibrated to keyboard data.',
    'If alternatives would have achieved significant adoption: the constraint''s extractiveness is authentic (manufacturers'' choice to not support them was active extraction). If alternatives would have remained niche regardless: QWERTY''s persistence is primarily due to coordination physics, not extraction, and constraint should reclassify toward Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_adoption_timeline, empirical, 'Counterfactual adoption rates under alternative manufacturing support scenarios').

omega_variable(
    beneficiary_identification_artifact,
    'Is the ''incumbent manufacturers'' beneficiary category a genuine structural relationship, or an artifact of retrospective beneficiary-hunting methodology that finds beneficiaries by construction?',
    'Temporal analysis: did manufacturers explicitly declare QWERTY as a value to protect, or is the beneficiary status inferred post-hoc by correlating their market dominance with QWERTY persistence? Distinguish between (a) manufacturers stating QWERTY as a strategic asset vs (b) analysts observing that manufacturers happened to benefit. Cross-reference with manufacturer archives and patent filings for explicit QWERTY protection language.',
    'If explicit protection: beneficiary declaration is justified (D3 ARTIFACT test case fails — genuine beneficiaries exist). If post-hoc inference: beneficiary status is analyst-constructed, not structurally grounded. This is the D3 ARTIFACT diagnostic — the methodology distinguishes genuine coordination constraints from retrofitted narratives of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_artifact, conceptual, 'Whether beneficiary identification is structural or methodological artifact').

omega_variable(
    timing_of_lock_in_formation,
    'When did the coordination lock-in actually form? At the typewriter era (1870s-1920s), at the electromechanical transition (1950s-60s), or at the digital transition (1980s onward)?',
    'Historical evidence: when did QWERTY become the dominant layout globally? When did switching costs become prohibitive? When did alternative layout development cease? Separate the eras: mechanical era (high switching cost per unit), electric era (medium cost), digital era (low unit cost but high ecosystem cost). Which era''s lock-in persists to present?',
    'If lock-in formed in mechanical era: constraint may be naturally emergent (mountain-like). If lock-in was actively enforced during digital transition despite lowered unit costs: extraction becomes visible (snare or tangled rope). Measurement trajectory should show when lock-in deepened vs when it plateaued.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timing_of_lock_in_formation, empirical, 'Historical era when coordination lock-in crystallized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_mechanical_era, coordination_lock_in, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_electric_era, coordination_lock_in, theater_ratio, 3, 0.35).
narrative_ontology:measurement(theater_early_digital, coordination_lock_in, theater_ratio, 6, 0.5).
narrative_ontology:measurement(theater_mature_digital, coordination_lock_in, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(extractiveness_mechanical_era, coordination_lock_in, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(extractiveness_electric_era, coordination_lock_in, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(extractiveness_early_digital, coordination_lock_in, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(extractiveness_mature_digital, coordination_lock_in, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_lock_in, information_standard).
narrative_ontology:affects_constraint(coordination_lock_in, network_effects_lock_in).
narrative_ontology:affects_constraint(coordination_lock_in, switching_cost_asymmetry).
narrative_ontology:affects_constraint(coordination_lock_in, path_dependence_technology_standards).

% DUAL FORMULATION NOTE:
% QWERTY persistence can be decomposed into three structurally distinct constraints: (1) network_effects_lock_in (ε≈0.15, Rope) — pure coordination where the standard matters more than which standard. (2) switching_cost_asymmetry (ε≈0.50, Snare) — asymmetric costs imposed on would-be adopters of alternatives. (3) path_dependence_technology_standards (ε≈0.38, contested Rope vs Tangled Rope) — this story, capturing whether active enforcement exists. The three are linked: network effects create switching costs; switching costs enable extraction if actively maintained. Separating them allows precise measurement of whether QWERTY lock-in is a coordination phenomenon or an extraction phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
