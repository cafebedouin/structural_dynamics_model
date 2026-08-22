% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation: Imperial Charisma + Institutional Incentives
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This reading instantiates the HYBRID LEGITIMATION mechanism for norm
 *   imposition: the center transfers symbolic authority to a new norm through
 *   imperial exemplification (the emperor performs the practice publicly,
 *   grants honors to early adopters, embeds the norm in court ritual)
 *   combined with institutional incentives (administrators reward conformity,
 *   penalize non-conformity disguised as administrative burden). The
 *   mechanism produces stratified adoption: elites adopt first (drawn by
 *   honors and administrative access), peripheral populations follow (through
 *   social learning and diffuse pressure), legitimacy appears to flow from
 *   charisma and cultural fitness rather than coercion. This reading
 *   explicitly rejects the ENDOGENOUS_CLIMB reading (which claims norms
 *   achieve legitimacy through grassroots adoption that precedes state
 *   mandate) and the EXOGENOUS_OVERRIDE reading (which claims norms are
 *   imposed purely through coercion and monopoly on violence). The hybrid
 *   reading is distinctive: it asserts that neither top-down coercion nor
 *   bottom-up demand is the primary driver, but rather a staged mechanism of
 *   elite signaling + institutional embedding that produces legitimacy
 *   effects from the combination of symbolic transfer + selective incentive.
 *
 * KEY AGENTS:
 *   - Imperial authority: the center (emperor, dynasty, state apparatus) that initiates the norm and benefits from its legitimation effect.
 *   - Traditionalist elites: powerful regional/local authorities who are both targeted first and offered selective incentives; their conformity legitimizes the norm downward.
 *   - Peripheral populations: moderately powerful actors whose identity is fused to old practices; they adopt after seeing elite adoption establish prestige.
 *   - Imperial bureaucracy: institutional machinery that embeds the norm through administrative practice (registries, credentials, incentives); they benefit from expanded jurisdictional reach.
 *   - Old-norm defenders: excluded actors who would contest the norm but lack access to the mechanism's design or resources.
 *   - Sociological observer: analytical seat mapping the stratified adoption pattern and measuring the gap between the mechanism's narrative (cultural fitness) and its structure (selective incentives).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.42).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation: Imperial Charisma + Institutional Incentives").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'ab2c615e-ba84-425e-aaf6-7b115b6b18f6').
narrative_ontology:cs_kernel_codification('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', formalized).
narrative_ontology:cs_authority_grounding('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', lineage).
narrative_ontology:cs_interpretation_layer_present('ab2c615e-ba84-425e-aaf6-7b115b6b18f6').
narrative_ontology:cs_reading_relation('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', foundational, legitimacy_transfer_via_elite_signaling).
narrative_ontology:cs_axiom_status(legitimacy_transfer_via_elite_signaling, holdable).
narrative_ontology:cs_axiom_grounding('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', legitimacy_transfer_via_elite_signaling, empirically_contingent).
narrative_ontology:cs_axiom('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', secondary, stratified_adoption_through_social_learning).
narrative_ontology:cs_axiom_status(stratified_adoption_through_social_learning, holdable).
narrative_ontology:cs_axiom_grounding('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', stratified_adoption_through_social_learning, empirically_contingent).
narrative_ontology:cs_reference_frame('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', center_authorized_elite_performance_framework).
narrative_ontology:cs_drift_state('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', post_initial_elite_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab2c615e-ba84-425e-aaf6-7b115b6b18f6', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_authority).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peripheral_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor or central state apparatus initiates a new norm by public exemplification (performing the new practice, granting honors to early adopters, embedding the norm in official ceremony). They benefit from the norm's legitimation of centralized authority and from reduced friction in implementing state policy. They maintain the norm through selective incentives (rewards for conformity) and institutional embedding (penalties for non-conformity disguised as administrative necessity rather than coercion). Their exit is to abandon the norm, which would erode the symbolic capital they invested in establishing it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Initially bear the cost of conforming to the new norm, sacrificing prestige derived from the old order. They are targeted first because their visibility and status influence others. They receive secondary benefit: early conformity earns honors, administrative positions, or access to imperial resources; non-conformity brings administrative harassment. Their constraint exit is to resist openly, which damages their social position and invites administrative penalty. The hybrid mechanism ensures they are both targets and partial beneficiaries, creating a coalition with the center against holdouts.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_elites, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_elites, beneficiary).

% Adopt the new norm gradually, after seeing elite adoption establish its legitimacy. They bear costs of departing from local tradition and identity-fusion with old norms. Unlike elites, they receive minimal direct incentives; adoption happens through social conformity (copying elevated status groups) and diffuse pressure (the norm becomes the assumed baseline). Their identity is fused to customary practice; exit from the old norm requires reconstructing self-understanding. The mechanism works because empire-derived prestige is transferred downward through elite performance; legitimacy appears to flow from charisma rather than command.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peripheral_populations, payer,
    moderate, biographical, identity_locked, local).

% Implements the norm through administrative machinery (registries, inspections, credentials tied to conformity). They benefit from the norm's expansion because it increases their jurisdictional reach and resources. They are trapped because their institutional identity is constituted through implementing state policy; departure would be career-ending. They enforce through selective incentives rather than raw force, which reduces enforcement costs and makes the norm appear legitimacy-based rather than coercion-based.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_bureaucracy, beneficiary,
    institutional, biographical, trapped, national).

% Would argue that the new norm destroys cultural coherence and violates sacred tradition. They are excluded from the mechanism's design phase and from the incentive distribution — their resistance is treated as cultural lag rather than legitimate disagreement. They lack access to the center's attention and resources to mount organized opposition. Their exclusion is structural: the mechanism works by translating center-driven change into elite performance, which silences alternatives.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, old_norm_defenders, excluded,
    moderate, biographical, trapped, local).

% Observes the mechanism's operation across historical cases and maps the stratified adoption pattern: elite early adoption driven by incentives, mass adoption driven by status transfer, maintained by diffuse institutional pressure. Measures the distinction between the mechanism's narrative (charisma, cultural fitness) and its structural operation (selective incentives, administrative embedding, stratified adoption).
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, sociological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_authority).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the state's collective-action problem: how to shift population behavior from old practices to new ones without universal deployment of force, which would require sustained occupation and would delegitimize the center's claim to cultural authority. The hybrid mechanism achieves coordination by leveraging elite status as a signal that the new norm is consistent with (or even elevates) their identity, and diffusing it downward through social learning rather than direct coercion.
% TRANSFER_FUNCTION: Moves legitimacy upward (from dispersed local authorities and traditional practices to the center) and resources downward (the center grants administrative positions, honors, and property access to early conformers). The center extracts authority; peripheral populations contribute conformity and identity-reconstruction effort. Traditional elites transfer prestige from old hierarchies to the new center-defined order.
% ABSENT_VOICES: Old-norm defenders and cultural specialists (priests, tradition-bearers, local authorities anchored in the old order) are structurally excluded. They are reframed as backward, conservative, or superstitious rather than as holders of legitimate competing claims. No seat represents their objection or engages their expertise in designing the transition. Alternative norms that might compete for legitimacy are never articulated because the mechanism ensures only the center-endorsed norm receives institutional embedding and elite performance.
% DISAPPEARANCE_RATIONALE: If the imperial mechanism vanished (center ceased exemplification, bureaucracy ceased incentivizing, elite adoption reversed), the new norm would collapse among peripheral populations within a generation because legitimacy was artificially transferred rather than grown from repeated successful practice. Elite reversion would cascade downward; populations would reconstruct the old norm or invent a new competing one anchored in local identity. The center would lose the authority gains it had accumulated through the norm's successful embedding.
% FOUNDING_PROBLEM: The center faced a problem of coordination at scale without universal occupation: how to shift dispersed populations away from practices the center deemed dysfunctional (decentralized justice, local currencies, competing authority structures, practices that weakened state capacity) while preserving the appearance of legitimacy, cultural coherence, and voluntary adoption rather than coerced compliance.
% FOUNDING_PROBLEM_CORROBORATION: Imperial historians and center-aligned sources attest the founding problem is eternal — each generation must re-legitimize the state. Anthropologists and historians outside the imperial tradition attest the problem was specific and contingent: centers faced it when attempting to consolidate territory, and the hybrid mechanism was one of several possible strategies (direct coercion, wholesale replacement, gradual tolerance were alternatives). Some attest the founding problem is mis-stated: the real problem was not coordination but authority accumulation, masked as coordination.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 (moderate-to-high): the center extracts legitimacy and authority through the norm's embedding; peripheral populations bear the cost of identity reconstruction and departure from culturally-constituted practice. Extractiveness rises (0.35 → 0.63 over the interval) as the norm becomes institutionalized and previously-optional conformity becomes administrative baseline. The rise then plateaus (0.58 at endpoint) because after normalization, the mechanism's extractive cost becomes diffuse and invisible — it is no longer recognized as extraction but as the natural order. Theater_ratio is high initially (0.72) because the mechanism is entirely performative at the start (the emperor performs, the elite performance is the norm-setting act). Theater_ratio falls (→ 0.45) as the norm becomes administratively embedded and the performance becomes institutional routine; it then rises slightly (→ 0.48) as the norm requires periodic re-performance to maintain legitimacy rather than being sustained by functional necessity. Suppression falls (0.58 → 0.38) because the mechanism works by reducing the need for active suppression; once elite adoption is achieved and peripheral adoption cascades through social learning, enforcement becomes nearly automatic (diffuse institutional pressure rather than visible coercion). The strategy of the hybrid mechanism is precisely to minimize active suppression while maintaining extraction — it achieves this by outsourcing enforcement to elite status transfer and identity reconstruction.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial authority seat, the constraint is legitimate coordination: the state solves a real collective-action problem (how to shift populations at scale without universal occupation) and the norm produces genuine coordination benefits (unified practices that increase state capacity). From the elite seat, the constraint is mixed: they gain honors and administrative access (partial beneficiary) but also bear the cost of abandoning their own traditional authority and performing conformity to the center (payer). From the peripheral seat, the constraint is primarily extractive: populations bear the identity reconstruction cost and receive no compensating incentives; legitimacy is something that happens to them, not something they create. From the old-norm defender seat (excluded), the constraint is coercive and destructive: the mechanism's strategy is precisely to silence their voice by framing old practices as backward rather than legitimate alternatives. The engine computes these divergent types from the structural data: the imperial authority and compliant elite seats may compute as rope (genuine coordination benefit), while the peripheral and excluded seats compute as snare (extraction with coercive infrastructure). This divergence is structurally real, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial authority: d ≈ 0.1 (strong beneficiary; extraction accrues upward through legitimacy and authority gains, very low exit cost — they can abandon the mechanism if it fails without losing their power base). Traditionalist elites: d ≈ 0.55 (near-symmetric; they gain honors and access but lose traditional authority prestige and bear conformity costs; constrained exit — abandoning conformity risks administrative penalty). Peripheral populations: d ≈ 0.8 (near-target; they bear identity reconstruction costs and diffuse pressure, receive no direct incentives, face high identity-lock exit costs). Bureaucracy: d ≈ 0.2 (beneficiary; they expand jurisdictional reach and institutional resources; trapped exit). Old-norm defenders: d ≈ 0.9 (full target; the mechanism's strategy is to exclude them and render their perspective illegitimate). The directionality derivation follows the beneficiary/victim declarations: imperial authority is the beneficiary (collects legitimacy and authority), traditionalist elites are listed as both payer and beneficiary (they pay conformity costs but receive selective incentives, so they sit asymmetrically between full payers and full beneficiaries), peripheral populations are victims (they pay identity reconstruction costs), bureaucracy is not explicitly listed but functions as beneficiary (gains institutional reach). No overrides are needed; the canonical derivation captures the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy in the early interval (0-25) because the mechanism is actively performing its function: the center is actively exemplifying, elite adoption is visibly driving peripheral adoption, the norm's embedding is proceeding. The slight uptick in theater_ratio at the endpoint (48%) and plateau in extractiveness (58%) suggests potential mandatrophy onset in the later interval: the norm has become so embedded that the mechanism no longer needs active performance to sustain it; conformity becomes reflexive, and the constraint risks becoming piton-like (maintained by inertia and administrative routine rather than active legitimation). The R5 genealogy interaction is important: the founding problem (how to shift populations at scale without occupation) is live in the early interval but dies out (peripheral populations are successfully shifted, the old norm is abandoned) by the endpoint. The constraint should therefore compute as having resolved its founding mandate; if it persists beyond the endpoint (which the measurement plateau suggests it does), that persistence becomes mandate-orphaned and the constraint moves toward piton status. This reading does not declare the constraint as piton at the endpoint; rather, it documents the trajectory that would lead to piton if extension continues unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charisma_versus_coercion_boundary,
    'Is the perceived legitimacy of the norm genuinely derived from imperial charisma and cultural prestige, or is it the subjective experience of successful coercion masked as cultural transfer?',
    'Ethnographic or archive-based evidence of whether populations internalize the norm as culturally coherent (repair/reinterpretation to fit old identity frameworks) or as imposed (resistance, resentment, performative compliance without internalization). Post-exit data: if populations revert to old norms after imperial authority weakens, charisma was the primary driver; if they maintain the new norm despite authority withdrawal, legitimacy had been genuinely reconstructed.',
    'If charisma is genuine (primary), the constraint''s classification as tangled_rope with moderate extractiveness holds. If coercion is primary (masked by charisma narrative), the constraint should reclassify toward snare (pure extraction with defensive theater). The theater_ratio measurement (which begins high and falls) is diagnostic: pure charisma-driven adoption should show theater_ratio rising (legitimacy increasingly intrinsic, less need for performance); the measured fall suggests theater dominates early (when the mechanism relies on performance) and becomes embedded in routine (low theater because conformity is now expected). This omega disambiguates whether the fall in theater signals legitimacy internalization or bureaucratic routinization of coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charisma_versus_coercion_boundary, empirical, 'Whether legitimacy derives from genuine charisma + cultural prestige or from coercion masked as cultural transfer.').

omega_variable(
    elite_coercion_versus_elite_voluntarism,
    'Do traditionalist elites adopt the new norm voluntarily (drawn by honors and administrative access) or under coercive duress (threatened with administrative penalty if they resist)?',
    'Historical evidence of the initial elite adoption phase: did early conformers actively seek honors and access (voluntarism signal), or did non-conformers face explicit penalties and did conformers avoid them (coercion signal)? Archive evidence of imperial announcements (framing adoption as voluntary vs. mandatory)? Comparative case evidence: did elites in centers without selective incentive capacity achieve similar adoption rates (suggesting coercion is primary)?',
    'Pure voluntarism would support the hybrid reading as genuine tangled_rope with asymmetric but consensual coordination function. Pure coercion would move the classification toward snare (coercion defended by legitimacy theater). The measurement series suggests a blend: extraction rises as the mechanism embeds (suggesting accumulation of coercive infrastructure), theater falls (suggesting coercion becomes routine and invisible), and suppression falls (suggesting coercion is replaced by diffuse institutional pressure). This pattern is compatible with initial voluntarism (elites voluntarily seek honors) giving way to coercion (non-conformity becomes administratively costly) over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_coercion_versus_elite_voluntarism, empirical, 'Whether elite adoption is driven by voluntary incentive-seeking or coercive threat-avoidance.').

omega_variable(
    reading_boundary_against_exogenous_override,
    'This reading claims the mechanism is neither pure climb nor pure override, but what empirical feature cleanly distinguishes hybrid legitimation from a sophisticated override that simply uses elite performance as a coercion tool?',
    'Measure the autonomy of elite decision-making: if elites are free to reject honors and maintain old practices (with only diffuse administrative costs), the mechanism is hybrid; if rejection triggers severe penalty, the mechanism is override dressed as legitimation. Measure the reversibility of peripheral adoption: if peripheral populations maintain the new norm after elite commitment weakens, legitimacy was reconstructed; if they revert, the mechanism was pure coercion machinery with elite performance as the visible component.',
    'This omega documents the core conceptual ambiguity in the reading: the distinction between genuine hybrid legitimation and pure coercion defended by charisma theater is empirically subtle and may not be fully resolvable without access to elite subjective states and counterfactual defection scenarios. If the distinction proves empirically empty (coercion and hybrid legitimation produce identical structural signatures), then the readings may compress into two: coercion (exogenous_override with various defensive narratives) and consensus (endogenous_climb). This omega is the boundary-case omega for the reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_against_exogenous_override, conceptual, 'Whether hybrid legitimation is structurally distinct from exogenous override with sophisticated theater.').

omega_variable(
    stratified_adoption_necessity,
    'Is stratified adoption (elites first, periphery later) a necessary feature of the hybrid mechanism, or is it contingent on elite visibility and prestige-transfer dynamics?',
    'Comparative historical evidence: did mechanisms that lack elite-first stages (e.g., charismatic movements that spread from periphery upward, religious conversions that begin in lower strata) achieve similar legitimacy outcomes? Did mechanisms that tried non-stratified adoption (simultaneous elite and peripheral imposition) succeed or fail? Can the mechanism operate with simultaneous adoption if selective incentives are distributed equally?',
    'If stratification is necessary, the reading''s claim about the mechanism''s distinctive structure is strengthened (stratified adoption is not incidental but central to how the mechanism achieves legitimacy). If stratification is contingent, the mechanism becomes more general (the core feature is selective incentives + institutional embedding; stratification is a particular instantiation). This affects whether the reading can accommodate variants and how precisely it differentiates from siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratified_adoption_necessity, empirical, 'Whether stratified adoption is a necessary feature of the hybrid mechanism or contingent on empire-specific conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 8, 0.68).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(impo_tr_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 35, 0.45).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(impo_be_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 35, 0.63).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(impo_su_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.12).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the IMPOSITION_MECHANISM_KERNEL — a contested claim about how new norms achieve legitimacy and spread through dispersed populations. The three readings are structurally distinct constraints with different ε values and different stakeholder structures: (1) ENDOGENOUS_CLIMB_READING (ε ≈ 0.25-0.40, rope-like): norms legitimate through grassroots adoption that enables and constrains state mandate; (2) EXOGENOUS_OVERRIDE_READING (ε ≈ 0.75-0.90, snare): norms are imposed by coercion; legitimacy is narrative defense; (3) HYBRID_LEGITIMATION_READING (this one, ε ≈ 0.58, tangled_rope): norms legitimate through elite charisma transfer + institutional incentives, producing stratified adoption. The readings coexist as live interpretive positions held by different schools of historical sociology (endogenous climb: social history traditions emphasizing grassroots agency; exogenous override: realist/coercionist traditions emphasizing state power; hybrid legitimation: historical sociology traditions studying state-formation as a dual process of coercion and legitimacy negotiation). They do not foreclose each other because each is empirically anchored in different historical cases and different normative commitments. The network structure enables corpus consumers to track how the same contested kernel decomposes into three distinct constraints and how they influence each other (hybrid mechanism introduces a third option that reframes how cases are sorted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
