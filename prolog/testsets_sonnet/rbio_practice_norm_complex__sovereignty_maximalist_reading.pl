% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty-Maximalist Reading of the RBIO Norm Complex
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-maximalist reading of the
 *   contested RBIO (Rules-Based International Order) norm complex: state
 *   sovereignty is treated as effectively absolute, RBIO norms are legitimate
 *   only insofar as they defend states against external interference, and
 *   humanitarian intervention claims are read as pretexts for regime change.
 *   This is one of three structurally distinct constraints emitted from a
 *   single contested kernel — the other two (liberal_institutional_reading,
 *   hegemonic_extraction_reading) are separate files with their own epsilon
 *   values and stakeholder structures, not alternative measurements of this
 *   one. The maximalist reading genuinely coordinates a real historical
 *   grievance (colonial and Cold War interventionism) but, as authored here,
 *   its metrics reflect substantial and rising capture by incumbent regimes
 *   and their P5 patrons who invoke the doctrine selectively.
 *
 * KEY AGENTS:
 *   - incumbent_authoritarian_regimes: primary beneficiary (institutional/arbitrage) — collect the shield
 *   - permanent_security_council_members_shielding_clients: agenda_setter and secondary beneficiary (institutional/arbitrage) — administer the veto gate selectively
 *   - populations_under_repressive_governance: primary target (powerless/trapped) — bear the cost of foreclosed external recourse
 *   - atrocity_survivors_without_external_recourse: primary target (powerless/trapped) — bear the cost most acutely in mass-atrocity cases
 *   - third_world_postcolonial_states: mixed beneficiary/payer (organized/constrained) — genuine defensive coordination coexisting with elite capture
 *   - human_rights_monitoring_bodies: excluded (organized/constrained) — document but cannot bind
 *   - international_law_scholars_comparative_observers: analytical observer — traces doctrinal genealogy and selective invocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.71).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty-Maximalist Reading of the RBIO Norm Complex").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '78586c82-1ed1-4090-b2dc-2d93b3fda710').
narrative_ontology:cs_kernel_codification('78586c82-1ed1-4090-b2dc-2d93b3fda710', distributed).
narrative_ontology:cs_authority_grounding('78586c82-1ed1-4090-b2dc-2d93b3fda710', distributed).
narrative_ontology:cs_reading_relation('78586c82-1ed1-4090-b2dc-2d93b3fda710', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('78586c82-1ed1-4090-b2dc-2d93b3fda710', rbio_practice_norm_complex__hegemonic_extraction_reading, influences).
narrative_ontology:cs_axiom('78586c82-1ed1-4090-b2dc-2d93b3fda710', foundational, sovereignty_is_absolute_and_non_derogable).
narrative_ontology:cs_axiom_status(sovereignty_is_absolute_and_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('78586c82-1ed1-4090-b2dc-2d93b3fda710', sovereignty_is_absolute_and_non_derogable, conventional).
narrative_ontology:cs_axiom('78586c82-1ed1-4090-b2dc-2d93b3fda710', foundational, humanitarian_exception_is_presumptively_pretextual).
narrative_ontology:cs_axiom_status(humanitarian_exception_is_presumptively_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('78586c82-1ed1-4090-b2dc-2d93b3fda710', humanitarian_exception_is_presumptively_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('78586c82-1ed1-4090-b2dc-2d93b3fda710', westphalian_non_intervention_baseline).
narrative_ontology:cs_drift_state('78586c82-1ed1-4090-b2dc-2d93b3fda710', post_r2p_adoption_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('78586c82-1ed1-4090-b2dc-2d93b3fda710', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_security_council_members_shielding_clients).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governance).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocity_survivors_without_external_recourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, third_world_postcolonial_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, third_world_postcolonial_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the sovereignty-maximalist reading to reject any external scrutiny of internal governance, treating the non-intervention norm as an absolute shield. They actively promote this reading in UN forums, regional blocs, and bilateral diplomacy, and benefit directly by converting a contested legal principle into a jurisdictional wall around their own conduct.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes, agenda_setter).

% Use veto power to entrench the maximalist reading selectively — for their own clients and allies — while permitting exceptions for their own strategic interventions. They administer the enforcement gate (Security Council authorization) that makes the reading operative in practice, and collect strategic loyalty and resource access from protected client states in exchange.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_security_council_members_shielding_clients, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_security_council_members_shielding_clients, beneficiary).

% Live under the governments the sovereignty-maximalist reading insulates from external accountability. They bear the direct cost of repression, have no internal recourse where courts and elections are captured, and the norm complex forecloses external recourse by defining any outside pressure as illegitimate interference regardless of the severity of internal abuse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governance, payer,
    powerless, biographical, trapped, national).

% Survive mass violence (genocide, ethnic cleansing, systematic starvation policies) that the maximalist reading treats as purely domestic, walling off humanitarian intervention debates as regime-change pretexts. Their testimony is heard in international forums but converts into no binding external obligation once the sovereignty veto is asserted.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocity_survivors_without_external_recourse, payer,
    powerless, biographical, trapped, national).

% Advance the maximalist reading defensively, having experienced intervention, colonization, and externally imposed regime change firsthand. They benefit from the shield against a repeat of historical interference, but some among them are themselves internally repressive and use the same shield to suppress domestic dissent — a genuine coordination function coexists with capture by their own governing elites.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, third_world_postcolonial_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, third_world_postcolonial_states, payer).

% Document abuses and would argue for a conditional-sovereignty or responsibility-to-protect framework, but their findings enter Security Council deliberation only as inputs subordinate to veto politics. They are structurally present in the process but their preferred normative resolution — that sovereignty is forfeitable upon atrocity — is foreclosed by the reading this constraint instantiates.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_monitoring_bodies, excluded,
    organized, biographical, constrained, global).

% Analyze the doctrinal history of non-intervention and humanitarian exception, tracing how the maximalist reading emerged from decolonization-era jurisprudence and persists through selective invocation. They can trace which invocations track principle versus which track the invoking state's own vulnerability to scrutiny.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_law_scholars_comparative_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable baseline against externally imposed regime change and coercive intervention — a genuine historical response to colonial and Cold War-era interventionism that many states, not only authoritarian ones, have structural reason to want preserved.
% TRANSFER_FUNCTION: Moves protection from external accountability to incumbent governing elites, and moves the cost of that protection onto populations who have no internal recourse and are denied international recourse by the same norm that shields their government.
% ABSENT_VOICES: Populations living under the shielded governments are almost entirely absent from the deliberative venues (Security Council, regional bodies) where the maximalist reading is asserted and enforced; human rights monitoring bodies document their situation but hold no vote and no veto.
% DISAPPEARANCE_RATIONALE: Incumbent regimes and their P5 patrons would experience the loss of the maximalist shield as an existential exposure to external accountability mechanisms — the world clearly rearranges for them. Populations under repression might see new (contested) intervention pathways open, though whether that produces protection or new externally imposed instability is itself disputed among the very victims the reading claims to speak for; hence the verdict is contested rather than a clean rearrangement.
% FOUNDING_PROBLEM: Post-colonial and Cold War-era states built the absolute non-intervention norm to prevent great powers from using humanitarian, ideological, or security pretexts to justify regime change, occupation, and resource extraction against weaker states — a problem with extensive documented historical basis (covert interventions, proxy wars, colonial 'civilizing mission' rhetoric).
% FOUNDING_PROBLEM_CORROBORATION: Postcolonial legal scholars and G77-aligned diplomats attest the founding problem remains live, citing recent interventions they characterize as pretextual. Human rights monitoring bodies and atrocity-documentation NGOs — outside the beneficiary set — attest that the founding problem has been substantially captured: the same absolute-sovereignty doctrine that once blocked colonial interference now blocks accountability for genuinely severe internal atrocities, and they document cases where invoking states show no independent commitment to the anti-imperial principle beyond shielding their own conduct.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) and suppression (0.71) are both substantial but not maximal: the maximalist reading does perform a real coordination function for postcolonial states with legitimate historical grievance, which caps how purely extractive the constraint can be authored as. Suppression is higher than extractiveness because the doctrine's enforcement mechanism (Security Council veto, diplomatic reciprocity norms) actively forecloses even the discussion of conditional sovereignty in binding fora, regardless of the severity of documented internal atrocity. Theater ratio (0.48) reflects that invocation of the doctrine has increasingly become rhetorical cover — states invoke 'sovereignty' selectively, defending it fiercely when their own conduct is scrutinized while treating it as negotiable when intervening against rivals, which is a performance divergence from principled consistency. Accessibility collapse (0.58) is moderate: alternative doctrines (responsibility to protect, conditional sovereignty) remain articulated and debated in scholarship and some fora, so alternatives have not collapsed as completely as they would under a genuine natural-law constraint. Resistance (0.55) is substantial and rising, driven by human rights bodies, R2P advocates, and affected populations' diaspora communities.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the P5 members shielding clients and incumbent regimes, the constraint reads as principled defense of sovereign equality against a resurgent interventionist agenda. From the seat of populations trapped under the shielded governments, the identical structure reads as an enforced wall between them and any external accountability, regardless of the severity of what is done to them. The engine computes these as different seat-level outcomes from the same structural data — this divergence is exactly what the sovereignty-maximalist reading, considered honestly, should produce, and is not resolved by picking one seat's account as 'the' truth of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent regimes and their P5 patrons sit near the full-beneficiary end: they administer the enforcement gate and collect the shield's benefit directly, with arbitrage-grade exit (they can invoke or waive the doctrine as convenient to their own interests). Populations under repression and atrocity survivors sit near the full-target end: trapped exit, no institutional voice, and the doctrine's operation directly forecloses their only plausible external recourse. Third-world postcolonial states occupy a genuinely mixed position — the coordination function is real for them as a class (protection against a repeat of colonial intervention) even where their own governing elites simultaneously exploit the same shield domestically; this dual position is why they are declared with both beneficiary and payer roles rather than forced into one direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anti-colonial protection against pretextual great-power intervention) was genuinely live through much of the twentieth century and its corroboration by postcolonial legal scholarship is not in dispute. What distinguishes this reading's classification as tangled_rope rather than a clean rope is that the same protective function has, per the corroborating testimony of human rights monitoring bodies (a source outside the beneficiary set), been substantially repurposed as a shield for internal atrocity where the founding anti-imperial problem is not actually at stake — the doctrine is invoked with no independent commitment to the anti-imperial principle beyond convenience. This is precisely the mandatrophy pattern the tangled_rope classification exists to capture: it must NOT be flattened into 'obviously mountain because sovereignty is foundational to the state system' (that erases the extraction) nor into 'obviously snare because authoritarian regimes exploit it' (that erases the genuine coordination interest of postcolonial states with real historical grievance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_maximalist_genuine_versus_captured,
    'Is the sovereignty-maximalist reading, as actually practiced by its leading invocants, a genuine principled commitment to anti-interventionism, or a selectively deployed shield whose invocation tracks the invoking state''s own exposure to scrutiny rather than any consistent doctrine?',
    'Comparative case analysis of invocation patterns: track whether states invoking the maximalist reading to block scrutiny of their own conduct also apply it consistently when evaluating interventions against states they oppose, versus supporting intervention selectively against rivals while shielding themselves.',
    'If invocation is principled and consistent, the reading is closer to a genuine (if contestable) rope-like coordination norm; if invocation tracks self-interest asymmetrically, the coordination story is substantially cover and the classification should sit further toward snare-like capture than the authored tangled_rope reflects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_maximalist_genuine_versus_captured, empirical, 'Whether maximalist-reading invocation is principled or self-serving in practice.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the RBIO norm complex supports at least three structurally distinct, non-reconcilable readings (this maximalist reading, the liberal_institutional_reading, and the hegemonic_extraction_reading), what determines which reading a given international actor adopts, and is that selection itself strategic rather than principled?',
    'Track whether individual states'' reading-selection correlates with their power position (P5 vs. non-P5), their governance type (democratic vs. authoritarian), or their historical exposure to intervention — a correlation with power/governance type rather than with consistent legal reasoning would suggest reading-selection is itself an instrument of the underlying extraction the hegemonic_extraction_reading identifies.',
    'If reading-selection tracks power and governance type rather than principled legal reasoning, this undermines the maximalist reading''s claim to be a principled doctrine and supports treating it as instrumentally adopted cover in many (not all) invocations, without denying its genuine coordination value for states with authentic historical grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading selection across the RBIO contest is principled or power-tracking.').

omega_variable(
    postcolonial_state_internal_capture,
    'Within the third_world_postcolonial_states seat, what proportion of governments invoking the maximalist reading do so primarily to protect against genuine external threat versus primarily to insulate their own internal repression from scrutiny?',
    'Cross-reference maximalist-reading invocation with independently documented domestic human rights records (e.g., from monitoring bodies outside the invoking government) across the population of postcolonial states over the measurement interval.',
    'A high proportion of invocation correlated with poor independently-documented human rights records would support treating the postcolonial beneficiary/payer dual role as substantially weighted toward elite capture rather than genuine collective defense; a low proportion would support treating the coordination function as the dominant real content of the reading for this seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(postcolonial_state_internal_capture, empirical, 'Internal heterogeneity within the postcolonial-state beneficiary class between genuine defense and elite capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.34).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel rbio_practice_norm_complex (see cs_structure.reading_relations for typed edges). The liberal_institutional_reading treats enforcement selectivity as a capacity problem within an otherwise universal, consent-based norm system; the hegemonic_extraction_reading treats the entire norm complex as a frozen hegemonic project whose formal revisability is practically foreclosed by P5 veto. This maximalist reading differs from both by denying any legitimate intervention authority beyond self-defense and treating humanitarian exception itself as structurally suspect. All three share the same underlying kernel (the RBIO norm complex as a contested commitment system) but instantiate different beneficiary/victim structures and different epsilon values; they are linked here rather than merged because merging would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
