% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense — Expansive Preventive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the expansive preventive reading of
 *   Article 51 of the UN Charter — the interpretation that self-defense
 *   extends to preemptive or preventive uses of force against non-state
 *   actors or emerging threats when necessity is demonstrated by the acting
 *   state itself. The reading emerged most forcefully after 9/11 but has
 *   doctrinal roots in anticipatory self-defense arguments (Caroline test)
 *   and the Nuclear Weapons Advisory Opinion. The constraint operates as a
 *   tangled_rope: it retains a genuine coordination function (collective
 *   security against armed attack) but has developed substantial asymmetric
 *   extraction — militarily capable states and their defense sectors gain
 *   low-constraint authorization for unilateral force, while target-region
 *   populations bear the costs of preventive strikes and the multilateral
 *   veto authority (UNSC) is structurally displaced. The self-judged
 *   necessity standard is the extraction mechanism: it converts a
 *   coordination constraint (armed attack trigger) into a permissive gateway.
 *   The measurement series shows extraction, theater, and suppression all
 *   rising sharply post-2001, tracking the doctrinal shift from 'armed attack
 *   occurred' to 'emerging threat demonstrated.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.68).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense — Expansive Preventive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '573eb035-1476-40c5-a002-d4332bc279d7').
narrative_ontology:cs_kernel_codification('573eb035-1476-40c5-a002-d4332bc279d7', formalized).
narrative_ontology:cs_authority_grounding('573eb035-1476-40c5-a002-d4332bc279d7', lineage).
narrative_ontology:cs_interpretation_layer_present('573eb035-1476-40c5-a002-d4332bc279d7').
narrative_ontology:cs_reading_relation('573eb035-1476-40c5-a002-d4332bc279d7', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('573eb035-1476-40c5-a002-d4332bc279d7', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('573eb035-1476-40c5-a002-d4332bc279d7', foundational, necessity_is_self_judged_by_acting_state).
narrative_ontology:cs_axiom_status(necessity_is_self_judged_by_acting_state, holdable).
narrative_ontology:cs_axiom_grounding('573eb035-1476-40c5-a002-d4332bc279d7', necessity_is_self_judged_by_acting_state, instrumental).
narrative_ontology:cs_axiom('573eb035-1476-40c5-a002-d4332bc279d7', foundational, non_state_actors_can_trigger_article_51_without_state_attribution).
narrative_ontology:cs_axiom_status(non_state_actors_can_trigger_article_51_without_state_attribution, holdable).
narrative_ontology:cs_axiom_grounding('573eb035-1476-40c5-a002-d4332bc279d7', non_state_actors_can_trigger_article_51_without_state_attribution, conventional).
narrative_ontology:cs_axiom('573eb035-1476-40c5-a002-d4332bc279d7', secondary, imminence_is_elastic_and_context_dependent).
narrative_ontology:cs_axiom_status(imminence_is_elastic_and_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('573eb035-1476-40c5-a002-d4332bc279d7', imminence_is_elastic_and_context_dependent, instrumental).
narrative_ontology:cs_reference_frame('573eb035-1476-40c5-a002-d4332bc279d7', post_911_preventive_self_defense_framework).
narrative_ontology:cs_drift_state('573eb035-1476-40c5-a002-d4332bc279d7', contemporary_multi_theater_operations, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('573eb035-1476-40c5-a002-d4332bc279d7', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, self_judged_necessity_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, preemptive_force_legitimacy).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, non_state_actor_armed_attack_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the military capacity to project force globally and the diplomatic capital to shape the necessity standard. They author the legal interpretations, control the UNSC veto, and decide when 'necessity is demonstrated.' They collect the security benefits of preventive action while externalizing kinetic costs to target regions. Their exit is arbitrage-grade: they can invoke or ignore the constraint at will.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Receive procurement contracts, operational authorizations, and institutional legitimacy from the expansive reading. The preventive force doctrine creates sustained demand for ISR platforms, precision munitions, and expeditionary capabilities. They lobby for doctrinal expansion but do not set the necessity standard. Their exit is mobile — they can pivot to other threat narratives if this one contracts.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    organized, biographical, mobile, global).

% Bear the kinetic effects of preventive strikes: casualties, displacement, infrastructure destruction, governance disruption, and long-term instability. They have no voice in the necessity determination, no access to the legal forums where the standard is shaped, and no exit from the geographic targeting logic. Their situation is defined by asymmetric exposure — they are the constraint's extraction surface.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, biographical, trapped, local).

% The UN Security Council's Chapter VII authorization gate is structurally bypassed when powerful states invoke self-judged preventive necessity. The veto power — the collective security architecture's core enforcement mechanism — is extracted from the multilateral system and relocated to the acting state's national decision process. The authority persists ceremonially but its functional control over force authorization has atrophied. Exit is constrained: the institution cannot abandon its mandate but cannot enforce it against the veto-holding beneficiaries.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).

% States and legal scholars who maintain the Caroline/Nicaragua standard: self-defense requires an actual or imminent armed attack by a state. They are excluded from the operational necessity determination but contest it in ICJ proceedings, UNGA debates, and treaty body interpretations. Their exclusion is structural — the expansive reading's self-judged standard does not accommodate their interpretive frame except as dissent.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, narrow_reading_adherents, excluded,
    organized, generational, mobile, global).

% States and scholars who advocate the intermediate doctrine: self-defense triggered when a non-state actor attacks from a host state unwilling/unable to suppress the threat. They are excluded from the expansive reading's self-judged necessity standard (which requires no host state nexus) but their doctrine is sometimes cited as a stepping stone to the expansive reading. They contest both the narrow reading's state-centrism and the expansive reading's limitlessness.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, unable_unwilling_proponents, excluded,
    organized, generational, mobile, global).

% ICJ judges, UN special rapporteurs, treaty bodies, and academic observers who track the doctrinal evolution. They document the constraint's operation across seats but do not collect from or pay into it. Their analytical seat sees the full structural asymmetry: the necessity standard's migration from objective trigger to subjective gateway, the veto authority's displacement, and the extraction flow from target populations to capable states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework authorizing force in response to armed attack — the collective security system's core exception to the Article 2(4) force prohibition. The expansive reading claims to extend this to emerging non-state threats where the traditional interstate trigger fails.
% TRANSFER_FUNCTION: Moves the authorization gate for preventive force from the UN Security Council (multilateral, veto-constrained) to the acting state's national necessity determination (unilateral, self-judged). Transfers the costs of kinetic action (casualties, displacement, destruction) from the acting state to target region populations. Transfers procurement and operational rents to defense sectors.
% ABSENT_VOICES: Target region populations are structurally excluded from the necessity determination — they cannot participate in the legal forums where 'imminence' and 'necessity' are defined. Future generations in target regions (who inherit the instability) are temporally excluded. Non-nuclear, non-permanent UNSC members are excluded from the veto calculus that enables or constrains the expansive reading.
% DISAPPEARANCE_RATIONALE: If the expansive preventive reading vanished overnight, the authorization gate for preventive force would revert to the UNSC (narrow reading) or the unable/unwilling hybrid. Militarily capable states would lose their self-judged necessity cover for unilateral preventive action. Target region populations would face fewer preventive strikes. Defense sectors would lose a major doctrinal driver for expeditionary procurement. The collective security architecture would revert to its 1945-2001 configuration — a significant rearrangement.
% FOUNDING_PROBLEM: The UN Charter's collective security system was designed for interstate war but faced non-state actor threats operating from weak or complicit host states. The 9/11 attacks demonstrated that the 'armed attack by a state' trigger failed to capture catastrophic non-state threats. The expansive reading was built to close this perceived gap.
% FOUNDING_PROBLEM_CORROBORATION: The 9/11 Commission Report and subsequent UN High-Level Panel reports attest the gap was real. The ICJ's Nicaragua and Nuclear Weapons opinions, the UNGA's Definition of Aggression (1974), and the non-aligned movement's consistent position attest the founding problem was not 'no trigger for non-state threats' but 'no trigger for unilateral preventive force against non-state actors without host state nexus' — a narrower gap that the unable/unwilling doctrine addresses without the expansive reading's self-judged necessity. The beneficiary states (US, UK, Israel, Russia) self-attest the problem remains live; the corroboration from outside the beneficiary set is mixed and contested.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint enables powerful states to externalize the costs of preventive force onto target populations while internalizing the security benefits. The self-judged necessity standard means the acting state is both judge and party — the classic extraction structure. Suppression (0.68) reflects the active diplomatic, legal, and military effort required to maintain the expansive reading against narrow readings and multilateral pushback. Theater ratio (0.42) is moderate: the collective security framing and legal formalism (necessity, proportionality, imminence language) perform coordination while the operational reality is permissive. Accessibility collapse (0.38) is modest — alternative frameworks (narrow reading, unable/unwilling doctrine) remain live and contested. Resistance (0.55) is significant: the narrow reading persists in ICJ jurisprudence, General Assembly resolutions, and non-aligned state practice.
 *
 * PERSPECTIVAL GAP:
 *   From the militarily capable state seat, the constraint appears as a necessary adaptation of collective security to non-state threats — a rope with genuine coordination value. From the target population seat, it appears as a snare — force authorized by a standard the target cannot contest, enforced by actors the target cannot influence. From the multilateral authority seat, it appears as a piton — the collective security architecture persists ceremonially but its authorization function has atrophied. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states (agenda_setter/beneficiary) sit at d ≈ 0.15: they author the necessity determination, control the enforcement machinery (UNSC veto, military capacity), and collect the security benefits. Defense sectors (beneficiary) at d ≈ 0.2: they receive procurement and operational authorization flows but don't set the agenda. Target region populations (payer) at d ≈ 0.85: they bear the kinetic costs, displacement, and governance disruption with no exit from the targeting logic and no voice in the necessity determination. Multilateral veto authority (payer) at d ≈ 0.7: the UNSC's authorization gate is structurally bypassed by the self-judged necessity claim — the veto power is extracted from the collective security architecture. The directionality spread is wide because the constraint's core mechanism (self-judged necessity) structurally advantages the powerful and disadvantages the targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (response to armed attack) is contested — the narrow reading says it's live, this reading says it's evolved. The constraint shows mandatrophy markers: theater rising (0.15→0.42), extraction rising (0.25→0.72), and the original coordination function (interstate armed attack response) now handles a minority of invocations. But the constraint is not a pure piton because the coordination function against non-state actors remains genuinely contested and the enforcement machinery is actively maintained, not just performed. The mandatrophy is partial and contested — hence tangled_rope, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expansive preventive reading a distinct constraint from the narrow armed attack reading and the unable/unwilling doctrine reading, or a single constraint measured differently?',
    'Structural decomposition: if the three readings produce different beneficiary/victim structures, different extraction profiles, and different enforcement requirements, they are distinct constraints linked by network.affects_constraints. The ε-invariance test applies — if changing the observable (what counts as ''armed attack'', ''imminence'', ''necessity'') changes ε, they are different constraints.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. The kernel_id article_51_self_defense becomes a family label, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Article 51 kernel decomposes into multiple structurally distinct constraints per the ε-invariance principle').

omega_variable(
    necessity_self_judgment_extraction,
    'Does the self-judged necessity standard function as a coordination mechanism (genuine collective security) or as an extraction enabler (cover for unilateral force by powerful states)?',
    'Comparative case analysis: examine instances where states invoked preventive self-defense against non-state actors. Code outcomes: (a) multilateral endorsement vs. unilateral action, (b) proportionality of response to threat, (c) duration and scope of force used. If pattern shows powerful states acting unilaterally with disproportionate force while weaker states cannot invoke the same standard, the necessity standard is an extraction enabler.',
    'If extraction enabler, the constraint''s coordination function is cover; classification shifts toward snare. If genuine coordination with asymmetric application, remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_extraction, empirical, 'Whether self-judged necessity is a genuine coordination standard or an extraction cover').

omega_variable(
    non_state_actor_attribution_boundary,
    'Where does the ''armed attack by non-state actors'' threshold actually operate — as a meaningful constraint on force or as a rhetorical gateway?',
    'Threshold tracing: track how the ''non-state actor armed attack'' concept has been applied from Nicaragua (1986) through post-9/11 practice. Measure the evidentiary standard required in each invocation and whether it constrains the acting state''s choice of target, timing, and scale.',
    'If the threshold is consistently met by minimal evidence and does not constrain targeting decisions, it is a gateway, not a constraint — extraction is higher than the coordination story admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_boundary, empirical, 'Whether the non-state actor armed attack threshold constrains or enables preventive force').

omega_variable(
    multilateral_veto_erosion,
    'Has the expansive reading structurally eroded the UN Security Council''s veto authority over authorized force, or does the veto remain a functional check?',
    'Institutional trajectory analysis: count UNSC authorized force resolutions vs. unilateral preventive invocations over the interval. Measure veto usage patterns — are vetoes cast against expansive self-defense claims, or has the veto been circumvented by doctrinal shift?',
    'If veto authority is circumvented, the multilateral_veto_authority victim seat experiences structural displacement — the constraint extracts from the collective security architecture itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multilateral_veto_erosion, empirical, 'Whether the expansive reading has displaced the UNSC veto as the authorization gate for force').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51_exp_prev_tr_t1945, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(a51_exp_prev_tr_t1960, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(a51_exp_prev_tr_t1980, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(a51_exp_prev_tr_t1990, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(a51_exp_prev_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(a51_exp_prev_tr_t2010, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(a51_exp_prev_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(a51_exp_prev_be_t1945, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(a51_exp_prev_be_t1960, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(a51_exp_prev_be_t1980, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(a51_exp_prev_be_t1990, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(a51_exp_prev_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(a51_exp_prev_be_t2010, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(a51_exp_prev_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(a51_exp_prev_su_t1945, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(a51_exp_prev_su_t1960, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(a51_exp_prev_su_t1980, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(a51_exp_prev_su_t1990, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(a51_exp_prev_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(a51_exp_prev_su_t2010, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(a51_exp_prev_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2024
narrative_ontology:measurement(a51_exp_prev_grid_01, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(class), 1945, 0.25).
narrative_ontology:measurement_basis(a51_exp_prev_grid_01, observed).
narrative_ontology:measurement(a51_exp_prev_grid_02, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(class), 2024, 0.42).
narrative_ontology:measurement_basis(a51_exp_prev_grid_02, observed).
narrative_ontology:measurement(a51_exp_prev_grid_03, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(individual), 1945, 0.1).
narrative_ontology:measurement_basis(a51_exp_prev_grid_03, observed).
narrative_ontology:measurement(a51_exp_prev_grid_04, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(individual), 2024, 0.28).
narrative_ontology:measurement_basis(a51_exp_prev_grid_04, observed).
narrative_ontology:measurement(a51_exp_prev_grid_05, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(organizational), 1945, 0.15).
narrative_ontology:measurement_basis(a51_exp_prev_grid_05, observed).
narrative_ontology:measurement(a51_exp_prev_grid_06, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(organizational), 2024, 0.38).
narrative_ontology:measurement_basis(a51_exp_prev_grid_06, observed).
narrative_ontology:measurement(a51_exp_prev_grid_07, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(structural), 1945, 0.2).
narrative_ontology:measurement_basis(a51_exp_prev_grid_07, observed).
narrative_ontology:measurement(a51_exp_prev_grid_08, article_51_self_defense__expansive_preventive_reading, accessibility_collapse(structural), 2024, 0.45).
narrative_ontology:measurement_basis(a51_exp_prev_grid_08, observed).
narrative_ontology:measurement(a51_exp_prev_grid_09, article_51_self_defense__expansive_preventive_reading, resistance(class), 1945, 0.3).
narrative_ontology:measurement_basis(a51_exp_prev_grid_09, observed).
narrative_ontology:measurement(a51_exp_prev_grid_10, article_51_self_defense__expansive_preventive_reading, resistance(class), 2024, 0.45).
narrative_ontology:measurement_basis(a51_exp_prev_grid_10, observed).
narrative_ontology:measurement(a51_exp_prev_grid_11, article_51_self_defense__expansive_preventive_reading, resistance(individual), 1945, 0.25).
narrative_ontology:measurement_basis(a51_exp_prev_grid_11, observed).
narrative_ontology:measurement(a51_exp_prev_grid_12, article_51_self_defense__expansive_preventive_reading, resistance(individual), 2024, 0.38).
narrative_ontology:measurement_basis(a51_exp_prev_grid_12, observed).
narrative_ontology:measurement(a51_exp_prev_grid_13, article_51_self_defense__expansive_preventive_reading, resistance(organizational), 1945, 0.35).
narrative_ontology:measurement_basis(a51_exp_prev_grid_13, observed).
narrative_ontology:measurement(a51_exp_prev_grid_14, article_51_self_defense__expansive_preventive_reading, resistance(organizational), 2024, 0.48).
narrative_ontology:measurement_basis(a51_exp_prev_grid_14, observed).
narrative_ontology:measurement(a51_exp_prev_grid_15, article_51_self_defense__expansive_preventive_reading, resistance(structural), 1945, 0.4).
narrative_ontology:measurement_basis(a51_exp_prev_grid_15, observed).
narrative_ontology:measurement(a51_exp_prev_grid_16, article_51_self_defense__expansive_preventive_reading, resistance(structural), 2024, 0.55).
narrative_ontology:measurement_basis(a51_exp_prev_grid_16, observed).
narrative_ontology:measurement(a51_exp_prev_grid_17, article_51_self_defense__expansive_preventive_reading, stakes_inflation(class), 1945, 0.2).
narrative_ontology:measurement_basis(a51_exp_prev_grid_17, observed).
narrative_ontology:measurement(a51_exp_prev_grid_18, article_51_self_defense__expansive_preventive_reading, stakes_inflation(class), 2024, 0.55).
narrative_ontology:measurement_basis(a51_exp_prev_grid_18, observed).
narrative_ontology:measurement(a51_exp_prev_grid_19, article_51_self_defense__expansive_preventive_reading, stakes_inflation(individual), 1945, 0.15).
narrative_ontology:measurement_basis(a51_exp_prev_grid_19, observed).
narrative_ontology:measurement(a51_exp_prev_grid_20, article_51_self_defense__expansive_preventive_reading, stakes_inflation(individual), 2024, 0.48).
narrative_ontology:measurement_basis(a51_exp_prev_grid_20, observed).
narrative_ontology:measurement(a51_exp_prev_grid_21, article_51_self_defense__expansive_preventive_reading, stakes_inflation(organizational), 1945, 0.25).
narrative_ontology:measurement_basis(a51_exp_prev_grid_21, observed).
narrative_ontology:measurement(a51_exp_prev_grid_22, article_51_self_defense__expansive_preventive_reading, stakes_inflation(organizational), 2024, 0.62).
narrative_ontology:measurement_basis(a51_exp_prev_grid_22, observed).
narrative_ontology:measurement(a51_exp_prev_grid_23, article_51_self_defense__expansive_preventive_reading, stakes_inflation(structural), 1945, 0.3).
narrative_ontology:measurement_basis(a51_exp_prev_grid_23, observed).
narrative_ontology:measurement(a51_exp_prev_grid_24, article_51_self_defense__expansive_preventive_reading, stakes_inflation(structural), 2024, 0.7).
narrative_ontology:measurement_basis(a51_exp_prev_grid_24, observed).
narrative_ontology:measurement(a51_exp_prev_grid_25, article_51_self_defense__expansive_preventive_reading, suppression(class), 1945, 0.25).
narrative_ontology:measurement_basis(a51_exp_prev_grid_25, observed).
narrative_ontology:measurement(a51_exp_prev_grid_26, article_51_self_defense__expansive_preventive_reading, suppression(class), 2024, 0.52).
narrative_ontology:measurement_basis(a51_exp_prev_grid_26, observed).
narrative_ontology:measurement(a51_exp_prev_grid_27, article_51_self_defense__expansive_preventive_reading, suppression(individual), 1945, 0.15).
narrative_ontology:measurement_basis(a51_exp_prev_grid_27, observed).
narrative_ontology:measurement(a51_exp_prev_grid_28, article_51_self_defense__expansive_preventive_reading, suppression(individual), 2024, 0.4).
narrative_ontology:measurement_basis(a51_exp_prev_grid_28, observed).
narrative_ontology:measurement(a51_exp_prev_grid_29, article_51_self_defense__expansive_preventive_reading, suppression(organizational), 1945, 0.3).
narrative_ontology:measurement_basis(a51_exp_prev_grid_29, observed).
narrative_ontology:measurement(a51_exp_prev_grid_30, article_51_self_defense__expansive_preventive_reading, suppression(organizational), 2024, 0.58).
narrative_ontology:measurement_basis(a51_exp_prev_grid_30, observed).
narrative_ontology:measurement(a51_exp_prev_grid_31, article_51_self_defense__expansive_preventive_reading, suppression(structural), 1945, 0.35).
narrative_ontology:measurement_basis(a51_exp_prev_grid_31, observed).
narrative_ontology:measurement(a51_exp_prev_grid_32, article_51_self_defense__expansive_preventive_reading, suppression(structural), 2024, 0.68).
narrative_ontology:measurement_basis(a51_exp_prev_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unsc_authorization_gate).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, collective_security_architecture).

% DUAL FORMULATION NOTE:
% Article 51 kernel decomposes into three structurally distinct constraints per ε-invariance: (1) narrow_armed_attack_reading — low ε, Mountain from most seats; (2) unable_unwilling_doctrine_reading — moderate ε, Tangled Rope (coordination via host state responsibility + extraction via lowered threshold); (3) expansive_preventive_reading — high ε, Tangled Rope (coordination via non-state actor threat framing + extraction via self-judged necessity). The three readings share the Article 51 text but instantiate different constraints with different ε, beneficiaries, victims, and enforcement requirements. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
