% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope: State-Centric Reading
 *   domain: legal/humanitarian/military
 *
 * SUMMARY:
 *   The Geneva Conventions establish a protective regime for combatants who
 *   meet Article 4 criteria: organized armed groups with responsible command,
 *   fixed distinctive signs, carrying arms openly, conducting lawful warfare.
 *   This state-centric reading interprets the criteria as exhaustive: only
 *   actors meeting all four conditions receive combatant immunity and POW
 *   status. Non-uniformed belligerents, insurgents, and non-state armed
 *   groups fall outside the treaty's scope, permitting states to target them
 *   as unlawful combatants without according them the legal status of lawful
 *   adversaries. The reading benefits conventional state militaries by
 *   granting them maximum operational latitude in asymmetric conflicts and by
 *   vindicating the legal doctrine that only states can make lawful war. The
 *   structural cost falls on non-uniformed actors and civilian populations
 *   living adjacent to them, whose legal status becomes indeterminate and
 *   whose targeting baseline shifts.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: Primary beneficiary (combatant immunity, POW protections, legal recognition)
 *   - non_uniformed_belligerents: Primary targets (excluded from protections, classified as unlawful combatants)
 *   - states_with_asymmetric_conflicts: Beneficiaries (operational latitude in counterinsurgency)
 *   - civilian_populations_adjacent_to_conflict: Secondary payers (collateral damage exposure from widened targeting latitude)
 *   - international_enforcement_bodies: Agenda-setters (interpret Article 4 criteria, adjudicate combatant status)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope: State-Centric Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "legal/humanitarian/military").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '85d3be4f-01bf-4f4a-a6da-536d392ddc86').
narrative_ontology:cs_kernel_codification('85d3be4f-01bf-4f4a-a6da-536d392ddc86', fixed_text).
narrative_ontology:cs_authority_grounding('85d3be4f-01bf-4f4a-a6da-536d392ddc86', lineage).
narrative_ontology:cs_interpretation_layer_present('85d3be4f-01bf-4f4a-a6da-536d392ddc86').
narrative_ontology:cs_reading_relation('85d3be4f-01bf-4f4a-a6da-536d392ddc86', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('85d3be4f-01bf-4f4a-a6da-536d392ddc86', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('85d3be4f-01bf-4f4a-a6da-536d392ddc86', foundational, state_monopoly_on_lawful_warfare).
narrative_ontology:cs_axiom_status(state_monopoly_on_lawful_warfare, holdable).
narrative_ontology:cs_axiom_grounding('85d3be4f-01bf-4f4a-a6da-536d392ddc86', state_monopoly_on_lawful_warfare, conventional).
narrative_ontology:cs_axiom('85d3be4f-01bf-4f4a-a6da-536d392ddc86', foundational, article_four_criteria_exhaustive).
narrative_ontology:cs_axiom_status(article_four_criteria_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('85d3be4f-01bf-4f4a-a6da-536d392ddc86', article_four_criteria_exhaustive, conventional).
narrative_ontology:cs_reference_frame('85d3be4f-01bf-4f4a-a6da-536d392ddc86', post_wwii_state_centric_order).
narrative_ontology:cs_drift_state('85d3be4f-01bf-4f4a-a6da-536d392ddc86', contemporary_niac_asymmetric_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('85d3be4f-01bf-4f4a-a6da-536d392ddc86', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_uniformed_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, hybrid_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_adjacent_to_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_adjacent_to_conflict).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, states_with_asymmetric_conflicts).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, state_monopoly_on_lawful_warfare).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, legal_distinction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State armed forces operating under national command structures and international treaty. They receive POW protections, combatant immunity (legal right to kill enemy combatants), and recognition of their personnel as lawful combatants. They set the interpretive agenda through treaty negotiation, military doctrine, and state delegation to international bodies. Their position is that the state form IS the legitimate carrier of lawful warfare authority and that the distinction between uniformed and non-uniformed belligerents is foundational to IHL.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter).

% Armed actors operating outside the formal state apparatus: insurgents, resistance movements, non-state armed groups, private military contractors not fully integrated into state command. Under this reading, they do NOT qualify for combatant immunity or POW status regardless of the military effectiveness of their operations or the legitimacy of their cause. They can be targeted as unlawful combatants and do not receive prisoner-of-war protections if captured. Their exit from the constraint is identity-locked: they cannot become 'lawful combatants' without dissolving into the state apparatus, yet their armed struggle is existentially tied to non-state status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_uniformed_belligerents, payer,
    organized, biographical, identity_locked, regional).

% Organizations that blur the state/non-state boundary: armed wings of political parties, militias with partial state backing, private contractors with de facto state delegation. They bear the ambiguity cost: their legal status oscillates with interpretive pressure, and the distinction between lawful and unlawful combatancy becomes a battleground in which their protections hinge on whether they are treated as state proxies or independent actors.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, hybrid_armed_groups, payer,
    moderate, biographical, constrained, regional).

% Communities living in or near territories where non-state armed groups operate. Under this reading, the bright-line rule—uniformed state combatants vs. unprivileged belligerents—produces collateral harm: state militaries operate with wider targeting latitude against non-uniformed actors, creating pressure to treat civilian-adjacent populations as harboring unprivileged belligerents. They benefit from the civilian immunity principle but suffer from enforcement that conflates non-uniformed combatancy with civilian proximity.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_adjacent_to_conflict, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_adjacent_to_conflict, beneficiary).

% International Criminal Court, human rights monitoring bodies, treaty-ratifying states through their binding interpretation machinery. They enforce Article 4 criteria and determine whether a belligerent qualifies for combatant status. Their authority rests on the state-centric reading's clarity: the criteria are jurisdictionally manageable because they attach to state-recognized forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, global).

% States fighting insurgencies, separatist movements, or non-state armed groups. This reading grants them maximum operational latitude: they can classify opponents as unlawful combatants, deny them POW status, and target them without the burden of treating them as lawful adversaries. The constraint benefits them structurally because it permits them to define the legal boundary (state vs. non-state) in their favor.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, states_with_asymmetric_conflicts, beneficiary,
    powerful, generational, mobile, national).

% NGOs, international bodies, and legal scholars arguing for universal human rights minimums regardless of combatant status. They are excluded from the formal treaty negotiation apparatus and must challenge the state-centric reading through parallel human rights instruments (ICCPR, CAT) rather than within the Geneva framework itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, excluded,
    moderate, biographical, constrained, global).

% The historical consensus that produced the 1949 Geneva Conventions and their Additional Protocols. This is not an actor but the institutional-historical backdrop: the drafting process was dominated by state delegations emerging from World War II, during which the state-war-form was hegemonic. The constraint embeds that era's assumptions.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, treaty_drafting_consensus, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(geneva_conventions_protective_scope__state_centric_reading, treaty_drafting_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal distinction between lawful and unlawful combatants to enable parties to an armed conflict to know which persons are entitled to protections. By codifying Article 4 criteria (uniform, insignia, responsible command, lawful conduct), the scheme theoretically solves the problem of identifying protected combatants without requiring case-by-case adjudication.
% TRANSFER_FUNCTION: Transfers legal immunity and protection status FROM non-state armed groups and hybrid actors TO state militaries and their uniformed personnel. State combatants receive POW status if captured, immunity from prosecution for lawful acts of war, and legal recognition; non-uniformed belligerents receive neither and may be prosecuted as common criminals despite military objectives.
% ABSENT_VOICES: Non-state armed groups and their constituencies are structurally excluded from treaty negotiation: the Geneva Conventions are negotiated BY states FOR states. Resistance movements, insurgents, and non-state armed organizations have no formal seat at the drafting table. Their objections—that the distinction privileges the powerful and denies protections based on the form of organization rather than conduct or proportionality—are carried only through parallel NGO and human rights bodies, not through the formal IHL apparatus.
% DISAPPEARANCE_RATIONALE: If the state-centric reading disappeared and protections extended uniformly to all combatants regardless of state status, state militaries would lose the legal latitude to target non-uniformed actors without combatant-immunity analysis. Hybrid actors would gain POW protections. The legal landscape of asymmetric conflicts would reorganize: states could no longer rely on the unlawful combatant category as a targeting justification. The strategic geometry of insurgency and state response would shift, likely raising the cost to states of asymmetric warfare.
% FOUNDING_PROBLEM: In the 1920s–1940s, international law needed to distinguish between combatants entitled to protection and illegitimate actors. The problem was acute during WWII when irregular forces, partisans, and resistance movements blurred the boundaries. States argued that without a bright-line rule (uniform + command + insignia), every armed actor could claim combatant status, collapsing the distinction between war and crime.
% FOUNDING_PROBLEM_CORROBORATION: State military establishments and international humanitarian law scholars trained in the conventional framework attest the founding problem is live: asymmetric conflicts, private military contractors, and armed groups still blur the boundaries, making the Article 4 distinction necessary. Conversely, human rights bodies, non-state armed groups, and legal scholars in the universal-rights tradition attest the founding problem has been overtaken by the emergence of non-international armed conflicts (NIAC) under AP II and Common Article 3, which extend baseline protections regardless of combatant status. The International Committee of the Red Cross (ICRC) officially splits the difference with the hybrid proportionality reading; no single corroborating authority outside the state military establishment fully endorses the state-centric reading alone.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the constraint narrows protections dramatically for non-state actors, permitting targeting without combatant-immunity analysis. This is extraction in the sense that states benefit from the asymmetric rule while bearing no corresponding cost. Suppression is similarly high (0.71) because maintaining the distinction requires active enforcement: states must continually adjudicate which actors qualify as Article 4 combatants, deny protections to those who do not, and defend the boundary against erosion. Theater has risen substantially over the 75-year interval (0.20→0.42) as non-international armed conflict has become the norm: the state-centric reading's original purpose—distinguishing lawful INTERNATIONAL warfare—is less functionally necessary, yet the apparatus persists. The Additional Protocols (AP I in 1977, AP II in 1977) were meant to clarify the state-centric rule for NIAC contexts, but they actually introduced proportionality and context-sensitivity, undermining the rule's clarity. Theater ratio increase reflects the growing gap between stated Article 4 criteria and actual application under NIAC conditions. Measurements use a shared time grid anchored at 1949 (Geneva Conventions entry into force) and 2024 (contemporary asymmetric warfare era). The trajectory is monotonically increasing in suppression requirement and theater, indicating institutional drift: the constraint is held in place less by functional necessity and more by performative commitment to a state-centric order.
 *
 * PERSPECTIVAL GAP:
 *   From the state military institutional seat, this constraint appears as a legitimate ordering rule: it codifies which actors are lawful combatants and preserves the state's role as the sole legitimate wielder of warfare authority. From the non-uniformed belligerent seat, the same constraint appears as a targeting mechanism: the Article 4 criteria are not objective facts about military organization but ex-post justifications for denying protections to actors outside the state apparatus. The engine should compute a high directionality divergence (state militaries near d=0.0, non-uniformed belligerents near d=1.0) from the beneficiary/victim declarations and exit-option asymmetry. The institutional actors have arbitrage-grade exit (they can declare their forces Article-4-compliant); non-uniformed actors have identity-locked exit (they cannot become uniformed state combatants without ceasing to exist as autonomous armed groups).
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries derive d ≈ 0.1–0.2 (deep beneficiary) from: (1) declared beneficiary status; (2) institutional power; (3) arbitrage exit (they set the rules and can comply with them by construction). Non-uniformed belligerents derive d ≈ 0.8–0.9 (deep target) from: (1) victim declaration; (2) organized but moderate power; (3) identity-locked exit (exit the armed struggle = exit the identity). Hybrid armed groups sit mid-d (0.4–0.6) because their status oscillates: they may or may not qualify for Article 4 criteria depending on state interpretation. Civilian populations derive d ≈ 0.6–0.7 (toward target) from secondary payer role and trapped exit, though they also receive nominal protection under the civilian immunity principle. The state beneficiary and non-state victim directionalities should produce a seat divergence: this constraint should compute as rope or scaffold from the state military seat, and as snare from the non-uniformed belligerent seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading faces a mandate obsolescence challenge: it was designed to solve the problem of distinguishing combatants in INTERNATIONAL armed conflicts between recognized states. Since 1990, the majority of armed conflicts are NON-INTERNATIONAL (state vs. non-state, or non-state vs. non-state). AP II and Common Article 3 were added to extend baseline protections to NIAC, but they introduced proportionality and context-sensitivity, which directly undermine the state-centric reading's bright-line rule. The mandate (distinguish lawful from unlawful combatants) has partially atrophied in NIAC contexts because AP II requires states to provide humanitarian protections regardless of combatant status. Yet the state-centric reading persists because states benefit from retaining the unlawful combatant category for targeting justification. This is a classic mandatrophy pattern: the original coordination function is degraded, but institutional inertia and beneficiary capture prevent rule revision. The constraint should be classified as tangled_rope (has coordination function, active enforcement, asymmetric extraction) rather than piton, but with high theater ratio and decaying functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_monopoly_contestation,
    'Is the state monopoly on lawful warfare a structural fact of international law, or a constructed doctrine that states have interest in maintaining?',
    'Comparative legal history: examine non-state armed group claims to combatant status in international forums (ICC cases, IHL scholarship by non-state actors) and trace whether the state-centric reading is applied uniformly or whether state interests shape enforcement.',
    'If the monopoly is a structural fact, the state-centric reading is a discovery. If constructed, it is extractive doctrine that benefits states while imposing costs on non-state actors. This is the core reading-contest between state_centric and universal_rights framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_contestation, conceptual, 'Whether state monopoly on lawful warfare is structural or constructed.').

omega_variable(
    article_four_criteria_objective,
    'Are the Article 4 criteria (uniform, insignia, responsible command, lawful conduct) objective or contingent on state interpretation?',
    'Systematic review of ICC and ICTY caselaw: do courts apply Article 4 criteria consistently across state and non-state actors, or do they interpret the criteria more flexibly when the armed group is a state proxy vs. a genuine non-state actor?',
    'If the criteria are objective, the state-centric reading is operationally neutral. If contingent on state interpretation, the criteria serve as a post-hoc justification for denying protections. High contingency would support the snare reading from the non-state victim seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_four_criteria_objective, empirical, 'Whether Article 4 criteria are applied uniformly or interpreted flexibly to benefit states.').

omega_variable(
    niac_mandate_drift,
    'Has the introduction of AP II and Common Article 3 (baseline protections in non-international conflicts) functionally superseded the Article 4 bright-line rule for most contemporary conflicts?',
    'Corpus analysis of state military doctrine, ICC prosecutorial decisions, and ICRC operational guidance in NIAC contexts: do states and international bodies apply Article 4 criteria, or do they apply AP II proportionality standards instead?',
    'If AP II has functionally superseded the state-centric rule, the constraint is piton-class (atrophied mandate, persisting through inertia). If states retain the Article 4 bright-line as a tactical targeting tool, the constraint remains snare-class for non-state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(niac_mandate_drift, empirical, 'Whether the state-centric reading''s mandate has atrophied in NIAC practice.').

omega_variable(
    reading_foreclosure,
    'Does the state-centric reading logically foreclose the universal_rights_reading, or do they coexist as genuinely alternative readings of the same kernel?',
    'Legal doctrinal analysis: can a single international legal framework simultaneously hold (a) combatant status is conditional on Article 4 criteria, AND (b) all persons affected by armed conflict receive baseline human-rights protections regardless of status? If yes, the readings coexist; if no, one reading forecloses the other.',
    'If foreclosure holds, the engine should emit a foreclosure edge in the reading_relations topology. If coexistence holds, the readings compete through parallel doctrinal systems (Geneva law vs. human rights law) without logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure, conceptual, 'Whether state-centric and universal-rights readings foreclose each other or coexist.').

omega_variable(
    suppression_internalization,
    'For non-uniformed belligerents, is the measured suppression primarily structural (legal barriers, targeting exposure, denial of protections) or internalized (psychological adoption of the unlawful-combatant framing by armed groups themselves)?',
    'Ethnographic and interview data from non-state armed groups: do they resist the unlawful-combatant label as externally imposed, or do they internalize it as legitimate? Does suppression persist after removal of legal barriers (e.g., in territories where they exercise de facto governance)?',
    'If suppression is internalized, non-state actors carry the constraint with them even in the absence of direct state enforcement. The effective suppression would be higher than the structural measure suggests, and exit from the constraint (if the state withdrew enforcement) would be slower than expected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether non-state combatant suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement_basis(gene_tr_t1949, observed).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement_basis(gene_tr_t1977, observed).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(gene_tr_t1990, observed).
narrative_ontology:measurement(gene_tr_t2005, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(gene_tr_t2005, observed).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(gene_tr_t2015, observed).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(gene_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.55).
narrative_ontology:measurement_basis(gene_be_t1949, observed).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1977, 0.62).
narrative_ontology:measurement_basis(gene_be_t1977, observed).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1990, 0.64).
narrative_ontology:measurement_basis(gene_be_t1990, observed).
narrative_ontology:measurement(gene_be_t2005, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement_basis(gene_be_t2005, observed).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(gene_be_t2015, observed).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(gene_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement_basis(gene_su_t1949, observed).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement_basis(gene_su_t1977, observed).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1990, 0.67).
narrative_ontology:measurement_basis(gene_su_t1990, observed).
narrative_ontology:measurement(gene_su_t2005, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement_basis(gene_su_t2005, observed).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(gene_su_t2015, observed).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(gene_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, asymmetric_conflict_targeting_latitude).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, international_criminal_court_jurisdictional_scope).

% DUAL FORMULATION NOTE:
% The kernel geneva_conventions_protective_scope splits into three structurally distinct readings: state_centric (this constraint, ε≈0.68, beneficiaries=states), hybrid_proportionality (ε≈0.45, beneficiaries=international humanitarian bodies), universal_rights (ε≈0.30, beneficiaries=all persons affected by conflict). The readings have incompatible victim sets and different extraction profiles. The state-centric reading narrows protections to uniformed state combatants; the universal reading extends protections universally; the hybrid reading grades protections by conflict type and proportionality. Each is a complete constraint with distinct beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
