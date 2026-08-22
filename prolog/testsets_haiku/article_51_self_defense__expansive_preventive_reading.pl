% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Expansive Preventive Self-Defense Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the expansive preventive reading of Article
 *   51 of the UN Charter. The reading permits militarily capable states to
 *   initiate force against non-state actors or emerging threats when
 *   necessity is self-judged, without prior armed attack or Security Council
 *   authorization. The reading is contested: narrow-reading advocates argue
 *   it violates the Charter's limits on unilateral force and enables endless
 *   preventive wars; unable-unwilling doctrine advocates propose a middle
 *   ground tied to host-state complicity. This story captures ONE reading
 *   only — the expansive preventive interpretation — assessing its structural
 *   effects on who benefits, who pays, and how authority is distributed. The
 *   claim is tangled_rope (coordinating against transnational threats while
 *   extracting unilateral advantage); the metrics describe high extraction,
 *   active suppression of multilateral veto, and rising theater (preventive
 *   justifications that increasingly cover interests beyond immediate threat
 *   response).
 *
 * KEY AGENTS:
 *   - militarily_capable_states: agenda-setters and primary beneficiaries; control necessity determination and set operational scope
 *   - defense_sector_industries: beneficiaries; gain from sustained procurement and operational tempo
 *   - target_region_populations: primary payers and victims; bear direct costs of military operations with no voice in necessity judgment
 *   - multilateral_veto_authority (UN Security Council): structural target; authority is circumvented by unilateral preventive action
 *   - non_state_actor_host_states: secondary payers; territory becomes legitimate target even without state consent
 *   - narrow_reading_advocates: excluded; would reframe preventive operations as illegal but lack enforcement power
 *   - intelligence_and_security_establishments: beneficiaries; gain expanded mandates and budgets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.79).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Expansive Preventive Self-Defense Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, 'f34cda64-92ef-4404-ba16-b99a9aa9a0b8').
narrative_ontology:cs_kernel_codification('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', fixed_text).
narrative_ontology:cs_authority_grounding('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', lineage).
narrative_ontology:cs_interpretation_layer_present('f34cda64-92ef-4404-ba16-b99a9aa9a0b8').
narrative_ontology:cs_reading_relation('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', foundational, necessity_self_judged_by_state).
narrative_ontology:cs_axiom_status(necessity_self_judged_by_state, holdable).
narrative_ontology:cs_axiom_grounding('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', necessity_self_judged_by_state, instrumental).
narrative_ontology:cs_axiom('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', foundational, preventive_force_permissible_non_state_threats).
narrative_ontology:cs_axiom_status(preventive_force_permissible_non_state_threats, holdable).
narrative_ontology:cs_axiom_grounding('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', preventive_force_permissible_non_state_threats, empirically_contingent).
narrative_ontology:cs_reference_frame('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', flexible_self_defense_authority).
narrative_ontology:cs_drift_state('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', contemporary_post_9_11_security_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f34cda64-92ef-4404-ba16-b99a9aa9a0b8', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sector_industries).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, non_state_actor_host_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, intelligence_and_security_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the expansive preventive self-defense doctrine to conduct military operations against non-state actors or emerging threats without prior Security Council authorization. They control the necessity determination (classify a potential threat as requiring preventive force), set the scope of operations, and avoid the constraint that narrow readings would impose. They benefit from doctrinal flexibility in unilateral action. Their exit from this reading would mean accepting multilateral constraints or waiting for imminent threat to materialize before acting.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Benefit from sustained demand for military hardware, services, and intelligence capabilities to support preventive operations. The expansive doctrine creates continuous justifications for defense spending and procurement. Their positioning is indirect: they do not set policy but lobby and align with militarily capable states, and gain from the increased operational tempo this reading generates.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sector_industries, beneficiary,
    powerful, biographical, mobile, global).

% Bear the direct costs of preventive military operations: civilian casualties, infrastructure destruction, displacement, and long-term security instability. They are classified as hosting or harboring potential threats but have no seat at the necessity determination. They cannot opt out of being in the geographic zone where the acting state judges prevention necessary.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The Security Council and the UN system are sidelined by unilateral preventive actions taken under this reading. Their authority to authorize or deny force is circumvented when capable states self-judge necessity. They retain formal veto power over resolutions but face de facto fait accompli situations. Their exit would mean reforming the international governance structure entirely; they are constrained to ex-post facto responses.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).

% Bear the costs of preventive action on their territory without consent or authorization, even when they have not attacked and may not control the non-state actors within their borders. This reading constructs them as legitimate targets of force if they are deemed unable or unwilling to suppress a threat. They have diplomatic options but limited military leverage to resist incursions justified under self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, non_state_actor_host_states, payer,
    moderate, biographical, constrained, regional).

% International law scholars, smaller states, and human rights organizations advocate for the narrow reading, requiring imminent armed attack and state attribution. They would reframe preventive operations as violations of international law and the UN Charter. They are excluded from setting the necessity determination and must respond post-hoc through diplomatic channels or courts that have limited enforcement power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, narrow_reading_advocates, excluded,
    organized, generational, constrained, global).

% Gain expanded mandates and budgets to conduct surveillance, threat assessment, and early-warning operations that justify preventive force. The burden of proof for 'emerging threat' and 'necessity' falls within their technical and analytical judgment. They benefit from expanded operational authority under this reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, intelligence_and_security_establishments, beneficiary,
    organized, biographical, mobile, global).

% The ICJ and other international courts review self-defense claims but lack enforcement mechanisms to compel compliance with their judgments. They observe the constraint's operation and can issue advisory opinions or orders, but their authority over the acting states is limited when those states reject the narrow reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_court_system, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrine under which militarily capable states can respond to non-state actor threats and emerging security challenges without waiting for an imminent armed attack to materialize. The coordination claim is that this flexibility enables collective security by allowing faster threat prevention and deterrence of state harboring of non-state actors.
% TRANSFER_FUNCTION: Transfers authority to conduct military force from the multilateral (Security Council) to the unilateral (acting state) level. Transfers the risk and costs of military operations from the acting state to target-region populations and host states. Transfers legitimacy determinations from external (international law standards) to internal (acting state necessity judgment).
% ABSENT_VOICES: Populations in target regions have no voice in necessity determination. Smaller states that cannot afford preventive military operations have limited say in doctrine formation. Non-state actors themselves and their host states cannot contest the threat classification before force is applied. Scholars and advocates of the narrow reading are excluded from the institutional decision process.
% DISAPPEARANCE_RATIONALE: If this expansive reading disappeared and only the narrow reading persisted, the international security order would reorganize significantly: unilateral preventive operations would cease or be reframed as violations; militarily capable states would either wait for imminent threat or seek Security Council authorization; defense spending priorities would shift; regional security dynamics would alter as some deterrence mechanisms became unavailable; the legal constraint would functionally return to the pre-2001 understanding of Article 51.
% FOUNDING_PROBLEM: The rise of transnational non-state actor threats (post-9/11 terrorism) that operate across borders without state sponsorship and can initiate attacks from weak or failed states where no imminent armed attack may be visible until operations are underway. The founding doctrine addressed the gap between the article 51 frame (state-to-state attack) and the threat reality of networked non-state actors.
% FOUNDING_PROBLEM_CORROBORATION: The United States, Israel, and other militarily capable states attest the founding problem is live and requires preventive doctrine. International law scholars, the UN Secretary-General, and developing states attest the founding problem is exaggerated and the remedy is broader than necessary, enabling mission creep. Empirical research on actual terrorist attack prevention effects of preventive operations is mixed; no consensus corroboration exists from outside the benefiting parties.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising (0.62→0.79 over the interval) because the reading progressively decouples force authorization from multilateral constraint, allowing capable states to collect the security benefits of prevention without bearing the costs of Security Council negotiation or international legal restraint. Suppression is substantial (0.72) because the reading's persistence requires actively defending it against narrower legal interpretations and sidelining the veto authority. Theater rises (0.28→0.41) as preventive justifications increasingly stretch beyond imminent threat to cover strategic interests, deterrence signaling, and long-term capability degradation — activities that serve state interests but claim necessity language. The measurement series show extraction accumulating as doctrine precedent hardens (landmark operations by capable states establish the doctrine as customary law, raising the barrier to rejecting it). Suppression requirement increases as resistance from narrow-reading advocates and smaller states intensifies, requiring more active legitimation work to maintain the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the capable state's position, this is genuine coordination against a real threat where flexibility is necessary; from the target population's position, it is coercive force authority divorced from consent; from the multilateral authority's position, it is structural erosion of collective decision-making. The engine should compute markedly different types across these seats: the capable state may compute rope or tangled_rope (coordination with some extraction), while the target population computes snare (pure extraction with no coordination benefit), and the veto authority computes tangled_rope (coordination function in theory but extraction of authority in practice).
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states sit near d=0.0 (beneficiary end): they collect unilateral authority, avoid multilateral constraint, and face minimal enforcement cost for self-judging necessity. The defense sector sits near d=0.1 (weak beneficiary): they gain from sustained operations but do not control the doctrine. Target populations sit near d=1.0 (full target): they bear military costs, have no exit, cannot contest threat classification, and receive no direct benefit from the doctrine. The multilateral veto authority sits near d=0.8 (strong target): their authority is systematically bypassed, but they retain formal veto power (not complete extraction). Host states sit near d=0.6 (moderate target): they bear some costs and lose sovereignty, but some may align with capable states. Intelligence establishments sit near d=0.05 (light beneficiary): they gain expanded mandates and operational authority without directly setting policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transnational non-state actor threats) remains contested — some attest it is live and urgent, others attest it is exaggerated or has been managed without preventive doctrine. The disappearance verdict is world_rearranges (the doctrine's removal would significantly alter unilateral action authority). These mismatch: a live founding problem with world_rearranges would support the doctrine's necessity, but the contested status and the rising theater_ratio (preventive language covering non-threat interests) suggest mandate expansion beyond the founding scope. No formal mandatrophy declaration is reached because the founding problem remains live for some parties, but the measurement series showing theater rising and suppression requirement increasing indicate degradation of the initial mandate boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_standard,
    'What standard determines whether a threat meets the ''necessity'' threshold for preventive force under this reading? Is necessity determined by objective threat probability, by the acting state''s subjective assessment, or by some hybrid?',
    'Post-operation audits by international courts or independent bodies comparing the pre-operation threat assessment against post-operation evidence; track false-positive preventive actions (operations against threats that did not materialize).',
    'If necessity is objective and verifiable, the reading constrains capability-based adventurism; if necessity is self-judged by the acting state, the reading becomes a blank check for unilateral force. This determines whether the constraint is a genuine coordination mechanism or pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_standard, empirical, 'The standard for determining necessity in preventive self-defense claims.').

omega_variable(
    non_state_actor_attribution_problem,
    'When a non-state actor operates from a host state''s territory, at what threshold of evidence is the host state deemed ''unable or unwilling'' to suppress the actor, triggering preventive force authority?',
    'Comparative case analysis of preventive operations: did the acting state provide evidence of host-state inability/unwillingness before operation, or was this determined retroactively? Did the host state have actual opportunity to suppress the actor?',
    'If the threshold is high (clear evidence, prior diplomatic demands), the reading constrains false characterizations; if low (mere presence of non-state actor), the reading permits vast preventive authority. This determines whether capability or genuine threat response shapes operations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_problem, empirical, 'The evidentiary standard for host-state inability or unwillingness.').

omega_variable(
    narrow_vs_expansive_foreclosure,
    'Do the expansive and narrow readings foreclose each other (logically incompatible premises) or coexist (different parties hold both simultaneously)?',
    'Assess whether a single state''s legal framework could adopt both readings: could a state endorse narrow self-defense in principle while claiming expansive authority in practice? Or does accepting narrow reading require rejecting expansive? The answer determines reading relations structure.',
    'If foreclosure: the readings are strictly competitive, and one must fail for the other to prevail. If coexistence: both readings remain live in international law as different parties'' positions, and the constraint''s persistence depends on power asymmetry (capable states enforce expansive, weaker states advocate narrow) rather than legal resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_vs_expansive_foreclosure, conceptual, 'Logical relationship between expansive and narrow self-defense readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of multilateral veto authority structural (institutional bypass, de facto circumvention) or internalized (host states and smaller states accept preventive doctrine as legitimate)?',
    'Post-operation state behavior: do target states and host states mount legal challenges and diplomatic resistance, or do they tacitly accept preventive operations? Track voting patterns in UN debates on preventive actions.',
    'If structural suppression persists after operations cease (states continue challenging doctrine, building institutional countermeasures), suppression is robust and the constraint remains high-extraction. If suppression is internalized (states accept the doctrine as legitimate), the constraint may reclassify toward coordination. The measurement is the trajectory of resistance after initial operations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of veto authority is structural or internalized.').

omega_variable(
    reading_divergence_scope,
    'Is the structural delta between expansive and narrow readings primarily about the trigger threshold (imminent vs. preventive) or about authority distribution (unilateral vs. multilateral)? These produce different constraint types.',
    'Thought experiment: if a multilateral body (Security Council) explicitly authorized preventive force, would the expansive reading still grant unilateral authority, or would it be satisfied? This tests whether expansive doctrine is fundamentally about scope of force or about authority distribution.',
    'If trigger is primary: both readings permit the same force once triggered; only the decision point differs. If authority distribution is primary: even identical force decisions would be legitimate under expansive (unilateral) but illegitimate under narrow (no unilateral authority without imminent attack). This determines how much the sibling readings actually diverge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_divergence_scope, conceptual, 'Whether reading divergence is about trigger threshold or authority distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__expansive_preventive_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__expansive_preventive_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__expansive_preventive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, sovereignty_non_intervention_constraint).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_charter_chapter_vii_authority).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, transnational_terrorism_response_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the Article 51 kernel. Each reading (expansive_preventive, narrow_armed_attack, unable_unwilling) has different ε values, beneficiary/victim structures, and classification consequences. The readings share a common referent (the Article 51 authorization clause) but instantiate different constraints because they carry different authority distributions and necessity standards. Link all three stories via network.affects_constraints to model the kernel contest. The expansive reading influences the narrow reading (higher precedent weight from capable-state practice) and influences the unable_unwilling reading (provides the doctrinal space that unable_unwilling attempts to constrain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
