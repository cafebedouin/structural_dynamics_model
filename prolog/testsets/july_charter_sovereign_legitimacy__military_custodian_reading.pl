% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Military Custodian Reading of July Charter Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   The July Charter, drafted by the military-led revolutionary council,
 *   enshrines the military as a permanent institutional guardian with
 *   explicit veto authority over major political decisions and the power to
 *   suspend civilian institutions when deemed necessary for state stability.
 *   This constraint story instantiates the military_custodian_reading of the
 *   contested sovereign-legitimacy kernel. Under this reading, the Charter
 *   ratifies military oversight as constitutionally necessary; civilian
 *   political contestation is bounded by security apparatus judgment; and the
 *   founding problem (post-revolutionary fragmentation) justifies permanent
 *   military subordination of civilian institutions. This is ONE reading of a
 *   three-way contest: the secular_democratic_reading frames the same Charter
 *   clauses as subordinating military to civilian authority; the
 *   guided_nationalism_reading grounds legitimacy in Islamic-national
 *   identity rather than institutional structure. The ε-invariance principle
 *   requires each reading to have its own ε, beneficiary structure, and type
 *   — this story does not describe the contest itself, only the
 *   military_custodian_reading's structural position.
 *
 * KEY AGENTS:
 *   - military_institutional_apparatus: sets and enforces military-guardian provisions; collects institutional autonomy and veto authority
 *   - autonomous_political_parties: payers subject to military security vetting; identity-locked by participation structure
 *   - student_movement: powerless payers subject to campus security apparatus; constrained exit
 *   - civilian_legislative_body: organized payer with residual legitimacy but subordinated decision authority
 *   - revolutionary_founding_authority: beneficiary justifying permanent guardianship as stability mechanism
 *   - external_state_actors: observers whose recognition affects the reading's international legitimacy
 *   - imprisoned_political_dissidents: excluded voices whose absence from the conversation means the constraint is never tested against those it most constrains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Military Custodian Reading of July Charter Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'e90d94a5-29a5-417c-a63f-7a30b6f9064f').
narrative_ontology:cs_kernel_codification('e90d94a5-29a5-417c-a63f-7a30b6f9064f', formalized).
narrative_ontology:cs_authority_grounding('e90d94a5-29a5-417c-a63f-7a30b6f9064f', lineage).
narrative_ontology:cs_interpretation_layer_present('e90d94a5-29a5-417c-a63f-7a30b6f9064f').
narrative_ontology:cs_reading_relation('e90d94a5-29a5-417c-a63f-7a30b6f9064f', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('e90d94a5-29a5-417c-a63f-7a30b6f9064f', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('e90d94a5-29a5-417c-a63f-7a30b6f9064f', foundational, military_permanent_custodianship_necessary).
narrative_ontology:cs_axiom_status(military_permanent_custodianship_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e90d94a5-29a5-417c-a63f-7a30b6f9064f', military_permanent_custodianship_necessary, empirically_contingent).
narrative_ontology:cs_axiom('e90d94a5-29a5-417c-a63f-7a30b6f9064f', foundational, civilian_autonomy_subordinate_to_security_judgment).
narrative_ontology:cs_axiom_status(civilian_autonomy_subordinate_to_security_judgment, holdable).
narrative_ontology:cs_axiom_grounding('e90d94a5-29a5-417c-a63f-7a30b6f9064f', civilian_autonomy_subordinate_to_security_judgment, instrumental).
narrative_ontology:cs_reference_frame('e90d94a5-29a5-417c-a63f-7a30b6f9064f', post_revolutionary_fragmentation_guard).
narrative_ontology:cs_drift_state('e90d94a5-29a5-417c-a63f-7a30b6f9064f', contemporary_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e90d94a5-29a5-417c-a63f-7a30b6f9064f', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_institutional_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislative_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislative_body).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_founding_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislative_body).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, military_guardian_stability_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, security_supremacy_over_civil_contestation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Charter's military-guardian clauses, retains veto authority over major political decisions framed as security-related, conducts internal security reviews of civilian institutions, and can dissolve legislatures or suspend constitutions when it deems the state endangered. Justifies this as a stabilizing function required by post-revolutionary fragility and external threats. Collects institutional power, budgetary autonomy, and immunity from civilian oversight.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate within bounds defined by military security vetting; cannot advocate policies the military labels destabilizing; face dissolution or arrest of leadership if the military deems their activity threatening. Exit means abandoning democratic participation or leaving the country. Identity-locked: political participation is constitutive of citizenship and party membership; departure from the political system is experienced as exile or loss of civic identity.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, identity_locked, national).

% Subject to military security monitoring and arrest for political organizing; campus activism is circumscribed by military-enforced campus security apparatus; student organizations require military approval to form. Exit options are limited: remaining in school means accepting constraints; dropping out forecloses educational advancement; emigration is available only to some. Coalition potential exists but faces rapid military suppression.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, biographical, constrained, local).

% Has formal legislative authority but faces military veto on security-classified matters (broadly defined to include labor policy, state enterprise management, foreign relations) and operational subordination to military-appointed security councils. Sees itself as the representative of popular will but operates under structural constraint that reserves ultimate authority to military judgment. Benefit: institutional legitimacy and continued operation; cost: decision autonomy severely curtailed.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislative_body, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_legislative_body, beneficiary).

% The military leadership that carried out the revolution and drafted the Charter. Justifies permanent military guardianship as the stabilizing force that prevented the revolution from fragmenting into civil war. Sees themselves as stewards of the revolutionary intent, protecting it from both internal factionalism and external subversion. Exit from this role would mean handing power to untested civilian institutions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_founding_authority, beneficiary,
    institutional, generational, analytical, national).

% Monitor whether the military custodian reading stabilizes or destabilizes the state; condition trade, aid, and security partnerships on the reading's operational trajectory. Some favor military stability; others condition support on civilian democratic transition. Their classification of the constraint (stabilizing coordination or destabilizing extraction) affects their policy stance and the constraint's international legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, external_state_actors, observer,
    powerful, generational, analytical, global).

% Detained under security laws that derive authority from the military-guardian reading; unable to participate in the constitutional conversation. Would testify that the reading operates as pure extraction if able to speak; their absence from the conversation means the arrangement is never tested against the voices it most constrains.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, imprisoned_political_dissidents, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_institutional_apparatus).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents fragmentation of the post-revolutionary state by centralizing security decision-making and veto authority in a single institutional apparatus oriented toward state preservation. Solves the coordination problem of how to avoid inter-factional civil war during the vulnerable consolidation phase following a transition.
% TRANSFER_FUNCTION: Transfers political decision autonomy from civilian bodies (legislatures, parties, civil society) to the military apparatus, and transfers deference/compliance from political actors to military authority. Extracts legitimacy from democratic structures while reserving ultimate authority to military judgment.
% ABSENT_VOICES: Imprisoned dissidents, banned organizations, and the student movement under surveillance cannot articulate their reading of the constraint. Those purged from the revolutionary coalition in the early consolidation phase are structurally excluded. If present, they would dispute the 'stability' framing and call the arrangement extraction by security apparatus rather than coordination for protection.
% DISAPPEARANCE_RATIONALE: If the military veto and guardian authority vanished overnight, civilian legislatures would expand their decision scope, political parties would operate without security vetting, student organizing would proceed without military surveillance, and the military would become subordinate to civilian command. The state would reorganize around civilian democratic authority; whether it would fragment (as the military argues) or stabilize (as democrats argue) is precisely the dispute the constraint brackets.
% FOUNDING_PROBLEM: Post-revolutionary state fragmentation: the revolution consolidated military power to prevent the emerging state from splintering into warring factions before institutions could stabilize. The military framing holds that civilian institutions lacked the unified authority to prevent civil war during the vulnerable transition period.
% FOUNDING_PROBLEM_CORROBORATION: The military and its civilian allies attest the fragmentation danger is ongoing, citing periodic factional tensions and external pressure. Opposition parties and international democracy advocates attest the danger has been substantially mitigated and the military custodianship now operates as entrenched extraction. Academic analyses and testimony from neighboring democracies that underwent similar transitions support the claim that the founding problem was real but time-limited; the persistence of military guardianship is better explained as institutional inertia and power preservation than as ongoing security necessity.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.55→0.78 over the interval) because the military's decision authority is decoupled from accountability to the civilian bodies it overrides. Suppression is even higher and steady (0.65→0.82) because the constraint's persistence depends on active policing of civilian organizing (student arrests, party vetting, dissidents detained). Theater ratio rises moderately (0.25→0.48) because institutional legitimacy ceremonies (legislative sessions, constitutional rhetoric about guardianship) persist but perform mainly to contain resistance rather than to solve the coordination problem the charter invokes. The measurement series on one shared time grid captures the constraint's lifecycle: early diffuse suppression (post-revolutionary vigilance) hardening into routine institutional dominance by interval end. Accessibility collapse is high (0.71) because exit from the political system means exile; alternatives to military-superintended governance exist theoretically but have been structurally foreclosed by the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   Military and revolutionary seats experience the constraint as coordination achieving what civilian democracy could not: unified decision-making preventing factionalism. Civilian and student seats experience it as institutional domination justified by invoked-but-diminishing security threats. The gap is not measurement error — it is structural asymmetry in how the same constraint is experienced. The engine computes this from stakeholder positions; the commentary explains why the divergence is real, not a classification mistake.
 *
 * DIRECTIONALITY LOGIC:
 *   Military apparatus: d near 0.0 (full beneficiary — sets rules, collects autonomy, low exit cost). Autonomous parties: d near 0.85 (high target — constrained participation, identity-locked, bear suppression cost). Student movement: d near 0.95 (full target — powerless, subject to surveillance, arrest, no effective exit). Civilian legislature: d near 0.65 (asymmetric payer — constrained decision authority but retains institutional legitimacy and operational continuity; exit would mean dissolution). Revolutionary authority: d near 0.2 (beneficiary — institutional preservation, ideological justification, but with some cost of legitimacy-management as the founding problem recedes).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is post-revolutionary state consolidation: preventing the revolution from fragmenting into civil war during the vulnerable transition. The founding_problem_status is contested: military says fragmentation risk is ongoing; others say it was real (t=0 to ~t=10) but has been substantially managed. The theater ratio rising (0.25→0.48) suggests performative maintenance increasing: military-guardian rhetoric persists beyond the coordination necessity that justified it. A tangled_rope rather than a snare because genuine coordination function (preventing state collapse) is real and remains, even though institutional capture and inertia now sustain it. If the founding problem is dead (opposite reading: secular_democratic_reading), the constraint should reclassify to pure extraction (snare) — but that is a different reading's computation. This reading holds the founding problem as live-but-contested, which supports tangled_rope: the coordination story is true but no longer the primary sustaining force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the post-revolutionary fragmentation danger still live, or has it been substantially mitigated by institutional consolidation?',
    'Empirical: comparative analysis of state capacity, institutional cohesion, and civil-war risk metrics at t=0 vs. t=current; testimony from analysts without institutional stake in military guardianship. Counterfactual: what would happen if military veto were removed tomorrow — would state fragment or stabilize?',
    'If the problem is dead, the constraint reclassifies from tangled_rope (real coordination + institutional capture) to snare (pure extraction). If the problem is live, tangled_rope holds and mandatrophy is not resolved. Contested status reflects the genuine empirical uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem remains live or has been substantially solved.').

omega_variable(
    institutional_capture_vs_necessity,
    'Does the military''s institutional dominance persist because the coordination problem requires it, or because the institutional beneficiary (military apparatus) controls the framing of what ''necessity'' means?',
    'Institutional analysis: compare the military''s stated security requirements against actual threats; analyze budget allocations and decision patterns to distinguish necessary functions from power-preservation functions. Comparative: how do similar post-revolutionary states manage military-civilian civil-military relations with civilian primacy?',
    'If institutional capture is the primary driver (over coordination necessity), the constraint should be reclassified as snare or demoted to piton (theatrical guardianship). If coordination necessity is primary, tangled_rope classification is robust. The rising theater_ratio suggests capture is increasing, but does not settle whether coordination necessity is also present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_necessity, empirical, 'Whether military dominance is necessary for state stability or has become self-perpetuating institutional inertia.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.82) structural (external barriers: arrest, detention, vetting authority) or internalized (target agents believe military guardianship is legitimate or necessary)?',
    'Post-escape or emigration analysis: do political actors who leave the jurisdiction continue to accept the military-guardian framing, or do they renounce it? Survey evidence from civilian elites on whether they accept the military framing as legitimate or see it as imposed. Comparison with periods of reduced suppression: if suppression were lifted, would civilian institutions accept military guardianship or move to subordinate it?',
    'If suppression is primarily structural (external barriers), removing enforcement would likely enable civilian institutional expansion. If internalized (actors believe the framing), institutional change would require both barrier removal and legitimacy shift. This affects what a transition away from military guardianship would require.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression operates through external barriers or internalized acceptance of military legitimacy.').

omega_variable(
    alternative_readings_of_same_kernel,
    'This constraint instantiates ONE reading of the july_charter_sovereign_legitimacy kernel. The secular_democratic_reading frames the same Charter as subordinating military to civilian authority; the guided_nationalism_reading grounds legitimacy in Islamic-national identity. What would the constraint''s ε, beneficiary structure, and type be under those alternative readings?',
    'Generate the sibling readings as separate constraint stories; compare their ε and type values. The three readings will have different ε values because they measure different aspects of Charter authority (military primacy vs. civilian supremacy vs. Islamic legitimacy). This is NOT measurement error — it is ε-invariance: different claims have different extractiveness structures.',
    'The three readings together form a constraint family. The military_custodian_reading''s tangled_rope classification may diverge from the other readings'' classifications (secular_democratic_reading likely computes as snare; guided_nationalism_reading may compute as rope with different beneficiary structure). The corpus needs all three stories to understand how the Charter is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_of_same_kernel, conceptual, 'This constraint is one reading of a three-way contest; sibling readings have different structural properties and will produce different engine classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(july_tr_t5, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(july_tr_t15, observed).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(july_tr_t25, observed).
narrative_ontology:measurement(july_tr_t35, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement_basis(july_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(july_be_t5, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(july_be_t15, observed).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(july_be_t25, observed).
narrative_ontology:measurement(july_be_t35, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(july_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(july_su_t5, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(july_su_t15, observed).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(july_su_t25, observed).
narrative_ontology:measurement(july_su_t35, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 35, 0.82).
narrative_ontology:measurement_basis(july_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the military_custodian_reading of the july_charter_sovereign_legitimacy kernel. The same Charter text is interpreted differently by three distinct readings, each with its own ε, beneficiary structure, and type. The secular_democratic_reading frames the Charter as subordinating military to civilian authority (snare-type, different beneficiary structure). The guided_nationalism_reading grounds legitimacy in Islamic-national identity (rope-type, identity-coordination). All three readings share the same formal text and institutional domain but instantiate different constraints because they identify different victims, beneficiaries, and extraction structures. The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
