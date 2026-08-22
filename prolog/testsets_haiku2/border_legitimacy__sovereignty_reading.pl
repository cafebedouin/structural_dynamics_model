% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority via Territorial Sovereignty (Sovereignty Reading)
 *   domain: political/international law/migration
 *
 * SUMMARY:
 *   This is the sovereignty reading of the border legitimacy kernel: the
 *   constraint that state authority to exclude derives from territorial
 *   sovereignty — the state owns/controls territory and therefore has the
 *   legitimate right to determine who enters and resides. This reading
 *   grounds border control in political self-determination and collective
 *   membership rather than in humanitarian obligation or human rights to
 *   movement. The constraint is CLAIMED as tangled_rope (genuine coordination
 *   function for state-system members + asymmetric extraction from those
 *   excluded) and the metrics are authored to reflect the reading's own
 *   assessment: from the sovereignty perspective, the constraint is
 *   legitimate enforcement of a real coordination problem; from the
 *   perspective of excluded migrants, it is substantive extraction. The
 *   kernel contest includes two sibling readings with different referents and
 *   different victim sets — the engine measures them as separate constraints.
 *   This story traces only the sovereignty reading's internal structure.
 *
 * KEY AGENTS:
 *   - state_apparatus: Institutional agenda-setter exercising border control authority
 *   - incumbent_citizens: Organized beneficiaries protected by membership boundaries
 *   - excluded_migrants: Powerless victims denied entry and residence
 *   - asylum_seekers: Powerless, identity-locked payers facing deportation or exclusion
 *   - receiving_state_competitors: Institutional beneficiaries of sovereignty doctrine
 *   - international_human_rights_bodies: Institutional observers with no enforcement power
 *   - origin_states: Moderate-power payers whose citizens are excluded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.72).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority via Territorial Sovereignty (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political/international law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '97d58c01-9101-4342-8186-01917f121b65').
narrative_ontology:cs_kernel_codification('97d58c01-9101-4342-8186-01917f121b65', formalized).
narrative_ontology:cs_authority_grounding('97d58c01-9101-4342-8186-01917f121b65', lineage).
narrative_ontology:cs_interpretation_layer_present('97d58c01-9101-4342-8186-01917f121b65').
narrative_ontology:cs_reading_relation('97d58c01-9101-4342-8186-01917f121b65', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('97d58c01-9101-4342-8186-01917f121b65', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('97d58c01-9101-4342-8186-01917f121b65', foundational, territorial_control_entails_exclusionary_prerogative).
narrative_ontology:cs_axiom_status(territorial_control_entails_exclusionary_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('97d58c01-9101-4342-8186-01917f121b65', territorial_control_entails_exclusionary_prerogative, conventional).
narrative_ontology:cs_axiom('97d58c01-9101-4342-8186-01917f121b65', foundational, state_membership_is_basis_of_rights).
narrative_ontology:cs_axiom_status(state_membership_is_basis_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('97d58c01-9101-4342-8186-01917f121b65', state_membership_is_basis_of_rights, deontological).
narrative_ontology:cs_reference_frame('97d58c01-9101-4342-8186-01917f121b65', westphalian_sovereignty).
narrative_ontology:cs_drift_state('97d58c01-9101-4342-8186-01917f121b65', contemporary_migration_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97d58c01-9101-4342-8186-01917f121b65', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, receiving_state_competitors).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, origin_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the border authority: defines entry criteria, operates immigration law, maintains enforcement machinery (patrol, detention, deportation). Justifies exclusion by territorial sovereignty doctrine. Collects legitimacy and political authority directly from border control — the right to exclude IS what makes the state a recognized sovereign in the international system. Could change the constraint but would forfeit recognized sovereignty.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Secure membership in the political community through borders. Protected from replacement in labor markets, shielded from rapid demographic change, guaranteed access to welfare state resources and political voice. Maintain cultural reproduction and collective identity. Can leave and return as citizens; retain rights elsewhere or acquire dual citizenship. High exit options relative to excluded migrants.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, incumbent_citizens, beneficiary,
    organized, generational, mobile, national).

% Denied entry and residence. Face detention, deportation, repeated exclusion. Trapped between origin states (where conditions motivated migration) and receiving states (that exclude them). Cannot access labor, education, healthcare, political voice in desired territory. No capacity to negotiate or challenge the constraint. Alternatives are constrained to countries willing to admit them, often with similarly poor conditions or even more danger. Directionality override to 0.95 reflects extreme asymmetry: pure target, zero negotiating power.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution, war, or disaster and seek refuge under international law. Under sovereignty reading, they have no presumptive claim — need does not override state prerogative. Face same border machinery as economic migrants. Identity-locked: their status as 'refugee' exists only as a category the state-system recognizes or denies. Cannot return home (persecution continues) and cannot exit into legitimacy (no state grants it). Locked into dependency on the system that excludes them.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, asylum_seekers, excluded).

% Other sovereign states benefit from the sovereignty doctrine itself — it legitimates their own border control and territorial authority. Mutual recognition of sovereignty is the foundation of international law. They compete in a system where border control is the standard mechanism for managing membership. Any state that relinquished borders would lose sovereignty recognition and institutional standing. Locked into collective benefit from the sovereignty doctrine.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_competitors, beneficiary,
    institutional, generational, analytical, global).

% Monitor border practices against human rights frameworks. From sovereignty reading's perspective, they lack authority to override state prerogative. Issue recommendations with persuasive but not binding force. Document violations and maintain alternative framings but structurally excluded from enforcement. Cannot compel change; can only bear witness and build pressure for voluntary compliance.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Bear the cost of being source of excluded migrants: loss of labor, loss of economic remittances that migrants would send, pressure on domestic resources from populations that cannot emigrate. Citizens' movement is constrained by receiving states' sovereignty; origin states have no remedy under sovereignty doctrine except to compete for receiving countries' favor through trade, diplomacy, or aid. Structurally asymmetric position: cannot require receiving states to admit their citizens.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, origin_states, payer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system of defined membership and territorial control: states coordinate on recognizing each other's borders and respecting each other's right to exclude non-members. This solves the problem of how to maintain distinct political communities with separate governance, welfare systems, and cultural reproduction in a world of migration pressure.
% TRANSFER_FUNCTION: Moves the right to residence, labor access, welfare benefits, and political voice from migrants to incumbent citizens and the state apparatus. The constraint transfers from those seeking entry to those who control territory: the excluded bear the cost of non-membership; the incumbent benefit from the exclusion that preserves their privileged position.
% ABSENT_VOICES: Excluded migrants themselves are structurally locked out of the conversation that determines the rules governing their exclusion. Their voice enters only as object (data about threat or need) not as subject (claim-maker). Origin-state representatives and international human rights advocates speak for them but are themselves excluded from enforcement decisions. The reading itself forecloses their voice by treating sovereignty as prior to any claim they might make.
% DISAPPEARANCE_RATIONALE: If border authority via sovereignty doctrine disappeared, the state-system's fundamental organizing principle would collapse. Political communities would lose the mechanism to maintain distinct membership, welfare systems could not protect resource distribution, and the territorial basis of governance would dissolve. Global reorganization would follow: either toward open movement (eliminating the border), toward imperial absorption (eliminating the state-system), or toward chaos (eliminating coordinated governance). The world does not remain remotely the same.
% FOUNDING_PROBLEM: How can distinct political communities with separate governance structures and resource distribution systems coexist in a world of migration pressure, where some people wish to move across territorial boundaries?
% FOUNDING_PROBLEM_CORROBORATION: State apparatus explicitly attests the founding problem remains live — migration pressure is undiminished, political communities require membership boundaries to function, welfare systems are threatened by open entry. International law scholars and state representatives from every region corroborate that the state-system's persistence depends on border control. This reading has the broad institutional consensus of state practice; the corroboration is unanimous among power-holders, which is precisely what critics of this reading identify as the problem.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.72 over the interval as migration pressure increases and states respond with stricter enforcement, making exclusion more costly and more actively maintained. The measurement grid captures a shift from baseline enforcement to enforcement intensification. Theater rises from 0.28 to 0.41 because the state increasingly deploys humanitarian language and development aid rhetoric to justify exclusion while enforcement becomes more visible and harsh — the performance of legitimacy increases as the raw enforcement cost becomes harder to conceal. Suppression requirement rises from 0.64 to 0.78 because excluded migrants and asylum advocates mount increasing resistance as the consequences of exclusion sharpen (climate displacement, violence, economic desperation), forcing the state to expand enforcement infrastructure to maintain the boundary. The shared time grid aligns all three metrics at every point: the constraint operates as enforced extraction requiring increasing coercive machinery to maintain, not as voluntary coordination.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and incumbent citizens perceive this constraint as coordination: establishing membership, protecting political community, enabling welfare distribution. The excluded and asylum seekers perceive it as extraction: denial of opportunity, coercive confinement, punishment for birth in the wrong place. The sovereignty reading makes the state's perception the ground truth (prerogative, not harm); the sibling readings reverse this (harm, not prerogative). The engine computes per-seat types from structural data — the payer seats should compute as snare or high-extraction tangled_rope, the beneficiary seats as genuine coordination. That divergence reflects the reading's own internal inconsistency: it claims tangled_rope (mixed) but the metrics and victim set suggest the reading describes asymmetric extraction. The inconsistency is NOT an error; it is the reading's structural signature.
 *
 * DIRECTIONALITY LOGIC:
 *   From the sovereignty reading's internal logic, the state and incumbent citizens are beneficiaries (they receive the coordination benefit of stable membership and resource control). Excluded migrants and asylum seekers are targets at the far extraction end: they bear the full cost of exclusion with minimal exit options (trapped or identity-locked) and powerless position. Origin states are partially-trapped payers — they have moderate power but constrained exit; their citizens are excluded by other states' sovereignty, creating a structural asymmetry. The reading itself does not dispute that migrants bear costs; it argues that those costs are legitimate consequences of state prerogative, not injustice. The engine computes different directionalities for different seats from this structural data — the payer seats experience high extraction, the beneficiary seats low or negative extraction (subsidy). The claim/metric independence: the claim is that this constraint is legitimate coordination (tangled_rope, not snare); the metrics describe substantially extractive, actively enforced operation. That gap is where the reading's contestation lives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live under this reading — distinct political communities do require membership boundaries to function in a world of migration pressure. However, the resistance measurement rising to 0.59 and suppression requirement rising to 0.78 suggest the founding problem's solution is increasingly contested. An omega (below) documents whether the constraint persists because the founding problem remains acute or because the sovereignty doctrine has calcified as institutional inertia despite the founding problem's solution. The theater rising to 0.41 indicates increasing performative deployment of legitimacy language while enforcement becomes more visible, a signature of mandatrophy — the constraint may persist because it serves incumbent interests (extraction), not because it solves a genuine coordination problem anymore.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_doctrine_grounding,
    'Does state sovereignty over territory logically entail the right to exclude non-members, or is sovereignty compatible with open borders under a different theory of the state''s authority?',
    'Jurisprudential analysis of whether exclusion follows from sovereignty as a necessary condition or as a contingent policy choice justified by other grounds (national security, welfare protection, cultural preservation). Examine whether any major legal tradition separates territorial control from exclusionary prerogative.',
    'If exclusion is logically entailed by sovereignty, then competing readings must deny either sovereignty or its applicability to borders. If exclusion is contingent, then the sovereignty reading is underdetermined — it would need additional justification beyond the territorial principle alone, collapsing it into a hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_doctrine_grounding, conceptual, 'Whether territorial sovereignty logically entails exclusionary prerogative or whether that link is contingent.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem — maintaining distinct political communities with separate governance and resource distribution — persist acutely, or has the constraint''s persistence become detached from solving that problem and now serves incumbent interests (extraction)?',
    'Empirical analysis: compare welfare-state sustainability and political community stability in jurisdictions with strict vs. moderate border enforcement. Examine whether enforcement intensity tracks migration pressure or incumbent political preference. Analyze whether the constraints imposed on asylum seekers exceed what the founding problem requires.',
    'If the founding problem persists acutely, the constraint is tangled_rope (genuine coordination + asymmetric extraction). If the founding problem is substantially solved and enforcement persists for incumbents'' benefit, the constraint reclassifies toward snare (pure extraction with a coordination cover story). This resolves whether the constraint is legitimately tangled or fraudulently snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the sovereignty reading''s founding problem remains the true driver of border enforcement or whether enforcement has become detached and purely extractive.').

omega_variable(
    identity_lock_mechanism_asylum_seekers,
    'For asylum seekers, is the exit option truly identity-locked (the state-system that creates refugees is the only path to legitimate refuge status) or are there genuine alternatives (UNHCR camps, third-country resettlement, return and internal displacement)?',
    'Ethnographic and legal analysis of asylum seekers'' actual option sets: can they secure protection without state recognition? What happens to those who reject recognition? Are third-country alternatives viable or are they constrained by receiving states'' sovereignty?',
    'If identity-locked is accurate, the constraint carries a distinctive suppression signature — the target''s very identity is constituted through the system excluding them, deepening psychological and institutional suppression. If alternatives exist but are constrained, the exit is constrained rather than identity-locked, which lowers suppression and suggests the constraint is more purely extractive (easier to exit, but exit carries severe costs). The classification should shift toward snare if identity-lock dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_asylum_seekers, empirical, 'Whether asylum seekers'' exit options are structurally identity-locked or constrained, and whether this affects the suppression mechanism''s depth.').

omega_variable(
    incumbent_citizen_benefit_distribution,
    'Do all incumbent citizens benefit equally from border control, or is the benefit concentrated in particular income/skill groups while others bear costs?',
    'Economic analysis of border effects on labor market by skill level, wage impact by sector, welfare access by income quintile. Examine whether low-skill incumbents bear wage competition from migrants or whether the constraint protects them. Examine whether the constraint enables middle-class welfare access while low-income incumbents pay through wage suppression in non-tradeable sectors.',
    'If benefit is distributed widely among incumbents, the beneficiary role is accurate and the constraint is tangled_rope from that seat''s perspective. If benefit is concentrated (wealthy citizens benefit, low-income bear net costs through wage suppression), then a significant portion of ''beneficiary'' incumbents are actually partially-trapped payers. The constraint could be a snare targeting low-skill incumbents disguised as coordination protecting the whole community.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_citizen_benefit_distribution, empirical, 'Whether incumbent-citizen benefits from border control are widely distributed or concentrated, affecting the accuracy of the beneficiary classification.').

omega_variable(
    reading_foreclosure_test,
    'Does the sovereignty reading''s core assertion — territorial control entails exclusionary prerogative — logically foreclose the humanitarian_obligation and freedom_of_movement readings, or do all three remain live positions within the state-system?',
    'Jurisprudential test: can a state simultaneously hold that it has sovereignty (and therefore can exclude), that it has humanitarian obligations (and therefore must admit persecuted), and that movement is a right (and therefore should not be restricted)? If yes, all three coexist; if any two are logically inconsistent in a single framework, declare foreclosure between them.',
    'If all three readings coexist within different states'' legal frameworks simultaneously, mark all relations as coexists_with. If any reading logically forecloses another (e.g., if absolute human rights to movement truly forecloses state sovereignty), mark forecloses and revise the axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the sovereignty reading''s core premise logically forecloses the sibling readings or whether all remain live positions in the state-system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__sovereignty_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__sovereignty_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__sovereignty_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(bord_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__sovereignty_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__sovereignty_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__sovereignty_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__sovereignty_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__sovereignty_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__sovereignty_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(bord_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__sovereignty_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__sovereignty_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__sovereignty_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__sovereignty_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__sovereignty_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__sovereignty_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(bord_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three constraint stories, each instantiating a different reading of territorial sovereignty and its implications for border authority. The sovereignty_reading (this file) grounds border authority in state territorial control and privileges incumbent citizens and the state apparatus. The freedom_of_movement_reading grounds borders in restriction of a human right and privileges migrants and movement advocates. The humanitarian_obligation_reading grants sovereignty but conditions it on duties to admit the persecuted. Each reading has a distinct epsilon (referent is the same standing arrangement — territorial border control — but assessed by each reading's own lights), distinct victims and beneficiaries, and distinct ε because the readings disagree on what extraction IS. The three are linked via network.affects_constraints because sibling readings compete to interpret the same kernel; a shift in one reading's institutional power affects the others' operating conditions. Do not collapse them into one story with measurement-dependent classification — they are three separate constraints with three separate omegas addressing the reading-indeterminacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
