% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: State Border Authority and Sovereign Exclusion (Sovereignty-Primary Reading)
 *   domain: political/legal
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   border-normative-status kernel: the sovereignty-primary reading, which
 *   asserts that territorial boundaries are legitimate instruments of
 *   collective self-determination and that states have foundational authority
 *   to exclude non-members. This is distinct from the freedom-primary reading
 *   (which treats freedom of movement as fundamental and borders as
 *   presumptively unjust) and the qualified-sovereignty reading (which
 *   acknowledges state authority but requires proportionality and
 *   human-rights consistency). The sovereignty-primary reading is authored as
 *   TANGLED ROPE: it performs a genuine coordination function (enabling
 *   bounded collective governance) AND structures asymmetric extraction
 *   (excluded migrants and displaced populations pay the cost; citizens and
 *   state institutions collect the benefit). The constraint is actively
 *   enforced (border guards, immigration law, deportation machinery) and its
 *   persistence depends on suppression of alternative framings. The
 *   measurement series show extractiveness and theater both rising over the
 *   interval, indicating the constraint's justificatory content is being
 *   increasingly performative while extraction grows — a piton-adjacent
 *   pattern where the coordination story carries more of the load as the
 *   genuine function atrophies.
 *
 * KEY AGENTS:
 *   - citizen_beneficiary_bloc: Primary beneficiary. Organized power, constrained exit (emigration is costly). Benefit from preferential access to territory and institutions; their self-determination is the constraint's framing justification.
 *   - state_institutional_apparatus: Agenda-setter. Institutional power, analytical exit. Administers exclusion, collects legitimacy and authority. The constraint serves the state's primary organizing principle.
 *   - excluded_migrants: Primary victim. Powerless, trapped. Denied access to territory and its institutional goods. Bear all costs of exclusion; no voice in rule-setting.
 *   - displaced_populations: Secondary victim. Powerless, identity-locked. Territorial attachment fused with identity; displacement is treated as justifiable state function. Unable to exit the constraint without radical identity reconstruction.
 *   - human_rights_authority_sector: Excluded (structurally outside rule-setting). Would contest the constraint's legitimacy frame. Their absence is maintained by the constraint itself.
 *   - neighboring_states: Observational. Benefit from sovereignty-primary reading (legitimates their own borders) but bear diffuse costs (refugee populations, border management burden).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.62).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.71).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "State Border Authority and Sovereign Exclusion (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political/legal").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'aeb1b7f3-e578-4ab5-b103-9e8a77e94d29').
narrative_ontology:cs_kernel_codification('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', formalized).
narrative_ontology:cs_authority_grounding('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', extraction).
narrative_ontology:cs_interpretation_layer_present('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29').
narrative_ontology:cs_reading_relation('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', foundational, state_authority_foundational_for_self_determination).
narrative_ontology:cs_axiom_status(state_authority_foundational_for_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', state_authority_foundational_for_self_determination, deontological).
narrative_ontology:cs_axiom('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', foundational, territorial_boundary_legitimacy_derives_from_membership_authority).
narrative_ontology:cs_axiom_status(territorial_boundary_legitimacy_derives_from_membership_authority, holdable).
narrative_ontology:cs_axiom_grounding('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', territorial_boundary_legitimacy_derives_from_membership_authority, instrumental).
narrative_ontology:cs_reference_frame('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', state_territorial_sovereignty_doctrine).
narrative_ontology:cs_drift_state('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aeb1b7f3-e578-4ab5-b103-9e8a77e94d29', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_beneficiary_bloc).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_institutional_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, displaced_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens of an established state who benefit from the constraint's operation: preferential access to territory, institutions, labor markets, welfare provision, and political voice. The constraint protects their collective self-determination claim and prioritizes their membership status. Exit involves emigration or renunciation, both costly in opportunity and identity.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_beneficiary_bloc, beneficiary,
    organized, generational, constrained, national).

% The executive and enforcement machinery (immigration authorities, border guards, policy courts) that administers and enforces the exclusion regime. Sets enforcement intensity, interprets the boundary, determines admission criteria, and justifies the arrangement as legitimate state function. Collects legitimacy and authority from the constraint.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_institutional_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and families outside the bounded territory who seek entry but are denied access by border enforcement. They bear the costs of exclusion: opportunity foregone (economic, educational, familial), physical danger at borders or in transit, indefinite legal precarity. Exit from the constraint requires successful entry (blocked) or acceptance of permanent exclusion.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens or former residents whose displacement from territory is treated as a legitimate byproduct of state sovereignty and border enforcement (refugees, internally displaced, those whose claims to belonging within the boundary have been severed). Identity fused with territorial attachment; exit from the constraint requires either regained access (unlikely) or radical identity reconstruction.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, displaced_populations, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, displaced_populations, observer).

% The system of nation-state sovereignty and territorially bounded governance is vindicated by the constraint's operation and legitimacy claim. The constraint operationalizes the doctrine that states are the primary units of legitimate political authority and that territorial self-determination is the appropriate locus of collective decision-making. This is a proposition, not an actor.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, liberal_international_order, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(border_normative_status__sovereignty_primary, liberal_international_order).

% International human rights bodies, NGOs, and advocates that contest the constraint's legitimacy frame and argue for recognition of migrants' human dignity and freedom of movement claims. They are structurally excluded from the enforcement machinery — their voice enters only as external pressure and dissent, not as a seat at the rule-setting table. Their participation would fundamentally alter the constraint's claimed scope.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_authority_sector, excluded,
    moderate, generational, constrained, global).

% Adjacent states that experience outflows of excluded migrants, political pressure from refugee populations, and their own border enforcement costs. They benefit from the sovereignty-primary reading insofar as it legitimates their own exclusionary borders, but bear costs from receiving populations that lack onward access. Their role is observational within this particular constraint but agenda-setting in parallel border regimes of their own.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, neighboring_states, observer,
    institutional, generational, constrained, national).

% Scholarly and jurisprudential observers examining the constraint's structural properties, legitimacy claims, and real-world operation independent of commitment to the sovereignty-primary reading.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, analytical_observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, state_institutional_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable territorial framework within which collective self-governance, mutual obligation, and institutional capacity can be organized. The constraint solves a genuine coordination problem: absent bounded membership, the distribution of resource access, welfare provision, and political voice becomes under-determined. Citizenship and territorial sovereignty create a frame for resolving collective-action problems that would otherwise remain deadlocked.
% TRANSFER_FUNCTION: Moves access to territory, institutional goods (rule of law, education, healthcare, welfare, political voice), and economic opportunity FROM excluded populations TO citizen-members and the state apparatus. Excluded migrants and displaced populations pay the cost; citizens and state institutions collect the benefit. The constraint is the mechanism that enables this transfer by treating exclusion as legitimate.
% ABSENT_VOICES: Human rights advocates, migrant organizations, and displaced populations themselves are structurally excluded from setting the rule. They would argue that freedom of movement is a fundamental right and that exclusion mechanisms rest on contingent power rather than genuine legitimacy. International human rights authorities contest the sovereignty-primary frame. Their absence from the agenda-setting seat is structural — the constraint exists partly to keep their claims out of the rule-determining process.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — if territorial boundaries ceased to be recognized as legitimate instruments of sovereign exclusion — the entire apparatus of nation-state welfare provision, citizenship regimes, and bounded political communities would face immediate reorganization. States would lose a primary justification for exclusion; resource access and institutional membership would become global-commons problems rather than state-managed allocations; political authority would fragment or reorganize around alternative principles. The modern state system itself is structurally dependent on the legitimacy of this constraint.
% FOUNDING_PROBLEM: The founding problem is the establishment of a just frame for collective self-governance in a world of scarce resources and diverse populations with conflicting claims to territory. The sovereignty-primary reading asserts that bounded states with authority to exclude are the solution: they enable communities to form binding agreements, accumulate institutional capacity, and distribute goods without perpetual contestation over membership terms.
% FOUNDING_PROBLEM_CORROBORATION: The state institutional apparatus and citizen beneficiary blocs attest the founding problem remains live and the solution indispensable. Political philosophers in the state-sovereignty tradition (e.g., Rawls's law of peoples, Miller on national self-determination) corroborate from outside the immediate benefiting parties. However, alternative reading advocates (freedom-primary and qualified-sovereignty traditions) and human rights authorities contest whether the founding problem is accurately characterized and whether the constraint's operation solves it justly. The corroboration is mixed and comes from partisan philosophical schools, not neutral external observation.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects asymmetric benefit distribution: citizens and state apparatus benefit substantially, excluded migrants and displaced populations pay entirely. The extracted value is access to territory and institutional goods; the mechanism is the state's exclusive authority to set membership terms. Suppression (0.71) is high because the constraint's persistence depends on actively defending the exclusion mechanism against competing framings — border enforcement, immigration law, and the sovereignty doctrine itself constitute the suppressive infrastructure. Theater (0.42) is moderate-high and rising, indicating an increasing share of enforcement activity is devoted to justifying the arrangement (international law, humanitarian exception-making, 'border security' framing) rather than purely mechanical exclusion. The measurement series show all three metrics rising together, with suppression outpacing extractiveness — this pattern is characteristic of constraints that begin with coordination function but increasingly rely on justificatory performance as alternatives emerge. The interval reflects the modern period of state-system consolidation through the rise of human-rights contestation (t=0 roughly 1945, t=25 roughly 2020).
 *
 * PERSPECTIVAL GAP:
 *   From the state institutional seat and citizen beneficiary seats, the constraint is genuine coordination: bounded membership enables welfare provision, mutual obligation, and self-governance that would otherwise be impossible. From the excluded-migrant seat, the same structure is pure extraction defended by power: the state invented the boundary and now uses it to deny access to resources and opportunities. From the human-rights authority seat, the constraint is foundationally unjust — freedom of movement is a human right that the state violates by treating its own boundaries as legitimate. The engine computes these divergences from the structural data (beneficiary/victim declarations + exit options + power atoms). The sovereignty-primary reading does not adjudicate between them; it is one framing among contested alternatives. Seat divergence here is maximal: the same rule is liberation (for citizens), legitimate state function (for the apparatus), and injustice (for excluded migrants) simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations and exit options. Citizens and state apparatus: beneficiaries with constrained/analytical exit → d near 0.0 (beneficiary end). Excluded migrants: victims with trapped exit → d = 1.0 (full target). Displaced populations: victims with identity-locked exit (territorial attachment fused with identity) → d = 0.95 (near-total target). Human rights authority: excluded stakeholder with moderate power and constrained exit → d = 0.65 (observer seat leaning toward target on principle, but analytical exit prevents full targeting). No directionality overrides are needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early signs of mandatrophy (founding problem dead, arrangement persists). The founding problem was enabling stable collective governance in a world of scarce resources and conflicting claims. That problem is substantially solved for established states: modern states have functioning institutions, stable borders, and stable resource allocation mechanisms. Yet the extraction machinery persists and intensifies. The theater ratio (rising from 0.25 to 0.42) indicates increasing justificatory load: the constraint must work harder to maintain its legitimacy claim because the founding problem's salience has declined. The suppression requirement (rising from 0.54 to 0.71) shows enforcement costs are increasing — more active work is required to maintain the boundary against contestation. The extractiveness rising alongside these suggests the constraint is increasingly about rent collection (state institutional control of access to territory) and decreasingly about solving a coordination problem. This is the classic mandatrophy signature: founding function atrophied, enforcement and justification intensified, extraction persists because institutional actors benefit. However, the constraint is NOT yet a piton because the coordination function is not negligible — it remains genuinely necessary for state welfare provision and political voice organization, even if the current extraction exceeds what coordination costs require. It sits at the boundary between Tangled Rope and Piton, with rising theater indicating drift toward Piton over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_constructed_boundary,
    'Are territorial boundaries and the state sovereignty system natural features of human political organization, or are they constructed institutional arrangements that benefit identifiable parties?',
    'Historical anthropology and institutional genealogy: tracing the contingent emergence of the nation-state system and the boundary regime, identifying decision points where alternatives existed, and examining what would be required to unmake the system.',
    'If boundaries are constructed, the constraint shifts structurally toward snare (extraction mechanism serving institutional and citizen interests, defended by selective naturalization claims). If boundaries emerge naturally from human social organization, the constraint retains mountain character (even if some parties benefit, the core function is unavoidable). The omegas_c framing ambiguity: is state sovereignty a discovery of legitimate order, or an invention that serves some at the expense of others?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_boundary, conceptual, 'Whether the nation-state boundary system is natural or constructed.').

omega_variable(
    alternative_coordination_frames,
    'Could collective self-governance and institutional capacity be organized through mechanisms other than territorial exclusion? What would be required?',
    'Examination of historical and contemporary federated, multi-level, and post-national governance experiments (EU, indigenous governance networks, cosmopolitan institutional designs) to assess whether coordination functions attributed to state boundaries could be delivered through alternative frames.',
    'If robust alternatives exist, the constraint''s claim to indispensability weakens and its extraction becomes more clearly unnecessary — the coordination function could be separated from the exclusion mechanism. If alternatives consistently fail or require infeasible preconditions, the sovereignty-primary reading''s coordination defense strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_frames, empirical, 'Whether boundary-based coordination is the only viable frame or one among alternatives.').

omega_variable(
    identity_fusion_suppression_mechanism,
    'Is the suppression of excluded migrants'' claims grounded structurally (economic barriers, legal barriers, geographic isolation) or internalized (migrants have internalized exclusion as justified, states have fused their authority claims with sovereignty doctrine)?',
    'Post-integration trajectory studies: when migrants gain access despite the constraint, do suppression patterns persist? If suppression remains internalized (migrants continue to defer to sovereignty claims, or states maintain authority claims even in integrated populations), the mechanism is partly identity-fused. If suppression dissolves with material access, the mechanism is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the constraint persists partly through internalization of state authority claims as legitimate. This would indicate stronger identity-coordination (in the Boltzmann sense) layered underneath the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_suppression_mechanism, empirical, 'Whether border suppression operates structurally or through internalized deference to sovereignty claims.').

omega_variable(
    kernel_reading_foreclosure_risk,
    'Does the sovereignty-primary reading logically foreclose the freedom-primary reading, or do they remain live alternative positions that can coexist across different institutional commitments?',
    'Axiomatic analysis: if the core premise of sovereignty-primary (states have foundational authority to exclude) and freedom-primary (freedom of movement is fundamental and borders are presumptively unjust) are each examined for internal coherence and mutual exclusion, does one logically eliminate the other? Or do they rest on different value hierarchies (collective self-determination vs. individual liberty) that cannot be ranked without introducing preference?',
    'If foreclosure holds, the kernel contest is resolvable by logic and evidence. If coexistence holds, the contest is between live alternative political commitments, and the outcome depends on institutional power, not truth. This is the deepest ambiguity in the constraint structure — it determines whether we are measuring discovery or power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_risk, conceptual, 'Whether competing readings of the border-sovereignty kernel are logically exclusive or value-incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_normative_status__sovereignty_primary, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__sovereignty_primary, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_normative_status__sovereignty_primary, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__sovereignty_primary, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_normative_status__sovereignty_primary, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(bord_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_normative_status__sovereignty_primary, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_normative_status__sovereignty_primary, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_normative_status__sovereignty_primary, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_normative_status__sovereignty_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_normative_status__sovereignty_primary, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(bord_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_normative_status__sovereignty_primary, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_normative_status__sovereignty_primary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_normative_status__sovereignty_primary, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_normative_status__sovereignty_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_normative_status__sovereignty_primary, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(bord_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_normative_status__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, national_citizenship_access_regime).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, welfare_state_membership_boundary).

% DUAL FORMULATION NOTE:
% The border-normative-status kernel decomposes into three readings distinguished by their core legitimacy claim. sovereignty_primary asserts state authority to exclude is foundational; freedom_primary asserts freedom of movement is foundational; qualified_sovereignty asserts authority exists but must be constrained by proportionality and rights. The ε values differ substantially: sovereignty_primary moderate-high (0.62) with coordination justification; freedom_primary high (0.75+) with minimal coordination; qualified_sovereignty lower (0.40-0.45) because rights-protections reduce extraction. Each reading produces different victim sets (sovereignty-primary includes excluded migrants and displaced populations; freedom-primary centers excluded migrants; qualified-sovereignty minimizes victim set). Link all three via network.affects_constraints in both directions to enable cross-reading analysis and contamination-drift modeling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
