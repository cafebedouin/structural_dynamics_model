% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA Binding Multilateral Treaty Constraint (Binding Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action) signed in 2015 by Iran,
 *   the P5+1, and the EU is contested as either a binding multilateral treaty
 *   or a transactional provisional framework. This story instantiates the
 *   BINDING MULTILATERAL READING: the agreement constitutes binding
 *   international law constraining all parties including powerful
 *   signatories; unilateral withdrawal is legally non-binding on other
 *   parties; sanctions relief modifications require multilateral consensus;
 *   Iranian compliance breaches trigger mandatory dispute resolution, not
 *   unilateral snapback. Under this reading, the constraint is substantively
 *   extractive for Iran (locked into compliance and contingent relief) and
 *   moderately constraining for the US (cannot unilaterally exit without
 *   international law consequences). The sibling readings —
 *   transactional_provisional and graduated_compliance — contest these
 *   structural facts. This story does not claim any reading is factually
 *   correct; it models the binding reading as a coherent constraint with its
 *   own structural properties, beneficiaries, and resistance.
 *
 * KEY AGENTS:
 *   - Multilateral institutional framework (Joint Commission, UNSC consensus requirement): sets interpretation, maintains modification procedure, benefits from constraint stability
 *   - Iran: moderately powerful state bound by verification obligations, benefits from sanctions relief but loses unilateral exit capacity, constrained by consensus-requirement on modification
 *   - United States: powerful UNSC member with veto on snapback, but bound by reading to multilateral consensus, cannot unilaterally rescind commitments or reimpose sanctions without consensus
 *   - EU signatories: benefit from non-proliferation stability, view binding reading as protecting regime integrity
 *   - China & Russia UNSC vetoes: institutional agenda-setters with leverage over consensus, benefit from stabilization
 *   - Regional actors (Israel, GCC): excluded from multilateral process, trapped by the constraint, cannot directly modify or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA Binding Multilateral Treaty Constraint (Binding Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '59d01014-6dfa-4957-9f3b-d356bb74e2d8').
narrative_ontology:cs_kernel_codification('59d01014-6dfa-4957-9f3b-d356bb74e2d8', fixed_text).
narrative_ontology:cs_authority_grounding('59d01014-6dfa-4957-9f3b-d356bb74e2d8', lineage).
narrative_ontology:cs_interpretation_layer_present('59d01014-6dfa-4957-9f3b-d356bb74e2d8').
narrative_ontology:cs_reading_relation('59d01014-6dfa-4957-9f3b-d356bb74e2d8', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('59d01014-6dfa-4957-9f3b-d356bb74e2d8', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('59d01014-6dfa-4957-9f3b-d356bb74e2d8', foundational, multilateral_consensus_modifies_unilateral_authority).
narrative_ontology:cs_axiom_status(multilateral_consensus_modifies_unilateral_authority, holdable).
narrative_ontology:cs_axiom_grounding('59d01014-6dfa-4957-9f3b-d356bb74e2d8', multilateral_consensus_modifies_unilateral_authority, conventional).
narrative_ontology:cs_axiom('59d01014-6dfa-4957-9f3b-d356bb74e2d8', foundational, treaty_pacta_sunt_servanda_binds_all_signatories_equally).
narrative_ontology:cs_axiom_status(treaty_pacta_sunt_servanda_binds_all_signatories_equally, holdable).
narrative_ontology:cs_axiom_grounding('59d01014-6dfa-4957-9f3b-d356bb74e2d8', treaty_pacta_sunt_servanda_binds_all_signatories_equally, deontological).
narrative_ontology:cs_reference_frame('59d01014-6dfa-4957-9f3b-d356bb74e2d8', multilateral_treaty_consensus_baseline).
narrative_ontology:cs_drift_state('59d01014-6dfa-4957-9f3b-d356bb74e2d8', contemporary_unilateral_defection_pressure, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('59d01014-6dfa-4957-9f3b-d356bb74e2d8', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutional_framework).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_state_exit_capacity).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_sanctions_relief_contingency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, european_union_signatories).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, china_and_russia_unsc_vetoes).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The JCPOA Joint Commission and UNSC permanent members collectively set the framework's interpretation, modification procedures, and dispute resolution mechanisms. They enforce the constraint by maintaining that unilateral withdrawal violates binding international law obligations and that sanctions relief cannot be unilaterally rescinded. They administer the consensus requirement for modification or dissolution.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutional_framework, agenda_setter,
    institutional, generational, analytical, global).

% Bound by verified compliance obligations under a reading that treats the treaty as multilaterally binding and immutable absent consensus. Benefits from sanctions relief contingent on continued compliance but faces the constraint that unilateral withdrawal by other signatories still triggers reimposition through UNSC mechanisms. Cannot unilaterally exit to recover sanctions relief; any breach triggers automatic dispute resolution, not immediate snapback. Bears the cost of sustained nuclear constraints.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary).

% A permanent UNSC member with a veto on snapback and modification, but bound under this reading by the treaty's multilateral character: unilateral withdrawal is legally non-binding, and reimposition of sanctions requires UNSC consensus (China and Russia must concur or abstain). The constraint limits unilateral exit and weaponization of the agreement; the US cannot rescind its own commitments without triggering countervailing international law claims.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states, payer,
    powerful, biographical, constrained, global).

% Benefit from a stable non-proliferation framework and from predictable Iranian nuclear constraints validated by multilateral consensus. Can negotiate modification through the Joint Commission but cannot unilaterally exit without legal consequences. View the binding multilateral reading as protecting the regime's integrity and their own security interests.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, european_union_signatories, beneficiary,
    institutional, generational, mobile, global).

% Hold permanent UNSC seats and can block or delay sanctions reimposition. Under the binding multilateral reading, they participate in consensus decisions on modification and enforcement. They benefit from the constraint's stabilization effect on regional nuclear proliferation and maintain structural leverage over any unilateral exit by other powers.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, china_and_russia_unsc_vetoes, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, china_and_russia_unsc_vetoes, beneficiary).

% Conducts intrusive inspections and reports compliance to the Joint Commission. Under the binding reading, IAEA findings trigger mandatory multilateral dispute resolution procedures before any enforcement action. The IAEA operates under the constraint's procedural discipline, not free to adjudicate unilaterally.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_compliance_verification, observer,
    institutional, generational, analytical, global).

% Are not parties to the JCPOA but have massive stakes in Iranian nuclear constraints. Excluded from the multilateral consensus process, they cannot modify the framework or withdraw from it; their only option is to contest its validity through pressure on signatories or to operate outside it. The binding multilateral reading locks the framework against their direct intervention.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_actors_israel_gulf_states, excluded,
    powerful, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutional_framework).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes nuclear proliferation risk in the Middle East and globally by binding Iran to verifiable uranium enrichment limits, establishing intrusive IAEA inspection access, and creating a multilateral consensus-based modification procedure that prevents any single state from catastrophically destabilizing the arrangement through unilateral withdrawal.
% TRANSFER_FUNCTION: Moves compliance obligations (Iran limits enrichment, submits to inspection) and sanctions relief (lifting of nuclear-related sanctions, unfreezing assets) in exchange; the constraint distributes modification authority to all parties (consensus required), preventing unilateral veto or exit.
% ABSENT_VOICES: Non-signatories with direct security interests (Israel, Gulf Cooperation Council states) are structurally excluded from the multilateral consent mechanism. They would argue for stricter enforcement, longer sunset provisions, and direct participation in dispute resolution, but the binding reading locks them out of the modification process.
% DISAPPEARANCE_RATIONALE: If the binding multilateral constraint and its consensus requirement vanished, unilateral state withdrawal would become normatively and legally unchallenged; sanctions relief could be rescinded unilaterally; Iran would lose the legal shield of collective enforcement; regional actors would pursue alternative security strategies including potential weaponization or military confrontation. The entire non-proliferation regime's architecture for this region would reorganize.
% FOUNDING_PROBLEM: Iranian nuclear program ambiguity and escalating regional/global proliferation risk created standoff between Iran and P5+1 states; the founding problem was to lock in verifiable constraints through a binding multilateral commitment that no single state could weaponize or escape from.
% FOUNDING_PROBLEM_CORROBORATION: Signatories and multilateral institutions attest the founding problem remains live and the binding reading protects against destabilization. The US under certain administrations contests the reading itself, arguing the problem is Iranian deception and the binding constraint is naïve. Non-signatories attest the problem is under-addressed by the agreement. Independent analysts (IISS, Carnegie, Belfer Center) document that the founding technical problem (breakout timelines, inspection access) is substantially solved under verification; whether the constraint can remain binding absent consensus is the contested question, not the technical baseline.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the binding reading locks Iran and any departing signatory into multilateral dispute procedures and consensus requirements, preventing unilateral remediation. The constraint extracts compliance and authority loss from Iran (and from any powerful state attempting unilateral exit) and transfers authority to the multilateral consensus mechanism. Suppression is moderate (0.55) because the constraint is enforced through international law doctrine and UNSC veto mechanics, not direct coercion, but the consensus requirement does suppress unilateral exit options. Theater ratio rises from 0.08 to 0.22 over the interval because the constraint's functional integrity (verification, compliance assessment) coexists with performance of consensus legitimacy that grows more elaborate as signatories question the framework's bindingness. Accessibility of alternatives collapses substantially (0.72) because exiting the binding multilateral framework requires either renegotiating international law doctrine itself or acting in violation of treaty obligations — the interpretive frame is locked in by the consensus mechanism. Resistance is high (0.71) because powerful states (US under certain administrations, Iran facing compliance burden) have mounted and continue to mount real resistance to the binding reading, contesting whether unilateral withdrawal should be permitted or whether compliance should be graduated rather than absolute.
 *
 * PERSPECTIVAL GAP:
 *   The core seat divergence is between the multilateral institutional perspective and the unilateral-state perspective. From the institutional seat, the binding reading is natural law (treaty obligations are binding, consensus is required, unilateral exit is void). From the US or Iranian seat, the binding reading is extraction (it constrains options, locks in authority to others, prevents them from exiting). From the EU seat, the reading is coordination (a shared security good, participation is genuine benefit). These divergences are structural, not mere opinion. The engine will compute each seat's type from the directionality data; where the computed type diverges from the claimed tangled_rope, that divergence is the signal the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The multilateral institutional framework is the structural beneficiary (d ≈ 0.1–0.2, near beneficiary end): it gains authority over modification, enforcement procedure, and dispute resolution. Iran sits near the target end (d ≈ 0.75–0.85): it bears compliance constraints and contingent relief, with identity-locked exit (admission of violations triggers international law consequences). The US under the binding reading has moderately high d (≈0.6–0.7): it retains veto power (mobile exit option via UNSC leverage) but is constrained from unilateral withdrawal or unilateral sanctions rescission — its directionality depends on whether it reads itself as bound by the consensus requirement or exempt from it (the reading contest IS the directionality dispute). EU signatories sit near symmetric (d ≈0.4–0.5): they coordinate a regional security benefit with participation in a binding framework; their exit options are more mobile than Iran's (diplomatic rather than verification-contingent). Excluded actors (Israel, GCC) face full targeting (d ≈0.95): they have no say in modification and cannot exit a constraint they never consented to, yet it binds Iran's actions that affect them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Iranian nuclear ambiguity, regional proliferation risk) is contested as to status. The signatories and IAEA attest it is live and the binding multilateral constraint addresses it through verification and consensus discipline. The US in certain administrations contests the founding problem's relevance (arguing Iran is inherently deceptive and no constraint works), which is a claim that the founding problem is mis-stated, not dead. Non-signatories attest the founding problem is under-addressed (the constraint does not prevent Iranian regional ambitions or account for other nuclear actors). The mandatrophy verdict is OPEN rather than resolved: the functional purpose of the constraint (verification, non-proliferation stability) is contested, but the constraint persists because the multilateral institutional framework has invested its authority in maintaining it. A true mandatrophy resolution would require consensus to either modify the founding problem definition or formally sunset the arrangement — neither has occurred. Theater ratio's rise (0.08 → 0.22) suggests performative maintenance is growing: ritual reaffirmation of the binding reading, elaborate dispute procedures that do not resolve, symbolic shows of compliance verification, increase in rhetorical commitment despite functional doubts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_provisional_contest,
    'Is the JCPOA a binding multilateral treaty with consensus-based modification requirements, or a provisional transactional framework voidable upon unilateral determination of bad faith?',
    'Test case: if a signatory unilaterally withdraws and other parties treat that withdrawal as legally non-binding and continue the agreement, the binding reading survives; if other parties honor the withdrawal and dissolve the framework, the provisional reading is vindicated.',
    'If binding, Iran is locked into compliance constraints and the constraint type is tangled_rope with high extractiveness for Iran and moderate suppression of unilateral exit. If provisional, the constraint type downgrades to snare (unilateral exit is permitted but other parties reimpose sanctions retroactively) and the structure is purely extractive for Iran with no consensus protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binding_vs_provisional_contest, conceptual, 'Whether the JCPOA kernel should be read as binding international law or transactional-provisional.').

omega_variable(
    consensus_legitimacy_decay,
    'Does the binding multilateral reading''s legitimacy decay as signatories defect or contest the framework''s interpretation, and at what defection rate does consensus cease to bind?',
    'Monitor signatory commitment signaling, UNSC veto alignment, and dispute resolution usage over time. If UNSC vetoes prevent consensus enforcement (China or Russia block snapback) repeatedly, consensus legitimacy erodes. If signatories formally reinterpret the binding reading (e.g., via new protocol), the reading either transforms or dissolves.',
    'If consensus legitimacy decays without formal modification, the constraint transitions from tangled_rope (real consensus) to piton (theatrical consensus, actual enforcement broken). If consensus holds despite pressure, the binding reading remains structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_legitimacy_decay, empirical, 'Whether the binding multilateral reading can sustain institutional legitimacy as consensus faces pressure.').

omega_variable(
    unilateral_exit_legality_contest,
    'Under customary international law, does a signatory to a multilateral treaty retain the right to unilateral withdrawal upon determination of material breach by another party, or is withdrawal only valid via formal amendment procedures?',
    'International Court of Justice advisory opinion or contentious case on treaty withdrawal under Vienna Convention Article 62 (fundamental change of circumstances) or Article 60 (material breach). State practice on treaty withdrawal in comparable situations (IAEA safeguards, NPT protocols, environmental agreements).',
    'If unilateral withdrawal is legally valid under material breach doctrine, the binding reading is weakened (d for powerful states decreases); if withdrawal requires amendment procedures, the binding reading is strengthened (d for powerful states increases). This reshapes the constraint''s type from the institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_exit_legality_contest, empirical, 'The legality of unilateral treaty withdrawal under international law doctrine.').

omega_variable(
    snapback_unilaterality_vs_consensus,
    'Do UNSC sanctions snapback automatically upon Iranian enrichment violation (unilateral trigger), or does reimposition require UNSC consensus (consensus-gated)?',
    'UNSC permanent member action: if China or Russia veto snapback when Iran violates, snapback is consensus-gated (supports binding reading). If permanent members cannot block snapback (technical mechanism or council procedure makes it automatic), snapback is unilateral (weakens binding reading, moves toward transactional reading).',
    'Unilateral snapback: Iran faces automatic enforcement for violations, incentive to exit is high, constraint operates like snare. Consensus-gated snapback: Iran has dispute resolution window before enforcement, constraint operates like tangled_rope. This is the core empirical test of the binding reading''s functional integrity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(snapback_unilaterality_vs_consensus, empirical, 'Whether UNSC sanctions snapback operates automatically or via consensus.').

omega_variable(
    regional_excluded_actors_pressure,
    'As excluded actors (Israel, GCC states) mount diplomatic or military pressure on signatories to exit or reinterpret the binding reading, does this external pressure successfully coerce signatories toward the transactional reading?',
    'Track diplomatic incidents, military exercises, formal letters to UNSC, and signatory rhetorical shifts toward conditional compliance or explicit sunset timelines. If signatories formally adopt sunset clauses or graduated compliance conditions, the binding reading''s institutional basis erodes.',
    'If excluded actors successfully coerce reinterpretation, the binding reading transforms into graduated_compliance or transactional_provisional (identity-locked suppression dissolves, exit-option constraints relax). If signatories maintain binding reading despite pressure, the constraint''s structural integrity holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_excluded_actors_pressure, empirical, 'Whether excluded regional actors can coerce reinterpretation of the binding reading.').

omega_variable(
    iran_dual_compliance_internalization,
    'Does Iranian compliance with JCPOA limits rest on structural suppression (fear of snapback/international isolation) or on internalized commitment to non-proliferation norms and regime legitimacy?',
    'Post-exit trajectory test: if a signatory withdraws and Iran abandons compliance immediately, suppression was structural. If Iran maintains compliance despite exit incentives (to preserve regime credibility or avoid escalation), suppression is partially internalized. Signatory signaling about the legitimacy of Iranian restraint versus forced compliance also reveals internalization depth.',
    'If internalized, the constraint''s suppression is more durable and less amenable to unilateral exit — the constraint strengthens the binding reading. If structural-only, the constraint dissolves immediately upon institutional failure — supports transactional reading. Mixed internalization suggests the constraint is transitioning (piton-candidate or degrading rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_dual_compliance_internalization, empirical, 'Whether Iranian compliance is structurally suppressed or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.22).
narrative_ontology:measurement(jcpo_tr_t2026, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(jcpo_be_t2026, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(jcpo_su_t2026, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_consensus_sanction_procedures).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_nuclear_enrichment_limits).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_treaty_regime).

% DUAL FORMULATION NOTE:
% The JCPOA kernel (jcpoa_treaty_bindingness) decomposes into three structurally distinct constraints representing different interpretive readings: binding_multilateral_reading (this story), transactional_provisional_reading, and graduated_compliance_reading. Each reading instantiates different ε, different beneficiary/victim structures, and different types. The readings coexist in international legal discourse and are held by different institutional actors. This constraint family should be analyzed as three linked stories, not as one constraint viewed from three angles. The ε values differ substantially because the readings assign different referents: the binding reading's referent is the standing multilateral consensus arrangement as understood by the institutional framework (high extraction of unilateral exit authority), while the transactional reading's referent is the same text interpreted as discretionary commitment (low extraction, unilateral exit available). These are not measurement-frame differences; they are reading-indexed structural differences per OQ-26 and OQ-258.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
