% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Militia Clause Conditions Right on Collective Security
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The collective security reading interprets the Second Amendment's militia
 *   clause as a conditional: the right to bear arms is grounded in and
 *   limited by organized state defense. Under this reading, the state may
 *   regulate individual arms-bearing through licensing, permitting, and
 *   training requirements justified as ensuring only those fit to serve
 *   collective security may access firearms. This stands in direct logical
 *   tension with the individual-right reading (operative clause guarantees
 *   individual right independent of militia status) and the originalist
 *   civic-virtue reading (founding militia was universal armed citizenry, not
 *   state-controlled forces). The constraint operates as tangled rope:
 *   genuine coordination function (linking individual rights to collective
 *   defense structures) coupled with asymmetric extraction (state regulatory
 *   authority over individual conduct, licensing fees, permit denial as
 *   enforcement leverage). The authored claim and metrics track the same
 *   structure independently: the claim identifies the type as tangled rope;
 *   the metrics (high extractiveness, high suppression, moderate theater)
 *   describe its actual operation.
 *
 * KEY AGENTS:
 *   - State licensing apparatus: agenda-setter, administers permit regimes justified by militia clause conditioning
 *   - Law enforcement bureaucracy: beneficiary, leverages licensing apparatus for regulatory and intelligence authority
 *   - Individual gun owners: payer (constrained by permits/licensing), identity-locked by Second Amendment self-conception
 *   - Militia practitioners: payer (subordinated to state control), identity-locked by militia constitutionalism
 *   - Public safety constituency: beneficiary (supports state gatekeeping on public safety grounds)
 *   - Constitutional originalists: excluded from the reading's framework, would foreclose it but lack adjudicative authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.68).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Militia Clause Conditions Right on Collective Security").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '5e478ac8-ac39-47c4-8f5c-b76372b7b56c').
narrative_ontology:cs_kernel_codification('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', fixed_text).
narrative_ontology:cs_authority_grounding('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', lineage).
narrative_ontology:cs_interpretation_layer_present('5e478ac8-ac39-47c4-8f5c-b76372b7b56c').
narrative_ontology:cs_reading_relation('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', foundational, militia_clause_conditions_operative_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_right, holdable).
narrative_ontology:cs_axiom_grounding('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', militia_clause_conditions_operative_right, deontological).
narrative_ontology:cs_axiom('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', foundational, state_regulatory_authority_serves_collective_security).
narrative_ontology:cs_axiom_status(state_regulatory_authority_serves_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', state_regulatory_authority_serves_collective_security, conventional).
narrative_ontology:cs_reference_frame('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', militia_clause_as_constitutional_condition).
narrative_ontology:cs_drift_state('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', contemporary_post_heller_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5e478ac8-ac39-47c4-8f5c-b76372b7b56c', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_licensing_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement_bureaucracy).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, militia_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers permit and licensing regimes justified by the collective security framing: requires background checks, waiting periods, training certification, and permit renewal. Interprets the militia clause as grounding state authority to condition individual rights on demonstrated fitness to serve collective defense. Collects fees, maintains regulatory infrastructure, and gates access to protected conduct.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_licensing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the licensing apparatus as a source of regulatory leverage, intelligence gathering through background-check data, and authority to enforce compliance. Uses permit denial and revocation as enforcement tools aligned with broader crime-control objectives. Does not run the licensing system but leverages it operationally.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement_bureaucracy, beneficiary,
    institutional, generational, analytical, national).

% Must submit to licensing and permitting regimes to exercise a constitutional right they view as individual and inviolable. Bear the compliance costs (fees, waiting periods, training requirements, administrative burden). Exit options are constrained by state jurisdiction (cannot move across borders to escape regulation) and identity-locked by Second Amendment self-conception as individual right-bearers independent of militia status. Resistance to licensing is high but enforcement is asymmetric: state has vastly greater coercive capacity.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, identity_locked, national).

% Organized militia organizations (constitutional militia theorists, state defense forces, unorganized militia adherents) contest the state's monopoly claim on what constitutes legitimate militia activity. They view the state's licensing regime as subordinating citizen militia to state control, effectively negating independent militia capacity. Identity-locked by commitment to militia constitutionalism; exit means abandoning foundational civic identity.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, militia_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Gun control advocacy groups and public safety coalitions support licensing regimes as serving collective security. They frame the state regulatory apparatus as protecting the public from unrestricted individual access to weapons, reading the militia clause as conditioning individual rights on subordination to organized state defense. They benefit from a licensing apparatus that channels demand for firearms through state-gatekeeping mechanisms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_constituency, beneficiary,
    organized, biographical, mobile, national).

% Are excluded from the collective security reading's operative framework: they would argue the militia clause is descriptive (explaining the right's origin) not conditional (limiting its scope), and that founding-era militia was universal armed citizenry, not state-controlled forces. Their interpretation would logically foreclose the collective security reading but they are structurally barred from adjudicating the text — courts aligned with other readings hold interpretive authority.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_originalists, excluded,
    institutional, generational, analytical, national).

% Holds the authoritative interpretive power over the Second Amendment text. Currently (as of 2026) produces mixed readings: DC v. Heller (2008) endorsed an individual-right core while preserving regulatory space; later decisions expanded individual rights while narrowing state licensing authority. The judicial system's role is simultaneously agenda-setter (enforcing whichever reading it adopts) and contested arbiter (different judicial coalitions adopt different readings).
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, courts_and_judicial_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, courts_and_judicial_system, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_licensing_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between individual arms-bearing and organized state defense: reads the militia clause as conditioning individual rights on their integration into a collective security apparatus, subordinating individual conduct to state licensing and regulatory authority over who may bear arms and under what conditions.
% TRANSFER_FUNCTION: Transfers from individual gun owners and militia practitioners (compliance costs: fees, waiting periods, training requirements, administrative burden, permit denial risk) to the state licensing apparatus and law enforcement agencies (regulatory authority, intelligence access, enforcement leverage, fee revenue).
% ABSENT_VOICES: Constitutional originalists (who would argue the militia clause is descriptive, not conditional) and individual-right advocates are partially excluded: they testify in litigation but are not admitted to the regulatory framing itself — the collective security reading does not take their objections as constraints on what 'militia clause' can mean. They would demand reframing the constraint to center the operative clause's individual-right protection; they are structurally barred from that reframing within this reading's framework.
% DISAPPEARANCE_RATIONALE: If the collective security reading and its licensing apparatus vanished, individual gun ownership would shift to demand-driven access (no permit gates), militia organizations would reframe as autonomous civic actors rather than state-subordinate forces, and regulatory authority over arms would revert to state authority grounded on direct public safety power rather than militia clause conditioning. The Second Amendment's operative meaning would reorganize around individual-right reading or originalist civic-virtue reading, with different regulatory permissibility implications.
% FOUNDING_PROBLEM: To reconcile individual arms-bearing with organized state defense: the reading asserts the militia clause solves this by conditioning individual rights on state control of militia structure, ensuring armed citizens serve collective security rather than private or factional ends.
% FOUNDING_PROBLEM_CORROBORATION: State licensing authorities and public safety advocates attest the founding problem remains live and their licensing apparatus solves it. Originalists and individual-right advocates attest the founding problem is misdescribed — the founding intent was universal citizen militia, not state-controlled forces, and the problem was preventing state disarmament of the people, not integrating individual rights into state control. Historians of the Second Amendment (Bogus, Cornell, Rakove) offer competing corroboration depending on their reading of founding documents; no monolithic external authority attests the collective security reading's specific framing of the founding problem.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.45→0.68 over interval) because licensing regimes systematically extract compliance costs from individual owners and militia practitioners, and the state's regulatory authority expands as courts stabilize the collective security reading. Suppression is high (0.72) because exit options for individual owners are identity-locked (cannot exit the Second Amendment reading that defines their rights without abandoning foundational civic identity) and spatially constrained (cannot evade state jurisdiction). Theater ratio rises from 0.25 to 0.42 (plateauing after t=40) because the constraint's performative component increases as the regulatory apparatus elaborates: background checks and waiting periods acquire symbolic meaning (public safety theatre) while gatekeeping authority concentrates extraction. The interval captures the period from post-Heller (2008, t≈0) through contemporary judicial narrowing of licensing authority (t≈50), showing extractiveness climbing as courts initially stabilized collective security readings, then plateauing as other readings reasserted competitive legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Why would payers and beneficiaries compute different constraint types? The payer seats (individual owners at d≈0.8, militia practitioners at d≈0.75) should compute the constraint as snare or high-extraction tangled rope under their directionality, because the cost of compliance exceeds coordination benefit and exit is heavily suppressed. The beneficiary seats (state apparatus at d≈0.1, law enforcement at d≈0.15) should compute it as rope or light tangled rope — genuine coordination with modest extraction costs. The excluded originalist seat (not a directionality seat, but an interpretive authority seat) should compute it as contested or foreclosed relative to their own reading. These divergences are exactly what the engine is built to detect: same constraint structure, different d values per seat, different computed types per seat. The divergence is structural, not measurement error, and reflects real asymmetry in costs and benefits across the party set.
 *
 * DIRECTIONALITY LOGIC:
 *   The state licensing apparatus and law enforcement are primary beneficiaries: they collect regulatory authority, administrative fees, intelligence data through background checks, and enforcement leverage via permit denial/revocation. Their directionality is low (near 0.0 beneficiary end) because they incur minimal costs and derive substantial benefits from the arrangement. Individual gun owners are primary payers: they bear licensing fees, waiting-period delays, training requirements, background-check intrusions, and permit-denial risk. Their directionality is high (near 0.8 target end) because exit options are deeply constrained — identity_locked by their self-conception as Second Amendment right-bearers, and spatially trapped (cannot relocate to escape state jurisdiction). Militia practitioners face similar payer directionality (high extraction exposure) plus the additional identity-lock of militia constitutionalism — commitment to civic militia as independent of state control, making exit from the state-subordination framing existentially costly. Public safety advocates sit lower (d≈0.3) because they derive coordination benefits (protection from unrestricted individual access) without direct identity-lock, though they have mobile exit options (could shift to other public safety mechanisms). This asymmetry across stakeholder seats drives the tangled rope classification: coordination function is real (linking individual rights to collective defense), but extraction is asymmetric and actively enforced.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved here; the constraint remains fully operative. The founding problem — reconciling individual arms-bearing with organized state defense — is contested (three live readings maintain it as live, contested, or dead). The state reads it as live and solved by collective security licensing regimes. Individual-right advocates read it as misdescribed (the problem was preventing state disarmament, not integrating individual rights into state control). Originalists read it as solved by universal militia, not state-controlled forces. Without consensus on mandate extinction, the constraint persists despite the fundamental contest over its justification. This is the characteristic mandatrophy signature: institutional persistence sustained by multiple incompatible narratives of purpose, with no dominant reading having sufficient authority to declare the founding problem dead and remove the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_conditionality,
    'Is the militia clause a CONDITION on the operative clause (limiting individual rights to militia-related purposes), or DESCRIPTIVE/EXPLANATORY of why the operative clause protects the right?',
    'Historical/textual analysis of founding documents and contemporaneous commentary; comparison to conditional vs. explanatory syntax in other constitutional provisions; contemporary judicial precedent reconciliation (DC v. Heller endorse individual core while leaving regulatory space; later decisions shift the balance).',
    'If conditional: this reading''s extraction framework stands; state licensing regimes serve legitimate militia-clause purposes. If descriptive: the operative clause protects individual right independent of militia, and state licensing must be justified on direct police power grounds, not militia clause grounds — the constraint reframes toward individual-right reading and suppression drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_conditionality, conceptual, 'Whether militia clause conditions or describes the operative clause').

omega_variable(
    collective_security_boundary,
    'What counts as ''collective security''? Does it require state-controlled/state-coordinated militia, or could citizen militia independent of state control satisfy the founding purpose?',
    'Historical evidence from founding era on militia structure and civic understanding; empirical comparison of security outcomes under state-controlled vs. independent citizen militia; contemporary militia legal status (unorganized militia, organized militia, state defense forces) and their relationships to state licensing authority.',
    'If independent citizen militia satisfies collective security: state monopoly on ''legitimate militia'' loses justification, licensing regimes cannot be justified on militia-clause grounds, constraint reframes toward individual-right reading. If only state-controlled forces satisfy it: this reading''s licensing authority is preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_security_boundary, empirical, 'Whether collective security requires state-controlled militia').

omega_variable(
    suppression_internalization_interpersonal,
    'Is the measured suppression (0.72) primarily structural (legal barriers, permit denial, administrative burden) or partly internalized (gun owners have internalized the state''s regulatory authority as legitimate, even where they contest it)?',
    'Post-licensing removal trajectories: if gun owners retain suppression behaviors after licensing regimes are invalidated, internalization has occurred; if suppression drops when enforcement ceases, it is purely structural.',
    'If internalized: the constraint''s effective suppression is higher than structural measures suggest, and the identity-lock mechanism is stronger. If structural: removing the state apparatus would unlock individual gun owners'' capacity to advocate for their preferred reading more fully.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    originalist_foreclosure_logic,
    'Does the collective security reading logically FORECLOSE the originalist civic-virtue reading within a single framework, or do the readings merely COEXIST as held by different parties with no framework that could hold both?',
    'Logical analysis of the core premises: collective security reading asserts militia must be state-controlled/coordinated to serve collective defense; originalist reading asserts universal citizen militia independent of state control. Do these logically contradict (foreclose) or merely disagree (coexist)?',
    'If foreclose: cs_structure.reading_relations declares ''forecloses'' and the engine routes to axiom_contradiction logic. If coexist: cs_structure declares ''coexists_with'' and the engine routes to legitimate-plurality logic. The classification of the logical relationship determines how the engine models the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_foreclosure_logic, conceptual, 'Whether this reading logically forecloses the originalist reading or coexists with it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__collective_security_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__collective_security_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__collective_security_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_text__collective_security_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_text__collective_security_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__collective_security_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__collective_security_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_text__collective_security_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__collective_security_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__collective_security_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__collective_security_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_text__collective_security_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__collective_security_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.18).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel decomposes into three structurally distinct constraints corresponding to three live readings: collective_security_reading (this story) interprets militia clause as condition, enabling state licensing regimes; individual_right_reading interprets operative clause as independent individual right, limiting state regulatory authority; originalist_civic_virtue_reading interprets founding militia as universal armed citizenry, protecting citizen-soldier capacity. Each reading instantiates different ε values, beneficiary/victim structures, and type classifications. All three remain live in contemporary constitutional discourse with no dominant reading holding consensus adjudicative authority. The three stories are linked via network.affects_constraints because each reading's strengthening weakens the others' legitimacy — a judicial shift toward originalism would reframe this constraint's extractiveness and suppression, potentially shifting its classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
