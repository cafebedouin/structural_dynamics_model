% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Maximal Withdrawal Reading
 *   domain: legal/diplomatic
 *
 * SUMMARY:
 *   This constraint instantiates the maximal withdrawal reading of the UNSC
 *   Resolution 242 withdrawal clause kernel. Under this reading, the
 *   authentic French text's definite article ('des territoires occupÃ©s')
 *   combined with Charter Article 2(4)'s territorial integrity default
 *   renders withdrawal mandatory from all occupied territories without
 *   exception. The kernel is contested among three readings: maximal
 *   withdrawal (this file), partial discretionary withdrawal
 *   (partial_withdrawal_reading), and contested interpretive authority
 *   (interpretive_authority_structure). The constraint coordinates the
 *   international community around a strict non-acquisition norm while
 *   extracting compliance from the occupying state.
 *
 * KEY AGENTS:
 *   - Dispossessed claimants: Primary beneficiary (organized/constrained) â gain enforceable legal position to full retrocession
 *   - Occupying state: Primary target (institutional/constrained) â bears mandatory withdrawal obligation and strategic loss
 *   - UN Security Council: Agenda setter (institutional/analytical) â administers Charter enforcement framework
 *   - International Court of Justice: Analytical observer (institutional/analytical) â adjudicates authentic text and drafting history
 *   - Settler populations: Excluded voice (moderate/trapped) â bear direct displacement costs but lack legal standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.68).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Maximal Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "legal/diplomatic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'a47d819f-0ccd-4f2a-b3e7-b634465c04a8').
narrative_ontology:cs_kernel_codification('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', formalized).
narrative_ontology:cs_authority_grounding('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', lineage).
narrative_ontology:cs_interpretation_layer_present('a47d819f-0ccd-4f2a-b3e7-b634465c04a8').
narrative_ontology:cs_reading_relation('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', foundational, territorial_integrity_absolute_default).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute_default, holdable).
narrative_ontology:cs_axiom_grounding('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', territorial_integrity_absolute_default, conventional).
narrative_ontology:cs_axiom('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', foundational, french_authentic_text_controls).
narrative_ontology:cs_axiom_status(french_authentic_text_controls, holdable).
narrative_ontology:cs_axiom_grounding('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', french_authentic_text_controls, conventional).
narrative_ontology:cs_reference_frame('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', charter_territorial_integrity_regime).
narrative_ontology:cs_drift_state('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', post_1967_occupation_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a47d819f-0ccd-4f2a-b3e7-b634465c04a8', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to territories under occupation and derive an enforceable claim to full retrocession from the maximal reading of Resolution 242 and Charter Article 2(4). Their ability to recover territory depends on the constraint's recognition and enforcement by the Security Council and international legal organs.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    organized, generational, constrained, national).

% Exercises effective control over territories acquired during conflict and is obligated under the maximal reading to withdraw from all such territories without exception. Retains strategic and settlement infrastructure that would be dismantled by full compliance. Exit from the obligation is constrained by UN membership and the threat of collective enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, constrained, national).

% Administers the Chapter VII and Article 2(4) framework through resolutions and enforcement mechanisms. Sets the interpretive agenda for what compliance means, though its own resolutions deliberately left textual ambiguity. Could mandate enforcement but is politically constrained by permanent member vetoes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, global).

% Provides advisory opinions and contentious jurisdiction on territorial disputes and treaty interpretation. Examines authentic texts and drafting history, but its findings depend on state consent and Security Council follow-through to become operative.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% Civilian populations residing in occupied territories under the occupying state's policy. They would face direct displacement and loss of property under full retrocession. They have no standing in the UN legal framework that adjudicates the withdrawal obligation and their interests are not represented in Charter Article 2(4) analysis.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations, excluded,
    moderate, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents territorial acquisition by force and maintains the territorial integrity framework of the UN Charter by binding occupying states to full retrocession, solving the collective-action problem of individual states resisting expansionist claims.
% TRANSFER_FUNCTION: Transfers territorial control from the occupying state to the dispossessed claimant; imposes compliance costs and strategic losses on the occupying state.
% ABSENT_VOICES: Settler populations in occupied territories bear the direct human cost of withdrawal but have no standing in the UN legal framework; third-party states with security guarantees to the occupier are structurally muted in the legal forum where the maximal reading is advanced.
% DISAPPEARANCE_RATIONALE: If the mandatory withdrawal obligation vanished, occupying states would retain territorial control acquired by force, the territorial integrity norm would collapse, and the post-1945 legal order would reorganize around strategic fait accompli rather than legal title.
% FOUNDING_PROBLEM: The problem of wars of conquest and territorial acquisition by force, which the UN Charter sought to eliminate through the Article 2(4) prohibition on the use of force and the territorial integrity default.
% FOUNDING_PROBLEM_CORROBORATION: The UN General Assembly and non-aligned movement attest the problem is live from the beneficiary side; the occupying state and allied security council members attest the problem is dead or superseded by security realities. Independent legal scholars and the ICJ Registry provide external corroboration that the territorial acquisition problem remains live in international law, though its solution via maximal withdrawal is disputed.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics are authored independently of the claim. The constraint is claimed as rope because it coordinates the international community around the territorial integrity norm, but extractiveness is high (0.82) because the mandatory comprehensive withdrawal imposes severe costs on the occupying state. Suppression is substantial (0.68) because the constraint persists only through active enforcement mechanisms, diplomatic pressure, and the legal performativity of UN organs. Theater ratio is moderate (0.35): the French definite article argument is partly substantive textual analysis and partly performative legal construction that sustains the norm despite incomplete enforcement. Accessibility collapse is high (0.78) because once this reading is accepted, partial withdrawal or territorial adjustment is legally foreclosed. Resistance is moderate-high (0.62) because occupying states and their allies actively contest the reading in favor of the partial alternative.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (dispossessed claimants) experiences this constraint as a rope: a genuine coordination mechanism that restores their legal position. The payer seat (occupying state) experiences it as substantially extractive â a forced surrender of strategic territory and settler investments. The agenda-setter seat (UN Security Council) sits between, administering a norm whose ambiguity allows political flexibility but whose maximal reading extracts maximum compliance. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed claimants are declared beneficiaries and derive low directionality (subsidized by the constraint's legal force). The occupying state is declared a victim and derives high directionality (target of extraction). The UN Security Council and ICJ are neither beneficiaries nor victims; they revert to the institutional fallback directionality, reflecting their administrative and analytical distance from the extractive transfer. No directionality overrides are needed because the beneficiary and victim declarations produce accurate d values for the principal seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy guard prevents mislabeling the constraint as pure extraction (snare) because the dispossessed claimants genuinely benefit from restored territorial integrity â the coordination function is real and not a cover story. Conversely, it prevents mislabeling as pure coordination (rope) because the occupier bears asymmetric costs without commensurate benefit, and the constraint requires active enforcement to hold against the occupier's resistance. The theater ratio (0.35) reflects legal argumentation that sustains the constraint's performative validity even as enforcement remains incomplete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_definite_article_semantic_control,
    'Does the French definite article ''des'' in the authentic French text of Resolution 242 semantically mandate withdrawal from all occupied territories, or is the English indefinite ''territories'' dispositive?',
    'Comparative treaty interpretation study by the ICJ or UN Office of Legal Affairs examining the trilingual drafting history and subsequent practice under VCLT Article 33.',
    'If the French text controls, the maximal reading is codified and the partial reading collapses; if the English text is dispositive, the maximal reading rests on Charter 2(4) alone and the textual foundation weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_definite_article_semantic_control, conceptual, 'Whether the French definite article mandates full withdrawal').

omega_variable(
    kernel_reading_stability,
    'Does the maximal withdrawal reading logically foreclose the partial withdrawal reading within a single legal framework, or do they remain coexisting live options?',
    'ICJ advisory opinion or Security Council determination adopting one reading as binding; or sustained state practice establishing one reading as the sole operative interpretation.',
    'If foreclosed, the partial reading becomes legally inoperative and the constraint stabilizes; if coexisting, the kernel remains irreducibly contested and the constraint''s classification depends on which authority is seated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Structural relationship between maximal and partial readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_maximal_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unsc_242_maximal_tr_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(unsc_242_maximal_tr_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(unsc_242_maximal_tr_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(unsc_242_maximal_tr_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(unsc_242_maximal_tr_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement(unsc_242_maximal_tr_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(unsc_242_maximal_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(unsc_242_maximal_be_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(unsc_242_maximal_be_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(unsc_242_maximal_be_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(unsc_242_maximal_be_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(unsc_242_maximal_be_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(unsc_242_maximal_be_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(unsc_242_maximal_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unsc_242_maximal_su_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(unsc_242_maximal_su_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(unsc_242_maximal_su_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(unsc_242_maximal_su_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(unsc_242_maximal_su_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 50, 0.67).
narrative_ontology:measurement(unsc_242_maximal_su_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is the maximal withdrawal reading of the UNSC 242 withdrawal clause kernel. It decomposes from the colloquial label 'UNSC 242 withdrawal obligation' into structurally distinct claims: maximal mandatory withdrawal (this file), partial discretionary withdrawal (partial_withdrawal_reading), and contested interpretive authority (interpretive_authority_structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
