% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 — Partial/Discretionary Withdrawal Reading (Secure Boundaries)
 *   domain: International Law / Diplomatic History / Treaty Interpretation
 *
 * SUMMARY:
 *   This story instantiates the partial/discretionary withdrawal reading of
 *   UNSC Resolution 242's withdrawal clause: the operative English text calls
 *   for withdrawal 'from territories occupied' — without a definite article —
 *   which the occupying state and negotiating partners read as licensing
 *   withdrawal from SOME but not necessarily ALL occupied territory,
 *   contingent on achievement of 'secure and recognized boundaries.' Under
 *   this reading, the resolution is not a fixed withdrawal mandate but a
 *   framework instrument that converts territorial return into a negotiated,
 *   phased, conditional process. The maximal withdrawal reading (French
 *   definite article, Article 2(4) territorial-integrity default) and the
 *   interpretive-authority-structure reading (who gets to adjudicate the
 *   ambiguity) are separate constraints, linked here by network edges — this
 *   story does not adjudicate between them; it authors only the structural
 *   consequences of the discretionary reading being operative in practice for
 *   decades.
 *
 * KEY AGENTS:
 *   - occupying_state: primary beneficiary (institutional/arbitrage) — controls pace and scope of any withdrawal
 *   - third_party_mediators: secondary beneficiary (institutional/mobile) — derive standing from perpetual brokering
 *   - displaced_claimant_population: primary target (powerless/trapped) — bears indefinite deferral with no fixed enforcement line
 *   - neighboring_frontline_states: secondary target (moderate/constrained) — absorbs downstream instability
 *   - un_security_council: agenda-setter/observer (institutional/analytical) — declines binding interpretation, ratifying discretion by inaction
 *   - international_law_scholars: analytical observer — documents drafting history and effect without power to bind
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 — Partial/Discretionary Withdrawal Reading (Secure Boundaries)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "International Law / Diplomatic History / Treaty Interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '75aff238-4d26-47ce-aa61-96131bdd5487').
narrative_ontology:cs_kernel_codification('75aff238-4d26-47ce-aa61-96131bdd5487', fixed_text).
narrative_ontology:cs_authority_grounding('75aff238-4d26-47ce-aa61-96131bdd5487', distributed).
narrative_ontology:cs_reading_relation('75aff238-4d26-47ce-aa61-96131bdd5487', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('75aff238-4d26-47ce-aa61-96131bdd5487', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('75aff238-4d26-47ce-aa61-96131bdd5487', foundational, withdrawal_scope_conditioned_on_negotiated_security).
narrative_ontology:cs_axiom_status(withdrawal_scope_conditioned_on_negotiated_security, holdable).
narrative_ontology:cs_axiom_grounding('75aff238-4d26-47ce-aa61-96131bdd5487', withdrawal_scope_conditioned_on_negotiated_security, conventional).
narrative_ontology:cs_axiom('75aff238-4d26-47ce-aa61-96131bdd5487', secondary, indefinite_article_reflects_authorial_compromise).
narrative_ontology:cs_axiom_status(indefinite_article_reflects_authorial_compromise, holdable).
narrative_ontology:cs_axiom_grounding('75aff238-4d26-47ce-aa61-96131bdd5487', indefinite_article_reflects_authorial_compromise, empirically_contingent).
narrative_ontology:cs_reference_frame('75aff238-4d26-47ce-aa61-96131bdd5487', ceasefire_stabilization_framework_1967).
narrative_ontology:cs_drift_state('75aff238-4d26-47ce-aa61-96131bdd5487', post_oslo_stalled_process_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75aff238-4d26-47ce-aa61-96131bdd5487', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, third_party_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_claimant_population).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, neighboring_frontline_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the territories acquired in the conflict and reads the resolution's indefinite article as licensing retention of what it deems strategically necessary pending negotiated 'secure and recognized boundaries.' Controls the pace, scope, and preconditions of any withdrawal, and can indefinitely defer full withdrawal by citing unresolved security guarantees. Bears diplomatic cost but no binding enforcement mechanism compels a specific withdrawal line.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state, agenda_setter).

% Diplomatic sponsors and mediating states derive ongoing relevance, leverage, and institutional standing from managing a permanently 'in-process' negotiation. A resolved, fixed withdrawal line would end their brokering role; the indefiniteness of scope keeps them structurally indispensable to any incremental progress.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, third_party_mediators, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, third_party_mediators, agenda_setter).

% Communities whose territorial and residency claims depend on withdrawal actually occurring to a defined line. Under the discretionary reading, no enforcement mechanism fixes when or whether withdrawal completes; the population bears indefinite displacement, statelessness, and loss of use of land and property while the 'secure boundaries' precondition remains permanently negotiable.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_claimant_population, payer,
    powerless, civilizational, trapped, regional).

% States bordering the occupied territories absorb refugee flows, security incidents, and diplomatic pressure generated by the unresolved status. They lack the leverage to force a fixed withdrawal timeline and depend on the same mediators who benefit from the arrangement's indefiniteness.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, neighboring_frontline_states, payer,
    moderate, generational, constrained, regional).

% Adopted the resolution's language and has since declined to issue a binding authoritative interpretation resolving the English/French textual divergence, effectively ratifying the discretionary reading as the operative diplomatic default through decades of non-enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, un_security_council, observer).

% Document the drafting history, the English/French discrepancy, and the practical effect of the discretionary reading on decades of negotiation dynamics, without power to bind any party to a resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formula flexible enough that all parties to the 1967 ceasefire could accept the resolution's text without conceding their maximal positions — enabling a ceasefire and a negotiating framework where no immediate agreement on final borders was possible.
% TRANSFER_FUNCTION: Converts textual indefiniteness into negotiating leverage held by the occupying state and the mediating parties, at the expense of a fixed enforcement timeline for the claimant population and frontline states, who instead receive an indefinitely deferred process.
% ABSENT_VOICES: The displaced claimant population was not a drafting party to the resolution and has no seat in the Security Council; their objection — that discretionary withdrawal converts a ceasefire instrument into permanent occupation cover — is registered only through advocacy and subsequent General Assembly resolutions, not through the enforcement mechanism itself.
% DISAPPEARANCE_RATIONALE: The occupying state and mediators would say the world rearranges catastrophically — the entire post-1967 negotiating architecture (land-for-peace framework, subsequent peace treaties, road-map processes) references this text and would lose its anchor. The claimant population and frontline states would say the world is substantially unchanged in their material situation, since the discretionary reading has produced no enforced withdrawal for decades; removing the clause would only make explicit what is already true in practice.
% FOUNDING_PROBLEM: The Security Council needed language that could secure an immediate ceasefire and a face-saving diplomatic path for all combatant parties after the 1967 war, without requiring any party to accept binding final-status terms it was not prepared to accept at the time.
% FOUNDING_PROBLEM_CORROBORATION: The occupying state and allied diplomatic historians attest the founding problem remains live — that security guarantees adequate to satisfy 'secure and recognized boundaries' have still not been achieved. Independent legal historians (drafters' own subsequent memoirs, e.g. Lord Caradon's later clarifications) and UN General Assembly majorities attest the ceasefire-stabilization problem was resolved decades ago and the indefiniteness is now used as a standing negotiating instrument rather than a live security necessity — corroboration exists outside the beneficiary set but is contested by the beneficiaries themselves.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is authored as moderate rather than severe because the constraint under this reading is genuinely conditional and phased — some territorial return has in fact occurred under this framework (e.g. Sinai), which is real coordination function, not pure cover. But extraction rises over the interval (0.35 -> 0.58) as the discretionary reading hardens from a plausible ceasefire-era ambiguity into a standing instrument for indefinite retention, tracked by rising suppression_requirement (0.40 -> 0.62) as the apparatus needed to maintain the discretionary reading against the counter-reading intensifies (legal briefs, diplomatic pressure, non-enforcement precedent). Theater ratio rises moderately (0.20 -> 0.40) as 'ongoing negotiation' increasingly substitutes for negotiation with a credible endpoint.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state's seat, this reading is a rope: a genuine, mutually accepted formula that let all 1967 parties exit an acute crisis without capitulating on final status. From the claimant population's seat, the identical text structure functions as a tangled rope shading toward snare: real coordination value at the ceasefire moment has been extracted for decades as cover for indefinite retention, with no enforcement mechanism ever activated to convert 'eventual withdrawal' into an actual line. The engine computing different types at different seats from the same structural data is the intended signature, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and mediators sit near the beneficiary end of directionality: the indefinite scope IS the leverage and the standing they hold. The displaced claimant population and frontline states sit near the target end: they bear the costs of an unresolved process with no fixed line and no enforcement mechanism compelling one. The UN Security Council occupies an unusual dual position — nominally the agenda-setter who could resolve the ambiguity, but functionally an observer by choice, since declining to rule is itself a structural choice that entrenches the discretionary reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing an immediate ceasefire and a face-saving framework after acute war — is largely dead by the corroboration of independent legal historians and successive General Assembly majorities, but the discretionary reading persists because it now serves an ongoing coordination function for the parties who benefit from perpetual process (negotiating leverage, mediator relevance) rather than because the original ceasefire-stabilization function is still live. This is the classic mandatrophy signature: a mandate whose founding function has been substantially achieved (a ceasefire exists, has held) being maintained past its function because dismantling the ambiguity would remove leverage from those who hold it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_indefiniteness_intent_or_accident,
    'Was the drafters'' choice of an indefinite English article a deliberate diplomatic compromise reflecting genuine intent to permit partial withdrawal, or a translation/drafting artifact that the occupying state has since exploited beyond what any drafter intended?',
    'Examination of the full UNSC deliberation record, drafters'' contemporaneous and later memoirs (e.g. Lord Caradon, Arthur Goldberg), and comparison with the equally authoritative French text, which uses the definite article and would not support this reading.',
    'If deliberate compromise, the discretionary reading has genuine textual and intentional grounding and should be weighted as legitimate treaty interpretation. If drafting artifact, the reading is a false summit dressed as authorial intent — extraction riding on an interpretive technicality rather than genuine ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_indefiniteness_intent_or_accident, conceptual, 'Whether the indefinite article reflects genuine drafting intent or an exploited translation artifact.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'This constraint is one reading (partial_withdrawal_reading) of the unsc_242_withdrawal_clause kernel. Its sibling maximal_withdrawal_reading holds that Article 2(4) territorial integrity plus the French definite article make withdrawal from ALL territories mandatory. Does adopting this reading logically foreclose the maximal reading within any single interpretive framework, or can both readings coexist as live positions held by different parties in an unresolved international dispute?',
    'This is inherently a matter of which interpretive authority is recognized as controlling (see the third sibling, interpretive_authority_structure) — an ICJ advisory opinion adopting one reading would foreclose the other within international judicial practice, but absent such a ruling both remain live in state practice.',
    'If the readings coexist (the authored relation in this story), classification analysis should treat both constraints as simultaneously live and structurally distinct rather than treating one as simply ''wrong.'' If a future authoritative ruling forecloses one reading, that constraint''s story should be updated to reflect resolved rather than contested status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the partial and maximal withdrawal readings can coexist or whether one forecloses the other pending authoritative interpretation.').

omega_variable(
    secure_boundaries_precondition_satisfiability,
    'Is the ''secure and recognized boundaries'' precondition for full withdrawal, under this reading, ever objectively satisfiable, or is it structured such that the occupying state retains permanent discretion to declare it unmet?',
    'Analysis of whether any objective, third-party-verifiable security criteria have ever been proposed or accepted by the occupying state as sufficient; comparison with cases where withdrawal to a fixed line did occur (Sinai) versus cases where it has not (West Bank, Golan Heights) to isolate what distinguished satisfiable from unsatisfiable applications of the principle.',
    'If the precondition is structured to be permanently unsatisfiable at the occupying state''s discretion, this reading functions less like a conditional coordination mechanism and more like a snare wearing coordination language — the ''moderate'' extraction score authored here would be too low. If satisfiable and historically satisfied in analogous cases, the moderate score and tangled_rope classification are well-grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secure_boundaries_precondition_satisfiability, empirical, 'Whether the secure-boundaries precondition is a genuine, satisfiable coordination condition or a permanently deferrable discretionary trigger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1979, 0.28).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(unsc_tr_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(unsc_tr_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(unsc_be_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1979, 0.42).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.48).
narrative_ontology:measurement(unsc_be_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(unsc_be_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc_su_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1979, 0.48).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.53).
narrative_ontology:measurement(unsc_su_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(unsc_su_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the unsc_242_withdrawal_clause constraint family, decomposed per the epsilon-invariance principle because 'the withdrawal clause' evaluated under different interpretive lenses yields incompatible epsilon values that cannot be averaged or hedged within a single story. partial_withdrawal_reading (this story, epsilon ~0.58, tangled_rope, moderate/conditional extraction) and maximal_withdrawal_reading (separate story, expected higher epsilon under this story's own lights since it treats the occupying state's retention as flatly non-compliant with a mandatory obligation) share the same underlying text but diverge structurally on beneficiary/victim sets and on whether any coordination function survives the occupying state's discretion. interpretive_authority_structure is the third member, addressing not what withdrawal is required but WHO has standing to decide between the other two readings — a distinct question about institutional authority (ICJ vs. drafting-state intent vs. customary practice) rather than about withdrawal scope itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
