% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Abolitionist Reading of State Killing Legitimacy
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of state
 *   killing legitimacy, which posits that state killing categorically
 *   violates human dignity regardless of desert or utility. From this
 *   perspective, the state's power to execute is inherently illegitimate and
 *   extractive, making the constraint a Snare. The condemned person is viewed
 *   as a rights-bearer whose dignity is violated, while the state's killing
 *   power itself is the 'victim' of this moral violation. The metrics reflect
 *   the high moral cost and the active suppression required to maintain state
 *   killing in the face of this categorical prohibition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Abolitionist Reading of State Killing Legitimacy").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '774761b5-fb30-4177-88d7-ac7c5dec9c36').
narrative_ontology:cs_kernel_codification('774761b5-fb30-4177-88d7-ac7c5dec9c36', formalized).
narrative_ontology:cs_authority_grounding('774761b5-fb30-4177-88d7-ac7c5dec9c36', lineage).
narrative_ontology:cs_interpretation_layer_present('774761b5-fb30-4177-88d7-ac7c5dec9c36').
narrative_ontology:cs_reading_relation('774761b5-fb30-4177-88d7-ac7c5dec9c36', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('774761b5-fb30-4177-88d7-ac7c5dec9c36', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('774761b5-fb30-4177-88d7-ac7c5dec9c36', foundational, human_dignity_is_inviolable).
narrative_ontology:cs_axiom_status(human_dignity_is_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('774761b5-fb30-4177-88d7-ac7c5dec9c36', human_dignity_is_inviolable, deontological).
narrative_ontology:cs_axiom('774761b5-fb30-4177-88d7-ac7c5dec9c36', secondary, state_power_is_conditional).
narrative_ontology:cs_axiom_status(state_power_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('774761b5-fb30-4177-88d7-ac7c5dec9c36', state_power_is_conditional, deontological).
narrative_ontology:cs_reference_frame('774761b5-fb30-4177-88d7-ac7c5dec9c36', universal_human_rights_framework).
narrative_ontology:cs_drift_state('774761b5-fb30-4177-88d7-ac7c5dec9c36', contemporary_legal_systems, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('774761b5-fb30-4177-88d7-ac7c5dec9c36', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_person_as_rights_bearer).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From this reading, the condemned person is the primary rights-bearer whose inherent dignity is violated by state killing, regardless of their actions. They are structurally trapped by the state's power.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_person_as_rights_bearer, beneficiary,
    powerless, immediate, trapped, national).

% The state's power to execute is seen as inherently illegitimate and extractive of human dignity. This reading argues that the state itself is 'victimized' by engaging in such practices, as it compromises its own moral standing and legitimacy.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_power, payer,
    institutional, generational, constrained, national).

% These groups actively campaign against capital punishment, seeking to dismantle the legal and cultural structures that permit state killing. They frame the issue as a categorical violation of human rights.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, agenda_setter,
    organized, generational, mobile, global).

% These groups argue for the legitimacy of capital punishment based on retribution or deterrence. From the abolitionist reading, their arguments are fundamentally flawed and their voices are excluded from the moral framework of inherent dignity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, pro_capital_punishment_advocates, excluded,
    organized, generational, constrained, national).

% These bodies monitor and condemn state killing, aligning with the abolitionist reading's emphasis on universal human dignity. They exert moral and political pressure on states that retain capital punishment.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading seeks to coordinate a universal moral standard where all human life is inviolable, preventing states from exercising ultimate power over individuals' existence.
% TRANSFER_FUNCTION: It seeks to transfer the absolute right to life from the state's conditional grant back to the individual as an inherent, unconditional right, thereby 'extracting' the power to kill from the state.
% ABSENT_VOICES: Proponents of retributive justice and deterrence are absent from this reading's moral framework, as their justifications for state killing are deemed incompatible with the categorical imperative of human dignity.
% DISAPPEARANCE_RATIONALE: If the abolitionist reading's constraint (categorical prohibition on state killing) were universally adopted overnight, legal systems would be fundamentally reformed, death rows emptied, and the moral landscape of state power would be irrevocably altered, leading to a global shift in human rights norms.
% FOUNDING_PROBLEM: The problem this reading addresses is the historical and ongoing practice of states exercising arbitrary or conditional power over human life, leading to irreversible injustices and the devaluation of human dignity.
% FOUNDING_PROBLEM_CORROBORATION: International human rights treaties, numerous philosophical traditions, and the consistent advocacy of global abolitionist movements corroborate that the problem of state killing's legitimacy remains a live and pressing concern, independent of the state's own justifications.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.95) because the act of state killing is seen as a complete and irreversible extraction of a person's fundamental right to life and dignity. Suppression is also high (0.88) because the state must actively suppress the moral and legal arguments for abolition, as well as the physical existence of the condemned, to maintain its power to kill. Resistance is high (0.9) due to the persistent global movement against capital punishment. The claimed type is Snare because the coordination story (e.g., 'justice' or 'public safety') is seen as a cover for an inherently extractive and dignity-violating practice, requiring active enforcement against fundamental human rights.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist perspective, there is no legitimate 'beneficiary' of state killing, only a victim (the condemned) and an entity that compromises its own moral standing (the state). This contrasts sharply with retributive or deterrence readings, which identify beneficiaries (e.g., 'justice' or 'society's safety') and frame the condemned as having forfeited rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'condemned_person_as_rights_bearer' is the full beneficiary of this constraint's moral claim (d=0.0), as the constraint seeks to protect their life and dignity. The 'state_killing_power' is the full target (d=1.0), as the constraint aims to abolish this power entirely. Abolitionist advocates are agenda-setters, working to enforce this moral constraint. Pro-capital punishment advocates are excluded, as their arguments are deemed morally incompatible.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling state killing as a legitimate 'Rope' or 'Tangled Rope' that serves a genuine coordination function. By classifying it as a Snare, the framework highlights the inherent extraction and suppression from the abolitionist viewpoint, preventing the 'mandate' of state justice from obscuring the fundamental violation of dignity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_consequentialist_justification,
    'Is the prohibition against state killing a categorical imperative (as this reading asserts) or a consequentialist calculation based on utility or desert?',
    'Philosophical debate and legal precedent: resolution would involve a global consensus on the foundational moral grounding of human rights and state power.',
    'If categorical, the Snare classification holds. If purely consequentialist, the constraint might be re-evaluated as a ''Tangled Rope'' or ''Rope'' depending on the balance of utility, potentially lowering extractiveness if benefits (e.g., deterrence) are proven to outweigh costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_consequentialist_justification, conceptual, 'Ambiguity in the moral grounding of the prohibition against state killing.').

omega_variable(
    state_legitimacy_paradox,
    'Does the state''s exercise of lethal force (e.g., in self-defense or war) inherently contradict the categorical prohibition on state killing, or are these distinct domains?',
    'Development of a coherent and universally accepted theory of legitimate state violence that reconciles these apparent contradictions, or a clear demarcation of their moral boundaries.',
    'If a contradiction is found, it could weaken the ''Snare'' classification by introducing internal inconsistencies in the abolitionist position. If distinct, the Snare classification for capital punishment remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_paradox, conceptual, 'Reconciling the abolitionist stance on capital punishment with other forms of state-sanctioned lethal force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(stat_be_t1948, state_killing_legitimacy__abolition_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__abolition_reading, base_extractiveness, 1970, 0.92).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1948, state_killing_legitimacy__abolition_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__abolition_reading, suppression_requirement, 1970, 0.82).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
