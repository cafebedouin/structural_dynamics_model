% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: State Authority to Execute for Deterrence
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence instrument' reading of state
 *   killing authority, where capital punishment is justified solely by its
 *   capacity to prevent future murders at an acceptable cost. It frames the
 *   condemned individual's life as an instrumental cost for a greater
 *   societal good. This reading is distinct from retributive justifications
 *   and categorical abolitionist stances, focusing on empirical outcomes and
 *   utilitarian calculus. The high extractiveness reflects the absolute cost
 *   borne by the condemned, while suppression is near-total due to the
 *   state's power. The rising theater ratio over time reflects increasing
 *   empirical challenges to the deterrence claim, making its justification
 *   more performative.
 *
 * KEY AGENTS:
 *   - state_authority: Agenda setter (institutional/analytical) — enforces and justifies
 *   - condemned_individuals: Primary target (powerless/trapped) — bears ultimate cost
 *   - potential_future_victims: Primary beneficiary (powerless/constrained) — theoretical benefit
 *   - abolitionist_advocates: Payer/Excluded (organized/constrained) — resists, bears moral cost
 *   - legal_scholars_and_researchers: Observer (analytical/analytical) — analyzes efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.9).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.95).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Authority to Execute for Deterrence").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'fc90b163-4065-4885-8f9a-7da850e86687').
narrative_ontology:cs_kernel_codification('fc90b163-4065-4885-8f9a-7da850e86687', formalized).
narrative_ontology:cs_authority_grounding('fc90b163-4065-4885-8f9a-7da850e86687', extraction).
narrative_ontology:cs_interpretation_layer_present('fc90b163-4065-4885-8f9a-7da850e86687').
narrative_ontology:cs_reading_relation('fc90b163-4065-4885-8f9a-7da850e86687', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('fc90b163-4065-4885-8f9a-7da850e86687', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('fc90b163-4065-4885-8f9a-7da850e86687', foundational, capital_punishment_deters_crime).
narrative_ontology:cs_axiom_status(capital_punishment_deters_crime, holdable).
narrative_ontology:cs_axiom_grounding('fc90b163-4065-4885-8f9a-7da850e86687', capital_punishment_deters_crime, empirically_contingent).
narrative_ontology:cs_axiom('fc90b163-4065-4885-8f9a-7da850e86687', foundational, human_life_can_be_instrumentalized_for_greater_good).
narrative_ontology:cs_axiom_status(human_life_can_be_instrumentalized_for_greater_good, holdable).
narrative_ontology:cs_axiom_grounding('fc90b163-4065-4885-8f9a-7da850e86687', human_life_can_be_instrumentalized_for_greater_good, instrumental).
narrative_ontology:cs_reference_frame('fc90b163-4065-4885-8f9a-7da850e86687', utilitarian_crime_prevention).
narrative_ontology:cs_drift_state('fc90b163-4065-4885-8f9a-7da850e86687', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc90b163-4065-4885-8f9a-7da850e86687', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, society_at_large).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_individuals).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, abolitionist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign power that claims the right to execute individuals, justifying it as a necessary instrument for crime prevention and public safety. It enforces capital punishment through its legal and penal systems.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Individuals sentenced to death. They bear the ultimate cost of the constraint, their lives, with no possibility of exit once the sentence is carried out. Their agency is entirely suppressed by the state's power.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_individuals, payer,
    powerless, immediate, trapped, local).

% The theoretical individuals whose lives are purportedly saved by the deterrent effect of capital punishment. They are diffuse and unidentifiable, benefiting from a perceived reduction in violent crime.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, biographical, constrained, national).

% The broader public that benefits from a perceived sense of security and justice, believing that capital punishment contributes to a safer environment. This benefit is often more symbolic than empirically proven.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, society_at_large, beneficiary,
    moderate, generational, mobile, national).

% Organizations and individuals who actively oppose capital punishment on moral, ethical, or practical grounds. They bear the moral and political cost of its continued existence and are often excluded from the core decision-making processes.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded).

% The family members of condemned individuals, who suffer profound emotional and social costs from the execution. They are directly impacted by the state's action and have virtually no power to alter the outcome.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_families, payer,
    powerless, biographical, trapped, local).

% Academics and researchers who study the efficacy, ethics, and legal implications of capital punishment, often providing empirical data on its deterrent effect and its application. They analyze the constraint's operation without directly participating in its enforcement or suffering its direct costs.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, legal_scholars_and_researchers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate societal efforts towards crime prevention by establishing a perceived ultimate deterrent against heinous murders, thereby fostering a sense of public safety and order.
% TRANSFER_FUNCTION: Transfers the life of the condemned individual from themselves to the state, in exchange for the perceived benefit of preventing future murders and enhancing societal security.
% ABSENT_VOICES: The condemned individuals themselves, whose perspectives are silenced by the act of execution. Their families, who bear immense grief and are often marginalized in public discourse. Abolitionist advocates, who are often dismissed as soft on crime despite presenting evidence against deterrence.
% DISAPPEARANCE_RATIONALE: If the state's authority to execute for deterrence vanished overnight, the criminal justice system would undergo a fundamental philosophical shift. Sentencing guidelines would change, the role of life imprisonment would be re-evaluated, and public discourse on crime and punishment would be forced to confront alternative strategies for safety and justice, leading to a significant reorganization of legal and penal structures.
% FOUNDING_PROBLEM: To establish a punishment severe enough to deter the most heinous crimes, particularly murder, and to protect society from dangerous offenders.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state authorities, some segments of the public) assert that the founding problem of deterring crime remains live, citing the need for ultimate penalties. However, a significant body of independent empirical research and legal scholarship, from outside the directly benefiting parties, widely contests the deterrent efficacy of capital punishment, suggesting the problem is either dead or that the constraint is ineffective in solving it.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.90) because the constraint involves the taking of a human life, the ultimate form of extraction. Suppression is also very high (0.95) as the state wields absolute power over the condemned, with virtually no exit options. The theater ratio, while starting lower, has risen to 0.40, reflecting the growing body of evidence that challenges the empirical claim of deterrence, making the 'deterrence' justification increasingly performative rather than functional. Accessibility collapse is high (0.85) for the condemned, as there are no alternatives to execution once the process is complete. Resistance is high (0.70) due to persistent abolitionist movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   The state authority perceives this constraint as a legitimate and necessary tool for public safety, a coordination mechanism. However, from the perspective of the condemned and abolitionist advocates, it is a pure act of extraction and suppression, with the coordination story serving as a cover for state power. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority is the primary beneficiary and agenda-setter, collecting the 'benefit' of perceived public safety and wielding the power to enforce. Potential future victims and society at large are diffuse beneficiaries. Condemned individuals and their families are the ultimate targets, bearing the full cost. Abolitionist advocates are also targets, bearing the moral and political costs of resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to deter crime. If empirical evidence conclusively demonstrates a lack of deterrent effect, the constraint would be revealed as a Snare, as its coordination function would have atrophied, leaving only extraction. The rising theater ratio and contested founding problem status indicate a drift towards mandatrophy, where the justification becomes increasingly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does capital punishment actually prevent future murders more effectively than life imprisonment, at an acceptable cost?',
    'Longitudinal, cross-jurisdictional empirical studies comparing murder rates in states with and without capital punishment, controlling for other variables. Meta-analyses of existing research.',
    'If deterrence is empirically disproven, the constraint''s coordination function collapses, reclassifying it closer to a Snare. If proven, its Tangled Rope classification is strengthened, though extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Uncertainty regarding the empirical effectiveness of capital punishment as a deterrent.').

omega_variable(
    acceptable_cost_definition,
    'What constitutes an ''acceptable cost'' for preventing future murders, particularly when that cost is a human life?',
    'Public deliberation, legislative debate, and judicial interpretation that explicitly define the ethical and societal boundaries of ''acceptable cost'' in this context, potentially incorporating human rights frameworks.',
    'A stricter definition of ''acceptable cost'' could lead to the constraint being deemed unjustifiable, pushing it towards a Snare or even a foreclosed status. A looser definition would reinforce its current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(acceptable_cost_definition, preference, 'Ambiguity in the ethical and societal definition of ''acceptable cost'' for capital punishment.').

omega_variable(
    instrumental_vs_deontological_conflict,
    'Is it morally permissible to treat a human life instrumentally, as a means to an end (deterrence), regardless of the outcome?',
    'Philosophical and ethical consensus-building within legal and moral philosophy, and its reflection in constitutional and international human rights law.',
    'If a strong deontological consensus against instrumentalizing human life emerges, this reading''s foundational axiom would be challenged, potentially foreclosing it or reclassifying it as a Snare based on a fundamental moral violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_deontological_conflict, conceptual, 'Conflict between utilitarian instrumentalism and deontological ethics regarding human life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1950, state_killing_authority__deterrence_instrument, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(stat_tr_t1970, state_killing_authority__deterrence_instrument, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__deterrence_instrument, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(stat_tr_t2010, state_killing_authority__deterrence_instrument, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1950, state_killing_authority__deterrence_instrument, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(stat_be_t1970, state_killing_authority__deterrence_instrument, base_extractiveness, 1970, 0.88).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__deterrence_instrument, base_extractiveness, 1990, 0.9).
narrative_ontology:measurement(stat_be_t2010, state_killing_authority__deterrence_instrument, base_extractiveness, 2010, 0.91).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1950, state_killing_authority__deterrence_instrument, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement(stat_su_t1970, state_killing_authority__deterrence_instrument, suppression_requirement, 1970, 0.92).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__deterrence_instrument, suppression_requirement, 1990, 0.94).
narrative_ontology:measurement(stat_su_t2010, state_killing_authority__deterrence_instrument, suppression_requirement, 2010, 0.96).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, criminal_justice_sentencing_guidelines).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, prison_industrial_complex).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_authority' kernel. It focuses on the deterrence justification, distinct from retributive desert and categorical abolition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
