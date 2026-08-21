% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'negative liberty' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), which states that
 *   'Everyone has the right to life, liberty and security of person.' This
 *   reading interprets Article 3 primarily as a prohibition on state action,
 *   meaning states must refrain from arbitrarily depriving individuals of
 *   these rights. It emphasizes freedom *from* state interference, requiring
 *   strict procedural justice for any state action that might impinge on life
 *   or liberty. This interpretation leads to high extraction from state
 *   power, advocating for capital punishment abolition, restrictive
 *   self-defense doctrines for states, and expansive due process rights for
 *   individuals.
 *
 * KEY AGENTS:
 *   - individuals: Primary beneficiary (powerless/identity_locked)
 *   - state_security_apparatus: Primary payer/victim (institutional/trapped)
 *   - human_rights_advocates: Beneficiary/enforcer (organized/mobile)
 *   - national_governments: Agenda-setter/payer (institutional/constrained)
 *   - international_courts: Observer/agenda-setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.75).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.8).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3: Negative Liberty Reading (Freedom from State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '1097e5f4-e211-412d-8ab0-2e9d82908976').
narrative_ontology:cs_kernel_codification('1097e5f4-e211-412d-8ab0-2e9d82908976', fixed_text).
narrative_ontology:cs_authority_grounding('1097e5f4-e211-412d-8ab0-2e9d82908976', lineage).
narrative_ontology:cs_interpretation_layer_present('1097e5f4-e211-412d-8ab0-2e9d82908976').
narrative_ontology:cs_reading_relation('1097e5f4-e211-412d-8ab0-2e9d82908976', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1097e5f4-e211-412d-8ab0-2e9d82908976', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1097e5f4-e211-412d-8ab0-2e9d82908976', foundational, state_non_interference_primacy).
narrative_ontology:cs_axiom_status(state_non_interference_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1097e5f4-e211-412d-8ab0-2e9d82908976', state_non_interference_primacy, deontological).
narrative_ontology:cs_axiom('1097e5f4-e211-412d-8ab0-2e9d82908976', foundational, individual_autonomy_absolute).
narrative_ontology:cs_axiom_status(individual_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1097e5f4-e211-412d-8ab0-2e9d82908976', individual_autonomy_absolute, deontological).
narrative_ontology:cs_reference_frame('1097e5f4-e211-412d-8ab0-2e9d82908976', post_wwii_human_rights_consensus).
narrative_ontology:cs_drift_state('1097e5f4-e211-412d-8ab0-2e9d82908976', contemporary_counter_terrorism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1097e5f4-e211-412d-8ab0-2e9d82908976', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, human_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, proponents_of_capital_punishment).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, proponents_of_unfettered_state_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of protection from arbitrary state deprivation of life and liberty. Their security and autonomy are enhanced by the constraint, but they rely entirely on state and international enforcement for its realization.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% Bears the costs of the constraint by having its power to use force, detain, or execute severely limited by procedural justice requirements. Must operate within strict legal frameworks, which can be seen as an impediment to 'efficient' security operations.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, immediate, trapped, national).

% Benefit from the constraint as it provides a foundational legal and moral basis for their work in monitoring state actions, litigating abuses, and campaigning for stronger protections. They actively enforce and interpret the constraint.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of the constraint's expansive due process requirements and its interpretation towards abolition, which limits their ability to implement or advocate for capital punishment.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, proponents_of_capital_punishment, payer,
    moderate, biographical, constrained, national).

% Are responsible for implementing and enforcing Article 3 within their jurisdictions. While they are the primary enforcers, they also bear the costs of the constraint by having their sovereign power limited, particularly in areas of security and criminal justice.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Interpret and apply Article 3 in cases of state violations, setting precedents and influencing national legal systems. They act as a check on state power and a forum for redress, but their enforcement relies on state cooperation.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_courts, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, international_courts, agenda_setter).

% Bear the cost of the constraint by having their ideological position challenged and their policy preferences (e.g., state's right to act without external human rights limitations) curtailed by international law.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, proponents_of_unfettered_state_sovereignty, payer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, individuals).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for individual freedom from arbitrary state power, coordinating expectations between states and citizens regarding the legitimate exercise of state coercion.
% TRANSFER_FUNCTION: Transfers power and discretion from the state (especially its coercive arms) to individuals, limiting the state's ability to deprive life or liberty without strict adherence to procedural justice.
% ABSENT_VOICES: Proponents of absolute state sovereignty, those who prioritize collective security over individual rights without due process, and those who believe in the state's inherent right to use force without external limitations are structurally excluded from setting the terms of this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, states would have unchecked power over life and liberty, leading to widespread arbitrary detention, extrajudicial killings, torture, and a collapse of human rights norms, fundamentally reorganizing global governance and individual security.
% FOUNDING_PROBLEM: The historical prevalence of arbitrary state violence, torture, and deprivation of life/liberty without legal recourse, particularly evident in the atrocities of World War II and totalitarian regimes.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and victims of state abuses consistently attest to the ongoing relevance of this prohibition, citing contemporary instances of state overreach, extrajudicial killings, and arbitrary detention across the globe. UN reports and NGO documentation provide external corroboration.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant limitations placed on state power, particularly its coercive functions, by this reading. The high suppression (0.80) indicates the active and continuous enforcement required to prevent states from overstepping these boundaries, often against political pressures to expand security powers. The low theater ratio (0.10) suggests that the constraint is genuinely functional, with enforcement efforts directly aimed at upholding the prohibition, rather than merely performing compliance. Accessibility collapse is high (0.85) for states seeking to act outside due process, as legal and normative alternatives are severely restricted. Resistance is moderate (0.40) as states often push back against strict interpretations, especially in times of perceived crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals and human rights advocates, this constraint is a vital 'rope' ensuring fundamental freedoms. From the perspective of state security apparatuses and proponents of unfettered state sovereignty, it operates as a 'snare' or 'tangled rope,' severely limiting their operational capacity and ideological preferences. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals are full beneficiaries (low d) as the constraint directly protects their life and liberty. Human rights advocates also benefit by having a strong legal framework for their work. State security apparatuses and proponents of capital punishment are targets (high d) as the constraint directly limits their power and policy objectives. National governments, while agenda-setters, also bear costs as their sovereign power is constrained. International courts act as analytical observers and secondary agenda-setters.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_state_deprivation,
    'What specific state actions constitute ''deprivation of life, liberty, or security of person'' under this reading (e.g., economic sanctions, environmental negligence, drone strikes, surveillance)?',
    'Further international jurisprudence, state practice, and scholarly consensus on the interpretation of ''deprivation'' in contemporary contexts.',
    'An expansive interpretation would increase the constraint''s effective extractiveness on states; a narrow interpretation would reduce it, potentially shifting the classification towards a weaker form of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_state_deprivation, conceptual, 'Ambiguity in the scope of prohibited state actions.').

omega_variable(
    balancing_collective_security,
    'How does this negative liberty reading balance individual rights against legitimate state needs for collective security, especially in times of crisis or conflict?',
    'Development of clear, internationally recognized proportionality tests and derogation clauses that are strictly applied and subject to independent review.',
    'If collective security concerns are frequently allowed to override individual rights, the constraint''s effective suppression and extractiveness would be lower than currently assessed, potentially weakening its classification. If individual rights are consistently prioritized, the current assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_collective_security, preference, 'Tension between individual rights and collective security.').

omega_variable(
    reading_ambiguity_udhr_article_3,
    'Is this constraint a genuine negative liberty principle, or is its framing as such a cover for other structural dynamics?',
    'Analysis of how this reading interacts with the ''positive_entitlement_reading'' and ''procedural_hybrid_reading'' of UDHR Article 3, and whether its enforcement consistently prioritizes non-interference over other state obligations.',
    'If the negative liberty framing is found to obscure unacknowledged positive obligations or procedural gaps, the constraint''s true nature might be a ''tangled_rope'' with different beneficiaries/victims, or even a ''snare'' if the procedural justice is merely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ambiguity_udhr_article_3, conceptual, 'This constraint is one reading of the UDHR Article 3 kernel; its structural properties are contingent on this specific interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__negative_liberty_reading, theater_ratio, 1968, 0.11).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__negative_liberty_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__negative_liberty_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__negative_liberty_reading, base_extractiveness, 1968, 0.65).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__negative_liberty_reading, base_extractiveness, 1988, 0.7).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__negative_liberty_reading, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__negative_liberty_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__negative_liberty_reading, suppression_requirement, 1988, 0.75).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__negative_liberty_reading, suppression_requirement, 2008, 0.78).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_5__torture_prohibition).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_6__right_to_life).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, geneva_conventions__civilian_protection).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the UDHR Article 3 kernel. Its structural properties and classification differ significantly from the 'positive_entitlement_reading' and 'procedural_hybrid_reading' due to differing interpretations of state obligations and individual rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
