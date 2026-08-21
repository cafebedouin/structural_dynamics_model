% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the 'credibility paradox' reading of nuclear
 *   deterrence: the threat of nuclear use, necessary for deterrence, is
 *   inherently incredible because its execution guarantees mutual
 *   destruction. This reading emphasizes the instability of deterrence and
 *   the continuous efforts by nuclear powers to make the incredible credible
 *   through doctrines like counterforce and limited nuclear war. The claimed
 *   type is 'tangled_rope' because it offers a coordination function
 *   (preventing great power war) but with asymmetric extraction (existential
 *   risk to global population, political power to elites) and requires active
 *   enforcement (maintaining arsenals, developing war plans, suppressing
 *   disarmament).
 *
 * KEY AGENTS:
 *   - nuclear_powers_political_elites: Agenda setter (institutional/identity_locked) — benefits from constraint, maintains arsenals
 *   - military_strategists: Beneficiary/Payer (organized/constrained) — plans for use, bears planning burden
 *   - global_population: Payer (powerless/trapped) — bears existential risk
 *   - non_nuclear_states: Payer (moderate/constrained) — subject to nuclear dynamics
 *   - arms_control_advocates: Excluded (organized/constrained) — seeks abolition, marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.65).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.9).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '0f4a8015-a086-40ff-8ac5-3c1d3e961750').
narrative_ontology:cs_kernel_codification('0f4a8015-a086-40ff-8ac5-3c1d3e961750', formalized).
narrative_ontology:cs_authority_grounding('0f4a8015-a086-40ff-8ac5-3c1d3e961750', extraction).
narrative_ontology:cs_interpretation_layer_present('0f4a8015-a086-40ff-8ac5-3c1d3e961750').
narrative_ontology:cs_reading_relation('0f4a8015-a086-40ff-8ac5-3c1d3e961750', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f4a8015-a086-40ff-8ac5-3c1d3e961750', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('0f4a8015-a086-40ff-8ac5-3c1d3e961750', foundational, nuclear_threat_inherently_incredible).
narrative_ontology:cs_axiom_status(nuclear_threat_inherently_incredible, holdable).
narrative_ontology:cs_axiom_grounding('0f4a8015-a086-40ff-8ac5-3c1d3e961750', nuclear_threat_inherently_incredible, deontological).
narrative_ontology:cs_axiom('0f4a8015-a086-40ff-8ac5-3c1d3e961750', secondary, escalation_control_is_illusory).
narrative_ontology:cs_axiom_status(escalation_control_is_illusory, holdable).
narrative_ontology:cs_axiom_grounding('0f4a8015-a086-40ff-8ac5-3c1d3e961750', escalation_control_is_illusory, empirically_contingent).
narrative_ontology:cs_reference_frame('0f4a8015-a086-40ff-8ac5-3c1d3e961750', cold_war_deterrence_doctrine).
narrative_ontology:cs_drift_state('0f4a8015-a086-40ff-8ac5-3c1d3e961750', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f4a8015-a086-40ff-8ac5-3c1d3e961750', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_political_elites).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_population).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategists).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the nuclear arsenals and the doctrine of deterrence, publicly asserting the credibility of nuclear threats while privately exploring limited use scenarios. Their power and status are tied to possessing these weapons, making exit from the nuclear club politically unthinkable.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers_political_elites, agenda_setter,
    institutional, biographical, identity_locked, global).

% Develop and refine nuclear war plans, counterforce capabilities, and escalation ladders, attempting to make the incredible credible. They benefit from the intellectual challenge and institutional funding, but bear the burden of planning for an unwinnable war.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategists, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategists, payer).

% Live under the constant, if often unacknowledged, threat of nuclear annihilation. They bear the ultimate cost of deterrence failure, with no agency in its maintenance or dissolution. Their 'exit' is non-existence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, global_population, payer,
    powerless, immediate, trapped, universal).

% Are subject to the strategic dynamics of nuclear powers without possessing the weapons themselves. They face the risk of becoming proxy battlegrounds or targets of limited nuclear exchanges, and must align with or resist nuclear powers, often at significant cost.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    moderate, generational, constrained, regional).

% Argue for disarmament and the abolition of nuclear weapons, highlighting the inherent instability of deterrence. Their proposals are often dismissed by nuclear powers as naive or destabilizing, keeping them outside the core decision-making loop.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the behavior of nuclear-armed states by establishing a shared understanding that direct military confrontation carries an unacceptable risk of nuclear escalation, thus preventing large-scale conventional wars between them.
% TRANSFER_FUNCTION: Transfers a sense of 'security' (from direct conventional war between nuclear powers) to the nuclear powers' political elites, at the cost of existential risk and resource diversion (to maintain arsenals) from the global population and non-nuclear states.
% ABSENT_VOICES: The global population, who would overwhelmingly reject the existential risk, are absent from the decision-making. Future generations, whose very existence is jeopardized, have no voice. Arms control advocates are marginalized.
% DISAPPEARANCE_RATIONALE: If the credibility paradox vanished (e.g., nuclear weapons became genuinely unusable or disappeared), the strategic landscape would fundamentally shift. Conventional warfare between great powers might become more likely, or a new form of global security architecture would emerge, as the current system is predicated on this paradox.
% FOUNDING_PROBLEM: The problem of preventing large-scale, devastating conventional wars between great powers, particularly after the two World Wars.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers' political elites and military strategists universally attest that the founding problem of great power war remains live, and that nuclear deterrence is the primary mechanism preventing it. Independent historians and international relations scholars corroborate the historical context of great power war prevention.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the 'benefit' of preventing great power war comes at the cost of existential risk for all, and the political elites of nuclear powers gain disproportionate power. Suppression is very high because the system relies on actively preventing alternatives (disarmament) and suppressing dissent. Theater ratio is high because much of the strategic discourse around 'credible threats' and 'limited use' is performative, designed to maintain the illusion of control over an inherently uncontrollable escalation ladder. Accessibility collapse is high because once nuclear weapons exist, the 'alternative' of a non-nuclear world is extremely difficult to reach. Resistance is moderate, as disarmament movements exist but are largely ineffective against institutional power.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers' elites perceive this as a necessary, if dangerous, 'rope' for global stability, where their agency is paramount. The global population experiences it as a 'snare' or 'mountain' of existential threat, with no agency. Military strategists operate within the paradox, attempting to rationalize the irrational, experiencing it as a 'tangled_rope' of professional duty and inherent contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers' political elites are beneficiaries (d=0.0-0.1) as their power is enhanced and direct conflict averted. The global population and non-nuclear states are victims (d=0.9-1.0) as they bear the existential risk and costs without agency. Military strategists are complex: beneficiaries of institutional funding/purpose, but also targets of the paradox's demands (d=0.4-0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing great power war) is still live, but its operational mechanism (credible threat of incredible use) is inherently contradictory. The high theater ratio and suppression indicate that the system persists not purely by its coordination function, but by active maintenance of the paradox and suppression of alternatives. This prevents mislabeling it as a pure 'rope' (ignoring extraction) or a pure 'snare' (ignoring the coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_vs_unthinkability,
    'Is the ''unthinkability'' of nuclear war a structural reality (making threats incredible) or a rhetorical construct (allowing for limited use scenarios)?',
    'Analysis of declassified strategic planning documents and military exercises: if plans consistently include limited nuclear options, ''unthinkability'' is rhetorical. If all plans lead to unavoidable escalation, it''s structural.',
    'If rhetorical, the constraint is more extractive (elites actively maintain the illusion of control). If structural, it''s closer to a mountain (inherent physical/logical limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_vs_unthinkability, empirical, 'Distinguishing between rhetorical and structural ''unthinkability'' of nuclear war.').

omega_variable(
    escalation_control_feasibility,
    'Are ''escalation ladders'' and ''limited nuclear war'' concepts genuinely controllable, or do they inevitably lead to full-scale exchange?',
    'Historical case studies of near-misses (e.g., Cuban Missile Crisis) and wargame simulations: if control is consistently lost, the concepts are theatrical. If control is maintained, they are genuine strategic tools.',
    'If uncontrollable, the theater_ratio is higher, and the constraint is more of a snare (the coordination story is cover). If controllable, it''s closer to a tangled_rope (genuine, albeit dangerous, coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_feasibility, empirical, 'The actual feasibility of controlling nuclear escalation in a crisis.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''nuclear_impossibility_kernel''. What would change if a sibling reading (e.g., ''structural_contraction_reading'') were adopted?',
    'Conceptual analysis of the logical implications of each reading for strategic doctrine and policy. The ''structural_contraction_reading'' would imply a lower extractiveness and higher accessibility_collapse for disarmament, as the physical impossibility of victory would be universally acknowledged.',
    'Adopting the ''structural_contraction_reading'' would shift this constraint towards a ''mountain'' or ''rope'' for disarmament, as the inherent impossibility of victory would reduce the perceived utility of maintaining arsenals, lowering extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of adopting a sibling reading of the nuclear impossibility kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.6).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1980, 0.75).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.65).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.75).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.95).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.8).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
