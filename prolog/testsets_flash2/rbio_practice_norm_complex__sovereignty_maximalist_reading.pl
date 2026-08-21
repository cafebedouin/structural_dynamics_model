% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Norms (Sovereignty Maximalist Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty maximalist' reading of the
 *   RBIO (Rules-Based International Order) practice-norm complex. In this
 *   reading, state sovereignty is considered absolute, and any external
 *   interference, including humanitarian intervention, is viewed as
 *   illegitimate and a pretext for regime change. RBIO norms are only
 *   legitimate insofar as they protect this absolute sovereignty. This
 *   reading is a Snare, as it primarily serves to protect authoritarian
 *   regimes from accountability, extracting from populations trapped under
 *   repressive governments. The metrics reflect high extraction and
 *   suppression, with a significant theatrical component as 'sovereignty
 *   protection' often masks internal repression.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: Primary beneficiary (institutional/arbitrage) — shielded from accountability
 *   - populations_under_repressive_governments: Primary victim (powerless/trapped) — denied external recourse
 *   - human_rights_advocates: Payer (moderate/constrained) — efforts undermined by this reading
 *   - liberal_institutionalists: Excluded (institutional/analytical) — their alternative framing is dismissed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.9).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Norms (Sovereignty Maximalist Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8').
narrative_ontology:cs_kernel_codification('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', formalized).
narrative_ontology:cs_authority_grounding('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', extraction).
narrative_ontology:cs_interpretation_layer_present('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8').
narrative_ontology:cs_reading_relation('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', foundational, absolute_state_sovereignty).
narrative_ontology:cs_axiom_status(absolute_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', absolute_state_sovereignty, conventional).
narrative_ontology:cs_axiom('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', foundational, humanitarian_intervention_as_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_as_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', humanitarian_intervention_as_regime_change_pretext, instrumental).
narrative_ontology:cs_reference_frame('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7991b26b-ee92-4ce7-9dd4-d3a8f11b30d8', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the absolute protection of state sovereignty, which shields them from external interference regarding internal human rights abuses. They actively promote and enforce this reading of RBIO norms to maintain their power and legitimacy.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% As the ruling class within authoritarian states, they directly benefit from the non-interference principle, which allows them to consolidate wealth and power without accountability to international norms or external pressure.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the full cost of this constraint, as it denies them any legitimate external recourse or protection against severe human rights violations by their own governments. Their options are limited to internal resistance, which is often met with overwhelming force.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, immediate, trapped, national).

% Work to promote universal human rights but find their efforts undermined by the sovereignty maximalist reading, which delegitimizes humanitarian intervention and external pressure. They face significant resistance and often operate with limited influence.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, payer,
    moderate, generational, constrained, global).

% Advocate for a rules-based international order with legitimate multilateral processes for intervention, but their arguments are dismissed as pretexts for regime change by this reading. They are structurally excluded from the discourse that shapes this constraint.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutionalists, excluded,
    institutional, generational, analytical, global).

% Are observed by this reading as potential external interferers, whose actions are viewed with suspicion as attempts to undermine sovereignty for their own strategic gain. Their interventions, even if framed as humanitarian, are seen as illegitimate.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_powers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior around the principle of non-interference in internal affairs, aiming to prevent external aggression and maintain international stability by respecting national borders and internal political systems.
% TRANSFER_FUNCTION: Transfers absolute authority over internal governance from international norms or external actors to the sovereign state, effectively transferring the right to self-determination from populations to their ruling regimes.
% ABSENT_VOICES: Populations suffering under repressive regimes, who would advocate for a right to external protection or intervention, are systematically excluded from the international discourse that legitimizes this reading of sovereignty. Their voices are suppressed by the very regimes that benefit from this constraint.
% DISAPPEARANCE_RATIONALE: If this maximalist reading of sovereignty vanished, it would fundamentally alter the international system. Authoritarian regimes would lose their primary shield against external pressure, leading to increased calls for intervention and potentially destabilizing internal political orders. Human rights would gain a stronger international enforcement mechanism, and the balance of power in international relations would shift dramatically.
% FOUNDING_PROBLEM: The founding problem was to prevent interstate aggression and maintain peace by establishing clear boundaries of national jurisdiction and non-interference after centuries of wars of conquest and intervention.
% FOUNDING_PROBLEM_CORROBORATION: Authoritarian regimes and their allies attest that the problem of external interference remains live, citing historical examples of colonialism and neo-colonialism. Liberal institutionalists and human rights advocates argue that while interstate aggression remains a concern, the absolute interpretation of sovereignty has become a pretext for internal repression, and the original problem has evolved beyond simple border protection.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the cost borne by populations denied protection, while the high suppression (0.90) indicates the active enforcement of non-interference by states benefiting from this reading, often through diplomatic blocking and rhetorical counter-attacks. The theater ratio (0.60) is significant because the 'protection of sovereignty' often serves as a performative justification for actions that are primarily about maintaining regime power, rather than genuine national security. The increasing trend in extractiveness and suppression over time reflects the hardening of this position in response to growing calls for human rights accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this constraint is a legitimate Rope, ensuring stability and national self-determination. From the perspective of victim populations and human rights advocates, it is a Snare, enabling severe human rights abuses under the guise of non-interference. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and state elites are clear beneficiaries (low d) as the constraint directly protects their power. Populations under repressive governments are clear targets (high d) as they bear the costs of denied protection. Human rights advocates are also targets, as their efforts are actively resisted. Liberal institutionalists are excluded, meaning their d is effectively high due to their inability to influence the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling this reading as a legitimate coordination mechanism. While it coordinates non-interference, its primary function has drifted from preventing interstate aggression to shielding internal repression. The high extractiveness and suppression, coupled with the identified victims, clearly mark it as a Snare, not a Rope, despite its claimed coordination function. The 'humanitarian exceptions are pretexts' argument is a key rhetorical tool for maintaining this Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' in this context an absolute, indivisible right of the state, or a conditional responsibility to its population?',
    'Analysis of state practice and international legal interpretations that prioritize human security over state security in cases of mass atrocities, or a shift in the UN Charter''s interpretation.',
    'If sovereignty is conditional, the extractiveness of this reading would be re-evaluated downwards, as the state''s right to non-interference would be limited by its obligations to its people. This would weaken the Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Ambiguity in the foundational definition of state sovereignty.').

omega_variable(
    humanitarian_intervention_pretext_empirical_status,
    'To what extent have ''humanitarian interventions'' genuinely been pretexts for regime change or resource extraction, as opposed to genuine attempts to protect populations?',
    'Empirical case studies and historical analysis of past interventions, evaluating their stated goals against their actual outcomes and motivations.',
    'If a significant number of interventions are found to be genuine, it would undermine a core axiom of this reading, potentially reducing its legitimacy and the perceived ''theater'' of its claims. If they are consistently pretexts, it would reinforce the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_pretext_empirical_status, empirical, 'Empirical status of humanitarian intervention as a pretext.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the RBIO practice-norm complex, or a strategic misrepresentation designed to serve specific state interests?',
    'Comparative analysis of state diplomatic discourse, voting patterns in international bodies, and domestic political rhetoric across different states and over time, to identify consistent adherence to the stated principles versus opportunistic invocation.',
    'If it''s a strategic misrepresentation, the ''claimed_type'' as a Rope (from the perspective of its proponents) would be further exposed as a cover for a Snare, strengthening the engine''s divergence signal. If it''s a genuine, albeit problematic, reading, it highlights a deep ideological cleavage within the international order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether this reading is a genuine interpretation or a strategic misrepresentation of the RBIO kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_criminal_justice_jurisdiction).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the RBIO practice-norm complex kernel. The 'liberal_institutional_reading' views RBIO norms as universal and consent-based, while the 'hegemonic_extraction_reading' sees them as a frozen hegemonic project. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
