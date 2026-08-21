% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Decline of Dueling: Composite Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint models the decline of dueling as an overdetermined
 *   process, where multiple reinforcing mechanisms (legal prohibition,
 *   changing social norms, economic shifts) converged to make the practice
 *   increasingly untenable. This 'composite reading' emphasizes that while
 *   cultural contraction (dueling becoming unthinkable) was dominant, it was
 *   significantly reinforced by material and institutional changes that would
 *   have independently suppressed the practice. The constraint is claimed as
 *   a Tangled Rope because it initially served a coordination function for
 *   honor, but its decline involved increasing extraction (penalties) and
 *   suppression by the state and emerging bourgeois society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.78).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Decline of Dueling: Composite Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '7ddcba0a-2986-4ae2-a916-6731a6eb5676').
narrative_ontology:cs_kernel_codification('7ddcba0a-2986-4ae2-a916-6731a6eb5676', implicit).
narrative_ontology:cs_authority_grounding('7ddcba0a-2986-4ae2-a916-6731a6eb5676', practice).
narrative_ontology:cs_interpretation_layer_present('7ddcba0a-2986-4ae2-a916-6731a6eb5676').
narrative_ontology:cs_reading_relation('7ddcba0a-2986-4ae2-a916-6731a6eb5676', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('7ddcba0a-2986-4ae2-a916-6731a6eb5676', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('7ddcba0a-2986-4ae2-a916-6731a6eb5676', foundational, decline_is_multi_causal).
narrative_ontology:cs_axiom_status(decline_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('7ddcba0a-2986-4ae2-a916-6731a6eb5676', decline_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('7ddcba0a-2986-4ae2-a916-6731a6eb5676', secondary, cultural_contraction_is_dominant).
narrative_ontology:cs_axiom_status(cultural_contraction_is_dominant, holdable).
narrative_ontology:cs_axiom_grounding('7ddcba0a-2986-4ae2-a916-6731a6eb5676', cultural_contraction_is_dominant, empirically_contingent).
narrative_ontology:cs_reference_frame('7ddcba0a-2986-4ae2-a916-6731a6eb5676', honor_code_as_legitimate_dispute_resolution).
narrative_ontology:cs_drift_state('7ddcba0a-2986-4ae2-a916-6731a6eb5676', late_19th_century_europe, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7ddcba0a-2986-4ae2-a916-6731a6eb5676', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_public_sphere).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, dueling_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose social standing and self-concept were tied to the honor code, for whom dueling was a legitimate means of dispute resolution. They faced increasing legal and social penalties for upholding this tradition.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, local).

% The evolving legal system that increasingly criminalized dueling, imposing fines, imprisonment, and social ostracization. It sought to monopolize legitimate violence and enforce a new public order.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% The emerging social order that valued rational discourse, legal process, and state-sanctioned justice over personal violence. It benefited from the decline of dueling as it solidified its own norms of civility and dispute resolution.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_public_sphere, beneficiary,
    organized, generational, mobile, regional).

% Those directly involved in duels, facing the immediate physical risks and the escalating legal consequences. Their options were to participate and risk severe penalties, or to suffer social dishonor within their subculture.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, dueling_participants, payer,
    powerless, immediate, trapped, local).

% Scholars who analyze the complex interplay of legal, social, and cultural factors that led to dueling's decline, seeking to understand the overdetermined nature of this historical shift.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes of honor and maintaining social hierarchy within specific aristocratic and military subcultures.
% TRANSFER_FUNCTION: Transferred social legitimacy and status to those who successfully navigated the honor code, while transferring risk of death or injury to participants and legal/social penalties from the state to those who engaged in dueling.
% ABSENT_VOICES: Victims of dueling (those killed or injured) and their families, who had no legal recourse within the honor system. Also, early proponents of state monopoly on violence and bourgeois civility, who were initially marginalized but eventually became dominant.
% DISAPPEARANCE_RATIONALE: If the composite mechanisms reinforcing dueling's decline had not emerged, the social landscape of honor, dispute resolution, and state authority would be fundamentally different. The state's monopoly on violence would be weaker, and personal honor would still be adjudicated through extra-legal means, leading to a very different social order.
% FOUNDING_PROBLEM: The need for a formalized system to resolve disputes of honor and maintain social standing among elites, where state legal systems were perceived as inadequate or irrelevant for such matters.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and legal scholars widely corroborate that the original problem of honor-based dispute resolution has been superseded by state legal systems and bourgeois social norms. The state legal apparatus and the bourgeois public sphere attest that the problem is dead, having been replaced by more 'civilized' forms of justice. Honor culture adherents, however, might contest this, arguing for a residual, if marginalized, need for such mechanisms.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the state actively imposed severe penalties on dueling, extracting social and economic costs from participants. Suppression is also high (0.78) due to the combined force of legal prohibition, social ostracization, and the erosion of the cultural framework that legitimized dueling. Theater ratio is low (0.15) because the decline was a genuine shift, not merely performative; the mechanisms actively worked to dismantle the practice. The slight dip in extractiveness and suppression by 1900 reflects that dueling had largely become a fringe activity, requiring less active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor culture adherents, the decline was a loss of a legitimate means of self-defense and status maintenance, an imposition of an alien legal order. From the state's perspective, it was a necessary step to establish a monopoly on violence and civil order. The composite reading attempts to integrate these perspectives by showing the reinforcing nature of the mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor culture adherents and dueling participants were increasingly targets (high d) as the state and bourgeois society extracted costs from their adherence to the old system. The state legal apparatus and the bourgeois public sphere were beneficiaries (low d), gaining legitimacy and social order from dueling's decline. Cultural historians are observers (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to suppress dueling) was initially live and became increasingly effective. The 'dead' status of the founding problem (the need for honor-based dispute resolution) combined with the 'world_rearranges' verdict for disappearance indicates that the constraint successfully transformed the social landscape, making its original 'problem' obsolete by replacing it with new forms of social order and legal authority. This prevents mislabeling it as a Piton, as the mechanisms were actively functional in achieving their goal, even if the original coordination function of dueling itself atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_causal_weight,
    'What was the precise relative causal weight of cultural contraction versus legal/material suppression in dueling''s decline?',
    'Detailed comparative historical analysis across different national contexts with varying legal enforcement timelines and cultural shifts, using counterfactual modeling.',
    'A higher weight for cultural contraction would push the classification closer to a Mountain (internalized shift), while a higher weight for legal/material suppression would reinforce the Tangled Rope classification (external enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_causal_weight, empirical, 'Determining the dominant causal pathway for dueling''s decline.').

omega_variable(
    identity_lock_persistence,
    'To what extent did ''identity_locked'' exit options persist for honor culture adherents even after dueling became legally and socially untenable?',
    'Analysis of personal memoirs, correspondence, and legal records of individuals who continued to adhere to honor codes despite severe penalties, examining their internal justifications and social networks.',
    'Stronger evidence of persistent identity lock would increase the effective suppression and extraction for this group, reinforcing the Snare-like aspects of the constraint for them, even as the overall practice declined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Measuring the persistence of identity-based adherence to dueling norms.').

omega_variable(
    framing_of_legitimacy,
    'Is the ''honor_settlement_legitimacy'' kernel primarily about the legitimacy of dueling as a practice, or the legitimacy of the state''s monopoly on violence?',
    'Conceptual analysis of historical legal texts and philosophical arguments regarding state sovereignty and individual rights to self-defense.',
    'Framing it as primarily about dueling''s legitimacy supports the Tangled Rope classification. Framing it as primarily about state legitimacy might shift the focus to a Mountain (state sovereignty) or Snare (state coercion) depending on the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_legitimacy, conceptual, 'Clarifying the core subject of the ''honor_settlement_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__composite_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__composite_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__composite_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1750, 0.5).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, bourgeois_civility_norms).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel. This 'composite_reading' emphasizes the overdetermined nature of dueling's decline, integrating cultural contraction with legal and material suppression. It is linked to the 'contraction_reading' (focus on cognitive unthinkability) and 'drop_reading' (focus on residual fringe practice) as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
