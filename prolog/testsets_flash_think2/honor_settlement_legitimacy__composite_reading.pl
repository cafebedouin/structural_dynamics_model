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
 *   human_readable: Decline of Dueling as Legitimate Honor Settlement (Composite Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint, the 'Decline of Dueling as Legitimate Honor Settlement,'
 *   is presented as a 'composite_reading' of the
 *   'honor_settlement_legitimacy' kernel. It argues that dueling's decline
 *   was overdetermined by multiple reinforcing mechanisms. While cultural
 *   unthinkability (contraction) played a dominant role, it was significantly
 *   reinforced by material and institutional changes, such as legal
 *   prohibitions and the rise of state authority, which would have
 *   independently suppressed the practice. The constraint is classified as a
 *   Tangled Rope because it coordinated a new social order (peace, state
 *   monopoly on violence) while extracting the traditional right to duel from
 *   honor culture adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.88).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.9).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Decline of Dueling as Legitimate Honor Settlement (Composite Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '0adf673a-8a92-4e65-b346-43accda65058').
narrative_ontology:cs_kernel_codification('0adf673a-8a92-4e65-b346-43accda65058', implicit).
narrative_ontology:cs_authority_grounding('0adf673a-8a92-4e65-b346-43accda65058', practice).
narrative_ontology:cs_interpretation_layer_present('0adf673a-8a92-4e65-b346-43accda65058').
narrative_ontology:cs_reading_relation('0adf673a-8a92-4e65-b346-43accda65058', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('0adf673a-8a92-4e65-b346-43accda65058', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('0adf673a-8a92-4e65-b346-43accda65058', foundational, honor_redefined_beyond_combat).
narrative_ontology:cs_axiom_status(honor_redefined_beyond_combat, holdable).
narrative_ontology:cs_axiom_grounding('0adf673a-8a92-4e65-b346-43accda65058', honor_redefined_beyond_combat, deontological).
narrative_ontology:cs_axiom('0adf673a-8a92-4e65-b346-43accda65058', foundational, state_monopoly_on_violence_is_legitimate).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0adf673a-8a92-4e65-b346-43accda65058', state_monopoly_on_violence_is_legitimate, conventional).
narrative_ontology:cs_axiom('0adf673a-8a92-4e65-b346-43accda65058', foundational, cultural_and_material_factors_reinforce_each_other).
narrative_ontology:cs_axiom_status(cultural_and_material_factors_reinforce_each_other, holdable).
narrative_ontology:cs_axiom_grounding('0adf673a-8a92-4e65-b346-43accda65058', cultural_and_material_factors_reinforce_each_other, empirically_contingent).
narrative_ontology:cs_reference_frame('0adf673a-8a92-4e65-b346-43accda65058', state_monopoly_on_violence_and_peaceful_dispute_resolution).
narrative_ontology:cs_drift_state('0adf673a-8a92-4e65-b346-43accda65058', late_19th_century_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0adf673a-8a92-4e65-b346-43accda65058', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, new_social_order).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, peaceful_citizens).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, residual_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose social identity and self-worth were deeply tied to the honor code that legitimized dueling. They faced increasing legal penalties, social ostracization, and cultural pressure to abandon the practice, making their preferred method of dispute resolution costly and illegitimate.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, national).

% Those who continued to engage in dueling despite its illegality and social stigma. They bore the direct costs of legal prosecution, social ruin, and physical danger, with no legitimate recourse for honor settlement through combat.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, residual_duelists, payer,
    powerless, immediate, trapped, local).

% The evolving legal and judicial apparatus that progressively criminalized dueling, enforced bans, and established a monopoly on legitimate violence. It benefited from increased authority and social order.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Intellectuals, moralists, and influential public figures who actively shaped the cultural narrative, redefining honor, promoting peaceful dispute resolution, and stigmatizing dueling as barbaric. They drove the 'contraction edge' of the decline.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, cultural_elites, agenda_setter,
    powerful, generational, mobile, national).

% The broader populace who benefited from a reduction in interpersonal violence, increased public safety, and a more stable social order where disputes were resolved through legal or social arbitration rather than combat.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, peaceful_citizens, beneficiary,
    organized, biographical, mobile, local).

% The emergent societal structure that had largely abandoned dueling, embracing state authority and non-violent means of conflict resolution. It benefited from the stability and legitimacy derived from the decline of dueling.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, new_social_order, beneficiary,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a state monopoly on violence and coordinated society towards peaceful, legalistic methods of dispute resolution, reducing interpersonal violence and challenges to state authority.
% TRANSFER_FUNCTION: Transferred the right and social legitimacy to settle honor disputes via combat from individuals to the state's legal system and social arbitration, along with the associated social capital and power.
% ABSENT_VOICES: Those who, often from marginalized or traditionalist segments of society, continued to believe in the necessity of dueling for upholding personal honor. Their perspectives were increasingly excluded from mainstream discourse and legal frameworks.
% DISAPPEARANCE_RATIONALE: If the illegitimacy of dueling and the associated legal/social enforcement vanished overnight, the state's monopoly on violence would be fundamentally challenged, and social norms around conflict resolution would revert towards more individualistic and potentially violent forms, reorganizing the social contract.
% FOUNDING_PROBLEM: The problem of unchecked interpersonal violence, frequent challenges to state authority, and a perceived barbaric practice in an increasingly 'civilized' society that sought to rationalize and centralize power.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars, and sociologists corroborate the historical problem of dueling's violence and its challenge to state authority. While dueling itself is largely a dead practice, the underlying problem of managing violence and maintaining state authority remains live, albeit manifested in different forms.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.88) reflects the severe costs imposed on those who continued to adhere to dueling, including legal penalties and social ostracization. Suppression (0.9) was equally high due to active legal enforcement, social stigma, and a profound cultural shift that made dueling unthinkable. The theater ratio is low (0.1) because the decline was a genuine, structural transformation, not merely performative. Resistance was moderate initially but declined as the reinforcing mechanisms took hold, leading to high accessibility collapse (0.9) for dueling as a viable option. The temporal measurements reflect a steady increase in both extractiveness and suppression over the 200-year period as the new social order consolidated its authority and cultural norms shifted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state legal system and peaceful citizens, the decline of dueling was a beneficial coordination mechanism, reducing violence and enhancing social order. However, for honor culture adherents and residual duelists, the same constraint operated as a highly extractive and suppressive force, stripping them of a culturally significant means of dispute resolution and identity expression. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system, cultural elites, and peaceful citizens are beneficiaries, as they gained from the reduction of violence and the consolidation of state authority (low directionality). Honor culture adherents and residual duelists are victims, as they lost the right and legitimacy to duel, facing severe penalties and social costs (high directionality). The 'identity_locked' exit option for honor culture adherents reflects how deeply their self-concept was tied to the practice, making exit from the old code extremely difficult.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_causal_weight,
    'What was the precise relative weight of cultural contraction versus legal/institutional changes in driving dueling''s decline?',
    'Detailed historical counterfactual analysis, comparing regions with differing rates of legal enforcement or cultural shifts, or quantitative historical sociology studies.',
    'If legal/institutional factors are found to be more dominant than cultural contraction, the constraint''s classification might lean more towards a Snare (purely coercive) rather than a Tangled Rope (coordination + extraction), as the ''coordination'' aspect of cultural shift would be less central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_causal_weight, empirical, 'Ambiguity in the precise contribution of different reinforcing mechanisms.').

omega_variable(
    identity_lock_persistence,
    'How long did the identity-lock of honor culture adherents persist after dueling became illegal and culturally unthinkable?',
    'Analysis of personal diaries, memoirs, and subcultural practices of residual honor groups, tracing the internal experience of identity and social pressure.',
    'If identity-lock persisted for a very long time, it suggests a higher effective suppression and extractiveness for those individuals, even in the absence of overt enforcement, amplifying their victim status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Duration and intensity of identity-based resistance to the constraint.').

omega_variable(
    counterfactual_decline_path,
    'If only one mechanism (e.g., legal ban without cultural shift, or vice versa) had been present, would dueling have declined as completely and rapidly?',
    'Comparative historical analysis of societies where only partial mechanisms were implemented, or theoretical modeling of social change dynamics.',
    'If a single mechanism would have been sufficient, it would challenge the ''overdetermined'' aspect of this composite reading, potentially strengthening the case for one of the sibling readings (e.g., pure contraction or pure legal suppression).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_decline_path, conceptual, 'Uncertainty about the necessity of multiple reinforcing mechanisms for the observed decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__composite_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1740, honor_settlement_legitimacy__composite_reading, theater_ratio, 1740, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_settlement_legitimacy__composite_reading, theater_ratio, 1780, 0.1).
narrative_ontology:measurement(hono_tr_t1820, honor_settlement_legitimacy__composite_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__composite_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1740, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1740, 0.55).
narrative_ontology:measurement(hono_be_t1780, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1780, 0.7).
narrative_ontology:measurement(hono_be_t1820, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1820, 0.8).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1860, 0.85).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1740, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1740, 0.45).
narrative_ontology:measurement(hono_su_t1780, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1780, 0.6).
narrative_ontology:measurement(hono_su_t1820, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1820, 0.75).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
