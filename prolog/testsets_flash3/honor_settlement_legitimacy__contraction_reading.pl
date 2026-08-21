% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'contraction reading' of the decline of
 *   dueling, arguing that dueling became cognitively unthinkable due to a
 *   fundamental transformation of cultural frameworks. It is not merely that
 *   dueling was outlawed or fell out of fashion, but that the very concept of
 *   settling disputes through personal combat became alien and illegitimate
 *   within the dominant cultural understanding. This reading emphasizes the
 *   deep, almost 'natural' quality of this shift, making the constraint
 *   operate like a Mountain from the perspective of modern society. The
 *   beneficiaries are the institutions and social structures that thrive
 *   under this new cultural framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '95b23642-64aa-4758-a68d-33fcbf5281bf').
narrative_ontology:cs_kernel_codification('95b23642-64aa-4758-a68d-33fcbf5281bf', implicit).
narrative_ontology:cs_authority_grounding('95b23642-64aa-4758-a68d-33fcbf5281bf', practice).
narrative_ontology:cs_interpretation_layer_present('95b23642-64aa-4758-a68d-33fcbf5281bf').
narrative_ontology:cs_reading_relation('95b23642-64aa-4758-a68d-33fcbf5281bf', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('95b23642-64aa-4758-a68d-33fcbf5281bf', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('95b23642-64aa-4758-a68d-33fcbf5281bf', foundational, private_violence_is_illegitimate).
narrative_ontology:cs_axiom_status(private_violence_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('95b23642-64aa-4758-a68d-33fcbf5281bf', private_violence_is_illegitimate, deontological).
narrative_ontology:cs_axiom('95b23642-64aa-4758-a68d-33fcbf5281bf', foundational, honor_is_not_defended_by_combat).
narrative_ontology:cs_axiom_status(honor_is_not_defended_by_combat, holdable).
narrative_ontology:cs_axiom_grounding('95b23642-64aa-4758-a68d-33fcbf5281bf', honor_is_not_defended_by_combat, conventional).
narrative_ontology:cs_reference_frame('95b23642-64aa-4758-a68d-33fcbf5281bf', post_enlightenment_civility).
narrative_ontology:cs_drift_state('95b23642-64aa-4758-a68d-33fcbf5281bf', contemporary_globalized_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('95b23642-64aa-4758-a68d-33fcbf5281bf', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, modern_state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_public_sphere).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the cultural shift that renders private violence illegitimate, reinforcing its exclusive claim to the use of force. This reading sees the state as a passive beneficiary of a deeper cultural transformation, not its primary driver.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, modern_state_monopoly_on_violence, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from the establishment of new norms of civility and rational discourse, where disputes are settled through legal or rhetorical means rather than personal combat. This cultural framework is reinforced by dueling's cognitive impossibility.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_public_sphere, beneficiary,
    organized, generational, mobile, national).

% Individuals for whom honor remained a central organizing principle, but whose cultural framework for resolving disputes through dueling became incomprehensible or illegitimate to the broader society. They are excluded from the new normative consensus.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents, excluded,
    powerless, biographical, identity_locked, local).

% Analyzes the historical process by which dueling transitioned from a legitimate, if contested, practice to a cognitively unthinkable one, focusing on the deep cultural shifts that made it so.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, contemporary_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared cultural framework where personal disputes are resolved through non-violent, institutional, or discursive means, rather than through ritualized combat, thereby coordinating social expectations around civility and legal process.
% TRANSFER_FUNCTION: Transfers the legitimacy of dispute resolution from individual honor and private violence to state-sanctioned legal systems and public discourse, effectively transferring the 'right to violence' to the state.
% ABSENT_VOICES: The voices of those for whom dueling was a necessary and legitimate means of upholding honor are absent from the modern normative consensus; their framework for understanding justice and personal integrity has been culturally foreclosed.
% DISAPPEARANCE_RATIONALE: If the cultural framework that makes dueling unthinkable were to vanish overnight, it would not 'rearrange' the world in the sense of bringing dueling back; rather, it would reveal the deep, almost 'natural' entrenchment of its illegitimacy. The constraint is so deeply embedded that its absence would be imperceptible, as the world has already rearranged around its non-existence.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a way that aligns with the evolving values of a modernizing society, moving away from aristocratic honor codes towards bourgeois civility and state authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians and cultural anthropologists corroborate that the problem of establishing and maintaining a monopoly on violence and a civil public sphere is an ongoing societal challenge, even if the specific form of dueling is no longer a threat. Legal scholars attest to the continued evolution of dispute resolution mechanisms.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint is not actively extracting resources but rather represents a deep cultural shift that makes dueling non-viable. Suppression is very high (0.95) because the cultural framework itself suppresses the very thought of dueling as a legitimate option; it's a cognitive suppression. Theater ratio is zero because there's no performative maintenance of dueling's illegitimacy; it's simply not part of the cultural script. Accessibility collapse is near total (0.98) as the cultural framework makes alternatives to non-violent dispute resolution almost impossible to conceive. Resistance is negligible (0.02) because the cultural shift is so profound that active resistance to dueling's illegitimacy is rare and ineffective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a modern observer, the constraint operates as a Mountain – an unchangeable feature of the cultural landscape. However, from the perspective of a historical honor culture adherent, it would have been a Snare, actively suppressing their traditional means of dispute resolution. This story focuses on the modern, post-transformation perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The modern state and bourgeois public sphere are beneficiaries because their legitimacy and operational modes are reinforced by the cultural shift away from dueling. Honor culture adherents are 'excluded' and 'identity_locked' because their worldview, which once legitimized dueling, is now culturally marginalized and their identity is tied to a foreclosed practice. Contemporary observers are analytical, seeking to understand the historical process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_causation,
    'To what extent was the decline of dueling driven by deep cultural transformation (as this reading suggests) versus explicit legal prohibition and enforcement?',
    'Comparative historical analysis of societies with similar legal prohibitions but differing cultural trajectories regarding honor and violence.',
    'If legal causation is dominant, the constraint would be reclassified closer to a Snare or Tangled Rope, with higher extractiveness and active suppression. If cultural causation is dominant, the Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_legal_causation, empirical, 'Distinguishing the primary causal mechanism for dueling''s decline.').

omega_variable(
    cognitive_unthinkability_threshold,
    'At what point did dueling transition from merely illegal or unfashionable to genuinely ''cognitively unthinkable'' as a legitimate form of dispute resolution?',
    'Detailed analysis of primary sources (diaries, literature, legal records) across the 18th and 19th centuries to identify shifts in language, moral condemnation, and the framing of dueling.',
    'Pinpointing this threshold would refine the temporal measurements and potentially reveal a more abrupt ''phase transition'' in the constraint''s operation, rather than a gradual decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_unthinkability_threshold, empirical, 'Identifying the precise moment of cognitive shift regarding dueling''s legitimacy.').

omega_variable(
    reading_framing_bias,
    'Is the ''contraction_reading'' itself a product of a modern cultural bias that struggles to comprehend historical honor cultures, thus overstating the ''unthinkability''?',
    'Engagement with historical empathy and counterfactual reasoning, attempting to reconstruct the internal logic of honor cultures without imposing modern normative frameworks.',
    'If a strong framing bias is identified, the ''unthinkability'' claim might be softened, potentially shifting the constraint towards a ''Piton'' (vestigial practice) or ''drop_reading'' (fringe persistence) for some historical periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_bias, conceptual, 'Assessing potential anachronistic bias in the ''cognitively unthinkable'' claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1800, 0.02).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.88).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.93).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
