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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Decline of Dueling: Composite Reading of Honor Settlement Legitimacy
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a legitimate means of
 *   honor settlement, viewed through a 'composite reading' of the
 *   honor_settlement_legitimacy kernel. This reading posits that dueling's
 *   decline was overdetermined by multiple reinforcing mechanisms: a primary
 *   cultural contraction (dueling became cognitively unthinkable) reinforced
 *   by material and institutional changes (e.g., strengthening state legal
 *   systems, changing social structures) that would have independently
 *   suppressed the practice. The constraint is framed as a 'mountain' because
 *   the convergence of these forces made the decline appear inevitable and
 *   irreversible, a structural feature of modernizing societies.
 *
 * KEY AGENTS:
 *   - honor_culture_adherents: Payer (powerless/trapped) — lose a means of honor defense
 *   - state_legal_system: Beneficiary (institutional/arbitrage) — gains monopoly on violence
 *   - bourgeois_society: Beneficiary (organized/mobile) — benefits from stable, predictable social order
 *   - analytical_historians: Observer (analytical/analytical) — reconstructs causal pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.3).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.7).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Decline of Dueling: Composite Reading of Honor Settlement Legitimacy").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, 'b2584241-7c03-45fd-9f73-5b1f99e06394').
narrative_ontology:cs_kernel_codification('b2584241-7c03-45fd-9f73-5b1f99e06394', implicit).
narrative_ontology:cs_authority_grounding('b2584241-7c03-45fd-9f73-5b1f99e06394', distributed).
narrative_ontology:cs_reading_relation('b2584241-7c03-45fd-9f73-5b1f99e06394', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('b2584241-7c03-45fd-9f73-5b1f99e06394', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('b2584241-7c03-45fd-9f73-5b1f99e06394', foundational, decline_is_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('b2584241-7c03-45fd-9f73-5b1f99e06394', decline_is_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('b2584241-7c03-45fd-9f73-5b1f99e06394', foundational, cultural_contraction_is_primary_driver).
narrative_ontology:cs_axiom_status(cultural_contraction_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('b2584241-7c03-45fd-9f73-5b1f99e06394', cultural_contraction_is_primary_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('b2584241-7c03-45fd-9f73-5b1f99e06394', honor_culture_legitimacy).
narrative_ontology:cs_drift_state('b2584241-7c03-45fd-9f73-5b1f99e06394', post_enlightenment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b2584241-7c03-45fd-9f73-5b1f99e06394', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose social standing and self-concept were tied to the code of honor, for whom dueling was a legitimate, even necessary, means of resolving disputes and defending reputation. They lost this option as it became legally prohibited and socially unthinkable.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_culture_adherents, payer,
    powerless, biographical, trapped, local).

% The evolving legal and judicial apparatus that increasingly criminalized dueling, asserting a monopoly on legitimate violence and dispute resolution. It benefited from the reduction of private violence and the strengthening of its own authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_system, beneficiary,
    institutional, generational, arbitrage, national).

% The rising middle classes who favored a more orderly, rational, and commercially oriented social environment. They benefited from the decline of dueling, which was seen as an aristocratic anachronism disruptive to business and civil life.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_society, beneficiary,
    organized, generational, mobile, national).

% Scholars who analyze the historical forces and mechanisms that led to dueling's decline, seeking to understand the interplay of cultural, legal, and social factors. They are outside the direct operation of the constraint but interpret its structure.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced a decentralized, violent, and often fatal method of honor settlement with a centralized, state-controlled, and non-lethal system of dispute resolution, coordinating social behavior around new norms of civility and legal process.
% TRANSFER_FUNCTION: Transferred the authority for dispute resolution and honor defense from individuals and their social circles to the state and its legal institutions, effectively transferring the 'right to violence' to the state.
% ABSENT_VOICES: The voices of those who continued to believe in the necessity and legitimacy of dueling, often from declining aristocratic or military classes, became increasingly marginalized and unheard in the dominant public discourse. Their arguments for honor as a non-negotiable value were dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: If the 'decline of dueling' constraint were to disappear overnight, the world would remain largely unchanged because the underlying cultural, legal, and social structures that superseded dueling are deeply entrenched. Dueling would not spontaneously re-emerge as a legitimate practice; its absence is now a fundamental feature of modern society.
% FOUNDING_PROBLEM: The founding problem was a social order where personal honor could be defended through lethal combat, leading to unpredictable violence, challenges to state authority, and a system of justice outside formal legal channels.
% FOUNDING_PROBLEM_CORROBORATION: The state legal system and bourgeois society attest that the problem is dead, citing the establishment of robust legal frameworks and a more peaceful civil society. Honor culture adherents, though marginalized, might argue the problem of honor defense persists, but their view is not widely corroborated by independent sources or the historical record of social change.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__composite_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'mountain' classification reflects the overdetermined nature of the decline, where multiple, independently sufficient causal pathways converged. Extractiveness is low (0.3) because the constraint primarily removes an option rather than actively extracting resources, and the 'cost' is borne by a diminishing group. Suppression is high (0.7) due to both legal prohibition and, more significantly, the cultural shift making dueling unthinkable. Theater ratio is low (0.1) as the decline was genuine, not merely performative. Accessibility collapse is high (0.8) because the option of dueling became culturally and legally unavailable. Resistance is low (0.15) as the practice faded into obsolescence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor culture adherents, the constraint was a 'snare' that removed their means of defending honor. From the perspective of the state legal system and bourgeois society, it was a 'mountain' or 'rope' that brought order and stability. The composite reading acknowledges the multiple forces at play, making the decline appear 'natural' from a macro-historical view, even if it was experienced as coercive by some.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor culture adherents are targets (d=1.0) as they lose a fundamental aspect of their social code. The state legal system and bourgeois society are beneficiaries (d=0.0-0.1) as they gain a more stable and controllable social order. The 'natural' aspect of the mountain means the extraction is diffuse and not actively collected by a single party, but the benefits accrue to these groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The concept of mandatrophy is less applicable here, as the 'mandate' of dueling (honor settlement) did not so much atrophy as become culturally and legally superseded. The constraint's persistence is not due to inertia but to the active, reinforcing mechanisms that led to its decline. The classification as 'mountain' prevents mislabeling a complex, multi-causal historical process as a simple 'snare' of state power, while still acknowledging the coercive aspects for those who valued dueling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''composite reading'' of the honor_settlement_legitimacy kernel, or is it primarily a ''contraction reading'' with secondary reinforcing mechanisms?',
    'Historical counterfactual analysis: if dueling would have persisted significantly longer without the cultural contraction, then the contraction reading is primary. If institutional/material changes alone would have suppressed it, the composite reading is more robust.',
    'If the contraction reading is primary, the constraint''s ''naturalness'' (emerges_naturally) is more strongly tied to cultural shifts than to material conditions, potentially reclassifying it as a ''snare'' of social norms rather than a ''mountain'' of converging forces. If the composite reading is robust, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the primary driver of dueling''s decline within the honor_settlement_legitimacy kernel.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the decline of dueling a ''natural law'' outcome of converging social forces, or a ''constructed'' outcome benefiting identifiable agents?',
    'Comparative historical analysis across cultures with similar initial conditions but different legal/social interventions. If the decline consistently follows similar patterns regardless of specific interventions, it leans towards natural law. If interventions are decisive, it leans towards constructed.',
    'If constructed, the ''mountain'' classification is a false summit, and the constraint would reclassify as a ''tangled_rope'' or ''snare'' benefiting the state legal system and bourgeois society.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Ambiguity between natural law and constructed constraint for dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__composite_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__composite_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__composite_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__composite_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__composite_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__composite_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, bourgeois_social_order).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel, alongside 'contraction_reading' and 'drop_reading'. Each reading offers a distinct causal explanation for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
