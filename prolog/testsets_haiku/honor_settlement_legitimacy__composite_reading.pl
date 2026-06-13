% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Based Settlement Legitimacy (Composite Causal Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   From roughly 1650 to 1900, dueling as a legitimacy claim and settlement
 *   mechanism within European aristocracies declined from a widespread,
 *   legally problematic practice to a marginal, anachronistic gesture. The
 *   constraint being modeled here — honor-settlement legitimacy — is the
 *   structural claim that disputes within the aristocracy could be
 *   legitimately resolved by the parties' own armed judgment, outside state
 *   courts, because honor is an intra-elite property that courts cannot
 *   adjudicate. This reading (composite_reading) asserts that dueling's
 *   decline resulted from multiple, reinforcing causal pathways: (1) the
 *   cultural-cognitive unthinkability of honor-based settlement (contraction
 *   mechanism — the core legitimacy axiom lost purchase); (2) the
 *   institutional suppression via police, prosecution, and state monopoly on
 *   violence; (3) the material squeeze from urbanization and mobility that
 *   made ritualized armed settlement impractical; (4) the rise of alternative
 *   legitimacy claims (aristocratic titles vested in state, wealth-based
 *   status, institutional position) that detached aristocratic identity from
 *   the honor-dueling mechanism. No single mechanism was sufficient; each
 *   reinforced the others. The constraint is here classified as SNARE rather
 *   than the reading's own self-presentation as ROPE, because the mechanism
 *   persisted primarily through the extraction of power from non-elites
 *   (immunity from legal accountability) rather than through genuine
 *   coordination of elite dispute settlement that no alternative was solving
 *   equally well.
 *
 * KEY AGENTS:
 *   - Aristocratic honor culture: the beneficiary stratum whose legitimacy claim rests on honor-based settlement; identity is bound to the practice; exit from the identity is identity-death.
 *   - State authority and legal system: payers bearing the cost of a fragmented legitimacy system; constrained from suppressing dueling without breaking aristocratic loyalty.
 *   - Community dispute-bearers and working-class victims: diffuse victims of unaccountable elite violence; excluded from the honor framework and from alternative recourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.76).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, snare).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Based Settlement Legitimacy (Composite Causal Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, 'b9300d61-3a8a-4d12-8670-55fa0c0fbee2').
narrative_ontology:cs_kernel_codification('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', distributed).
narrative_ontology:cs_authority_grounding('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', extraction).
narrative_ontology:cs_interpretation_layer_present('b9300d61-3a8a-4d12-8670-55fa0c0fbee2').
narrative_ontology:cs_reading_relation('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', foundational, honor_overdetermined_decline).
narrative_ontology:cs_axiom_status(honor_overdetermined_decline, holdable).
narrative_ontology:cs_axiom_grounding('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', honor_overdetermined_decline, empirically_contingent).
narrative_ontology:cs_axiom('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', secondary, contraction_structural_dominance).
narrative_ontology:cs_axiom_status(contraction_structural_dominance, holdable).
narrative_ontology:cs_axiom_grounding('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', contraction_structural_dominance, empirically_contingent).
narrative_ontology:cs_reference_frame('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', aristocratic_honor_autonomy).
narrative_ontology:cs_drift_state('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', post_enlightenment_rationalization_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b9300d61-3a8a-4d12-8670-55fa0c0fbee2', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocratic_honor_culture).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, legal_system_agents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, community_dispute_bearers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, working_classes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).

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
 *   The extractiveness reading (0.68 at interval end) is grounded in the constraint's core mechanism: it legitimizes the exemption of an elite from ordinary legal accountability for violence. Suppression (0.76) rises over the interval as state enforcement machinery intensifies — police forces, prosecution, exile threats, and social stigma are deployed to kill the practice. Theater (0.41, rising to peak ~0.42 then declining slightly) tracks the increasingly ceremonial and performative character of the few duels that persisted into the 1800s — by that point, the constraint is maintained more through ritualized legitimacy-claims than through actual settlement function. The accessibility_collapse metric in the coercion grid rises sharply (0.62 → 0.88 at structural level, 0.52 → 0.71 at individual level) because the cognitive framework that made honor-settlement thinkable erodes — alternatives (legal adjudication, reputation systems, economic status) become available and normalized. Resistance rises throughout (0.35 → 0.68 structural level) as bourgeois reformers, legal theorists, and the state itself mount sustained opposition. The measurement series are on a single shared time grid: every metric is authored at every examined time point (1650, 1725, 1775, 1825, 1870, 1900), which is the correct surface for temporal drift analysis and prevents the OQ-105-style misalignment that injects end-state values into earlier moments.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (aristocratic honor culture) and the payers (legal system, community bearers) should compute to different types from the engine. From the aristocratic perspective, dueling was a genuine coordination mechanism solving a real problem (intra-elite dispute settlement without submission to bourgeois courts); from the payer seats, it appears as extractive exemption from accountability maintained by suppression. The engine computes per-seat classifications from the structural power and exit data; the authored claim diverges from this on purpose — the constraint is CLAIMED as SNARE (extraction-dominated) while the internal logic of the stakeholders might frame it as ROPE or even SCAFFOLD (transitional mechanism as centralized authority consolidated). The perspectival gap is where the analytical work is: why does the same constraint look coordinate to one seat and extractive to others?
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic honor culture: d near 0.0 (full beneficiary) — controls the rules, benefits from exemption, identity-locked to the framework. Legal system agents: d ~ 0.65 (targets, but institutional actors with some autonomy) — pay the cost of fragmented legitimacy but constrained from exit by state interests. Community dispute-bearers: d ~ 0.72 (high targets) — no voice in honor rules, no alternative recourse. Working-class victims: d ~ 0.85 (near-full targets) — unaccountable elite violence, no recourse via honor or court. State authority: d ~ 0.50 (symmetric) — benefits from elite stability (dueling contains intra-elite violence) but pays the cost of legal fragmentation and inability to monopolize violence. No directionality overrides are needed; the structural data (beneficiary/victim, power, exit_options) drive the derivation chain correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved = TRUE. The founding problem (intra-elite disputes without neutral adjudicator) is dead by 1900 — centralized state authority, rational courts, and alternative status mechanisms have solved what dueling was built to solve. The constraint persists theatrically (T=0.41) and is maintained by institutional suppression (S=0.76) rather than by endorsement of its original function. The founding_problem_status is coded as CONTESTED because aristocratic apologists continue to claim honor cannot be court-adjudicated, while the historical record shows courts, arbitration, and reputation systems all working without dueling. The disappearance_verdict is world_rearranges, confirming that the constraint's removal would require institutional reorganization — the alternative mechanisms exist and operated for decades after dueling's legal prohibition. The mismatch (status=dead, verdict=world_rearranges) flags a zombie/piton condition: the constraint has lost its reason-for-being but persists through suppression and inertia. This is the classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_material_suppression,
    'Did dueling decline primarily because the cultural framework (honor logic) became cognitively unthinkable (contraction mechanism), or primarily because material/institutional changes (police, prosecution, mobility, urbanism) made the practice impossible to sustain (institutional suppression mechanism)?',
    'Historical counterfactual: if police capacity and legal prosecution had remained weak, would honor culture have persisted longer? Comparative analysis across regions with different police/institutional development but similar cultural transformations.',
    'Contraction mechanism dominates = the constraint is a defeated legitimacy claim (the axiom unraveled); material suppression dominates = the constraint is institutionally killed (the practice became impossible). The composite reading assumes both operated together. Identifying the primary mechanism rewires the terminal classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_material_suppression, empirical, 'Whether dueling''s decline was primarily cultural (contraction/unthinkability) or institutional/material.').

omega_variable(
    honor_framework_revival_risk,
    'Is the honor-settlement legitimacy framework structurally dead (the axiom has been culturally foreclosed), or merely dormant (could revive under stress, as in honor-cultures post-state-collapse or during civil war)?',
    'Post-1900 instances of honor-system revival in elite subcultures, or in contexts of state weakness. Modern dueling persistence in specific military/aristocratic subcultures as a signal of dormancy vs. death.',
    'If dead = the composite reading correctly identifies a completed mandatrophy; if dormant = the framework persists as a latent alternative legitimacy claim that could reactivate, meaning the constraint is not truly resolved but suspended by material conditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_framework_revival_risk, conceptual, 'Whether honor-settlement legitimacy is permanently delegitimized or merely suppressed by conditions.').

omega_variable(
    identity_lock_recovery,
    'How deep was the identity fusion between aristocratic selfhood and the honor-settlement mechanism? Did aristocrats who abandoned dueling experience it as identity loss, status injury, or merely as pragmatic adaptation?',
    'Historical testimony, memoirs, and institutional record of aristocratic identity reconstruction post-dueling. Speed of status reassignment to alternative legitimacy claims (titles, wealth, institutional position).',
    'Deep identity lock would predict continued resistance and latent legitimacy claims (supporting dormancy); pragmatic identity-switching would support the contraction reading (the framework lost its grip). Affects classification of terminal state and risk of revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_recovery, empirical, 'Depth of identity-fusion between aristocratic selfhood and honor-settlement practice.').

omega_variable(
    committer_frame_alternative_readings,
    'This constraint is one reading (composite_reading) of the contested kernel honor_settlement_legitimacy. The sibling readings are: (1) contraction_reading — dueling became cognitively unthinkable through pure cultural framework transformation; (2) drop_reading — dueling persisted as fringe practice among residual honor-culture adherents, not truly eliminated. The composite reading asserts multiple causal pathways converged and reinforced each other, with contraction as the dominant mechanism. Which reading — contraction, drop, or composite — best explains the actual historical record?',
    'Comparative genealogy across multiple societies with different timelines and mechanisms of dueling decline. Post-1900 persistence data in fringe populations (does drop reading hold?). Reconstruction of the belief-transformation timeline (does contraction reading hold as primary?). The composite reading is falsified if ONE mechanism can account for the full decline without the reinforcing pathways.',
    'Contraction_reading isolates the cultural transformation as sufficient; drop_reading asserts the practice never fully ended; composite_reading claims redundancy of suppression (multiple independent mechanisms would each have killed it). Terminal classification pivots on which reading the evidence supports.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_alternative_readings, conceptual, 'Kernel contest: which reading of honor-settlement-legitimacy decline best fits the historical record?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1650, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_settlement_legitimacy__composite_reading, theater_ratio, 1650, 0.18).
narrative_ontology:measurement(hono_tr_t1725, honor_settlement_legitimacy__composite_reading, theater_ratio, 1725, 0.24).
narrative_ontology:measurement(hono_tr_t1775, honor_settlement_legitimacy__composite_reading, theater_ratio, 1775, 0.31).
narrative_ontology:measurement(hono_tr_t1825, honor_settlement_legitimacy__composite_reading, theater_ratio, 1825, 0.38).
narrative_ontology:measurement(hono_tr_t1870, honor_settlement_legitimacy__composite_reading, theater_ratio, 1870, 0.42).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1650, 0.42).
narrative_ontology:measurement(hono_be_t1725, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1725, 0.55).
narrative_ontology:measurement(hono_be_t1775, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1775, 0.61).
narrative_ontology:measurement(hono_be_t1825, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1825, 0.68).
narrative_ontology:measurement(hono_be_t1870, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1870, 0.71).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1650, 0.48).
narrative_ontology:measurement(hono_su_t1725, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1725, 0.58).
narrative_ontology:measurement(hono_su_t1775, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1775, 0.65).
narrative_ontology:measurement(hono_su_t1825, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1825, 0.72).
narrative_ontology:measurement(hono_su_t1870, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1870, 0.76).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel has three readings: (1) composite_reading (this file) — multiple causal pathways converge, contraction-dominant; (2) contraction_reading — cultural unthinkability as primary mechanism; (3) drop_reading — residual practice persistence in fringe populations. All three are linked via network.affects_constraints. Each story has its own constraint_id, its own ε, its own stakeholder map, and its own terminal classification. The kernel contest is recorded in omega variables and cs_structure elements. Readers should generate all three stories and compare the structural data (axioms, reading_relations, drift_states) to understand the genealogy of the decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
