% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Persistence as Adequate Equilibrium
 *   domain: economic_history/technology_standards
 *
 * SUMMARY:
 *   The QWERTY keyboard layout has dominated input devices since the 1870s,
 *   despite competing designs like Dvorak that proponents claim are superior.
 *   The naturalization reading argues that QWERTY persists because it
 *   genuinely became adequate through market selection and fair competition,
 *   not because of lock-in or suppression. Users face real switching costs
 *   (retraining), but those costs represent their own skill investment in
 *   QWERTY literacy, not imposed scarcity. Manufacturers have no systematic
 *   incentive to maintain QWERTY beyond following user preference. Dvorak and
 *   alternatives have remained marginal despite decades of opportunity,
 *   suggesting their claimed advantages do not justify the retraining cost.
 *   This reading instantiates a clean ε-invariant constraint with no
 *   beneficiary extraction, minimal enforcement, and stable equilibrium.
 *
 * KEY AGENTS:
 *   - keyboard users (billions globally): have invested in QWERTY literacy; face retraining cost to switch; benefit from universal standardization
 *   - typing teachers and education sector: benefit from training on the standard layout; no active defense required
 *   - computer manufacturers: produce QWERTY keyboards because users expect it; follow market demand; take no anti-competitive action
 *   - Dvorak researchers and alternatives proponents: advocate for alternative layouts; remain free to publish and promote; ideas have not achieved mainstream adoption
 *   - economic historians: study the persistence question; dispute remains open across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Persistence as Adequate Equilibrium").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '569e351f-b48f-4243-8b09-5e8868873955').
narrative_ontology:cs_kernel_codification('569e351f-b48f-4243-8b09-5e8868873955', distributed).
narrative_ontology:cs_authority_grounding('569e351f-b48f-4243-8b09-5e8868873955', distributed).
narrative_ontology:cs_reading_relation('569e351f-b48f-4243-8b09-5e8868873955', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('569e351f-b48f-4243-8b09-5e8868873955', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('569e351f-b48f-4243-8b09-5e8868873955', foundational, market_selection_adequacy_principle).
narrative_ontology:cs_axiom_status(market_selection_adequacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('569e351f-b48f-4243-8b09-5e8868873955', market_selection_adequacy_principle, empirically_contingent).
narrative_ontology:cs_axiom('569e351f-b48f-4243-8b09-5e8868873955', secondary, skill_investment_switching_cost).
narrative_ontology:cs_axiom_status(skill_investment_switching_cost, holdable).
narrative_ontology:cs_axiom_grounding('569e351f-b48f-4243-8b09-5e8868873955', skill_investment_switching_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('569e351f-b48f-4243-8b09-5e8868873955', competitive_equilibrium_baseline).
narrative_ontology:cs_drift_state('569e351f-b48f-4243-8b09-5e8868873955', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('569e351f-b48f-4243-8b09-5e8868873955', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no systematic beneficiary collects from QWERTY's persistence. Users pay a switching cost (their own skill retraining), not a transfer to someone else. Manufacturers coordinate on the standard but do not extract rents through it—they simply follow user preference to minimize friction. Suppression is minimal (0.08) because no active enforcement machinery is required to maintain QWERTY's dominance; it persists through voluntary choice and network effects. Theater ratio is near-zero (0.05) because QWERTY is what it claims to be: a standard that solves a coordination problem. Accessibility collapse is high (0.72) because once users have learned QWERTY, the alternative layouts become inaccessible to them without substantial retraining—but this inaccessibility is the natural consequence of skill investment, not imposed suppression. Resistance is low (0.15) because the arrangement faces little organized opposition—advocates for alternatives exist but do not mount sustained campaigns against QWERTY, suggesting they accept the trade-off between claimed technical advantage and retraining cost.
 *
 * PERSPECTIVAL GAP:
 *   The naturalization reading predicts minimal perspectival divergence: all seats (users, manufacturers, educators) should perceive QWERTY as a neutral standard that coordinates activity efficiently. The lock-in reading would predict that users and Dvorak proponents perceive QWERTY as an imposed constraint, while manufacturers perceive it as a natural choice. The beneficiary-extraction reading would predict that users and workers perceive QWERTY as defended by manufacturers for profit, while manufacturers deny active enforcement. The engine computes these seat-level divergences from the structural data; the naturalization reading's metrics (low extraction, minimal suppression, no identifiable beneficiary) should produce convergent perception across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the naturalization reading, there is no clear target/beneficiary asymmetry. Users benefit from coordination (one standard across all devices) and pay a coordination cost (skill retraining to switch). Manufacturers benefit from serving user demand efficiently but do not extract rents. Dvorak proponents are excluded not by suppression but by competition—their ideas have not prevailed in the market. Directionality is near-symmetric (d ≈ 0.5) across all agents because the constraint operates as a genuine coordination equilibrium, not an extractive arrangement. This stands in sharp contrast to the lock-in and beneficiary-extraction readings, where directionality would diverge significantly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented keyboard layouts, retraining friction) remains live under this reading. QWERTY persists because it solves the coordination problem well enough, not because the problem has become obsolete. There is no mandatrophy signal: the constraint's mandate and its function are aligned. Alternative readings (lock-in, extraction) would argue for mandatrophy—that QWERTY persists despite its founding problem being solved or the solution becoming inferior. The naturalization reading resolves this by asserting that the founding problem endures (users still benefit from universal standardization) and QWERTY still solves it adequately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_empirical_superiority_contested,
    'Does Dvorak layout objectively provide measurable typing speed or ergonomic advantages over QWERTY in controlled testing?',
    'Systematic meta-analysis of peer-reviewed typing speed and ergonomic studies comparing QWERTY and Dvorak in matched populations with equal training time. Blind trials controlling for expectancy effects.',
    'If Dvorak shows statistically significant, economically meaningful advantage: switches the reading toward lock-in (the advantage exists but was not adopted). If advantage is marginal or within noise: supports the naturalization reading (QWERTY''s adequacy is genuine, Dvorak''s superiority is mythologized). If advantage exists but is offset by retraining costs: supports the naturalization reading (adequacy dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_empirical_superiority_contested, empirical, 'Whether Dvorak''s claimed technical superiority stands up to rigorous comparative testing.').

omega_variable(
    active_manufacturer_suppression_of_alternatives,
    'Have computer manufacturers, during the digital era (post-1950), actively suppressed alternative keyboard layouts through exclusive contracting, patent litigation, or deliberate incompatibility?',
    'Historical archives of manufacturer decisions, patent disputes, and licensing agreements. Testimony from keyboard designers and manufacturer engineers about design trade-offs.',
    'If active suppression is documented: undermines the naturalization reading (QWERTY persists partly through enforcement, not just adequacy). If manufacturers simply follow user demand without anti-competitive action: supports the naturalization reading (market selection, not suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_manufacturer_suppression_of_alternatives, empirical, 'Whether QWERTY''s dominance rests on active corporate defense or on user preference and coordination.').

omega_variable(
    skill_switching_cost_vs_imposed_lock_in,
    'To what extent are users'' switching costs from QWERTY attributable to their own skill investment versus to technological or contractual barriers imposed by manufacturers?',
    'Survey of users attempting alternative layouts; measurement of retraining curves and subjective cost perception. Analysis of whether users could legally and technically switch if they chose to.',
    'If switching costs are primarily skill-based (own investment): supports naturalization reading (users prefer QWERTY because they have invested in it, not because they are trapped). If substantial costs are imposed (incompatible hardware, proprietary lock-in, contractual barriers): suggests lock-in or extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_switching_cost_vs_imposed_lock_in, empirical, 'Whether QWERTY persistence reflects rational preference or technological/contractual lock-in.').

omega_variable(
    sibling_reading_foreclosure_status,
    'Do the axioms of the naturalization reading logically foreclose the lock-in reading and the beneficiary-extraction reading, or can different parties coherently hold different readings simultaneously?',
    'Structural analysis: if naturalization''s core premise (QWERTY is adequately chosen through market selection) logically entails that lock-in and extraction readings are false, then foreclosure holds. If the readings are about different causal mechanisms that could coexist (adequacy AND imperfect information, adequacy AND suppression), then coexistence holds.',
    'If naturalization forecloses the siblings: the readings are mutually exclusive and the evidence must choose. If they coexist: multiple mechanisms may operate simultaneously (QWERTY is adequate AND path-dependent AND supported by suppression), requiring more nuanced analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'The logical relationship between the naturalization reading and its siblings in the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_nat_tr_t1873, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwerty_nat_tr_t1930, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1930, 0.03).
narrative_ontology:measurement(qwerty_nat_tr_t1970, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1970, 0.04).
narrative_ontology:measurement(qwerty_nat_tr_t2000, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t2024, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qwerty_nat_be_t1873, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1873, 0.08).
narrative_ontology:measurement(qwerty_nat_be_t1930, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1930, 0.1).
narrative_ontology:measurement(qwerty_nat_be_t1970, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1970, 0.11).
narrative_ontology:measurement(qwerty_nat_be_t2000, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(qwerty_nat_be_t2024, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_nat_su_t1873, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1873, 0.05).
narrative_ontology:measurement(qwerty_nat_su_t1930, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1930, 0.06).
narrative_ontology:measurement(qwerty_nat_su_t1970, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1970, 0.07).
narrative_ontology:measurement(qwerty_nat_su_t2000, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(qwerty_nat_su_t2024, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into three structurally distinct constraint stories with different ε values, stakeholder structures, and persistence mechanisms. The naturalization_reading (this constraint) claims QWERTY persists through adequate equilibrium (low ε, no beneficiary, fair competition). The lock_in_reading claims path-dependent coordination failure (moderate-high ε, asymmetric vulnerability, no active beneficiary but locked users). The beneficiary_extraction_reading claims manufacturers actively maintain QWERTY to protect training-cost sunk investment (high ε, identified beneficiary, active enforcement). Each reading grounds its ε in different structural mechanisms: naturalization in market selection, lock-in in path-dependency dynamics, extraction in manufacturer power and intent. The three stories share a kernel (QWERTY's dominance) but emit different constraints because their ε values differ by wide margins and their beneficiary/victim structures differ fundamentally. All three readings remain live in the scholarly literature; the corpus jointly documents the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
