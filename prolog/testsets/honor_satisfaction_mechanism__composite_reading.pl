% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction via Multiple Extractive Mechanisms (Composite Reading)
 *   domain: legal/social/normative
 *
 * SUMMARY:
 *   Between roughly 1650 and 1900, European honor-satisfaction underwent a
 *   composite transformation operated via four distinct mechanisms: (1) state
 *   monopoly on legitimate violence enforcement, moving honor-disputes from
 *   interpersonal combat into courtrooms; (2) emergence of bourgeois
 *   respectability norms that redefine honor away from martial prowess toward
 *   economic and social standing; (3) insurance industry recategorization of
 *   honor-risk as insurable mortality and reputation-loss; (4) a broader
 *   cognitive shift that made dueling unthinkable as a valid form of
 *   honor-satisfaction. This reading instantiates the view that
 *   honor-satisfaction persisted but was eroded through composite,
 *   multi-institutional extraction. The alternative readings (contraction:
 *   dueling became categorically impossible; decline: dueling simply
 *   persisted at declining frequency) are structurally different accounts of
 *   the same historical phenomenon. This reading emphasizes
 *   mechanism-coordination and extractive intent; the others emphasize
 *   cognitive impossibility or simple attrition.
 *
 * KEY AGENTS:
 *   - State judiciary: monopolizes legitimate adjudication of honor; prosecutes dueling as murder while offering civil remedies
 *   - Bourgeois honor claimants: access new honor frames (respectability, business reputation) that don't require dueling; benefit from recategorization
 *   - Insurance underwriters: extract rents by converting honor-disputes into insurable risks; create new financial dependencies for reputation-management
 *   - Traditional aristocratic practitioners: identity-locked to martial honor; criminalized and excluded from emerging frames
 *   - Lower-class duelists: prosecuted more heavily; lack resources for legal or insurance-based honor-satisfaction
 *   - Women: systematically excluded from both traditional and emergent honor-satisfaction systems
 *   - Normative authority keepers (churches, guilds): lose authority as state monopolizes discourse; some migrate to enforcing bourgeois norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction via Multiple Extractive Mechanisms (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "legal/social/normative").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '17c9c652-e72d-4bd7-b66e-1c64510f7eba').
narrative_ontology:cs_kernel_codification('17c9c652-e72d-4bd7-b66e-1c64510f7eba', formalized).
narrative_ontology:cs_authority_grounding('17c9c652-e72d-4bd7-b66e-1c64510f7eba', extraction).
narrative_ontology:cs_interpretation_layer_present('17c9c652-e72d-4bd7-b66e-1c64510f7eba').
narrative_ontology:cs_reading_relation('17c9c652-e72d-4bd7-b66e-1c64510f7eba', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('17c9c652-e72d-4bd7-b66e-1c64510f7eba', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_axiom('17c9c652-e72d-4bd7-b66e-1c64510f7eba', foundational, honor_satisfaction_requires_institutional_monopoly).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_institutional_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('17c9c652-e72d-4bd7-b66e-1c64510f7eba', honor_satisfaction_requires_institutional_monopoly, instrumental).
narrative_ontology:cs_axiom('17c9c652-e72d-4bd7-b66e-1c64510f7eba', secondary, extractive_mechanisms_structurally_necessary).
narrative_ontology:cs_axiom_status(extractive_mechanisms_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('17c9c652-e72d-4bd7-b66e-1c64510f7eba', extractive_mechanisms_structurally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('17c9c652-e72d-4bd7-b66e-1c64510f7eba', plural_honor_satisfaction_system).
narrative_ontology:cs_drift_state('17c9c652-e72d-4bd7-b66e-1c64510f7eba', industrial_bourgeois_consolidation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17c9c652-e72d-4bd7-b66e-1c64510f7eba', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_judiciary).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_honor_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_underwriters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, lower_class_duelists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, women_excluded_from_honor_satisfaction).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, traditional_aristocratic_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 to 0.68 over 250 years as the multiple mechanisms accumulate. Early extractiveness (1650) is low because the plural honor-system still allows exit: traditional practitioners retain full agency. By 1720, state prosecution begins; extractiveness rises to 0.35. By 1780, bourgeois norms and early insurance appear; extractiveness reaches 0.52. By 1840, all four mechanisms are operational; extractiveness plateaus near 0.68 as the system stabilizes. Suppression follows a similar trajectory: enforcement infrastructure hardens over time. Theater-ratio rises more slowly (0.08 to 0.41), indicating that while the system is substantially extractive and suppressive, much of the enforcement remains functionally tied to actual state prosecution, legal adjudication, and insurance underwriting—not pure theater. However, by 1900, theater-ratio is rising (0.41), suggesting the extractive mechanisms are increasingly maintained through symbolic affirmation of bourgeois norms rather than direct enforcement, a sign of piton-drift. The coercion grid shows level-resolved dynamics: individual-level accessibility collapses most dramatically (0.22 → 0.71), meaning an individual duelist faces nearly total suppression of alternatives by 1900; organizational resistance falls (0.68 → 0.44), meaning institutional pushback weakens; class-level resistance falls (0.71 → 0.35), meaning coordinated class-based resistance becomes impossible; structural-level resistance falls most (0.62 → 0.28), indicating the broader legitimacy of alternative honor-frames is gone.
 *
 * PERSPECTIVAL GAP:
 *   The state judiciary and bourgeois claimants compute as seeing coordination (a solution to a real plural-honor problem); traditional practitioners and lower-class duelists compute as seeing pure extraction (the theft of their honor-system through criminalization and exclusion). Women and excluded populations compute as seeing complete structural barrier—not even extraction within a system, but exclusion from any system. The engine should compute three distinct per-seat types: institutional seats see rope or coordination-heavy tangled_rope; victim seats see snare or extraction-heavy tangled_rope; excluded seats see snare with no beneficiary role.
 *
 * DIRECTIONALITY LOGIC:
 *   State judiciary is beneficiary (d ≈ 0.15); it collects legitimacy and institutional authority from monopolizing honor-adjudication. Bourgeois claimants are complex (d ≈ 0.45): they benefit enormously from access to new honor-frames but also pay through conformity to bourgeois norms and participation in the legal/insurance systems—near symmetric. Insurance underwriters are beneficiary (d ≈ 0.20); they extract pure financial rents with mobile exit (they can always exit to other underwriting). Traditional aristocratic practitioners are victim (d ≈ 0.85): they are criminalized, identity-locked, with no exit; the constraint targets them directly. Lower-class duelists are victim (d ≈ 0.95): powerless, trapped, prosecuted disproportionately, with no resources for legal or insurance access. Women are victim (d ≈ 0.90): excluded entirely from all frames, structurally barred from honor-satisfaction regardless of mechanism. The derived directionalities show asymmetric extraction concentrated on powerless and identity-locked populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: a plural, overlapping honor-system (dueling, guild arbitration, church mediation, street violence, formal law) produced coordination failures and endless cycles of vendetta. But the 'solution'—consolidating into a state monopoly coordinated with bourgeois norms, insurance, and cognitive recategorization—does not simply solve the problem. It solves it FOR bourgeois, economically-mobile populations while excluding and criminalizing everyone else. By 1880–1900, the constraint begins to show piton-like characteristics: enforcement is increasingly theatrical (bourgeois conformity, literary condemnation of dueling, insurance policy language) rather than direct prosecution; a new generation has internalized the category-shift so thoroughly that dueling feels unthinkable, not merely illegal. This suggests the constraint may be transitioning from active tangled_rope to degraded piton, where the extraction persists through institutional inertia and norm-theater more than through actual enforcement. The mandatrophy signal is moderate: the founding coordination problem is substantially solved (reducing mandatrophy), but the solution persists in extracting from populations no longer capable of mounting coordinated resistance (raising mandatrophy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence_vs_coordination,
    'Are the four mechanisms (state monopoly, bourgeois norms, insurance, category-shift) logically independent pressures that happen to align, or are they functionally interdependent—each enabling and requiring the others?',
    'Comparative historical analysis: examine cases where one mechanism advanced without the others (e.g., state monopoly without insurance development, or bourgeois norm-shift without state enforcement) and assess outcomes.',
    'If independent, the constraint is best understood as four separate tangled ropes in coordination, each extractive; if interdependent, it is a single complex system where each mechanism''s extraction depends on the others'' persistence. This shifts the classification between ''four linked snares'' and ''one composite tangled rope.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_vs_coordination, conceptual, 'Whether the composite mechanisms are structurally separable or functionally unified.').

omega_variable(
    category_shift_mechanism_primacy,
    'Is the cognitive recategorization of honor (from martial to bourgeois/financial) a consequence of the other mechanisms, or the primary driver that enabled them?',
    'Chronological and causal analysis: trace which mechanism emerged first and whether the others depended on the category-shift or vice versa.',
    'If category-shift is primary, the constraint is fundamentally about epistemic capture—redefining what honor means—and the other mechanisms are secondary enablers; if consequential, the constraint is primarily about institutional and financial capture, with the category-shift as post-hoc legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_shift_mechanism_primacy, empirical, 'The causal priority of cognitive recategorization within the composite mechanism.').

omega_variable(
    sibling_reading_observability,
    'This reading instantiates the composite-mechanism frame. How would the empirical record differ if the contraction_reading (honor becomes cognitively unthinkable) or decline_reading (dueling persists at declining frequency) were the true structural account?',
    'Compare historical traces: persistence of dueling in residual populations, frequency trends, cognitive markers of possibility vs. impossibility, institutional language shifts.',
    'If the contraction reading is more empirically grounded, the engine reclassifies this reading as a misframing—the mechanisms are secondary to a fundamental category-level shift. If decline is more grounded, this reading over-attributes extractive intent to mechanisms that were incidental to a slow-frequency decline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_observability, empirical, 'Observational divergence between the composite, contraction, and decline readings.').

omega_variable(
    extractiveness_attribution,
    'Who captures the extraction in this composite mechanism? The state judiciary (authority), bourgeois claimants (norm-shift access), or insurance underwriters (financial securitization)?',
    'Trace rents and institutional growth: measure state-judicial revenue/authority growth, bourgeois wealth and standing gains, insurance premium volumes and underwriter profits.',
    'If state captures most, the mechanism is state-monopoly-primary; if bourgeois claimants capture most, it is norm-capture; if insurance captures most, it is financialization. The distribution of captured extraction informs whether payers are (1) traditional practitioners, (2) lower-class duelists, or (3) all classes equally, and thus the shape of asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_attribution, empirical, 'The primary beneficiary within the composite mechanism.').

omega_variable(
    identity_lock_mechanism_persistence,
    'For traditional aristocratic practitioners classified as identity_locked, what mechanisms would cause the identity-lock to break? Economic collapse of the honor-economy, generational replacement, or something intrinsic to the mechanisms themselves?',
    'Post-constraint observation: do populations with identity-locked exit eventually internalize the state/bourgeois/insurance frames, or do they resist? Does intergenerational identity transmission persist or attenuate?',
    'If identity-lock requires active enforcement to maintain, the suppression metric understates true extraction; if identity-lock is self-sustaining, suppression underestimates capacity for persistence even without enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_persistence, empirical, 'The stability of identity-locked exit when the constraint persists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1650, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1650, 0.08).
narrative_ontology:measurement(hono_tr_t1720, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1720, 0.14).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1780, 0.24).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1840, 0.35).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1880, 0.4).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1650, 0.18).
narrative_ontology:measurement(hono_be_t1720, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1720, 0.35).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1780, 0.52).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1840, 0.63).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1880, 0.68).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1650, 0.25).
narrative_ontology:measurement(hono_su_t1720, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1720, 0.41).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1780, 0.58).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1840, 0.68).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1880, 0.71).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).

% DUAL FORMULATION NOTE:
% The three readings of honor_satisfaction_mechanism are structurally distinct constraints on the same kernel. This reading (composite) emphasizes multi-mechanism extraction and institutional capture; contraction emphasizes cognitive category-shift as primary; decline emphasizes slow practice attrition. Each reading has its own ε, beneficiary/victim structure, and type. The readings are linked via network.affects_constraints to indicate family membership. The empirical divergence between readings is the point: the corpus measures which reading's structural account is best-supported by evidence and by the constraint's operation over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
