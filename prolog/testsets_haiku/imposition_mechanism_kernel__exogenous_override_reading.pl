% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norms via Monopoly on Violence (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This is the exogenous_override_reading of the
 *   imposition_mechanism_kernel: a state consolidates authority over diverse
 *   populations by imposing unified norms through its monopoly on legitimate
 *   violence. The reading asserts that compliance is conditional on
 *   continuous enforcement, that prior cultural systems were displaced by
 *   suppression rather than persuaded by superiority, and that legitimacy is
 *   performative—a cover story for extraction of cultural monopoly. The
 *   constraint persists because noncompliance triggers material punishment,
 *   not because populations have accepted the norm's legitimacy. The
 *   theater_ratio rises over time as the state invests more in ideological
 *   justification and less in direct punishment—yet the suppression
 *   requirement remains high, indicating that the theatrical legitimation has
 *   not fully replaced enforcement dependency.
 *
 * KEY AGENTS:
 *   - state_apparatus: The monopoly holder on violence; agenda-setter that decrees and enforces norms; benefits from cultural consolidation; requires continuous enforcement infrastructure
 *   - coercive_agents: Career professionals (police, military, administrators) who execute enforcement; benefit from institutional position but pay through moral hazard and identity-lock
 *   - subject_population: Bears the direct cost of compliance; noncompliance risks material punishment; trapped exit; resistance is suppressed
 *   - cultural_practitioners: Custodians of displaced prior norms; face suppression of practice; identity-locked exit (abandoning practice = cultural death)
 *   - resistance_movements: Organized opposition; pay through suppression and asymmetric intelligence disadvantage; constrained exit
 *   - competing_legitimacy_sources: Religious authorities, aristocracies, guilds—excluded by design; their presence as alternatives would undermine the state's monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.82).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norms via Monopoly on Violence (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '71c7fa7b-f9b7-4673-a878-2c8aeb280425').
narrative_ontology:cs_kernel_codification('71c7fa7b-f9b7-4673-a878-2c8aeb280425', distributed).
narrative_ontology:cs_authority_grounding('71c7fa7b-f9b7-4673-a878-2c8aeb280425', extraction).
narrative_ontology:cs_interpretation_layer_present('71c7fa7b-f9b7-4673-a878-2c8aeb280425').
narrative_ontology:cs_reading_relation('71c7fa7b-f9b7-4673-a878-2c8aeb280425', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('71c7fa7b-f9b7-4673-a878-2c8aeb280425', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('71c7fa7b-f9b7-4673-a878-2c8aeb280425', foundational, legitimacy_derives_from_force_monopoly).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_force_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('71c7fa7b-f9b7-4673-a878-2c8aeb280425', legitimacy_derives_from_force_monopoly, empirically_contingent).
narrative_ontology:cs_axiom('71c7fa7b-f9b7-4673-a878-2c8aeb280425', foundational, compliance_conditional_on_enforcement_monitoring).
narrative_ontology:cs_axiom_status(compliance_conditional_on_enforcement_monitoring, holdable).
narrative_ontology:cs_axiom_grounding('71c7fa7b-f9b7-4673-a878-2c8aeb280425', compliance_conditional_on_enforcement_monitoring, empirically_contingent).
narrative_ontology:cs_reference_frame('71c7fa7b-f9b7-4673-a878-2c8aeb280425', pre_imposition_pluralistic_authority).
narrative_ontology:cs_drift_state('71c7fa7b-f9b7-4673-a878-2c8aeb280425', contemporary_post_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71c7fa7b-f9b7-4673-a878-2c8aeb280425', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, coercive_agents).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, cultural_practitioners).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, resistance_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) and rising slightly over the interval because the constraint's persistence depends on coercive machinery that extracts compliance from the subject population and suppression capacity from coercive agents. Suppression is very high (0.88) because continuous enforcement is the only mechanism holding the norm in place—without it, reversion to prior practices would be immediate. Theater_ratio rises from 0.45 to 0.65 and back down to 0.62, indicating a classic pattern: early enforcement is crude and visible (low theater); mid-interval sees ideological elaboration (high theater—the state invests in legitimacy myths); late-interval shows the theater cannot fully replace enforcement (theater drops as monitoring resumes). Accessibility_collapse is moderate-high (0.71) because alternatives persist in memory, diaspora, and resistance networks—they are not genuinely extinct, only suppressed. Resistance remains substantial (0.78) throughout, indicating active opposition despite enforcement; a purely internalized norm would show lower measured resistance. The coercion grid shows stakes_inflation highest at the individual level (0.85 at t0)—noncompliance risks personal punishment—and lower at the organizational level (0.72 at t0), where institutions can negotiate or defect collectively. Suppression is consistently very high across all levels (0.87–0.91), indicating pervasive enforcement machinery. The measurements share one time grid (t0, t12, t25, t37, t50, t62, t75, t100) so every metric is authored at every examined point. The basis field distinguishes observed measurements (0–50) from projected (62–100), marking the shift to forward-looking analysis.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and the subject population compute fundamentally different types from the same structural data. The state apparatus seat sees coordination (unification of previously fragmented norms, reduction of inter-group conflict) and denies extraction—the apparatus argues the new norms are superior and have achieved legitimacy. The subject population seat sees pure extraction (loss of prior cultural autonomy, suppression of alternatives, compliance conditional on fear). The coercive agents sit between: they enforce the norm and see both coordination function (their career advancement and institutional stability depend on the norm persisting) and extraction (the moral cost of suppressing populations that do not accept the norm). The engine computes these divergences from the structural data—the beneficiary/victim declarations and exit_options differences. This reading's claim (snare) asserts that extraction dominates coordination from the structurally-determined perspective of those who are targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_apparatus is the beneficiary (sets the rules, enforces them, collects the benefit of cultural consolidation and reduced legitimacy competitors—d near 0.0, full subsidy). The subject_population is the full target (pays through compliance costs, suppression risk, loss of cultural autonomy—d at or near 1.0, full extraction). Coercive_agents are near-symmetric: they benefit from career advancement and institutional position (d lowered by beneficiary effects) but pay through moral hazard and identity-lock (d raised by payer effects)—computed d around 0.45–0.55. Cultural_practitioners are deep victims (identity-locked exit, suppression of core practice—d near 1.0). Resistance_movements are moderately targeted (suppression, asymmetric intelligence disadvantage—d around 0.70). Competing_legitimacy_sources are structurally excluded and would be maximally targeted if brought into the frame (hypothetical d at 1.0), but their exclusion is the point—the constraint exists to prevent their voice from mattering. The directionality overrides are not needed here; the structural derivation captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading declares mandatrophy_resolved: false because the founding problem (consolidation of state authority and elimination of competing legitimacy) is live and actively contested. The state apparatus maintains that consolidation is necessary for order and that the norms have achieved legitimacy through time and effectiveness. The subject population, cultural practitioners, and resistance movements contest both claims: they assert the founding problem was a desire for power, not order; that the norms remain imposed rather than accepted; and that legitimacy is performative. The disappearance_verdict is world_rearranges—if coercive capacity collapsed, the norms would be abandoned and prior practices would resurface. This divergence between the state's claim that the founding problem is solved (order is stable, norms are legitimate) and the divergent stakeholder assessments (order is fragile, norms are enforced) marks an active mandate crisis. The theater_ratio rise to 0.65 and subsequent decline to 0.62 is classic mandatrophy drift: the state invests in legitimacy theater because naked enforcement is becoming costly or ineffective; when theater fails to achieve compliance, enforcement must intensify again. The constraint persists not because the founding problem is solved but because the state apparatus has the enforcement capacity to hold it regardless of legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_compliance_mechanism,
    'Is the measured compliance genuine acceptance of the new norm''s legitimacy, or persistent fear-compliance conditional on enforcement monitoring?',
    'Compare compliance rates when enforcement intensity drops (during crises, reduced funding, administrative collapse) against baseline. If compliance collapses, the mechanism is enforcement-dependent; if it persists, legitimacy has achieved some internalization.',
    'If fear-compliance is the mechanism, the constraint remains a snare even after decades. If legitimacy has internalized across generations, it may be reclassified as a rope (coordination norm that no longer requires as much active suppression). This reading claims the mechanism remains enforcement-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_compliance_mechanism, empirical, 'Whether compliance is conditional on state monitoring or has achieved internalized legitimacy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression structural (external barriers to alternative practices) or internalized (the population believes the new norm is correct and suppresses themselves)?',
    'Post-exit suppression trajectory: if individuals who escape state surveillance (diaspora, emigration) retain the norm without enforcement, suppression has internalized; if they revert to prior practices, suppression is structural. Cultural genealogy interviews with second-generation diaspora populations provide data.',
    'If suppression is structural only, individuals carry low psychological burden post-exit. If internalized, the state has captured cultural identity and the suppression persists in the population''s self-image even after external enforcement ends. This reading suggests suppression has NOT substantially internalized—the population remains coerced rather than convinced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural coercion or internalized cultural adoption.').

omega_variable(
    kernel_reading_contest,
    'How is this constraint—state-imposed norms via violence—one reading of a contested kernel, and what do the sibling readings claim differently?',
    'Historical evidence from norm adoption sequences: (1) Do records show coercive enforcement preceding or following mass adoption? (2) Do early adopters testify that they accepted the norm because of state mandate or because it solved a problem? (3) Does removal of enforcement machinery cause reversion to prior norms? This reading predicts coercive enforcement precedes adoption and reversion occurs without enforcement.',
    'If evidence shows the endogenous_climb_reading is correct, this constraint should be reclassified as a rope (coordination norm that populations adopted because it worked, not because they were forced). If the hybrid reading is correct, the constraint is tangled_rope (both coordination and extraction operate together). If this exogenous_override reading is correct, it remains a snare (pure extraction with compliance conditional on state monitoring). The three readings have different terminal classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the imposition_mechanism_kernel is structurally correct: exogenous override (this reading), endogenous climb (sibling), or hybrid legitimation (sibling)?').

omega_variable(
    beneficiary_vs_performing_compliance,
    'The state apparatus is listed as a beneficiary, but is the actual beneficiary the state qua institution, or the specific regime in power that derives legitimacy from the new norms?',
    'Observe what happens when state apparatus changes hands (regime change, revolution, succession of different ideology). If the new regime retains the old norms because they reinforce state consolidation generally, beneficiary is the institution. If the new regime abandons the old norms and imposes new ones, beneficiary is the specific regime that needed those specific norms.',
    'If the specific regime is the actual beneficiary, removal of the regime should destabilize the norms (prediction of this reading). If the state apparatus qua institution is the beneficiary, norm change at regime level does not destabilize the broader state-imposition machinery—the constraint persists because state consolidation persists even across regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_performing_compliance, empirical, 'Whether the state apparatus or the specific regime is the primary beneficiary of the imposed norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(impo_tr_t12, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 12, 0.51).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(impo_tr_t37, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 37, 0.62).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(impo_tr_t62, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 62, 0.66).
narrative_ontology:measurement(impo_tr_t75, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 75, 0.64).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(impo_be_t12, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(impo_be_t37, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 37, 0.82).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(impo_be_t62, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 62, 0.81).
narrative_ontology:measurement(impo_be_t75, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 75, 0.8).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.91).
narrative_ontology:measurement(impo_su_t12, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 25, 0.89).
narrative_ontology:measurement(impo_su_t37, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 37, 0.88).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(impo_su_t62, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 62, 0.87).
narrative_ontology:measurement(impo_su_t75, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 75, 0.86).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel has three competing readings reflecting different theories of state norm legitimation. The exogenous_override_reading (this constraint) asserts norms are imposed by force and compliance is enforcement-dependent. The endogenous_climb_reading claims norms achieved legitimacy first (bottom-up) and state mandate followed. The hybrid_legitimation_reading claims both mechanisms operated (symbolic transfer + institutional incentives). Each reading is a distinct constraint with its own ε-value, structural data, and terminal classification. They form a family related via network.affects_constraints. The ε-invariance principle applies: measuring legitimation one way (via compliance under enforcement) yields this reading's high extraction; measuring via post-exit behavior would shift the picture. The three readings operationalize three different observables of the same historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
