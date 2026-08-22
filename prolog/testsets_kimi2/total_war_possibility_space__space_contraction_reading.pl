% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear-Era Contraction of Total War Strategic Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the space_contraction_reading of the
 *   total_war_possibility_space kernel: the claim that nuclear weapons did
 *   not merely raise the cost or establish a taboo against great-power total
 *   war, but removed it entirely from the strategically thinkable, producing
 *   categorical impossibility in planning space. The reading predicts
 *   institutional atrophyâmobilization doctrines disappearing,
 *   general-staff war-gaming ceasing, strategic studies shifting to
 *   sub-nuclear domainsâand treats this atrophy as evidence that the
 *   constraint operates as a material-cognitive limit rather than a norm or
 *   equilibrium. The kernel is contested by two sibling readings:
 *   deterrence_equilibrium (mutual vulnerability sustains a thinkable but
 *   deterred option) and nuclear_taboo (normative prohibition independent of
 *   material capability). This story authors the metrics for the
 *   space_contraction reading alone; the divergence between its Mountain
 *   claim and its moderate theater ratio is the measurement the corpus exists
 *   to take.
 *
 * KEY AGENTS:
 *   - nuclear_strategists: Analytical observers tracking the contraction of their field
 *   - general_staff_planners: Institutional actors whose planning repertoire has atrophied
 *   - great_power_polities: States whose strategic possibility space is delimited by the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.02).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.96).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Era Contraction of Total War Strategic Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, 'bbc7362f-a457-4876-8945-eaa0ed1a22dd').
narrative_ontology:cs_kernel_codification('bbc7362f-a457-4876-8945-eaa0ed1a22dd', implicit).
narrative_ontology:cs_authority_grounding('bbc7362f-a457-4876-8945-eaa0ed1a22dd', self_enforcing).
narrative_ontology:cs_reading_relation('bbc7362f-a457-4876-8945-eaa0ed1a22dd', total_war_possibility_space__deterrence_equilibrium_reading, influences).
narrative_ontology:cs_reading_relation('bbc7362f-a457-4876-8945-eaa0ed1a22dd', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('bbc7362f-a457-4876-8945-eaa0ed1a22dd', foundational, total_war_categorically_unthinkable).
narrative_ontology:cs_axiom_status(total_war_categorically_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('bbc7362f-a457-4876-8945-eaa0ed1a22dd', total_war_categorically_unthinkable, empirically_contingent).
narrative_ontology:cs_axiom('bbc7362f-a457-4876-8945-eaa0ed1a22dd', foundational, strategic_thought_follows_material_capability).
narrative_ontology:cs_axiom_status(strategic_thought_follows_material_capability, holdable).
narrative_ontology:cs_axiom_grounding('bbc7362f-a457-4876-8945-eaa0ed1a22dd', strategic_thought_follows_material_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('bbc7362f-a457-4876-8945-eaa0ed1a22dd', total_war_planning_norm).
narrative_ontology:cs_drift_state('bbc7362f-a457-4876-8945-eaa0ed1a22dd', nuclear_age_institutional_atrophy, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bbc7362f-a457-4876-8945-eaa0ed1a22dd', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates the need for interstate coordination over total-war thresholds by removing total war from the set of strategically conceivable actions; the constraint operates as a material limit rather than a bargained equilibrium.
% TRANSFER_FUNCTION: No inter-agent resource transfer; the constraint reallocates strategic cognitive capacity and institutional focus from total-war mobilization planning toward sub-nuclear and deterrence frameworks within polities.
% ABSENT_VOICES: Pre-nuclear total-war theorists and mass-mobilization planners are epistemically absentâtheir frameworks are not refuted but rendered operationally obsolete. Non-great-power actors and tactical-nuclear advocates are underrepresented in the canonical strategic discourse.
% DISAPPEARANCE_RATIONALE: If total war re-entered the strategically thinkable, general staffs would revive mobilization doctrines, war colleges would reintroduce great-power total-war scenarios, defense industrial bases would reorient toward mass mobilization, and strategic studies would reclaim the classical canon. The institutional landscape would fundamentally reorganize around the restored possibility.
% FOUNDING_PROBLEM: The material condition of great-power politics under nuclear-armed states, where the destructive capacity of warfare outstrips any coherent political objective.
% FOUNDING_PROBLEM_CORROBORATION: Materialist strategic theorists (e.g., Bernard Brodie, Kenneth Waltz) attest the problem as structurally solved by technological imperative. Constructivists and deterrence theorists contest that the arrangement is material rather than normative or equilibrium-based. No independent corroboration exists from outside the strategic studies discourse; the genealogy is self-attested by the reading's own theoretical tradition.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.05) because the reading asserts no rent transfer; the constraint is a material limit analogous to a physical law of strategy. Suppression is near zero (0.02) because persistence requires no enforcement. Accessibility collapse is extreme (0.96) because once the nuclear condition is understood, total war alternatives evaporate from planning space. Resistance is negligible (0.02) because no actor seriously contests the constraint's operation. Theater ratio is modest (0.20 at interval end) because while the constraint is material, the institutional remnants of total-war planning (ceremonial war colleges, obsolete mobilization bureaucracies) generate performative maintenance that is not functional. The measurement series tracks rising theater against flat extraction to model institutional atrophy without extraction accumulation.
 *
 * PERSPECTIVAL GAP:
 *   No seated payer/beneficiary divergence is authored because the constraint is modeled as having no parties: it is a Mountain. However, if the constraint were re-read as a normative or institutional artifact, general staffs and defense industrial bases would appear as payers (functional atrophy as cost) and civilian populations as beneficiaries (avoided annihilation). That re-reading would produce a Tangled Rope or Snare profile; the gap between the authored Mountain and the counterfactual seated reading is documented in the kernel-context omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are made, so directionality reverts to canonical fallbacks per power atom. For a Mountain, this is appropriate: the constraint does not subsidize or extract from specific agents; it delimits the environment within which all agents operate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by distinguishing material impossibility from coordination. A deterrence equilibrium could decay into mandatrophy if the coordination function atrophied while the threat posture persisted as theater. Here, the reading claims the atrophy is the constraint's mechanism, not its pathology: as total war becomes unthinkable, the institutions that planned for it correctly wither. The theater ratio captures remnant performance, not a hidden coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_vs_constructed_unthinkability,
    'Is the unthinkability of total war a direct material consequence of nuclear destructiveness, or is it mediated by constructed norms, doctrines, and institutional taboos?',
    'Comparative analysis of strategic planning documents across nuclear and pre-nuclear eras; examination of whether planners literally cannot conceive of total war or merely choose not to plan for it.',
    'If constructed, effective extraction rises and classification shifts toward normative or equilibrium constraint; if material, Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_constructed_unthinkability, conceptual, 'Whether total war unthinkability is material or constructed').

omega_variable(
    institutional_atrophy_reversibility,
    'Is the institutional atrophy of total-war planning apparatus reversible, or has the epistemic community lost the generative capability to regenerate it?',
    'Observation of re-mobilization exercises, curriculum revivals, or planning shifts under changed geopolitical conditions.',
    'If irreversible, the constraint is deeper than institutional habit; if reversible, the constraint contains inertial or performative components.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_atrophy_reversibility, empirical, 'Whether institutional atrophy is reversible').

omega_variable(
    deterrence_credibility_under_atrophy,
    'Does the atrophy of total-war planning undermine the credibility of nuclear deterrence, which relies on the believability of the threat?',
    'Strategic simulations and historical case studies comparing deterrent postures with and without active total-war planning staff.',
    'If credibility erodes, the constraint may generate destabilizing feedback loops despite its apparent stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_credibility_under_atrophy, empirical, 'Impact of planning atrophy on deterrence credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(total_war_space_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(total_war_space_tr_t10, total_war_possibility_space__space_contraction_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(total_war_space_tr_t20, total_war_possibility_space__space_contraction_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(total_war_space_tr_t30, total_war_possibility_space__space_contraction_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(total_war_space_tr_t40, total_war_possibility_space__space_contraction_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(total_war_space_tr_t50, total_war_possibility_space__space_contraction_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(total_war_space_tr_t60, total_war_possibility_space__space_contraction_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(total_war_space_tr_t70, total_war_possibility_space__space_contraction_reading, theater_ratio, 70, 0.27).
narrative_ontology:measurement(total_war_space_tr_t80, total_war_possibility_space__space_contraction_reading, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(total_war_space_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(total_war_space_be_t10, total_war_possibility_space__space_contraction_reading, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(total_war_space_be_t20, total_war_possibility_space__space_contraction_reading, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(total_war_space_be_t30, total_war_possibility_space__space_contraction_reading, base_extractiveness, 30, 0.03).
narrative_ontology:measurement(total_war_space_be_t40, total_war_possibility_space__space_contraction_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(total_war_space_be_t50, total_war_possibility_space__space_contraction_reading, base_extractiveness, 50, 0.04).
narrative_ontology:measurement(total_war_space_be_t60, total_war_possibility_space__space_contraction_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(total_war_space_be_t70, total_war_possibility_space__space_contraction_reading, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(total_war_space_be_t80, total_war_possibility_space__space_contraction_reading, base_extractiveness, 80, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel total_war_possibility_space. The kernel decomposes into three structurally distinct claims: deterrence equilibrium (coordination via mutual vulnerability), nuclear taboo (normative prohibition), and space contraction (material-cognitive impossibility). Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
