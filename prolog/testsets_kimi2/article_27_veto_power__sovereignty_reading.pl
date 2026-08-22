% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto â Sovereignty Reading (Westphalian Mountain)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   The P5 veto power under Article 27 of the UN Charter is read here as an
 *   instantiation of the Westphalian sovereignty principle: no state can be
 *   bound by international law without its consent, and this principle
 *   becomes materially irresistible when applied to great powers possessing
 *   global-reach enforcement capacity and nuclear deterrents. In this
 *   reading, the veto is not an institutional grant of privilege but a
 *   recognition of a structural limit â any attempt by the Security Council
 *   to compel a great power against its will would fail because no superior
 *   enforcement mechanism exists. The constraint therefore has near-zero
 *   extractiveness: it does not transfer value from non-P5 states to P5
 *   states; rather, it registers the absence of any feasible enforcement
 *   path. This constraint is one reading of the contested
 *   article_27_veto_power kernel. Sibling readings (coordination_reading,
 *   oligopoly_reading) instantiate structurally distinct constraints and are
 *   documented as separate stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: Great powers whose enforcement capacity and nuclear arsenals make external compulsion structurally impossible â they are not beneficiaries of extraction but occupy positions of structural autonomy.
 *   - non_p5_un_member_states: The broad UN membership, subject to Council authority on paper but not the seat from which the constraint's structural limit emanates.
 *   - un_security_council_secretariat: Administrative body that operates the voting machinery; it does not extract from the veto but maintains the procedural form through which the structural limit is expressed.
 *   - international_court_of_justice: Judicial body that has occasionally challenged great-power immunity, representing tension between legal formalism and the sovereignty reading's power-realist premise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.06).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.15).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto â Sovereignty Reading (Westphalian Mountain)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'd9a13cad-bad9-4fc0-9438-8e59c6b91827').
narrative_ontology:cs_kernel_codification('d9a13cad-bad9-4fc0-9438-8e59c6b91827', formalized).
narrative_ontology:cs_authority_grounding('d9a13cad-bad9-4fc0-9438-8e59c6b91827', lineage).
narrative_ontology:cs_interpretation_layer_present('d9a13cad-bad9-4fc0-9438-8e59c6b91827').
narrative_ontology:cs_reading_relation('d9a13cad-bad9-4fc0-9438-8e59c6b91827', article_27_veto_power__coordination_reading, influences).
narrative_ontology:cs_reading_relation('d9a13cad-bad9-4fc0-9438-8e59c6b91827', article_27_veto_power__oligopoly_reading, forecloses).
narrative_ontology:cs_axiom('d9a13cad-bad9-4fc0-9438-8e59c6b91827', foundational, westphalian_consent_principle).
narrative_ontology:cs_axiom_status(westphalian_consent_principle, holdable).
narrative_ontology:cs_axiom_grounding('d9a13cad-bad9-4fc0-9438-8e59c6b91827', westphalian_consent_principle, deontological).
narrative_ontology:cs_axiom('d9a13cad-bad9-4fc0-9438-8e59c6b91827', secondary, great_power_autonomy_irresistible).
narrative_ontology:cs_axiom_status(great_power_autonomy_irresistible, holdable).
narrative_ontology:cs_axiom_grounding('d9a13cad-bad9-4fc0-9438-8e59c6b91827', great_power_autonomy_irresistible, empirically_contingent).
narrative_ontology:cs_reference_frame('d9a13cad-bad9-4fc0-9438-8e59c6b91827', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('d9a13cad-bad9-4fc0-9438-8e59c6b91827', contemporary_multilateral_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9a13cad-bad9-4fc0-9438-8e59c6b91827', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

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
% COORDINATION_FUNCTION: Prevents the Security Council from issuing resolutions that a great power would reject and defy, thereby avoiding institutional collapse or systemic war that would follow from an unenforceable command.
% TRANSFER_FUNCTION: No material transfer; the constraint registers the impossibility of transferring enforcement capacity from the collective to the great power.
% ABSENT_VOICES: Advocates of supranational enforcement (world federalists), scholars of Responsibility to Protect who argue sovereignty is conditional, and non-P5 state delegations that opposed the veto at San Francisco but were excluded from the final framing.
% DISAPPEARANCE_RATIONALE: If the veto rule vanished overnight but the underlying distribution of nuclear and conventional enforcement capacity remained, great powers would continue to ignore or defy any purportedly binding resolution they opposed; the procedural form would disappear but the structural impossibility of binding them would remain unchanged.
% FOUNDING_PROBLEM: How to construct a post-war collective security architecture that includes all major military powers as members rather than adversaries, given that no institution could credibly compel them by force.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the 1945 San Francisco Conference from non-P5 national archives and critical international legal scholars outside the P5 foreign-policy establishments attest that the veto was designed to solve the specific problem of great-power inclusion; small-power objections at San Francisco corroborate that the problem was defined by the great powers and imposed on others.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.06, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.06 because the sovereignty reading treats the veto as epiphenomenal to the distribution of coercive capacity: the transfer surface is empty. Suppression is 0.15 because the constraint does not require active coercion to persist; its force is the absence of a plausible enforcer. Accessibility collapse is 0.92 because once the enforcement asymmetry is understood, alternatives (e.g., a binding supranational security regime) collapse as structurally infeasible. Resistance is 0.05 because the constraint meets little active resistance from non-P5 states in the specific dimension of great-power bindability â they do not attempt to enforce against a P5 because they know it would fail. Theater ratio is 0.10 because the procedural performance of veto votes is largely faithful to the underlying power distribution; there is little performative maintenance detached from function. The measurement series shows stability across the interval, consistent with a mountain whose parameters are anchored to military-technical realities rather than institutional drift.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty reading (this story) computes as Mountain because it treats the veto as the visible form of an underlying power reality. The oligopoly reading computes as Snare or Tangled Rope because it identifies identifiable beneficiaries (P5 states capturing authority rents) and victims (non-P5 states blocked from institutional change). The coordination reading computes as Rope because it foregrounds the war-prevention function. These divergences are not errors but the corpus's intended output: the same kernel emits different constraints when read through different structural framings.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading there are no declared beneficiaries or victims. The P5 states are structurally autonomous rather than extracting through the constraint; non-P5 states are structurally subordinate in enforcement capacity but not paying through this specific mechanism. Directionality is therefore not derived from an asymmetric transfer. The engine will compute near-zero effective extraction for all seats because the base extractiveness is near-zero and no beneficiary-victim asymmetry is declared.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading resists mandatrophy by grounding the veto's persistence in the ongoing problem of enforcement asymmetry. If the great powers' military capacity were equalized or a supranational enforcement mechanism emerged, the founding problem would be dead and the veto would become a Piton (theatrical maintenance of an atrophied function). As of the interval end, the founding problem remains live: nuclear deterrence and global force projection still make P5 compulsion infeasible. Thus the constraint is not a zombie institution but a continuing register of structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_oligopoly_framing,
    'Is the P5 veto an unavoidable structural feature of an anarchic international system with extreme power asymmetry, or a constructed institutional privilege that perpetuates a geopolitical oligopoly?',
    'Comparative historical analysis of attempts to enforce Security Council resolutions against P5 interests, and counterfactual analysis of whether a binding collective security mechanism could operate without great-power consent.',
    'If the constraint is constructed rather than structural, the Mountain classification collapses and the constraint reclassifies as Tangled Rope or Snare; if structural, the oligopoly reading is a misattribution of institutional form to power reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_oligopoly_framing, conceptual, 'Core framing ambiguity between natural-law and constructed-privilege readings of the veto').

omega_variable(
    nuclear_deterrent_irreversibility,
    'Does the constraint''s structural inevitability depend on the persistence of nuclear deterrence and conventional enforcement asymmetry, or would it persist even under radical power redistribution?',
    'Empirical observation of systemic change in great-power military capacity and strategic stability; hypothetical analysis of enforcement-capacity equalization.',
    'If the constraint is contingent on current military-technology and power distribution, its Mountain status is conditional rather than absolute; if it persists under any power distribution, it is a deeper institutional Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deterrent_irreversibility, empirical, 'Whether the veto''s structural inevitability is contingent on current military-technical realities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(arti_tr_t15, article_27_veto_power__sovereignty_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(arti_tr_t45, article_27_veto_power__sovereignty_reading, theater_ratio, 45, 0.09).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(arti_tr_t77, article_27_veto_power__sovereignty_reading, theater_ratio, 77, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(arti_be_t15, article_27_veto_power__sovereignty_reading, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(arti_be_t45, article_27_veto_power__sovereignty_reading, base_extractiveness, 45, 0.07).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(arti_be_t77, article_27_veto_power__sovereignty_reading, base_extractiveness, 77, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, oligopoly_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the article_27_veto_power family. The kernel (UN Charter Article 27) decomposes into three structurally distinct constraints because the epsilon values and beneficiary/victim structures differ across readings: sovereignty_reading (epsilon near-zero, Mountain), coordination_reading (coordination function, lower extraction), and oligopoly_reading (asymmetric extraction, Snare/Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
