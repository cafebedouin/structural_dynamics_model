% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Nuclear Weapons Remove Total War from Reachable Strategic Space (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   The structural-contraction reading asserts that nuclear weapons created a
 *   physical impossibility: no state can achieve victory in a total war when
 *   both sides possess second-strike capability (assured mutual destruction
 *   geometry). This is not a normative claim that total war became
 *   illegitimate, nor a cultural observation that strategic elites stopped
 *   thinking about it — it is a geometric fact about the reachable state
 *   space. Once nuclear weapons exist, the strategy space contracts so that
 *   total-war outcomes (unconditional surrender, decisive military victory,
 *   territorial conquest) become non-winnable. The impossibility is
 *   structural, not chosen; it persists regardless of normative commitments
 *   or strategic culture, because the physics of mutual vulnerability does
 *   not care about human intention.
 *
 * KEY AGENTS:
 *   - Humanity (undifferentiated): structurally protected from total war victimization by nuclear weapons' mutual-vulnerability geometry, despite no explicit coordinating body or beneficiary institution
 *   - Strategic planners (during 1945–2045 interval): their abandonment of total-war doctrine follows the contraction of winnability, not normative legislation or cultural shift
 *   - Nuclear weapons states (as a class): individually constrained by mutual vulnerability; any one could attempt total war, but the physics of retaliation makes victory unattainable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.08).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Nuclear Weapons Remove Total War from Reachable Strategic Space (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '38f96811-3b89-47aa-9108-c9cfbb898cf8').
narrative_ontology:cs_kernel_codification('38f96811-3b89-47aa-9108-c9cfbb898cf8', distributed).
narrative_ontology:cs_authority_grounding('38f96811-3b89-47aa-9108-c9cfbb898cf8', none).
narrative_ontology:cs_reading_relation('38f96811-3b89-47aa-9108-c9cfbb898cf8', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('38f96811-3b89-47aa-9108-c9cfbb898cf8', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('38f96811-3b89-47aa-9108-c9cfbb898cf8', foundational, mutual_vulnerability_makes_total_war_unwinnable).
narrative_ontology:cs_axiom_status(mutual_vulnerability_makes_total_war_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('38f96811-3b89-47aa-9108-c9cfbb898cf8', mutual_vulnerability_makes_total_war_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('38f96811-3b89-47aa-9108-c9cfbb898cf8', foundational, winnability_is_structural_not_chosen).
narrative_ontology:cs_axiom_status(winnability_is_structural_not_chosen, holdable).
narrative_ontology:cs_axiom_grounding('38f96811-3b89-47aa-9108-c9cfbb898cf8', winnability_is_structural_not_chosen, deontological).
narrative_ontology:cs_reference_frame('38f96811-3b89-47aa-9108-c9cfbb898cf8', nuclear_mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('38f96811-3b89-47aa-9108-c9cfbb898cf8', contemporary_2025, gap(stable, minor, false)).
narrative_ontology:cs_created_at('38f96811-3b89-47aa-9108-c9cfbb898cf8', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is minimal (0.08 at interval end, starting near 0.05) because the constraint is not being actively wielded by beneficiaries or administrators — it operates as a structural fact, like gravity. No institution collects rents from total-war impossibility; no party exerts enforcement effort to maintain the physics. The constraint is discovered, not built. Theater ratio is similarly minimal (0.05–0.02 range): there is no performative maintenance, no symbolic ritual of keeping the constraint alive, because it does not depend on social coordination or institutional theater. Suppression is negligible (0.01–0.02) because there is no suppression — total war is not suppressed by agents; it is made impossible by physics. Accessibility-collapse is very high (0.92): once the nuclear-mutual-vulnerability frame is understood, the impossibility of total war is complete and unavoidable. No side can access a path to total-war victory, no matter the military expenditure or technology short of fundamentally breaking mutual vulnerability (which the constraint treats as fixed within the interval). Resistance is minimal (0.03): there is little meaningful resistance to a physical law; some states might wish to escape mutual vulnerability, but wishing does not overcome the constraint.
 *
 * PERSPECTIVAL GAP:
 *   All seats (nuclear states, non-nuclear states, populations, strategic planners) perceive the constraint identically: total war is not reachable. Unlike Tangled Rope constraints that create beneficiary/victim asymmetry, this constraint operates uniformly across all stakeholders because it is a fact of physics, not a distributed institutional arrangement. The strategic posture of any state cannot change the geometry that makes total war impossible. This uniformity of perception is itself the signature of a genuine Mountain constraint — the constraint is not perspectival; it does not dissolve or reappear depending on who observes it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is inapplicable in the classical sense because there is no extraction, no transfer, and no beneficiary/victim structure in the institutional sense. The hypothetical victims (populations that would die in a total war if it were winnable) are protected by the constraint, but they do not benefit from an extracted transfer; they benefit from the constraint's structural imposition. There is no agenda-setter, no payer, no coordinated exchange — there is only a state of the world that makes certain outcomes impossible. The entire population of Earth sits at d ≈ 0.0 (full beneficiary of the protection), but this beneficiary status is not purchased at a cost paid by others; it is simply the consequence of living in a world where mutual nuclear vulnerability makes total war non-winnable.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint because there is no mandate, no founding problem that the constraint was built to solve. The constraint is not an institution or policy; it is a physical fact. Mandatrophy asks whether the original problem still exists (live/dead/contested); this constraint does not rely on any problem remaining live. Even if the original founding problem (the terror of conventional total war) were entirely solved by other means (precision weapons, international law, cultural shift), the nuclear-mutual-vulnerability constraint would remain in place because it is not contingent on the founding problem — it is a geometric feature of the state space once nuclear weapons exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_kernel,
    'Is the structural impossibility of total war a law of physics (the reading''s axiom), or a contingent property of the current technological state that could be altered by weapons development or delivery system innovation?',
    'Long-term tracking of weapons capability development; assessment of whether anti-ballistic missile, space-based defense, or other technological advancement could restore winnability of total war despite mutual vulnerability.',
    'If the impossibility is contingent on current technology, the constraint should reclassify to Tangled Rope (constructed institutional commitment relying on technological contingency). If the impossibility is intrinsic to mutual-vulnerability geometry, Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_kernel, empirical, 'Whether structural contraction is a permanent physical law or a temporary technological window.').

omega_variable(
    sibling_reading_epistemic_status,
    'Which reading of the total_war_winnability_post1945 kernel most accurately describes the mechanism by which total war disappeared from elite strategic planning: (a) structural impossibility imposed by physics (this reading), (b) normative delegitimation through law and humanitarian doctrine, or (c) ideational drift in strategic culture independent of physical constraint?',
    'Discourse analysis of strategic literature and policy documents from 1945–present, cross-referenced against weapons capability timelines; interviews with strategic planners about their models of victory/defeat in nuclear scenarios; empirical test of whether normative and cultural shifts would have occurred in the absence of mutual vulnerability.',
    'If empirical analysis shows strategic planners abandoned total war BECAUSE of mutual vulnerability physics (not despite it), this reading''s axiom holds and sibling readings are partial explanations of mechanisms, not independent causes. If abandonment is shown to be primarily normative/cultural (weapons physics merely provided occasion), this reading should reclassify and the sibling readings rise to primary explanatory status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_epistemic_status, conceptual, 'Epistemic competition among three readings of the same kernel: mechanism of total war''s disappearance.').

omega_variable(
    hypothetical_victim_set_modal_status,
    'In the counterfactual world where nuclear weapons were never invented, would populations that are currently protected by the impossibility-of-total-war constraint have been victimized by conventional total war? Or would other technologies (precision munitions, drone warfare, autonomous systems) have made conventional total war equally non-viable?',
    'Counterfactual historical analysis; expert assessment of whether conventional technologies alone would have produced mutual vulnerability and winnability impossibility; comparison to pre-nuclear total-war casualty profiles.',
    'If nuclear weapons are the unique technology preventing total war victimization, the hypothetical victim set is real and the constraint is non-extractive (populations bear no cost, gain defensive shelter). If other technologies would produce equivalent constraint, the victim counterfactual is attenuated and the classification remains robust as Mountain but the causal narrative is revised.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypothetical_victim_set_modal_status, empirical, 'What counterfactual world the hypothetical victims inhabit and whether nuclears are the unique cause of their protection.').

omega_variable(
    reading_kernel_committer_contest,
    'The three readings (structural_contraction, normative_reading_drop, strategic_culture_drift) all describe a real historical fact: total war dropped from elite discourse and practice after 1945. Does the structural-contraction reading claim to EXPLAIN that fact (mechanism of disappearance from reachable space) or merely to GROUND its persistence (why it doesn''t re-emerge when normative commitments erode or cultural drift reverses)?',
    'Specification of what the reading claims to explain vs. what it leaves open to sibling readings. A Mountain reading that grounds persistence but leaves disappearance to siblings is coherent; a Mountain reading claiming to explain why it disappeared commits to the irrevocability of the impossibility, which is a stronger empirical claim.',
    'If the reading is a persistence-grounder, sibling readings can explain initial abandonment without logical conflict (coexists_with relation holds). If the reading claims to explain disappearance as well, one of the siblings must foreclose (the reading''s impossibility-axiom rules out the normative/cultural-shift reading as the primary cause).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_committer_contest, conceptual, 'Clarification of what the structural-contraction reading''s scope of explanation is within the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2045).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1965, 0.03).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.05).
narrative_ontology:measurement(tota_tr_t2045, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2045, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1985, 0.07).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2005, 0.08).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement(tota_be_t2045, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2045, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1945, 0.01).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1965, 0.01).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1985, 0.02).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2005, 0.02).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2025, 0.02).
narrative_ontology:measurement(tota_su_t2045, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2045, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
