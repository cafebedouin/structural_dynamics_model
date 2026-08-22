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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Post-1945 Structural Unreachability of Total War (Structural Contraction Reading)
 *   domain: international relations/strategic studies/commitment systems
 *
 * SUMMARY:
 *   Between 1945 and the mid-1960s the material basis of great-power war
 *   changed: thermonuclear yields, dispersed and survivable delivery, and the
 *   arithmetic of retaliation made total war — society-mobilizing war fought
 *   to the adversary's capitulation — a strategy with no victory condition.
 *   This story instantiates the structural_contraction_reading of the kernel
 *   total_war_winnability_post1945: on this reading the disappearance is
 *   physical, not normative or cultural. Total war was not abandoned; it
 *   became unreachable, the way destinations beyond the speed of light are
 *   unreachable. The constraint would hold regardless of who defends it,
 *   enforces it, or believes in it; no party collects from its operation; its
 *   only conceivable victims are the hypothetical populations of a
 *   counterfactual exchange, who would be victims of the violation scenario,
 *   not of the constraint's holding. Beneficiaries and victims are
 *   deliberately undeclared (party-less mountain, gravity-class exemption for
 *   the stakeholder surface's coverage rule). The epsilon referent is the
 *   standing arrangement under contest — the post-1945 strategic environment
 *   in which total war is foreclosed — assessed by this reading's own lights,
 *   giving epsilon near zero: nothing is extracted by a boundary whose
 *   violation destroys the violator. The claim (mountain) and the metrics are
 *   authored independently; the metrics below are this reading's honest
 *   description of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - No primary target exists: the constraint extracts from no actor — the foreclosed strategy carried negative expected value for every potential wager, so its foreclosure takes nothing from anyone.
 *   - No primary beneficiary exists: survival benefits are diffuse, universal, and collected by no seat as rent; the beneficiary declaration is deliberately left empty.
 *   - Hypothetical populations of a counterfactual exchange: the only conceivable victim class — victims of the violation scenario, not of the constraint's operation; counterfactual, therefore absent from the victim declaration and from the stakeholder surface.
 *   - strategic_studies_community: analytical observer — hosts the kernel contest among the three readings, sees the full structure including its contingency on arsenal survivability, and collects nothing from any outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.03).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Post-1945 Structural Unreachability of Total War (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international relations/strategic studies/commitment systems").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad').
narrative_ontology:cs_kernel_codification('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', distributed).
narrative_ontology:cs_authority_grounding('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', foundational, thermonuclear_exchange_precludes_victory).
narrative_ontology:cs_axiom_status(thermonuclear_exchange_precludes_victory, holdable).
narrative_ontology:cs_axiom_grounding('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', thermonuclear_exchange_precludes_victory, empirically_contingent).
narrative_ontology:cs_axiom('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', foundational, option_space_contraction_is_material_not_volitional).
narrative_ontology:cs_axiom_status(option_space_contraction_is_material_not_volitional, holdable).
narrative_ontology:cs_axiom_grounding('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', option_space_contraction_is_material_not_volitional, empirically_contingent).
narrative_ontology:cs_reference_frame('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', materially_contracted_strategic_option_space).
narrative_ontology:cs_drift_state('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', contemporary_counterforce_revival_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('7d4b619e-a8e2-43aa-83fc-b39ca7bf24ad', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, thermonuclear_exchange_consequence_physics).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_annihilation_under_second_strike).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces, contests, and transmits the competing explanations of why total war disappeared from great-power practice; this story is one product of that activity. Gains nothing material from the arrangement it studies and loses nothing by it; professional incentives attach to the contest among explanations rather than to any one explanation winning. Exit is total: if the community stopped analyzing, the situation it studies would be unchanged.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__structural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the direct sense: the contraction solves no collective-action problem and coordinates no behavior — it is a material boundary on the reachable strategy space. Adjacent arrangements (crisis-communication channels, arms-control verification, extended-deterrence guarantees) coordinate because of the condition, but the condition itself is not a coordination mechanism.
% TRANSFER_FUNCTION: Nothing moves. No money, work, attention, or status flows through the constraint's operation; unlike a coordination channel or an extraction pipe, it is a closed door, not a conduit.
% ABSENT_VOICES: No silenced objectors: no living actor defends restoring full total war, and the constraint's only conceivable victims — the populations of a counterfactual exchange — cannot speak; their absence is constitutive rather than procedural. The nearest dissenting voice, the counterforce-optimist doctrinal schools, is present in the professional literature and contests the reading's margins (limited options, escalation control, damage limitation), not its core.
% DISAPPEARANCE_RATIONALE: If the contraction vanished overnight — if total war again offered a credible victory path — alliance architectures, force postures, war plans, and crisis bargaining would rebuild around restored war-termination prospects within a planning cycle or two. Nearly every post-1945 strategic arrangement presupposes unwinnability at the top of the escalation ladder and would have to be re-derived from scratch; the rearrangement would be total even though no seat currently collects from the constraint's holding.
% FOUNDING_PROBLEM: None in the design sense: the contraction was an emergent consequence of building deliverable thermonuclear arsenals for war-fighting and deterrent purposes. It was discovered — in war games, net assessments, and the Cuban close call — not founded to solve a problem. The nearest candidate founding problem, preventing a third world war, was adopted retrospectively by the arrangements that grew up in the contraction's shadow.
% FOUNDING_PROBLEM_CORROBORATION: There are no benefiting parties to self-attest, so the usual origin-myth risk is structurally absent here. Corroboration comes from outside any beneficiary set by construction: declassified war-game and net-assessment records of both cold-war blocs (each independently concluded by the early 1960s that general war had no acceptable termination), the weapons-effects physics literature, and the convergent memoir record of political leaderships on opposing sides. No source attests a designed founding problem, because none exists — that corroborated absence is itself the signal.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is near zero (0.03 at interval end) because the constraint forecloses only options whose pursuit annihilates the option-holder; there is no transfer to intercept. Suppression is near zero (0.02) because no coercive machinery patrols the boundary — physics does not enforce, and the remaining strategy space (limited war, coercion below the threshold, deterrence signaling) stays wide open. Theater is low (0.05): the impossibility requires no performance; the visible nuclear symbolism of the cold war (tests, parades, civil-defense drills, declaratory doctrine) served the adjacent deterrence-credibility and taboo arrangements, with only minor spillover attribution to this one — the measurement series shows that spillover peaking in the atmospheric-test and civil-defense decades and declining thereafter. Accessibility collapse is high (0.93): once exchange dynamics are understood, no workaround restores a victory path, exactly as with natural law — you cannot outmaneuver fallout arithmetic. Resistance is low (0.08): doctrinal schools periodically attempt to re-carve winnable margins (counterforce optimism, limited-nuclear-war theory, damage-limitation claims), but no state has planned or behaved as if full total war were reachable since the early 1960s; the resistance is epistemic, not behavioral. The temporal series are deliberately flat for extractiveness (no T17 accumulation signature — nothing accumulates in a constraint that transfers nothing) and rise-and-fall for theater (tracking the declaratory-symbolic era, not the constraint itself). Both series share one nine-point grid (years since 1945) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The familiar payer-versus-beneficiary seat divergence is absent here by construction: there are no extraction-bearing seats, so per-seat classifications computed from the structural data should converge on the mountain profile for every seated agent. The real perspectival gap in this domain runs ACROSS READINGS rather than across seats: from the normative reading's position, the same seven decades of non-occurrence look like successful law; from the strategic-culture position, successful socialization; from this reading's position, irrelevant physics wearing borrowed credit. Each reading is a separate constraint file with its own epsilon and victim set; this file authors only its own. The one seated observer (the analytical community) sees the full structure, including the contingency of the closure on survivable arsenals — the seat from which the material_basis_contingency omega is visible.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored: no seat collects from the constraint's operation and no seat bears its costs. The foreclosed option carried negative expected value for every potential wager, so closing it extracts nothing and subsidizes nothing. Survival benefits are diffuse, universal, and uncollected by any seat; gain_flow is authored as 'diffuse' as an affirmative checked claim — the only seated actor (the analytical observer) was checked and collects nothing. Directionality derivation therefore runs on canonical fallbacks for the single analytical seat, and no directionality overrides are authored because there is no extraction-bearing relationship for the derivation to get wrong. Scope amplification of effective extraction is inert here: with base epsilon near zero and no targets, scope scales nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy questions presuppose a designed mandate that can outlive its function; this constraint has none — it was discovered, not founded (see six_questions.founding_problem). The classification discipline prevents two characteristic mislabels. First, crediting arms-control treaties or the UN Charter with the non-occurrence of total war would misread a physics-doing as a rope's coordination work; the treaty machinery is real but adjacent, operating in the shadow of the closure rather than producing it. Second, a piton reading would require theatrical maintenance and an administrator who could change the arrangement but declines to; the constraint has neither — no agenda-setter seat exists, and the theater_ratio (0.05) records that almost nothing performative attaches to the closure itself. The genuine obsolescence risks in this kernel live in the sibling constraints (prohibition regimes and doctrines can outlive their functions); this file routes that possibility through the kernel_reading_commitment omega rather than importing it into its own profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the structural_contraction_reading of kernel total_war_winnability_post1945 — what would the sibling readings (normative_reading_drop, strategic_culture_drift) change structurally if adopted instead?',
    'Cross-reading comparison within the kernel family: classify the sibling stories and compare victim sets, epsilon, and type. The disagreement is located in the CAUSE of total war''s disappearance — physics versus legal prohibition versus elite discursive shift.',
    'Adopting normative_reading_drop converts the arrangement from physical impossibility into an enforced prohibition (coordination-plus-enforcement profile with treaty machinery and a real enforcement history); adopting strategic_culture_drift restores reachability and relocates the explanation to ideational drift, yielding a weaker, discourse-maintained constraint. Either adoption dissolves this file''s mountain profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraint files, not parts of this one.').

omega_variable(
    material_basis_contingency,
    'Is the contraction robust to changes in arsenal composition and defensive technology, or is it contingent on the current survivable-second-strike balance?',
    'Net assessment of counterforce and missile-defense trajectories: if deployable defenses ever credibly negate retaliation, the physical closure weakens and the constraint''s basis shifts from material necessity to maintained balance.',
    'Robust closure confirms the mountain profile; fragile closure recasts the constraint as an artifact of a technological equilibrium maintained by arsenal policy, which would migrate its classification as the balance shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_basis_contingency, empirical, 'Whether the physical impossibility is deep (physics) or shallow (current survivability balance).').

omega_variable(
    beneficiary_structure_absence,
    'Is the absence of declared beneficiaries correct — does the arrangement confer only diffuse, uncollected survival benefits, or does some seat covertly collect (for example, arsenal-maintenance budgets justified by appeal to the impossibility)?',
    'Trace budgetary and career rent flows attributable to the impossibility itself versus the adjacent deterrence-posture arrangements; run a false-summit-style audit for concealed capture before accepting the party-less profile.',
    'A concealed collector would trigger false-summit reevaluation away from the mountain claim; confirmed absence secures the party-less profile and the gain_flow=''diffuse'' assertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_absence, empirical, 'Party-less verification: no seat collects from the constraint''s operation.').

omega_variable(
    total_war_boundary_ambiguity,
    'Where does ''total war'' end — does the contraction cover limited nuclear exchanges and protracted conventional great-power war, or only full society-mobilizing, annihilation-seeking war?',
    'Apply the epsilon-invariance decomposition test: if measuring the contraction at the full-exchange extreme versus the limited-options margin yields different structural profiles, author separate stories for (a) full-exchange unwinnability, (b) uncontrollability of limited nuclear war, and (c) unreachability of conventional total war, then compare.',
    'If the components differ structurally, the single-story epsilon masks a constraint family; the mountain claim may hold firmly for (a) while (b) and (c) carry rope-like or contested profiles at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_war_boundary_ambiguity, conceptual, 'Boundary of the contracted region within the reachable strategy space.').

omega_variable(
    counterfactual_victim_status,
    'The only conceivable victim class — populations of a counterfactual exchange — is hypothetical; does a constraint whose victims exist only in the violated counterfactual carry a victim set at all?',
    'Conceptual analysis of victimhood under non-operation: compare with paradigmatic natural limits, where those harmed by the limit''s operation are harmed by events, not by the limit''s holding. Victims of prevention-by-physics are victims of nothing that occurs.',
    'Counting hypothetical exchange populations as a real victim set would inject a victim structure and pull the profile toward extraction-family classifications; denying it secures the party-less mountain. The expected structural delta for this reading assumes denial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_victim_status, conceptual, 'Whether counterfactual populations constitute a victim set for classification purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t7, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 7, 0.04).
narrative_ontology:measurement_basis(tota_tr_t7, observed).
narrative_ontology:measurement(tota_tr_t15, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(tota_tr_t15, observed).
narrative_ontology:measurement(tota_tr_t17, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 17, 0.09).
narrative_ontology:measurement_basis(tota_tr_t17, observed).
narrative_ontology:measurement(tota_tr_t27, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 27, 0.08).
narrative_ontology:measurement_basis(tota_tr_t27, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t56, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 56, 0.05).
narrative_ontology:measurement_basis(tota_tr_t56, observed).
narrative_ontology:measurement(tota_tr_t68, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 68, 0.05).
narrative_ontology:measurement_basis(tota_tr_t68, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t7, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 7, 0.02).
narrative_ontology:measurement_basis(tota_be_t7, observed).
narrative_ontology:measurement(tota_be_t15, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 15, 0.03).
narrative_ontology:measurement_basis(tota_be_t15, observed).
narrative_ontology:measurement(tota_be_t17, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 17, 0.03).
narrative_ontology:measurement_basis(tota_be_t17, observed).
narrative_ontology:measurement(tota_be_t27, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 27, 0.04).
narrative_ontology:measurement_basis(tota_be_t27, observed).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t56, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 56, 0.03).
narrative_ontology:measurement_basis(tota_be_t56, observed).
narrative_ontology:measurement(tota_be_t68, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 68, 0.03).
narrative_ontology:measurement_basis(tota_be_t68, observed).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 80, 0.03).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% Kernel total_war_winnability_post1945 decomposes into three readings held by different parties, authored as separate constraint files per the epsilon-invariance principle. This file instantiates structural_contraction_reading: a physics-closed option space, mountain profile, epsilon near zero, no parties. normative_reading_drop instantiates a prohibition-based arrangement (treaty and customary-law machinery, real enforcement history, coordination-plus-extraction question live). strategic_culture_drift instantiates a discourse-maintained avoidance (reachability retained, ideationally steered). The epsilon values differ because the referent arrangements differ: a physically foreclosed strategy extracts nothing; a prohibited one raises enforcement and compliance costs; a culturally avoided one raises only discourse-maintenance costs. This reading sits upstream in the family: if the physical closure holds, it drains independent explanatory work from both siblings — norms and culture cannot take credit for preventing the physically impossible — which is why both network edges run from this file. The edges record structural influence on the siblings' operating environments, not endorsement or refutation of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
