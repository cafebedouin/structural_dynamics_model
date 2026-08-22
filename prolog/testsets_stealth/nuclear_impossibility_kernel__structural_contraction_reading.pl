% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Mutual-Annihilation Impossibility (Structural Contraction Reading)
 *   domain: strategic studies/international relations
 *
 * SUMMARY:
 *   The standing arrangement under contest is the condition of great-power
 *   relations under guaranteed mutual annihilation. This file instantiates
 *   the structural_contraction_reading of the nuclear_impossibility_kernel:
 *   on this reading the condition is a physical and logical limit, not a
 *   policy choice — once survivable second-strike forces exist, general war
 *   terminates in mutual destruction as surely as unsupported mass falls, and
 *   war exits the reachable set of statecraft. Epsilon is authored for THAT
 *   standing arrangement, by this reading's own lights, and is therefore very
 *   low: the arrangement removes only options whose exercise meant death, and
 *   it is enforced by no one. Beneficiaries are declared (populations,
 *   leaderships, establishments), which makes this a deliberate false-summit
 *   candidate: the story carries omegas documenting the irreducible ambiguity
 *   between natural-law and maintained-condition readings, and the engine is
 *   expected to evaluate the FSM signature rather than have the claim
 *   pre-reconciled to the metrics. The sibling readings are separate
 *   constraints with their own epsilon values and are linked, not absorbed.
 *   KEY AGENTS (by structural relationship): -
 *   great_power_civilian_populations: Primary beneficiary (moderate/trapped)
 *   — receives the great-power peace the condition guarantees; cannot exit
 *   mutual vulnerability - national_security_establishments: Administrator
 *   and secondary beneficiary (institutional/identity_locked) — maintains the
 *   second-strike forces that constitute the condition's substrate -
 *   national_political_leaderships: Beneficiary with sovereignty costs
 *   (powerful/constrained) — protected from total war, stripped of war as an
 *   instrument - non_nuclear_states: Excluded voice (organized/trapped) —
 *   bound by an order structured by arsenals they do not hold -
 *   counterforce_warfighting_advocates: Excluded voice (moderate/constrained)
 *   — internal dissenters whose escape-route program the condition rules out
 *   - strategic_studies_analysts: Analytical observer (analytical/analytical)
 *   — sees the full structure and the contest among readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.08).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Mutual-Annihilation Impossibility (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic studies/international relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '8c259bbe-50c2-4a43-86c6-860fd4587269').
narrative_ontology:cs_kernel_codification('8c259bbe-50c2-4a43-86c6-860fd4587269', distributed).
narrative_ontology:cs_authority_grounding('8c259bbe-50c2-4a43-86c6-860fd4587269', expertise).
narrative_ontology:cs_interpretation_layer_present('8c259bbe-50c2-4a43-86c6-860fd4587269').
narrative_ontology:cs_reading_relation('8c259bbe-50c2-4a43-86c6-860fd4587269', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('8c259bbe-50c2-4a43-86c6-860fd4587269', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('8c259bbe-50c2-4a43-86c6-860fd4587269', foundational, mutual_annihilation_only_general_war_outcome).
narrative_ontology:cs_axiom_status(mutual_annihilation_only_general_war_outcome, holdable).
narrative_ontology:cs_axiom_grounding('8c259bbe-50c2-4a43-86c6-860fd4587269', mutual_annihilation_only_general_war_outcome, empirically_contingent).
narrative_ontology:cs_axiom('8c259bbe-50c2-4a43-86c6-860fd4587269', foundational, victory_not_in_reachable_set).
narrative_ontology:cs_axiom_status(victory_not_in_reachable_set, holdable).
narrative_ontology:cs_axiom_grounding('8c259bbe-50c2-4a43-86c6-860fd4587269', victory_not_in_reachable_set, empirically_contingent).
narrative_ontology:cs_axiom('8c259bbe-50c2-4a43-86c6-860fd4587269', secondary, restraint_independent_of_threat_credibility).
narrative_ontology:cs_axiom_status(restraint_independent_of_threat_credibility, holdable).
narrative_ontology:cs_axiom_grounding('8c259bbe-50c2-4a43-86c6-860fd4587269', restraint_independent_of_threat_credibility, instrumental).
narrative_ontology:cs_reference_frame('8c259bbe-50c2-4a43-86c6-860fd4587269', assured_second_strike_equilibrium).
narrative_ontology:cs_drift_state('8c259bbe-50c2-4a43-86c6-860fd4587269', contemporary_counterforce_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('8c259bbe-50c2-4a43-86c6-860fd4587269', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, great_power_civilian_populations).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, national_security_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, national_political_leaderships).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, second_strike_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the condition that any general war between their states ends in everyone's destruction. They receive the longest great-power peace in modern history as a direct effect of that condition. They did not choose it and cannot opt out of it — moving abroad does not remove them from a planet-scale consequence — and they pay for it only indirectly, through taxes that fund the forces that keep the condition in place.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, great_power_civilian_populations, beneficiary,
    moderate, generational, trapped, global).

% Design, build, and operate the submarines, missiles, bombers, and command systems whose survivability makes the annihilation outcome certain. Their budgets, careers, and institutional continuity depend on those forces staying ready, and their members carry standing personal responsibility for avoiding accidental catastrophe. Leaving the profession means abandoning the expertise that defines them; the mission has no successor skill set.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, national_security_establishments, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, national_security_establishments, beneficiary).

% Hold nominal command of instruments they can never rationally use against another nuclear state. They gain regime survival and freedom from great-power attack, and they lose war as an instrument of policy — a loss felt acutely when local defeats cannot be escalated. Renouncing the forces unilaterally would expose their state during the transition; no leader has found an exit that does not pass through vulnerability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, national_political_leaderships, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, national_political_leaderships, agenda_setter).

% Inherited an international order priced and policed by arsenals they do not hold. They signed non-proliferation bargains under implicit duress, finance the order's institutions, and host the proxy battlefields where great-power rivalry is displaced. They have no seat where the condition is managed and no way to leave the order it structures.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, excluded,
    organized, generational, trapped, global).

% Strategists inside and adjacent to the establishments who argue that damage limitation and war-fighting options are achievable and necessary. Their programs — hardened-kill accuracy, missile defense, prompt strike — are the main funded attempt to route around the annihilation outcome. Their proposals are repeatedly ruled out by the physics they are asked to plan against, and their careers advance only at the margins.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, counterforce_warfighting_advocates, excluded,
    moderate, biographical, constrained, national).

% Scholars and former officials who map the condition, run the wargames, and referee the argument over what nuclear weapons created. They hold no stake in the forces themselves; their influence runs through publication and advisory channels.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__structural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the outcome of any general war between nuclear-armed states as mutual annihilation, solving the problem of how rival great powers can coexist indefinitely without repeating the total-war pattern that twice destroyed the international order. Restraint is not chosen crisis by crisis; it is structurally compelled.
% TRANSFER_FUNCTION: Moves nothing material. It deletes war-as-instrument from every great-power menu simultaneously, converting would-be conquest gains into guaranteed losses and transferring security from whichever side holds conventional advantage to all sides equally, through parity of vulnerability.
% ABSENT_VOICES: Non-nuclear states never consented to a world order structured by mutual hostage-taking and appear only as treaty signatories under duress. Counterforce and warfighting strategists sit inside the establishments, but the program they exist to advance is ruled out by the condition they are asked to plan against. Future generations, who bear the tail risk of arsenal accidents and proliferation cascades, have no seat at all.
% DISAPPEARANCE_RATIONALE: If mutual annihilation ceased to be guaranteed overnight — through perfect defenses, disarmed arsenals, or a decoupling of use from annihilation — the entire post-1945 great-power order rearranges: alliance architectures built on extended deterrence dissolve or re-price, war plans revive, conventional force balances regain decisive weight, and the treaties, commands, and budgets organized around the impossibility lose their object.
% FOUNDING_PROBLEM: After 1945, and decisively once thermonuclear weapons and survivable second-strike forces emerged in the late 1950s, the great powers faced a problem with no historical precedent: how to remain rivals without repeating the total-war pattern. The arrangement consolidated around accepting mutual vulnerability rather than pursuing damage limitation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: adversary archives agree — Soviet leadership records from the Cuban Missile Crisis show both sides behaving as though general war were unsurvivable, and declassified US net assessments of the same period concede the absence of a victory path. Neutral diplomatic histories and the NPT bargaining record of non-nuclear states attest both the problem's reality and its continued liveness. No party attests the problem is solved except insofar as the condition itself is the solution.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.05 at interval end) because the arrangement confiscates nothing anyone can rationally want: the deleted option-space had negative value under this reading. Suppression is low (0.08) because nothing enforces the condition — no compliance machinery holds it up; physics and force survivability do. Accessibility collapse is high (0.92): once the annihilation outcome is understood, alternatives do not merely look bad, they cease to be reachable — only speculative technologies promise a route around it. Resistance is low but nonzero (0.12): the counterforce, missile-defense, and prompt-strike investment stream is real, funded resistance aimed at restoring a victory path, and it has so far failed. Theater ratio is the one moving quantity: it rises from 0.05 to 0.34 across the interval because the condition's operation recedes into background assumption while symbolic activity (parades, exercise signaling, anniversary declaratory policy) grows — under this reading such signaling is increasingly performance, since the deterring work is done by the outcome itself, not by communicated resolve. The measurement series run on one shared time grid (every tracked metric authored at every examined point, 1945–2025). No suppression_requirement series is authored: the enforcement picture is static across the whole interval (there is no enforcement to build up or decay), so the scalar carries it, per the alignment and static-enforcement rules. The claim (mountain) and the metrics are independent authored facts; where the engine's computed type diverges from the claim — particularly via the false-summit chain — that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural facts. From the civilian-population seat the condition is a pure protective limit: no extraction is felt, no enforcement is visible — a mountain experience. From the leadership seat the same condition carries a mild extraction flavor: command of unusable instruments is experienced as stripped sovereignty, a cost paid in option space rather than money. From the establishment seat the condition is a mission-constituting order: identity-locked administrators who both keep the substrate in place and collect their continuity from it — the seat where false-summit dynamics would live if the maintained-condition omega resolves adversely. From the counterforce-advocate seat the condition reads as a wall: a barrier that rules out their entire program regardless of merit. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: great_power_civilian_populations sit at the full-beneficiary pole (d near 0.0) — subsidized by the condition, trapped into it, bearing only diffuse tax costs. national_political_leaderships derive slightly off the pole (option-stripping is a real positional cost, but the removed option was worthless to exercise). national_security_establishments are the one seat where the automatic derivation would err: reading them as pure beneficiaries from the beneficiaries list would place d near 0.05, but they administer the arrangement, absorb permanent readiness burdens, and carry personal responsibility for catastrophe avoidance — a genuinely dual position. A directionality override sets the institutional seat to d = 0.2 to reflect that administrator burden. The vindicated propositions (mutual_assured_destruction, second_strike_stability) are listed separately and collect no rents — they are not beneficiaries. The excluded seats (non_nuclear_states, counterforce_warfighting_advocates) carry high latent directionality but stand outside the arrangement's operative geometry: they pay for an order they never agreed to, or are barred from a program they exist to run.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — great-power rivalry without civilizational suicide — is live, and the arrangement has no sunset clause and no resolved mandate; mandatrophy is not declared. The classification discipline guards both characteristic errors for this domain. First, the disarmament-advocacy error: reading the condition as a snare (pure extraction with populations as victims) — wrong under this reading, because no victim seat exists; the arrangement extracts nothing anyone rationally valued. Second, the naturalization error: reading a maintained condition as untouched physics — exactly what the false-summit machinery tests, since identifiable agents (the establishments) both maintain the substrate and collect from maintaining it. The rising theater series is the early-warning instrument for a third failure mode: if the condition's function ever fully recedes into ceremony while the apparatus persists, the story drifts toward the piton cell, and the temporal data exist to date that transition rather than discover it retrospectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_maintained_condition,
    'Is the impossibility a genuine structural limit that would hold regardless of anyone''s choices, or a constructed condition that persists only because identifiable agents actively maintain the survivable second-strike forces that make annihilation certain?',
    'Counterfactual and maintenance analysis: trace whether the condition survives hypothetical cessation of arsenal maintenance, modernization, and readiness spending, and compare against historical episodes where survivability was genuinely in doubt.',
    'If the condition is load-bearing on active maintenance, the false-summit signature applies and the classification migrates from mountain toward tangled_rope — populations coordinated into safety while the establishments collect budgets through the same structure. If it holds without maintenance, the mountain claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_maintained_condition, conceptual, 'Whether the impossibility is physics-like or sustained by interested maintainers.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (structural_contraction_reading) of the nuclear_impossibility_kernel; would instantiating the sibling readings — rational_dropout_reading or credibility_paradox_reading — produce different epsilon values, beneficiary/victim structures, and classifications?',
    'Generate the sibling stories as separate constraints and compare computed per-seat classifications across the kernel family.',
    'The dropout reading would raise epsilon (the arrangement as a choice-tax on leadership autonomy, adding a victim seat) and the credibility reading would raise suppression (arrangement dependent on sustained threat-signaling). Classification of THIS file is valid only for THIS reading; cross-reading averaging is prohibited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: epsilon is a property of this reading, not of the topic.').

omega_variable(
    second_strike_durability,
    'Will second-strike survivability endure against improving counterforce accuracy, missile defense, hypersonic delivery, and AI-enabled targeting — the funded attempts to make damage limitation feasible?',
    'Technical assessment of penetration aids, basing-mode diversity, and defense breakthrough thresholds against deployed force structures over successive modernization cycles.',
    'If survivability fails, the impossibility dissolves, the constraint converts from a standing structural limit into a historical episode, and every seat''s classification resets around restored war-fighting option space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_strike_durability, empirical, 'Whether the physical substrate of the impossibility is durable.').

omega_variable(
    proxy_war_substitution_status,
    'Does proxy warfare confirm the constraint — conflict displaced, not suppressed — or reveal leakage, with violence continuing at lower levels in ways the constraint''s coordination function does not cover?',
    'Comparative analysis of proxy-conflict rates and intensities before and after nuclearization against a matched counterfactual baseline of non-nuclear rival pairs.',
    'If substitution, the constraint is intact with scope limited to direct great-power war. If leakage, the coordination function is narrower than claimed and the unmanaged violence belongs to a separate constraint story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_substitution_status, empirical, 'Whether displaced conflict validates or erodes the impossibility claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nim_structural_contraction_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1945, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t1955, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1955, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.06).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1962, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t1972, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1972, 0.14).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1972, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t1985, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1985, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t1995, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t1995, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t2010, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t2010, observed).
narrative_ontology:measurement(nim_structural_contraction_tr_t2025, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement_basis(nim_structural_contraction_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nim_structural_contraction_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1945, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t1955, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1955, 0.04).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1955, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.05).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1962, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t1972, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1972, 0.06).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1972, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t1985, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1985, 0.07).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1985, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t1995, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1995, 0.04).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t1995, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t2010, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t2010, observed).
narrative_ontology:measurement(nim_structural_contraction_be_t2025, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2025, 0.05).
narrative_ontology:measurement_basis(nim_structural_contraction_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial claim 'nuclear weapons made great-power war impossible' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints forming the nuclear_impossibility_kernel family. This file authors the structural_contraction_reading: an impossibility account with epsilon near zero, beneficiaries only, and no enforcement dependence. The rational_dropout_reading authors the same history as a choice-architecture constraint (victory possible, declined on cost — higher epsilon, a leadership-autonomy victim seat). The credibility_paradox_reading authors it as a speech-act instability (incredible threats — higher suppression, enforcement-dependent persistence). Upstream/downstream structure: the structural claim is the strongest; if it holds, it drains the dropout calculus of its object and the credibility debate of its stakes, which is why this story links to both siblings and why the siblings, if generated, should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__structural_contraction_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
