% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Nuclear-Age Contraction of the Total-War Feasible Set
 *   domain: international_relations/strategic_studies/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This story instantiates the contraction_reading of the
 *   total_war_reachability_boundary kernel: the claim that the advent of
 *   thermonuclear weapons and assured mutual destruction capability did not
 *   merely lower the probability of great-power total war (the
 *   dropping_reading) or produce a reversible technological state (the
 *   contingent_reachability_reading), but removed winnable total war from the
 *   feasible strategic set as a matter of physical/logical fact. Under this
 *   reading, no rational actor's total-war 'victory' condition survives
 *   contact with second-strike destructive capacity sufficient to end
 *   organized society for attacker and defender alike — the strategic space
 *   itself contracted. There is no beneficiary structure: unlike a Rope or a
 *   Snare, no party collects rents from this boundary's existence, because
 *   the boundary's defining feature is that it forecloses a payoff structure
 *   for everyone, including nuclear-armed states themselves (who lose the
 *   'winnable war' option they might otherwise have valued). The victim class
 *   is not a subset extracted from — it is universal: every human population
 *   lives under the residual extinction-tail risk inherent to the existence
 *   of the arsenals that produced the boundary, a risk borne collectively and
 *   asymmetrically by no one's design.
 *
 * KEY AGENTS:
 *   - global_human_population: universal residual-risk bearer (organized/trapped) — bears the extinction-tail risk inherent in a boundary from which no party benefits
 *   - nuclear_weapon_states: administer the arsenals that constitute the boundary's physical substrate but do not benefit from the boundary itself — they lose the option of a winnable total war exactly as everyone else does
 *   - non_nuclear_states: live inside the boundary's effects (extended deterrence, alliance structures) without controlling the substrate
 *   - strategic_studies_analysts: analytical observers who characterize the boundary's structure but do not administer it
 *   - future_generations: temporally displaced victims of irreversible existential risk accumulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.03).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Nuclear-Age Contraction of the Total-War Feasible Set").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence_theory").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '8cfb247f-753c-480c-9933-76a7f7770ed8').
narrative_ontology:cs_kernel_codification('8cfb247f-753c-480c-9933-76a7f7770ed8', distributed).
narrative_ontology:cs_authority_grounding('8cfb247f-753c-480c-9933-76a7f7770ed8', diffuse_epistemic).
narrative_ontology:cs_reading_relation('8cfb247f-753c-480c-9933-76a7f7770ed8', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('8cfb247f-753c-480c-9933-76a7f7770ed8', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('8cfb247f-753c-480c-9933-76a7f7770ed8', foundational, assured_destruction_is_physical_not_institutional).
narrative_ontology:cs_axiom_status(assured_destruction_is_physical_not_institutional, holdable).
narrative_ontology:cs_axiom_grounding('8cfb247f-753c-480c-9933-76a7f7770ed8', assured_destruction_is_physical_not_institutional, empirically_contingent).
narrative_ontology:cs_axiom('8cfb247f-753c-480c-9933-76a7f7770ed8', secondary, feasible_set_removal_requires_no_active_maintenance).
narrative_ontology:cs_axiom_status(feasible_set_removal_requires_no_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('8cfb247f-753c-480c-9933-76a7f7770ed8', feasible_set_removal_requires_no_active_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('8cfb247f-753c-480c-9933-76a7f7770ed8', pre_nuclear_great_power_total_war_option).
narrative_ontology:cs_drift_state('8cfb247f-753c-480c-9933-76a7f7770ed8', contemporary_multipolar_nuclear_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8cfb247f-753c-480c-9933-76a7f7770ed8', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_human_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lives under the residual extinction-tail risk that is the byproduct of the arsenals whose existence produced the total-war foreclosure. Cannot individually or collectively opt out of this risk exposure; there is no exit from a planet holding thermonuclear arsenals. Receives no compensating benefit specific to bearing this risk — the risk is simply diffusely borne as a condition of the boundary's existence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_human_population, payer,
    powerless, civilizational, trapped, universal).

% Administer and maintain the arsenals whose existence constitutes the physical substrate of the boundary. They set doctrine and strategic posture but do not benefit from the boundary itself in the rent-collection sense — the boundary removes from THEM, too, the option of a winnable total war. Their exit from the arrangement (unilateral disarmament) would not remove the boundary for other possessors and would expose them asymmetrically, so in practice they are as trapped by the logic of mutual destruction as any other party, despite holding the administering power.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, trapped, global).

% Live within alliance structures (extended deterrence, nuclear umbrellas) shaped by the boundary without possessing or controlling the underlying arsenals. They benefit incidentally from great-power total war being off the table, but also bear the same universal residual extinction-tail risk as everyone else, and have essentially no ability to alter the boundary's terms.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_states, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contraction_reading, non_nuclear_states, payer).

% Inherit whatever accumulated existential risk the current arrangement carries forward — waste, close calls, arsenal maintenance failures, proliferation dynamics — without any voice in how the boundary is currently managed. They cannot participate in present strategic decision-making at all.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Study and characterize the boundary's structure — debating precisely the contraction/dropping/contingent-reachability distinction this kernel formalizes — without administering the arsenals or bearing risk differently from the general population they belong to.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no coordination problem this reading identifies as solved by an institution — the contraction_reading's core claim is that the removal of winnable total war from the feasible set is a physical/logical fact about destructive capacity and delivery assurance, not a coordination equilibrium that any institution maintains. To the extent any 'coordination' exists, it is the passive, non-institutional fact that no rational planner's total-war strategy survives contact with assured second-strike retaliation.
% TRANSFER_FUNCTION: Under this reading nothing is transferred between parties in the rent-extraction sense. What the boundary imposes is a universally shared residual risk (extinction-tail probability from accident, miscalculation, or arsenal degradation) borne by all human populations without a corresponding flow of benefit to any specific party.
% ABSENT_VOICES: Future generations are structurally absent from any decision-making about arsenal levels, doctrine, or risk tolerance, yet they inherit the accumulated tail risk in full. Non-signatory populations and stateless persons are also absent from the diplomatic architecture (NPT, START-series treaties) that surrounds but does not constitute this boundary.
% DISAPPEARANCE_RATIONALE: If the physical boundary itself vanished overnight — meaning nuclear weapons somehow ceased to produce assured mutual destruction capability — the strategic feasible set would immediately re-expand to include winnable total war as a live option for great powers, exactly as it existed before 1945. Alliance structures built on extended deterrence, the entire architecture of arms control diplomacy, and great-power strategic planning would all have to reorganize around the reopened option. This is a world-rearranging counterfactual precisely because, under this reading, the boundary's disappearance is not merely institutional decay but the removal of a fact that currently structures the entire strategic environment.
% FOUNDING_PROBLEM: Before 1945, great-power total war was a live, rationally pursuable strategic option — a state could, in principle, calculate a path to victory through total war and act on it. The founding problem this boundary closes is the existence of that calculable win-condition among peer or near-peer powers.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies analysts across competing schools (deterrence theorists, arms control scholars, and proliferation researchers) broadly attest that the specific problem of a calculable great-power total-war win-condition is dead under current arsenal and delivery configurations — this is corroborated by military planning doctrine in nuclear states themselves, which does not model total war against another nuclear power as an achievable objective. However, the BROADER problem (existential risk from the arsenals that closed the narrower problem) is attested by arms-control NGOs, historians of near-miss incidents, and some retired strategic-forces officers as very much live — the boundary closed one problem by creating a different one. No party benefiting from continued arsenal maintenance has an interest in this reading being correct in its strongest form, so the corroboration here comes substantially from outside interested parties: independent risk researchers and historians of nuclear near-misses.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because under this reading no party extracts rent from the boundary's existence — it is not a toll gate, it is a foreclosure. Suppression is low (0.05) because the boundary does not require active coercive enforcement to persist; it persists because the physics of thermonuclear yield and assured delivery do not go away when unenforced (contrast with the dropping_reading's Rope, which WOULD require active coordination maintenance — arms control treaties, crisis hotlines, doctrine signaling — to hold). Accessibility collapse is very high (0.92): once a state possesses assured second-strike capability, the alternative of 'total war as a rational strategic choice' is not merely discouraged but effectively removed from the analytically serious option set for any planner. Resistance is low (0.08): there is essentially no organized actor arguing FOR the revival of winnable total war as a live strategic option — the closest analogue is theoretical missile-defense-shield advocacy, captured instead in the second_strike_survivability_dependency omega. The declining theater_ratio and extractiveness trend over the interval reflects the boundary becoming MORE settled/naturalized as a strategic fact across the nuclear age (Cold War theatricality around civil defense and 'winnable nuclear war' doctrine debates receding as MAD became analytically dominant), not less real.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading there should be minimal seat divergence in TYPE (all seats should compute close to Mountain, since no seat captures rent from the boundary), but there IS legitimate divergence in SALIENCE: nuclear weapon states experience the boundary as a strategic planning constraint (their war colleges do not model winnable total war as an option), while ordinary populations experience it as an ambient, mostly invisible background risk. The engine should compute both as structurally consistent with Mountain given the absence of any declared beneficiary — this reading predicts LOW seat divergence, which is itself a distinguishing feature relative to the dropping_reading (which would show a Rope with genuine coordination costs falling asymmetrically on crisis-management institutions) and the contingent_reachability_reading (which would show Piton-like divergence between states investing in missile defense R&D and those relying on legacy deterrence).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared under this reading — this is the defining structural delta from any Rope or Tangled Rope reading of nuclear deterrence. Nuclear weapon states are NOT coded as beneficiaries here, because under the contraction_reading their arsenals do not purchase them a winnable-war option; they purchase mutual foreclosure, which is qualitatively different from extraction. The victim class (global_human_population) is universal and undifferentiated by directionality in the ordinary beneficiary/victim sense — everyone bears the same structural residual risk, including the possessor states' own populations. This is why the constraint is authored as a Mountain rather than a Snare or Tangled Rope: there is no asymmetric transfer to identify, only a universally shared foreclosure with a universally shared tail risk.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable in the ordinary sense here because there is no mandate that has outlived its function in the extraction sense — the founding 'problem' (total war as a live strategic option among great powers) has not persisted as a live problem for the beneficiaries of a coordination arrangement; rather, this reading claims the problem itself was retired from the feasible set by a physical fact, not by an institution that could atrophy. The R5 genealogy question here is unusual: the 'founding problem' predates the constraint (great-power total war was live before 1945) and the constraint's operation is precisely the CLOSING of that problem, not its perpetuation. founding_problem_status is authored as 'live' in a restricted sense — the underlying species-level extinction-tail risk persists — while the specific problem the constraint closes (winnable total war as a rational strategic choice) is dead. This dual status is the R5 signal that distinguishes contraction from dropping: a Rope reading would show the founding problem (great-power war) as still live and merely managed, requiring the coordination apparatus (arms control, hotlines, deterrence signaling) to keep it suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_with_universal_victim_coherence,
    'Can a constraint be a genuine Mountain (natural-law impossibility) while simultaneously naming a victim class (the species-wide extinction risk borne by everyone), or does naming a victim class always imply an actor benefits and thus a constructed constraint?',
    'Structural test: does removing all human institutions (treaties, deterrence doctrine, arms control) eliminate the boundary, or does the boundary persist as a physical fact about weapons yield and delivery even absent any institution? If the latter, the victim class exists without a corresponding beneficiary class, which is consistent with Mountain classification with a universal-risk-bearer rather than a rent-collecting victim.',
    'If the boundary is confirmed physical (not institutional), the Mountain claim holds despite the victim declaration — this is the intended structural delta for this reading. If any actor is found to derive strategic advantage FROM the contraction itself (e.g., an incumbent nuclear power benefiting from the fact that rivals also cannot pursue total war), the constraint reclassifies toward false-summit territory and FSM logic should be reconsidered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_with_universal_victim_coherence, conceptual, 'Whether a Mountain classification is coherent with a declared universal victim class in the absence of any declared beneficiary.').

omega_variable(
    kernel_reading_selection_contraction_vs_dropping_vs_contingent,
    'This constraint instantiates the contraction_reading of the total_war_reachability_boundary kernel — that nuclear weapons removed winnable total war from the feasible set entirely, making it a Mountain. Two sibling readings exist: the dropping_reading (total war became less probable but remains reachable — a Rope, i.e. a maintained coordination equilibrium) and the contingent_reachability_reading (current unreachability is contingent on present technology and could reverse — a Piton, i.e. an atrophied-but-potentially-revivable capability). Which reading correctly characterizes the structural relationship between nuclear weapons and the total-war feasible set?',
    'The three readings differ on whether the change is physical/logical (contraction — permanent removal from feasible set), probabilistic/institutional (dropping — deterrence equilibrium requiring active maintenance), or technological/contingent (contingent_reachability — reversible if delivery systems, defenses, or doctrine change, e.g. reliable missile defense or fully survivable second-strike erosion). No single empirical test resolves this because it depends on whether one treats MAD as a physical fact about destructive capacity (favors contraction) or as a strategic equilibrium requiring continuous signaling and credibility (favors dropping) or as a capability state that could be engineered away (favors contingent_reachability).',
    'If the dropping_reading is correct, deterrence requires active enforcement and coordination costs, and total war should be modeled as a Rope with genuine (if fragile) coordination benefit rather than a zero-degrees-of-freedom Mountain. If the contingent_reachability_reading is correct, the current state is a Piton — an atrophied pathway maintained more by treaty inertia and doctrine than physical impossibility, which could reverse under sufficiently effective missile defense or novel delivery denial. This reading (contraction) treats the removal as a structural/physical fact independent of any party''s maintenance, which is the most totalizing of the three claims and should be tested against evidence of active enforcement costs (arms control regimes, extended deterrence commitments) that would suggest the boundary requires upkeep rather than being self-sustaining.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_contraction_vs_dropping_vs_contingent, conceptual, 'Which of three sibling readings of the total_war_reachability_boundary kernel is structurally correct: contraction (Mountain), dropping (Rope), or contingent_reachability (Piton).').

omega_variable(
    second_strike_survivability_dependency,
    'Is the Mountain-grade impossibility this reading claims actually dependent on a specific technical condition (assured second-strike survivability) that could itself degrade, meaning the ''natural law'' character is really contingent on maintained technical parity?',
    'Track whether any nuclear-armed state achieves a credible disarming first-strike capability or a sufficiently effective strategic defense shield; either development would falsify the naturality claim by showing the boundary was a capability artifact, not a physical law.',
    'If second-strike survivability erodes for any major power, the contraction_reading''s Mountain claim becomes empirically falsified for that dyad, and the contingent_reachability_reading gains support. This omega documents the load-bearing assumption underneath the naturality claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_strike_survivability_dependency, empirical, 'The Mountain claim rests on assured mutual destruction remaining technically assured; this is a testable, not axiomatic, condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tota_tr_t13, total_war_reachability_boundary__contraction_reading, theater_ratio, 13, 0.18).
narrative_ontology:measurement(tota_tr_t26, total_war_reachability_boundary__contraction_reading, theater_ratio, 26, 0.15).
narrative_ontology:measurement(tota_tr_t39, total_war_reachability_boundary__contraction_reading, theater_ratio, 39, 0.13).
narrative_ontology:measurement(tota_tr_t52, total_war_reachability_boundary__contraction_reading, theater_ratio, 52, 0.11).
narrative_ontology:measurement(tota_tr_t65, total_war_reachability_boundary__contraction_reading, theater_ratio, 65, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tota_be_t13, total_war_reachability_boundary__contraction_reading, base_extractiveness, 13, 0.06).
narrative_ontology:measurement(tota_be_t26, total_war_reachability_boundary__contraction_reading, base_extractiveness, 26, 0.05).
narrative_ontology:measurement(tota_be_t39, total_war_reachability_boundary__contraction_reading, base_extractiveness, 39, 0.04).
narrative_ontology:measurement(tota_be_t52, total_war_reachability_boundary__contraction_reading, base_extractiveness, 52, 0.035).
narrative_ontology:measurement(tota_be_t65, total_war_reachability_boundary__contraction_reading, base_extractiveness, 65, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contraction_reading, 0.02).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_reachability_boundary kernel, decomposed per the ε-invariance principle because the three readings assign structurally different ε, beneficiary structures, and classifications to the same natural-language phenomenon (the effect of nuclear weapons on the feasibility of total war). contraction_reading (this file) claims Mountain: ε near zero, no beneficiaries, universal victim class, foreclosure is physical/logical and requires no active maintenance. dropping_reading claims Rope: total war remains reachable but less probable, requiring active institutional coordination (arms control, crisis management, deterrence signaling) to sustain the lowered probability — ε would be authored higher there to reflect real coordination costs and a genuine (if fragile) coordination benefit captured by participating states. contingent_reachability_reading claims Piton: the current unreachability is a technology-contingent, potentially atrophying state that could reverse with missile defense advances or second-strike survivability erosion — that story would carry a meaningfully different theater_ratio trajectory (rising, as doctrine maintenance becomes performative relative to eroding technical assurance) and a lower accessibility_collapse. All three stories share the same underlying empirical substrate (arsenal sizes, delivery systems, deterrence doctrine history) but diverge irreducibly on what kind of fact the boundary is — physical, institutional, or technological-contingent — which is exactly the kind of disagreement the framework resolves by decomposition rather than by forcing one ε to cover all three claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
