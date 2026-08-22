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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility (Structural Contraction of the Victory Set)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates one reading of the nuclear_impossibility_kernel:
 *   the structural contraction reading. On this reading, the arrival of
 *   mutual second-strike nuclear capability did not merely make war costly or
 *   make deterrence threats incredible — it removed direct great-power war
 *   from the reachable outcome set entirely, as a matter of
 *   physical/strategic structure rather than rational calculation or
 *   credibility signaling. This is a distinct claim from the
 *   rational_dropout_reading (which holds victory remains structurally
 *   possible but irrational) and the credibility_paradox_reading (which holds
 *   the threat structure is internally paradoxical). Under this reading,
 *   proxy wars, arms racing, and covert competition are not degraded
 *   continuations of great-power war but substitutions occupying the space
 *   the contraction vacated — a structurally different claim than 'war
 *   becomes costly' or 'the threat becomes incredible.' The claimed type is
 *   mountain because the underlying physical fact (guaranteed mutual
 *   annihilation given current arsenals) is treated by this reading as a
 *   structural feature of the world once the weapons exist, not a policy
 *   choice; but beneficiaries are declared (nuclear weapon states, the
 *   deterrence establishment) because identifiable institutional actors gain
 *   status, budget, and geopolitical leverage from administering and
 *   interpreting the impossibility — this triggers FSM evaluation, which is
 *   intentional: the omegas below document the natural-law-vs-constructed
 *   ambiguity this reading must carry.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: institutional beneficiaries who administer and derive leverage from the impossibility
 *   - nuclear_deterrence_establishment: agenda-setting institutional layer that operationalizes and maintains doctrine
 *   - populations_of_nuclear_states: powerless payers bearing tail risk with no doctrinal voice
 *   - conventional_and_proxy_conflict_populations: powerless payers absorbing substituted violence
 *   - non_nuclear_states: excluded from the dyad's calculus entirely
 *   - arms_control_analysts: analytical observers assessing whether the contraction still holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.58).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.71).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility (Structural Contraction of the Victory Set)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '8b7e5e23-8730-4591-9604-4a15fa15ff26').
narrative_ontology:cs_kernel_codification('8b7e5e23-8730-4591-9604-4a15fa15ff26', distributed).
narrative_ontology:cs_authority_grounding('8b7e5e23-8730-4591-9604-4a15fa15ff26', distributed).
narrative_ontology:cs_reading_relation('8b7e5e23-8730-4591-9604-4a15fa15ff26', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b7e5e23-8730-4591-9604-4a15fa15ff26', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('8b7e5e23-8730-4591-9604-4a15fa15ff26', foundational, reachable_set_contraction_is_categorical).
narrative_ontology:cs_axiom_status(reachable_set_contraction_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('8b7e5e23-8730-4591-9604-4a15fa15ff26', reachable_set_contraction_is_categorical, empirically_contingent).
narrative_ontology:cs_axiom('8b7e5e23-8730-4591-9604-4a15fa15ff26', secondary, proxy_conflict_is_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_conflict_is_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('8b7e5e23-8730-4591-9604-4a15fa15ff26', proxy_conflict_is_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('8b7e5e23-8730-4591-9604-4a15fa15ff26', pre_nuclear_great_power_war_reachability).
narrative_ontology:cs_drift_state('8b7e5e23-8730-4591-9604-4a15fa15ff26', post_missile_defense_and_limited_nuclear_options_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('8b7e5e23-8730-4591-9604-4a15fa15ff26', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_deterrence_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, populations_of_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, conventional_and_proxy_conflict_populations).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, great_power_war_avoidance_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutually_assured_destruction_stability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold arsenals whose second-strike capacity forecloses direct great-power war between nuclear peers as a reachable outcome. They administer deterrence doctrine, budget it as national security necessity, and derive geopolitical status and alliance leverage from possessing the capability, independent of ever using it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Strategists, military planners, and policy institutions that maintain, model, and justify deterrence posture. They administer the doctrine that operationalizes the physical impossibility into targeting plans, alliance commitments, and budget lines; their institutional relevance depends on the impossibility being treated as a live strategic parameter requiring continuous management.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_deterrence_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_deterrence_establishment, beneficiary).

% Live under the umbrella of a constraint that removes direct great-power war from the table but does so by holding open, permanently, the possibility of civilization-ending failure. They bear the tail risk of accident, miscalculation, or escalation without having any say in doctrine, and cannot individually exit the arrangement their states have entered into on their behalf.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, populations_of_nuclear_states, payer,
    powerless, civilizational, trapped, global).

% Bear the substituted conflict: because direct war between nuclear powers is physically foreclosed, contestation is displaced into proxy wars, conventional conflicts fought through client states, and covert competition. These populations absorb violence that would, absent the impossibility, potentially have been part of a directly fought (and possibly shorter, though this is contested) great-power war. Proxy war is a substitution, not a continuation, of the foreclosed path.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, conventional_and_proxy_conflict_populations, payer,
    powerless, immediate, trapped, regional).

% Operate inside a global order shaped by an impossibility they did not create and cannot alter. They have no seat in the nuclear dyad's structural mathematics; their security depends on extended deterrence guarantees or on remaining outside the calculus entirely, a position assigned to them rather than chosen.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, excluded,
    moderate, generational, constrained, national).

% Study the structural mathematics of mutual assured destruction, model escalation pathways, and assess whether the reachable-set contraction actually holds under new delivery systems, missile defense, or doctrine shifts. They neither benefit from nor pay the constraint's costs directly, but their findings inform which reading of the kernel policymakers act on.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, properly speaking — this reading holds that the arrangement is not a coordination solution to a game but a physical fact about the reachable outcome set: given mutual second-strike capability, direct great-power war ceases to be an option within the space of possible actions, regardless of what any party wants or threatens.
% TRANSFER_FUNCTION: The arrangement does not transfer resources between parties in the way a coordination mechanism would; it redirects contestation. Conflict energy that would otherwise flow into direct great-power war is displaced into proxy conflicts, arms racing, and covert competition, with the costs of that displacement falling on populations in conventional and proxy theaters rather than on the nuclear-armed states themselves.
% ABSENT_VOICES: Populations in proxy-conflict regions and inside nuclear states have no vote in doctrine that treats their territories as substitution channels or their societies as acceptable tail-risk bearers; they are structurally outside the strategic calculus that produced the arrangement.
% DISAPPEARANCE_RATIONALE: If nuclear weapons and the physical contraction they impose vanished overnight, this reading holds that direct great-power war would re-enter the reachable outcome set — the world would rearrange sharply, with conventional deterrence and historical patterns of great-power conflict resuming. The credibility-paradox and rational-dropout readings would predict different rearrangements (renewed brinksmanship vs. a return to cost-benefit calculation), which is exactly why the kernel splits into distinct constraints rather than one.
% FOUNDING_PROBLEM: The physical existence of weapons capable of guaranteed mutual annihilation was not built to solve a problem in the ordinary sense; it emerged from the technological fact of fission/fusion weapons reaching a destructive scale where no plausible war-fighting doctrine survives contact with retaliation. The 'problem' it is read as solving retrospectively — preventing great-power war — is a byproduct of the physical constraint, not its design purpose.
% FOUNDING_PROBLEM_CORROBORATION: Independent nuclear risk researchers, arms control verification bodies, and historians of the Cold War (outside the states that benefit from possessing arsenals) corroborate that the physical basis of mutual assured destruction has not been technically superseded — no missile defense or counterforce doctrine has restored a credible war-winning path between peer nuclear states. This corroboration is contested by strategists within the deterrence establishment itself who argue doctrine evolution (e.g., limited nuclear options, missile defense) may be eroding the contraction — hence founding_problem_status is authored as live rather than settled.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Extractiveness is authored moderate (0.58 by 2025) because the primary claim is a physical-structural one, not an extraction mechanism — but non-zero and rising because the deterrence establishment increasingly captures budget, status, and policy influence by administering the impossibility, and proxy-conflict populations bear the substituted costs. Suppression is authored high (0.71) because the arrangement is actively maintained through continuous force posture, alliance commitments, and doctrine enforcement — the contraction does not persist passively once acknowledged; states invest heavily in ensuring the second-strike capability that makes the contraction hold. Accessibility collapse is authored very high (0.88), consistent with a mountain claim: once the physical logic of guaranteed mutual annihilation is understood, no alternative path to great-power military victory between nuclear peers presents itself as reachable. Resistance is authored low-moderate (0.35): there is little active resistance to the physical fact itself (no one seriously proposes that mutual annihilation isn't mutual annihilation), though there is real contestation over whether doctrine innovations (missile defense, limited nuclear options) are eroding the contraction — this contestation is captured in the drift_state and omegas rather than in the resistance score, which tracks resistance to the base claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and the deterrence establishment sit near the beneficiary end: they administer the impossibility, derive strategic status and budgetary continuity from it, and have arbitrage-grade exit (they can adjust doctrine, modernize arsenals, or shift alliance structures without losing their institutional position). Populations of nuclear states and conventional/proxy-conflict populations sit near the target end: they are trapped by the arrangement (cannot individually exit a structural feature of great-power relations) and bear its risks and substituted violence without corresponding institutional benefit. Non-nuclear states occupy an excluded middle: structurally outside the dyad's calculus, dependent on extended deterrence, with only constrained exit options (alliance realignment is possible but costly and slow).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing deliberately treats the arrangement's origin as physical/technological rather than designed to solve a stated problem — this guards against reading the deterrence establishment's continuous doctrine-maintenance activity as pure institutional self-perpetuation (mandatrophy) when the underlying physical fact (mutual annihilation given current arsenal configurations) may still be live. The founding_problem_status is authored 'live' rather than 'dead,' reflecting that independent corroboration (arms control researchers outside the beneficiary set) has not established that missile defense or counterforce doctrine has actually restored a war-winning path. If that corroboration shifted — if independent analysts concluded the contraction no longer holds due to technological change — the mountain claim would need re-examination, and the deterrence establishment's continued doctrine-maintenance activity would look more like theater over a resolved physical fact than management of a live one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_law_vs_administered_doctrine,
    'Is the reachable-set contraction a genuine physical/structural fact independent of any party''s maintenance of it, or is it partly a constructed doctrinal artifact that the nuclear weapon states and deterrence establishment have institutional incentive to represent as immutable?',
    'Assess whether the contraction would hold under counterfactual doctrine changes (e.g., unilateral disarmament by one dyad member, or breakthrough missile defense) that the deterrence establishment has institutional incentive to resist studying. If the contraction is robust to doctrine changes the establishment doesn''t control, it leans natural-law; if it depends heavily on continuous, expensive doctrinal maintenance, it leans constructed.',
    'If the contraction is confirmed as physical/structural regardless of institutional maintenance, the mountain classification with FSM-triggered scrutiny is the correct read — genuine natural constraint, incidentally beneficial to some. If the contraction depends substantially on continuous costly maintenance and would decay without it, the constraint drifts toward false-summit tangled-rope: an institutionally-administered arrangement dressed as physical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_law_vs_administered_doctrine, conceptual, 'Whether the contraction is genuine physical law or partly a maintained doctrinal artifact benefiting nuclear states.').

omega_variable(
    kernel_disaggregation_location,
    'Where exactly does this reading''s claim (reachable-set contraction) diverge empirically from the sibling readings (rational dropout: victory possible but irrational; credibility paradox: threat is incredible)? Is the divergence testable, or purely a difference in framing over the same underlying strategic facts?',
    'Examine historical crisis decision-making (Cuban Missile Crisis, 1983 war scare, India-Pakistan crises) for evidence of whether decision-makers treated direct war as literally unreachable (this reading), as reachable-but-irrational (rational dropout), or as blocked by threat incredibility specifically (credibility paradox). Documented internal deliberations distinguishing ''we cannot do this'' from ''we should not do this'' from ''they won''t believe we''d do this'' would locate the true structural claim.',
    'If historical evidence shows decision-makers treating war as calculably-possible-but-not-worth-it, the rational_dropout_reading is better supported and this reading''s mountain/impossibility framing overstates the case. If evidence shows war was treated as literally off the table structurally, this reading is vindicated as the operative one historically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disaggregation_location, empirical, 'Whether the three kernel readings are empirically distinguishable in historical crisis behavior or are three framings of one underdetermined fact.').

omega_variable(
    proxy_substitution_vs_continuation,
    'Are proxy wars and covert competition genuinely substitutions occupying the space vacated by foreclosed direct war (this reading''s claim), or are they degraded/partial continuations of the same underlying great-power conflict, merely displaced in venue?',
    'Compare the scale, frequency, and strategic objectives of proxy conflicts in the nuclear era against modeled counterfactual great-power war intensity; assess whether proxy conflict intensity correlates with periods of heightened direct-war-reachability perception (e.g., doctrine shifts, arms race intensification).',
    'If proxy wars are substitutions, this reading''s ''exits the reachable set entirely'' claim is structurally supported — the M-set genuinely contracted rather than merely shifted form. If proxy wars are continuations, the sharp reachable-set-contraction claim overstates the discontinuity and the rational_dropout_reading''s smoother cost-benefit degradation may be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_substitution_vs_continuation, conceptual, 'Whether proxy conflict is a true substitution for foreclosed direct war or a continuation in different form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.28).
narrative_ontology:measurement(nucl_tr_t1979, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1979, 0.34).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(nucl_be_t1979, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1979, 0.55).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1991, 0.48).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(nucl_su_t1979, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1979, 0.62).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(nucl_su_t2008, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the nuclear impossibility/deterrence paradox' per the ε-invariance principle. structural_contraction_reading claims the reachable outcome set for great-power war categorically contracted to exclude war (an M-set/possibility-space claim). rational_dropout_reading claims war remains possible but is irrational given costs (a cost-benefit claim within an unchanged possibility space). credibility_paradox_reading claims the deterrence threat structure is internally self-undermining (a signaling/credibility claim). The three share subject matter but differ in structural claim, victim/beneficiary framing nuance, and what would falsify each. All three link to each other via affects_constraints; none forecloses the others outright except where axioms directly contradict (see cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
