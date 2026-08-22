% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Strategy: Nuclear War Remains Winnable Through Counterforce Targeting
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story instantiates the 'countervailing_thinkable' reading of the
 *   post-1945 war-winnability kernel: the claim that nuclear weapons raise
 *   the cost of major war without removing victory from reachable strategic
 *   space, and that counterforce targeting (as formalized in PD-59, the
 *   Countervailing Strategy, and its doctrinal descendants) keeps limited
 *   nuclear victory a coherent planning object rather than a rhetorical
 *   fiction or a logical impossibility. This is emphatically NOT the
 *   'deterrence_unthinkable' reading (which holds that assured mutual
 *   destruction makes victory incoherent) nor the 'rhetorical_contraction'
 *   reading (which holds that winnability persisted operationally while
 *   becoming publicly unsayable). Those are separate constraints, filed
 *   separately, linked here only through network edges and
 *   cs_structure.reading_relations. The ε authored here (0.68) reflects the
 *   extraction this reading's own lights attribute to the countervailing
 *   arrangement: it treats the ongoing diversion of doctrinal legitimacy,
 *   procurement authority, and crisis-stability risk toward
 *   mission-continuity actors as real and rising, while war-winnability
 *   itself is asserted (by this reading) to remain a live strategic fact, not
 *   an illusion.
 *
 * KEY AGENTS:
 *   - strategic_targeting_planners: institutional agenda-setters who author and maintain counterforce doctrine
 *   - military_industrial_complex: primary beneficiary via mission continuity and procurement justification
 *   - arms_control_regimes: primary victim, whose negotiating leverage is undermined by winnable-war planning
 *   - civilian_populations_in_targeted_regions: powerless payer bearing the physical risk the doctrine treats as bounded
 *   - deterrence_theorists_unthinkable_camp: excluded sibling-reading advocates structurally routed around by the planning apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.6).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Strategy: Nuclear War Remains Winnable Through Counterforce Targeting").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '234b08ec-c539-4742-b14e-7545aef8a4c9').
narrative_ontology:cs_kernel_codification('234b08ec-c539-4742-b14e-7545aef8a4c9', distributed).
narrative_ontology:cs_authority_grounding('234b08ec-c539-4742-b14e-7545aef8a4c9', practice).
narrative_ontology:cs_interpretation_layer_present('234b08ec-c539-4742-b14e-7545aef8a4c9').
narrative_ontology:cs_reading_relation('234b08ec-c539-4742-b14e-7545aef8a4c9', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('234b08ec-c539-4742-b14e-7545aef8a4c9', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('234b08ec-c539-4742-b14e-7545aef8a4c9', foundational, limited_nuclear_exchange_is_escalation_controllable).
narrative_ontology:cs_axiom_status(limited_nuclear_exchange_is_escalation_controllable, holdable).
narrative_ontology:cs_axiom_grounding('234b08ec-c539-4742-b14e-7545aef8a4c9', limited_nuclear_exchange_is_escalation_controllable, empirically_contingent).
narrative_ontology:cs_axiom('234b08ec-c539-4742-b14e-7545aef8a4c9', secondary, extended_deterrence_requires_credible_intermediate_options).
narrative_ontology:cs_axiom_status(extended_deterrence_requires_credible_intermediate_options, holdable).
narrative_ontology:cs_axiom_grounding('234b08ec-c539-4742-b14e-7545aef8a4c9', extended_deterrence_requires_credible_intermediate_options, instrumental).
narrative_ontology:cs_reference_frame('234b08ec-c539-4742-b14e-7545aef8a4c9', flexible_response_credibility_doctrine).
narrative_ontology:cs_drift_state('234b08ec-c539-4742-b14e-7545aef8a4c9', post_cold_war_arms_reduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('234b08ec-c539-4742-b14e-7545aef8a4c9', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_targeting_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, counterforce_weapons_contractors).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, national_security_establishment).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, flexible_response_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, escalation_dominance_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain counterforce targeting doctrine (e.g. PD-59, NUWEP successors) that treats limited nuclear exchange as a controllable, war-terminable event. They author the target lists, damage-expectancy models, and escalation-control protocols that keep 'winnable war' a live planning category. Their institutional relevance and budget authority depend on the premise that victory scenarios remain analytically tractable; they face no binding external check on the doctrine's continuation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_targeting_planners, agenda_setter,
    institutional, generational, arbitrage, global).

% Manufactures and sustains the counterforce-capable systems (accurate MIRVs, silo-busting warheads, command-and-control hardening) that only make sense if limited nuclear victory is a coherent goal. Contracts, research programs, and force modernization cycles are justified by reference to maintaining a credible warfighting/counterforce option rather than pure retaliatory deterrence. Exit from this constraint would mean losing a primary line of procurement justification.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Treaty frameworks and verification bodies (START-lineage agreements, non-proliferation institutions) built on the premise that nuclear war has no acceptable outcome and should be minimized toward abolition or stable minimum deterrence. Counterforce/winnability doctrine undercuts their negotiating leverage — if planners can credibly claim limited victory is achievable, the case for deep reductions and no-first-use weakens. They can lobby and negotiate but cannot force targeting doctrine to change; their tools are diplomatic, not coercive.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Live near military and industrial targets whose classification as 'counterforce' rather than 'countervalue' is precisely what makes limited nuclear war seem survivable to planners. They bear the actual fallout, casualty, and infrastructure-collapse risk that the doctrine treats as an acceptable, boundable cost of a winnable exchange. They have no seat in doctrine formulation and no meaningful exit from geographic proximity to targeted infrastructure.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeted_regions, payer,
    powerless, generational, trapped, continental).

% The set of doctrinal and technical practices (second-strike assurance, launch-on-warning avoidance, transparency measures) that keep crises from escalating to nuclear use. Countervailing/counterforce doctrine erodes these norms by incentivizing first-strike-capable postures and rapid escalation-control planning, since a 'winnable' war rewards preemption and disfavors patient de-escalation. Listed as a non-agent structural casualty, not a decision-making party.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__countervailing_thinkable, crisis_stability_norms).

% Political and military leadership that finds counterforce/winnability doctrine useful for signaling resolve, extracting alliance commitments, and justifying force posture to legislatures. Benefits from the doctrine's existence as a bargaining and budget tool even when its members privately doubt any nuclear exchange would remain limited in practice.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, national_security_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, national_security_establishment, agenda_setter).

% Strategists and scholars (the sibling 'deterrence_unthinkable' reading) who argue that treating nuclear war as winnable is a category error given assured retaliatory destruction. They publish, testify, and advise arms-control negotiators but are structurally excluded from the counterforce planning apparatus itself, which proceeds on its own premises regardless of their critique.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_theorists_unthinkable_camp, excluded,
    organized, civilizational, constrained, global).

% Historians and political scientists who trace the doctrine's persistence across administrations, its budgetary and organizational drivers, and its divergence from declaratory rhetoric. They document the gap between what is said publicly and what is planned operationally without themselves holding planning authority.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the alliance and domestic political system with a credible, escalatory-ladder theory of nuclear use — a way to signal resolve short of total annihilation, coordinate extended deterrence commitments to allies, and give civilian leadership actionable options in a crisis rather than an all-or-nothing choice.
% TRANSFER_FUNCTION: Moves budget authority, doctrinal legitimacy, and institutional mission continuity to targeting planners and weapons contractors who sustain the counterforce force structure; moves risk and normative erosion onto arms-control institutions and onto civilian populations near targeted military-industrial infrastructure, who bear the actual physical cost that the doctrine treats as boundable.
% ABSENT_VOICES: Civilian populations near targeted sites have no representation in doctrine formulation. The 'deterrence_unthinkable' camp is heard in academic and advisory channels but does not sit inside the counterforce planning apparatus that actually sets target lists and escalation protocols; their critique is structurally routed around rather than answered.
% DISAPPEARANCE_RATIONALE: If the countervailing/counterforce planning apparatus were dismantled overnight, force structure justifications tied to warfighting capability would collapse toward pure minimum/assured-retaliation postures, arms-control negotiators would gain significant leverage for deep cuts, and a large share of targeting, C2-hardening, and precision-warhead procurement would lose its doctrinal rationale — the strategic force posture and associated budgets would visibly reorganize.
% FOUNDING_PROBLEM: Pure mutual-assured-destruction doctrine offered no credible response short of full retaliation, making extended deterrence commitments to allies seem incredible (why would a president trade Chicago for Hamburg?) and leaving civilian leadership with no options between surrender and apocalypse in a crisis.
% FOUNDING_PROBLEM_CORROBORATION: Targeting planners and the national security establishment attest the credibility gap remains live, citing extended-deterrence commitments to allies as ongoing justification. Arms-control scholars and independent historians (outside the beneficiary set) attest that flexible-response/countervailing doctrine has, since the 1970s, functioned primarily to sustain force-modernization budgets and bureaucratic mission continuity rather than to solve a genuinely unresolved credibility problem, pointing to declassified planning documents showing damage-expectancy models persisted through periods of stable extended deterrence with no crisis-driven demand for them.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is authored high but not maximal: the coordination function (credible extended deterrence, crisis flexibility short of total war) is genuine and non-trivial, which caps extraction below what a pure snare would show, but the doctrine's persistence through periods (post-Cold War drawdown, post-2010 arms-reduction treaties) where the original credibility problem was arguably resolved indicates rent-seeking layered onto the coordination core — hence the classification lean toward tangled_rope rather than rope. Suppression (0.6) reflects that alternatives (minimum deterrence, no-first-use, launch-under-attack-only postures) are actively argued down within planning circles rather than genuinely absent — this is enforced doctrinal preference, not physical impossibility. Theater ratio (0.42) captures that a meaningful share of counterforce planning activity (elaborate escalation-ladder exercises, damage-expectancy modeling long after crisis credibility was established) functions as institutional performance sustaining budget lines rather than operationally decisive planning. accessibility_collapse (0.5) and resistance (0.58) are mid-range: alternative doctrines (minimum deterrence) remain articulable and are actively argued by a real opposing camp, so alternatives have not collapsed the way they would under a mountain, but institutional resistance to abandoning counterforce planning is substantial.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic targeting planners and the military-industrial complex sit near the beneficiary end: their institutional relevance, budget, and mission scope depend on winnability remaining a live category, and they hold arbitrage-grade exit (they can shift the doctrine's emphasis and terminology across administrations without losing institutional standing). Arms control regimes and crisis stability norms sit near the target end: winnable-war planning directly erodes their negotiating leverage and normative force, and their exit options are constrained to diplomatic argument rather than binding authority over targeting doctrine. Civilian populations near targeted infrastructure are the most fully-targeted seat: trapped exit, powerless, and bearing the doctrine's central risk-bearing assumption (that counterforce exchanges are survivable and boundable) with zero voice in its formulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extended-deterrence credibility under pure MAD) was genuinely live at the doctrine's 1970s founding. The R5 interview marks its status contested rather than dead: planners still cite live extended-deterrence commitments, while independent historians point to declassified evidence that damage-expectancy modeling persisted through periods of stable deterrence with no crisis-driven demand, suggesting institutional mission continuity has substantially replaced the original credibility rationale as the doctrine's actual maintenance driver. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine (if now partially obsolete) coordination function as pure extraction — the credibility problem was real when the doctrine was founded, and a residual version of it persists in extended-deterrence commitments to allies today. Classifying it as tangled_rope rather than rope prevents treating the arrangement's continued expansion of counterforce capability, well past the point where the original crisis was resolved, as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_reality_vs_doctrinal_artifact,
    'Is limited nuclear victory through counterforce targeting a genuine strategic possibility this reading correctly identifies, or is it a doctrinal artifact sustained by institutional actors who benefit from its continued plausibility regardless of its operational truth?',
    'Independent technical assessment of escalation-control feasibility (whether counterforce exchanges have historically or could plausibly remain limited given launch-on-warning postures, C2 vulnerabilities, and use of nuclear weapons against dual-use targets), cross-checked against declassified war-gaming outcomes and the post-hoc judgment of participants across administrations.',
    'If winnability is a genuine strategic fact, this reading''s coordination function is stronger and closer to rope; if it is substantially a doctrinal artifact serving mission continuity, extraction is higher than authored here and the classification moves further toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(winnability_reality_vs_doctrinal_artifact, conceptual, 'Whether counterforce winnability is descriptively true or institutionally constructed.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the ''countervailing_thinkable,'' ''deterrence_unthinkable,'' and ''rhetorical_contraction'' readings of the war-winnability kernel are all held by real, organized factions simultaneously, which reading actually governs operational U.S. and allied nuclear planning at any given historical moment?',
    'Comparative analysis of classified targeting doctrine (where declassified) against declaratory public rhetoric across administrations, to determine whether the operational planning apparatus tracks the countervailing reading consistently or shifts between readings by administration.',
    'If operational planning consistently tracks a different reading (e.g., rhetorical_contraction, where winnability is planned but never stated), this story''s claim that winnability is affirmatively thinkable and openly reasoned-about (rather than practiced under euphemism) would need revision, though the underlying ε and stakeholder structure could remain largely intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading actually describes operational doctrine at a given time.').

omega_variable(
    arms_control_erosion_causal_link,
    'Is the erosion of arms control regimes'' negotiating leverage causally attributable to counterforce/winnability doctrine specifically, or to broader great-power competition dynamics that would erode arms control regardless of doctrine content?',
    'Historical comparison of arms-control negotiation outcomes during periods of stated minimum-deterrence posture versus stated countervailing posture, controlling for broader geopolitical tension.',
    'If the causal link is weak, the victim designation of arms_control_regimes overstates this doctrine''s specific contribution to their erosion relative to independent geopolitical drivers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_control_erosion_causal_link, empirical, 'Whether counterforce doctrine specifically, versus broader competition, drives arms-control erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.3).
narrative_ontology:measurement(war__tr_t1983, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1983, 0.28).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1991, 0.4).
narrative_ontology:measurement(war__tr_t2001, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2001, 0.45).
narrative_ontology:measurement(war__tr_t2010, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.55).
narrative_ontology:measurement(war__be_t1983, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1983, 0.6).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement(war__be_t2001, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(war__be_t2010, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.5).
narrative_ontology:measurement(war__su_t1983, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1983, 0.58).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement(war__su_t2001, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(war__su_t2010, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'nuclear war winnability' claim per the ε-invariance principle. 'deterrence_unthinkable' asserts a fundamentally different ε (near-zero extraction, Mountain-leaning: victory is treated as logically incoherent under assured destruction). 'rhetorical_contraction' locates the constraint's action in public discourse rather than doctrine, with a distinct victim set (public deliberation, democratic oversight) rather than arms-control regimes specifically. All three share the same underlying kernel (war_winnability_post_1945) but instantiate structurally distinct constraints with different beneficiary/victim structures and different ε values; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
