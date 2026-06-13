% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-as-Competence-Anchor: Real Emergencies Required for Professional Readiness
 *   domain: organizational_learning/safety_engineering
 *
 * SUMMARY:
 *   The constraint asserts that only real catastrophic events or near-misses
 *   provide irreducible, embodied competence exercise for high-stakes
 *   professionals. Simulation, despite high fidelity, is treated as
 *   supplementary rehearsal — muscle memory and psychological readiness
 *   atrophy in the absence of actual emergency. This is ONE READING of a
 *   contested competence kernel. The sibling readings are: (1)
 *   simulation_as_adequate_exercise — high-fidelity simulation with
 *   structured debriefing fully maintains competence; (2) hybrid_dependency —
 *   both simulation foundation AND periodic real-world anchoring
 *   (non-jeopardy operations, line audits) are necessary and sufficient. This
 *   reading (catastrophe_as_necessary_anchor) claims real events are
 *   IRREDUCIBLE and cannot be replaced, even by excellent simulation. The
 *   constraint is CLAIMED as tangled_rope because it coordinates a real
 *   problem (competence maintenance) while extracting a cost (keeping
 *   practitioners in high-operational-tempo roles, internalizing emergency
 *   readiness as professional identity, permitting catastrophe-driven
 *   disruption). The engine measures the extraction and suppression
 *   independently of the claim.
 *
 * KEY AGENTS:
 *   - High-reliability organization leadership: institutional power, sets the competence doctrine and operational standards; defines what counts as 'real exercise'; justifies resource allocation by invoking catastrophe-necessity.
 *   - Professional practitioners (pilots, surgeons, etc.): moderate power, identity-locked exit; internalize the doctrine as professional truth; bear the psychological and physiological cost of real emergencies.
 *   - Simulation technology vendors: powerful, excluded from the decision; would benefit from simulation-adequacy framing; suppressed from setting the competence standard.
 *   - Training efficiency advocates: organized, constrained exit; bear the cost of accepting real-event-driven operational disruption; pressure to justify why their organizations tolerate inefficiency.
 *   - Stable-period organizations: pay a cost by maintaining readiness despite low real-event volume; can attempt exit but face professional stigma.
 *   - Catastrophe victims and families: powerless beneficiaries; trapped in dependence on operator readiness anchored by past catastrophes.
 *   - Research epistemology observer: analytical seat; can measure whether the embodied-knowledge decay claim is empirically true or a professional identity narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.72).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-as-Competence-Anchor: Real Emergencies Required for Professional Readiness").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "organizational_learning/safety_engineering").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '8ab79e7a-1fba-4222-8813-d4dda5902957').
narrative_ontology:cs_kernel_codification('8ab79e7a-1fba-4222-8813-d4dda5902957', distributed).
narrative_ontology:cs_authority_grounding('8ab79e7a-1fba-4222-8813-d4dda5902957', lineage).
narrative_ontology:cs_interpretation_layer_present('8ab79e7a-1fba-4222-8813-d4dda5902957').
narrative_ontology:cs_reading_relation('8ab79e7a-1fba-4222-8813-d4dda5902957', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('8ab79e7a-1fba-4222-8813-d4dda5902957', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('8ab79e7a-1fba-4222-8813-d4dda5902957', foundational, real_events_irreducibly_teach_competence).
narrative_ontology:cs_axiom_status(real_events_irreducibly_teach_competence, holdable).
narrative_ontology:cs_axiom_grounding('8ab79e7a-1fba-4222-8813-d4dda5902957', real_events_irreducibly_teach_competence, empirically_contingent).
narrative_ontology:cs_axiom('8ab79e7a-1fba-4222-8813-d4dda5902957', secondary, simulation_is_supplementary_rehearsal_not_replacement).
narrative_ontology:cs_axiom_status(simulation_is_supplementary_rehearsal_not_replacement, holdable).
narrative_ontology:cs_axiom_grounding('8ab79e7a-1fba-4222-8813-d4dda5902957', simulation_is_supplementary_rehearsal_not_replacement, empirically_contingent).
narrative_ontology:cs_reference_frame('8ab79e7a-1fba-4222-8813-d4dda5902957', embodied_competence_through_real_events).
narrative_ontology:cs_drift_state('8ab79e7a-1fba-4222-8813-d4dda5902957', contemporary_simulation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ab79e7a-1fba-4222-8813-d4dda5902957', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, professional_cadre_wielding_authority).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_advocates).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_efficiency_seekers).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_in_stable_periods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_efficiency_advocates).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_survivors_and_families).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, professional_practitioners).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_efficiency_advocates).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, stable_period_organizations).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, irreducible_embodied_knowledge_thesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_inadequacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets operational tempo, competence standards, and resource allocation. Justifies sustained readiness by invoking past catastrophes as proof that 'only real events teach what matters.' Maintains expensive redundancy, drill schedules, and operational burden partly by citing the catastrophe-anchor doctrine. Can shift investment to simulation-dominant or hybrid regimens but chooses the catastrophe-anchor framing because it legitimates their authority to keep practitioners in high-stakes roles and to tolerate operational disruption as necessary.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organization_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Operator pilots, emergency responders, trauma surgeons, nuclear reactor operators. Internalize the constraint as professional identity: 'real experience is what defines competence; simulation is rehearsal.' Bear the psychological, physiological, and career costs of real emergencies: shift work, sleep deprivation, moral injury from losses, career lock-in to high-stakes roles. Cannot exit because leaving means abandoning professional identity. The constraint enforces their participation in real emergencies as the proof of competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, professional_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Manufacture high-fidelity flight simulators, surgical simulators, tactical training systems, virtual-reality platforms. Would benefit from 'simulation is adequate' or 'hybrid with heavy simulation' framings that redirect investment from real-world events to simulator technology. Excluded from control over what counts as sufficient competence; their voice argues for simulation adequacy but cannot set the standard. Face the constraint as a suppression mechanism that privileges real-event narratives over technology advancement.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_technology_vendors, excluded,
    powerful, biographical, constrained, global).

% Hospital administrators, airline training directors, military logistics planners seeking to optimize competence maintenance while minimizing cost and operational disruption. Want to shift from real-event-dependent training to high-fidelity simulation and structured non-jeopardy operations. Pay a cost in deferred simulator investment, in pressure to justify lower-intensity training, and in pressure from practitioners and leadership to tolerate operational losses. Cannot exit because they sit inside institutions where the catastrophe-anchor doctrine is enforced from above. Benefit incidentally if simulator technology improves (reduces the cost of maintaining competence), but are suppressed from advocating this benefit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_efficiency_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_efficiency_advocates, beneficiary).

% Organizations in sectors or periods without recent catastrophic events: regional hospitals with low trauma volume, airlines on stable routes, rural emergency services. The constraint requires them to maintain readiness and competence despite low real-world exercise opportunities, incurring training costs and operational inefficiency. Can attempt exit by reducing readiness levels or shifting to simulation-dominant regimens; doing so is professionally stigmatized and organizationally risky because the doctrine frames low readiness as negligent.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, stable_period_organizations, payer,
    moderate, biographical, mobile, regional).

% Persons affected by high-consequence emergencies. Benefit from operators who maintain real-world competence anchored by past catastrophes. The constraint's logic is that their survival depends on a pilot or surgeon or responder whose embodied readiness was forged in actual emergencies. Trapped in dependence; cannot influence the competence standard; can only voice retrospective vindication if an emergency occurs and survival depends on readiness.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_survivors_and_families, beneficiary,
    powerless, biographical, trapped, local).

% Researchers in learning science, cognitive psychology, expertise studies, human factors. Investigate whether real-event-anchored competence differs structurally from simulation-anchored competence, and whether differences are empirical (muscle memory truly does decay differently) or normative (practitioners believe real events are necessary but measurement shows simulation is equivalent). Produce meta-analyses and experimental designs that could resolve whether the catastrophe-anchor doctrine rests on demonstrable skill preservation or on professional identity fusion and organizational inertia.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, research_epistemology_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_organization_leadership).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a doctrine that forces organizations and practitioners to treat real catastrophic events and near-misses as irreducible learning exercises, preventing drift into purely theoretical or simulator-based competence in domains where high-stakes operations are rare. The coordination problem solved: how do we keep people sharp when actual emergencies are unpredictable and infrequent? The constraint's answer: we cannot use simulators alone; real events must remain central to identity and readiness, and practitioners must internalize catastrophe-exposure as constitutive of professional competence.
% TRANSFER_FUNCTION: Moves psychological, physiological, and career costs from organizations (which could pay for high-fidelity simulators, non-jeopardy training, and reduced operational load) to practitioners (who must remain in high-stakes roles, internalize emergency readiness as professional identity, and accept the psychological burden of real events), and to stable-period organizations and efficiency advocates (who must accept operational disruption and inefficiency as the necessary price of maintaining the doctrine).
% ABSENT_VOICES: Simulation technology vendors sit at industry tables but are structurally excluded from authority over what counts as 'adequate' competence exercise; their voice proposes 'simulation can be made adequate' but cannot set the standard. Training efficiency advocates within organizations propose hybrid or simulation-heavy regimens but face suppression from leadership and practitioners wedded to the catastrophe-anchor doctrine. Practitioners themselves are not typically conscious agents in the doctrinal debate; the constraint operates as a taken-for-granted professional truth, enforced through hiring, licensing, and status allocation rather than through explicit argument.
% DISAPPEARANCE_RATIONALE: If the catastrophe-anchor doctrine disappeared overnight, organizations would shift within 2–3 years to simulation-heavy, lower-operational-tempo training regimens. Pilot training would move to synthetic environments; surgical residencies would increase simulator time and reduce actual-patient volume; emergency responders would depend on scenario drills and non-jeopardy line audits rather than rare real-call volume. Professional identity would reconstruct around 'competence through simulation plus periodic real-world validation' rather than 'forged in catastrophe.' Readiness metrics would initially degrade in the first real emergencies, revealing the atrophy that was previously masked by the doctrine's enforcement. The competence landscape would reorganize around efficiency optimization, simulator fidelity, and risk-managed training rather than real-event necessity.
% FOUNDING_PROBLEM: High-stakes professionals (pilots, surgeons, trauma specialists, nuclear operators) undergo long quiet periods without real emergencies. During these periods, competence decays in ways that are hard to measure but empirically observable in crisis performance. Simulation and drills slow decay but may not arrest the embodied, psychological, and physiological components of emergency readiness. Early aviation disasters (especially 1970s–1980s) revealed that pilots extensively trained in procedure could fail in novel real-world conditions where judgment, instinct, and emotional regulation became decisive. The founding problem: how to keep practitioners sharp and ready when the actual irreducible learning opportunity (real emergency) is rare, dangerous to manufacture deliberately, and unpredictable. The doctrine claims the answer is: you cannot replace real events, so the organization must structure itself around the assumption that real emergencies are the irreducible teacher.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability organization leaders and senior practitioners attest the problem is live and urgent: recent aviation incidents (aircraft accidents, loss-of-control events), trauma cases (surgical judgments), and emergency-response failures (decision paralysis) show gaps that simulator training alone did not close, supporting the competence-decay narrative. Simulation technology vendors and training efficiency researchers attest that simulator fidelity has advanced dramatically since the 1970s; they argue the problem is substantially solved by better simulators and structured debriefing. Independent cognitive science research shows mixed evidence: some studies support the claim that embodied-knowledge and psychological readiness decay absent real-world exposure; other studies using high-fidelity simulations show performance maintenance equivalent to real-practice cohorts. No outside-beneficiary source fully corroborates either position; the contest is genuine and ongoing. Research from commercial aviation (where incident rates are extremely low and simulator dominance is high) shows that real accidents are rare enough that the embodied-decay claim cannot be directly tested — pilots exit the profession before they face a life-or-death emergency, so the claim remains empirically underconstrained.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) and rising over the interval (0.52 to 0.68 from t0 to t25, then stable). The rising trajectory reflects accumulating pressure on training-efficiency advocates and stable-period organizations to accept the doctrine despite expanding simulation technology. Suppression is high (0.72) because the constraint's persistence depends on actively excluding alternative framings: simulation-adequacy is suppressed as 'naive underestimation of competence decay,' efficiency arguments are suppressed as 'penny-wise, pound-foolish,' and lower-readiness options are suppressed as 'professionally negligent.' Theater ratio rises from 0.32 to 0.58 — a striking trajectory indicating growing performative activity (drills, post-incident reviews, competence certifications) relative to actual irreducible learning. The rising theater suggests that as real-event frequency may be dropping (in stable sectors), organizational response is to emphasize the ritual of competence-via-catastrophe rather than its substance. Accessibility of alternatives collapses substantially (0.79): once practitioners internalize the doctrine as professional identity ('I am someone who has faced real emergencies'), the alternative framings (simulation adequacy, hybrid dependency) become psychologically inaccessible. Resistance is moderate (0.61): simulation vendors resist; training directors resist; but the doctrine is reinforced by genuine past disasters and by the professional cultures of high-reliability sectors. The measurements share one time grid across all metrics.
 *
 * PERSPECTIVAL GAP:
 *   The organization leadership seat and the practitioner seat should compute differently. From leadership's position, the constraint solves a real coordination problem (keeping people sharp) and is justified by past catastrophes — a genuine rope. From the practitioner's position (identity-locked), the same constraint extracts constant psychological cost and prevents career mobility while deepening their fusion with a dangerous role — a snare masquerading as a rope. From the excluded simulation-vendor seat, the constraint is pure extraction protecting an incumbent institutional arrangement against technological displacement. The engine computes these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Organization leadership benefits from the doctrine (it legitimates their operational authority and resource allocation) — d near beneficiary end (~0.2). Professional practitioners pay a cost (psychological load, identity lock, career constraint) and nominally benefit (competence maintenance) but the benefit is conflated with their identity — net directionality toward target (~0.7). Training efficiency advocates and stable-period organizations clearly pay without clear benefit — d in target range (~0.75). Simulation vendors are excluded from decision-making — d near identity-locked target end (~0.85). Catastrophe victims nominally benefit but are powerless to influence the standard — d near dependent beneficiary end (~0.15). The powered analytical observer has d near 0.5 (symmetric, neither extracting nor benefiting). No directionality overrides are needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows signs of mandatrophy (doctrine persisting past its founding problem's resolution). The founding problem was: how do we maintain competence during long quiet periods before aviation was as safe as it is now? Modern simulation fidelity, scenario-based training, and non-jeopardy operational exercises (line checks, surgical grand rounds) have matured substantially since the 1970s. Yet the catastrophe-anchor doctrine persists, enforced as professional dogma. The rising theater ratio (0.32 to 0.58) is a symptom: drills and post-incident reviews are performed with increasing ritualism while the actual irreducible learning from real catastrophes may be declining (better safety engineering, fewer incidents). The constraint prevents mandatrophy resolution by suppressing the alternative readings (simulation adequacy, hybrid dependency) that would adjust the standard. A mandatrophy reading would emerge if evidence accumulated that: (a) simulation-trained operators perform as well as real-event-anchored operators in actual emergencies (falsifying the irreducibility claim), or (b) the high cost of maintaining operational readiness via real-event exposure (in lives lost, training inefficiency, burnout) exceeds the benefit (marginal competence preservation). Currently, the constraint deflects this calculation by treating the 'irreducibility' as axiomatic rather than empirical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_decay_empirical_claim,
    'Does competence in high-stakes emergency response actually decay faster in simulator-only regimens than in mixed real-event-exposure regimens? Is the decay measurable and practically significant?',
    'Longitudinal cognitive and performance studies comparing simulator-trained vs. real-event-exposed cohorts in controlled domains (e.g., trauma surgery, aviation); measurement of response time, decision quality, and physiological markers of readiness in actual emergencies; meta-analysis of incident data correlating operator training history with performance.',
    'If decay is not measurable or is negligible compared to simulator-fidelity effects, the irreducibility claim collapses and the constraint becomes a Piton (myth-maintained doctrine) rather than Tangled Rope. If decay is substantial and irreplaceable, the constraint''s classification as Tangled Rope (real coordination function + extraction) is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_decay_empirical_claim, empirical, 'Whether the psychological/embodied components of emergency readiness decay absent real-event exposure, and whether the decay is practically irreducible by simulation.').

omega_variable(
    identity_fusion_mechanism,
    'Is the constraint''s persistence maintained by genuine competence need, or by professional-identity fusion that treats real-emergency-exposure as constitutive of self?',
    'Qualitative research with practitioners exploring how they perceive simulator training vs. real-event experience; exit interviews with practitioners who leave high-stakes professions; cross-cultural comparison with countries that have shifted to simulation-heavy regimens and assess whether professional identity reconstruction occurred.',
    'If identity fusion is the primary mechanism (practitioners cannot psychologically accept ''I am competent'' without real-event anchoring), the constraint''s suppression mechanism is internalized rather than structural. Practitioners would need to deconstruct their professional identity to exit — the constraint becomes more extractive than the structural data alone suggests. This would support a piton or high-suppression snare classification rather than tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, conceptual, 'Whether suppression is structural (real competence need) or internalized (professional identity fusion making simulation feel inadequate psychologically).').

omega_variable(
    simulation_adequacy_boundary,
    'What level of simulation fidelity, scenario variety, and feedback structure would constitute ''adequate'' exercise under this reading? Is adequacy a technical threshold, or is it permanently receding as practitioners and institutions raise the bar?',
    'Review historical claims about simulation adequacy (e.g., ''full-motion simulator + debriefing is sufficient'' circa 1990s) against current practice; track whether organizations accepted simulation-dominant regimens and later reinstated real-event exposure; analyze whether simulator fidelity improvements are met with acceptance of simulation-adequacy or with escalated claims about real-event necessity.',
    'If adequacy is a fixed technical threshold that simulation can eventually meet, the constraint is potentially temporary (Scaffold on a very long fuse). If adequacy is permanently receding (doctrine always claims ''simulation is not quite good enough yet''), the constraint is Piton — a myth maintained by continuously shifting the bar rather than by genuine irreducibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_adequacy_boundary, conceptual, 'Whether simulation adequacy is a reachable technical target or a permanently receding normative standard.').

omega_variable(
    cost_of_real_event_maintenance,
    'What is the total cost to organizations and practitioners of maintaining real-event-anchored competence (operational disruption, training inefficiency, practitioner burnout, lives lost in training accidents) relative to the measurable benefit (competence preservation)?',
    'Cost-benefit analysis across sectors: calculate training burden, operational redundancy costs, burnout rates, and training-related fatalities; compare to estimated competence gains from real-event exposure vs. simulation; assess whether the cost is receding (becoming harder to justify) or stable.',
    'If cost substantially exceeds benefit, the constraint is on a path to mandatrophy resolution — the founding problem is solved, the arrangement persists by inertia and identity fusion, and the constraint should be reclassified as Piton once the cost exceeds benefit openly. If benefit exceeds cost, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_real_event_maintenance, empirical, 'Whether the resource and human costs of maintaining real-event-dependent competence remain justified by the competence preservation gains.').

omega_variable(
    sibling_reading_container,
    'Is this reading''s core claim (real events are IRREDUCIBLE for competence) logically compatible with the hybrid_dependency reading (both simulation and real-world anchoring needed, but the real-world anchoring could be non-jeopardy operations, line audits, etc., not full emergencies)?',
    'Conceptual analysis of what ''irreducible'' means: if it means ''some element of real-world operation is necessary,'' then hybrid_dependency is a weaker version and coexists; if it means ''only CATASTROPHIC events provide the necessary exercise,'' then it forecloses hybrid_dependency''s claim that line audits and non-jeopardy operations suffice.',
    'If this reading forecloses hybrid_dependency, the two readings cannot both be true in one framework, and the kernel contest is a genuine logical contradiction. If coexistence is possible, the readings differ in degree of strictness rather than kind, and both could be defended depending on one''s empirical claim about what counts as ''irreducible real exercise.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_container, conceptual, 'Whether catastrophe-necessity forecloses the possibility of hybrid real-world anchoring (non-jeopardy operations, line checks) being sufficient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 25, 0.56).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.18).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. Sibling readings (simulation_as_adequate_exercise, hybrid_dependency) instantiate the same coordination problem (maintaining competence in high-stakes domains) but assign different ε values because they make different empirical and normative claims about what exercise counts as adequate. All three stories are linked via network.affects_constraints because they are in contest — changing which reading is dominant would alter the others' operating environment and legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
