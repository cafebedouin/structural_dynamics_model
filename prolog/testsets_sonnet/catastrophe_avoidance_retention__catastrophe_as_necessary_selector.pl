% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe-as-Necessary-Selector Doctrine of Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability industries (nuclear power, aviation, healthcare,
 *   chemical process safety), a persistent institutional belief holds that
 *   genuine competence renewal requires the visceral shock of actual
 *   catastrophe — that simulations, however sophisticated, cannot replicate
 *   the mortality salience and organizational trauma that forces deep
 *   learning and lasting behavioral change. This belief elevates those who
 *   possess lived-catastrophe experience (incident commanders, post-disaster
 *   consultants, regulators whose mandates expanded after failures) and
 *   systematically discounts the competence of those who have only trained
 *   through simulation and near-miss review, regardless of demonstrated
 *   safety record. The doctrine captures a real phenomenon (skill atrophy
 *   during long calm periods, normalization of deviance) but converts it into
 *   a claim that only catastrophe itself can fix, which conveniently
 *   privileges those whose authority derives from catastrophe experience and
 *   disadvantages those maintaining safety without it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.41).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe-as-Necessary-Selector Doctrine of Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '36bb2c38-abd4-48c3-99b5-101b11b2c78b').
narrative_ontology:cs_kernel_codification('36bb2c38-abd4-48c3-99b5-101b11b2c78b', distributed).
narrative_ontology:cs_authority_grounding('36bb2c38-abd4-48c3-99b5-101b11b2c78b', practice).
narrative_ontology:cs_interpretation_layer_present('36bb2c38-abd4-48c3-99b5-101b11b2c78b').
narrative_ontology:cs_reading_relation('36bb2c38-abd4-48c3-99b5-101b11b2c78b', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('36bb2c38-abd4-48c3-99b5-101b11b2c78b', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('36bb2c38-abd4-48c3-99b5-101b11b2c78b', foundational, only_lived_trauma_resets_competence).
narrative_ontology:cs_axiom_status(only_lived_trauma_resets_competence, holdable).
narrative_ontology:cs_axiom_grounding('36bb2c38-abd4-48c3-99b5-101b11b2c78b', only_lived_trauma_resets_competence, empirically_contingent).
narrative_ontology:cs_axiom('36bb2c38-abd4-48c3-99b5-101b11b2c78b', secondary, simulation_fidelity_cannot_substitute_for_mortality_salience).
narrative_ontology:cs_axiom_status(simulation_fidelity_cannot_substitute_for_mortality_salience, holdable).
narrative_ontology:cs_axiom_grounding('36bb2c38-abd4-48c3-99b5-101b11b2c78b', simulation_fidelity_cannot_substitute_for_mortality_salience, empirically_contingent).
narrative_ontology:cs_reference_frame('36bb2c38-abd4-48c3-99b5-101b11b2c78b', post_disaster_institutional_memory_primacy).
narrative_ontology:cs_drift_state('36bb2c38-abd4-48c3-99b5-101b11b2c78b', extended_contemporary_peacetime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('36bb2c38-abd4-48c3-99b5-101b11b2c78b', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_incident_reform_consultants).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_commanders).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies_post_disaster).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators_in_peacetime).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, communities_near_high_hazard_facilities).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, workers_subject_to_deferred_safety_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rose to authority through direct experience commanding responses to actual disasters. Their institutional standing, promotion history, and internal credibility rest on having 'been there' during chaos. They set training doctrine and staffing priorities, and they consistently privilege lived catastrophe experience over simulated competence when evaluating readiness, which channels resources and prestige toward themselves and away from simulation-based training investment.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_commanders, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_incident_commanders, agenda_setter).

% Firms and individuals who are retained after major failures to conduct root-cause reviews, redesign protocols, and retrain staff. Their business model depends on catastrophes recurring at some cadence; a genuinely catastrophe-free industry has no market for their services. They have no incentive to validate that simulation alone could have prevented the trauma that generated their contract.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_incident_reform_consultants, beneficiary,
    organized, biographical, arbitrage, national).

% Regulatory agencies gain expanded mandate, budget, and legislative authority in the aftermath of catastrophic failures. Their institutional growth is historically catastrophe-triggered, which creates a structural (not necessarily conscious) disincentive to certify that simulation-based competence maintenance is sufficient, since that certification would undercut the narrative that justifies periodic mandate expansion.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies_post_disaster, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_bodies_post_disaster, agenda_setter).

% Work the systems day to day during long stretches without major incidents. Under this doctrine, their skills are treated as inevitably decaying no matter how much they train or drill, because only 'real' chaos counts as adequate selection pressure. This delegitimizes their simulation-based training investments, denies them credit for maintained competence, and leaves them exposed to blame if a long-simulated-but-untested system fails.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators_in_peacetime, payer,
    moderate, biographical, trapped, local).

% Live adjacent to industrial, nuclear, aviation, or medical systems whose safety depends on the competence this doctrine claims can only be renewed by catastrophe. If the doctrine is correct, these communities bear the tail risk of the next 'necessary' catastrophe as the price of institutional relearning; if the doctrine is wrong, they bear the cost of underinvestment in simulation anyway, with no compensating benefit.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, communities_near_high_hazard_facilities, payer,
    powerless, generational, trapped, regional).

% Where organizations internalize the belief that only catastrophe truly resets competence, simulation and drill budgets are systematically deprioritized relative to post-disaster remediation budgets. These workers absorb the accumulated risk of that underinvestment during the long peacetime interval, without the bargaining power to redirect resources toward preventive practice.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, workers_subject_to_deferred_safety_investment, payer,
    powerless, biographical, trapped, national).

% Study high-reliability organizations and competence decay curves across industries. They can compare organizations that experienced recent catastrophes against those relying purely on simulation and near-miss review, and are positioned to test the doctrine's core empirical claim rather than assume it.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineering_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine coordinates institutional attention and resources around the genuine problem that skills atrophy during long incident-free periods and that untested assumptions accumulate silently in complex systems — it names a real decay dynamic that organizations otherwise ignore during calm.
% TRANSFER_FUNCTION: Moves prestige, budget, and doctrinal authority toward those who possess or broker lived-catastrophe experience (veteran commanders, post-incident consultants, expanded regulators), and moves risk exposure and blame toward frontline operators and nearby communities who bear the consequences of both the doctrine's underinvestment logic and any catastrophe treated as 'necessary.'
% ABSENT_VOICES: Frontline operators who maintain competence through disciplined simulation and near-miss analysis without ever experiencing a qualifying catastrophe are structurally unable to prove their competence within this doctrine's own terms — their evidence (a clean safety record) is read as absence of the very selection pressure the doctrine claims is required, not as success. Communities near hazard facilities have no seat in doctrinal debates about what counts as adequate readiness.
% DISAPPEARANCE_RATIONALE: If organizations abandoned the belief that catastrophe is the necessary selector, veteran commanders and post-incident consultants would lose a structural argument for their authority and market, and regulators would need a different justification for mandate growth — those parties dispute that anything would change operationally. Safety researchers and frontline operators argue the world would rearrange toward much heavier investment in simulation fidelity and near-miss reporting infrastructure, on the view that the doctrine currently suppresses that investment.
% FOUNDING_PROBLEM: Organizations genuinely do lose sharpness during long periods without incidents: procedures become rote, warning signs get normalized, and the visceral memory of what failure costs fades from institutional culture (the 'normalization of deviance' pattern documented after Challenger, Bhopal, and similar disasters).
% FOUNDING_PROBLEM_CORROBORATION: Veteran commanders and post-incident consultants attest the problem is live and specifically requires catastrophic experience to solve. Safety engineering researchers studying high-reliability organizations (nuclear carrier operations, air traffic control) report organizations sustaining high competence for decades through rigorous simulation and near-miss analysis alone, without an intervening catastrophe — this is corroboration from outside the benefiting parties that the founding problem may be real but the catastrophe-specific solution is not the only or best-supported one.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the doctrine's operation transfers institutional resources and legitimacy toward catastrophe-experienced authorities and away from simulation investment, without a settled empirical basis for that transfer — the extraction is in the misallocation, not in catastrophes themselves. Theater ratio is high and rising (0.62) because as peacetime periods extend, organizations increasingly perform deference to catastrophe-derived authority (ritual invocation of 'we haven't been tested yet') rather than substantively investing in the near-miss and simulation infrastructure that could address the real decay problem the doctrine correctly identifies. Suppression is moderate (0.41) and rises slowly: it operates less through coercion and more through epistemic foreclosure — the doctrine is structured so that a clean safety record cannot count as evidence against it, only as evidence that catastrophe hasn't yet 'done its work.' Accessibility collapse and resistance are set at 0.5/0.55 reflecting that this is a genuinely contested doctrinal claim, not a settled fact or an obviously unjust arrangement: some organizations resist it vigorously (safety researchers, high-reliability theorists) while others treat it as unfalsifiable common sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran incident commanders, post-incident consultants, and regulatory bodies sit near the beneficiary end: their authority, market, and mandate growth are structurally tied to catastrophe recurrence and to the delegitimization of simulation-only competence claims. Frontline operators, nearby communities, and workers subject to deferred safety investment sit near the target end: they bear the accumulated risk of underinvestment during peacetime (justified by 'decay is inevitable anyway') and the tail risk if a catastrophe is treated as a necessary and acceptable cost of relearning. The doctrine's directionality is asymmetric precisely because the two groups have opposite stakes in whether simulation is judged sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine competence decay during long calm intervals — is real and well-documented (normalization of deviance, procedural drift). What has drifted is the SOLUTION claimed necessary: rather than treating catastrophe as one costly and undesirable teacher among several, this reading elevates it to the ONLY sufficient teacher, which forecloses investment in cheaper, non-catastrophic alternatives and vindicates the institutional position of those whose authority derives from catastrophe response. Classifying this as tangled_rope rather than snare preserves the fact that decay is a genuine coordination problem this doctrine responds to — it is not pure invention — while still registering the asymmetric extraction: those positioned to benefit from catastrophe-derived authority have structural reasons not to certify that cheaper alternatives (simulation, near-miss learning) are adequate, even where evidence supports them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_vs_correlation,
    'Does catastrophic experience CAUSE superior competence retention, or are organizations that experience catastrophes simply the ones whose prior underlying risk profile made both the catastrophe and subsequent visible ''improvement'' more likely (survivorship and selection confound)?',
    'Comparative longitudinal study of high-reliability organizations matched on hazard profile and system complexity, some of which experienced major incidents and some of which did not, tracking objective safety performance (not perceived competence) over subsequent decades.',
    'If catastrophe experience does not causally outperform rigorous simulation-based programs when properly matched, this reading''s core empirical claim collapses and the constraint''s extractive function (privileging catastrophe-derived authority) loses its coordination cover entirely, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_correlation, empirical, 'Whether catastrophe experience causally produces superior competence retention versus being confounded with prior risk exposure.').

omega_variable(
    committer_framing_which_reading_is_dominant,
    'Within a given industry or organization at a given time, which of the three kernel readings (catastrophe-as-necessary-selector, simulation-as-proxy, hybrid near-miss) is actually operative in resource allocation and promotion decisions, and can this be observed rather than inferred from official doctrine?',
    'Trace actual budget and promotion decisions across a sample of high-hazard organizations following long incident-free periods: does investment flow toward simulation fidelity, toward retention/promotion of catastrophe-experienced staff, or toward distributed near-miss reporting infrastructure? The revealed allocation pattern indicates which reading the organization actually holds, regardless of its stated safety philosophy.',
    'If most organizations'' revealed behavior tracks the hybrid reading despite this doctrine''s rhetorical dominance in post-incident narratives, this reading''s real-world extractive footprint is smaller than its cultural prominence suggests — a conceptual and empirical gap between stated doctrine and operative practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_which_reading_is_dominant, conceptual, 'Whether organizations'' actual resource allocation reveals a different operative kernel reading than the doctrine they officially espouse.').

omega_variable(
    acceptable_cost_of_necessary_catastrophe,
    'If catastrophe genuinely is a necessary competence selector, what human and material cost is treated as an acceptable price for that renewal, and who bears it versus who decides it is acceptable?',
    'Policy and ethics review of how organizations that hold this doctrine explicitly or implicitly price catastrophic risk against competence-maintenance benefit, compared against the communities and workers who bear realized catastrophic harm.',
    'If the doctrine implies an acceptable catastrophe rate but the deciders (institutional authorities) are structurally insulated from bearing the realized cost (borne instead by frontline workers and nearby communities), this sharpens the victim/beneficiary asymmetry and supports classification stability as tangled_rope rather than a softer rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_of_necessary_catastrophe, preference, 'Who decides what catastrophic cost is an acceptable price for competence renewal, versus who actually bears that cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 8, 0.4).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 16, 0.48).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 24, 0.54).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 32, 0.59).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_avoidance_retention kernel. 'simulation_as_proxy_catastrophe' claims high-fidelity drills are functionally equivalent to real catastrophic events and would show much lower extractiveness (no structural need to privilege catastrophe-experienced authority). 'hybrid_near_miss_learning' claims neither pure simulation nor pure catastrophe is sufficient and would likely classify closer to rope (distributed, lower-coercion coordination via near-miss reporting networks). Each reading has its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are not measurement variants of one constraint but three structurally distinct claims about how competence is actually maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
