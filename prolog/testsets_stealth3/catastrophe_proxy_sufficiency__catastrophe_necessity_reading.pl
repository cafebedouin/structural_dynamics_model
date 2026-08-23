% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity Doctrine: Lived Events as the Only Sufficient Teacher
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   In safety-critical industries — aviation, nuclear power, surgery,
 *   emergency command — the standing preparation regime replaces lived
 *   catastrophe with simulation: full-motion flight decks, control-room
 *   replicas, tabletop exercises, standardized-patient crises. The constraint
 *   this story authors is the catastrophe_necessity_reading's claim about
 *   that regime: that genuine catastrophe-response competence is constituted
 *   by stress and uncertainty only real events supply, that simulation cannot
 *   manufacture them, and that during long accident-free stretches readiness
 *   therefore decays silently — with the bill presented at the next real
 *   event. On this reading the regularity presents itself as a natural limit
 *   on substitution, akin to a physical bound: no administrator enforces it,
 *   and it executes itself through reality. Yet identifiable seats do collect
 *   from its operation — veterans whose experience cannot be replicated,
 *   firms whose survived disasters function as credentials, an
 *   inquiry-and-resilience industry whose demand tracks each event — while
 *   the exposed public and each simulation-raised cohort bear its costs. This
 *   file is one member of a four-story family decomposing the colloquial
 *   label 'can simulation replace real catastrophes'; the siblings
 *   (proxy-sufficiency, hybrid-degradation, fidelity-threshold) assign
 *   different epsilon values and different victim sets to the same training
 *   regimes and are linked through network.affects_constraints. Within this
 *   file the reading is kept clean: one epsilon, one beneficiary/victim
 *   structure, one type. KEY AGENTS (by structural relationship): -
 *   training_standards_regulators: agenda-setter (institutional/constrained)
 *   — administers the simulation-reliant preparation regime -
 *   live_event_veteran_operators: primary beneficiary
 *   (moderate/identity_locked) — scarcity value of unrepeatable experience -
 *   catastrophe_experienced_firms: secondary beneficiary
 *   (institutional/arbitrage) — converts survived catastrophe into market
 *   credential - failure_analysis_consultancy_sector: tertiary beneficiary
 *   (moderate/mobile) — demand rises with each real event - exposed_public:
 *   primary payer (powerless/trapped) — bears the casualty cost of readiness
 *   decay - simulation_raised_operator_cohorts: secondary payer
 *   (moderate/constrained) — enters events without consequence-hardened
 *   response - hro_research_community: analytical observer
 *   (analytical/analytical) — supplies the evidence base all seats cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.66).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.5).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity Doctrine: Lived Events as the Only Sufficient Teacher").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '4fca2cf6-0cb0-4621-a291-618b1a4ee5dc').
narrative_ontology:cs_kernel_codification('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', distributed).
narrative_ontology:cs_authority_grounding('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', practice).
narrative_ontology:cs_interpretation_layer_present('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc').
narrative_ontology:cs_reading_relation('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_reading_relation('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', foundational, stress_acquisition_requires_real_consequence).
narrative_ontology:cs_axiom_status(stress_acquisition_requires_real_consequence, holdable).
narrative_ontology:cs_axiom_grounding('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', stress_acquisition_requires_real_consequence, empirically_contingent).
narrative_ontology:cs_axiom('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', foundational, rare_event_recognition_requires_lived_exposure).
narrative_ontology:cs_axiom_status(rare_event_recognition_requires_lived_exposure, holdable).
narrative_ontology:cs_axiom_grounding('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', rare_event_recognition_requires_lived_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', lived_catastrophe_competence_doctrine).
narrative_ontology:cs_drift_state('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', contemporary_simulation_mature_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4fca2cf6-0cb0-4621-a291-618b1a4ee5dc', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live_event_veteran_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_experienced_firms).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, failure_analysis_consultancy_sector).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, exposed_public).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_raised_operator_cohorts).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, stress_inoculation_requires_authentic_arousal).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, normalization_of_deviance_decay_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the training-hour mandates and simulator certification rules that define how safety-critical operators prepare for rare events. Their regimes assume rehearsal transfers to response; when a real event exposes a gap between certified competence and actual performance, their credibility absorbs the impact. They cannot abandon mandated-training oversight, and their reach is bounded by jurisdiction.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, training_standards_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operators who responded to real catastrophes earlier in their careers. Their firsthand knowledge anchors incident-command roles, instructor posts, and investigation panels, and commands a premium that classroom or simulator credentials do not. Their professional standing is built on having been present at events that cannot be arranged on demand; stepping away from that identity would mean surrendering the authority their careers rest on.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live_event_veteran_operators, beneficiary,
    moderate, biographical, identity_locked, global).

% Organizations that absorbed a major accident, paid the remediation and reputational cost, and now hold institutional knowledge, hardened procedures, and safety narratives that younger rivals lack. They market that experience through safety records, lessons-learned programs, and consulting arms, competing partly on a credential their competitors cannot buy except by living through their own catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_experienced_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Investigators, safety scientists, and conference-and-training businesses whose demand rises with each real event. Post-accident inquiries, expert testimony, resilience workshops, and safety-culture engagements follow the news cycle. They can pivot toward adjacent risk services when events are scarce, so their dependence on real catastrophes is strong but not total.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, failure_analysis_consultancy_sector, beneficiary,
    moderate, biographical, mobile, global).

% Passengers, patients, and communities downstream of energy, transport, chemical, and medical systems. They carry the casualty and economic cost whenever decayed readiness meets a real event, have no seat in the committees that set rehearsal requirements, and cannot opt out of dependence on the systems themselves — only choose among providers facing the same readiness problem.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, exposed_public, payer,
    powerless, generational, trapped, global).

% Operators whose entire preparation took place in simulators and tabletop exercises during long accident-free stretches. They enter live events having rehearsed procedures many times but never having borne real consequences; changing employers does not change the composition of their experience, and the gap surfaces only under genuine conditions.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_raised_operator_cohorts, payer,
    moderate, biographical, constrained, global).

% Researchers in high-reliability organizing, naturalistic decision-making, and training science who study whether rehearsal preserves performance under real stress. They produce the retention curves, after-action datasets, and theoretical frameworks that every camp in the training-policy dispute cites, and they hold no operational stake in the outcome.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hro_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The simulation-reliant preparation regime lets large cohorts of safety-critical operators rehearse rare, dangerous procedures repeatedly, at scale, without harming anyone — solving the problem that genuine catastrophes are too rare, too dangerous, and too expensive to train on directly.
% TRANSFER_FUNCTION: Moves quiet-period training budgets from operating firms to simulation vendors and internal training departments; and, on this reading's account, moves operational safety margin from the future to the present — competence debt issued against the next real event — while concentrating unrepeatable experiential knowledge in a shrinking veteran cohort instead of diffusing it through the workforce.
% ABSENT_VOICES: Those harmed in events where preparedness gaps surfaced are dead, injured, or dispersed and cannot testify in training-policy settings; the exposed public has no seat in the standards committees that set rehearsal requirements; simulation engineers and fidelity researchers are heard only as vendors, not as parties to what counts as adequate preparation.
% DISAPPEARANCE_RATIONALE: If real-event exposure were suddenly unnecessary — if rehearsal alone preserved full catastrophe-response competence — training economics would invert overnight: veteran experience premiums would evaporate, catastrophe-seasoned firms would lose a credential rivals cannot buy, the failure-analysis industry would lose its demand engine, and standards bodies would redirect mandates from simulator hours toward whatever cheaper regimen provably sufficed.
% FOUNDING_PROBLEM: How to give large cohorts of safety-critical operators rehearsal access to rare, lethal events without waiting for or causing real ones — the ethical and economic impossibility of on-the-job catastrophe training.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards in aviation, chemical, and nuclear domains, along with insurer loss data, attest from outside the beneficiary set that preparedness gaps surface at real events after long quiet periods; high-reliability and naturalistic-decision research corroborates partial decay of tacit and stress-response capacity. No source outside the veteran-practitioner tradition attests the stronger categorical claim that simulation is never sufficient — that portion rests on the authority of those whose standing depends on it.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66 at interval end) is assessed on the standing arrangement — simulation-reliant competence maintenance during catastrophe-free periods — as this reading sees it: readiness erodes invisibly while budgets book the savings, and the deficit is paid at the next real event in casualties and losses. Suppression (0.50) is structural rather than coercive: liability rules and regulatory prohibitions bar deliberately staging real catastrophes for practice, and quiet-period budget pressure crowds out expensive high-fidelity alternatives, but no actor enforces the regularity itself — it self-executes. Theater (0.55) reflects the share of preparation activity that is performative: scheduled scenarios with known outcomes, anniversary drills, and lessons-learned rituals that rehearse the appearance of readiness. Accessibility collapse (0.72) is high but short of natural-law completeness because workable substitutes persist at the fidelity frontier; resistance (0.60) is substantial — a simulation industry, a training-technology research base, and standards bodies all actively bet against the categorical claim. Coordination type identity_coordination: the regime's primary function is maintaining the profession's qualified-operator boundary — a shared, auditable baseline of who counts as ready; its characteristic failure is boundary erosion (credentialed but not competent), and the identity framing must not be allowed to excuse the extraction this reading measures. The measurement series share one eleven-point grid (units are years; t0 approximates 1975, t50 approximates 2025) and trace roughly two and a half sawtooth cycles: extraction and theater climb through long quiet stretches, reset sharply when a real event forces reinvestment in realism, then resume climbing. The oscillation is not noise — intermittent reinforcement is part of the mechanism, since each reset repurchases the same lesson at full price, and the secular envelope of both series rises as quiet intervals lengthen. No suppression_requirement series is authored: enforcement of the regularity is nil and static, so the scalar in base_properties carries the whole picture.
 *
 * PERSPECTIVAL GAP:
 *   From the exposed public's seat the arrangement is a silent erosion they cannot observe until it kills; from the veteran's seat it is a meritocracy of scar tissue that rightly prices experience; from the seasoned firm's seat it is earned authority; from the regulator's seat it is a compliant, auditable training regime. Same structure, four different constraints as experienced. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put live_event_veteran_operators, catastrophe_experienced_firms, and failure_analysis_consultancy_sector near the subsidized pole: each converts the unreplicability of lived catastrophe into standing, credential, or fee income. Victim declarations put exposed_public (trapped, powerless) near the full-target pole and simulation_raised_operator_cohorts (constrained) close behind. Two overrides correct the derivation: veterans are identity_locked, which normally marks a target, but here the identity fusion binds them to the constraint's continuation — their authority exists only while live experience stays irreplaceable — so d is overridden down to 0.12; training_standards_regulators declare no beneficiary or victim relation, so the canonical fallback would misplace them, and they are overridden to 0.55 — mildly target-side because their legitimacy absorbs the damage when certified readiness fails a real event.
 *
 * MANDATROPHY ANALYSIS:
 *   The misclassification risk here runs in both directions. Reading a possible false summit as mountain would naturalize a contestable claim and immunize it from revision — precisely the service it renders to seats whose standing depends on lived-catastrophe scarcity; the false-summit path plus the natural_law_vs_constructed_limit omega keep that question open instead of settling it by assertion. Reading a genuine limit as pure extraction would send policy hunting for an extractor to remove when no extractor exists, diverting effort from the only real responses — budgeting for periodic live exposure or pushing the fidelity frontier. On the genealogy side the founding problem (safe rehearsal at scale) is contested rather than dead, and the arrangement demonstrably rearranges the world if it vanishes, so no zombie condition is available: whatever else this arrangement is, it is not maintained out of habit alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_limit,
    'Is the regularity this reading asserts — that genuine catastrophe-response competence decays without real events and cannot be rebuilt synthetically — a genuine psychological and organizational limit, or a constructed regularity whose naturalness serves identifiable beneficiaries?',
    'Pre-registered, evaluator-blinded longitudinal comparison of retention curves for live-event-exposed versus highest-fidelity simulation-trained cohorts, run as an adversarial collaboration between high-reliability traditionalists and simulation vendors.',
    'If constructed, the false-summit path opens: beneficiary-backed naturalization of a contestable claim, reclassification away from mountain toward an enforced extraction arrangement. If genuine, the constraint certifies as a natural limit and safety policy must budget for periodic real-event exposure rather than simulation investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_limit, empirical, 'Whether the asserted competence law is natural or interest-serving construction.').

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the catastrophe_necessity_reading of kernel catastrophe_proxy_sufficiency; how would the sibling readings (simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading, simulation_fidelity_threshold) change the structural picture?',
    'Cross-fidelity retention benchmarking plus explicit adjudication of what counts as genuine competence — procedural execution versus adaptive performance under novelty.',
    'Proxy-sufficiency collapses measured extraction toward zero; the hybrid reading confines decay to tacit and stress-response domains on generational timescales; the fidelity-threshold reading makes extraction fall as technology crosses the threshold — each sibling yields a different victim set and a different epsilon for the same training regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading among four of the catastrophe-proxy kernel.').

omega_variable(
    goalpost_falsifiability_risk,
    'Can the categorical insufficiency claim be falsified, or does every simulation success get discounted as not a real catastrophe, moving the goalposts?',
    'Pre-commitment to operational sufficiency criteria — blinded crisis-performance parity between live-veteran and high-fidelity-simulation cohorts — before outcome data are collected.',
    'Without pre-commitment the constraint''s epsilon cannot stabilize and classification oscillates between natural law and unfalsifiable belief; with pre-commitment the claim joins ordinary empirical adjudication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(goalpost_falsifiability_risk, conceptual, 'Falsifiability of the categorical insufficiency claim.').

omega_variable(
    survivorship_bias_in_decay_evidence,
    'Does the evidence base for competence decay systematically undercount, because organizations whose decay produced fatal outcomes stop existing to report it?',
    'Industry-panel reconstruction including dissolved operators, insurer loss runs, and regulator enforcement archives rather than surviving-organization surveys.',
    'True decay-driven extraction exceeds measured values; this reading''s epsilon is a lower bound if survivorship bias is material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_decay_evidence, empirical, 'Survivorship bias in the decay evidence base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_necessity_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cat_necessity_tr_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cat_necessity_tr_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(cat_necessity_tr_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(cat_necessity_tr_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cat_necessity_tr_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(cat_necessity_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cat_necessity_tr_t35, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 35, 0.32).
narrative_ontology:measurement(cat_necessity_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(cat_necessity_tr_t45, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 45, 0.51).
narrative_ontology:measurement(cat_necessity_tr_t50, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(cat_necessity_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cat_necessity_be_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(cat_necessity_be_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(cat_necessity_be_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(cat_necessity_be_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cat_necessity_be_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(cat_necessity_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(cat_necessity_be_t35, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 35, 0.47).
narrative_ontology:measurement(cat_necessity_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cat_necessity_be_t45, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 45, 0.64).
narrative_ontology:measurement(cat_necessity_be_t50, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 50, 0.66).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'simulation versus real catastrophe for competence maintenance' into four epsilon-invariant claims: categorical necessity (this file, mountain-claimed, epsilon approximately 0.66 on the simulation-reliant arrangement), categorical sufficiency (proxy reading, epsilon near zero), partial generational decay (hybrid reading, intermediate epsilon confined to tacit and stress-response domains), and technology-dependent sufficiency (threshold reading, epsilon falling in fidelity). The necessity reading is the traditional position from which the other three depart; each sibling constrains what this reading's categorical claim can absorb without collapsing into it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, moderate, 0.12).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
