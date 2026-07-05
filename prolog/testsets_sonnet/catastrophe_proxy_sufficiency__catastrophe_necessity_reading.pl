% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Catastrophe-Necessity Reading: Only Real Disaster Maintains Genuine Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   `catastrophe_proxy_sufficiency`: the claim that only actual catastrophic
 *   events can generate the irreducible stress and uncertainty conditions
 *   necessary to produce genuine operator competence, and that no simulation,
 *   regardless of fidelity, is categorically capable of substituting for
 *   this. Under this reading the constraint behaves as a Mountain: it is
 *   presented as a fixed psychological/physiological limit on what rehearsed,
 *   anticipated, consequence-bounded exercises can produce in a human nervous
 *   system and organizational culture, independent of who defends it. The
 *   declared victim is not a person but a structural quantity — operational
 *   safety margins, which this reading holds decay in any catastrophe-free
 *   interval no matter the simulation investment. Two beneficiary groups are
 *   named (veteran disaster-experienced operators, post-incident
 *   investigators) because their institutional standing is elevated by
 *   acceptance of this reading, which is why an omega on
 *   natural-law-vs-constructed status is required by the FSM gate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe-Necessity Reading: Only Real Disaster Maintains Genuine Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '543d9b24-ea63-4561-b02b-557b82fa6672').
narrative_ontology:cs_kernel_codification('543d9b24-ea63-4561-b02b-557b82fa6672', distributed).
narrative_ontology:cs_authority_grounding('543d9b24-ea63-4561-b02b-557b82fa6672', practice).
narrative_ontology:cs_interpretation_layer_present('543d9b24-ea63-4561-b02b-557b82fa6672').
narrative_ontology:cs_reading_relation('543d9b24-ea63-4561-b02b-557b82fa6672', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('543d9b24-ea63-4561-b02b-557b82fa6672', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('543d9b24-ea63-4561-b02b-557b82fa6672', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('543d9b24-ea63-4561-b02b-557b82fa6672', foundational, catastrophe_stress_categorically_irreplicable).
narrative_ontology:cs_axiom_status(catastrophe_stress_categorically_irreplicable, holdable).
narrative_ontology:cs_axiom_grounding('543d9b24-ea63-4561-b02b-557b82fa6672', catastrophe_stress_categorically_irreplicable, empirically_contingent).
narrative_ontology:cs_axiom('543d9b24-ea63-4561-b02b-557b82fa6672', secondary, real_consequence_irreversibility_is_the_active_ingredient).
narrative_ontology:cs_axiom_status(real_consequence_irreversibility_is_the_active_ingredient, holdable).
narrative_ontology:cs_axiom_grounding('543d9b24-ea63-4561-b02b-557b82fa6672', real_consequence_irreversibility_is_the_active_ingredient, empirically_contingent).
narrative_ontology:cs_reference_frame('543d9b24-ea63-4561-b02b-557b82fa6672', post_incident_disaster_derived_competence_standard).
narrative_ontology:cs_drift_state('543d9b24-ea63-4561-b02b-557b82fa6672', contemporary_high_fidelity_simulation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('543d9b24-ea63-4561-b02b-557b82fa6672', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_incident_safety_investigators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_operators_with_disaster_experience).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, frontline_operators_in_catastrophe_free_intervals).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, irreducible_stress_uncertainty_thesis).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_categorical_insufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The aggregate buffer of organizational competence and error-tolerance that exists between normal operations and catastrophic failure. Under this reading, this buffer decays structurally during any catastrophe-free interval no matter how much simulation is run, because simulation cannot replicate the specific phenomenology (irreversibility, personal risk, unbounded consequence) that produces genuine skill retention. It has no advocate of its own; it is simply what erodes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).

% Operators who lived through an actual catastrophic event (a real reactor excursion, a real platform blowout, a real mass-casualty event) and carry tacit, embodied competence that this reading holds as categorically superior to simulator-trained competence. Their status, authority, and internal credibility within the organization derive from having 'been there' — this reading elevates their experience to a structural requirement rather than a contingent advantage.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_operators_with_disaster_experience, beneficiary,
    moderate, biographical, constrained, national).

% Boards and agencies that investigate real disasters and issue findings. Under this reading, their institutional relevance is affirmed as the primary legitimate source of new safety knowledge — since simulation is held insufficient, only real events generate the raw material investigators analyze, which secures their continued institutional necessity and funding.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_incident_safety_investigators, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_incident_safety_investigators, observer).

% Firms building high-fidelity simulators, digital twins, and VR training systems whose entire commercial premise this reading undermines. They are not consulted in the framing of this claim and have no seat in how competence-sufficiency debates are adjudicated within safety-critical industries, despite bearing the commercial consequence of the claim's acceptance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_and_training_vendors, excluded,
    organized, biographical, constrained, global).

% Workers currently operating safety-critical systems who have never experienced a real catastrophic event. Under this reading their competence is structurally treated as inferior and decaying regardless of training investment, which can affect promotion, trust, and psychological standing within the organization, even though they had no choice in whether a catastrophe occurred during their tenure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, frontline_operators_in_catastrophe_free_intervals, payer,
    powerless, biographical, trapped, national).

% Agencies that must decide whether to mandate or credit simulation-based competence certification. They observe the dispute between readings and must act despite the underlying uncertainty being irreducible in the near term.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the pure sense — this reading does not coordinate a joint enterprise; it asserts a physical/psychological limit on what artificial stress can substitute for. To the extent it 'coordinates' anything, it coordinates organizational deference toward veterans of real disasters and toward post-incident investigation as the sole legitimate knowledge source.
% TRANSFER_FUNCTION: No resource transfer in the economic sense. What moves is epistemic authority and institutional credibility: away from simulation-based training programs and toward disaster-experienced personnel and after-the-fact investigation bodies. The 'cost' borne is the erosion of confidence in currently-employed simulation-trained operators' competence.
% ABSENT_VOICES: Simulation and training vendors, and cognitive/behavioral scientists who study skill transfer under artificial stress conditions, are largely absent from the framing of this claim as stated. Practitioners who have successfully handled near-miss events under high-fidelity simulation are also underrepresented — their counter-evidence would complicate the categorical claim.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel vanished — if organizations stopped believing catastrophe exposure was categorically necessary — training budgets might shift heavily toward simulation fidelity investment, veteran operators might lose some structural deference, and post-incident investigation bodies might lose some monopoly on legitimate knowledge production. Whether this constitutes 'the world rearranging' or 'nothing changes because organizations already hedge with hybrid approaches' is exactly the dispute between this reading and its siblings — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The founding problem is the persistent post-disaster finding, across many high-reliability domains, that personnel who had extensive simulator training still froze, misjudged, or failed to adapt when a real catastrophic event diverged from any trained scenario — suggesting some component of competence is not producible by rehearsal of anticipated scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Some independent human-factors researchers outside any single industry's safety establishment attest that stress-response and tacit judgment under genuine irreversible risk differ measurably from simulator performance (citing physiological studies of cortisol response and decision degradation under simulated versus real threat). However, other independent researchers in the same field attest the gap is a fidelity/technology artifact, not a categorical one, and is closing as simulation technology improves — placing the founding-problem's continued validity in live, externally-attested dispute rather than settled by either side's own testimony.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

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
 *   Extractiveness is authored low (0.06-0.08) because this reading, taken on its own terms, does not describe anyone actively extracting rents from anyone else — it describes a claimed physical/psychological ceiling. Suppression is low (0.05) because nothing coercive enforces belief in the claim; it persists (if it persists) by argument and institutional habit, not by threat. Accessibility collapse is high (0.82): if the claim is true, there genuinely is no alternative pathway to the specific competence it describes — that is what a categorical necessity claim asserts. Resistance is moderate (0.35), reflecting the live empirical and institutional pushback from simulation-fidelity researchers and vendors, which is real but does not (within this reading) constitute evidence against the underlying claim, merely disagreement with it.
 *
 * PERSPECTIVAL GAP:
 *   Frontline operators without disaster experience and simulation vendors would compute this constraint very differently from veteran operators and investigators — the former experience the claim as a standing discount on their competence and market relevance; the latter experience it as vindication. The engine should register this asymmetry from the structural stakeholder data (power, exit, situation) even though no explicit transfer mechanism is authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators and post-incident investigators are named beneficiaries because acceptance of this reading elevates their epistemic and institutional standing without their having engineered that elevation for extraction — hence they sit near the beneficiary end of directionality without triggering a tangled_rope reading (no active enforcement, no named victim group of persons). Operational safety margins is the payer/victim, but it is a non-agent structural quantity (agent: false), so it does not feed directionality computation as if it collected anything; it is named only to keep the story's own claimed harm visible in the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem — real catastrophic events reveal competence gaps simulation training failed to catch — remains genuinely contested rather than resolved or dead, because both supporting and opposing empirical evidence exist in the human-factors literature and neither side of the kernel dispute has produced a decisive resolution. Declaring `founding_problem_status: contested` with corroboration from researchers outside the immediate beneficiary set prevents this reading from being mislabeled as either settled science or pure institutional self-interest; it is presented instead as an open, load-bearing empirical dispute that different organizational actors resolve differently depending on incentive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_limit_vs_institutional_convenience,
    'Is the claimed impossibility of simulation substituting for real catastrophic stress a genuine, fixed feature of human psychophysiology and organizational learning, or is it a convenient belief that elevates the standing of disaster-experienced veterans and post-incident investigation bodies regardless of its truth?',
    'Longitudinal comparison of operator performance in real catastrophic events, stratified by simulator-only training versus real-disaster-exposed training, controlling for simulator fidelity generation — if performance gaps persist even against maximally advanced simulators, the natural-limit reading gains support; if gaps close with fidelity, the institutional-convenience reading gains support.',
    'If resolved toward institutional convenience, this constraint reclassifies away from Mountain toward a constructed constraint benefiting identifiable institutional actors (likely tangled_rope or piton), since declared beneficiaries already exist in the structural data — this is precisely the false-summit pattern the schema''s FSM gate is designed to surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_institutional_convenience, empirical, 'Whether the catastrophe-necessity claim is a genuine natural limit or a self-serving institutional narrative.').

omega_variable(
    kernel_reading_disambiguation,
    'This story is one reading (`catastrophe_necessity_reading`) of the contested kernel `catastrophe_proxy_sufficiency`. The sibling readings — `simulation_as_proxy_catastrophe_reading` (simulation is fully sufficient indefinitely), `hybrid_degradation_reading` (procedural competence holds under simulation but tacit/stress competence degrades generationally), and `simulation_fidelity_threshold` (sufficiency is a technology-dependent threshold, not categorical) — instantiate structurally distinct constraints with different ε values, different beneficiary/victim sets, and in the fidelity-threshold and proxy-sufficiency cases, likely different claimed_types (rope or tangled_rope rather than mountain). Which reading should govern actual training-investment policy in a given safety-critical organization?',
    'Domain-specific empirical review per industry (aviation, nuclear, offshore drilling, surgery) comparing incident outcomes against training regime, since the correct reading may differ by domain and technology maturity rather than holding universally across all catastrophe-relevant institutions.',
    'Adopting the necessity reading versus the proxy-sufficiency reading has direct budgetary and personnel-credibility consequences — training investment flows very differently depending on which reading an organization''s safety culture internalizes as true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Location of the kernel dispute across four structurally distinct sibling readings, each a separate constraint.').

omega_variable(
    safety_margin_measurability,
    'Can ''operational safety margin decay'' during catastrophe-free intervals actually be measured directly, or is it inferred backward from the fact that a later catastrophe occurred (making the claim partly unfalsifiable / confirmation-biased)?',
    'Prospective measurement of leading safety indicators (near-miss reporting quality, procedural drift, simulator performance variance) during long catastrophe-free intervals, compared against eventual outcomes, rather than only retrospective post-incident attribution.',
    'If margin decay cannot be measured prospectively and is only inferred after the fact, the victim declaration (operational_safety_margins) rests on a weaker empirical foundation than the reading assumes, which would weaken the mountain claim''s accessibility_collapse justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_margin_measurability, empirical, 'Whether the claimed victim (decaying safety margins) is independently measurable or an artifact of retrospective attribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 16, 0.07).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the kernel `catastrophe_proxy_sufficiency`. Each reading is authored as its own ε-invariant constraint with its own claimed_type, beneficiaries/victims, and metrics, per the ε-invariance principle: measuring 'competence sufficiency' via the necessity-reading observable (does simulation categorically fail?) yields a very different ε and type (mountain, near-zero extraction) than measuring it via the proxy-sufficiency observable (is simulation being oversold as fully sufficient to cut training costs?), which is expected to read closer to rope or tangled_rope with real beneficiaries (organizations economizing on catastrophe-adjacent training risk) and real victims (operators whose competence gaps go undetected). The hybrid_degradation and fidelity_threshold readings are expected to occupy intermediate structural positions. All four should be generated as separate files linked via this field, not as one story with a measurement-basis parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
