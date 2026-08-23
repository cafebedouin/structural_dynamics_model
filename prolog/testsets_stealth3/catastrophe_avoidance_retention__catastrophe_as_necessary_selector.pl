% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe-Necessity Doctrine in High-Hazard Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-hazard industries (nuclear, aviation, chemical processing,
 *   offshore energy), a governing doctrine holds that only actual
 *   catastrophic events supply the chaos, mortality salience, and
 *   organizational trauma required to keep rare-event competence alive;
 *   everything synthetic is presumed to breed false confidence. The doctrine
 *   began as a description of a real problem — unused emergency skills decay,
 *   and classroom-trained crews historically underperformed event-hardened
 *   veterans — but over five decades it has migrated into a justification
 *   structure: training budgets are deferred because their payoff is
 *   unverifiable until an event arrives, authority concentrates in
 *   catastrophe veterans, and exposed populations absorb the readiness gap.
 *   This file instantiates ONE reading of the kernel
 *   catastrophe_avoidance_retention (Rule 1): the ε referent is the standing
 *   arrangement under contest — the industry-wide practice regime that
 *   allocates readiness investment and operational authority on the
 *   assumption that actual catastrophe is the irreplaceable teacher —
 *   assessed by this reading's own lights, not by the sibling readings'
 *   endorsed alternatives. The claim/metric gap is deliberate: the doctrine
 *   is CLAIMED by its holders as a discovered iron law approaching mountain
 *   status, while the authored metrics describe a substantially extractive,
 *   actively enforced hybrid — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - executive_budget_holders: Agenda-setter (institutional/arbitrage) — sets training and preparedness budgets, cites unverifiable simulation payoff to defer investment, captures the avoided cost directly
 *   - - catastrophe_veteran_senior_operators: Primary beneficiary with secondary agenda-setting role (organized/identity_locked) — collects authority rents from event-experienced standing while personally bearing the events' costs
 *   - - frontline_operations_staff: Primary target (moderate/constrained) — absorbs the gap between assumed and actual readiness when severe events arrive
 *   - - junior_engineers_and_trainees: Secondary target (powerless/constrained) — advancement gated on crisis judgment they cannot legitimately acquire outside a real event
 *   - - downstream_exposed_communities: Structural victim and excluded voice (powerless/trapped) — bear catastrophe consequences of decisions in which they hold no seat
 *   - - liability_insurers: Secondary beneficiary (institutional/arbitrage) — premium models stabilized by the assumption that severe events recur at some baseline rate
 *   - - safety_science_researchers: Analytical observer (analytical/analytical) — documents skill decay and simulator-transfer evidence; sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.64).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe-Necessity Doctrine in High-Hazard Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'cc7b216b-376d-48ec-bf37-8284f03ec993').
narrative_ontology:cs_kernel_codification('cc7b216b-376d-48ec-bf37-8284f03ec993', distributed).
narrative_ontology:cs_authority_grounding('cc7b216b-376d-48ec-bf37-8284f03ec993', practice).
narrative_ontology:cs_interpretation_layer_present('cc7b216b-376d-48ec-bf37-8284f03ec993').
narrative_ontology:cs_reading_relation('cc7b216b-376d-48ec-bf37-8284f03ec993', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('cc7b216b-376d-48ec-bf37-8284f03ec993', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('cc7b216b-376d-48ec-bf37-8284f03ec993', foundational, actual_catastrophe_necessary_for_competence).
narrative_ontology:cs_axiom_status(actual_catastrophe_necessary_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('cc7b216b-376d-48ec-bf37-8284f03ec993', actual_catastrophe_necessary_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('cc7b216b-376d-48ec-bf37-8284f03ec993', foundational, mortality_salience_irreplaceable_by_synthetic_stress).
narrative_ontology:cs_axiom_status(mortality_salience_irreplaceable_by_synthetic_stress, holdable).
narrative_ontology:cs_axiom_grounding('cc7b216b-376d-48ec-bf37-8284f03ec993', mortality_salience_irreplaceable_by_synthetic_stress, empirically_contingent).
narrative_ontology:cs_reference_frame('cc7b216b-376d-48ec-bf37-8284f03ec993', catastrophe_selection_competence_regime).
narrative_ontology:cs_drift_state('cc7b216b-376d-48ec-bf37-8284f03ec993', contemporary_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cc7b216b-376d-48ec-bf37-8284f03ec993', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_senior_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, liability_insurers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operations_staff).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, junior_engineers_and_trainees).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, downstream_exposed_communities).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_competence_decay_law).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, survivorship_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set annual training and preparedness budgets for high-hazard operations. Cite the impossibility of proving a prevented catastrophe to defer investment in drills and synthetic programs, directing funds toward response capacity and post-event investigation instead. Compensation cycles reward short-term cost control; if the deferral logic lost credibility they could redirect budgets within a fiscal year, and many hold board positions across multiple firms, giving them mobility beyond any single operator.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders, agenda_setter,
    institutional, biographical, arbitrage, global).

% Senior operators and incident commanders whose professional standing rests on having managed real emergencies. Staff review boards, mentor junior staff, and decide whose judgment counts in a crisis. Many carry lasting injuries or lost colleagues from the events that credentialed them; leaving the profession would mean discarding the identity and authority built on that experience, so they remain and defend the standard their standing depends on.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_senior_operators, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_senior_operators, agenda_setter).

% Control-room operators, pilots, and plant technicians who staff systems during rare severe events. Absorb the gap between assumed and actual readiness when an event arrives, executing procedures written for conditions their training never reproduced. Changing employer or sector does not remove exposure, since every high-hazard employer runs the same deferral logic.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operations_staff, payer,
    moderate, biographical, constrained, national).

% New hires whose advancement is gated on demonstrating crisis judgment they cannot legitimately acquire outside a real event. Inherit reduced drill hours and are evaluated against veterans' event-hardened baselines. Leaving means abandoning the profession they trained for; staying means waiting indefinitely for an event that may injure them before it promotes them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, junior_engineers_and_trainees, payer,
    powerless, biographical, constrained, global).

% Residents near chemical plants, flight paths, dams, and reactors who bear the consequences of readiness gaps they had no part in setting. Cannot relocate cheaply and hold no seat in the budget or credentialing decisions that determine their exposure; their consent is never sought because the arrangement treats their risk as the tuition of others' competence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, downstream_exposed_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, downstream_exposed_communities, excluded).

% Underwrite catastrophe risk across the sector. The assumption that severe events recur at some baseline rate stabilizes premium models and grounds resistance to zero-event mandates that would upend actuarial tables; they collect premiums sized to a world in which catastrophes continue.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Study how organizations retain rare-event competence: high-reliability organization research, resilience engineering, simulator fidelity and skill-decay studies. Publish evidence that structured near-miss analysis and high-realism drilling preserve skills; their findings are received by operators as academically interesting but operationally unproven.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_science_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, executive_budget_holders).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real problem: rare-event competence genuinely decays without reinforcement, and organizations need some account of how readiness is maintained and whose judgment to trust in a crisis. The doctrine coordinates expectations by designating event-experienced practitioners as the calibration standard and explaining why preparedness spending resists verification.
% TRANSFER_FUNCTION: Moves safety investment away from preparation and toward response capacity and post-event investigation; moves the risk of unpreparedness onto frontline staff, trainees, and exposed communities; moves authority, status, and career velocity toward practitioners with actual event exposure.
% ABSENT_VOICES: Exposed communities never consented to serving as the selection pressure and hold no seat in the budget or credentialing decisions that set their risk. Junior professionals cannot vote on the development regime that gates their careers. Simulation vendors and high-reliability-program advocates are heard but discounted as lacking operational credibility — their exclusion from the 'who counts as experienced' boundary is maintained by the very standard the doctrine enforces.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, training budgets would reflow toward simulation and near-miss programs, credentialing would shift toward demonstrated synthetic performance, insurers would reprice toward prevention rather than recurrence, and the veteran authority premium would erode — the industry's entire readiness economy reorganizes around whichever competence-maintenance account replaces it.
% FOUNDING_PROBLEM: Mid-twentieth-century high-hazard industries repeatedly observed classroom-trained crews failing in real emergencies while event-experienced veterans performed; the doctrine was built to explain that gap and to ration scarce training money toward response capacity and veteran retention rather than unverifiable synthetic preparation.
% FOUNDING_PROBLEM_CORROBORATION: The underlying decay phenomenon is corroborated from outside the benefiting parties: skill-decay studies, regulator accident investigations, and high-reliability field research all document that unused emergency skills fade and that event-experienced judgment is real. But the necessity framing — that nothing short of actual catastrophe suffices — is attested almost exclusively by the parties who benefit from deferring investment; no independent source attests the exclusivity claim, and simulator-transfer research actively disputes it. The corroboration split (phenomenon corroborated, exclusivity uncorroborated) is itself signal.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72 at interval end) because the doctrine converts a real epistemic problem — you cannot prove a prevented catastrophe — into a standing excuse for transferring readiness costs onto those exposed to the gap: deferred drill hours, gated careers, and accepted residual risk are all paid by others. Suppression (0.64) is structural rather than prohibition-based: alternatives are not banned but systematically discredited ('false confidence'), and credentialing gates enforce conformity to event-experienced standards. Theater ratio (0.51) reflects the growing share of post-event 'lessons learned' rituals, anniversary commemorations, and checkbox drills that perform vigilance without transferring rare-event skill — activity that has drifted from functional review toward institutional mourning-as-compliance. Accessibility collapse is low-to-moderate (0.40): high-reliability organization programs, high-fidelity simulators, and near-miss databases remain visible and adoptable, so alternatives have not collapsed. Resistance (0.60) is substantial and organized: the safety-science community, resilience engineering, and parts of the regulatory apparatus actively contest the necessity claim. The three measurement series share one time grid (points 0,10,20,30,40,50) so every metric is authored at every examined time point; trajectories are monotonic rather than cyclical — enforcement hardened gradually as peacetime lengthened, with no oscillation driving the dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the doctrine as prudent realism about the limits of training: from the budget office, unverifiable prevention spending is indistinguishable from waste, and the doctrine simply states that fact. The payer seats experience the same structure as unpaid exposure: their training was cut on a theory they never assented to, and they discover the shortfall only when the event arrives. The veteran seat computes as genuinely split — it collects authority rents from the doctrine while having personally paid the tuition (injury, bereavement, trauma) the doctrine romanticizes. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive budget holders derive near-beneficiary directionality: they collect the avoided training cost and hold arbitrage-grade exit (board seats across firms). Liability insurers similarly derive low d — the doctrine stabilizes the recurrence assumption their premium models price. Frontline staff, junior professionals, and downstream communities derive high-to-maximal target directionality, amplified by constrained or trapped exit: the public cannot relocate away from hazard proximity, and juniors cannot manufacture legitimate event experience. One override is declared: catastrophe_veteran_senior_operators hold the 'organized' power atom, and the derivation from their beneficiary declaration alone would place them near the beneficiary pole (d ~0.1); the override to 0.30 records that they are simultaneously collectors of authority rents and bearers of the events' personal costs — injured, bereaved, identity-fused — making their structural relationship genuinely mixed rather than purely extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine wears mountain clothing — it presents as an iron law of organizational nature ('inevitable decay', 'nothing substitutes for the real thing') — but it declares identifiable beneficiaries and victims, so it cannot certify as natural law; the false-summit signature applies. Tangled rope is the honest classification because BOTH components are structurally real: the coordination function (rare-event competence genuinely decays without reinforcement, and event-experienced judgment is genuinely valuable in crises) and the asymmetric extraction (deferred investment, gated careers, and accepted public risk flow through the same doctrine that explains the decay). Mandatrophy status: the founding problem (skill decay) is still live, so the mandate has not died — but the exclusivity thesis is precisely what the sibling readings contest. If a sibling regime proves sufficient, the doctrine degrades toward piton (ritualized lessons-learned maintenance with the learning function atrophied) or resolves entirely. The classification prevents mislabeling in both directions: calling this a mountain launders deferral as physics; calling it a snare erases the real learning problem it partially addresses and the real competence veterans carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_exclusivity_contest,
    'This constraint instantiates the catastrophe_as_necessary_selector reading of the catastrophe_avoidance_retention kernel; if either sibling reading (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) proved correct, how would this constraint''s structure change?',
    'Comparative longitudinal studies of industries operating under each regime: retention curves for rare-event skills under simulation-heavy versus event-calibrated versus hybrid learning regimes, scored against subsequent real-event performance.',
    'If a sibling regime sustains competence without actual catastrophes, this constraint''s coordination function collapses into pure deferral justification and its classification shifts toward snare or piton; if the siblings fail in practice, this reading regains quasi-natural-law standing and its extraction reads closer to irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity_contest, empirical, 'Committer-frame uncertainty: which reading of the kernel the evidence ultimately supports.').

omega_variable(
    constructed_vs_discovered_law,
    'Is ''only actual catastrophes maintain competence'' a discovered regularity of human skill retention, or a constructed doctrine that persists because it excuses deferred safety investment?',
    'Meta-analysis of skill-decay and simulator-transfer literature combined with natural experiments where firms adopted intensive high-realism drilling and subsequently faced real severe events.',
    'If constructed, the constraint is a false summit presenting as natural law — identifiable beneficiaries exist and false-summit reclassification applies; if discovered, part of its measured extraction is the irreducible price of genuine readiness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_discovered_law, conceptual, 'Natural-law versus constructed-doctrine ambiguity at the heart of the necessity claim.').

omega_variable(
    survivorship_bias_confound,
    'Does catastrophe experience cause veteran competence, or do catastrophes merely select for pre-existing competence by eliminating or sidelining everyone else?',
    'Cohort tracking comparing event survivors'' pre-event performance distributions against matched non-exposed cohorts, controlling for hiring selectivity.',
    'If selection dominates causation, the claim''s evidential foundation inverts — catastrophes filter rather than teach — and the constraint loses even its coordination-function cover, becoming pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_confound, empirical, 'Whether the veteran-performance gap is causal or a survivorship artifact.').

omega_variable(
    veteran_identity_fusion_mechanism,
    'Is veteran resistance to simulation-based competence standards driven by structural incentive (authority rents tied to event-experienced standing) or by internalized identity (self-concept constituted by having endured real events)?',
    'Longitudinal study of veterans who move into simulation-centric organizations: does opposition to synthetic standards persist once the authority rent disappears?',
    'If internalized, the constraint''s suppression outlives its incentive structure and reform requires identity-level change rather than policy change; if structural, compensation and credentialing redesign dissolves the resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(veteran_identity_fusion_mechanism, empirical, 'Structural versus internalized source of the doctrine''s enforcement personnel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.26).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.46).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 50, 0.51).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'catastrophes keep us sharp' decomposes, per the epsilon-invariance principle, into three structurally distinct claims about the shared kernel catastrophe_avoidance_retention. This story is the traditional/folk reading from which the other two emerged as challenges; its persistence lends rhetorical cover to deferral practices the sibling readings would dismantle, so its edges run downstream into both siblings. Each member carries its own epsilon, beneficiary/victim structure, and classification; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
