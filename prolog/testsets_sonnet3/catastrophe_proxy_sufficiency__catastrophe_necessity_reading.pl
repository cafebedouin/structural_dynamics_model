% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Catastrophe Necessity: Irreducible Stress Cannot Be Simulated
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'catastrophe necessity' reading of a
 *   contested kernel about competence maintenance in high-reliability
 *   organizations. Under this reading, genuine catastrophic events supply a
 *   form of irreducible stress, consequence-reality, and outcome-uncertainty
 *   that no simulation — however sophisticated — can reproduce, and that this
 *   specific ingredient is structurally necessary (not merely helpful) for
 *   maintaining true operational competence. The reading treats this as
 *   approaching a psychological/physical limit: a mountain, not a policy
 *   choice. The claimed victim is diffuse and abstract — 'operational safety
 *   margins' as a stock that quietly erodes during long catastrophe-free
 *   intervals, especially if organizations mistakenly believe
 *   simulation-based training is fully protective. This is a
 *   mountain-with-beneficiaries (FSM candidate): veteran operators and
 *   investigation bodies gain institutional standing and authority from a
 *   reading that could also, in a less charitable framing, be partly
 *   self-serving mythology about the irreplaceability of lived catastrophe.
 *   The omega variables route this ambiguity to its proper place rather than
 *   resolving it here.
 *
 * KEY AGENTS:
 *   - veteran_frontline_operators: primary beneficiary of the reading's legitimacy claim (moderate/constrained)
 *   - incident_investigation_bodies: institutional beneficiary, agenda-setting authority reinforced by the reading (institutional/analytical)
 *   - operational_safety_margins: abstract victim — the stock that decays if the reading is true and unaddressed (powerless/trapped)
 *   - simulation_program_administrators: excluded voice whose professional claim this reading does not incorporate (organized/constrained)
 *   - regulators_and_licensing_bodies: analytical observer forced to act under irreducible uncertainty about which reading is correct (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.15).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.1).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity: Irreducible Stress Cannot Be Simulated").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e5ee3caf-044f-4589-ac8d-b69910ff0aff').
narrative_ontology:cs_kernel_codification('e5ee3caf-044f-4589-ac8d-b69910ff0aff', distributed).
narrative_ontology:cs_authority_grounding('e5ee3caf-044f-4589-ac8d-b69910ff0aff', practice).
narrative_ontology:cs_interpretation_layer_present('e5ee3caf-044f-4589-ac8d-b69910ff0aff').
narrative_ontology:cs_reading_relation('e5ee3caf-044f-4589-ac8d-b69910ff0aff', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('e5ee3caf-044f-4589-ac8d-b69910ff0aff', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5ee3caf-044f-4589-ac8d-b69910ff0aff', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('e5ee3caf-044f-4589-ac8d-b69910ff0aff', foundational, consequence_reality_is_categorically_irreplaceable).
narrative_ontology:cs_axiom_status(consequence_reality_is_categorically_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('e5ee3caf-044f-4589-ac8d-b69910ff0aff', consequence_reality_is_categorically_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('e5ee3caf-044f-4589-ac8d-b69910ff0aff', secondary, veteran_incident_experience_confers_nontransferable_competence).
narrative_ontology:cs_axiom_status(veteran_incident_experience_confers_nontransferable_competence, holdable).
narrative_ontology:cs_axiom_grounding('e5ee3caf-044f-4589-ac8d-b69910ff0aff', veteran_incident_experience_confers_nontransferable_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('e5ee3caf-044f-4589-ac8d-b69910ff0aff', lived_catastrophe_as_sole_competence_substrate).
narrative_ontology:cs_drift_state('e5ee3caf-044f-4589-ac8d-b69910ff0aff', post_high_fidelity_simulation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e5ee3caf-044f-4589-ac8d-b69910ff0aff', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, psychological_realism_irreproducibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operators who lived through an actual catastrophic event (a reactor scram, a refinery fire, a mass-casualty response) carry a durable calibration of risk perception, stress tolerance, and improvisational judgment that colleagues who only trained in simulators visibly lack under real pressure. Their standing within the organization, their authority in post-incident debriefs, and their promotion trajectories are reinforced by this reading — it validates the value of what they alone possess.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_frontline_operators, beneficiary,
    moderate, biographical, constrained, national).

% Bodies such as safety boards and root-cause review panels find their institutional mandate reinforced by a reading that treats catastrophes as epistemically irreplaceable: every incident becomes a uniquely valuable data point that no simulation could have generated, justifying continued investigative authority, budget, and the doctrine that near-misses and drills are structurally second-tier evidence.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, incident_investigation_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, incident_investigation_bodies, agenda_setter).

% The abstract but real stock of organizational competence — the workforce's aggregate ability to respond correctly under true stress — is what erodes during long catastrophe-free intervals if the necessity reading is correct: no amount of simulation investment can substitute, so margins quietly decay while institutions may believe (falsely, per rival readings) that their simulation programs are protective. This 'agent' bears the cost of being wrong in either direction: over-investing in simulation that cannot work, or under-investing because leadership believes simulation suffices.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, civilizational, trapped, global).

% The engineers and training officers who design, fund, and certify simulator programs have professional and budgetary stakes in simulation being sufficient. Under the necessity reading their entire function is recast as producing at best a partial, ultimately inadequate substitute — a conclusion they would contest but which this reading does not incorporate; they appear here as excluded rather than participating in this reading's own account of itself.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_program_administrators, excluded,
    organized, biographical, constrained, national).

% Set minimum training and certification requirements based on some working theory of what maintains competence. If they adopt the necessity reading they must grapple with a policy conclusion they cannot act on directly — regulators cannot ethically manufacture catastrophes — leaving them to decide how much weight to give tacit degradation risk in licensing renewal cycles built around simulation-based recertification.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, regulators_and_licensing_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination arrangement between parties but a claimed empirical/psychological limit: that irreducible stress and consequence-uncertainty of a kind only genuine catastrophe supplies is a necessary substrate for competence formation, and no designed proxy reproduces it.
% TRANSFER_FUNCTION: If true, the claim transfers epistemic authority from simulation designers and administrators toward veterans of real incidents and toward investigation bodies that study those incidents, without moving money or labor directly — the transfer is of legitimacy and institutional deference.
% ABSENT_VOICES: Simulation program administrators and cognitive-training researchers who argue fidelity can approach or cross a functional threshold are structurally outside this reading's own account of itself — this reading does not model their objection as correct, only as external to what it asserts.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel were simply discarded, organizations that currently defer to catastrophe-derived competence and discount simulation would begin trusting simulation-based certification as fully sufficient — a real behavioral and budgetary shift. Whether the underlying physical/psychological limit itself would disappear (i.e., whether competence would actually degrade) is precisely the empirical question the reading asserts an answer to and that rival readings dispute; the verdict is contested between the reading's proponents (world would rearrange badly, margins would erode) and simulation-sufficiency proponents (nothing real changes).
% FOUNDING_PROBLEM: High-reliability organizations (nuclear, aviation, emergency medicine, military command) observed repeatedly that personnel who had lived through genuine catastrophic events performed qualitatively differently under real pressure than equally well-drilled personnel who had only trained in simulators — motivating a claim about what kind of experience actually builds competence.
% FOUNDING_PROBLEM_CORROBORATION: Some psychological and human-factors researchers outside the veteran-operator and investigation-body beneficiary groups (e.g., stress-inoculation researchers studying combat and disaster responders) corroborate that consequence-reality and irreversibility contribute something simulators historically have not replicated. However, other researchers in the same outside fields report simulation fidelity has closed much of this gap with modern immersive and consequence-weighted training, so the corroboration is split rather than unanimous, and no fully disinterested resolution exists.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.15, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.15) because, taken on its own terms, this reading describes a genuine limit rather than a rent-collection arrangement — nobody profits from catastrophes happening, and the reading does not require anyone to suppress alternatives to extract value. Suppression is low (0.1) for the same reason: the claim is not maintained by coercion but by asserted psychological/physical necessity. Accessibility collapse is high (0.8): if the claim is true, there genuinely is no substitute path to the competence in question, which is precisely the mountain signature. Resistance is moderate (0.35) because the claim is empirically contested by simulation researchers and administrators, unlike an uncontested physical law — this is a claimed mountain, not a certified one, and the resistance metric should reflect that contestation honestly. Theater ratio rises modestly over the interval (0.25 to 0.4) reflecting a plausible dynamic: as the necessity claim becomes institutionally entrenched, some of the deference paid to 'catastrophe-tested' judgment in organizational ritual (seniority privileges, storytelling in briefings, veto power in reviews) outpaces its demonstrated predictive value, without this drift being severe enough to invalidate the underlying claim.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran-operator and investigation-body seats, this reading validates real, hard-won expertise and a real epistemic limit of designed training. From the simulation-administrator seat (excluded from this reading's own account), the same claim reads as an unfalsifiable status claim that can never be contradicted by simulation performance data, because any simulator improvement can always be met with 'still not the real thing.' The engine's per-seat computation should reflect that the beneficiary seats see something close to a mountain while an excluded, differently-positioned seat would see something closer to an unverifiable status-preserving doctrine — that divergence is exactly what the kernel-reading decomposition is for.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (veteran operators, investigation bodies) sit near the low-extraction end because the reading enhances their standing without requiring a transfer from an identifiable payer in the ordinary economic sense. The payer here, operational_safety_margins, is not a self-interested agent but a systemic stock; it is declared powerless and trapped because if the necessity claim is correct, no design choice by any actor can substitute for the missing ingredient — the decay proceeds regardless of intent. This is a mountain-shaped directionality: the 'cost' is a structural consequence of the claimed limit, not a transfer captured by any party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (observed performance gaps between catastrophe-experienced and simulation-only personnel) is authored as contested-status rather than resolved, because whether that gap reflects an irreducible limit (this reading) or a closeable fidelity gap (sibling readings) is exactly what remains open. Treating this as settled mandatrophy in either direction would be premature; the six-questions corroboration deliberately reports split expert testimony rather than manufacturing consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_limit_vs_status_mythology,
    'Is the catastrophe-necessity claim a genuine, non-negotiable psychological/physiological limit on competence formation, or is it partly a status-preserving belief that benefits veteran operators and investigation bodies by making their experience irreplaceable by construction?',
    'Controlled longitudinal comparison of performance decay curves between simulation-only cohorts and catastrophe-experienced cohorts across multiple domains and multiple decades, ideally with blind evaluation of real emergency response to avoid halo-effect scoring of ''veteran'' status.',
    'If genuine limit: the mountain classification holds and organizations should budget for irreducible risk during long catastrophe-free periods. If partly status mythology: the claim functions as a false summit that entrenches veteran authority and investigation-body mandate beyond what the evidence supports, and the reading would better classify as a tangled_rope (real partial coordination function around genuine tacit-knowledge transfer, mixed with self-serving authority preservation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_limit_vs_status_mythology, empirical, 'Whether the necessity claim is a genuine limit or partly self-serving status mythology for its beneficiaries.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given four live readings of the same kernel (necessity, proxy-sufficiency, hybrid-degradation, fidelity-threshold), what evidence or framing led to selecting the strong categorical necessity reading rather than the fidelity-threshold reading, which explains the same observed veteran/simulator performance gap without asserting a categorical, technology-independent limit?',
    'Track whether reported performance gaps shrink as simulator fidelity (haptic realism, consequence-weighting, physiological stress induction) improves over successive technology generations; a shrinking gap with improving fidelity would favor the threshold reading over the categorical necessity reading.',
    'If gaps shrink with fidelity, this reading (catastrophe_necessity_reading) is empirically disfavored relative to simulation_fidelity_threshold, and the mountain classification here would be a false summit relative to a more accurate technology-dependent framing. If gaps persist despite fidelity improvements, this reading is favored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the two most defensible readings (categorical necessity vs. fidelity threshold) better fits the evidence, and why this file selected the categorical framing.').

omega_variable(
    safety_margin_decay_measurability,
    'Can ''operational safety margin decay'' during catastrophe-free intervals actually be measured independently of after-the-fact rationalization following a new incident (i.e., is the victim group''s harm falsifiable in advance, or only narratable in hindsight)?',
    'Prospective competence audits (blind red-team drills with real consequence-weighted scoring) conducted at fixed intervals in catastrophe-free organizations, compared against organizations that experienced an intervening real incident.',
    'If margin decay is measurable prospectively and correlates with catastrophe-free duration, the victim declaration (operational_safety_margins) is empirically grounded. If margin decay claims only appear in retrospective post-incident narratives, the victim declaration risks being an artifact of hindsight bias rather than a real, trackable cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_margin_decay_measurability, empirical, 'Whether the claimed victim (decaying safety margins) is measurable in advance or only narrated after subsequent incidents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This is one of four sibling readings of the kernel 'catastrophe_proxy_sufficiency'. The kernel is the underlying empirical/psychological question of what maintains genuine operational competence over time. This reading (catastrophe_necessity_reading) claims a categorical, technology-independent limit — simulation cannot suffice regardless of fidelity — and authors a low-extraction mountain-shaped constraint with a diffuse structural victim. The sibling 'simulation_as_proxy_catastrophe_reading' takes the opposite categorical position (simulation is fully sufficient) and would author very different beneficiary/victim structure (likely favoring simulation-industry beneficiaries with veteran-authority claims as the contested cost). 'hybrid_degradation_reading' splits competence into procedural (simulation-maintainable) and tacit/stress-response (catastrophe-dependent) components, producing a more mixed classification. 'simulation_fidelity_threshold' rejects the categorical framing entirely, making sufficiency a continuous function of technology, which likely produces a scaffold-like or rope-like classification (transitional insufficiency, closing over time as fidelity improves) rather than a mountain. Each story keeps a single stable epsilon and is linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
