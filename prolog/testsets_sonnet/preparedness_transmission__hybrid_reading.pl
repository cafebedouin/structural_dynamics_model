% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness: Engineering Intact, Civilian Coordination Decayed
 *   domain: disaster risk management / institutional memory / civil defense
 *
 * SUMMARY:
 *   A regional hazard-preparedness system bundles two functionally separate
 *   coordination problems under one public narrative of 'preparedness':
 *   physical infrastructure resilience (seawalls, levees, retrofits, warning
 *   hardware) and civilian evacuation/shelter coordination (planning,
 *   drilling, interagency communication, public warning dissemination).
 *   Post-event after-action reports consistently show the engineering half
 *   performing to spec while the coordination half fails or improvises under
 *   stress. The single label 'preparedness' obscures that one subsystem is a
 *   well-maintained Rope-like coordination structure and the other has
 *   drifted toward Tangled Rope territory — nominally coordinating but now
 *   also quietly extracting legitimacy and funding away from the layer that
 *   actually needs it, while residents bear the risk of the gap.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness: Engineering Intact, Civilian Coordination Decayed").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster risk management / institutional memory / civil defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'aca532d4-511e-4a34-aee1-e93a9cca3a90').
narrative_ontology:cs_kernel_codification('aca532d4-511e-4a34-aee1-e93a9cca3a90', distributed).
narrative_ontology:cs_authority_grounding('aca532d4-511e-4a34-aee1-e93a9cca3a90', practice).
narrative_ontology:cs_interpretation_layer_present('aca532d4-511e-4a34-aee1-e93a9cca3a90').
narrative_ontology:cs_reading_relation('aca532d4-511e-4a34-aee1-e93a9cca3a90', preparedness_transmission__husk_reading, influences).
narrative_ontology:cs_reading_relation('aca532d4-511e-4a34-aee1-e93a9cca3a90', preparedness_transmission__competence_reading, influences).
narrative_ontology:cs_axiom('aca532d4-511e-4a34-aee1-e93a9cca3a90', foundational, preparedness_competence_is_domain_separable).
narrative_ontology:cs_axiom_status(preparedness_competence_is_domain_separable, holdable).
narrative_ontology:cs_axiom_grounding('aca532d4-511e-4a34-aee1-e93a9cca3a90', preparedness_competence_is_domain_separable, empirically_contingent).
narrative_ontology:cs_axiom('aca532d4-511e-4a34-aee1-e93a9cca3a90', secondary, differential_decay_requires_differential_remedy).
narrative_ontology:cs_axiom_status(differential_decay_requires_differential_remedy, holdable).
narrative_ontology:cs_axiom_grounding('aca532d4-511e-4a34-aee1-e93a9cca3a90', differential_decay_requires_differential_remedy, instrumental).
narrative_ontology:cs_reference_frame('aca532d4-511e-4a34-aee1-e93a9cca3a90', unified_dual_competence_standard).
narrative_ontology:cs_drift_state('aca532d4-511e-4a34-aee1-e93a9cca3a90', post_series_of_coordination_failures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aca532d4-511e-4a34-aee1-e93a9cca3a90', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_certifying_bodies).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, residents_in_hazard_zones).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, municipal_emergency_managers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civil_defense_coordination_offices).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, engineering_standards_are_load_bearing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, certify, and maintain seawalls, levees, retrofitted structures, and warning hardware to exacting codes; their competence is regularly re-validated by inspection regimes, professional licensure, and structural failure post-mortems. They receive continued funding, prestige, and regulatory authority premised on the overall preparedness system 'working.' Their domain performs largely as specified and they have every incentive to let that performance stand for the whole system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, engineering_agencies, agenda_setter).

% Issue compliance certificates for physical hazard infrastructure and audit engineering firms against codified standards. Their metrics track only the physical layer; they collect fees and legitimacy from certifying a preparedness posture that the public reasonably assumes includes functioning evacuation and coordination, which is outside their certification scope.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_certifying_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Nominally responsible for evacuation planning, public warning dissemination, shelter logistics, and interagency coordination during a hazard event. Budgets have shrunk relative to engineering programs; staff turnover is high, drills are infrequent or scripted, and institutional memory of how prior evacuations actually failed or succeeded has not been transmitted to current staff. They administer the coordination layer but lack the resources or authority to fix its atrophy, and they absorb blame when coordination fails despite functioning infrastructure.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_coordination_offices, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, civil_defense_coordination_offices, payer).

% Charged with executing evacuation and shelter operations at the point of contact with residents, using coordination protocols and training pipelines that have degraded generationally. They inherit plans written by people no longer employed, run drills with declining participation, and are held accountable for outcomes shaped by a coordination deficit they did not create and cannot resource their way out of.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, municipal_emergency_managers, payer,
    moderate, biographical, trapped, local).

% Live behind engineering that will very likely hold, but depend on evacuation orders, shelter routing, and real-time coordination that has not been meaningfully exercised in a generation. When an event exceeds the infrastructure's design threshold or requires coordinated response, they bear the consequences of a coordination failure invisible to them until the moment it matters. They have no way to independently audit which layer of preparedness is real.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, residents_in_hazard_zones, payer,
    powerless, immediate, trapped, local).

% Point to certified, engineered infrastructure as evidence of preparedness in budget cycles and campaigns, since it is legible and photogenic, while coordination-layer funding is easier to cut because its absence is invisible until an event. They are structurally excluded from the room where coordination-office staff would explain the actual erosion, because that testimony complicates a preparedness narrative that serves them politically.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, elected_officials, beneficiary,
    powerful, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, elected_officials, excluded).

% Study post-event after-action reports and find a consistent pattern: physical infrastructure performs to spec while evacuation timing, shelter routing, and interagency communication fail or improvise under stress. They document the stratification but their findings compete for attention against the more legible engineering-success narrative.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The system as a whole is meant to coordinate two distinct things: physical hazard resistance (engineering) and human movement/communication under stress (evacuation coordination). The engineering half genuinely solves its coordination problem — shared codes, inspection cycles, and licensure keep structural competence current across generations of engineers.
% TRANSFER_FUNCTION: Funding, political credit, and legitimacy flow toward the engineering/certification apparatus, which produces visible, auditable artifacts (structures, certificates). Attention, budget, and institutional memory flow away from civilian coordination offices, whose failures are invisible until a live event, at which point the cost of the coordination deficit is transferred onto trapped residents and onto municipal managers who inherit undocumented, unexercised plans.
% ABSENT_VOICES: Coordination-office staff and disaster researchers who could testify to the specific erosion of evacuation competence are structurally absent from budget and political conversations, which are dominated by the more legible, better-funded engineering side. Residents in hazard zones have no seat at all and cannot distinguish which layer of the preparedness claim is real.
% DISAPPEARANCE_RATIONALE: If the entire preparedness apparatus vanished, engineering standards and inspection regimes would need urgent replacement — the world clearly rearranges there. But if only the coordination layer's current (already-degraded) state vanished, many parties dispute whether much would actually change in practice, since drills are already largely unexercised; the contest is over whether the coordination layer currently does enough real work to count as something that would be missed.
% FOUNDING_PROBLEM: The dual system was built to ensure that, when a hazard event occurs, structures survive the physical stress AND people are moved out of harm's way and sheltered/coordinated in time. Both halves were originally treated as equally live operational capacities requiring regular exercise.
% FOUNDING_PROBLEM_CORROBORATION: Disaster researchers, publishing independent after-action analyses outside the certifying and coordination institutions, corroborate that the engineering half of the founding problem remains live and well-served while the coordination half has quietly gone dead in practice despite continued nominal drills; civil defense coordination office staff privately corroborate the same erosion, but engineering agencies and elected officials, who benefit from the undifferentiated 'we are prepared' narrative, do not corroborate a bifurcated reading and instead treat the whole system as uniformly functional.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the extraction here is indirect: no party directly profits from residents' exposure, but funding, political credit, and institutional attention are systematically diverted from the coordination layer toward the more legible engineering layer, and that diversion has a real victim (residents, municipal managers) and a real (if diffuse) beneficiary structure (engineering agencies, certifying bodies, elected officials who get to claim 'preparedness' cheaply). Theater ratio rises steadily (0.12 to 0.48) as coordination-layer drills continue nominally while their operational content hollows out — this is the memorial-ritual dynamic the husk_reading sibling names in full, but here it applies only to HALF the system, which is exactly the structural delta this reading asserts. Suppression is moderate and rising (0.20 to 0.38): no one is actively coerced into silence, but budget structures and political incentives structurally suppress attention to the coordination deficit until an event forces it into visibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering agencies and certifying bodies sit near the beneficiary end: they receive continued authority and funding premised on overall system credibility while their own subsystem genuinely performs, so they are net beneficiaries of the undifferentiated 'preparedness' label even though their own work is not the extractive part. Civil defense coordination offices are a mixed seat — they administer the failing subsystem (agenda_setter role) but are also structurally starved payers, which is why they carry a secondary payer role. Residents and municipal emergency managers are the clearest targets: trapped exit options, immediate/biographical time horizons, and no ability to independently verify which layer of the preparedness claim is sound before an event tests it.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents two mislabeling errors symmetric with its sibling readings: it does not let the genuine, ongoing competence of the engineering layer get discounted by the coordination layer's decay (which the husk_reading would risk implying if applied wholesale), and it does not let the coordination layer's decay be laundered by the engineering layer's continued success (which the competence_reading would risk implying if applied wholesale). By splitting the constraint into two subsystems with different classifications — Rope-adjacent engineering, Tangled-Rope-adjacent coordination — the mandate to prepare is neither declared fully alive nor fully dead; it is declared alive in one place and needing active repair in another, which is the structurally accurate reading given the after-action evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_separability_of_the_kernel,
    'Is ''preparedness transmission'' genuinely one kernel with two separable sub-competencies (as the hybrid reading holds), or is the appearance of separability itself an artifact of the engineering layer''s institutional visibility crowding out an equally uniform decay in the coordination layer that the competence_reading and husk_reading dispute at the whole-system level?',
    'Systematic cross-jurisdictional after-action review comparing engineering performance metrics against coordination performance metrics across many hazard events, controlling for event severity, to test whether the split is a stable empirical pattern or an artifact of which failures get investigated.',
    'If the split does not hold up under systematic review, this hybrid reading collapses into either the husk_reading (uniform hollowing) or the competence_reading (uniform vitality) and should be retired as a separate constraint rather than treated as the accurate middle reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_separability_of_the_kernel, empirical, 'Whether the engineering/coordination split this reading asserts is a real structural feature or an artifact of unequal institutional visibility.').

omega_variable(
    coordination_layer_remediability,
    'Given that the coordination layer''s decay is diagnosed, is it remediable through renewed investment and exercise, or has the loss of tacit, practice-based coordination knowledge crossed a threshold where it cannot simply be re-funded back into existence within a single generation?',
    'Pilot intensive re-drilling and knowledge-transfer programs in a subset of municipalities and measure whether coordination performance in subsequent real or simulated events recovers to historical baselines within a defined period.',
    'If remediable, the coordination-layer classification moves toward scaffold (transitional repair with a sunset once competence is restored); if not remediable within relevant timeframes, it settles more firmly as tangled_rope or drifts toward snare as the gap between claimed and actual preparedness widens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_layer_remediability, empirical, 'Whether the diagnosed coordination decay can be reversed or represents an effectively permanent capacity loss.').

omega_variable(
    beneficiary_awareness_of_stratification,
    'Do engineering agencies and elected officials who benefit from the undifferentiated preparedness narrative actually know that the coordination layer has decayed, or is the stratification itself opaque to the beneficiary seats and not merely convenient for them?',
    'Internal agency correspondence, budget-hearing transcripts, and interviews to establish whether beneficiary seats have received and suppressed the disaster-researcher findings, versus genuinely not having integrated cross-domain after-action data.',
    'Knowing suppression would push the coordination-layer component toward snare (deliberate extraction via engineered ignorance); genuine non-integration would support the tangled_rope reading of structural, not deliberate, extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_awareness_of_stratification, conceptual, 'Whether beneficiaries'' advantage from the undifferentiated narrative is knowing or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__hybrid_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__hybrid_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__hybrid_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__hybrid_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__hybrid_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__hybrid_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__hybrid_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__hybrid_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__hybrid_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the preparedness_transmission kernel, decomposed per the epsilon-invariance principle because the natural-language label 'preparedness' conflates structurally distinct claims about which sub-competencies remain live. competence_reading claims both engineering and coordination remain exercised and current (low decay, low epsilon). husk_reading claims both have hollowed into memorial ritual (high theater, moderate-to-high epsilon uniformly). This hybrid_reading claims a differential: engineering stays live (low epsilon there) while coordination has decayed (moderate epsilon, rising theater_ratio, driven specifically by that subsystem). Each reading is generated as its own clean, epsilon-invariant constraint story per Rule 1; the contest among them is not resolved by averaging but is documented via omega variables in each file and linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
