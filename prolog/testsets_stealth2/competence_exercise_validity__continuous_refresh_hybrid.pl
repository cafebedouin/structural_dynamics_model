% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Drill-Cycle Mandate for Competence Retention (Hybrid Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Across high-hazard industries, regulators require operators to run
 *   recurring exercise cycles — simulator sessions, mock evacuations,
 *   unannounced drills, after-action reviews — on the finding that certified
 *   competence decays without use. This story instantiates ONE reading of the
 *   contested kernel competence_exercise_validity: the
 *   continuous_refresh_hybrid reading, which holds that simulation is
 *   necessary but not sufficient, that retention is process-dependent rather
 *   than state-validated, and that the safety record attests continuous
 *   exercise works — not that simulation equals catastrophe. The sibling
 *   readings (simulation_as_proxy, real_catastrophe_only) are separate
 *   constraints in separate files; their contest is routed to omega
 *   variables, not folded into this classification. The ε referent is the
 *   standing arrangement under contest — the mandated continuous-exercise
 *   regime as it actually operates — assessed by this reading's own lights.
 *   KEY AGENTS (by structural relationship): - safety_regulators:
 *   agenda-setter and beneficiary (institutional/constrained) — writes and
 *   enforces the cycle, collects mandate scope - emergency_exercise_vendors:
 *   primary concentrated beneficiary (organized/arbitrage) — collects fees
 *   that scale with mandate - internal_training_departments: identity-locked
 *   beneficiary (organized/identity_locked) — exists because the cycle exists
 *   - frontline_response_teams: dual-positioned participant
 *   (moderate/constrained) — receives retained competence, pays in hours -
 *   high_hazard_operators: primary target (powerful/constrained) — funds the
 *   regime, cannot leave licensed operation - production_workforces: target
 *   (moderate/trapped) — carries drill hours and fatigue -
 *   small_regional_operators, facility_neighbor_communities: excluded seats -
 *   human_factors_researchers: analytical observer. The claim/metric gap is
 *   deliberate: the reading CLAIMS tangled_rope (genuine retention function
 *   plus identifiable extraction), while the metrics are authored from the
 *   regime's observed operation; the engine computes per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - safety_regulators: agenda-setter and beneficiary (institutional/constrained) — sets frequency and realism standards, enforces via licensure, collects scope and budget
 *   - emergency_exercise_vendors: concentrated beneficiary (organized/arbitrage) — sells the cycle, revenue tracks mandate elaborateness
 *   - internal_training_departments: identity-locked beneficiary (organized/identity_locked) — professional existence fused with administering the program
 *   - frontline_response_teams: dual-positioned participant (moderate/constrained) — gains retained competence, pays in diverted hours
 *   - high_hazard_operators: primary target (powerful/constrained) — bears direct cost, exit means surrendering the license
 *   - production_workforces: target (moderate/trapped) — bears drill hours, no design voice
 *   - small_regional_operators: excluded (powerless/trapped) — uniform mandate is proportionally crushing, no seat in standard-setting
 *   - facility_neighbor_communities: excluded (powerless/trapped) — carry residual decay risk, learn drill quality only after incidents
 *   - human_factors_researchers: analytical observer (analytical/analytical) — supplies the evidence both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.52).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.5).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill-Cycle Mandate for Competence Retention (Hybrid Reading)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'fb66772a-eacd-4bd7-b048-c91687166750').
narrative_ontology:cs_kernel_codification('fb66772a-eacd-4bd7-b048-c91687166750', distributed).
narrative_ontology:cs_authority_grounding('fb66772a-eacd-4bd7-b048-c91687166750', expertise).
narrative_ontology:cs_interpretation_layer_present('fb66772a-eacd-4bd7-b048-c91687166750').
narrative_ontology:cs_reading_relation('fb66772a-eacd-4bd7-b048-c91687166750', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('fb66772a-eacd-4bd7-b048-c91687166750', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('fb66772a-eacd-4bd7-b048-c91687166750', foundational, competence_decays_without_continuous_practice).
narrative_ontology:cs_axiom_status(competence_decays_without_continuous_practice, holdable).
narrative_ontology:cs_axiom_grounding('fb66772a-eacd-4bd7-b048-c91687166750', competence_decays_without_continuous_practice, empirically_contingent).
narrative_ontology:cs_axiom('fb66772a-eacd-4bd7-b048-c91687166750', foundational, one_time_state_validation_cannot_certify_retention).
narrative_ontology:cs_axiom_status(one_time_state_validation_cannot_certify_retention, holdable).
narrative_ontology:cs_axiom_grounding('fb66772a-eacd-4bd7-b048-c91687166750', one_time_state_validation_cannot_certify_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('fb66772a-eacd-4bd7-b048-c91687166750', continuous_process_validity).
narrative_ontology:cs_drift_state('fb66772a-eacd-4bd7-b048-c91687166750', contemporary_compliance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb66772a-eacd-4bd7-b048-c91687166750', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, emergency_exercise_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, internal_training_departments).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_response_teams).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, high_hazard_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, production_workforces).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, skill_decay_empirical_literature).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, continuous_readiness_assurance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes exercise-frequency and exercise-realism requirements into operating licenses following accident-inquiry recommendations. Staffs inspection and exercise-approval offices whose remit grows with every mandate expansion. Collects budget, jurisdiction, and standing from administering the regime; its public legitimacy is bound to the assurance system it runs, so stepping back from it is not a live option.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, beneficiary).

% Sells scenario design, simulator time, exercise evaluation, and after-action reporting to regulated operators. Revenue scales with mandated frequency and elaborateness of drills. Serves multiple industries and jurisdictions and can shift clients or product lines freely; nothing about the arrangement holds it in place.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, emergency_exercise_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Permanent organizational units whose headcount, curricula, scheduling machinery, and career ladders exist because continuous exercise is required. Staff careers are built as exercise planners and evaluators; proposing that the program shrink would be proposing their own obsolescence, and the unit's self-concept is fused with running the cycle.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, internal_training_departments, beneficiary,
    organized, biographical, identity_locked, national).

% Crews, code teams, and shift operators who actually rehearse. They gain maintained skills and practiced coordination that show up in real event performance, and they pay in hours spent away from production and in drill fatigue. They cannot opt out individually, and the benefit they receive depends heavily on whether drills are realistic and unannounced rather than scripted.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_response_teams, beneficiary,
    moderate, biographical, constrained, regional).

% Nuclear utilities, airlines, chemical processors, and hospital systems that fund the exercise regime: vendor contracts, simulator capital, downtime for drills, and documentation overhead. They lobby on burden, negotiate frequency with regulators, and pass some cost to customers, but they cannot abandon licensed operation, and exiting the regime means surrendering the license.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, high_hazard_operators, payer,
    powerful, generational, constrained, global).

% Shift workers whose scheduled hours are redirected into mandatory drills regardless of production pressure. They carry the opportunity cost directly and report drill fatigue, but have little voice in scenario design or frequency, and declining participation is a disciplinary matter.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, production_workforces, payer,
    moderate, immediate, trapped, local).

% Regional carriers, small clinics, and minor chemical facilities for whom a uniform exercise mandate written around large-operator economics is proportionally crushing. They have no seat in standard-setting consultations; their practical choice is absorbing the cost or closing.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, small_regional_operators, excluded,
    powerless, biographical, trapped, regional).

% People living and working near plants, air corridors, and hospitals who bear the residual consequences if response competence has quietly decayed. They have no seat in exercise design, frequency, or evaluation decisions, and typically learn the quality of the drills only after an incident tests them.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, facility_neighbor_communities, excluded,
    powerless, generational, trapped, local).

% Publishes skill-decay curves, exercise-transfer studies, and evaluations of simulation fidelity. Supplies the evidence that regulators cite when expanding mandates and that skeptics cite when challenging them. Holds no operational stake in whether any particular regime persists.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, human_factors_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, emergency_exercise_vendors).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the perishable-skill problem: response competence decays between rare high-consequence events, individual operators discount the tail risk and would underinvest in rehearsal, and the regime coordinates sustained, organization-wide investment in keeping capability occupied through recurring cycles.
% TRANSFER_FUNCTION: Moves operating budget and worker hours from production organizations into exercise provision — vendor fees, simulator capital, internal training apparatus, and regulator oversight — and moves frontline time from output work into rehearsal.
% ABSENT_VOICES: Small regional operators priced out by uniform mandates, facility-neighbor communities who carry the residual risk of decayed competence, and frontline workers whose judgments about which drills teach anything are rarely solicited in scenario design. All three are outside the rooms where frequency and realism standards are set.
% DISAPPEARANCE_RATIONALE: If the continuous-cycle requirement vanished overnight, readiness would decay silently between rare events, incident-command performance would degrade on the timescale of skill half-lives, the exercise vendor sector and internal training apparatus would contract sharply, and regulators would lose a primary instrument of assurance — the preparedness economy would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Post-accident inquiries found crews and command structures whose certified, once-trained competencies had decayed by the time a rare event arrived; the arrangement was built to bridge the gap between infrequent real events and durable response capability.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the skill-decay experimental literature, accident-investigation board findings across jurisdictions, and insurer loss data linking exercise cadence to event outcomes. The vendor community's attestation is discounted as self-interested; the external sources independently attest both the original problem and its persistence.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): the retention function is real and accident-record-attested, which caps extraction well below snare territory, but vendor margins, mandate expansion, and compliance ritual push it clearly above pure coordination cost. Suppression (0.50) is administrative rather than physical: the regime holds through licensure and accreditation, where refusal ends the ability to operate, but employs no apparatus beyond exclusion from the licensed channel. Theater_ratio (0.48) is the story's sharpest signal — a large and growing share of mandated activity is scripted, announced in advance, and leniently evaluated, i.e., performed to satisfy the requirement rather than to occupy competence; the measurement series documents this as monotonic Goodhart drift rather than oscillation, so no cyclical pattern is claimed. Accessibility_collapse (0.55): alternatives exist in principle — reliance on workforce tenure, hiring experienced staff, learning from real events — but accreditation forecloses them inside licensed sectors, leaving partial collapse. Resistance (0.42): operators lobby on burden after economic shocks, workforces report fatigue, and periodic deregulatory pushes recur, but no sustained opposition has formed because the founding problem is visibly live. All three tracked series run on one shared time grid (T=0..30 at 5-unit steps) so every metric is authored at every examined point; end-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the regulator and vendor positions the arrangement is a functioning assurance system they operate or supply — coordination-forward, with extraction reading as the price of readiness. From the operator position it is a costly mandate whose elaborateness is set partly by the people who sell it — extraction-forward, with coordination as the cover the invoice travels under. From the frontline-team position it splits by drill quality: realistic unannounced exercises read as genuine benefit, scripted annual rituals read as pure time tax. The researcher seat sees the underlying empirical contest rather than any seat's settlement. The engine computes these divergences from power, exit, and directional position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the collecting seats: vendors sit nearest the beneficiary pole (arbitrage-grade exit, nothing binds them to the arrangement), regulators near it (they collect scope and budget, and their constrained exit reflects legitimacy fusion rather than cost-bearing), and training departments near it with identity lock amplifying their defense of the cycle. Victim declarations drive high directionality for the paying seats: operators sit far toward the target pole with constrained exit amplifying effective extraction, and workforces farther still — trapped, with the least voice per unit of cost borne. Frontline teams derive mid-low from their beneficiary declaration, but their situation is genuinely dual: they pay hours and receive skill, so their computed seat should land nearer symmetric than any other declared beneficiary. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct ordering, and the override array keys on power atoms too coarsely to separate the dual-positioned frontline seat from the trapped workforce seat without corrupting the latter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — competence decay between rare events — is live and externally corroborated, so this is not a resolved-mandatrophy case and the status x disappearance pairing (live x world_rearranges) raises no zombie flag. The classification work the analysis does is boundary-keeping in both directions: reading the arrangement as pure rope would erase the documented vendor capture and the theater trajectory, while reading it as a snare would erase the retention function that accident records and skill-decay studies independently attest. The tangled_rope claim holds both facts in one structure. The forward risk the measurements track is mandate atrophy in place: theater_ratio approaching 0.5 means the cycle increasingly persists as compliance performance even where its training function has hollowed — the classic precursor signature, watched rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_within_kernel,
    'Which reading of the competence_exercise_validity kernel governs: continuous mixed-process exercise (this reading), simulation alone as valid proxy (simulation_as_proxy), or real catastrophe as the only true exercise (real_catastrophe_only)?',
    'Comparative outcome studies across jurisdictions and industries adopting different exercise mixes, plus adversarial review of the safety-record attribution this reading relies on.',
    'If simulation_as_proxy prevails, mandated non-simulation cycle components lose their justification and this constraint''s extraction profile collapses toward pure coordination cost; if real_catastrophe_only prevails, even continuous cycles fail the validity test and the coordination claim itself fails, leaving enforcement without function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_within_kernel, conceptual, 'Kernel-level contest over what validates competence exercise; this story is one reading of three.').

omega_variable(
    retention_share_of_expenditure,
    'What fraction of mandated exercise expenditure produces measurable competence retention, versus vendor margin, internal-apparatus overhead, and compliance ritual?',
    'Matched-cohort skill-decay studies comparing drilled and undrilled teams at varying drill intensity and cost, with cost accounting separated from training effect.',
    'A high retention share supports the rope-leaning side of the tangled_rope claim; a low share indicates the extraction component dominates and the arrangement is drifting toward snare or, if the function hollows entirely, piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_share_of_expenditure, empirical, 'Decomposition of exercise spend into retention-producing versus captured components.').

omega_variable(
    theater_measurement_validity,
    'How much observed drill activity is unannounced and realistic versus scripted, pre-notified, and leniently evaluated?',
    'Unannounced-drill audits and evaluator-independence studies comparing announced versus no-notice exercise performance deltas.',
    'If audited theater exceeds 0.5, the Goodhart drift in the measurement series is confirmed as the dominant mode and the piton trajectory becomes the live hypothesis; if audits show most activity is genuine, the theater scalar is overstated and the rope component is stronger than authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_measurement_validity, empirical, 'Direct auditability of the performative share of mandated exercise activity.').

omega_variable(
    frequency_calibration_domain_variance,
    'Is a uniform mandated cycle frequency calibrated to the domains where skill decay is fastest, or does one-size-fits-all scheduling over-drill slow-decay domains and under-drill fast-decay ones?',
    'Domain-specific skill half-life measurement mapped against actual mandated cadences by sector.',
    'Miscalibration inflates measured extraction with deadweight cost in over-drilled domains while leaving genuine risk in under-drilled ones — meaning part of the authored epsilon is waste rather than capture, and part of the coordination promise is unmet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frequency_calibration_domain_variance, conceptual, 'Whether uniform frequency mandates match domain-specific decay rates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_refresh_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cev_refresh_tr_t5, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cev_refresh_tr_t10, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cev_refresh_tr_t15, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 15, 0.35).
narrative_ontology:measurement(cev_refresh_tr_t20, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cev_refresh_tr_t25, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 25, 0.44).
narrative_ontology:measurement(cev_refresh_tr_t30, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(cev_refresh_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(cev_refresh_be_t5, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cev_refresh_be_t10, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(cev_refresh_be_t15, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(cev_refresh_be_t20, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(cev_refresh_be_t25, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(cev_refresh_be_t30, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cev_refresh_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cev_refresh_su_t5, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(cev_refresh_su_t10, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cev_refresh_su_t15, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(cev_refresh_su_t20, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cev_refresh_su_t25, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(cev_refresh_su_t30, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, resource_allocation).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'competence exercise validity' decomposes into three structurally distinct readings of one kernel. This file authors the continuous_refresh_hybrid reading (process-dependent retention; moderate extraction riding a genuine coordination function). The simulation_as_proxy sibling authors the claim that simulation alone constitutes valid exercise (lower apparent cost, higher theater exposure); the real_catastrophe_only sibling authors the claim that only real events exercise competence (negates the exercise economy entirely). Each story carries its own epsilon, beneficiaries, and victims; the family is linked through network.affects_constraints and cs_structure.reading_relations rather than averaged into one story, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
