% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Flood Preparedness: Technical Core / Ceremonial Periphery
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the preparedness-retention
 *   kernel: neither the competence reading (drills preserve real capacity)
 *   nor the husk reading (all preparedness activity is empty ritual) is fully
 *   correct, because the two claims apply to different tiers of the same
 *   system. Rijkswaterstaat and the regional water boards genuinely retain
 *   and exercise hydraulic engineering competence through daily operational
 *   work on the physical infrastructure. Meanwhile the broader societal layer
 *   — municipal emergency planning, volunteer civil defense, household-level
 *   flood knowledge — has drifted into ceremonial compliance: drills staged
 *   for reassurance and legal box-ticking rather than skill maintenance. The
 *   coordination function (specialization concentrating scarce expertise) is
 *   real, but it is coupled to an asymmetric cost: distributed societal
 *   resilience has been allowed to atrophy because the technical tier's
 *   existence lets everyone else believe the problem is handled. This creates
 *   a single point of failure — if the technical institutions are wrong,
 *   overwhelmed, or politically captured, there is no living fallback layer.
 *
 * KEY AGENTS:
 *   - rijkswaterstaat_technical_corps: primary agenda-setter and beneficiary — retains and exercises real technical competence
 *   - regional_water_boards: co-administering institutional beneficiary with genuine but narrower operational competence
 *   - coastal_municipal_residents: primary payer — bears the risk of the hollowed-out distributed layer
 *   - volunteer_civil_defense_networks: payer whose function has become substantially ceremonial while still being relied upon in crisis
 *   - non_specialist_local_governments: dependent payer, compliance-driven rather than competence-driven
 *   - national_flood_insurance_framework: institutional beneficiary of centralization's auditability
 *   - flood_risk_researchers: analytical observer documenting the single-point-of-failure risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.52).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Flood Preparedness: Technical Core / Ceremonial Periphery").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, 'd542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67').
narrative_ontology:cs_kernel_codification('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', distributed).
narrative_ontology:cs_authority_grounding('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', practice).
narrative_ontology:cs_interpretation_layer_present('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67').
narrative_ontology:cs_reading_relation('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', foundational, competence_is_institutionally_stratified_not_uniform).
narrative_ontology:cs_axiom_status(competence_is_institutionally_stratified_not_uniform, holdable).
narrative_ontology:cs_axiom_grounding('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', competence_is_institutionally_stratified_not_uniform, empirically_contingent).
narrative_ontology:cs_axiom('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', foundational, centralized_expertise_creates_distributed_vulnerability).
narrative_ontology:cs_axiom_status(centralized_expertise_creates_distributed_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', centralized_expertise_creates_distributed_vulnerability, empirically_contingent).
narrative_ontology:cs_reference_frame('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', post_watersnoodramp_centralized_engineering_mandate).
narrative_ontology:cs_drift_state('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', contemporary_climate_risk_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d542fa7e-ca0f-4e28-9aa3-b05b5f2dfa67', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_corps).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, regional_water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, national_flood_insurance_framework).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, coastal_municipal_residents).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, non_specialist_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains hydraulic engineering expertise, runs simulation models, inspects dikes and storm surge barriers, and sets the technical standards that determine what counts as 'prepared.' Its staff cycle through real engineering problems continuously, so their competence stays live. It controls which knowledge gets institutionalized versus left to municipal volunteers, and its budget and prestige depend on being seen as the indispensable expert layer.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_corps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, rijkswaterstaat_technical_corps, beneficiary).

% Centuries-old polder-management bodies with independent taxing power, operating pumps and local water levels day-to-day. Their competence is genuinely operational because their staff work the physical system constantly. They benefit from continued institutional centrality but depend on Rijkswaterstaat's technical standards and funding formulas, limiting their independence.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, regional_water_boards, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, regional_water_boards, beneficiary).

% Live behind the dikes and barriers the technical corps maintains, but their own household- and community-level flood knowledge has thinned to annual evacuation drills and school programs. If a levee fails outside the modeled scenarios, or if the technical institutions are slow or wrong, residents have no fallback layer of distributed competence — they are structurally dependent on a system they no longer understand or can independently verify.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, coastal_municipal_residents, payer,
    powerless, biographical, trapped, local).

% Community flood-response volunteers who once carried real sandbagging, evacuation, and communication skills passed through lived crisis experience. Their function is now largely ceremonial — annual exercises staged for media and political reassurance rather than skill maintenance. They bear the cost of this hollowing: when disaster strikes, they are activated but under-equipped, absorbing blame and labor without the competence base the arrangement pretends they have.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, volunteer_civil_defense_networks, excluded).

% Municipal councils responsible for local emergency planning but lacking in-house hydraulic expertise. They are legally required to have preparedness plans, which they largely satisfy by adopting templates and running compliance drills designed by the technical corps. Their real capacity to act independently in a crisis is thin; they depend entirely on the specialized institutions to tell them what is actually true about risk.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, non_specialist_local_governments, payer,
    moderate, biographical, constrained, regional).

% Prices flood risk and underwrites recovery based on the technical corps's models and certifications. It benefits from the stratified system because a small number of credible technical bodies is cheaper to audit and contract with than distributed community-level competence, but this concentrates systemic risk if the technical layer is ever wrong, overwhelmed, or politically compromised.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, national_flood_insurance_framework, beneficiary,
    institutional, generational, arbitrage, national).

% Study the gap between institutional technical capacity and ground-level societal resilience, publishing warnings about the single-point-of-failure risk this stratification creates, without holding power to change the resource allocation between the two tiers.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, flood_risk_researchers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrating scarce, expensive hydraulic and engineering expertise inside a small number of well-resourced institutions solves a genuine problem: flood defense requires continuous technical mastery that cannot be maintained at the level of every municipality or household, so specialization allows the actual engineering competence to remain live and current.
% TRANSFER_FUNCTION: Resources, technical authority, and the practical burden of maintaining 'preparedness' are redistributed: funding, prestige, and decision-rights concentrate in Rijkswaterstaat and the water boards, while distributed community-level competence atrophies into scheduled ceremony — residents and volunteers retain the appearance of preparedness (drills, plans, signage) without retaining the underlying capability, and in a crisis outside the modeled envelope they have no independent layer to fall back on.
% ABSENT_VOICES: Coastal residents and volunteer networks who experience the ceremonial layer as functionally hollow rarely have a forum to challenge the technical corps's risk models or resource allocation directly; their objections surface mainly after failure events, in inquiry testimony, rather than in the ongoing planning process.
% DISAPPEARANCE_RATIONALE: If the specialized technical institutions vanished, the physical flood-defense system would fail catastrophically within a short time — genuine dependency exists there. But if only the ceremonial layer (drills, volunteer exercises, public preparedness messaging) vanished, the technical corps argues nothing operationally would change since real competence never lived there; critics argue removing the ceremony would at least expose the true resilience gap rather than papering over it, which is itself a consequential change. The verdict differs depending on which half of the dual-track system is asked about.
% FOUNDING_PROBLEM: Historically, flood defense required an entire population to hold sandbagging, dike-watching, and evacuation knowledge because failure was frequent and defenses were distributed and low-tech; the founding problem was catastrophic, recurrent flooding with no reliable centralized engineering capacity to prevent it.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and the water boards attest the founding problem is being solved better than ever, citing statistical decline in flood events and improved barrier engineering. Independent hazard researchers and post-incident inquiry reports (e.g., analyses following near-miss high-water events) attest that the original problem has partially mutated: engineering failure risk has fallen, but a new problem — brittle, centralized single-point-of-failure dependency with hollowed local capacity — has emerged and is not yet acknowledged as a distinct founding problem requiring its own remedy.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate rather than severe because there is a genuine, substantial coordination function being served — technical concentration is not pure rent-seeking, it reflects a real economy-of-scale in expertise maintenance. But it rises over the measured interval as the gap between the two tiers widens: theater_ratio climbs from 0.2 to 0.55, tracking the observed substitution of ceremonial compliance activity for genuine distributed competence-building at the municipal and volunteer level. Suppression is moderate (0.38) and rises slowly — the arrangement is not held together primarily by coercion but by legal compliance requirements (mandated emergency plans, mandated drills) that quietly narrow what counts as adequate local preparedness to what the technical corps prescribes.
 *
 * PERSPECTIVAL GAP:
 *   From the technical corps's seat, the system looks like efficient, functioning coordination — competence is being maintained exactly where it matters. From the residents' and volunteers' seat, the same system looks like an extraction of confidence: they are told they are prepared, participate in rituals that reinforce that belief, and bear the tail risk if the concentrated expertise fails or is overwhelmed by a scenario outside its models.
 *
 * DIRECTIONALITY LOGIC:
 *   The technical institutions sit near the beneficiary end: they retain real capability, real budget, real prestige, and arbitrage-grade exit (they can always claim indispensability). The insurance framework benefits similarly by having a small, auditable expert layer to underwrite against. Coastal residents, volunteers, and non-specialist local governments sit near the target end: they bear the systemic risk created by the hollowed distributed layer, without the exit options (trapped or constrained) to independently rebuild that capacity themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mislabeling this as either pure coordination (which would ignore the real cost imposed on distributed resilience) or pure extraction (which would ignore that the technical core's competence is genuinely maintained and genuinely valuable). Classifying it as tangled_rope captures both: a real coordination function (concentrated expertise) bundled with an asymmetric cost (atrophied distributed capacity) that requires active enforcement (compliance mandates) to sustain the appearance that the ceremonial layer is adequate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratification_boundary_location,
    'Where exactly does the boundary between the genuinely-competent tier and the ceremonial tier fall — is it a clean institutional line (Rijkswaterstaat/water boards vs. everyone else) or does competence decay gradually as one moves outward from the technical core?',
    'Structured competence audits comparing actual crisis-response performance (evacuation timing, communication accuracy, equipment readiness) across institutional tiers during real flood events or full-scale exercises, not just self-reported drill completion.',
    'A clean boundary supports the hybrid reading''s dual-track structural claim as authored; a gradual-decay finding would suggest the hybrid reading understates how far the erosion penetrates into the technical institutions themselves, moving the story closer to the husk reading at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_location, empirical, 'Whether competence retention is sharply bimodal or a continuous gradient.').

omega_variable(
    single_point_of_failure_materiality,
    'How material is the single-point-of-failure risk in practice — would the technical institutions'' failure modes (funding cuts, model error, political capture, extreme event exceeding design basis) actually manifest as catastrophic loss given no distributed backup layer, or do other redundancies (military mobilization, international mutual aid) substitute?',
    'Scenario stress-testing and comparison to historical near-miss events (e.g., 1990s high-water crises) to see whether backup mechanisms activated meaningfully when the primary technical response was strained.',
    'If real substitute redundancies exist, the victim classification (distributed resilience) is less severe than authored and extractiveness should be read as lower; if no substitutes exist, the tangled_rope classification understates the risk and the constraint drifts toward snare in a crisis-realized reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_point_of_failure_materiality, empirical, 'Whether centralization actually creates uninsured systemic risk or is backstopped by unmodeled redundancy.').

omega_variable(
    hybrid_versus_sibling_readings_framing_choice,
    'Is the stratified/hybrid framing itself the correct decomposition of the underlying phenomenon, or does it smuggle in an assumption that the two tiers are cleanly separable when the actual mechanism is a continuous, system-wide competence decay that merely progresses at different rates in different institutions?',
    'Cross-reference with the sibling constraint stories (competence_reading, husk_reading) and longitudinal institutional case studies; if independent evidence consistently supports a bimodal split, the hybrid framing is well-grounded; if evidence shows continuous decay even within Rijkswaterstaat, the kernel''s true structure may be better captured by a decay-rate model rather than a stratification model.',
    'Determines whether the hybrid_reading remains the most defensible reading of the kernel or should itself be further decomposed into sub-readings by institutional tier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_versus_sibling_readings_framing_choice, conceptual, 'Whether the dual-track framing is the right level of decomposition for this kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__hybrid_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__hybrid_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__hybrid_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__hybrid_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__hybrid_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__hybrid_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__hybrid_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__hybrid_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__hybrid_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__hybrid_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__hybrid_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__hybrid_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_retention kernel. competence_reading asserts system-wide live competence (near-Rope); husk_reading asserts system-wide ceremonial hollowing (near-Snare); this hybrid_reading asserts a bimodal split with a real coordination function bundled to an asymmetric cost, hence tangled_rope. Each reading authors its own epsilon against the same underlying institutional arrangement, per the ε-invariance decomposition principle — they are not the same constraint measured three ways, they are three structurally distinct claims about where competence actually lives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
