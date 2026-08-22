% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Engineering/Ritualization Reading)
 *   domain: institutional/safety/commitment_systems
 *
 * SUMMARY:
 *   Disaster preparedness systems in most jurisdictions display a persistent
 *   pattern: technical infrastructure inspection (structural engineering,
 *   equipment certification) operates at sustained professional competence
 *   levels, while organizational response protocols (evacuation drills,
 *   communication procedures) degrade over time into ritualized performance
 *   with minimal functional connection to actual readiness. This constraint
 *   story instantiates the HYBRID reading of the preparedness_persistence
 *   kernel — the claim that preparedness itself is stratified, with different
 *   components operating at different functional levels simultaneously. The
 *   constraint is neither a pure natural law of disaster response (competence
 *   reading) nor pure theater (husk reading), but a mixed system where
 *   institutional legitimacy depends on sustaining the visible competent
 *   stratum while the ritualized stratum erodes. The structure creates
 *   asymmetric extraction: credentialing bodies benefit from the perception
 *   of unified preparedness, political authorities and insurers benefit from
 *   cost distribution across strata, while field personnel and exposed
 *   populations bear the execution burden and safety gap. The founding
 *   problem (organizing heterogeneous preparedness across cost and complexity
 *   constraints) is live in any large system, but the institutional response
 *   has become institutionalizing the stratification itself rather than
 *   resolving it.
 *
 * KEY AGENTS:
 *   - operational_field_personnel: powerless, trapped — execute both competent and ritual protocols despite resource constraints; bear blame for stratum-specific failures
 *   - institutional_credentialing_bodies: institutional, arbitrage — set compliance standards that permit stratification; benefit from certification of mixed systems
 *   - exposed_populations: powerless, identity_locked — depend on preparedness; have no choice or exit; safety depends on which stratum activates
 *   - engineering_inspection_bodies: powerful, arbitrage — maintain professional standards in technical stratum; professionalization sustains their competence
 *   - political_authorities: institutional, analytical — set budgets and mandates; benefit from visible preparedness without full-system cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.52).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Engineering/Ritualization Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '1c24cafa-756a-44d5-a071-44cce748011b').
narrative_ontology:cs_kernel_codification('1c24cafa-756a-44d5-a071-44cce748011b', distributed).
narrative_ontology:cs_authority_grounding('1c24cafa-756a-44d5-a071-44cce748011b', extraction).
narrative_ontology:cs_reading_relation('1c24cafa-756a-44d5-a071-44cce748011b', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c24cafa-756a-44d5-a071-44cce748011b', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('1c24cafa-756a-44d5-a071-44cce748011b', foundational, preparedness_stratification_institutional).
narrative_ontology:cs_axiom_status(preparedness_stratification_institutional, holdable).
narrative_ontology:cs_axiom_grounding('1c24cafa-756a-44d5-a071-44cce748011b', preparedness_stratification_institutional, conventional).
narrative_ontology:cs_axiom('1c24cafa-756a-44d5-a071-44cce748011b', secondary, mixed_competence_extractive_equilibrium).
narrative_ontology:cs_axiom_status(mixed_competence_extractive_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('1c24cafa-756a-44d5-a071-44cce748011b', mixed_competence_extractive_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('1c24cafa-756a-44d5-a071-44cce748011b', unified_preparedness_mandate).
narrative_ontology:cs_drift_state('1c24cafa-756a-44d5-a071-44cce748011b', contemporary_budget_constraint_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c24cafa-756a-44d5-a071-44cce748011b', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_credentialing_bodies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, regulatory_compliance_administrators).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, operational_field_personnel).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, exposed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_bodies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, evacuation_drill_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First responders and disaster-response teams execute both technically competent procedures (structural inspection, equipment calibration) and ritualized safety protocols (evacuation drills, siren tests) that may diverge from actual readiness. They bear execution cost, resource constraints force prioritization, and they are blamed when either stratum fails despite lacking resources or authority to improve them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, operational_field_personnel, payer,
    powerless, biographical, trapped, local).

% Government agencies and professional certification bodies that license preparedness programs and maintain compliance standards. They benefit from a stratified system: the competent engineering stratum provides legitimacy, while the ritual evacuation stratum remains low-cost to audit. They set the standards that permit and reinforce stratification.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_credentialing_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, institutional_credentialing_bodies, agenda_setter).

% Insurance carriers that underwrite disaster risk and set premiums based on certified preparedness status. They benefit from credentialed preparedness systems that signal competence even where competence is selective. Premium rates reflect certified status, allowing favorable underwriting assumptions without bearing full exposure to preparedness deficits.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, insurance_underwriters, beneficiary,
    institutional, biographical, mobile, global).

% Residents and communities in disaster-exposed areas who depend on preparedness systems for protection. They cannot exit or choose alternatives. Their safety depends on which stratum (competent or ritualized) actually activates during crisis. Ritualization creates false assurance while competence gaps remain unknown until disaster strikes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, exposed_populations, payer,
    powerless, immediate, identity_locked, local).

% Professional engineering firms and technical inspection services that conduct structural assessment and equipment certification. They benefit from high technical standards in their domain and capture professional fees. Their work remains functionally competent because professional credibility depends on actual engineering outcomes, not compliance theater.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_bodies, beneficiary,
    powerful, generational, arbitrage, national).

% School and institutional safety coordinators who organize evacuation drills. They benefit from a ritualized script that meets compliance with minimal resource demand. Their legitimacy is secured by drill completion and documented participation, not evacuation effectiveness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, evacuation_drill_administrators, beneficiary,
    moderate, biographical, constrained, local).

% Municipal and state officials who set preparedness mandates and allocate budgets. They benefit from a stratified system: visible preparedness (via engineering competence) at distributed costs (budgets split across strata). This permits claiming high preparedness while constraining total expenditure.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, political_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Academic and independent researchers studying preparedness effectiveness and disaster response. They are excluded from core credentialing and policy design loops. Their findings about stratification and competence gaps are treated as outside criticism rather than inputs to mandate revision.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, independent_safety_researchers, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, institutional_credentialing_bodies).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes heterogeneous safety functions (structural integrity verification, emergency response sequencing, risk communication, public notification) into a unified credentialing regime that signals institutional competence to regulators, insurers, and residents without requiring cost-equivalent delivery or maintenance across all domains.
% TRANSFER_FUNCTION: Moves compliance burden and operational cost FROM political authorities and certification administrators (who avoid full-system resource allocation) TO operational field personnel and exposed populations (who execute both competent and ritual protocols, bearing execution cost and safety exposure). Moves underwriting margin and credibility value TO insurance companies and certification bodies.
% ABSENT_VOICES: Independent safety researchers and operational field personnel in the ritual stratum have limited voice in standards-setting. Communities that have experienced preparedness failures are structurally excluded from credentialing and policy design loops. Competing preparedness models that reject stratification are not formally considered in mandate design.
% DISAPPEARANCE_RATIONALE: If stratified preparedness disappeared, political authorities would face unified pressure to either fund integrated competence across all domains or explicitly acknowledge which preparedness functions they are abandoning. Insurance underwriting would shift to demand higher premiums for documented gaps rather than relying on certification status. Communities would pressure for either genuine preparedness or honest risk communication. Budget allocation would consolidate rather than distribute.
% FOUNDING_PROBLEM: Disaster preparedness requires both technical competence (structural integrity assurance, equipment readiness) and organizational coordination (response sequencing, communication protocols). Achieving high competence across both domains is expensive and requires sustained professional attention. Historical pattern: authorities choose to fund the technically complex, visible domain (engineering inspection) while allowing the organizationally complex, routine domain (drills) to ritualize, treating stratification as inevitable rather than a policy choice.
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster investigations by non-credentialing bodies (Congressional committees, international disaster response teams, academic forensic researchers) consistently document this pattern: engineering systems perform to specification while evacuation and communication protocols fail despite documented compliance. After-action reports from multiple jurisdictions show the same stratum-specific failure modes. Insurance industry studies note that certified preparedness often does not predict disaster outcomes. Researchers outside the credentialing system (Wildavsky 1988, O'Neill 2012, Stern et al. 2015, Klinenberg 2018) have documented the stratification pattern across multiple types of disasters.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The hybrid reading assigns moderate-high extractiveness (0.58) because the constraint does solve a real coordination problem (organizing multiple preparedness functions) but does so asymmetrically: the cost of maintaining multiple strata is distributed so that beneficiaries (administrators, certifiers, insurers) capture the credibility value while payers (field personnel, communities) absorb the gaps. Suppression is moderate (0.52) because the stratification is not maintained by external coercion alone — institutional memory and professional norms sustain the engineering stratum, while budget constraints and compliance compliance norms sustain the ritual stratum. Neither group is forced into line; rather, the structure channels resources and attention differentially. Theater ratio is high (0.62) because the ritual stratum (evacuation drills) operates largely as compliance theater — its function is demonstrating institutional care rather than maintaining evacuation readiness — while the technical stratum (structural inspection) remains functionally grounded. The measurement series shows extractiveness rising through the interval (time 0–20) as political authorities increasingly rely on stratification to manage budget pressures, then stabilizing (time 20+) as the constraint settles into equilibrium: field personnel accept the mixed regime as normal, communities lose institutional memory of higher preparedness, certification bodies internalize the stratified standard. Theater_ratio rises through the interval as the ritual stratum becomes more explicitly performative (siren tests replaced with simulations, drill scenarios disconnected from actual response chains), then stabilizes as the performance becomes routine and unquestioned. Suppression_requirement tracks similar arc: more active suppression is needed early (preventing field personnel from openly acknowledging gaps, preventing independent audits of drill effectiveness), then stabilizes as compliance becomes normalized and independent voices are structurally excluded. The measurement grid is aligned: every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats (credentialing bodies, political authorities, insurance companies) experience this constraint as necessary coordination — they genuinely believe preparedness requires stratification and that competent inspection is the core function. From their seat, ritual compliance is a reasonable accommodation to budget realities. The payer seats (field personnel, exposed populations) experience the same constraint as risk-shifting and cost-shifting — they bear the burden of maintaining appearance (executing ritualized drills) while bearing actual exposure to gaps (inadequate evacuation procedures). The field personnel seat specifically experiences a dual burden: they must maintain both systems even when resource constraints force a choice, and they are blamed if the ritual stratum fails in crisis even though they lacked resources to prevent degradation. The engine computes this divergence per-seat from the structural data: beneficiary seats derive low d (beneficiaries, mobile exit, powerful position → low extraction directionality); payer seats derive high d (victims, trapped/identity_locked exit, powerless → high extraction directionality). This divergence is the measurement the framework exists to take — a constraint CLAIMED as coordination that computes as locally extractive is exactly how institutional capture is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary/victim declarations and exit structure: institutional_credentialing_bodies, being declared beneficiaries with institutional power and mobile exit (they can regulate in multiple jurisdictions), derive d near 0.15 (full beneficiary end). engineering_inspection_bodies, beneficiaries with powerful institutional position, derive d near 0.20. political_authorities, agenda_setters, derive d near 0.25 (they set the rules but also face institutional constraints and electoral accountability). operational_field_personnel, declared victims with powerless position and trapped exit (cannot refuse to execute assigned protocols), derive d near 0.85 (full target end). exposed_populations, declared victims with powerless position and identity_locked exit (cannot move away from hazard), derive d near 0.90 (near-maximal target). insurance_underwriters, beneficiaries but with arbitrage exit (can move business to different jurisdictions), derive d near 0.35 (beneficiary with mobility buffer). evacuation_drill_administrators, beneficiaries with moderate power and constrained exit, derive d near 0.40. This spread reflects the structural asymmetry: the constraint distributes directionality heavily toward powerless, immobile groups and away from institutional actors with multiple exit routes. No directionality overrides are needed — the beneficiary/victim structure and exit declarations produce accurate relative positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids the false natural-law trap that would arise from a pure competence reading (claiming mountain on the grounds that some parts remain functionally sound) by explicitly acknowledging stratification and asymmetry. It also avoids the false-piton misclassification that would arise from looking only at the ritual stratum's theater ratio (0.62 is high enough to suggest piton, but the competent stratum's theater_ratio would be much lower, ~0.25). The claimed_type of tangled_rope is justified: the constraint possesses genuine coordination function (organizing multiple preparedness domains into a credible regime) AND asymmetric extraction (credibility accrues to authorities while safety gaps accrue to populations). The requires_active_enforcement flag is true because the stratification's persistence depends on continuous institutional work (budgets, certification cycles, policy decisions) to maintain the competent stratum while allowing ritual erosion. Without active enforcement of standards in the competent domain, the whole system would collapse into pure theater. The founding_problem is live (preparedness coordination remains unsolved in most jurisdictions — they have not integrated the strata or chosen a unified commitment level) and the disappearance verdict is world_rearranges (if the stratified regime vanished, authorities would face unified pressure to either fund integrated preparedness or openly accept preparedness gaps). This combination avoids mandatrophy: the system persists not because its founding problem is solved, but because the institutional solution has become institutionalizing the problem itself (creating and maintaining stratification as the default response to unsolvable cost/competence tradeoff).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stratum_independence_vs_coupling,
    'Are the competent and ritualized strata functionally independent, or does failure in one stratum systematically cascade into failure in the other during crisis?',
    'Post-disaster forensic analysis: trace failures in evacuation execution back to structural deficits or upstream decision-trees. Controlled stress-testing of mixed-stratum protocols. Comparative study of single-stratum vs. stratified regions after equivalent disasters.',
    'If independent: the constraint can be classified as piton (one stratum performs, one performs theatrically, no causal linkage). If coupled: the constraint is tangled_rope (the competent stratum''s legitimacy depends on the ritual stratum''s compliance appearance; false assurance is the extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratum_independence_vs_coupling, empirical, 'Whether strata are functionally isolated or mutually dependent.').

omega_variable(
    committer_kernel_framing_choice,
    'Is this constraint best characterized as a SINGLE mixed-competence system (hybrid reading, what this story authors) or as TWO separate constraints (competence reading: inspections carry real function; husk reading: drills carry mostly ritual)? Which framing did the institutional actors intend, and which does the ε-invariance test favor?',
    'Examine institutional mandate documents and budget structures: do credentialing bodies treat engineering and evacuation as one unified preparedness concept or as separable functions? Do budget lines collapse them or distinguish them? Interview institutional designers and administrators about whether they view preparedness as holistic or stratified by design.',
    'If unified (one constraint, hybrid reading): ε = 0.58, claimed_type = tangled_rope, mixed extraction. If stratified into two: engineering_preparedness becomes mountain/rope (ε near 0.2, high accessibility_collapse, low resistance), evacuation_readiness becomes piton (ε near 0.65, high theater_ratio, low resistance). This story is authored on the unified framing; sibling constraints would instantiate the decomposed reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing_choice, conceptual, 'Whether preparedness is one hybrid system or two separable constraints.').

omega_variable(
    extraction_mechanism_identity,
    'What specifically is being extracted? Is it: (a) cost-shifting (full-system competence cost → false-assurance credibility at partial cost), (b) risk-shifting (hazard exposure from authorities → field personnel and residents while maintaining liability protection), (c) both?',
    'Budget analysis: compare full-competence cost estimate with budgeted expenditure and identify where the gap is absorbed (operationally, legally, or via insurance). Liability track record: examine how failure claims are resolved — are administrators protected by certification while field personnel bear blame?',
    'If (a) cost-shifting dominates: the constraint is primarily rent-seeking; suppression is administrative (keeping full cost information hidden). If (b) risk-shifting dominates: suppression includes identity-locked compliance (field personnel cannot refuse to execute insufficient protocols). If both: extraction operates on two axes simultaneously, which deepens both the directionality and the suppression requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_identity, empirical, 'The specific form of asymmetric value extraction.').

omega_variable(
    competence_atrophy_timing,
    'In the ritual (evacuation) stratum, how long after a competence-maintenance event (last major disaster, last comprehensive retraining) does degradation to theater become observable? Is the decay rate constant across organizations and geographic regions?',
    'Longitudinal study tracking drill execution quality as a function of time-since-last-crisis and time-since-last-comprehensive-training. Compare organizations with different drill-frequency and retraining schedules. After-action reports from districts with varying resource levels.',
    'If decay is slow and gradual (half-life >10 years): the constraint might be classified as rope (periodic exercises sustain enough competence to justify coordination framing). If decay is rapid (competence loss within 3-5 years of no crisis): the constraint is piton (ritual maintenance cannot sustain readiness; theater is the constraint''s actual function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_atrophy_timing, empirical, 'Rate of competence atrophy in ritualized preparedness components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__hybrid_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.61).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__hybrid_reading, theater_ratio, 15, 0.64).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.66).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__hybrid_reading, theater_ratio, 25, 0.63).
narrative_ontology:measurement_basis(prep_tr_t25, projected).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__hybrid_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(prep_tr_t30, projected).
narrative_ontology:measurement(prep_tr_t35, preparedness_persistence__hybrid_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement_basis(prep_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__hybrid_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__hybrid_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(prep_be_t25, projected).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(prep_be_t30, projected).
narrative_ontology:measurement(prep_be_t35, preparedness_persistence__hybrid_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(prep_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__hybrid_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__hybrid_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_persistence__hybrid_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement_basis(prep_su_t25, projected).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__hybrid_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(prep_su_t30, projected).
narrative_ontology:measurement(prep_su_t35, preparedness_persistence__hybrid_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(prep_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.14).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, insurance_underwriting__disaster_risk).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, municipal_budget_allocation__safety_tradeoffs).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three structurally distinct constraints with different ε values and beneficiary structures. This story (hybrid_reading) models the empirically observable pattern where institutional systems maintain mixed competence across preparedness domains. The competence_reading and husk_reading are sibling constraints in the same family. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
