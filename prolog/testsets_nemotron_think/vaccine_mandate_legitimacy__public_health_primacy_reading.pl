% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Authority — Public Health Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the public_health_primacy_reading of the
 *   vaccine_mandate_legitimacy kernel: the claim that the state's duty to
 *   prevent collective harm justifies mandatory vaccination authority,
 *   framing unvaccinated status as a negative externality. The reading
 *   asserts that mandate authority is a legitimate exercise of police power
 *   for public health coordination. The authored metrics describe a
 *   constraint with substantial and rising extractiveness (bureaucracy gains
 *   authority beyond epidemiological necessity), high and escalating
 *   suppression (coercive enforcement expanding in scope and severity), and
 *   growing theater (compliance performativeness decoupled from transmission
 *   dynamics). The claimed_type is tangled_rope — genuine coordination
 *   function (population immunity) coexisting with asymmetric extraction
 *   (authority capture by bureaucracy, costs borne by refusers).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.78).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Authority — Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'c89e9f06-a917-4b91-ac85-9440197dc42e').
narrative_ontology:cs_kernel_codification('c89e9f06-a917-4b91-ac85-9440197dc42e', fixed_text).
narrative_ontology:cs_authority_grounding('c89e9f06-a917-4b91-ac85-9440197dc42e', lineage).
narrative_ontology:cs_interpretation_layer_present('c89e9f06-a917-4b91-ac85-9440197dc42e').
narrative_ontology:cs_reading_relation('c89e9f06-a917-4b91-ac85-9440197dc42e', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c89e9f06-a917-4b91-ac85-9440197dc42e', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('c89e9f06-a917-4b91-ac85-9440197dc42e', foundational, state_duty_prevent_collective_harm_justifies_mandate).
narrative_ontology:cs_axiom_status(state_duty_prevent_collective_harm_justifies_mandate, holdable).
narrative_ontology:cs_axiom_grounding('c89e9f06-a917-4b91-ac85-9440197dc42e', state_duty_prevent_collective_harm_justifies_mandate, deontological).
narrative_ontology:cs_axiom('c89e9f06-a917-4b91-ac85-9440197dc42e', foundational, unvaccinated_status_creates_negative_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_creates_negative_externality, holdable).
narrative_ontology:cs_axiom_grounding('c89e9f06-a917-4b91-ac85-9440197dc42e', unvaccinated_status_creates_negative_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('c89e9f06-a917-4b91-ac85-9440197dc42e', jacobson_police_power_tradition).
narrative_ontology:cs_drift_state('c89e9f06-a917-4b91-ac85-9440197dc42e', post_covid_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c89e9f06-a917-4b91-ac85-9440197dc42e', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_institutions).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, state_duty_prevent_collective_harm).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_status_negative_externality).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, police_power_justifies_health_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, promulgates, and enforces vaccine mandates through regulatory rulemaking and emergency orders. Gains expanded institutional authority, budget, and policy reach through mandate administration. Justifies mandates as necessary to prevent collective harm from disease spread. Can shift between agencies and advisory roles if political winds change.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Face employment termination, educational exclusion, travel restrictions, and social marginalization for declining vaccination. Bear the direct coercive force of the mandate. Some hold sincere religious or philosophical objections; others distrust institutions or assess personal risk differently. Exit requires either compliance (vaccination) or accepting severe civic and economic exclusion. Limited ability to organize politically due to stigma.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Receive the primary coordination benefit: reduced disease transmission, protected healthcare capacity, and restored social functioning. Also bear indirect costs (tax funding for mandate enforcement, potential adverse events). Generally supportive of mandates but not the constituency capturing the authority gains. Can exit by relocating to jurisdictions with different policies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).

% Cannot be vaccinated or mount adequate immune response; depend entirely on population-level suppression of pathogen circulation for survival. Have no meaningful exit — their safety is structurally bound to the mandate's coordination function. Do not capture authority gains; their interest is purely in the public health outcome.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Enact enabling statutes for mandate authority, appropriate funding, and conduct oversight hearings. Some legislators leverage mandate politics for electoral gain; others seek to constrain executive overreach. Share institutional interest in maintaining legislative primacy over emergency powers. Can shift policy direction through new legislation but face political costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, legislative_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate constitutional challenges to mandates (free exercise, due process, equal protection, non-delegation). Apply varying standards of review (rational basis, strict scrutiny, Jacobson reasonableness). Their rulings shape the enforceable boundary of mandate authority but they do not administer mandates nor bear their costs. Institutional legitimacy depends on perceived neutrality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, courts, observer,
    institutional, generational, analytical, national).

% Compelled to enforce mandates on employees/students (verification, termination, accommodation processes). Bear compliance costs, litigation risk, and workforce disruption. Some support mandates for operational stability; others resist as overreach. Cannot easily exit — non-compliance brings fines and liability. Caught between state coercion and constituent pressure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_institutions, payer,
    organized, biographical, constrained, national).

% Gain guaranteed demand, liability shields, and regulatory priority through mandate-driven procurement. Capture significant financial gains but do not administer mandates. Their interest aligns with mandate expansion but they are not the authority-holding bureaucracy. Can redirect production globally if national markets shift.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population-level immunity sufficient to interrupt pathogen transmission, protect healthcare system capacity, and safeguard those who cannot be vaccinated — a genuine collective action problem where individual vaccination decisions create externalities.
% TRANSFER_FUNCTION: Moves compliance costs, bodily autonomy burdens, and civic exclusion onto unvaccinated individuals; moves institutional authority, budgetary control, and policy discretion to public health bureaucracy; moves financial gains to vaccine manufacturers; moves epidemiological protection to the general and immunocompromised populations.
% ABSENT_VOICES: Children and future cohorts subject to mandate precedent without current political voice; undocumented immigrants excluded from both mandate protections and enforcement data; global populations in vaccine-scarce regions whose access is affected by high-income country mandate-driven procurement. The bodily_autonomy_primacy reading's constituency is structurally excluded from the public health bureaucracy's decision loop.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, vaccination rates would drop in key subpopulations, endemic transmission would resume, immunocompromised individuals would lose their primary protection layer, public health agencies would lose a core regulatory lever, and manufacturers would face demand collapse. The epidemiological and institutional landscape would reorganize within months.
% FOUNDING_PROBLEM: Recurrent epidemic cycles overwhelming healthcare infrastructure and causing mass mortality/morbidity before vaccines; the free-rider problem where individual vaccination decisions under-protect the collective; the need for a state capacity to respond to novel pathogens with speed and coordination.
% FOUNDING_PROBLEM_CORROBORATION: Historical epidemiology (smallpox, polio eradication) corroborates the coordination necessity. The public health bureaucracy attests the problem remains live (novel pathogen threat, waning immunity, variant evolution). The bodily_autonomy_primacy reading and risk_stratification reading attest the founding problem is either solved by less coercive means or never justified blanket authority — legislative dissent, judicial opinions, and international policy divergence (e.g., Nordic non-mandate approaches achieving similar outcomes) corroborate the contested status from outside the benefiting bureaucracy.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the gap between the epidemiological justification (targeted, proportional measures) and the actual mandate scope (broad, often indiscriminate, persisting after emergency phase). Suppression (0.78) is high because the constraint's persistence depends on active enforcement — employment termination, school exclusion, travel bans — not voluntary compliance. Theater (0.42) captures the growing share of enforcement activity that serves bureaucratic self-preservation rather than transmission reduction (e.g., mandates for low-transmission settings, booster requirements without transmission-blocking evidence). Accessibility collapse (0.62) is moderate: alternatives (exemptions, testing, remote work) exist but are narrowing. Resistance (0.71) is high and sustained across legal, political, and civil society channels.
 *
 * PERSPECTIVAL GAP:
 *   From the bureaucracy's seat, the constraint is a rope (coordination solving a real collective action problem). From the unvaccinated seat, it is a snare (pure extraction suppressing exits). From the immunocompromised seat, it is a mountain (their survival structurally depends on it). From the employer seat, it is a tangled rope (they coordinate compliance but extract nothing). The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health bureaucracy is the structural beneficiary (d ~ 0.15): it sets the agenda, collects authority gains, and has arbitrage-grade exit (can rotate to advisory roles, NGOs, academia). Unvaccinated individuals are full targets (d ~ 0.9): they bear the coercive force, have constrained exit (comply or accept exclusion), and moderate power (some legal/political recourse but stigmatized). Vaccinated population and immunocompromised are beneficiaries of the coordination function (d ~ 0.3-0.4) but not the extraction. Employers are payers (d ~ 0.65): they bear enforcement costs with constrained exit. Manufacturers are beneficiaries (d ~ 0.2) with arbitrage exit. Courts are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic control) is real but the mandate's proportionality has drifted. The bureaucracy's authority has expanded beyond the epidemiological necessity — mandates persist for pathogens with lower severity, broader age groups, and diminishing transmission-blocking efficacy. This is mandatrophy: the coordination function (population immunity) has been partially achieved or changed context, but the authority structure persists and expands. The risk_stratification reading captures this drift by demanding actuarial proportionality; the bodily_autonomy reading rejects the premise entirely. The tangled_rope classification captures the hybrid: coordination remains real but extraction has layered on top.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the public_health_primacy_reading logically foreclose the bodily_autonomy_primacy_reading within a single legal framework, or do they coexist as competing but structurally compatible positions?',
    'Constitutional doctrine analysis: if a framework can simultaneously recognize a state duty to prevent harm AND a sphere of medical self-sovereignty (e.g., via strict scrutiny with compelling interest test), they coexist. If the duty is treated as categorically overriding, the bodily_autonomy reading is foreclosed.',
    'If forecloses, the kernel has a binary structure — one reading wins, the other is excluded. If coexists_with, the kernel sustains permanent contestation with shifting dominance. This changes the CS drift analysis: foreclosure implies terminal attractor; coexistence implies cyclical or stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the two primacy readings are logically mutually exclusive or can both be live in one framework.').

omega_variable(
    proportionality_threshold_ambiguity,
    'At what epidemiological threshold does this reading''s mandate authority become disproportionate — and who decides?',
    'Judicial review standards (Jacobson reasonableness vs. strict scrutiny), legislative sunset provisions, or independent epidemiological trigger metrics. The reading''s silence on a specific threshold is the ambiguity.',
    'If no threshold exists, the reading tends toward unbounded authority (extraction without coordination limit). If a threshold exists and is binding, the reading self-limits toward rope. The risk_stratification reading exists precisely to operationalize this threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Whether the reading contains an internal proportionality brake or requires external constraint.').

omega_variable(
    bureaucracy_capture_vs_mission,
    'Is the measured extractiveness driven by public health bureaucracy''s self-interested authority expansion, or by genuine mission demands in a changing pathogen landscape?',
    'Compare mandate scope/timing to independent epidemiological indicators (R0, IFR, healthcare capacity, variant immune escape). If mandates expand when indicators contract, capture is indicated. If mandates track indicators with lag, mission demand is indicated.',
    'If capture, the extraction is structural and the tangled_rope classification understates the snare component. If mission-driven, the rising ε reflects genuine coordination difficulty and the classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucracy_capture_vs_mission, empirical, 'Whether authority growth tracks epidemiological necessity or bureaucratic self-preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vaccine_mandate_ph_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(vaccine_mandate_ph_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vaccine_mandate_ph_be_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(vaccine_mandate_ph_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(vaccine_mandate_ph_be_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(vaccine_mandate_ph_be_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vaccine_mandate_ph_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vaccine_mandate_ph_su_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(vaccine_mandate_ph_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(vaccine_mandate_ph_su_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(vaccine_mandate_ph_su_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_emergency_powers).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, school_vaccination_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_worker_mandates).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the vaccine_mandate_legitimacy constraint family. The public_health_primacy_reading (this story) claims the kernel authorizes broad mandate authority. The bodily_autonomy_primacy_reading claims the kernel forbids any mandate. The risk_stratification_reading claims the kernel authorizes only proportionate, risk-calibrated mandates. Their ε values diverge because they assess different structural referents: this reading assesses the actual standing mandate regime; the bodily_autonomy reading assesses the same regime as categorically extractive; the risk_stratification reading assesses a hypothetical calibrated regime. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, moderate, 0.85).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
