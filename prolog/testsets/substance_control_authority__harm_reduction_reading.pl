% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm Reduction Authority: Drug Use Acceptance via Public Health Intervention
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A public health authority decriminalizes drug use and establishes harm
 *   reduction services (needle exchanges, supervised consumption sites,
 *   medication-assisted treatment, overdose reversal) as the primary state
 *   mechanism for managing drug use while minimizing health harms. This is
 *   ONE READING of the contested kernel 'substance_control_authority' —
 *   specifically, the harm reduction reading. Other readings (prohibition,
 *   legalization) instantiate different constraints from the same kernel but
 *   with different ε values, beneficiary structures, and classifications.
 *   This reading authorizes drug use while the authority manages health
 *   externalities through services rather than criminalization. The
 *   constraint's persistence requires active non-enforcement (police do not
 *   arrest for possession) and sustained funding of service infrastructure,
 *   making it a Tangled Rope: genuine coordination function (disease control,
 *   overdose prevention) combined with asymmetric extraction (users bear
 *   health risks and service-defined identity-locking; neighbors and
 *   emergency services bear externality costs). The claim/metric gap is
 *   intentional: this reading's structural premise is that decriminalization
 *   + services constitute legitimate state authority (rope-like
 *   coordination); the authored metrics reflect how that authority operates
 *   in practice (substantial suppression of criminal enforcement, asymmetric
 *   burden distribution, moderate theater as services become bureaucratic).
 *
 * KEY AGENTS:
 *   - people_using_drugs: primary beneficiary (exit criminal justice) and victim (remain in health-managed victim set via identity-locking)
 *   - public_health_authority: primary agenda-setter, authorizes decriminalization and services
 *   - law_enforcement: secondary agenda-setter, enforces non-arrest policy; benefits from resource relief, pays through authority reduction
 *   - neighbors_of_service_sites: payer, bear visible externalities without choice in site placement
 *   - emergency_medical_services: payer, increased overdose response volume
 *   - political_opposition: excluded, would argue for prohibition-based enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Authority: Drug Use Acceptance via Public Health Intervention").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '5df93f88-e95a-4fc2-8286-613bab0fef78').
narrative_ontology:cs_kernel_codification('5df93f88-e95a-4fc2-8286-613bab0fef78', fixed_text).
narrative_ontology:cs_authority_grounding('5df93f88-e95a-4fc2-8286-613bab0fef78', extraction).
narrative_ontology:cs_interpretation_layer_present('5df93f88-e95a-4fc2-8286-613bab0fef78').
narrative_ontology:cs_reading_relation('5df93f88-e95a-4fc2-8286-613bab0fef78', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5df93f88-e95a-4fc2-8286-613bab0fef78', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('5df93f88-e95a-4fc2-8286-613bab0fef78', foundational, decriminalization_of_use_is_legitimate).
narrative_ontology:cs_axiom_status(decriminalization_of_use_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5df93f88-e95a-4fc2-8286-613bab0fef78', decriminalization_of_use_is_legitimate, deontological).
narrative_ontology:cs_axiom('5df93f88-e95a-4fc2-8286-613bab0fef78', foundational, health_services_reduce_population_harm_better_than_enforcement).
narrative_ontology:cs_axiom_status(health_services_reduce_population_harm_better_than_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5df93f88-e95a-4fc2-8286-613bab0fef78', health_services_reduce_population_harm_better_than_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('5df93f88-e95a-4fc2-8286-613bab0fef78', public_health_primacy_framework).
narrative_ontology:cs_drift_state('5df93f88-e95a-4fc2-8286-613bab0fef78', contemporary_political_opposition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5df93f88-e95a-4fc2-8286-613bab0fef78', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_using_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_system).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, law_enforcement_resource_conservation).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_using_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighbors_of_injection_sites).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, emergency_health_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, addiction_treatment_system).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighbors_of_service_sites).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, emergency_medical_services).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, harm_reduction_epidemiology).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit the criminal justice system (no arrest for possession/use) and gain access to sterile supplies, supervised consumption facilities, medication-assisted treatment, and overdose reversal without legal penalty. They remain exposed to health harms (infection, overdose, addiction) and bear the costs of managing these harms through the constraint's service infrastructure. Their identity as people who use drugs becomes structurally recognized rather than criminalized, but this recognition locks them into a health-serviced population category.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_using_drugs, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_using_drugs, payer).

% Set and administer the harm reduction framework: operate needle exchanges, supervised consumption sites, distribution of naloxone, linkage to treatment. They argue this approach reduces overdose deaths and infectious disease while respecting user dignity. They have discretionary authority over service design and site location, constrained by public opposition and political approval.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, agenda_setter,
    organized, generational, mobile, regional).

% Declares drug use as a public health problem rather than a criminal problem; authorizes and funds harm reduction services; reframes enforcement from criminalization to health intervention. They bear responsibility for measured outcomes (infection rates, overdose deaths, community health) and face political pressure when harms occur in their jurisdiction.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Exit the resource-intensive enforcement role (drug arrest, possession prosecution, incarceration) and redirect capacity to violent crime. They benefit from decriminalized use reducing enforcement demand; they also bear the cost of managing open drug scenes and associated disorder without criminalization as a tool. The constraint requires active non-enforcement — officers must not arrest for possession, which some resist as a loss of authority.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement, payer).

% Bear visible externalities: injection sites in their neighborhoods, discarded equipment, congregating users, increased street-level disorder. They pay through reduced property values, visible drug use, and perceived safety decline. Their exit options are limited — relocating is expensive and geographically constrained by where service infrastructure concentrates.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, neighbors_of_service_sites, payer,
    moderate, immediate, constrained, local).

% Bear the cost of overdose response: ambulance deployment, emergency department treatment, repeat presentations. The constraint increases their service volume (more users survive overdoses via naloxone distribution; they then seek emergency care). They pay through resource depletion and crowding; the benefit (lives saved) is diffuse across the public health system.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, emergency_medical_services, payer,
    organized, biographical, constrained, regional).

% Would argue for prohibition-based enforcement, framing harm reduction as capitulation to drug use and enablement of addiction. They are excluded from the constraint's legitimacy structure (which is grounded in public health authority) but retain political power to challenge site operations, defund services, and reverse policies through electoral and legislative pressure.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, political_opposition_to_harm_reduction, excluded,
    powerful, biographical, constrained, national).

% Receives users into treatment pathways via harm reduction linkage: medication-assisted treatment, counseling, residential programs. They benefit from the constraint generating demand and access pathways. They also depend on sustained funding and political support for the harm reduction infrastructure that feeds their client base.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, addiction_treatment_system, beneficiary,
    organized, biographical, mobile, regional).

% Measure outcomes and validate the harm reduction framework: overdose mortality, HIV and hepatitis C prevalence, treatment engagement, cost-effectiveness. They take no direct role in the constraint's operation but furnish the empirical legitimacy claims on which the public health authority grounds its mandate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, epidemiologists_and_public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, public_health_authority).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of managing widespread drug use while containing infectious disease transmission and overdose mortality: instead of distributed, under-resourced enforcement against individuals, establishes centralized public health infrastructure (needle programs, supervised sites, treatment linkage, overdose response) that reaches users outside criminal justice channels and improves measurable population health outcomes.
% TRANSFER_FUNCTION: Moves resources (public funds, service provision capacity) from criminal enforcement toward health services; transfers authority over drug use from law enforcement to public health; transfers the obligation to manage health harms from individuals to the public health system. Users transfer from victim status in the criminal justice system (arrest, incarceration, criminal record) to victim status in the health system (disease exposure, overdose risk, addiction), but with decriminalized access to harm reduction and treatment services.
% ABSENT_VOICES: Political prohibition constituencies (who would argue drug use should remain criminalized to protect communities and deter addiction) are structurally excluded from the public health authority's legitimacy frame. Neighbors experiencing street-level externalities at service sites have limited voice in site placement decisions. People with opioid use disorder who have achieved abstinence-based recovery (and may view harm reduction as antithetical to recovery) are often marginalized in harm reduction program design.
% DISAPPEARANCE_RATIONALE: If the harm reduction authority and decriminalized framework disappeared overnight, law enforcement would resume arrests for possession, treatment capacity would shift from harm reduction services to incarceration, infectious disease control would collapse (no needle programs, no supervised sites), and overdose deaths would rise sharply. The health and criminal justice systems would reorganize around enforcement; public health resources would redirect to post-hoc emergency response rather than prevention.
% FOUNDING_PROBLEM: Two connected crises: (1) mass incarceration for drug possession was consuming law enforcement and correctional resources without reducing drug use; (2) infectious disease (HIV, hepatitis C) was spreading through injection-drug populations without effective prevention pathways. Criminalization created barriers to public health intervention — users avoided services to evade law enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The public health authority and harm reduction providers attest both crises are ongoing (new viral variants spreading, incarceration still consuming resources). Epidemiological data from jurisdictions with established harm reduction programs corroborates disease prevalence reduction. Law enforcement in harm-reduction jurisdictions attests resource relief from reduced drug enforcement. Political prohibition advocates dispute whether harm reduction adequately addresses public safety and social order concerns.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint transfers significant costs to people using drugs (managed health risks, service-defined identity) and neighbors (visible externalities, property impacts) while providing genuine coordination benefits (disease prevention, overdose survival). The metric rises slightly over the interval (0.48 to 0.58) as the service infrastructure matures and institutional capture becomes visible — early phases focus on health outcomes; later phases show bureaucratic mission drift where service providers' interests (maintaining funding, expanding programs) begin to diverge from user autonomy and treatment outcomes. Suppression is moderate (0.42) because the constraint requires active suppression of criminalization (police must refrain from arrests, resist institutional pressure for enforcement) but does not require high coercive force against users themselves — the identity-locking operates through service dependency rather than direct coercion. Theater rises slowly (0.18 to 0.28) as the constraint matures: early phases emphasize genuine public health rationale; later phases introduce more performative elements (ceremonial community engagement, media-focused naloxone distributions) that operate alongside core service delivery. Accessibility collapse is substantial (0.62 baseline) because alternatives to the managed health framework close: once users engage with services, criminal alternatives (self-management, unregulated supply) become less viable; the service system becomes structurally difficult to exit without facing health risks. Resistance is high (0.71) because political opposition from prohibition constituencies remains powerful and organized, and neighbors mount sustained NIMBY campaigns against site placement — the constraint survives despite substantial organized resistance.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat and the people-using-drugs seat should compute very differently. From the authority's position, the constraint is legitimate coordination (managing a genuine public health crisis through evidence-based services); it computes as rope-like in the authority's operational frame. From the powerless-trapped-identity-locked seat of people using drugs, the same structure operates as enforced, beneficially paternalistic but still extraction-bearing — users must enter service infrastructure, accept the identity of 'person in treatment,' and submit to program rules in exchange for accessing harm reduction. The engine computes this divergence from directionality: the authority has institutional power and arbitrage exit (can shift to enforcement if political pressure rises); people using drugs have powerless status and identity-locked exit (cannot exit without facing health/social consequences). These structural differences produce different d values and thus different per-seat classifications. Law enforcement occupies an intermediate position: they benefit from resource relief (lower drug-arrest volume) but pay through institutional authority reduction (cannot use drug-law enforcement as a tool) and must defend decriminalized spaces.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: people using drugs benefit from decriminalization (exit criminal justice) but remain victims of health costs; public health system benefits from mandate and resources; law enforcement benefits from resource relief. Victims: people using drugs (health burdens, identity-locking); neighbors of sites (externalities, property impact); emergency services (increased demand volume). The constraint is tangled because the same population (people using drugs) is both beneficiary (decriminalized) and victim (serviced/identity-locked). Directionality for the authority is low-d (beneficiary: institutional mandate, budgets, legitimacy); directionality for people using drugs is middle-d (both beneficiary from decriminalization and victim from identity-locking and health burden); directionality for neighbors is high-d (targets of externality, no exit). The identity-locking mechanism is critical: users cannot simply refuse services without facing health consequences (overdose, infection) that make exit costly; the constraint locks them into a 'managed person' identity within the public health system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids Piton classification (mere theater) because genuine coordination function persists: disease prevention through needle programs and supervised consumption sites produces measurable epidemiological benefit; overdose response infrastructure directly reduces mortality. However, the constraint shows mandatrophy risk as the interval progresses: (1) the founding problem (incarceration overload, disease transmission) shows only partial resolution — drug use persists, political opposition remains organized; (2) service providers' institutional interests (program expansion, budget growth, professional authority) begin to diverge from the founding mandate (user autonomy, treatment outcomes); (3) theater ratio rises, suggesting performative elements ('harm reduction' framing used to justify service-provider authority) begin to displace functional elements. The six-questions mismatch (founding_problem_status=live, disappearance_verdict=world_rearranges) confirms the constraint has not become vestigial — removing it would cause substantial rearrangement. But the slight extraction rise over the interval signals early-stage extraction drift: the constraint may be beginning to function as a rationalizing cover story for health system expansion and user-management authority, not merely as a legitimate response to the founding problem. This is pre-Piton (still substantially functional) but shows the trajectory toward Piton if extraction continues rising and theater continues growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locking_reversibility,
    'Is the identity-locking of people using drugs through service dependency structurally reversible, or does it create path dependence that prevents exit to independent self-management?',
    'Longitudinal tracking of users who exit services: do they maintain abstinence or return to use without service support? Post-exit survey data on whether service labels (person in treatment, person in recovery) persist in self-identification and community perception.',
    'If reversible, identity-locking is a manageable coordination cost (users can leave and rebuild non-service-defined identity). If path-dependent, the constraint''s effective suppression and extraction are higher than authored — the service infrastructure creates dependency bonds that exceed voluntary participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_reversibility, empirical, 'Whether service-defined identity can be shed post-exit or persists as limiting internalization.').

omega_variable(
    service_provider_mission_capture,
    'As harm reduction services mature and institutionalize, do institutional interests (budget growth, professional autonomy, program expansion) capture the original health mandate, such that services become tools of user management rather than user health optimization?',
    'Comparative organizational analysis of early-phase (5-year) vs. late-phase (15+ year) harm reduction programs: tracking of budget allocation shifts, staff professional identity consolidation, and user satisfaction/autonomy metrics over program lifecycle.',
    'Evidence of capture would reclassify the constraint from Tangled Rope toward Snare or Piton (extraction becomes primary; coordination becomes cover story). Evidence of sustained user-centered mandate would confirm Tangled Rope with high coordination integrity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(service_provider_mission_capture, empirical, 'Whether institutional mission remains user-health-centered or drifts toward provider-interest-centered.').

omega_variable(
    neighbor_externality_justice,
    'Is the distribution of site externalities (concentration in lower-income neighborhoods with less political power) a feature of the harm reduction constraint itself, or a distributional failure upstream of the constraint?',
    'Geographic analysis of site placement: are sites placed proportionally across neighborhoods by population and service demand, or concentrated in politically powerless areas? Comparative analysis across harm-reduction jurisdictions to identify if placement reflects evidence-based epidemiology or political feasibility.',
    'If the constraint itself (harm reduction mechanism) requires externality concentration to function, then the constraint is structurally inequitable and extraction from ''neighbors'' should be weighted as structurally necessary rather than incidental. If externality concentration reflects political choice upstream of the constraint, then the constraint could function with equitable distribution (the failure is in governance, not in the harm reduction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neighbor_externality_justice, conceptual, 'Whether concentrated externalities are intrinsic to harm reduction or artifacts of implementation justice failures.').

omega_variable(
    kernel_reading_scope,
    'Does this harm_reduction_reading of the substance_control_authority kernel describe a necessary decomposition from sibling readings, or could the readings coexist in a framework that accommodates both harm reduction and prohibition?',
    'Textual and institutional analysis: examining whether jurisdictions holding a strong harm reduction mandate simultaneously maintain criminal penalties for drug trafficking (not possession/use) or whether the reading''s core premise (decriminalization of use) logically forecloses the prohibition reading''s core premise (criminalization as protective enforcement).',
    'If the readings logically foreclose each other, the constraint is a clean kernel decomposition and the sibling readings are genuinely alternative systems. If jurisdictions successfully hold both (decriminalized possession + criminalized trafficking), then the readings may not fully decompose, and the boundary between them is more permeable than the kernel analysis suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether harm reduction logically forecloses prohibition or whether both can coexist in mixed regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__harm_reduction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__harm_reduction_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__harm_reduction_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__harm_reduction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__harm_reduction_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__harm_reduction_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__harm_reduction_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__harm_reduction_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__harm_reduction_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(subs_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(subs_grid_01, substance_control_authority__harm_reduction_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(subs_grid_02, substance_control_authority__harm_reduction_reading, accessibility_collapse(class), 25, 0.6).
narrative_ontology:measurement(subs_grid_03, substance_control_authority__harm_reduction_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(subs_grid_04, substance_control_authority__harm_reduction_reading, accessibility_collapse(individual), 25, 0.68).
narrative_ontology:measurement(subs_grid_05, substance_control_authority__harm_reduction_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(subs_grid_06, substance_control_authority__harm_reduction_reading, accessibility_collapse(organizational), 25, 0.62).
narrative_ontology:measurement(subs_grid_07, substance_control_authority__harm_reduction_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(subs_grid_08, substance_control_authority__harm_reduction_reading, accessibility_collapse(structural), 25, 0.58).
narrative_ontology:measurement(subs_grid_09, substance_control_authority__harm_reduction_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(subs_grid_10, substance_control_authority__harm_reduction_reading, resistance(class), 25, 0.76).
narrative_ontology:measurement(subs_grid_11, substance_control_authority__harm_reduction_reading, resistance(individual), 0, 0.65).
narrative_ontology:measurement(subs_grid_12, substance_control_authority__harm_reduction_reading, resistance(individual), 25, 0.68).
narrative_ontology:measurement(subs_grid_13, substance_control_authority__harm_reduction_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(subs_grid_14, substance_control_authority__harm_reduction_reading, resistance(organizational), 25, 0.74).
narrative_ontology:measurement(subs_grid_15, substance_control_authority__harm_reduction_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(subs_grid_16, substance_control_authority__harm_reduction_reading, resistance(structural), 25, 0.68).
narrative_ontology:measurement(subs_grid_17, substance_control_authority__harm_reduction_reading, stakes_inflation(class), 0, 0.22).
narrative_ontology:measurement(subs_grid_18, substance_control_authority__harm_reduction_reading, stakes_inflation(class), 25, 0.26).
narrative_ontology:measurement(subs_grid_19, substance_control_authority__harm_reduction_reading, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(subs_grid_20, substance_control_authority__harm_reduction_reading, stakes_inflation(individual), 25, 0.35).
narrative_ontology:measurement(subs_grid_21, substance_control_authority__harm_reduction_reading, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(subs_grid_22, substance_control_authority__harm_reduction_reading, stakes_inflation(organizational), 25, 0.32).
narrative_ontology:measurement(subs_grid_23, substance_control_authority__harm_reduction_reading, stakes_inflation(structural), 0, 0.18).
narrative_ontology:measurement(subs_grid_24, substance_control_authority__harm_reduction_reading, stakes_inflation(structural), 25, 0.25).
narrative_ontology:measurement(subs_grid_25, substance_control_authority__harm_reduction_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(subs_grid_26, substance_control_authority__harm_reduction_reading, suppression(class), 25, 0.45).
narrative_ontology:measurement(subs_grid_27, substance_control_authority__harm_reduction_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(subs_grid_28, substance_control_authority__harm_reduction_reading, suppression(individual), 25, 0.32).
narrative_ontology:measurement(subs_grid_29, substance_control_authority__harm_reduction_reading, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(subs_grid_30, substance_control_authority__harm_reduction_reading, suppression(organizational), 25, 0.38).
narrative_ontology:measurement(subs_grid_31, substance_control_authority__harm_reduction_reading, suppression(structural), 0, 0.38).
narrative_ontology:measurement(subs_grid_32, substance_control_authority__harm_reduction_reading, suppression(structural), 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three kernel readings of substance_control_authority. The harm_reduction_reading (this file) decriminalizes use and establishes health services as the primary mechanism. The prohibition_reading instantiates the kernel via criminalization (affects this constraint by competing for state authority). The legalization_reading instantiates the kernel via commercial markets (affects this constraint by offering market-based alternative to service-based coordination). All three readings share the same kernel ('state authority to manage drug use') but differ structurally in scope, mechanism, and extraction profile. Network edges run between all three — each reading influences the institutional landscape and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, powerless, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
