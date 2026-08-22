% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Substrate Control (Health Paternalism + Supply Criminalization)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   Harm reduction is one reading of the contested kernel 'what is substance
 *   use and how should the state respond?' Under this reading, substance use
 *   is a health condition (not moral transgression, not purely individual
 *   choice matter). The state's role is pragmatic health intervention: reduce
 *   overdose mortality, disease transmission, and social harms; engage users
 *   in managed care; decriminalize possession; criminalize supply to preserve
 *   public health authority. Users are moved from criminal defendants to
 *   health clients, but this shift embeds them in lifelong paternalistic
 *   management. The constraint coordinates public health response but
 *   extracts from users through identity fusion with treatment status and
 *   from supply operators through continued criminalization. The claim is
 *   Tangled Rope: genuine coordination function (public health response) +
 *   asymmetric extraction (users and suppliers bear costs for health
 *   authority growth; public health institutions benefit from expanded
 *   mandate).
 *
 * KEY AGENTS:
 *   - people_with_substance_use_disorder: Primary targets of the constraint; decriminalized but identity-locked to managed care
 *   - public_health_institutions: Agenda setter; benefits from expanded authority and treatment infrastructure
 *   - harm_reduction_service_providers: Beneficiaries; gain professional authority and stable funding
 *   - informal_supply_operators: Victims; remain criminalized under this reading
 *   - criminal_justice_system: Shifted role; maintains surveillance/coercion authority via treatment conditions
 *   - legalization_advocates: Excluded; argue for user autonomy
 *   - prohibition_advocates: Excluded; argue for abstinence-focused coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.41).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Substrate Control (Health Paternalism + Supply Criminalization)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '88b25551-3ece-4d42-b6ce-85ee2fabd956').
narrative_ontology:cs_kernel_codification('88b25551-3ece-4d42-b6ce-85ee2fabd956', distributed).
narrative_ontology:cs_authority_grounding('88b25551-3ece-4d42-b6ce-85ee2fabd956', distributed).
narrative_ontology:cs_reading_relation('88b25551-3ece-4d42-b6ce-85ee2fabd956', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('88b25551-3ece-4d42-b6ce-85ee2fabd956', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('88b25551-3ece-4d42-b6ce-85ee2fabd956', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('88b25551-3ece-4d42-b6ce-85ee2fabd956', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('88b25551-3ece-4d42-b6ce-85ee2fabd956', foundational, health_paternalism_justified_by_harm_evidence).
narrative_ontology:cs_axiom_status(health_paternalism_justified_by_harm_evidence, holdable).
narrative_ontology:cs_axiom_grounding('88b25551-3ece-4d42-b6ce-85ee2fabd956', health_paternalism_justified_by_harm_evidence, instrumental).
narrative_ontology:cs_axiom('88b25551-3ece-4d42-b6ce-85ee2fabd956', secondary, user_decriminalization_supply_criminalization).
narrative_ontology:cs_axiom_status(user_decriminalization_supply_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('88b25551-3ece-4d42-b6ce-85ee2fabd956', user_decriminalization_supply_criminalization, instrumental).
narrative_ontology:cs_reference_frame('88b25551-3ece-4d42-b6ce-85ee2fabd956', public_health_authority_harm_reduction_mandate).
narrative_ontology:cs_drift_state('88b25551-3ece-4d42-b6ce-85ee2fabd956', contemporary_expansion_of_managed_care_scope, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88b25551-3ece-4d42-b6ce-85ee2fabd956', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_institutions).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, treatment_infrastructure).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, informal_supply_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to health paternalism: mandatory treatment offers, surveillance via health systems, required participation in managed care to access medication-assisted treatment (MAT), needle exchange programs, overdose prevention services. Simultaneously, they are decriminalized at point of use — possession of small quantities for personal consumption is treated as health matter, not criminal offense. However, they remain trapped in the constraint because identity as 'person with addiction' is fused with the care system itself (treatment is the only path to managed use); exit would mean unmanaged use with higher overdose risk. They benefit from harm reduction services (overdose prevention, infection prophylaxis, MAT access) but at cost of lifelong managed status and intimate state surveillance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder, beneficiary).

% Set the harm reduction agenda: define what counts as acceptable intervention (MAT, naloxone distribution, supervised consumption sites, testing services), allocate funding to treatment and prevention, conduct surveillance of overdose mortality and disease transmission, and adjudicate which substances and which forms of use qualify as 'health problems' requiring intervention. They frame the arrangement as scientific and compassionate—evidence-based health response rather than punishment. They directly benefit from the growth in treatment infrastructure funding and institutional authority over addiction management.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% NGOs, clinics, and community health workers who operate needle exchange, overdose prevention sites, MAT clinics, and peer support services. They benefit from stable funding streams tied to harm reduction mandates, and they gain professional authority as experts in the field. Their work is meaningful and evidence-based, but their institutional survival depends on maintaining the harm reduction framing of substance use as a health problem requiring managed intervention rather than individual choice.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    moderate, biographical, constrained, national).

% Street-level dealers, darknet vendors, unlicensed manufacturers of non-pharmaceutical substances. Under harm reduction reading, they remain criminalized—supply chain interdiction continues, manufacturing penalties remain severe, distribution is felony. The constraint decriminalizes use and treats users as health clients, but criminalizes supply to preserve public health authority. These operators have no exit: their activity is illegal by definition under this reading.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, informal_supply_operators, payer,
    powerless, immediate, trapped, local).

% The system of MAT clinics, supervised consumption sites, naloxone distribution networks, harm reduction training, and epidemiological monitoring. It is not an agent but a reified outcome of the constraint—it grows in capacity and legitimacy under the harm reduction reading. It would not exist at this scale under prohibition (minimized treatment offered) or legalization (treatment becomes market service, not public health mandate).
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, treatment_infrastructure, beneficiary,
    moderate, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__harm_reduction_reading, treatment_infrastructure).

% Shifts from primary enforcement mechanism for user possession to supply chain interdiction and harm reduction surveillance. Police and prosecutors redirect prosecution toward suppliers; prisons redeploy capacity. Courts manage treatment-as-alternative-to-incarceration pathways. The system remains extractive (maintains arrest authority over users for violations of treatment conditions, probation conditions, or continued supply involvement) but reduces direct incarceration of users. CJ maintains control over coercive authority (treatment conditions, monitoring, breach penalties) while appearing to step back from punishment.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminal_justice_system, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, criminal_justice_system, payer).

% Argue that harm reduction is disguised paternalism—that users should decide their own risk management, and that treating use as a health problem requiring managed intervention is state overreach. They are excluded from setting the agenda (the harm reduction reading has institutional authority). Their position: decriminalization of supply and user autonomy in risk management would be more protective of freedom than health-mandated intervention.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, biographical, constrained, national).

% Argue that harm reduction normalizes addiction and that treatment should be coercive and abstinence-focused. They view supply criminalization alone as insufficient—users themselves should face arrest and mandatory abstinence-based treatment. They are excluded from the agenda under this reading (decriminalization of use is locked in). Their position: decriminalization enables continued substance use rather than driving treatment.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, biographical, constrained, national).

% The scientific data showing overdose mortality reduction, disease transmission reduction (HIV, hepatitis C), and treatment engagement rates under harm reduction versus abstinence-only or prohibition models. This evidence is cited as the legitimating frame for the entire reading: 'harm reduction works by the metrics we care about.' The evidence is interpreted through the reading itself (what counts as success is defined by harm reduction premises).
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, epidemiological_evidence, beneficiary,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__harm_reduction_reading, epidemiological_evidence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_institutions).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health response to substance use: centralizes treatment access, disease surveillance, overdose prevention, and medical management of addiction under unified health authority. Without the constraint, substance use management would fragment across underground markets (prohibition) or individualized medical/commercial markets (legalization), losing public health coordination of contagion risks and mortality.
% TRANSFER_FUNCTION: Transfers state authority and institutional legitimacy from criminal justice to public health: users are moved from criminal defendant status to health client status. Simultaneously, transfers resources (treatment funding, infrastructure investment) from enforcement budgets to health budgets, and transfers operational authority from police/prosecutors to clinicians and public health officials. Users remain subject to state intervention but the intervention mechanism shifts from punishment to paternalistic management.
% ABSENT_VOICES: People who use drugs autonomously (not engaged in treatment) are excluded—they are the implicit problem the system is designed to intervene on and bring into managed care. Legalization advocates (who would argue for user autonomy in risk management) are structurally excluded by the reading's own premises. Informal supply operators have no seat at the table. Low-income communities where enforcement often recedes unevenly are not represented in agenda-setting.
% DISAPPEARANCE_RATIONALE: If harm reduction infrastructure vanished overnight, overdose mortality would rise sharply (no naloxone distribution), disease transmission would accelerate (no needle exchange), treatment engagement would collapse (MAT clinics closed), and the arrangement of substance use management would revert either to prohibition (criminal enforcement surge) or fragmented legalization (market-based services). The entire coordination of public health response would reorganize.
% FOUNDING_PROBLEM: Injection drug use was causing epidemics of HIV and hepatitis C transmission; overdose mortality from heroin was rising; criminal punishment was failing to reduce use and was creating additional harms (incarceration, stigma, exclusion from treatment). Harm reduction emerged as a pragmatic public health response: keep people alive while managed, reduce disease transmission, increase treatment engagement by removing criminal barriers to accessing care.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists, harm reduction researchers, and addiction medicine physicians attest the founding problem remains live and that harm reduction interventions measurably reduce overdose mortality and disease transmission (comparative outcome data from harm reduction jurisdictions vs. prohibition-heavy jurisdictions). Legalization advocates contest the framing—arguing the problem is state overreach, not inadequate management. Prohibition advocates contest effectiveness, citing persistent use rates and arguing harm reduction enables continued addiction rather than driving recovery. Evidence base is cited by public health, but the interpretation of what counts as 'solving the problem' is reading-dependent.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint transfers authority and resources to health institutions while keeping users in managed status indefinitely (identity_locked exit prevents autonomous choice). The rising trajectory (0.45 → 0.62 over 40 years) models institutional mission creep: harm reduction infrastructure grows, managed care becomes more comprehensive, and the scope of 'health problems' requiring intervention expands (behavioral health, mental health co-morbidities, housing stability become conditions of treatment receipt). Suppression is moderate (0.41) because users are decriminalized at point of use, but this is offset by surveillance through health systems (data on prescribing, clinic attendance, urine screens) and coercive treatment conditions (breach penalties, probation conditions tied to treatment compliance). Theater is moderate-low (0.28) because the core public health interventions are evidence-based and functionally real (naloxone distribution demonstrably saves lives; MAT reduces relapse; needle exchange reduces disease transmission), but an increasing share of the constraint's operation is aesthetic ('treatment is the answer') rather than functional (users choosing to avoid treatment are sanctioned). Accessibility_collapse is moderate (0.48): alternatives exist (unmanaged use, black-market supplies, illicit manufacturing) but are risky and increasingly stigmatized once the health paternalism reading is institutionalized. Resistance is high (0.72): users resist treatment conditions, informal suppliers resist enforcement, legalization advocates and prohibition advocates both contest the reading's premises.
 *
 * PERSPECTIVAL GAP:
 *   From the public health institution's seat, harm reduction is clearly coordination—they solved a real epidemiological crisis (HIV transmission, overdose mortality) using evidence-based interventions. From the person with substance use disorder's seat, the same structure is extractive paternalism: decriminalization is offset by lifelong managed status, mandatory treatment participation, and intimate surveillance. From the informal supply operator's seat, it is pure snare—supply is criminalized while use is not, so the operator bears all enforcement costs for a market that public health claims to be managing. From the legalization advocate's seat, it is extractive overreach—users are denied autonomy in risk management. The engine computes these per-seat types from the structural data (power, exit_options, beneficiary/victim status). The authored claim (Tangled Rope) asserts genuine coordination function + extraction; the metrics (moderate-high ε, moderate suppression, moderate theater) are consistent with that claim; the per-seat computations will diverge (public health sits as beneficiary with low d; users sit as identity-locked payers with high d).
 *
 * DIRECTIONALITY LOGIC:
 *   Public health institutions: Beneficiary role, institutional power, arbitrage exit (they can redefine the harm reduction agenda, shift resources between interventions, appeal to evidence to resist pressure). Directionality near 0.0 (full beneficiary). People with SUD: Payer role, powerless, identity_locked exit (their survival depends on engagement with the constraint; they cannot exit without reputational/medical/legal consequences). Directionality near 1.0 (full target). Harm reduction service providers: Beneficiary role, moderate power, constrained exit (they need the harm reduction mandate to fund their work, but they could in principle shift to legalization-model work or prohibition-model work). Directionality near 0.2 (strong beneficiary). Supply operators: Payer role, powerless, trapped exit (their activity is illegal by definition; no pathway to legitimacy under this reading). Directionality at 1.0 (full target). The overrides are not needed—the structural derivation from roles + exit handles the directionality accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is LIVE for this constraint. The founding problem (HIV epidemics, overdose mortality from prohibition-era heroin) is substantially solved in harm reduction jurisdictions (disease transmission has fallen, overdose mortality is manageable with naloxone and MAT, treatment engagement is higher than under prohibition). Yet the constraint persists and expands (treatment infrastructure grows, managed care mandates become more comprehensive, supply criminalization is maintained). The six_questions data shows founding_problem_status = contested; prohibition advocates argue the problem was exaggerated or solved by other means, legalization advocates argue the problem is paternalism not inadequate management. The divergence between (founding problem solved / contested) and (disappearance_verdict = world_rearranges) suggests the constraint has accumulated secondary functions (institutional authority, funding streams, surveillance capability) that now sustain it independently of the founding health emergency. Theater_ratio is moderate (0.28) consistent with partial mandatrophy—the core public health interventions work, but increasing share of enforcement is theatrical (treatment as condition of liberty, surveillance as health management rather than disease prevention). The constraint should be monitored for the type drift that marks true mandatrophy: if theater_ratio rises above 0.5 while base_extractiveness plateaus, the arrangement has inverted (maintenance of authority, not health coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_autonomy_kernel_contest,
    'Is treating substance use as a health problem requiring managed intervention a legitimate reading of public health authority, or is it disguised paternalism that violates user autonomy?',
    'This ambiguity is CONSTITUTIVE of the kernel contest itself. The three readings (harm reduction, legalization, prohibition) each answer differently. No empirical resolution exists—the question is fundamentally about how much autonomy is owed to individuals and what role the state should play in managing risk on behalf of its citizens.',
    'This is the core axis of the reading: harm reduction asserts health paternalism is justified by public health evidence and outcomes; legalization asserts user autonomy overrides health paternalism; prohibition asserts moral authority overrides autonomy. The reading''s legitimacy depends on accepting the harm reduction answer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy_kernel_contest, preference, 'Whether health paternalism in substance use management is a legitimate expression of state authority or an illegitimate override of individual autonomy.').

omega_variable(
    supply_criminalization_persistence,
    'Why does harm reduction maintain criminal prohibition of supply while decriminalizing use? Is this structurally necessary or operationally convenient?',
    'Compare jurisdictions with supply decriminalization (Portugal, Switzerland partial models) to those with supply criminalization + use decriminalization. Measure whether public health outcomes (overdose mortality, disease transmission, treatment engagement) differ materially based on supply criminalization status.',
    'If supply decriminalization produces better public health outcomes, the continued supply criminalization is unwarranted paternalism. If outcomes are substantially equivalent, supply criminalization is revealed as enforcement convenience (maintaining police/prosecutor roles) rather than public health necessity. If outcomes are worse with supply decriminalization, supply criminalization is justified by empirical evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_criminalization_persistence, empirical, 'Whether supply criminalization is public health necessity or enforcement inertia.').

omega_variable(
    identity_lock_trajectory,
    'For individuals in long-term harm reduction management, does identity fusion with treatment (''person with addiction'' as permanent status requiring managed care) perpetuate dependence on the system, or does it stabilize harm and enable autonomous functioning within the constraint?',
    'Longitudinal studies of individuals exiting harm reduction systems: do they sustain autonomous unmanaged use, or do they return to treatment? Are there cohorts that achieve autonomous use without formal treatment engagement? Post-exit suppression measurements.',
    'If identity lock perpetuates system dependence beyond clinical necessity, the extraction component of the constraint is higher than the public health benefit suggests. If exit outcomes show harm escalates, identity lock is protective even if it feels confining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_trajectory, empirical, 'Whether lifelong managed status is necessary for harm reduction or is over-reach.').

omega_variable(
    reading_instantiation__harm_reduction_vs_sibling_readings,
    'This constraint instantiates harm reduction as a specific reading of the substance control kernel. The sibling readings (legalization, prohibition) are distinct constraints with different ε values, different beneficiary/victim structures, and different type classifications. What structural features of THIS reading distinguish it from the siblings?',
    'The reading defines its core commitments in cs_structure.axioms (health paternalism justified by evidence; user decriminalization with supply criminalization). The axioms are unique to this reading. Sibling readings hold different axioms. The engine computes per-reading type classifications from the structural data; divergence from the siblings is the measurement.',
    'This omega documents the committer-frame structure (Rule 2): routing the contest into omega variables rather than inventing fields. The contest is real, the readings are distinct, and the engine''s per-reading classification is the apparatus for measuring the structural differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation__harm_reduction_vs_sibling_readings, conceptual, 'Kernel contest: harm reduction reading vs. legalization and prohibition readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__harm_reduction_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__harm_reduction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__harm_reduction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__harm_reduction_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__harm_reduction_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__harm_reduction_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__harm_reduction_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__harm_reduction_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__harm_reduction_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(subs_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, opioid_supply_chain_criminalization).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, treatment_access_as_coercive_condition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest (substance_control_kernel). The harm_reduction_reading (this file) treats substance use as health condition with managed intervention; the legalization_reading treats it as individual liberty; the prohibition_reading treats it as moral transgression requiring punishment. Each reading has distinct ε, beneficiary/victim structure, and type. The readings coexist—different jurisdictions and different parties within jurisdictions hold different readings simultaneously. They are linked via network.affects_constraints because the validity of one reading undermines the others' legitimacy claims (if harm reduction works by public health metrics, prohibition's claim that criminalization is necessary fails; if legalization produces better autonomy outcomes, harm reduction's paternalism is overreach). The constraint family is: three kerneled readings + two derivative constraints capturing the institutional lock-in (treatment as coercive condition, supply criminalization persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
