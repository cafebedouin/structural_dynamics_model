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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Harm Reduction Public Health Model for Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading constitutes substance use as a health
 *   condition requiring pragmatic intervention to minimize overdose death and
 *   disease transmission, independent of cessation goals. Users are
 *   decriminalized (exit the criminal victim set) but remain subject to
 *   paternalistic health monitoring and mandatory or coercive treatment
 *   engagement. Enforcement against users recedes; enforcement against supply
 *   workers does NOT. The constraint coordinates public health systems around
 *   epidemiological observables (overdose mortality, viral transmission)
 *   while extracting authority over users' health trajectories and
 *   maintaining supply-side criminalization. This is a kernel reading: the
 *   same contested domain (substance use governance) is read differently by
 *   prohibition (moral transgression, state punishment), legalization
 *   (individual liberty, minimal state), and harm reduction (health
 *   condition, pragmatic intervention). This story instantiates the harm
 *   reduction reading only—the other readings are separate constraints in the
 *   same kernel family.
 *
 * KEY AGENTS:
 *   - people_with_substance_use_disorder: primary targets of paternalistic intervention; decriminalized but identity-locked to the regulated health condition
 *   - public_health_institutions: agenda-setters who define harm reduction protocols, set intervention priorities, claim epidemiological authority
 *   - healthcare_providers: beneficiaries who operate treatment infrastructure and capture funding streams from medicalization
 *   - criminalized_supply_workers: trapped payers whose enforcement burden is NOT lifted under this reading (supply criminalization persists)
 *   - prohibition_regime_apparatus: excluded institutional actor whose jurisdiction and budget are directly threatened by user decriminalization
 *   - epidemiological_science: non-agent beneficiary vindicated by the reading's operation and data generation
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
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Public Health Model for Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '566b63e4-c85e-4f8a-81f5-97898fc996f8').
narrative_ontology:cs_kernel_codification('566b63e4-c85e-4f8a-81f5-97898fc996f8', distributed).
narrative_ontology:cs_authority_grounding('566b63e4-c85e-4f8a-81f5-97898fc996f8', distributed).
narrative_ontology:cs_reading_relation('566b63e4-c85e-4f8a-81f5-97898fc996f8', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('566b63e4-c85e-4f8a-81f5-97898fc996f8', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('566b63e4-c85e-4f8a-81f5-97898fc996f8', foundational, substance_use_is_health_condition_not_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition_not_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('566b63e4-c85e-4f8a-81f5-97898fc996f8', substance_use_is_health_condition_not_moral_transgression, empirically_contingent).
narrative_ontology:cs_axiom('566b63e4-c85e-4f8a-81f5-97898fc996f8', foundational, paternalistic_health_intervention_justified_by_harm_reduction_outcomes).
narrative_ontology:cs_axiom_status(paternalistic_health_intervention_justified_by_harm_reduction_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('566b63e4-c85e-4f8a-81f5-97898fc996f8', paternalistic_health_intervention_justified_by_harm_reduction_outcomes, instrumental).
narrative_ontology:cs_reference_frame('566b63e4-c85e-4f8a-81f5-97898fc996f8', evidence_based_health_governance_framework).
narrative_ontology:cs_drift_state('566b63e4-c85e-4f8a-81f5-97898fc996f8', contemporary_supply_legalization_challenge, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('566b63e4-c85e-4f8a-81f5-97898fc996f8', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_institutions).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, healthcare_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, epidemiological_knowledge_base).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, criminalized_supply_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, pharmaceutical_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to mandatory health interventions—supervised consumption sites, medication-assisted treatment (MAT), needle exchange, disease screening—framed as health services rather than criminal punishment. They benefit from reduced overdose mortality, disease prevention, and decriminalization that removes criminal justice entanglement. They bear the cost of paternalistic regulation: mandatory medical engagement, state surveillance of their consumption patterns, compulsory treatment referrals, and loss of autonomy over their own health decisions. Identity is fused to the substance use condition by stigma, isolation, and institutional definition; exit means abstinence or relocating to a jurisdiction with different policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_with_substance_use_disorder, beneficiary).

% Design and implement the harm reduction framework, setting policy priorities, defining who counts as 'in need of intervention,' and claiming authority over substance use as a medical rather than criminal matter. They benefit from expanded institutional budgets, professional prestige tied to the coordination function, and data-generation that vindicates epidemiological models of addiction. They set the terms of engagement—which interventions are offered, mandatory vs. voluntary, the thresholds that trigger involvement, and the performance metrics that measure success (overdose reduction, disease transmission, treatment engagement rates).
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate treatment programs, dispensaries, supervised consumption sites, and medication-assisted treatment clinics. They collect funding streams created by the harm reduction framework (government contracts, insurance reimbursement for MAT, public health grants). They benefit from medicalization of substance use—a stable patient population, funding predictability, and professional authority. Their exit is to relocate to other healthcare markets; the constraint does not trap them individually, though the institutional ecosystem depends on their compliance with harm reduction protocols.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, healthcare_providers, beneficiary,
    powerful, biographical, mobile, national).

% Continue to operate outside legality; the harm reduction reading does NOT decriminalize supply. They bear the extraction: criminal prosecution, asset seizure, incarceration remain the state's tools against supply-side actors. Harm reduction *decouples* supply criminalization from consumption criminalization, leaving supply workers trapped in a now-even-more-visible illegal status (users are protected; suppliers are not). They have no exit—relocation is deportation or prison.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminalized_supply_workers, payer,
    powerless, immediate, trapped, local).

% In jurisdictions that maintain prohibition rather than harm reduction, users remain criminalized and subject to criminal justice enforcement. They are excluded from harm reduction's coordination benefits but also from its paternalistic regulation. Their presence frames the reading—harm reduction's claim depends on contrast with prohibition-era criminalization of users. Policy migration from prohibition to harm reduction does NOT include their consent; the shift happens to them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminalized_users_in_prohibition_jurisdictions, excluded,
    powerless, biographical, trapped, national).

% Manufactures and distributes medications central to MAT (buprenorphine, methadone, naloxone). They benefit from government procurement, insurance coverage expansion, and the institutional commitment to medication-based intervention. Their profit stream depends on the state's role as service provider rather than exclusive enforcer; they capture the coordination function's value-added. Exit is to other markets; the constraint does not trap them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, pharmaceutical_industry, beneficiary,
    powerful, generational, mobile, global).

% Operate outside government but advocate for and sometimes operate harm reduction services (NGOs, peer-led programs, harm reduction coalitions). They observe the constraint from the standpoint of advocacy: they see themselves as pushing institutional health systems toward genuine user-centered care. The constraint they inhabit is the one *implemented* by public health institutions—often more paternalistic and less user-directed than the advocates' own framings. Their role bridges agenda-setting and excluded: they have voice in policy but no veto; policy implementations can diverge sharply from advocacy positions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_advocates, observer,
    moderate, generational, mobile, national).

% The harm reduction reading vindicates addiction epidemiology as a legitimate frame for understanding substance use. The constraint's operation generates data streams (overdose mortality, disease transmission, treatment enrollment, drug purity) that confirm the epidemiological models and justify continued investment in the research program. This is a non-agent beneficiary: epidemiology does not collect rents, but the constraint's operation proves epidemiology's utility and drives funding to epidemiological research.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, epidemiological_science, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_kernel__harm_reduction_reading, epidemiological_science).

% Law enforcement, drug courts, incarceration infrastructure designed under the prohibition reading. Harm reduction's decriminalization of users directly threatens this apparatus's jurisdiction and budget. Under harm reduction, police shift from enforcement to service referral, prosecutors lose cases, incarceration facilities lose a population segment, and the criminal justice system's historical claim to substance use governance erodes. This apparatus would advocate for return to prohibition if given voice; it is excluded by policy choice, not by structural necessity.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_regime_apparatus, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_institutions).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent overdose death and disease transmission (viral hepatitis, HIV) among people using substances by offering supervised consumption, medication-assisted treatment, and risk-reduction supplies without requiring cessation as a precondition. Shift the observable from criminalized individuals to epidemiological metrics (overdose rates, disease incidence, treatment access).
% TRANSFER_FUNCTION: Moves authority over substance use from criminal justice to public health; moves funding from incarceration budgets to healthcare and harm reduction services. Users lose criminal liability (extraction recedes); supply workers remain criminalized. State becomes service provider and epidemiological monitor rather than punisher of users. Paternalistic regulation of users' health trajectories replaces criminal punishment.
% ABSENT_VOICES: Users themselves are substantially excluded from defining what 'harm reduction' means and which interventions are mandatory vs. optional—the reading is authored by public health experts, not by people with lived experience in substance use. Prohibitionists (law enforcement, some victim advocates citing drug-related crime) are excluded from policy-setting but would argue for criminalization's continued necessity. Supply-side workers have no representation and no pathway to legalization under this reading.
% DISAPPEARANCE_RATIONALE: If harm reduction policy disappeared overnight, jurisdictions would either revert to prohibition (criminalization of users, incarceration ramp-up, enforcement intensification) or pivot to legalization (user autonomy, supply regulation, tax collection). The substitution would be rapid and consequential: overdose mortality would spike if reversion to prohibition occurred; criminal justice caseloads would surge; healthcare systems would lose treatment infrastructure. The constraint's existence makes both user decriminalization and paternalistic health governance structurally dependent on its persistence.
% FOUNDING_PROBLEM: Prohibition-era drug policy criminalized users while failing to suppress supply or reduce use; incarceration, HIV transmission, and overdose mortality soared. Harm reduction was developed to address the empirical failure of prohibition: given that people use substances despite criminalization, reduce the harms (overdose, disease) that policy enforcement itself exacerbated.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists, harm reduction practitioners, and independent researchers document that prohibition-era criminalization increased HIV transmission in injection communities, overdose mortality, and incarceration without reducing substance use prevalence (peer-reviewed studies from the Lancet, JAMA, American Journal of Public Health spanning three decades). Law enforcement and some victim advocates contest the framing, arguing that drug enforcement reduces secondary crime and that harm reduction incentivizes continued use. Corroboration comes from outside the benefiting parties (public health institutions): the epidemiological case is developed by international research communities, not by the institutional agenda-setters, though institutions now implement the research conclusions.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness trajectory (0.38 → 0.62) models the reading's development from an alternative practice (supervised injection sites, needle exchange) into institutionalized public health policy with expanding surveillance and mandated engagement. Early extractiveness is lower because the reading began as harm reduction coalitions offering services without state enforcement; as public health institutions adopted the reading and formalized it into policy, extractiveness rose—the paternalistic health regulation deepened (mandatory treatment referrals, state-monitored consumption sites, epidemiological tracking of users). Suppression falls over the same interval (0.65 → 0.41) because the reading replaces criminal enforcement of users with therapeutic engagement: users can access services without fear of arrest, which reduces the active suppressive force. Theater rises modestly (0.12 → 0.28) because as the constraint matures, more enforcement activity is directed at defending supply-side criminalization while presenting the user-facing apparatus as pure health service—the gap between the epidemiological coordination function and the supply-side extraction widens. Accessibility collapse is moderate (0.48): users cannot exit the regulated health system without relocating, but alternatives (prohibition, legalization) remain live in neighboring jurisdictions. Resistance is high (0.72) because people with substance use disorders mount ongoing resistance to paternalistic regulation, supply-side workers organize to resist continued criminalization, and prohibitionists resist the decriminalization of users. The claim is tangled_rope because the reading genuinely coordinates public health (epidemiological observability, disease prevention) AND extracts authority over users' health decisions (mandatory engagement, paternalistic regulation).
 *
 * PERSPECTIVAL GAP:
 *   The public health institution seat experiences this as pure coordination—a functioning health system addressing a population-level problem. The user seat experiences it as extraction: decriminalization is genuine benefit, but paternalistic regulation of their health decisions is a cost they did not choose to bear. The supply worker seat experiences it as pure extraction: their criminalization persists and may deepen (law enforcement now has precise epidemiological data on where users gather, enabling targeted raids on supply networks). The prohibition apparatus seat experiences it as displacement: the constraint removes their jurisdiction over users without replacing it with legalization (which would at least offer a different rule set)—harm reduction leaves law enforcement without a user-side role but still responsible for supply-side enforcement, a bifurcated jurisdiction that erodes their institutional power. The engine computes these divergences from power, exit, and role differences; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   People with substance use disorder: powerless + identity-locked exit + payer role = high directionality toward extraction (d near 1.0), though they also benefit from decriminalization (which moderates d toward the symmetric zone). Public health institutions: institutional power + beneficiary role + arbitrage exit = low directionality (d near beneficiary end, ~0.15–0.25), collected via claim of coordination authority. Healthcare providers: powerful + beneficiary role + mobile exit = low directionality (~0.20–0.30). Criminalized supply workers: powerless + trapped exit + payer role = highest directionality (~0.95), no beneficiary offset. Supply workers subsidize the entire system: their continued criminalization funds enforcement apparatus and surveillance infrastructure that protects supply workers' elimination. The paternalistic extraction from users is partial—decriminalization removes criminal extraction, but therapeutic coercion substitutes paternalistic extraction. The supply-side extraction is total: no coordination benefit, no decriminalization, no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm reduction reading's founding problem (prohibition's empirical failure: increased mortality and disease transmission) remains contested but live. The coordination function (disease prevention via supervised consumption, medication provision) is real and measurable. The extraction (paternalistic health governance, mandatory engagement, epidemiological surveillance of users) is the mechanism by which the coordination is enforced—it is NOT disguised or vestigial. This is tangled_rope, not piton or snare: the coordination function generates genuine public health benefit (overdose mortality reduction, viral suppression); the extraction is the active cost users pay for that benefit. The bifurcation of supply criminalization means some actors (supply workers) are pure snare—no coordination benefit, only extraction—while the same constraint simultaneously offers rope to users (decriminalization gain, health coordination). This is possible because the constraint has multiple stakeholders with structurally different relationships; tangled_rope at the story level accommodates the per-seat divergence (the engine computes per-seat types, revealing the internal asymmetry).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_decriminalization_benefit_tradeoff,
    'Does the paternalistic health intervention of the harm reduction reading extract more from users than the decriminalization benefit provides, or are they genuinely balanced?',
    'Post-implementation measurement of user autonomy loss (mandatory treatment refusal penalties, surveillance burden, coercive engagement) against mortality reduction gains; comparison with legalization jurisdictions offering comparable mortality reduction without paternalistic regulation.',
    'If paternalism exceeds the decriminalization gain, the reading should be reclassified as snare at the user seat (pure extraction). If balanced, tangled_rope holds. If decriminalization gain exceeds paternalism, the reading shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_decriminalization_benefit_tradeoff, empirical, 'Whether decriminalization benefit and paternalistic cost are actually balanced in lived experience.').

omega_variable(
    supply_criminalization_necessity,
    'Is supply-side criminalization structurally necessary to the harm reduction reading''s coordination function, or is it an retained vestige from prohibition?',
    'Natural experiment from jurisdictions that decouple harm reduction (user services) from supply criminalization (regulated, legal supply); measurement of whether epidemiological outcomes (overdose, disease) improve, worsen, or remain stable.',
    'If supply legalization maintains or improves coordination outcomes, supply criminalization is an unjustified extraction (snare), not a tangled_rope component. If supply legalization destabilizes the coordination function, the criminalization is necessary to the reading (justified extraction, tangled_rope). If outcomes remain unchanged under either regime, supply criminalization is a piton—theatrical maintenance of a vestigial enforcement function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_criminalization_necessity, empirical, 'Whether supply-side criminalization is integral to harm reduction''s public health function or a retained prohibition-era artifact.').

omega_variable(
    user_voice_exclusion_in_reading_definition,
    'Is the harm reduction reading''s definition of ''harm'' and ''pragmatic intervention'' authored by people with lived substance use experience, or primarily by public health experts and institutions?',
    'Systematic review of who authors harm reduction policy in a jurisdiction; comparison of user-defined priorities (e.g., supply safety, access to chosen medications, non-coercive engagement) with institutional-defined priorities (e.g., overdose reduction, disease prevention, treatment enrollment).',
    'High expert-authorship without user voice suggests the reading''s beneficiary claim (public health institutions) may be capturing the definition of ''harm reduction'' itself. The constraint''s claimed coordination function may be real, but the reading''s articulation of it excludes the stakeholder it most affects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_voice_exclusion_in_reading_definition, conceptual, 'Whether harm reduction is authored by affected populations or imposed by experts.').

omega_variable(
    kernel_reading_contestation,
    'Which sibling reading (prohibition or legalization) most directly challenges harm reduction''s core premise that the state should paternalistically intervene in health decisions independent of cessation?',
    'Logical mapping: prohibition reading challenges the decriminalization half; legalization reading challenges the paternalistic intervention half; neither directly contradicts the empirical premise that harm reduction reduces mortality and disease.',
    'Helps clarify whether the contest is empirical (different readings agree on the facts but differ on values) or foundational (different readings disagree on what constitutes the problem). Informs whether the readings coexist (different parties, same facts) or foreclose each other (contradictory foundational claims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural relationship of harm reduction reading to its sibling readings in the substance_control_kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__harm_reduction_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__harm_reduction_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__harm_reduction_reading, suppression_requirement, 25, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel constraint family decomposes the single contested concept 'substance use policy' into three structurally distinct constraints, one per reading. Each reading instantiates a different ε (harm reduction: moderate extraction via paternalism; prohibition: high extraction via criminalization; legalization: low extraction via market regulation), different beneficiary/victim sets, and different enforcement mechanisms. The readings coexist as live policy options in different jurisdictions and advocacy communities; none logically forecloses the others at the foundational level (each differs in empirical premises and normative commitments). The harm_reduction_reading (this file) decouples user criminalization from supply criminalization while introducing paternalistic health governance; it is upstream of policy legalization attempts (which often invoke harm reduction evidence) but downstream of prohibition regimes (which it explicitly rejects). Links: harm_reduction_reading -> prohibition_reading (coexists), legalization_reading (coexists).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
