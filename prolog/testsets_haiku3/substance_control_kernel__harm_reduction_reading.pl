% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction as Health Intervention (Substance Control Kernel)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading of the substance control kernel repositions
 *   substance use from a criminal transgression to a persistent health
 *   condition requiring pragmatic intervention. Users are decriminalized—no
 *   longer liable for possession—and offered access to health services:
 *   supervised consumption, medication-assisted treatment (methadone,
 *   buprenorphine), overdose prevention, and disease screening. The state
 *   shifts its role from punisher to service provider and health monitor.
 *   This reading creates a tangled structure: users benefit from
 *   decriminalization and health access, but remain subject to paternalistic
 *   intervention (mandatory treatment, surveillance, compliance requirements)
 *   and the supply chain remains criminalized. The constraint's persistence
 *   requires active enforcement of the decriminalization boundary and
 *   supply-side criminalization, generating asymmetric beneficiary/victim
 *   positions. The expected structural delta—users exit criminal victim
 *   status but remain subject to health intervention; enforcement recedes for
 *   users but intensifies for supply—is inscribed in the stakeholder
 *   positions and measurement trajectory.
 *
 * KEY AGENTS:
 *   - Substance users: powerless, identity-locked, primary payers (subject to paternalistic intervention and surveillance) and secondary beneficiaries (access to overdose prevention and disease management).
 *   - Public health authorities: institutional, analytical scope, agenda-setters defining what counts as 'harm reduction' and determining treatment protocols.
 *   - Harm reduction providers (NGOs, clinics): organized, beneficiaries receiving funding and institutional legitimacy under this reading.
 *   - Drug supply workers: powerless, trapped, primary payers (remain criminalized despite user decriminalization).
 *   - Prohibition advocates: excluded from agenda-setting under this reading; their punishment-focused framing is displaced.
 *   - Legalization advocates: excluded; their liberty-focused reading competes for institutional authority.
 *   - Criminal justice system: observer, diminished role for users but reinforced for supply chain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction as Health Intervention (Substance Control Kernel)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '874a979e-bd6f-44db-9ba5-37dd5ea740c7').
narrative_ontology:cs_kernel_codification('874a979e-bd6f-44db-9ba5-37dd5ea740c7', fixed_text).
narrative_ontology:cs_authority_grounding('874a979e-bd6f-44db-9ba5-37dd5ea740c7', expertise).
narrative_ontology:cs_interpretation_layer_present('874a979e-bd6f-44db-9ba5-37dd5ea740c7').
narrative_ontology:cs_reading_relation('874a979e-bd6f-44db-9ba5-37dd5ea740c7', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('874a979e-bd6f-44db-9ba5-37dd5ea740c7', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('874a979e-bd6f-44db-9ba5-37dd5ea740c7', foundational, persistent_use_decriminalization_compatible).
narrative_ontology:cs_axiom_status(persistent_use_decriminalization_compatible, holdable).
narrative_ontology:cs_axiom_grounding('874a979e-bd6f-44db-9ba5-37dd5ea740c7', persistent_use_decriminalization_compatible, empirically_contingent).
narrative_ontology:cs_axiom('874a979e-bd6f-44db-9ba5-37dd5ea740c7', foundational, paternalistic_health_intervention_justified_by_public_health_evidence).
narrative_ontology:cs_axiom_status(paternalistic_health_intervention_justified_by_public_health_evidence, holdable).
narrative_ontology:cs_axiom_grounding('874a979e-bd6f-44db-9ba5-37dd5ea740c7', paternalistic_health_intervention_justified_by_public_health_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('874a979e-bd6f-44db-9ba5-37dd5ea740c7', decriminalization_health_service_model).
narrative_ontology:cs_drift_state('874a979e-bd6f-44db-9ba5-37dd5ea740c7', post_opioid_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('874a979e-bd6f-44db-9ba5-37dd5ea740c7', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, overdose_prevention_services).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, drug_supply_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, substance_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to paternalistic health intervention (supervised consumption, medication-assisted treatment, disease screening) framed as care rather than punishment. Removed from criminal liability for possession under this reading, but must comply with treatment protocols and surveillance systems to access services. Their identity as 'patient' is constituted through the intervention framework; exit would require abandoning both the treatment benefits and the social identity the system provides. They bear the cost of mandatory health surveillance and treatment coercion, while gaining access to overdose prevention and disease management.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, substance_users, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, substance_users, beneficiary).

% Defines the boundary between health intervention and punishment, sets treatment protocols, deploys supervised consumption facilities and medication-assisted therapy. Administers the constraint by establishing clinical criteria for 'harm reduction' and determining which interventions count as medical versus which constitute punishment. They frame the constraint as evidence-based public health, justified by mortality and morbidity reduction data.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% NGOs, clinics, and social services that deliver needle exchange, supervised consumption, medication-assisted treatment, and disease screening. They receive funding and institutional legitimacy under the harm reduction framework. Their professional authority expands under this reading: substance use becomes a health domain they control rather than a criminal justice domain. They have the exit option of reformulating within other constraint readings if funding shifts.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_providers, beneficiary,
    organized, biographical, mobile, national).

% The supply chain remains criminalized under this reading—production, distribution, and sales are still prohibited and enforced against. Workers remain subject to criminal liability and incarceration. They bear the extraction cost (legal risk) while the harm reduction intervention flows only to users, not suppliers. They cannot exit into legalization within this framework; they remain targets of enforcement while users are repositioned as patients.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, drug_supply_workers, payer,
    powerless, biographical, trapped, national).

% Institutions and coalitions that hold the prohibition reading (punishment for moral transgression) are structurally excluded from agenda-setting under the harm reduction framework. Their claim that substance use requires criminal sanction is displaced by the health intervention framing. They retain institutional presence (law enforcement, drug courts) but their authority over substance policy is diminished.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Parties advocating for decriminalization and legalization are also excluded from this framework. The harm reduction reading positions users as subjects of ongoing health intervention and surveillance, not as autonomous agents free to choose. Legalization advocates argue for individual liberty; harm reduction advocates claim paternalistic intervention is justified by public health evidence. Both are excluded from the other reading's institutional logic.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% Emergency response, naloxone distribution, and poison-control services that directly reduce overdose mortality. Benefit from the harm reduction framing because their work is reframed as public health infrastructure rather than enabling use. They have the exit option of operating under different readings if constraints shift.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, overdose_prevention_services, beneficiary,
    organized, biographical, mobile, national).

% Law enforcement and prosecution apparatus traditionally positioned to criminalize substance use. Under harm reduction, their role is diminished for users (decriminalization pressure) but reinforced for supply chain (enforcement against production and distribution remains active). They observe the constraint's operation and resist aspects that transfer authority from punishment to health.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminal_justice_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose mortality, infectious disease transmission (HIV, hepatitis C), and social dysfunction by treating substance use as a health condition requiring pragmatic intervention rather than criminal punishment. Coordinates multiple services (medical treatment, harm reduction supply, disease screening, emergency response) around the primary goal of keeping users alive and reducing disease prevalence.
% TRANSFER_FUNCTION: Transfers authority and institutional resources from criminal justice systems to public health systems. Moves funding and legitimacy toward harm reduction providers and overdose prevention services. Requires substance users to accept paternalistic health surveillance and treatment protocols as the price of decriminalization and access to services.
% ABSENT_VOICES: Drug supply chain workers are structurally excluded: the constraint decriminalizes users while keeping suppliers criminalized, so suppliers have no seat at the table and would object to the asymmetry if heard. Legalization advocates are also excluded: they would argue for user autonomy and supply legalization, challenging the paternalism built into the reading. Prohibition advocates argue that decriminalization abandons moral education and community protection—their objection is also absent from the policy consensus this reading requires.
% DISAPPEARANCE_RATIONALE: If harm reduction as a binding intervention framework vanished, substance users would lose access to supervised consumption facilities, medication-assisted treatment protocols, and the legal protection from criminal liability. Overdose mortality would rise absent the infrastructure, and substance use policy would revert toward either prohibition or legalization. The public health system would reorganize away from the specialized harm reduction institutions currently staffed and funded.
% FOUNDING_PROBLEM: Substance use persists despite prohibition; criminalization alone does not eliminate use. Rising overdose mortality in the 2010s–2020s, particularly from fentanyl and synthetic opioids, showed that prohibition was failing to prevent deaths. Infectious disease transmission among people who use drugs remained high. The founding problem is the observed failure of prohibition to achieve its stated goals—users continued using despite legal sanctions, and the harms (overdose, disease) mounted.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, medical organizations (WHO, CDC, national medical associations), and operational data from jurisdictions with harm reduction programs attest that prohibition alone does not eliminate substance use and that harm reduction reduces overdose and disease transmission. Prohibition advocates contest this framing: they argue the problem is insufficient enforcement, not the strategy itself. Legalization advocates argue the real problem is prohibition's existence, not its incompleteness. The founding problem is empirically attested by health data but its interpretation and proposed solution are contested.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint imposes paternalistic constraints on users (compliance requirements, surveillance, treatment mandates) while granting some autonomy compared to prohibition and access to services compared to legalization. The distribution is asymmetric: harm reduction providers and public health authorities collect institutional authority and funding; users remain subjects of intervention despite decriminalization. Suppression is lower than prohibition (0.42 vs. 0.55) because the decriminalization removes criminal liability fear, but suppression is still present because users must comply with health protocols to access services—a gentler coercion, but coercion. Theater is moderate (0.28): the constraint is primarily functional (overdose prevention and disease management are real public health needs) but some enforcement effort goes to maintaining the user/supply asymmetry rather than delivering health outcomes. Accessibility collapse is moderate (0.61): users have an exit option—they can stop using and abandon the intervention system—but identity_locked status (the 'patient' or 'in recovery' role becomes self-constituted) keeps many within it despite the formal exit. Drug supply workers have almost no exit (trapped status, 0.0). The temporal trajectory shows extractiveness rising slightly in early years as the intervention infrastructure grows and then plateauing, while suppression requirement drops as the decriminalization normalizes and users internalize the patient identity. This suggests a shift from active coercion to internalized compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authorities' seat, the constraint is genuine coordination: they built the intervention system to reduce overdose and disease, and the data show that it works—users benefit from access and survival improves. From substance users' seats the same structure operates as paternalistic extraction: they are required to undergo treatment and surveillance to access services, their autonomy is constrained, and their identity is reconstructed as 'patient' or 'addict-in-recovery' whether they choose it or not. From drug supply workers' seats it is pure snare: they remain criminalized while users are decriminalized, bearing the entire extraction cost. From legalization advocates' seats it is a false coordination that trades one form of control (criminal) for another (medical), without addressing the core question of user autonomy. The engine computes these divergences from the structural data: public health authorities (institutional, analytical) and harm reduction providers (organized, mobile) sit at different directionality vectors than substance users (powerless, identity-locked, trapped in the system) and drug supply workers (powerless, trapped entirely). The divergence is not a measurement error; it is structural fact about who benefits and who pays.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users are partial targets (d ≈ 0.65–0.75): they bear the cost of paternalistic intervention, treatment compliance, and surveillance; they lack genuine exit options (identity_locked status means the 'patient' identity becomes self-constituted, making exit costly even formally); but they also gain access to overdose prevention and disease management that their alternative would not provide. From the public health framing, they are beneficiaries; from their own framing (legalization or prohibition advocates), they are victims. Drug supply workers are full targets (d ≈ 0.95): they remain criminalized, face incarceration and asset seizure, and gain nothing from the decriminalization that flows only to users. They have zero exit options (trapped). Public health authorities and harm reduction providers are beneficiaries (d ≈ 0.15–0.25): they gain institutional authority, funding, professional legitimacy, and (in harm reduction providers' case) career expansion. The asymmetry is the constraint's core structure: two classes of people whose activity (substance use, supply) the state regulates, but the state decriminalizes one and intensifies criminalization of the other. This asymmetry is maintained by active enforcement—the supply chain must be kept criminalized to sustain the user decriminalization boundary (if supply legalized, users would be purchasing from legal vendors and no longer need health intervention).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overdose mortality, disease transmission rising despite prohibition) was live and empirically documented when harm reduction policies began in the 1990s–2000s. The problem remains contested: prohibition advocates argue it reflects insufficient enforcement, not failed strategy; legalization advocates argue prohibition itself is the problem; harm reduction advocates argue the problem is persistent and intervention-responsive. The disappearance verdict is world_rearranges—if the harm reduction framework vanished, substance users would lose infrastructure and public health protections, and policy would revert or shift. The founding problem and the constraint's persistence are aligned, so no mandatrophy is present. However, there is significant asymmetry: the constraint solves a coordination problem (reducing overdose, organizing prevention services) for substance users while maintaining a pure snare for drug supply workers. This asymmetry is structurally unstable and likely to generate pressure toward either full legalization (supply + user) or reversion to prohibition (user + supply), because the current configuration requires continuous effort to maintain the boundary between decriminalized demand and criminalized supply. The constraint is therefore a tangled rope with embedded piton characteristics: the coordination component (overdose prevention) is real and valued, but the extraction component (supply criminalization without legalization or user license to produce) persists not because it solves a coordination problem but because it maintains asymmetric authority—a theater element that grows over time as the supply side innovates to escape criminalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_autonomy_boundary,
    'Where is the boundary between health intervention legitimately paternalistic and intervention that becomes coercive extraction? Does the user''s consent to treatment protocols change the classification?',
    'Longitudinal data on user experience of ''choice'' in treatment entry and compliance; qualitative research on whether users experience harm reduction as enabling agency or constraining autonomy; comparison with jurisdictions that offer low-barrier access without treatment mandate.',
    'If intervention is predominantly experienced as coercive (users report constraints on autonomy despite nominal health framing), extraction component rises toward snare classification. If predominantly enabling (users experience access and autonomy together), coordination component dominates and tangled_rope framing holds. If autonomy splits across user populations, the classification becomes contingent on which users'' experience grounds the verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy_boundary, empirical, 'Whether paternalistic health intervention is experienced as legitimate coordination or as coercive extraction.').

omega_variable(
    supply_chain_asymmetry,
    'Is the criminalization of the supply chain while users are decriminalized a coherent policy or an unstable asymmetry that will eventually resolve toward either legalization or full prohibition?',
    'Historical precedent from jurisdictions that have sustained supply criminalization while decriminalizing users (Portugal since 2001 is the longest-running case); analysis of whether supply networks adapt or collapse under persistent criminalization; policy evolution in European harm reduction jurisdictions.',
    'If the asymmetry is unstable, the harm reduction reading is a transitional state (scaffold-adjacent), not a stable equilibrium. Supply workers would accumulate grievance, and political pressure would eventually resolve toward full legalization or revert to prohibition. If stable, the constraint is a genuine tangled_rope with distributed victims and persistent institutional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_asymmetry, empirical, 'Whether user decriminalization plus supply criminalization is a sustainable equilibrium.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the substance_control_kernel. The reading contest positions harm_reduction_reading against prohibition_reading (punishment-focused) and legalization_reading (liberty-focused). Is the harm reduction reading''s core claim—that substance use requires ongoing paternalistic health intervention independent of cessation—empirically grounded or normatively stipulated?',
    'Compare empirical health outcomes (overdose, disease prevalence) across jurisdictions instantiating different readings. If harm reduction produces superior outcomes independent of legalization/prohibition on supply, the reading is empirically justified. If outcomes vary by supply policy or user demographics more than by intervention type, the reading is partially stipulated (value-laden selection of which harms to prioritize).',
    'If empirically privileged, harm reduction has the strongest claim to the kernel and influences the other readings. If normatively stipulated, the reading coexists with siblings without foreclosing them—a matter of which harms the polity chooses to prioritize, not which is factually true. This shapes whether the reading''s authority is grounded in expertise (empirically justified) or lineage/extraction (politically maintained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the harm reduction reading is empirically grounded or normatively stipulated within the kernel contest.').

omega_variable(
    internalized_suppression_mechanism,
    'Substance users comply with paternalistic treatment protocols partly through coercion (loss of access to services, legal pressure) and partly through internalized identity (adopting the ''patient'' or ''in recovery'' role). What proportion of the measured suppression is structural versus internalized?',
    'Post-exit trajectory: if users retain treatment compliance and health behavior patterns after exiting the formal intervention system, suppression is partially internalized. If compliance collapses, suppression is primarily structural. Qualitative data on whether users describe the intervention as ''helping me'' or ''controlling me'' traces the internalization process.',
    'High internalization means the suppression persists even after formal enforcement ends—the constraint travels with the user. High structurality means suppression depends on active institutional presence. The mix determines whether the constraint is truly stable or whether it requires continuous coercive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Proportion of suppression that is structural versus internalized identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__harm_reduction_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__harm_reduction_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__harm_reduction_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(subs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three structurally distinct readings based on where the boundary between intervention and liberty is drawn. Prohibition reading criminalizes all use and supply; harm reduction reading decriminalizes users but keeps supply criminalized while providing health services; legalization reading removes criminal liability from both users and supply (with or without regulation). Each reading has a different ε, different beneficiary/victim structure, different authority grounding, and different terminal state. The three are not the same constraint measured from different angles—their ε values are stable under different observables. Each is a live institutional position held by different coalitions. They affect one another via competition for policy authority, not via causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
