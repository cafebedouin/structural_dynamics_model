% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   Statutory credential requirements are justified as consumer-protection
 *   mechanisms that prevent harm from incompetent practitioners. This reading
 *   instantiates a different structural reading: credential requirements as
 *   mechanisms that sort labor market access by prior resource availability
 *   and social-network access, creating a tiered system where marginalized
 *   workers without the capital or networks to acquire credentials are
 *   systematically excluded from regulated occupations. The constraint
 *   functions as a Snare: the credential requirement is presented as a
 *   natural, necessity-driven rule (public safety), but its persistence
 *   depends on suppression of alternative pathways (apprenticeship,
 *   portfolio-based licensing) and on excluding those who would advocate for
 *   lower barriers. The beneficiary class (incumbent practitioners) captures
 *   the regulatory bodies that ostensibly represent the public interest. The
 *   victim class (marginalized workers, entry-level practitioners) bears both
 *   the direct cost (credential acquisition barriers) and the indirect cost
 *   (restricted labor supply, higher prices, intergenerational poverty
 *   replication).
 *
 * KEY AGENTS:
 *   - Incumbent credentialed practitioners: Protected labor market position; beneficiary through supply restriction
 *   - Credential gatekeeping bodies: Institutional agenda-setter; beneficiary through fees and authority; controls the rule-setting
 *   - Marginalized workers without resources: Trapped; systematic exclusion from regulated occupations; primary victim
 *   - Entry-level practitioners: Constrained; credentialing burden exceeds job requirements; secondary victim
 *   - Workers from excluded backgrounds: Identity-locked; discriminatory implementation of seemingly neutral requirements; intersectional victim
 *   - State regulatory authority: Ostensible arbiter; captured by incumbent practitioners; observer seat with blocked access to countervailing influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.72).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'd6e1b261-b027-4385-97fa-ecf1b80f65a8').
narrative_ontology:cs_kernel_codification('d6e1b261-b027-4385-97fa-ecf1b80f65a8', fixed_text).
narrative_ontology:cs_authority_grounding('d6e1b261-b027-4385-97fa-ecf1b80f65a8', extraction).
narrative_ontology:cs_interpretation_layer_present('d6e1b261-b027-4385-97fa-ecf1b80f65a8').
narrative_ontology:cs_reading_relation('d6e1b261-b027-4385-97fa-ecf1b80f65a8', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('d6e1b261-b027-4385-97fa-ecf1b80f65a8', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('d6e1b261-b027-4385-97fa-ecf1b80f65a8', foundational, credential_barriers_sort_by_class).
narrative_ontology:cs_axiom_status(credential_barriers_sort_by_class, holdable).
narrative_ontology:cs_axiom_grounding('d6e1b261-b027-4385-97fa-ecf1b80f65a8', credential_barriers_sort_by_class, empirically_contingent).
narrative_ontology:cs_axiom('d6e1b261-b027-4385-97fa-ecf1b80f65a8', secondary, apprenticeship_entry_equivalent_to_credentialing).
narrative_ontology:cs_axiom_status(apprenticeship_entry_equivalent_to_credentialing, holdable).
narrative_ontology:cs_axiom_grounding('d6e1b261-b027-4385-97fa-ecf1b80f65a8', apprenticeship_entry_equivalent_to_credentialing, empirically_contingent).
narrative_ontology:cs_reference_frame('d6e1b261-b027-4385-97fa-ecf1b80f65a8', permissive_occupational_entry).
narrative_ontology:cs_drift_state('d6e1b261-b027-4385-97fa-ecf1b80f65a8', contemporary_credential_mandate_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('d6e1b261-b027-4385-97fa-ecf1b80f65a8', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credential_gatekeeping_bodies).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_resources).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, entry_level_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, workers_from_excluded_backgrounds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, small_business_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold credentials granted under prior, less stringent requirements or via established social networks. Benefit from reduced competition as statutory barriers exclude new entrants. Their credentials remain valid; they face no re-qualification burden. They maintain professional associations that lobby to maintain or increase credential requirements, which protects their labor market position.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners, beneficiary,
    organized, generational, arbitrage, national).

% State licensing boards, professional associations, and educational institutions that set and enforce credential requirements. They administer examinations, accredit training programs, and discipline licensees. They collect examination fees, accreditation fees, and derive institutional authority from the credentialing monopoly. Resist lowering requirements or creating alternative pathways.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credential_gatekeeping_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, credential_gatekeeping_bodies, beneficiary).

% Lack capital to fund credential acquisition (tuition, opportunity cost during training, examination fees, licensing fees). Cannot afford unpaid internships or apprenticeships. Often work multiple jobs and cannot attend daytime-only training programs. Face credential requirements that explicitly exclude them from regulated occupations, trapping them in lower-wage unregulated work or unemployment. Geographic and transportation barriers further constrain access to credential acquisition sites.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_resources, payer,
    powerless, biographical, trapped, local).

% Have basic competence and willingness to work but face credential requirements that exceed what the job actually requires. Must invest 2-4 years of training and thousands of dollars to enter occupations that require 6 months of practical learning. Cannot demonstrate competence through work history or apprenticeship because the legal barrier requires the credential BEFORE entry.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, entry_level_practitioners, payer,
    moderate, biographical, constrained, regional).

% Face credential requirements whose design and administration have structural bias: testing language disadvantages non-native speakers, credential costs are proportionally higher for low-income applicants, apprenticeship networks are closed to outsiders (gender, race, family connections), and criminal record bars (even for minor/expunged offenses) create legal exclusion. The credential requirement's formal neutrality masks discriminatory implementation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, workers_from_excluded_backgrounds, payer,
    powerless, biographical, identity_locked, regional).

% Would argue for risk-based credentialing (higher standards for high-risk work, lower or alternative standards for low-risk work) and competence verification through apprenticeship or portfolio rather than exam-passing. Are structurally excluded from license-setting discussions, which are dominated by incumbent practitioners and gatekeeping bodies. Their voice would push toward lower barriers and alternative pathways.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_protection_advocates, excluded,
    organized, generational, constrained, national).

% Point out that credential requirements function as class sorting mechanisms that replicate intergenerational poverty by making licensed occupations inaccessible to workers without initial resources or family connections. Would advocate for apprenticeship-based entry, credential reciprocity across states, and portfolio-based licensing. Are excluded from the regulatory decisions that drive credential design.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, economic_mobility_advocates, excluded,
    organized, generational, constrained, national).

% Face credential requirements that force them to hire only credentialed workers even when they could train capable people on the job. Cannot access a larger pool of potential employees. Must pay higher wages to attract credentialed labor because the credential artificially restricts supply. Cannot scale their business by training workers internally.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, small_business_employers, payer,
    moderate, biographical, constrained, regional).

% Ostensibly sets credential requirements to protect public safety but inherits structures where incumbent practitioners dominate the licensing boards and design requirements to maximize barriers rather than minimize harm. State authority is theoretically distinct from the gatekeeping bodies but in practice is captured by the credentialed class.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_regulatory_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes minimum competence standards that theoretically prevent consumer harm and ensure service quality by restricting practice to trained, tested, and monitored professionals.
% TRANSFER_FUNCTION: Moves economic rents from would-be practitioners (credential acquisition costs, lost wages during training, examination fees) and from employers and consumers (higher prices due to restricted supply) to incumbent practitioners (protected labor market position, higher wages) and gatekeeping institutions (examination fees, accreditation fees, institutional authority).
% ABSENT_VOICES: Marginalized workers and entry-level practitioners are structurally excluded from regulatory discussions — they lack the organizational power and institutional access to contest credential requirements. Consumer protection advocates who might argue for risk-based credentialing are not at the table. Economic mobility advocates who could argue for apprenticeship-based entry are excluded. Small business employers who want access to a broader labor pool are sidelined.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished, labor market access would open overnight: marginalized workers could enter regulated occupations directly, entry-level practitioners could demonstrate competence through work, small employers could hire and train, and incumbent practitioners would face increased competition and downward wage pressure. The occupational stratification would reorganize rapidly. Some genuine consumer harm might occur in high-risk professions, but low-risk credentialing could shift to apprenticeship, portfolio-based assessment, or bonding.
% FOUNDING_PROBLEM: Historical: protect consumers from incompetent or fraudulent practitioners in high-risk occupations (medicine, law, engineering). Legitimate concern: someone's bad practice in surgery or building design kills or injures people.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners and gatekeeping bodies assert the founding problem is still live and that credential requirements prevent ongoing harm. Economic research from outside the benefiting parties (labor economists, mobility researchers, occupational regulation scholars) attests that credential requirements exceed what risk levels actually require, that they function as supply-side cartels more than consumer-protection mechanisms, and that the founding problem (protecting against incompetence in high-risk work) can be addressed through narrower, risk-calibrated requirements. Apprenticeship-based entry has delivered safer outcomes in jurisdictions where it is permitted.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) and rising through the interval because the credential requirement systematically sorts people by prior wealth and network access, channeling economic rents to incumbents. The measurement series shows extraction accumulating over 35 years: at t=0 (0.48), the barrier exists but alternatives remain somewhat accessible (apprenticeship, portfolio licensing still quasi-viable); by t=35 (0.72), every alternative pathway has been legislatively foreclosed and the credential requirement is the only legally recognized entry mechanism. Suppression is high (0.68) because the constraint's persistence depends on actively preventing the emergence of alternative credentials, banning apprenticeship-based entry, and excluding practitioners who lack formal credentials even when they demonstrate equivalent competence. Theater is moderate-to-high (0.41): the public-safety narrative is real (some credentials do protect), but an increasing share of enforcement activity (from t=0 to t=35, the theater ratio rises from 0.22 to 0.41) focuses on defending credential monopoly rather than assessing actual risk or competence. Accessibility collapse is high (0.79): once a credential requirement is statute, alternatives collapse; a person without resources cannot credibly enter except through the credential. Resistance is moderate (0.52): entry-level practitioners and marginalized workers resist through informal work-arounds and political advocacy, but their organized power is low and incumbent practitioners' organized power is high.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent practitioners and gatekeeping bodies experience this constraint as legitimate rule maintenance protecting public safety — they author a 'rope' or 'mountain' reading where the credential requirement is a natural necessity. The marginalized workers experience it as a snare: a legal barrier that traps them in lower-wage work because they cannot afford the gatekeeping price. The engine will compute different per-seat types from this structural data: the beneficiary seat (organized, arbitrage exit, legislative access) will compute toward rope or even natural-law framing; the victim seats (powerless/moderate power, trapped/constrained/identity-locked exits, excluded from legislative process) will compute toward snare. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent credentialed practitioners: d ≈ 0.1 (full beneficiary — they benefit from supply restriction, face zero barrier to practice their current vocation, have arbitrage-grade exit options into adjacent fields, organized power). Gatekeeping bodies: d ≈ 0.2 (beneficiary through fees and authority, though they also bear some cost from political controversy; institutional power with distributed exit options). Marginalized workers without resources: d ≈ 0.95 (full target — they bear the extraction through exclusion, cannot exit the labor market entirely, trapped by poverty itself, individual power, no organized representation). Entry-level practitioners: d ≈ 0.85 (near-target; they pay the credential cost and face credentialing burden, but have some mobility through education investment; moderate power). Workers from excluded backgrounds: d ≈ 0.9 (near-target through identity-lock; they cannot exit their identity, face discriminatory implementation, have powerless organization). Small business employers: d ≈ 0.72 (target; they pay through wage bills and labor supply restrictions, moderate power, constrained exit). The directionality derivation is automatic from beneficiary/victim declarations + exit options; no overrides needed here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting consumers from incompetent practitioners) is live in high-risk work (surgery, structural engineering, law) and largely dead in low-risk work (hair braiding, interior design, many trades). Yet the credential requirement persists across all risk levels, from high-risk to negligible-risk occupations. This is the mandatrophy signature: the constraint's founding mandate has outlived its function (it is not necessary for consumer protection in low-risk work) but the constraint persists because the beneficiary class (incumbents) captures the regulatory authority. The theater ratio rising from 0.22 to 0.41 shows this dynamic: early in the interval, the constraint had real consumer-protection work to do; by t=35, most enforcement activity is defending the credential monopoly rather than assessing competence. The constraint evolved from a rope (legitimate risk management) to a snare (supply-side cartel) without formal reclassification — mandatrophy, not transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_barrier_mechanism_ambiguity,
    'Are the graduated credential barriers structurally necessary to prevent consumer harm, or are they artifacts of incumbent practitioner capture of regulatory bodies?',
    'Comparative analysis: jurisdictions with apprenticeship-based entry vs. credential-only entry in the same occupation. If apprenticeship produces equivalent or better consumer-protection outcomes (safety record, complaint rate, harm prevention) while opening labor access, then the barriers are not necessary for their stated function.',
    'If barriers are not necessary for consumer protection, the constraint reclassifies from rope (genuine coordination) to snare (extraction riding on a cover story). If apprenticeship does produce inferior outcomes, the barriers are legitimate coordination and the constraint remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_mechanism_ambiguity, empirical, 'Whether credential barriers are structurally necessary for consumer protection or are unnecessary supply-side restrictions.').

omega_variable(
    class_sorting_vs_risk_calibration,
    'Does the credential requirement''s tiered access follow logically from risk levels (high-risk work requiring high barriers, low-risk work requiring low barriers), or does it reflect incumbent practitioner preferences to restrict supply regardless of risk?',
    'Occupational regulatory audit: compare credential requirements across occupations by actual risk level (injury/death rate, consumer harm incidents, dispute frequency). If low-risk occupations have high credential requirements while high-risk occupations have low barriers, the pattern is inverted from risk-calibration and indicates capture.',
    'Risk-calibrated credentialing would support the rope reading (necessary safety coordination). Supply-side capture (barriers high in low-risk work) would support snare classification and indicate the constraint functions primarily to restrict labor supply rather than prevent harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_sorting_vs_risk_calibration, empirical, 'Whether credential requirements are calibrated to actual risk or driven by supply-side capture.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression that keeps marginalized workers out of regulated occupations structural (legal barriers, cost barriers, time barriers) or internalized (self-exclusion from a profession they perceive as ''not for people like me'')?',
    'Audit of failed credential acquisition attempts: proportion due to financial inability to pay vs. test failure rate vs. application rejection vs. voluntary withdrawal. Post-exit follow-up: do workers who successfully acquire credentials and enter the occupation report reduced internalized suppression?',
    'If mostly structural, fixing the constraint requires lowering legal and cost barriers. If mostly internalized, fixing it requires also addressing belief/identity barriers. If mixed, both interventions are needed. High internalization would indicate the constraint has calcified into identity-lock and even legal barrier removal would not fully open access.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression keeping marginalized workers out is structural (external barriers) or internalized (self-exclusion beliefs).').

omega_variable(
    kernel_reading_contest_scope,
    'This constraint is one reading of a contested kernel (the statutory credential requirement itself). Do the three sibling readings — ''public_safety_coordination'', ''rent_seeking_suppression'', ''graduated_access_filter'' — represent exhaustive framings of the same constraint, or are there other framings?',
    'Systematic review of regulatory statements, incumbent practitioner justifications, advocacy-group framing, and consumer surveys. Identify all distinct normative claims about why the credential requirement exists and persists. Map each to an existing reading or identify gaps.',
    'If the three readings exhaust the domain, the kernel contest is bounded. If other readings exist (e.g., ''paternalism_coordination'', ''equal_opportunity_barrier''), the kernel is under-specified and the constraint story should decompose further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_scope, conceptual, 'Whether the three sibling readings capture the full scope of the kernel contest or whether additional readings are missing.').

omega_variable(
    identity_lock_mechanism_for_excluded_backgrounds,
    'For workers from excluded backgrounds facing discriminatory implementation of credential requirements, is the exclusion due to external barriers (cost, geography, testing bias) that could be removed, or does it also involve internalized belief that ''people like me don''t become licensed practitioners''?',
    'Qualitative research with workers from excluded backgrounds who did and did not pursue credentials: what barriers did they cite, what beliefs did they hold about accessibility and belonging? Post-credential follow-up: do newly credentialed workers from excluded backgrounds report identity shift (seeing themselves as ''practitioners'' rather than ''outsiders'')?',
    'If purely external, lowering barriers opens access. If identity-locked, removing legal barriers alone is insufficient; the constraint has calcified into self-exclusion and needs identity-restoration work. If mixed, both are needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_excluded_backgrounds, empirical, 'Whether credential barriers for workers from excluded backgrounds are structural or identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__graduated_access_filter, theater_ratio, 5, 0.26).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.31).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__graduated_access_filter, theater_ratio, 15, 0.36).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.39).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__graduated_access_filter, theater_ratio, 25, 0.4).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.41).
narrative_ontology:measurement(lice_tr_t35, licensing_statute_mandate__graduated_access_filter, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(lice_be_t35, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 35, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(lice_su_t35, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.12).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, occupational_cartels_wage_setting).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, apprenticeship_foreclosure_statute).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the licensing_statute_mandate kernel. The three readings ('graduated_access_filter', 'public_safety_coordination', 'rent_seeking_suppression') represent distinct structural interpretations of the same statutory framework. Each reading has its own constraint_id, its own epsilon value, its own beneficiary/victim structure, and its own computed type. The graduated_access_filter reading treats the credential requirement as a mechanism for class-sorting labor-market access; public_safety_coordination treats it as necessary risk management; rent_seeking_suppression treats it as straightforward supply-side cartel. They are NOT variants of one constraint — they are three separate constraints with different omegas, different stakeholder orderings, and different structural properties, unified by their common referent (the statutory credential requirement) and their family relationship (each reading changes what that requirement means and how it functions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
