% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Medicalized Harm Reduction Authority (Non-Criminalization Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   Under the harm-reduction reading of substance control legitimacy, the
 *   state derives its authority to regulate substance use from a duty to
 *   minimize harm to users and communities, not from moral condemnation or
 *   paternalistic prohibition. Users are medicalized — treated as individuals
 *   with a medical condition requiring treatment access — rather than
 *   criminalized. The constraint operates by authorizing treatment providers,
 *   public health agencies, and harm-reduction organizations to intervene in
 *   substance use patterns, often involuntarily (court-mandated treatment,
 *   conditional release programs, mandatory monitoring). Simultaneously, the
 *   constraint permits a regulated market for harm-reducing practices (needle
 *   exchange, medication-assisted treatment with pharmaceutical opioids,
 *   supervised consumption sites) that prohibition-era authority would have
 *   criminalized. The reading is contestable because it retains significant
 *   state authority over substance supply and use patterns, creates new forms
 *   of coercion (treatment mandates), and coexists with a persistent black
 *   market for unregulated supply. The claim/metric gap is deliberate: this
 *   reading is authored as tangled_rope because it genuinely solves a
 *   coordination problem (users get access to treatment, providers get
 *   legitimacy, communities get public health infrastructure) but does so
 *   asymmetrically — substance users pay through coerced treatment
 *   participation and loss of autonomy, while treatment sectors and public
 *   health agencies benefit from expanded institutional authority and
 *   funding.
 *
 * KEY AGENTS:
 *   - Substance users (powerless → moderate): targeted by treatment mandates, coercive monitoring; exit to unregulated markets remains available but carries legal and health risks; identity-locked by addiction and criminalized status.
 *   - Treatment providers (institutional): benefit from expanded authority, funding, and legitimacy; agenda-setters for treatment protocols; manage the medicalization infrastructure.
 *   - Public health agencies (institutional): set policy, define harm-reduction standards, coordinate enforcement; collect institutional legitimacy and budget authority.
 *   - Enforcement workers / drug courts (powerful → organized): administer treatment mandates, monitor compliance, bridge criminalization and medicalization; experience role ambiguity between enforcement and care.
 *   - Harm-reduction advocates (organized): demand non-criminalization, shape policy, benefit from legitimacy shift; may experience tension between advocacy for user autonomy and participation in coercive treatment systems.
 *   - Black market suppliers (excluded): would object to regulation but are functionally prevented from participating; their exclusion is what the enforcement infrastructure maintains.
 *   - Families and communities (powerless → organized): experience reduced crime/disorder if treatment succeeds; bear costs if enforcement is felt as intrusive or treatment is experienced as forced.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Medicalized Harm Reduction Authority (Non-Criminalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '054d7244-d3c2-4298-99fa-eb50b63a805c').
narrative_ontology:cs_kernel_codification('054d7244-d3c2-4298-99fa-eb50b63a805c', distributed).
narrative_ontology:cs_authority_grounding('054d7244-d3c2-4298-99fa-eb50b63a805c', extraction).
narrative_ontology:cs_interpretation_layer_present('054d7244-d3c2-4298-99fa-eb50b63a805c').
narrative_ontology:cs_reading_relation('054d7244-d3c2-4298-99fa-eb50b63a805c', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('054d7244-d3c2-4298-99fa-eb50b63a805c', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('054d7244-d3c2-4298-99fa-eb50b63a805c', foundational, state_duty_harm_minimization_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_harm_minimization_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('054d7244-d3c2-4298-99fa-eb50b63a805c', state_duty_harm_minimization_without_criminalization, empirically_contingent).
narrative_ontology:cs_axiom('054d7244-d3c2-4298-99fa-eb50b63a805c', foundational, substance_use_is_medical_condition_not_moral_failure).
narrative_ontology:cs_axiom_status(substance_use_is_medical_condition_not_moral_failure, holdable).
narrative_ontology:cs_axiom_grounding('054d7244-d3c2-4298-99fa-eb50b63a805c', substance_use_is_medical_condition_not_moral_failure, empirically_contingent).
narrative_ontology:cs_axiom('054d7244-d3c2-4298-99fa-eb50b63a805c', secondary, treatment_access_reduces_individual_and_collective_harm).
narrative_ontology:cs_axiom_status(treatment_access_reduces_individual_and_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('054d7244-d3c2-4298-99fa-eb50b63a805c', treatment_access_reduces_individual_and_collective_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('054d7244-d3c2-4298-99fa-eb50b63a805c', medicalized_harm_minimization_framework).
narrative_ontology:cs_drift_state('054d7244-d3c2-4298-99fa-eb50b63a805c', contemporary_post_opioid_crisis_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('054d7244-d3c2-4298-99fa-eb50b63a805c', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_advocates).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, enforcement_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, enforcement_workers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, families_and_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, families_and_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to treatment mandates via drug courts, conditional release, and probation conditions. Must participate in treatment programs to avoid incarceration or to regain custody/employment. Experience coercive monitoring and surveillance. Can exit by using black-market supply, migrating to permissive jurisdictions, or enduring criminal consequences. Addiction and criminalized status make identity-locked: they internalize the stigma and become bound to the role of 'substance abuser' requiring treatment.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users, payer,
    powerless, biographical, identity_locked, local).

% Receive expanded funding, legitimacy, and institutional authority under harm-reduction framing. Set treatment protocols, define recovery standards, control access to medication-assisted treatment. Benefit from both public funding and private insurance reimbursement. Can exit by opposing harm-reduction policy or specializing in prohibition-era criminal-justice treatment, but most have invested in the harm-reduction model.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, treatment_providers, agenda_setter).

% Design harm-reduction policy, coordinate treatment networks, manage public-health epidemiology and outcomes. Expand institutional authority and budget allocation. Constrained exit because the agency mandate is now embedded in statute; exiting requires legislative change.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Drug court judges, probation officers, parole agents administer treatment mandates and monitor compliance. Experience role ambiguity: they are agents of both punishment and care, enforcing legal conditions while navigating therapeutic relationships. Career depends on the drug-court system, which benefits from harm-reduction funding and legitimacy. Constrained exit because specialization in this role is deep. Bear costs through ideological conflict (punishment vs. care), emotional labor, and low public status.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, enforcement_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, enforcement_workers, beneficiary).

% Pushed for non-criminalization and achieved policy shift. Benefit from legitimacy, advocacy funding, and policy influence. Also constrained by participation in coercive treatment systems: may experience tension between advocacy for user autonomy and participation in enforcement machinery. Mobile exit: can form alternative organizations, withdraw from policy engagement, or migrate to more permissive jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_advocates, beneficiary,
    organized, biographical, mobile, national).

% Functionally prevented from participating in regulated supply chains. Excluded from harm-reduction infrastructure. Would benefit from decriminalization or legalization but are trapped by criminalization and enforcement machinery. Their exclusion is what the constraint maintains.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_suppliers, excluded,
    powerful, biographical, trapped, global).

% Benefit from reduced visible drug use, crime, and disorder when users engage with treatment. Also bear costs through mandatory family involvement in treatment programs, loss of family member's autonomy, and surveillance of family dynamics. Constrained exit because community is geographic and family bonds are non-negotiable.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, families_and_communities, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, families_and_communities, payer).

% Oppose harm-reduction framing; argue substance use requires criminalization and moral condemnation. Excluded from policy-setting under harm-reduction regime. Can exit through jurisdictional migration, litigation, or legislative campaigns to restore prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of providing treatment access, reducing drug-related harms to individuals and communities, and maintaining public-health epidemiology without relying solely on criminalization. Creates a unified treatment infrastructure with standardized protocols, funding mechanisms, and monitoring systems that individual users or communities could not organize independently.
% TRANSFER_FUNCTION: Moves agency and autonomy from substance users to treatment/enforcement sectors. Users transfer control over their substance use, medical decisions, and monitoring to courts, treatment providers, and public-health agencies. In exchange, users receive access to treatment, reduced criminal penalties, and (potentially) reduced health harms. Treatment providers and agencies receive funding, professional expansion, and institutional authority. The transfer is asymmetric: users lose autonomy; providers gain authority and resources.
% ABSENT_VOICES: Black-market suppliers, individuals who prefer unregulated substance use and refuse treatment, autonomy-focused critics who oppose involuntary medicalization, and those who experienced or anticipate harms from enforcement machinery (relatives of incarcerated users, disability advocates). These parties would object to treatment mandates and coercive monitoring but are structurally excluded from policy-setting; their objections appear in litigation, advocacy campaigns, and user-led movements.
% DISAPPEARANCE_RATIONALE: If harm-reduction authority and treatment mandates disappeared overnight, the substance-use landscape would reorganize: black markets would expand, treatment infrastructure would collapse or privatize, criminalization might return, or legalization might advance. Communities would experience different visibility of drug use, different crime patterns, and different public-health epidemiology. The arrangements depend on this constraint being actively maintained.
% FOUNDING_PROBLEM: Substance use causes individual and community harm. Prohibition-era criminalization increased that harm by driving use underground, increasing overdose risk, adding carceral trauma, and failing to reduce use. The founding problem is: how to reduce harm and provide treatment access without relying on criminalization as the primary mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists, public-health researchers, and user-led harm-reduction organizations outside the treatment-provider sector attest the problem is live: substance use persists, overdose deaths rise and fall with enforcement patterns and treatment availability, and criminalization correlates with worse outcomes. Treatment providers also attest the problem is live but differ on whether non-criminalization is sufficient without mandatory treatment. Prohibition advocates attest the problem is criminalization-era policy failure, not the existence of substance use itself.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint moves agency from substance users to treatment/enforcement sectors and imposes mandatory participation in treatment systems, even when users did not request it. Suppression is moderate (0.48) because the constraint relies on legal enforcement (drug courts, conditional release) to mandate treatment, but suppression is lower than pure criminalization because users have access to treatment rather than purely carceral outcomes — the suppression is present but justified as medical necessity. Theater ratio rises from 0.22 to 0.41 over the interval, indicating growing performative activity: early expansion of harm-reduction infrastructure is genuine (low theater); later plateau suggests shift toward ritualized treatment compliance-checking and outcome theater (statistics, graduation ceremonies) as the system matures. Accessibility collapse is moderate (0.64) because users technically have alternatives (refuse treatment, use black market, migrate jurisdictions) but exercise of these alternatives carries severe penalties (incarceration, health risk, social loss). Resistance is high (0.72) because substance users, black-market participants, and autonomy-focused advocates mount real resistance — treatment non-compliance, black market persistence, policy contestation — and this resistance is visible in measurement drift where extractiveness plateaus rather than increasing. The shared time grid ensures all three measured metrics show convergence around t=15 and stabilization thereafter, consistent with a constraint whose initial expansion matured into an enforced steady state with internal limits.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (substance users) and the beneficiary seats (treatment providers, public health agencies) compute fundamentally different constraint types from the same structural facts. Users see coercive medicalization: they are mandated into treatment they may not want, monitored, and punished for non-compliance — snare-like. Treatment providers see expanded authority and resources: they serve a population with identifiable needs and state backing, legitimizing their work — rope-like. The gap exists because the constraint's beneficiaries designed and staff the coordination mechanism, creating asymmetry in who gains and who bears costs. The engine's per-seat computation captures this: from a powerless, identity-locked position with constrained exit, effective extraction is higher; from an institutional position with beneficiary role and expanded authority, effective extraction is lower or inverted into subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   From the user/payer seat: the constraint extracts agency and autonomy through treatment mandates; users are identity-locked (addiction + criminalized status make exit to unregulated use extremely costly). Directionality is high (0.8+) — users are full targets. From the treatment-provider/agenda-setter seat: the constraint provides legitimacy, institutional authority, funding, and professional expansion. Directionality is low (0.2–0.3) — they are beneficiaries. From the enforcement-worker seat: role ambiguity (moderately powerful but ideologically split between punishment and care; constrained exit because career depends on the system). Directionality is near-symmetric (0.5). From the harm-reduction advocate seat: mixed — they pushed for non-criminalization and achieved a policy victory, but sit inside a coercive system they partly oppose. Directionality is asymmetric (0.4–0.6 depending on whether they view medicalization as progress or capture). The structure produces per-seat divergence: users experience snare, treatment providers experience rope, enforcement sits between. The engine computes this divergence from the structural data (beneficiary/victim declarations + exit options + power); the authored claim (tangled_rope) reflects the system's aggregate behavior, not any single seat's experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is genuine and live: substance use causes individual and community harm, and prohibition-era criminalization increased that harm while failing to reduce use. The harm-reduction reading offers a legitimate alternative: treat it as a public health problem, provide treatment access, reduce criminalization. However, the measured extraction (0.62) is not trivial, and the divergence between the claim (tangled_rope) and user-seat experience (closer to snare) suggests potential mandatrophy risk. The rising theater ratio (0.22 → 0.41) indicates the system is increasingly performing harm reduction while extracting compliance — outcome metrics, graduation ceremonies, compliance statistics — rather than deepening actual treatment efficacy or user autonomy. This is mandatrophy warning: the founding problem (harm reduction) is being displaced by the institutional problem (system maintenance and extraction). However, the measured extraction plateaus rather than accelerating (t=15 onward), suggesting the system has reached an equilibrium where extraction is bounded by user resistance and black-market competition. The constraint avoids acute mandatrophy by maintaining a genuine (if limited) coordination function: users who engage with treatment do reduce harm; treatment providers do expand access; communities do see reduced crime. But the constraint is at risk of slow mandatrophy if extraction continues to rise while the founding problem's solution is increasingly performed rather than lived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_care_boundary,
    'Does the constraint''s enforcement infrastructure coerce treatment participation or genuinely offer care — and does the user experience distinguish them?',
    'Longitudinal user interviews tracking voluntary vs. court-mandated treatment pathways; comparison of completion rates and self-reported harm reduction outcomes between cohorts with and without legal pressure.',
    'If enforcement is experienced as coercive despite medicalized framing, the constraint''s suppression is higher than authored (internalized enforcement); the effective extraction rises and the constraint may reclassify toward snare. If care infrastructure is genuinely accessible without legal pressure, the coercion is a separable enforcement layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_care_boundary, empirical, 'Whether medicalization decouples from actual coercion or merely relabels it.').

omega_variable(
    black_market_persistence_function,
    'Does the persistent black market exist because treatment access is insufficient (coordination failure), because users prefer unregulated supply (autonomy preference), or because enforcement creates scarcity that the market fills?',
    'Natural experiment from jurisdictions that expand treatment capacity without enforcement: if markets contract, the market persists due to access failure; if markets persist at scale, user preference or enforcement-induced scarcity is the driver.',
    'If access-failure driven: the constraint''s mandate is live but undersupported — medicalization is sound but incompletely implemented, and additional resources would reduce extraction. If preference or enforcement-driven: the black market is a structural feature of the reading itself, and the constraint carries systemic extraction by design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_persistence_function, empirical, 'What perpetuates the coexisting black market under harm reduction authority.').

omega_variable(
    reading_vs_prohibition_kernel_contest,
    'How does this reading''s commitment to non-criminalization coexist with prohibition''s core premise that criminalization is morally necessary?',
    'The kernel (substance control legitimacy) admits both readings, but they hold incompatible axioms about state moral authority. No single unified framework can endorse both non-criminalization and mandatory criminalization simultaneously — the axioms foreclose each other at the framework level, but different jurisdictions/parties hold each simultaneously.',
    'This declares the reading_relations edge to prohibition_reading as coexists_with (not forecloses), because different parties maintain the readings across time and space despite logical incompatibility. Within any single party''s framework, one reading would foreclose the other; but the kernel sustains a multi-party contest without resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_prohibition_kernel_contest, conceptual, 'The logical status of this reading relative to prohibition within the contested kernel.').

omega_variable(
    legalization_boundary_ambiguity,
    'Does harm reduction''s authorization of regulated treatment markets constitute a step toward legalization, or a structurally distinct arrangement that maintains state authority over supply?',
    'Examine whether treatment-authorized supply is fully decriminalized, whether there are quantity limits or coercive monitoring, whether the supply infrastructure creates rents that benefit state or treatment sectors. If treatment monopolies extract comparable rents to prohibition-era enforcement, the boundary is pragmatic rather than principled.',
    'If treatment supply is genuinely market-competitive and non-extractive, harm reduction is structurally distinct from legalization. If treatment supply creates new rents or is rationed by state authority, harm reduction may be a midpoint reading that maintains extraction under a different justification — reclassification toward snare if extraction is underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legalization_boundary_ambiguity, empirical, 'Whether harm reduction''s authority structure is distinct from legalization or a pragmatic intermediate step.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way contested kernel. The prohibition_reading and legalization_reading are sibling constraints instantiating the same kernel with different legitimacy grounds. All three share referent (substance control authority) but differ in ε, beneficiary/victim structure, and type due to incompatible axioms about state authority. See the reading_relations and axioms in cs_structure for the logical map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
