% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibition Legitimacy Framing: Substance Use as Inherent Harm Justifying Criminalization
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   The prohibition reading frames substance use as inherently harmful and
 *   justifies state authority to criminalize possession and consumption as a
 *   moral necessity. This reading treats the constraint as a natural law of
 *   harm prevention—use always harms, therefore criminalization is always
 *   justified. Under this reading, users and targeted communities are the
 *   problem to be controlled, not agents whose interests require negotiation.
 *   Law enforcement and correctional institutions benefit from the sustained
 *   illegality and the population it produces. The constraint's measured
 *   extractiveness (0.81) reflects that it takes freedom, dignity, and
 *   economic access from users and communities while delivering institutional
 *   expansion and budgetary justification to enforcement. The suppression
 *   requirement (0.89) reflects that the constraint's persistence depends on
 *   active enforcement, not on participant agreement or coordination benefit.
 *   Theater ratio (0.52) reflects that while a genuine public-health concern
 *   motivated the founding constraint, enforcement now operates substantially
 *   to maintain institutional capacity rather than to address harm reduction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.89).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibition Legitimacy Framing: Substance Use as Inherent Harm Justifying Criminalization").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '9154eb58-ca19-46bd-9a46-63ae7d5e7506').
narrative_ontology:cs_kernel_codification('9154eb58-ca19-46bd-9a46-63ae7d5e7506', formalized).
narrative_ontology:cs_authority_grounding('9154eb58-ca19-46bd-9a46-63ae7d5e7506', extraction).
narrative_ontology:cs_interpretation_layer_present('9154eb58-ca19-46bd-9a46-63ae7d5e7506').
narrative_ontology:cs_reading_relation('9154eb58-ca19-46bd-9a46-63ae7d5e7506', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('9154eb58-ca19-46bd-9a46-63ae7d5e7506', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('9154eb58-ca19-46bd-9a46-63ae7d5e7506', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('9154eb58-ca19-46bd-9a46-63ae7d5e7506', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('9154eb58-ca19-46bd-9a46-63ae7d5e7506', foundational, state_duty_criminalize_inherent_harm).
narrative_ontology:cs_axiom_status(state_duty_criminalize_inherent_harm, holdable).
narrative_ontology:cs_axiom_grounding('9154eb58-ca19-46bd-9a46-63ae7d5e7506', state_duty_criminalize_inherent_harm, deontological).
narrative_ontology:cs_reference_frame('9154eb58-ca19-46bd-9a46-63ae7d5e7506', moral_duty_prevention_through_criminal_sanction).
narrative_ontology:cs_drift_state('9154eb58-ca19-46bd-9a46-63ae7d5e7506', contemporary_post_mass_incarceration_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9154eb58-ca19-46bd-9a46-63ae7d5e7506', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, correctional_industries).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, drug_prohibition_enforcement_infrastructure).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, communities_targeted_for_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, collateral_family_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, state_executives_and_legislators).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to criminal penalties for possession and use. Face incarceration, fines, permanent criminal records that block employment and housing access. The 'inherent harm' framing justifies state intervention in their most intimate choices. Exit (choosing not to use) is impossible for dependent users; exit (relocating to jurisdictions without criminalization) requires resources most lack. The constraint explicitly targets them as the problem to be solved, not as agents with interests to be negotiated.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Disproportionately policed neighborhoods experience enforcement intensity unrelated to consumption prevalence. The constraint's operation concentrates incarceration among Black and Latino communities despite equal or lower usage rates in white communities. They bear the external costs of enforcement (police presence, family disruption, school-to-prison pipeline) and are excluded from policy-setting that determines enforcement intensity in their neighborhoods.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, communities_targeted_for_enforcement, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, communities_targeted_for_enforcement, excluded).

% Enforce the prohibition through arrest, prosecution, and incarceration. The 'inherent harm' framing provides moral legitimacy for enforcement activity and justifies budget allocations to drug enforcement units. Enforcement operations are presented as necessary harm prevention, not as extraction of control over a population.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate prisons, jails, and re-entry programs that expand with incarceration volume. The constraint's operation produces a reliable, growing population of incarcerated individuals. Private prison contracts, public sector employment, and supply-chain vendors all benefit from sustained criminalization and high incarceration rates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, correctional_industries, beneficiary,
    institutional, generational, arbitrage, national).

% A complex of agencies, contractors, and institutional positions (DEA, narcotics units, asset forfeiture operations, treatment court infrastructure) all depend on the sustained illegality of substances for their institutional existence and budget justification. The 'inherent harm' framing is the legitimation story that maintains funding and operational authority.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, drug_prohibition_enforcement_infrastructure, beneficiary,
    institutional, generational, arbitrage, national).

% Criminalization creates scarcity rents and eliminates legal competition, making drug supply highly profitable. Violence emerges as the enforcement mechanism of contracts that cannot be enforced through courts. They are not named as beneficiaries (they do not endorse the constraint's legitimacy framework), but they structurally depend on the constraint's persistence for their market position.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_suppliers, excluded,
    moderate, biographical, trapped, regional).

% Tasked with health outcomes but operate under constraints set by criminal-justice framing. The 'inherent harm' reading subordinates harm-reduction approaches and evidence-based treatment to law-enforcement priorities. They can gather epidemiological data, publish research, and advocate for policy change, but enforcement remains the dominant institutional response.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% Operate needle-exchange programs, medication-assisted treatment, and overdose prevention services that reduce death and disease without addressing underlying use. The 'inherent harm' reading marginalizes these approaches as insufficient and potentially enabling, subordinating them to abstinence-first criminal enforcement. They would advocate for integration of harm reduction into public health policy but lack the institutional authority to override the criminal-justice framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% Bear diffuse, unmeasured costs: loss of income earners, emotional trauma from separation, visitation travel costs, collateral legal fees, and stigma. The constraint's operation distributes these costs invisibly; they are not named as victims by the prohibition framework but absorb significant harm from enforcement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_persons, payer,
    powerless, biographical, constrained, national).

% Sustain the prohibition through legislation and appropriations. The 'inherent harm' framing provides political cover: supporting enforcement appears as moral necessity rather than as policy choice. They benefit from simplified narratives that justify large public expenditures without requiring difficult tradeoff analysis or evidence evaluation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, state_executives_and_legislators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, state_executives_and_legislators, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves no genuine coordination problem—no party's interests align around the claim that substance use is inherently harmful. Instead, it enforces a hierarchical moral claim: the state unilaterally declares use inherently harmful and reserves the authority to criminalize possession and consumption as a matter of moral duty. There is no coordination among substance users, communities, or families—only enforcement of state-declared moral obligation.
% TRANSFER_FUNCTION: The constraint moves freedom, dignity, and economic access FROM substance users, targeted communities, and their families TO law enforcement and correctional institutions. It converts individual choices into crimes, generating a population of incarcerated individuals who provide justification for enforcement budgets, institutional expansion, and political claims about moral necessity. Secondary transfer: authority to define and criminalize 'harm' flows upward to the state; authority to resist or negotiate flows downward nowhere.
% ABSENT_VOICES: Substance users themselves are structurally excluded from policy-setting that defines them as problems requiring state control. Communities experiencing disproportionate enforcement have no veto over the enforcement intensity applied to their neighborhoods. Harm-reduction practitioners and public-health researchers whose evidence contradicts the 'inherent harm' framing are marginalized by law-enforcement authority. These absences are not accidental—the constraint's operation depends on excluding the voices that would contest the legitimation story.
% DISAPPEARANCE_RATIONALE: If criminalization disappeared overnight, the black market (currently profitable due to scarcity rents from illegality) would collapse or transition to legal supply chains. Incarceration rates would fall dramatically, freeing incarcerated individuals and eliminating a major public expenditure. Harm-reduction and public-health approaches would become primary institutional responses rather than marginalized alternatives. The enforcement infrastructure itself (narcotics units, asset-forfeiture operations, incarceration capacity dedicated to drug offenses) would no longer be justified and would face restructuring or elimination.
% FOUNDING_PROBLEM: Concern that recreational substance use, particularly of newly synthesized or imported drugs, posed public-health and social-order risks in the early 20th century, and that state authority to criminalize use was a proportional response to prevent harm.
% FOUNDING_PROBLEM_CORROBORATION: Public-health authorities and epidemiologists note that criminalization has not reduced substance use prevalence over a century of enforcement; instead, it has shifted the composition of drugs used (from heroin and cocaine to fentanyl and synthetic opioids) and increased overdose deaths by creating an unregulated, high-potency supply. Law-enforcement and correctional authorities attesting to the founding problem's continued 'liveness' cite the ongoing use and associated harms, but this circularity—use persists therefore prohibition is necessary—does not establish that the founding problem remains alive or that criminalization addresses it. The independent attestation comes from researchers, public-health officials, and international bodies (WHO, UN) finding that criminalization increases rather than decreases harm-related outcomes.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the constraint flows control, incarceration, and institutional capacity from users to the state and correctional industries without delivering coordination benefit or addressing stated harms. The upward trend (0.65 to 0.81) reflects accumulating extraction: as incarceration infrastructure expanded, the constraint captured more extractive value through carceral employment, asset forfeiture, and budget justification—the founding problem (preventing use) atrophied while institutional dependence on criminalization deepened. Suppression is higher (0.89) because criminalization is the operational mechanism; alternatives (harm reduction, legalization, regulated supply) are actively foreclosed by law enforcement. Theater ratio rising (0.28 to 0.52) indicates that enforcement increasingly operates to sustain institutional capacity rather than achieve stated harm-reduction goals—the constraint's operation has become substantially performative. Accessibility collapse (0.78) reflects that exit options for users are severely constrained: they cannot simply choose not to use if dependent, cannot relocate without resources, and face legal barriers to reintegration even after serving sentences. Resistance (0.72) reflects sustained opposition from public-health authorities, harm-reduction practitioners, and increasingly from affected communities and civil-rights organizations, yet enforcement persists despite this resistance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (law enforcement, executives) experience the constraint as legitimate moral authority and institutional opportunity; they compute it as a coordination mechanism defending social order against inherent harm. The payer seats (substance users, targeted communities) experience it as enforced control without coordination benefit and with catastrophic personal costs; they compute it as a snare whose legitimation story is false. The excluded seats (harm-reduction practitioners) perceive the constraint's operation as suppressing evidence-based alternatives. The engine should compute different types for different seats: enforcement positions near rope (coordination, legitimate authority), payer positions near snare (no coordination, pure extraction with false legitimation). The perspectival gap reflects that the constraint's legitimation story (inherent harm justifies criminalization) is contested—the prohibition reading asserts the justification; the harm-reduction reading rejects it by asserting public-health authority should override criminal authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users carry d near 1.0 (full target): criminalization is the enforcement object itself; they have trapped exit options and no arbitrage. Targeted communities carry d near 0.95 (near-full target): disproportionate enforcement creates asymmetric costs; they lack resources to exit or contest enforcement decisions. Law enforcement carries d near 0.0 (full beneficiary): the constraint justifies institutional expansion, budget allocation, and operational authority; their exit options are arbitrage (they can reposition to other enforcement domains). Correctional industries carry d near 0.0 (full beneficiary): the constraint produces a reliable incarcerated population; their exit is arbitrage (reposition to other security or containment functions). Public-health authorities carry d near 0.65 (partial target): they are constrained by law-enforcement authority and marginalized in policy-setting, yet retain some influence through evidence production and advocacy. Harm-reduction practitioners carry d near 0.70 (near-target): their primary function (harm reduction without enforcement) is structurally suppressed; they lack agenda-setting power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits severe mandatrophy: the founding problem (preventing substance use prevalence) is dead—a century of criminalization has not reduced use prevalence and instead has shifted to more lethal substances and higher overdose mortality. Yet the constraint persists and intensifies (extractiveness and suppression both rising over the interval) due to institutional dependence on criminalization. The enforcement infrastructure (narcotics units, asset-forfeiture operations, private prisons, drug-court bureaucracy) now depends on sustained criminalization for existence and budget justification, not on solving the founding problem. The theater ratio rising (0.28 to 0.52) confirms this: an increasing share of enforcement activity operates to sustain institutional capacity rather than address the stated founding problem. The disappearance verdict (world_rearranges) combined with founding_problem_status (dead) is the signature mandatrophy cell: the world would rearrange if the constraint vanished (incarceration would fall, enforcement infrastructure would shrink), yet the founding problem that originally justified the constraint is no longer live. Classification should reflect this: the constraint is a snare maintained by institutional capture, not a rope solving a live coordination problem. The mandatrophy resolution: the constraint persists because multiple institutional actors (law enforcement, correctional industries, state legislatures deriving simplified political narratives) benefit from its operation, not because the founding problem remains unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_and_measurement,
    'What constitutes ''inherent harm'' from substance use, and is it measurable in a way that justifies criminalization as the primary response?',
    'Comparative epidemiology: measure health and social outcomes (overdose mortality, incarceration mortality, employment and housing stability, family disruption, disease transmission) across jurisdictions with criminalization vs. harm-reduction vs. legalization approaches, controlling for baseline prevalence and demographics. Natural experiments (Portugal decriminalization, Switzerland heroin-assisted treatment, US cannabis legalization) provide observational data on outcome divergence.',
    'If criminalization produces worse outcomes than harm-reduction or legalization on measured harm metrics (overdose deaths, disease, family stability, recidivism), the legitimation story (inherent harm justifies criminalization) is empirically false. The constraint''s classification would shift from snare-with-false-legitimation to unambiguous snare: extraction defended by a false harm narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_and_measurement, empirical, 'Whether criminalization actually reduces the harm it claims to prevent, or increases it.').

omega_variable(
    moral_authority_scope,
    'Does the state possess legitimate moral authority to criminalize individual substance use in the absence of demonstrated third-party harm?',
    'This is a normative philosophical question, not empirically resolvable. Resolution depends on which foundational axioms the policy framework accepts: autonomy-based (individuals have rights to self-determination in private decisions), paternalist (the state has duty to prevent harm even from private choices), or harm-principle-based (state authority justified only to prevent third-party harm). Different readings of the kernel instantiate different axioms.',
    'A framework that prioritizes autonomy or harm-principle grounds would foreclose the prohibition reading''s core claim (inherent individual harm justifies criminalization). If that axiom is formally overridden or abandoned by the political authority that endorsed it, the reading would shift to overridden status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_authority_scope, conceptual, 'Whether state moral authority extends to controlling private consensual substance use.').

omega_variable(
    institutional_capture_and_persistence,
    'To what extent does the constraint persist because of institutional dependence on criminalization (correctional employment, law-enforcement budgets, political simplicity) rather than because of genuine persistence of the founding problem?',
    'Institutional analysis: map the distribution of budget, employment, and political incentives flowing from sustained criminalization. Trace how enforcement agencies defend criminalization in policy debates even when faced with evidence of harm-reduction superiority. Examine whether advocates for prohibition include only those directly benefiting from enforcement infrastructure, or whether it includes parties without institutional stake.',
    'If the primary defenders of prohibition are parties with institutional dependence on criminalization, and if constraint persistence would entail significant institutional disruption and budget loss to abandon it, the classification confidence for snare increases: institutional capture fully explains persistence independent of the legitimation story''s truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_and_persistence, empirical, 'Whether the constraint persists due to institutional capture rather than genuine policy necessity.').

omega_variable(
    reading_contest_foreclosure,
    'Do the prohibition reading''s core premises (substance use is inherently harmful, state has duty to criminalize) logically foreclose the sibling readings'' core premises, or do the readings represent different normative frameworks that can coexist across different parties?',
    'Structural analysis of the axioms: if prohibition axiom (inherent harm justifies criminalization) is accepted, harm-reduction reading (harm is minimized without criminalization) would be logically incompatible in the same framework. If the readings contest empirical questions about what minimizes harm, they coexist (one is empirically wrong, but both are held). If they contest foundational normative claims (whether state authority extends to private harm), they coexist across different political traditions.',
    'If foreclosure is true, the readings represent logical contradictions that cannot be simultaneously held by one authority. If coexistence is true, the readings represent different parties'' legitimate normative stances, and the constraint contest is a political dispute, not a logical contradiction. Foreclosure status affects the cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the kernel''s readings logically foreclose each other or represent coexistent normative frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__prohibition_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__prohibition_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__prohibition_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(subs_tr_t35, substance_control_legitimacy__prohibition_reading, theater_ratio, 35, 0.51).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__prohibition_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__prohibition_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__prohibition_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(subs_be_t35, substance_control_legitimacy__prohibition_reading, base_extractiveness, 35, 0.8).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__prohibition_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__prohibition_reading, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__prohibition_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(subs_su_t35, substance_control_legitimacy__prohibition_reading, suppression_requirement, 35, 0.88).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel has three constraint stories, each instantiating one reading of the contested kernel. All three share the same material domain (substance regulation) but diverge on what 'harm' means, what legitimates state authority, and whether criminalization is justified. The prohibition_reading (this file) frames inherent harm as justifying criminalization and produces high extractiveness and suppression. The harm_reduction_reading frames harm as minimizable through public health rather than criminalization, producing lower extractiveness. The legalization_reading frames autonomy as paramount and state authority as limited to third-party harm, producing different victim and beneficiary sets. The three stories are linked via this network field; each documents in cs_structure the reading_relations and axioms that distinguish it from its siblings. This decomposition follows the ε-invariance principle: each reading instantiates a different ε, different beneficiary/victim structure, and different classification—they are not different measurements of one constraint, but three structurally distinct constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
