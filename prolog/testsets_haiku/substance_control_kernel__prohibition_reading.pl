% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Use as Moral Transgression — Prohibition Reading
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'substance_control_kernel.' The prohibition reading frames substance use
 *   as moral transgression requiring criminal state punishment to protect
 *   social order. This reading is structurally distinct from sibling readings
 *   (harm-reduction, legalization) in its ε, victim/beneficiary structure,
 *   and foundational axioms. Under this reading, users and marginalized
 *   communities are victims; law enforcement and incarceration infrastructure
 *   are beneficiaries. The constraint's persistence depends on active
 *   enforcement of criminalization and suppression of alternative framings.
 *   The measurement series tracks rising extractiveness and theater over the
 *   interval, indicating the constraint's function has drifted from the
 *   founding problem (moral purity) toward enforcement-apparatus maintenance.
 *
 * KEY AGENTS:
 *   - substance_users: targets of criminalization, bearers of incarceration and collateral penalties
 *   - marginalized_communities: disproportionately enforced against regardless of actual use prevalence; trapped by structural racism in policing
 *   - people_with_substance_use_disorder: treated as criminals rather than patients; incarceration replaces medical treatment
 *   - law_enforcement_apparatus: primary beneficiary; expands budgets and authority through drug-enforcement mandates
 *   - incarceration_infrastructure: benefits from conviction pipeline; 15–20% of U.S. prison population held for drug offenses
 *   - political_authorities: enforce the reading; face electoral disincentives for admitting policy failure
 *   - harm_reduction_advocates: structurally excluded; their evidence is dismissed as enabling addiction
 *   - legalization_advocates: excluded from mainstream policy when prohibition dominates
 *   - observer_epidemiologist: analytical seat documenting overdose mortality, incarceration disparities, and treatment barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.79).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Moral Transgression — Prohibition Reading").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'd69ddcf2-a3d8-42a7-ac95-7ca74c7fc156').
narrative_ontology:cs_kernel_codification('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', implicit).
narrative_ontology:cs_authority_grounding('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', extraction).
narrative_ontology:cs_reading_relation('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', foundational, punishment_protects_social_order).
narrative_ontology:cs_axiom_status(punishment_protects_social_order, overridden).
narrative_ontology:cs_axiom_grounding('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', punishment_protects_social_order, empirically_contingent).
narrative_ontology:cs_reference_frame('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', moral_purity_through_criminalization).
narrative_ontology:cs_drift_state('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', contemporary_decriminalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d69ddcf2-a3d8-42a7-ac95-7ca74c7fc156', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, incarceration_infrastructure).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, correctional_industry).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, people_with_substance_use_disorder).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, moral_purity_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, social_order_through_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face criminal penalties for possession and use. Their behavior is framed as moral transgression rather than health condition or personal choice. Exit options collapse: they cannot cease use through willpower alone (neurological dependency), cannot access treatment without criminal exposure, and cannot migrate to jurisdictions with different frameworks without abandoning social networks and employment. Identity fusion occurs through repeated criminalization and incarceration record: 'I am a criminal' becomes self-understood despite the ontological mismatch with the act itself.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, biographical, identity_locked, national).

% Experience disproportionate enforcement regardless of actual use prevalence. Policing concentrates in neighborhoods where substance use is already visible due to poverty, housing instability, and lack of private spaces — not due to higher prevalence. A single arrest generates collateral damage: employment barriers, housing discrimination, family separation through incarceration. Communities lack resources to defend themselves or alter the constraint; their inability to exit is structural, not volitional.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, generational, trapped, national).

% Are treated as moral failures rather than patients. Incarceration becomes the default intervention, displacing evidence-based medical treatment. They are doubly trapped: addiction neurochemistry prevents voluntary cessation, and criminalization makes treatment-seeking self-incriminating. Medical exceptions exist (drug courts, diversion) but are scarce, discretionary, and contingent on admitting guilt — not guarantees.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, people_with_substance_use_disorder, payer,
    powerless, biographical, constrained, national).

% Expands budgets, personnel, and equipment through drug-enforcement mandates. Substance control provides continuous enforcement work that justifies departments' existence and generates political support from lawmakers who benefit from appearing 'tough on crime.' Asset forfeiture provides direct financial incentive. Police have structural interest in maintaining prohibition enforcement even as alternative policy frameworks gain credibility.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, agenda_setter).

% Grows occupancy and operational revenue through substance-related convictions. Approximately 15–20% of U.S. incarcerated population is held for drug offenses. Public and private prison systems depend on conviction pipelines for budgetary justification and revenue. Decriminalization directly threatens institutional survival, creating structural resistance to policy change.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, incarceration_infrastructure, beneficiary,
    institutional, generational, analytical, national).

% Private corporations operating correctional facilities, telephone services, commissaries, and re-entry programs extract value from incarceration. They lobby against decriminalization and for sentencing enhancement. Their profit model depends on stable or rising incarceration numbers; falling numbers are existential threat.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, correctional_industry, beneficiary,
    powerful, generational, analytical, national).

% Legislate and enforce drug prohibition. They face electoral incentives to appear tough on substance use, creating path dependence: announcing a shift to harm reduction or legalization is politically dangerous even when evidence supports it. The prohibition framework is maintained partly because changing course looks like weakness, not because evidence supports the original framing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, political_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from legal status of particular substances while others are criminalized. Opioid manufacturers profited from legal distribution of addiction-causing drugs while street-level users face felony prosecution for the same neurochemical dependency. The constraint creates asymmetric legal treatment that favors pharmaceutical over street supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, pharmaceutical_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Are structurally excluded from policy-making forums when the prohibition reading dominates. Their evidence on overdose prevention, needle-exchange efficacy, and medication-assisted treatment is dismissed as 'enabling addiction.' Underground harm-reduction operations (needle programs, supervised consumption sites) exist despite criminalization, but lack legal protection or funding. Their voice would fundamentally reframe the constraint, but they lack institutional power to enforce that reframing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% Are excluded from mainstream policy conversation when prohibition dominates, though their reading has gained credibility in some jurisdictions (cannabis legalization, opioid policy reform). They argue substance use is a liberty issue or a health issue, not a moral issue — a claim that directly contests the prohibition reading's foundational premise.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, legalization_advocates, excluded,
    moderate, biographical, constrained, national).

% Documents the structural outcomes of the prohibition reading: overdose mortality, incarceration disparities, black-market violence, treatment-seeking barriers, and policy incoherence (legal pharmaceutical addiction vs. criminalized street addiction). From the analytical seat, the constraint's extractiveness and suppression are visible as structural facts that the constraint's defenders must actively reinterpret or deny.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, observer_epidemiologist, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prohibition claims to solve: preventing substance abuse from destabilizing social order and moral norms. The real coordination problem is managing divergent preferences about what counts as moral — some see substance use as inherent vice requiring state sanction, others see it as health or liberty issue. The constraint does not solve the divergence; it enforces one reading.
% TRANSFER_FUNCTION: Moves freedom, income, and life-opportunity from substance users and marginalized communities (through criminalization, incarceration, employment barriers, asset forfeiture) to law enforcement, incarceration infrastructure, and political authorities (through budgets, personnel, and electoral capital). A secondary transfer moves legitimacy from harm-reduction and legalization readings to the prohibition reading.
% ABSENT_VOICES: Harm-reduction practitioners, people in active recovery, epidemiologists documenting overdose mortality, and legalization advocates are structurally excluded from policy deliberation when the prohibition reading dominates. They would argue the constraint is killing people and generating greater harm than it prevents. Their absence from the table is not accidental — the constraint's enforcer seats have structural interest in excluding evidence that contradicts the moral-transgression framing.
% DISAPPEARANCE_RATIONALE: If the prohibition reading and its enforcement vanished overnight, the world reorganizes substantially: incarceration numbers collapse (15–20% of U.S. prison population released), criminal supply chains dissolve, overdose-prevention resources shift from law enforcement to public health, people with substance-use disorder can access treatment without criminalization, and social policy reframes around harm reduction or legalization. The constraint is not a natural fact; it is sustained by active institutional choice.
% FOUNDING_PROBLEM: Early 20th-century moral panics about opium and cocaine linked substance use to criminality, racialized immigrant communities, and cultural decline. The prohibition reading emerged from that moment: substance use is moral transgression, criminalization is the appropriate response. The founding problem was framed as 'drug epidemics threaten the nation's moral fiber.'
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and conservative political authorities attest the founding problem is live and permanent — substance use is inherently a moral threat. Epidemiologists, harm-reduction practitioners, and public-health officials attest the founding problem is misframed: the problem is not substance use itself but the health and social harms associated with particular patterns of use under particular policy regimes. Jurisdictions that adopted harm-reduction or legalization policies report no moral collapse and lower overdose mortality, suggesting the founding problem as originally stated is either dead or misdescribed. No corroboration from outside the law-enforcement and pro-prohibition political seats.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) and rising because the constraint transfers freedom, income, employment, and life-opportunity from users and marginalized communities to enforcement and incarceration apparatus. This transfer is not incidental to the constraint — it is the mechanism by which punishment functions. Suppression is higher still (0.79) because the constraint persists partly through coercion (arrest machinery, incarceration threat) and partly through internalized shame and moral self-condemnation. Theater ratio (0.41) is moderate-to-high and rising, indicating that enforcement activity increasingly serves to maintain the apparatus itself rather than to achieve the stated founding problem (preventing moral degradation of society). The measurement series on one shared time grid shows extractiveness and theater rising together while founding problem outcomes (overdose prevention, social stability) do not improve and may worsen. The coercion grid differentiates levels: individual-level suppression is highest (0.78 at t50); class-level resistance is also high (0.75), indicating organized pushback from marginalized communities and harm-reduction advocates, while organizational resistance is moderate (0.72) — beneficiary organizations inside the law-enforcement seat are weakly resisting decriminalization pressure. Structural-level resistance (0.61) is lower, indicating mainstream policy discourse is slowly shifting toward alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   From the law-enforcement and pro-prohibition political seat, the constraint solves a genuine coordination problem (protecting social order from moral hazard of substance tolerance). From the substance-user and marginalized-community seats, the same constraint operates as pure extraction masked by moral framing. The engine computes per-seat classifications from the structural data: agenda-setter seats (law enforcement, political authorities) should compute snare-with-beneficiary confirmation; payer seats (users, marginalized communities) should compute snare-with-victim confirmation; observer seats should compute snare from the structural asymmetry. The perspectival gap is the divergence in how these seats experience and classify the same constraint — not an error or ambiguity, but the central finding: the constraint extracts from one seat while claiming to protect from harm experienced by another.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users and marginalized communities are structural targets (d near 1.0): they pay through criminalization, incarceration, employment barriers, and collateral damage, while lacking exit options (neurological dependency for users, structural racism for communities) or arbitrage (no alternative jurisdiction offers the same opportunities without comparable substance-control constraints). Law enforcement and incarceration infrastructure are structural beneficiaries (d near 0.0 or negative, meaning the constraint subsidizes their operation): they collect budgets, personnel growth, and authority expansion from the prohibition constraint itself. Political authorities sit between: they are beneficiaries electorally (appearing tough on crime) but payers operationally (managing the fiscal and social costs of mass incarceration). The directionality derivation from beneficiary/victim + exit naturally produces this structure; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was 'substance use destabilizes social order through moral contagion.' The constraint was built to solve it through criminalization. At interval start, extractiveness was 0.58 — still partially addressing the stated problem. By interval end, extractiveness has risen to 0.82 and theater has risen to 0.41, indicating the constraint's primary function has shifted from moral protection to enforcement-apparatus maintenance. The founding-problem status is CONTESTED: law enforcement attests it is live (substances still threaten order); epidemiologists and harm-reduction practitioners attest it is dead (substance use itself is not causing the harms — prohibition is). The disappearance verdict is WORLD_REARRANGES: the constraint is sustained by active institutional choice, not by underlying necessity. This (status: contested, verdict: world_rearranges) mismatch triggers the mandatrophy flag: the constraint's mandate has outlived its function, and its persistence is now driven by beneficiary institutional interests, not by genuine coordination need. The rising theater ratio (0.41 at t50) supports this reading: enforcement activity is increasingly performative — defending the apparatus against decriminalization pressure — rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is substance use a moral transgression (prohibition reading), a health condition (harm-reduction reading), or a liberty issue (legalization reading)?',
    'Comparison across jurisdictions: track overdose mortality, incarceration rates, treatment access, and community health metrics under different readings to establish which framing produces better outcomes on contested dimensions (harm reduction vs. moral purity vs. individual liberty).',
    'This is the core kernel contest. Each reading produces a different constraint with different ε, different victim/beneficiary structure, and different classifications. The prohibition reading classifies as snare; harm-reduction reads as tangled rope or rope; legalization reads as rope or mountain (depending on externality framing). The engine cannot resolve this — the contest is irreducibly conceptual and political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which framing of substance use is correct — moral, medical, or liberty?').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression (0.79) driven by external coercion (arrest machinery, incarceration threat, legal barriers to treatment) or internalized shame and moral self-condemnation?',
    'Post-decriminalization outcome tracking: if suppression of substance use persists after external enforcement is removed (e.g., in jurisdictions that decriminalized), it indicates internalized suppression; if it declines, the suppression was primarily structural coercion.',
    'If suppression is primarily structural, the measured suppression accurately reflects the constraint''s enforcement cost. If suppression is primarily internalized, the true suppression level may persist after decriminalization, making the constraint more durable than its metrics suggest and indicating deeper identity-fusion with the criminal label than the external enforcement picture alone reveals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural coercion versus internalized moral suppression in substance-use prohibition.').

omega_variable(
    black_market_extraction_externality,
    'Is the black-market violence and organized-crime profit that the prohibition reading generates a structural side effect of prohibition itself (extraction hidden in externalities) or an independent harm that prohibition was meant to prevent?',
    'Controlled comparison: compare black-market violence and organized-crime presence in prohibition vs. harm-reduction vs. legalization jurisdictions. If violence scales with enforcement intensity (not use prevalence), prohibition itself is the violence generator.',
    'If black-market violence is prohibition-generated, then measured extraction is understated: the constraint extracts not only incarceration and lost opportunity from users but also violence and destabilization from the wider community. True extractiveness might be higher than 0.82 when externalities are included.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_extraction_externality, empirical, 'Whether prohibition generates the harms it claims to prevent.').

omega_variable(
    pharmaceutical_asymmetry_structural_or_historical,
    'Is the legal treatment of pharmaceutical addiction (tolerated by medical authority) versus street-use criminalization a necessary feature of prohibition or a historical artifact of patent protection and medical profession licensing?',
    'Regulatory history and comparative policy analysis: examine how pharmaceutical and street supply are treated under the same active molecule (opioids under prescription vs. heroin). If the distinction tracks institutional power rather than pharmacology, it is structural injustice, not principled differentiation.',
    'If the pharmaceutical asymmetry is historical artifact, not principled, then the prohibition reading is hiding a principle-to-application gap: it claims to protect against moral transgression but actually protects institutional interests (pharmaceutical manufacturers, medical licensing). This would deepen the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_asymmetry_structural_or_historical, empirical, 'Whether the prohibition reading treats substances consistently or protects institutional interests through differential enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__prohibition_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__prohibition_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(subs_tr_t16, observed).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__prohibition_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(subs_tr_t24, observed).
narrative_ontology:measurement(subs_tr_t32, substance_control_kernel__prohibition_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement_basis(subs_tr_t32, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(subs_tr_t40, observed).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(subs_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__prohibition_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__prohibition_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement_basis(subs_be_t16, observed).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__prohibition_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement_basis(subs_be_t24, observed).
narrative_ontology:measurement(subs_be_t32, substance_control_kernel__prohibition_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement_basis(subs_be_t32, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(subs_be_t40, observed).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(subs_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__prohibition_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__prohibition_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(subs_su_t16, observed).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__prohibition_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement_basis(subs_su_t24, observed).
narrative_ontology:measurement(subs_su_t32, substance_control_kernel__prohibition_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement_basis(subs_su_t32, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(subs_su_t40, observed).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(subs_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(subs_grid_01, substance_control_kernel__prohibition_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(subs_grid_02, substance_control_kernel__prohibition_reading, accessibility_collapse(class), 50, 0.68).
narrative_ontology:measurement(subs_grid_03, substance_control_kernel__prohibition_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(subs_grid_04, substance_control_kernel__prohibition_reading, accessibility_collapse(individual), 50, 0.74).
narrative_ontology:measurement(subs_grid_05, substance_control_kernel__prohibition_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(subs_grid_06, substance_control_kernel__prohibition_reading, accessibility_collapse(organizational), 50, 0.61).
narrative_ontology:measurement(subs_grid_07, substance_control_kernel__prohibition_reading, accessibility_collapse(structural), 0, 0.7).
narrative_ontology:measurement(subs_grid_08, substance_control_kernel__prohibition_reading, accessibility_collapse(structural), 50, 0.71).
narrative_ontology:measurement(subs_grid_09, substance_control_kernel__prohibition_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(subs_grid_10, substance_control_kernel__prohibition_reading, resistance(class), 50, 0.75).
narrative_ontology:measurement(subs_grid_11, substance_control_kernel__prohibition_reading, resistance(individual), 0, 0.42).
narrative_ontology:measurement(subs_grid_12, substance_control_kernel__prohibition_reading, resistance(individual), 50, 0.48).
narrative_ontology:measurement(subs_grid_13, substance_control_kernel__prohibition_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(subs_grid_14, substance_control_kernel__prohibition_reading, resistance(organizational), 50, 0.72).
narrative_ontology:measurement(subs_grid_15, substance_control_kernel__prohibition_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(subs_grid_16, substance_control_kernel__prohibition_reading, resistance(structural), 50, 0.61).
narrative_ontology:measurement(subs_grid_17, substance_control_kernel__prohibition_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(subs_grid_18, substance_control_kernel__prohibition_reading, stakes_inflation(class), 50, 0.58).
narrative_ontology:measurement(subs_grid_19, substance_control_kernel__prohibition_reading, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(subs_grid_20, substance_control_kernel__prohibition_reading, stakes_inflation(individual), 50, 0.71).
narrative_ontology:measurement(subs_grid_21, substance_control_kernel__prohibition_reading, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(subs_grid_22, substance_control_kernel__prohibition_reading, stakes_inflation(organizational), 50, 0.45).
narrative_ontology:measurement(subs_grid_23, substance_control_kernel__prohibition_reading, stakes_inflation(structural), 0, 0.61).
narrative_ontology:measurement(subs_grid_24, substance_control_kernel__prohibition_reading, stakes_inflation(structural), 50, 0.64).
narrative_ontology:measurement(subs_grid_25, substance_control_kernel__prohibition_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(subs_grid_26, substance_control_kernel__prohibition_reading, suppression(class), 50, 0.71).
narrative_ontology:measurement(subs_grid_27, substance_control_kernel__prohibition_reading, suppression(individual), 0, 0.75).
narrative_ontology:measurement(subs_grid_28, substance_control_kernel__prohibition_reading, suppression(individual), 50, 0.78).
narrative_ontology:measurement(subs_grid_29, substance_control_kernel__prohibition_reading, suppression(organizational), 0, 0.45).
narrative_ontology:measurement(subs_grid_30, substance_control_kernel__prohibition_reading, suppression(organizational), 50, 0.48).
narrative_ontology:measurement(subs_grid_31, substance_control_kernel__prohibition_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(subs_grid_32, substance_control_kernel__prohibition_reading, suppression(structural), 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, racial_disparity_in_incarceration).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, pharmaceutical_industry_regulatory_capture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of substance_control_kernel. The harm-reduction and legalization readings are sibling constraints (different stories, same kernel). They share measurement apparatus but produce different classifications and have different victims/beneficiaries. All three stories must be linked via network.affects_constraints to enable contamination analysis across the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
