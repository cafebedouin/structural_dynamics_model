% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Use as Moral Transgression (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of the broader
 *   'substance_control_kernel', where substance use is framed as a moral
 *   transgression requiring state punishment to protect social order. This
 *   reading leads to high extraction from users and marginalized communities,
 *   high suppression through aggressive law enforcement, and significant
 *   benefits for the enforcement apparatus and black market actors. The
 *   state's authority is coercive, not service-provision.
 *
 * KEY AGENTS:
 *   - law_enforcement_agencies: Agenda setter (institutional/constrained) — primary beneficiary of the system's perpetuation.
 *   - substance_users: Primary payer (powerless/trapped) — bears criminalization and social stigma.
 *   - marginalized_communities: Payer (powerless/identity_locked) — disproportionately targeted, bears systemic oppression.
 *   - private_prison_industry: Beneficiary (organized/mobile) — profits from incarceration rates.
 *   - black_market_actors: Beneficiary (organized/arbitrage) — thrives on prohibition-driven profits and violence.
 *   - public_health_systems: Payer (institutional/constrained) — burdened by health consequences and diverted resources.
 *   - moral_conservatives: Beneficiary (organized/mobile) — gains political capital from punitive stance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Moral Transgression (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'a93d0cd1-1d00-489e-b304-9546da854a59').
narrative_ontology:cs_kernel_codification('a93d0cd1-1d00-489e-b304-9546da854a59', formalized).
narrative_ontology:cs_authority_grounding('a93d0cd1-1d00-489e-b304-9546da854a59', extraction).
narrative_ontology:cs_interpretation_layer_present('a93d0cd1-1d00-489e-b304-9546da854a59').
narrative_ontology:cs_reading_relation('a93d0cd1-1d00-489e-b304-9546da854a59', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93d0cd1-1d00-489e-b304-9546da854a59', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('a93d0cd1-1d00-489e-b304-9546da854a59', foundational, substance_use_is_moral_failing).
narrative_ontology:cs_axiom_status(substance_use_is_moral_failing, holdable).
narrative_ontology:cs_axiom_grounding('a93d0cd1-1d00-489e-b304-9546da854a59', substance_use_is_moral_failing, deontological).
narrative_ontology:cs_axiom('a93d0cd1-1d00-489e-b304-9546da854a59', foundational, punishment_protects_social_order).
narrative_ontology:cs_axiom_status(punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('a93d0cd1-1d00-489e-b304-9546da854a59', punishment_protects_social_order, instrumental).
narrative_ontology:cs_reference_frame('a93d0cd1-1d00-489e-b304-9546da854a59', moral_purity_social_order).
narrative_ontology:cs_drift_state('a93d0cd1-1d00-489e-b304-9546da854a59', contemporary_public_health_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a93d0cd1-1d00-489e-b304-9546da854a59', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, black_market_actors).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, public_health_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, moral_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces drug laws, receiving significant funding, resources, and expanded powers for interdiction, arrest, and incarceration. Benefits from the perpetuation of the 'drug problem' as a justification for its mandate and budget.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Face criminalization, incarceration, social stigma, and barriers to employment and housing. Their health issues are treated as moral failings, leading to punitive rather than therapeutic interventions. Exit from substance use is made harder by the criminal justice system.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by enforcement, leading to family separation, community destabilization, and cycles of poverty and incarceration. The moral framing of substance use reinforces existing social inequalities and provides cover for systemic oppression.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Profits directly from increased incarceration rates driven by drug offenses, lobbying for stricter laws and longer sentences. Their business model is directly tied to the punitive approach to substance use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_industry, beneficiary,
    organized, biographical, mobile, national).

% Thrive in the absence of legal regulation, controlling supply and pricing, and operating with impunity due to the criminalization of alternatives. They benefit from the high profits generated by prohibition and the violence used to maintain market control.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_actors, beneficiary,
    organized, biographical, arbitrage, global).

% Are burdened by the health consequences of unregulated substances and the lack of access to care for criminalized users. Resources are diverted from prevention and treatment to managing the fallout of punitive policies, leading to worse public health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_systems, payer,
    institutional, generational, constrained, national).

% See the prohibition framework as upholding traditional moral values and protecting society from perceived decay. They benefit from the political capital gained by advocating for punitive measures and maintaining a 'tough on crime' stance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_conservatives, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state power to enforce a moral code against substance use, aiming to protect societal norms and public order through deterrence and punishment.
% TRANSFER_FUNCTION: Transfers individual liberty and resources (via fines, incarceration costs, lost productivity) from substance users and marginalized communities to law enforcement, the private prison industry, and black market actors.
% ABSENT_VOICES: Public health advocates, civil liberties organizations, and former substance users would argue for a health-centered, rights-respecting approach, but their perspectives are marginalized in policy debates dominated by punitive rhetoric.
% DISAPPEARANCE_RATIONALE: If the prohibition framework vanished overnight, the criminal justice system would be dramatically reshaped, black markets would collapse, and public health systems would need to rapidly scale up treatment and regulation infrastructure. Society would reorganize around a new approach to substance use.
% FOUNDING_PROBLEM: The perceived moral decay and social disorder associated with certain substances, leading to a belief that state intervention was necessary to uphold public morality and protect society from itself.
% FOUNDING_PROBLEM_CORROBORATION: Moral conservatives and law enforcement agencies attest the problem is live, citing ongoing social harms. Public health experts and civil liberties advocates, corroborated by empirical data on crime rates and health outcomes, attest the founding problem is misdiagnosed and the punitive approach exacerbates rather than solves it.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is very high (0.85) because the system criminalizes a health issue, extracting liberty, wealth, and social standing from users. Suppression is also very high (0.92) due to aggressive policing, mandatory minimum sentences, and the active suppression of harm reduction or legalization alternatives. The theater ratio (0.45) reflects that while some enforcement activity genuinely aims to reduce harm, a significant portion is performative, maintaining the moralistic framework and justifying the system's existence, even as it creates secondary harms like black market violence. Accessibility collapse is high (0.75) as legal alternatives are suppressed, and resistance is moderate (0.7) from affected communities and advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   Law enforcement and moral conservatives perceive this as a necessary, legitimate constraint for social order, while substance users, marginalized communities, and public health systems experience it as a deeply extractive and harmful snare. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and the private prison industry are clear beneficiaries (low d) as they gain resources and legitimacy. Black market actors also benefit from the market conditions created by prohibition. Substance users and marginalized communities are clear targets (high d) as they bear the brunt of criminalization and its social costs. Public health systems are targets as their resources are diverted and their mission undermined. Moral conservatives are beneficiaries of the political and social capital derived from this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting social order by punishing moral transgression) is increasingly contested. While the founding problem (perceived social decay) is still invoked, the actual operation of the constraint has shifted to primarily benefit the enforcement apparatus and black markets, while creating significant social harms. This suggests a strong mandatrophy signal, where the original coordination function (if any) has atrophied, and the constraint persists due to the concentrated benefits to its administrators and the suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_vs_health_framing,
    'Is substance use fundamentally a moral failing requiring punishment, or a public health issue requiring treatment and regulation?',
    'Longitudinal studies comparing societal outcomes (crime rates, public health metrics, economic impact) in jurisdictions adopting punitive vs. public health approaches.',
    'If framed as a health issue, the constraint''s extractiveness and suppression would be re-evaluated as counterproductive, leading to reclassification towards a scaffold (transitional support for health) or rope (regulated market). If the moral framing is upheld, the current classification as a snare would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_health_framing, conceptual, 'The core conceptual ambiguity driving the prohibition reading.').

omega_variable(
    prohibition_vs_harm_reduction_efficacy,
    'Does the prohibition reading effectively reduce overall societal harm from substance use, or does it create greater harms (e.g., black market violence, disease transmission, mass incarceration)?',
    'Comparative empirical analysis of public health and safety outcomes under prohibition versus harm reduction or legalization regimes.',
    'If prohibition is shown to create greater harms, its justification as a coordination mechanism collapses, reinforcing its classification as a snare. If it demonstrably reduces harm, its extractiveness might be re-evaluated as a necessary cost of a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_vs_harm_reduction_efficacy, empirical, 'Empirical efficacy of prohibition in achieving its stated goals.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''prohibition_reading'' of the ''substance_control_kernel''. How would the classification change under the ''harm_reduction_reading'' or ''legalization_reading''?',
    'Analyze the structural deltas: harm_reduction_reading would shift users from victims to beneficiaries of health services (lower extraction, lower suppression); legalization_reading would remove criminal penalties entirely, shifting state role to regulation and taxation (minimal extraction, minimal suppression).',
    'The prohibition_reading is a snare due to high extraction and suppression. The harm_reduction_reading would likely be a tangled_rope or scaffold, while the legalization_reading would likely be a rope or even a mountain (if the market self-regulates effectively).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative readings of the substance_control_kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__prohibition_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(subs_tr_t2024, substance_control_kernel__prohibition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(subs_be_t2024, substance_control_kernel__prohibition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__prohibition_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(subs_su_t2024, substance_control_kernel__prohibition_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, criminal_justice_system_funding).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, public_health_resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, black_market_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel'. Other readings include 'harm_reduction_reading' and 'legalization_reading', which would yield different classifications and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
