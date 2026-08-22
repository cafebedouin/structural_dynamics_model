% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalized Substance Market with Externality Capture
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the legalization_reading of the
 *   substance_control_kernel. It treats adult substance use as an individual
 *   liberty interest and limits state intervention to preventing third-party
 *   harm (DUI, public exposure) and capturing externality costs via taxation.
 *   The reading exits substance users from the victim set entirelyâunlike
 *   prohibitionâand introduces the legal industry and state treasury as
 *   beneficiaries, while third parties enter the victim set via uncompensated
 *   externalities. The constraint is claimed as tangled_rope because it
 *   combines a genuine coordination function (safe supply, tax revenue,
 *   reduced incarceration) with asymmetric extraction borne by diffuse
 *   third-party victims who did not consent to the market's risk profile.
 *
 * KEY AGENTS:
 *   - substance_users: Primary beneficiary (organized/constrained) â gain liberty and safety but pay embedded taxes and accept use restrictions.
 *   - legal_substance_industry: Dual-positioned beneficiary/payer (powerful/constrained) â gains legitimacy and market access but pays heavy excise taxes and compliance costs.
 *   - state_tax_authority: Agenda-setter (institutional/analytical) â designs and enforces the tax/regulatory framework, collects revenue.
 *   - third_party_harm_victims: Primary payer (powerless/trapped) â bear uncompensated externality costs with no contractual relationship to the market and limited exit from public-space exposure.
 *   - prohibitionist_advocates: Excluded voice (organized/constrained) â previously dominant, now marginalized from the policy conversation.
 *   - black_market_operators: Excluded (moderate/trapped) â displaced but not eliminated, operating in tax-arbitrage gray zones.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.58).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.48).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalized Substance Market with Externality Capture").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, 'e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03').
narrative_ontology:cs_kernel_codification('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', distributed).
narrative_ontology:cs_authority_grounding('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', distributed).
narrative_ontology:cs_reading_relation('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', foundational, substance_use_as_individual_liberty).
narrative_ontology:cs_axiom_status(substance_use_as_individual_liberty, holdable).
narrative_ontology:cs_axiom_grounding('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', substance_use_as_individual_liberty, deontological).
narrative_ontology:cs_axiom('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', foundational, state_intervention_externality_bound).
narrative_ontology:cs_axiom_status(state_intervention_externality_bound, holdable).
narrative_ontology:cs_axiom_grounding('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', state_intervention_externality_bound, conventional).
narrative_ontology:cs_reference_frame('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', liberty_regulatory_framework).
narrative_ontology:cs_drift_state('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', mature_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e70ca4ad-2034-4bfc-bfba-ebf7ac27ea03', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_authority).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_harm_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, legal_substance_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to regulated substances without criminal penalty, benefiting from product safety standards and known potency. Bear excise taxes and compliance costs embedded in retail prices. Subject to public-use restrictions, driving prohibitions, and possession limits that bound their liberty within the framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    organized, biographical, constrained, national).

% Operate within a licensed legal market with legitimacy and banking access. Pay substantial excise taxes, licensing fees, and comply with packaging, marketing, and distribution restrictions. Compete against persistent gray-market operators who avoid these costs.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, legal_substance_industry, payer).

% Designs and administers the tax and regulatory framework, collecting revenue earmarked for public services. Sets enforcement priorities around DUI prevention, youth access, and tax compliance. Could alter tax rates or regulatory intensity but is constrained by political economy of the legal industry and voter expectations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_authority, agenda_setter,
    institutional, generational, analytical, national).

% Bear uncompensated costs of substance-related externalities: DUI collisions, secondhand inhalation exposure, neighborhood disorder near dispensaries. Have no contractual relationship to the substance market and limited ability to opt out of public-space exposure. Rarely compensated by tax revenue streams.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_harm_victims, payer,
    powerless, immediate, trapped, local).

% Advocate for criminalization or re-criminalization of substance use. Were central to the previous policy regime but are now largely excluded from regulatory design tables. Their moral-frame objections are treated as outside the Overton window of the legalization consensus.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, prohibitionist_advocates, excluded,
    organized, generational, constrained, national).

% Previously served the demand under prohibition; now partially displaced by legal market but persist in tax-arbitrage gray zones. Excluded from legitimacy and subject to ongoing enforcement, but continue to capture price-sensitive consumers.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    moderate, immediate, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces prohibition-era black markets with a legal, regulated supply chain that provides product safety standards, transparent transactions, and tax-funded public services while respecting adult use as a liberty interest.
% TRANSFER_FUNCTION: Moves excise tax revenue from substance consumers and licensed industry to the state; moves externality costs (DUI risk, secondhand exposure, neighborhood impacts) from users to third parties; moves market share and legitimacy from black-market operators to licensed firms.
% ABSENT_VOICES: Prohibitionist advocates who frame all substance use as moral failure are politically marginalized from the regulatory design process; black-market operators are criminalized and excluded from policy tables.
% DISAPPEARANCE_RATIONALE: If the legalization framework vanished overnight, licensed supply chains would collapse, tax revenue would disappear, the black market would rapidly reconstitute to serve demand, users would lose product safety assurances, and third-party harm prevention mechanisms such as DUI enforcement and youth-access bans would cease.
% FOUNDING_PROBLEM: Prohibition produced black-market violence, unsafe adulterated products, mass incarceration of users, and failed to reduce demand while imposing enormous fiscal and social costs.
% FOUNDING_PROBLEM_CORROBORATION: Criminal justice reform scholars and public health economists outside the legal industry corroborate the prohibition failure; tax authorities and industry lobbyists self-assert the benefit. Prohibitionist advocates dispute the founding problem framing entirely.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high: the tax burden on industry and consumers is substantial, but the more significant extraction is the uncompensated externality cost imposed on third parties who cannot opt out. Suppression (0.48) reflects active enforcement of DUI laws, tax compliance, and licensing without the totalizing carceral intensity of prohibition. Theater ratio (0.28) is low-moderate: most enforcement is functional, though some regulatory activity performs virtue without measurably reducing harm. Accessibility collapse (0.60) captures the partial suppression of the black market and the channeling of consumers into the taxed legal regime. Resistance (0.42) reflects ongoing prohibitionist mobilization, NIMBY opposition to dispensaries, and industry lobbying against tax increases. Temporal measurements track the maturation of the regulatory state: extraction rises as taxes and compliance harden, suppression requirement grows as enforcement infrastructure professionalizes, and theater remains modest.
 *
 * PERSPECTIVAL GAP:
 *   From the user and industry seats, the constraint is a liberation from prohibition and a provider of order and safety. From the third-party victim seat, the same constraint is a state-licensed market that socializes risks onto bystanders while collecting revenue. The agenda-setter seat experiences it as a successful policy reform; the powerless victim seat experiences it as an arrangement that trades their safety for fiscal and liberty gains elsewhere. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users and the legal industry sit near the beneficiary end: they gain liberty, safety, and legitimacy from the constraint's operation. The state tax authority is a concentrated beneficiary of revenue. Third-party harm victims sit at the full-target end: they bear costs (DUI risk, secondhand exposure) that the constraint formally aims to prevent but does not fully internalize. The directionality derivation from beneficiary/victim declarations plus exit options places users and industry at low d, the state at low d, and third-party victims at high d. No override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading avoids mislabeling because it acknowledges the genuine coordination benefit (safe supply, tax revenue, reduced incarceration) while not treating that benefit as disproof of extraction. The third-party victim set is structurally necessary to the reading: if the state successfully prevented all third-party harm, the constraint would approach a purer coordination mechanism (rope). The persistence of uncompensated victims is what keeps it tangled. Conversely, if one ignored the liberty and safety benefits and focused only on taxation, one would mislabel it as a snare. The metrics and the claimed type are authored independently to preserve this diagnostic tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_compensation_gap,
    'Do excise taxes on legal substances fully compensate third-party harm victims for the externalities they bear, or do victims suffer net uncompensated costs?',
    'Comparative actuarial analysis of tax revenue allocated to harm remediation versus quantified third-party damages (DUI injuries, property loss, health costs from secondhand exposure).',
    'If victims are net uncompensated, the constraint''s extraction on the powerless third-party seat is higher than the tax-transfer framing suggests, strengthening the tangled_rope classification. If fully compensated, the externality cost is closer to a coordination-priced risk pool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_compensation_gap, empirical, 'Whether tax revenue closes the externality compensation gap for third-party victims.').

omega_variable(
    black_market_displacement,
    'Does the legal framework successfully displace the black market, or does over-taxation and over-regulation sustain a persistent gray market that generates equivalent externalities without safety standards?',
    'Cross-jurisdictional comparison of legal-market price differentials versus black-market seizure/volume data; measurement of gray-market product safety outcomes.',
    'If the gray market persists at scale, the coordination function is partially undermined and the constraint extracts from licensed industry through compliance burden while failing to deliver the promised safety and order benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_displacement, empirical, 'Whether legalization displaces or sustains a tax-arbitrage gray market.').

omega_variable(
    kernel_sibling_foreclosure,
    'This constraint is the legalization reading of the substance_control_kernel. Does its core premise of individual liberty logically foreclose the prohibition reading, or can both coexist in a single policy framework?',
    'Jurisdictional case study: identify any framework that simultaneously maintains a legal adult-use market and criminalizes all use as moral transgression. Absence of such cases supports foreclosure.',
    'If foreclosure holds, the engine should treat prohibition as structurally displaced by legalization in any single framework, not as a live alternative within the same seat. This affects contamination-network propagation across the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, conceptual, 'Structural foreclosure relationship between legalization and prohibition readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_legalization_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(substance_legalization_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(substance_legalization_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(substance_legalization_tr_t24, substance_control_kernel__legalization_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(substance_legalization_tr_t32, substance_control_kernel__legalization_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(substance_legalization_tr_t40, substance_control_kernel__legalization_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(substance_legalization_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(substance_legalization_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(substance_legalization_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(substance_legalization_be_t24, substance_control_kernel__legalization_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(substance_legalization_be_t32, substance_control_kernel__legalization_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(substance_legalization_be_t40, substance_control_kernel__legalization_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(substance_legalization_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(substance_legalization_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(substance_legalization_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(substance_legalization_su_t24, substance_control_kernel__legalization_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(substance_legalization_su_t32, substance_control_kernel__legalization_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(substance_legalization_su_t40, substance_control_kernel__legalization_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three structurally distinct readings: prohibition (criminalization), harm_reduction (medicalization), and legalization (liberty/regulation). Each reading assigns different directionalities to users, constructs different beneficiary/victim sets, and produces different epsilon values. They form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
