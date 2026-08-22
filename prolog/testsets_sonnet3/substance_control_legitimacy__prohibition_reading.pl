% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Criminalization of Substance Use as State Moral Duty (Prohibition Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story authors the prohibition reading of the
 *   substance_control_legitimacy kernel: the claim that substance use is
 *   inherently harmful and that state authority to criminalize derives from a
 *   moral duty to prevent that harm. Under this reading, the standing
 *   arrangement is mass criminalization enforced through police, prosecutors,
 *   and correctional infrastructure. Users and low-income offenders enter the
 *   victim set directly through the enforcement mechanism itself (arrest,
 *   prosecution, incarceration), not merely through substance-related harm.
 *   The ε referent is the criminalization regime as the prohibition reading
 *   itself endorses it — not the harm-reduction or legalization alternatives,
 *   which are separate constraints (see kernel_context and network fields).
 *
 * KEY AGENTS:
 *   - substance_users: primary target of criminalization, trapped exit, powerless
 *   - carceral_industry_contractors: concentrated financial beneficiary of incarceration volume
 *   - law_enforcement_agencies: agenda-setter and secondary beneficiary via budget/asset-forfeiture incentives
 *   - organized_trafficking_networks: beneficiary of the price premium and reduced competition criminalization creates
 *   - public_health_researchers: analytical observer generating the comparative evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Criminalization of Substance Use as State Moral Duty (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '1ed6bc21-ad5d-4c11-b629-2bf817024b88').
narrative_ontology:cs_kernel_codification('1ed6bc21-ad5d-4c11-b629-2bf817024b88', distributed).
narrative_ontology:cs_authority_grounding('1ed6bc21-ad5d-4c11-b629-2bf817024b88', extraction).
narrative_ontology:cs_interpretation_layer_present('1ed6bc21-ad5d-4c11-b629-2bf817024b88').
narrative_ontology:cs_reading_relation('1ed6bc21-ad5d-4c11-b629-2bf817024b88', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ed6bc21-ad5d-4c11-b629-2bf817024b88', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('1ed6bc21-ad5d-4c11-b629-2bf817024b88', foundational, substance_use_inherently_harmful_regardless_of_context).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful_regardless_of_context, holdable).
narrative_ontology:cs_axiom_grounding('1ed6bc21-ad5d-4c11-b629-2bf817024b88', substance_use_inherently_harmful_regardless_of_context, empirically_contingent).
narrative_ontology:cs_axiom('1ed6bc21-ad5d-4c11-b629-2bf817024b88', foundational, state_moral_duty_justifies_coercive_self_harm_prevention).
narrative_ontology:cs_axiom_status(state_moral_duty_justifies_coercive_self_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('1ed6bc21-ad5d-4c11-b629-2bf817024b88', state_moral_duty_justifies_coercive_self_harm_prevention, deontological).
narrative_ontology:cs_axiom('1ed6bc21-ad5d-4c11-b629-2bf817024b88', secondary, criminalization_is_necessary_deterrence_mechanism).
narrative_ontology:cs_axiom_status(criminalization_is_necessary_deterrence_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1ed6bc21-ad5d-4c11-b629-2bf817024b88', criminalization_is_necessary_deterrence_mechanism, instrumental).
narrative_ontology:cs_reference_frame('1ed6bc21-ad5d-4c11-b629-2bf817024b88', moral_harm_prevention_mandate).
narrative_ontology:cs_drift_state('1ed6bc21-ad5d-4c11-b629-2bf817024b88', contemporary_policy_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ed6bc21-ad5d-4c11-b629-2bf817024b88', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, carceral_industry_contractors).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prohibition_aligned_political_actors).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, organized_trafficking_networks).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, low_income_drug_offenders).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, incarcerated_nonviolent_offenders).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_moral_duty_to_prevent_self_harm).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, criminalization_as_deterrence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess, use, or are dependent on controlled substances and are subject to arrest, prosecution, and incarceration regardless of whether their use harms anyone else. Exit from the constraint means either abstaining entirely (often blocked by dependency) or accepting permanent criminal-record consequences that follow into housing, employment, and voting rights.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Bear the enforcement weight disproportionately: cannot afford private counsel or bail, plead to charges wealthier defendants would fight, and accumulate records that compound poverty. Have essentially no capacity to alter the arrangement that processes them.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, low_income_drug_offenders, payer,
    powerless, biographical, trapped, national).

% Neighborhoods subject to concentrated policing, stop-and-search practices, and mass arrest sweeps justified by the same moral-harm rationale. Bear destabilized family structures, distrust of institutions, and reduced economic investment as downstream costs; some organize politically but cannot unilaterally exit the jurisdiction's enforcement regime.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement, payer,
    moderate, generational, constrained, regional).

% Physically confined for possession or low-level distribution offenses under sentencing structures built on the moral-harm rationale. No exit until sentence completion; forfeit labor market position, custody rights, and civic standing during and after confinement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, incarcerated_nonviolent_offenders, payer,
    powerless, biographical, trapped, national).

% Operate private prisons, supply chain contracts, and prison-labor arrangements whose revenue scales directly with incarceration volume. Lobby to preserve sentencing structures and mandatory minimums tied to drug offenses; can redeploy capital elsewhere if the arrangement weakens, but currently profit from its persistence.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, carceral_industry_contractors, beneficiary,
    organized, generational, arbitrage, national).

% Administer enforcement, receive asset-forfeiture revenue and federal grant funding tied to drug arrest quotas, and shape prosecutorial priorities. Justify the arrangement through the moral-duty-to-prevent-harm framing while collecting budgetary and institutional benefits from its continuation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, beneficiary).

% Legislators, prosecutors, and advocacy organizations who derive electoral capital, funding, and institutional legitimacy from championing tough-on-substance policy. Set statutory penalty structures and resist reform that would reduce their platform's salience.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prohibition_aligned_political_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, prohibition_aligned_political_actors, agenda_setter).

% Profit from the price premium and reduced competition that criminalization creates by driving supply into unregulated channels. Absorb enforcement risk as a cost of doing business and pass it downstream as violence and market instability onto users and adjacent communities.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, organized_trafficking_networks, beneficiary,
    organized, generational, arbitrage, continental).

% Public health practitioners and reform advocates who argue the moral-harm-prevention rationale is empirically unsupported as a basis for criminal penalties. Testify at hearings and publish research but are structurally excluded from statutory drafting processes dominated by the enforcement and prosecutorial apparatus.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_and_treatment_advocates, excluded,
    moderate, biographical, constrained, national).

% Study incarceration outcomes, overdose rates, and black-market violence associated with criminalization regimes across jurisdictions, generating the comparative evidence base used by all sides of the kernel contest.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared societal mechanism for expressing collective moral disapproval of substance use and, in principle, coordinates deterrence signaling so that no single locality bears the reputational or practical cost of unilaterally tolerating use that others criminalize.
% TRANSFER_FUNCTION: Moves liberty, labor-market standing, and family stability from substance users and their communities to carceral contractors, enforcement agencies, and political actors who convert enforcement volume into budget, contract revenue, and electoral capital; simultaneously moves market rents from law-abiding commerce to organized trafficking networks that absorb the risk premium criminalization creates.
% ABSENT_VOICES: Harm reduction and treatment advocates are testimony-only participants excluded from statutory drafting; currently incarcerated individuals have no direct voice in the legislative process that sustains their confinement; communities bearing concentrated enforcement have organized political voice but rarely proportionate influence over sentencing structure.
% DISAPPEARANCE_RATIONALE: If criminalization vanished overnight, carceral contractors would lose a substantial revenue stream, law enforcement budgets tied to drug-arrest metrics would need restructuring, trafficking network profit margins would collapse as supply normalized through legal channels, and millions of people currently under supervision or incarcerated for substance offenses would be released — an enormous rearrangement of labor markets, correctional infrastructure, and municipal budgets.
% FOUNDING_PROBLEM: Early twentieth-century concern that unregulated substance markets caused visible social harm (addiction, adulterated products, associated violence) with no institutional mechanism to intervene, combined with a moral commitment that the state has a duty to prevent citizens from harming themselves.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement agencies and prohibition-aligned political actors attest the founding problem remains live, citing overdose deaths and trafficking violence as ongoing harm. Independent public health researchers and multiple national drug policy commissions (bodies outside the beneficiary set) have published findings that criminalization itself is a primary driver of the overdose and violence outcomes cited to justify it, and that treatment-based approaches in comparator jurisdictions produce better health outcomes at lower carceral cost — suggesting the founding problem, as originally framed, has been substantially supplanted by the enforcement apparatus's own effects.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.45 -> 0.78) reflecting the documented escalation of mandatory minimums, asset forfeiture practice, and incarceration rates from the mid-20th century policy expansion onward. Suppression is very high (0.87) because the arrangement depends on continuous active enforcement — police powers, prosecutorial discretion, and correctional custody — not participant consent; there is no meaningful opt-out for a user once identified. Theater ratio is moderate-rising (0.42) because a growing share of enforcement activity (symbolic sentencing enhancements, media-visible raids) functions as political signaling rather than harm prevention, while resistance (0.75) reflects sustained organized pushback from public health, civil rights, and reform coalitions. Accessibility collapse is comparatively low (0.4) because, unlike a mountain, credible policy alternatives (decriminalization, treatment-based models) are visibly implemented elsewhere and documented — the alternatives have not collapsed, they have been suppressed within this jurisdiction's framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users, low-income offenders, and incarcerated individuals sit at the high-target end of directionality: they are named victims with trapped exit and powerless standing, and the constraint's enforcement mechanism operates directly on their bodies and liberty. Carceral contractors, enforcement agencies, and prohibition-aligned political actors sit near the beneficiary end: they collect budgetary, financial, or electoral rents from enforcement volume and have arbitrage-grade exit (they can redeploy capital or platform elsewhere if the arrangement weakens, unlike the people it processes). Organized trafficking networks are a structurally paradoxical beneficiary: they profit from the very prohibition that also targets a subset of their operations, because enforcement risk becomes a barrier to entry that protects their market position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated substance markets causing visible harm with no institutional response) is contested as still-live: enforcement-side actors point to overdose deaths as ongoing crisis justifying continuation, while independent researchers and national drug policy commissions attest the enforcement apparatus itself now drives much of the cited harm (overdose deaths concentrated in criminalized, unregulated supply; incarceration destabilizing the same communities framed as needing protection). This is the mandatrophy signature: a mandate whose original justification has been substantially undercut by its own operation, sustained now primarily by the institutional and financial interests that have accreted around it rather than by its original coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_duty_versus_institutional_capture,
    'Is the state''s continued reliance on criminalization genuinely driven by an unresolved moral-harm-prevention duty, or has that duty become a legitimating narrative for an enforcement-and-incarceration apparatus whose primary function has shifted to revenue and political capital generation?',
    'Comparative policy analysis: track sentencing severity and enforcement intensity against independently measured harm reduction outcomes across jurisdictions that have decriminalized versus those that have intensified enforcement; if enforcement-intensifying jurisdictions show worse harm outcomes over time, the moral-duty justification is empirically undercut.',
    'If the moral duty framing is substantially decoupled from harm outcomes, the prohibition reading''s coordination claim collapses toward pure extraction (snare) rather than the tangled_rope claimed here, since the coordination function (harm prevention) would no longer be genuinely served.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_duty_versus_institutional_capture, empirical, 'Whether the moral-duty justification tracks actual harm-prevention outcomes or has decoupled into pure legitimation.').

omega_variable(
    kernel_framing_choice,
    'Is the prohibition reading''s premise — that substance use is inherently and uniformly harmful regardless of context — a defensible unitary framing, or does it itself conflate structurally distinct claims (harm from substance pharmacology versus harm from the criminalization response) that would, under stricter decomposition, split into further separate constraints?',
    'Compare outcomes attributable to substance pharmacology alone (measured in decriminalized/regulated jurisdictions) against outcomes attributable to enforcement mechanisms (arrest, incarceration, black-market violence) to determine whether ''inherent harm'' as authored here is doing analytical work independent of the enforcement apparatus it justifies.',
    'If pharmacological harm and enforcement-caused harm are shown to be largely separable and enforcement-caused harm dominates the measured extractiveness, this would support treating criminalization-caused harm as its own constraint rather than folding it into a single ''inherent harm'' premise — a further decomposition beyond the three-reading kernel split already performed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether ''inherent harm'' as framed by this reading conflates pharmacological and enforcement-caused harm.').

omega_variable(
    black_market_externality_scope,
    'How much of the violence and instability associated with substance markets is caused by prohibition-driven market structure versus factors that would persist under any regulatory regime?',
    'Cross-jurisdictional and historical comparison (e.g., alcohol prohibition and repeal, cannabis legalization state-by-state rollouts) isolating violence rates attributable to enforcement-created market conditions versus baseline substance-related conflict.',
    'A high attribution to prohibition-driven structure would strengthen the claim that organized trafficking networks are a direct beneficiary of this specific constraint rather than an independent phenomenon, reinforcing the tangled_rope classification''s asymmetric-extraction gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_externality_scope, empirical, 'How much market violence is attributable to the prohibition structure itself versus independent factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the substance_control_legitimacy kernel. All three share the same underlying phenomenon (state authority over substance use) but author structurally distinct ε values, beneficiary/victim sets, and classifications because each reading endorses a different standing arrangement as the referent for its own ε: prohibition_reading measures the criminalization regime (high ε, tangled_rope, users as direct victims of enforcement); harm_reduction_reading measures a public-health-oriented non-carceral regime (lower ε, likely rope or scaffold); legalization_reading measures a regulated-market regime bounded by third-party-harm prevention (lowest ε among the three, likely rope). Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
