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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Prohibition Reading: Substance Use as Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition_reading of the
 *   substance_control_kernel. It treats substance use as a moral
 *   transgression that the state must punish to preserve social order,
 *   mobilizing the criminal justice system against users and generating a
 *   black-market violence externality. The enforcement apparatus becomes the
 *   primary beneficiary of the resulting carceral expansion, while substance
 *   users and affected communities bear the costs.
 *
 * KEY AGENTS:
 *   - State prohibition authority (agenda_setter): Sets and maintains the punitive statutory framework.
 *   - Law enforcement apparatus (agenda_setter/beneficiary): Enforces prohibition and captures budgets, forfeiture, and institutional growth.
 *   - Carceral system operators (beneficiary): House incarcerated users and receive occupancy-dependent revenue.
 *   - Substance users (payer): Criminalized, incarcerated, and stigmatized; denied health-oriented alternatives.
 *   - Communities impacted by black market violence (payer): Bear the externalized violence and instability of illicit markets.
 *   - Public health and harm reduction advocates (excluded): Marginalized by a framing that treats use as moral failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Prohibition Reading: Substance Use as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '82b046e2-a343-4fc3-b864-15cde765f80a').
narrative_ontology:cs_kernel_codification('82b046e2-a343-4fc3-b864-15cde765f80a', formalized).
narrative_ontology:cs_authority_grounding('82b046e2-a343-4fc3-b864-15cde765f80a', extraction).
narrative_ontology:cs_interpretation_layer_present('82b046e2-a343-4fc3-b864-15cde765f80a').
narrative_ontology:cs_reading_relation('82b046e2-a343-4fc3-b864-15cde765f80a', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_reading_relation('82b046e2-a343-4fc3-b864-15cde765f80a', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('82b046e2-a343-4fc3-b864-15cde765f80a', foundational, substance_use_as_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_as_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('82b046e2-a343-4fc3-b864-15cde765f80a', substance_use_as_moral_transgression, deontological).
narrative_ontology:cs_axiom('82b046e2-a343-4fc3-b864-15cde765f80a', foundational, punitive_state_preserves_social_order).
narrative_ontology:cs_axiom_status(punitive_state_preserves_social_order, holdable).
narrative_ontology:cs_axiom_grounding('82b046e2-a343-4fc3-b864-15cde765f80a', punitive_state_preserves_social_order, empirically_contingent).
narrative_ontology:cs_reference_frame('82b046e2-a343-4fc3-b864-15cde765f80a', moral_order_punitive_state).
narrative_ontology:cs_drift_state('82b046e2-a343-4fc3-b864-15cde765f80a', post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82b046e2-a343-4fc3-b864-15cde765f80a', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, carceral_system_operators).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, communities_impacted_by_black_market_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the statutory framework that criminalizes substance possession and use, appropriates enforcement budgets, and claims moral and police-power authority to punish pharmacological deviance in defense of social order.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, state_prohibition_authority, agenda_setter,
    institutional, generational, constrained, national).

% Executes arrests, surveillance, seizures, and interdiction; receives budget allocations, civil asset forfeiture, and institutional mission from the prohibition regime. Its organizational growth and financial flows are structurally tied to the continued criminalization of substance use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, law_enforcement_apparatus, beneficiary).

% Operates jails, prisons, and detention facilities that house substance users sentenced under prohibition statutes; receives per-capita funding and occupancy-based revenue that scales with prohibition-driven incarceration.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, carceral_system_operators, beneficiary,
    institutional, biographical, constrained, national).

% Criminalized for possession or use; subject to arrest, prosecution, incarceration, fines, and stigmatization. Under this reading they are denied harm-reduction services and framed as moral failures rather than patients or rights-holders.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, national).

% Bear the externalized costs of prohibition-induced underground markets, including territorial violence, unsafe product adulteration, property crime, and loss of social capital in neighborhoods where illicit distribution concentrates.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, communities_impacted_by_black_market_violence, payer,
    powerless, immediate, trapped, local).

% Advocate for treatment, needle exchange, and overdose-prevention policies, but are structurally excluded from policy-setting in a framework that defines substance use as moral transgression punishable by the carceral state.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, law_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to protect social order and deter pharmacological deviance by using state punishment to signal moral boundaries and suppress substance use through fear of sanction.
% TRANSFER_FUNCTION: Moves bodily autonomy, liberty, and economic resources from substance users and affected communities to law enforcement and carceral systems via fines, forfeiture, and incarceration; simultaneously transfers violence and risk to underground markets and surrounding neighborhoods.
% ABSENT_VOICES: Substance users are criminalized rather than consulted; harm reduction and public health advocates are excluded because the framing treats use as moral failure; legalization proponents are absent from the policy conversation.
% DISAPPEARANCE_RATIONALE: If the prohibition reading vanished overnight, millions of substance users would exit the criminal justice system, law enforcement budgets and forfeiture streams would contract, carceral populations would drop sharply, black-market violence would attenuate, and public health or regulatory frameworks would replace punitive control.
% FOUNDING_PROBLEM: Early-20th-century moral panic over pharmacological threats to productivity, racialized fears of substance use, and perceived loss of state authority over bodily conduct and communal norms.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and civil liberties scholars outside the enforcement apparatus attest the founding narrative was constructed through racialized moral panic; prohibitionist political coalitions and early temperance lineages attest it was a genuine response to disorder.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.85) because the regime extracts liberty, health, and economic resources from users and communities to feed enforcement and carceral systems. Suppression is very high (0.88) because the constraint persists only through active policing, incarceration, and systematic suppression of harm-reduction and legalization alternatives. Theater ratio is moderate-high (0.45) reflecting the performative drug-war spectacle (media-intensive seizures, moral-panic campaigns) that supplements functional incarceration. Accessibility collapse is high (0.70) because once prohibition is institutionalized, regulatory and public-health alternatives become politically unthinkable. Resistance is moderate (0.60) because reform movements exist but are politically marginalized by the enforcement coalition.
 *
 * PERSPECTIVAL GAP:
 *   The enforcement apparatus experiences the constraint as necessary moral maintenance and organizational survival; substance users experience it as criminalization and bodily dispossession; violence-impacted communities experience it as an externally imposed security crisis generated by the black market the prohibition creates.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and carceral systems are structural beneficiaries (low directionality, subsidized by the constraint). Substance users and violence-impacted communities are structural targets (high directionality, amplified effective extraction). The state prohibition authority sits as agenda-setter with mixed directionality: it gains political authority from the kernel but does not capture rents as directly as enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition reading claims to solve a founding problem of moral disorder, but the constraint has accumulated extraction (growing carceral populations, enforcement budgets, and black-market violence) long after the original moral panic. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals that the arrangement may have outlived its claimed mandate, yet it is not a piton because the enforcement apparatus actively benefits and maintains it with high theater and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the prohibition_reading of the substance_control_kernel. How would classification change if the harm_reduction_reading or legalization_reading were adopted instead?',
    'Cross-reference sibling constraint stories in the same kernel family; compare victim/beneficiary sets, directionality derivations, and epsilon values.',
    'In the prohibition reading, substance_users are victims and law_enforcement_apparatus are beneficiaries; in sibling readings, these roles invert or dissolve entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Reading-indexed classification within the substance control kernel family').

omega_variable(
    prohibition_efficacy,
    'Does punitive prohibition of substance use reduce consumption and protect social order relative to regulatory or public health alternatives?',
    'Comparative cross-jurisdictional policy analysis and criminological meta-analysis of prohibition effects on use prevalence and social harm.',
    'If prohibition is empirically ineffective, the coordination story collapses and the constraint reads as pure extraction (snare intensification); if effective, the tangled_rope gate could open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_efficacy, empirical, 'Empirical test of the prohibition reading''s core causal claim').

omega_variable(
    enforcement_rent_vs_cost,
    'Do budget flows and civil asset forfeiture to law enforcement represent captured rent or necessary coordination overhead?',
    'Forensic accounting of prohibition enforcement budgets and forfeiture flows; comparison with regulatory enforcement costs in legalized jurisdictions.',
    'If rent, directionality for enforcement shifts toward beneficiary extraction and snare classification strengthens; if necessary cost, effective extraction is damped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_rent_vs_cost, empirical, 'Whether enforcement gains are extracted rent or necessary overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sub_proh_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sub_proh_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(sub_proh_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(sub_proh_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(sub_proh_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(sub_proh_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(sub_proh_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sub_proh_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sub_proh_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(sub_proh_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(sub_proh_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(sub_proh_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sub_proh_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sub_proh_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(sub_proh_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(sub_proh_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(sub_proh_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(sub_proh_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the substance_control_kernel family, decomposed from the colloquial label 'substance control' into three structurally distinct readings with different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
