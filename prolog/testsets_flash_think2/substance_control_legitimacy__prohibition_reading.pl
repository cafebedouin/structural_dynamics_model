% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Substance Prohibition as Moral Duty
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the 'prohibition_reading' of the
 *   'substance_control_legitimacy' kernel. It posits that substance use is
 *   inherently harmful and that the state has a moral duty to prevent this
 *   harm through criminalization. This reading grounds state authority in a
 *   moral imperative, leading to policies of strict prohibition and active
 *   enforcement via the criminal justice system. The core contest with
 *   sibling readings (harm_reduction_reading, legalization_reading) lies in
 *   the nature of harm, the scope of state authority, and individual
 *   autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Prohibition as Moral Duty").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, 'c65132d4-a7f4-48b0-8df4-86cdbedae5cd').
narrative_ontology:cs_kernel_codification('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', formalized).
narrative_ontology:cs_authority_grounding('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', extraction).
narrative_ontology:cs_interpretation_layer_present('c65132d4-a7f4-48b0-8df4-86cdbedae5cd').
narrative_ontology:cs_reading_relation('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', foundational, substance_use_inherently_immoral).
narrative_ontology:cs_axiom_status(substance_use_inherently_immoral, holdable).
narrative_ontology:cs_axiom_grounding('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', substance_use_inherently_immoral, deontological).
narrative_ontology:cs_axiom('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', foundational, state_has_moral_duty_to_prevent_self_harm).
narrative_ontology:cs_axiom_status(state_has_moral_duty_to_prevent_self_harm, holdable).
narrative_ontology:cs_axiom_grounding('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', state_has_moral_duty_to_prevent_self_harm, deontological).
narrative_ontology:cs_reference_frame('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', moral_purity_public_order).
narrative_ontology:cs_drift_state('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', contemporary_evidence_based_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c65132d4-a7f4-48b0-8df4-86cdbedae5cd', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, black_market_actors).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, public_health_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces prohibition laws through arrests, prosecutions, and incarceration. Benefits from increased budgets, asset forfeiture, and political capital derived from 'tough on crime' stances. Justifies its actions as upholding public morality and safety.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of criminalization: arrests, fines, incarceration, loss of employment, housing, and social stigma. Their health and safety are often compromised by unregulated black markets and fear of seeking medical help.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by prohibition enforcement, leading to higher rates of arrest, incarceration, and family separation. The social fabric of these communities is eroded, and trust in state institutions is severely damaged. Exit is identity-locked due to systemic racism and poverty.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Profit immensely from the illegality of substances, which creates artificial scarcity and high prices. They operate outside legal frameworks, often using violence to enforce contracts and maintain control, creating a parallel economy that thrives on prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_actors, beneficiary,
    organized, biographical, arbitrage, global).

% Advocate for public health approaches to substance use, emphasizing treatment, prevention, and harm reduction over criminalization. Their voices are often marginalized in policy debates dominated by carceral logic, and their proposed alternatives are suppressed.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% Argue for individual autonomy and the right of competent adults to make choices about their own bodies, including substance use, provided it doesn't harm others. They are excluded from the prohibition framework which views substance use as inherently immoral and subject to state control.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% Bear the financial costs of prohibition enforcement, including police, courts, prisons, and the social costs of mass incarceration. While some support the moral stance, many question the efficacy and economic burden of the policy.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate public safety and moral order by deterring substance use and preventing associated social harms through criminal penalties.
% TRANSFER_FUNCTION: Transfers freedom, wealth (fines, asset forfeiture), and social capital from substance users and marginalized communities to the state's carceral apparatus. It also transfers significant profits to black market actors due to artificial scarcity.
% ABSENT_VOICES: Public health experts, harm reduction advocates, civil liberties organizations, and medical professionals who advocate for evidence-based, non-carceral approaches to substance use are systematically excluded from the policy-making process under this framework.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the criminal justice system would undergo a massive restructuring, black markets would collapse, and public health infrastructure would need to rapidly expand to manage substance use as a health issue. Social dynamics in marginalized communities would shift dramatically.
% FOUNDING_PROBLEM: The perceived moral decay, social disorder, and health harms associated with substance use, framed as a threat to public order and individual virtue requiring state intervention.
% FOUNDING_PROBLEM_CORROBORATION: State authorities, some religious organizations, and segments of the public attest that the founding problem (moral harm, social disorder) is still live. However, public health organizations, civil liberties groups, and independent researchers widely attest that the problem is either dead, misframed, or exacerbated by the prohibition itself, citing evidence of increased violence, disease, and social inequality.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) due to the severe penalties, fines, and incarceration imposed on users, and the economic burden on communities. Suppression is extremely high (0.92) as the state actively criminalizes and punishes, while black markets use violence to maintain control, effectively trapping users between state coercion and criminal exploitation. Theater ratio is moderate (0.45) because while some enforcement genuinely aims at public safety, a significant portion is performative, maintaining the moral stance of prohibition despite evidence of its ineffectiveness in reducing overall harm and its exacerbation of social problems. Accessibility collapse is high (0.70) as legal alternatives are completely absent, forcing users into dangerous black markets. Resistance is also high (0.60) from users, advocacy groups, and the inherent instability of black markets.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and its supporters, this constraint is a necessary measure to uphold public morality and protect individuals from self-harm, framed as a 'rope' or even a 'mountain' of moral truth. From the perspective of substance users and marginalized communities, it is a 'snare' that extracts freedom, wealth, and well-being, often disproportionately, while failing to address underlying issues. The engine's computation of a 'snare' classification from the authored metrics highlights this divergence from the claimed moral imperative.
 *
 * DIRECTIONALITY LOGIC:
 *   The state carceral apparatus is a primary beneficiary, gaining resources and political power from enforcement. Paradoxically, black market actors also benefit significantly, as prohibition creates their market. Substance users and marginalized communities are clear targets, bearing the brunt of criminalization and its social costs. Public health and legalization advocates are excluded, their perspectives actively suppressed by the dominant prohibition framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare, despite the claimed moral duty, prevents mislabeling. The 'founding problem' of moral decay and social disorder is contested, with significant evidence suggesting the constraint itself generates new harms (black market violence, mass incarceration) rather than solving the original ones. The high extractiveness and suppression, coupled with the contested founding problem status, indicate that the constraint persists not purely from its stated moral mandate, but from the entrenched interests of its beneficiaries and the suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_duty_vs_empirical_harm,
    'Is substance use inherently harmful, justifying state criminalization, or is the harm largely a consequence of the prohibition framework itself (e.g., black markets, lack of regulation, social stigma)?',
    'Comparative analysis of health and social outcomes in jurisdictions with different regulatory approaches (prohibition vs. harm reduction vs. legalization).',
    'If harm is primarily a consequence of prohibition, the moral duty argument weakens, shifting the constraint''s justification from inherent harm prevention to a constructed mechanism that generates harm, supporting a higher extractiveness and snare classification. If harm is truly inherent and reduced by prohibition, the moral duty argument gains strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_duty_vs_empirical_harm, empirical, 'Whether the harm attributed to substance use is inherent or policy-induced.').

omega_variable(
    prohibition_efficacy_vs_unintended_consequences,
    'Does criminalization effectively deter substance use and reduce overall societal harm, or does it primarily create unintended consequences like black markets, violence, and public health crises?',
    'Longitudinal studies comparing rates of substance use, overdose deaths, crime, and public health indicators in pre- and post-prohibition eras, or across jurisdictions with differing policies.',
    'If prohibition is shown to be ineffective or counterproductive in achieving its stated goals, its ''coordination function'' collapses, revealing it as primarily an extractive mechanism. This would further solidify a snare classification and potentially increase the theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_efficacy_vs_unintended_consequences, empirical, 'Efficacy of prohibition in achieving its stated goals versus generating negative externalities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.92) primarily structural (legal penalties, economic barriers) or internalized (stigma, fear, self-blame among users)?',
    'Post-decriminalization/legalization studies: if suppression (e.g., reluctance to seek help, self-stigma) persists after legal penalties are removed, it indicates a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as users carry the suppression with them even if legal barriers are reduced. This makes exit harder and amplifies extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in substance prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_legitimacy__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1980, substance_control_legitimacy__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(subs_tr_t1990, substance_control_legitimacy__prohibition_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__prohibition_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(subs_tr_t2020, substance_control_legitimacy__prohibition_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1980, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(subs_be_t1990, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement(subs_be_t2020, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(subs_su_t1990, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(subs_su_t2020, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2020, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
