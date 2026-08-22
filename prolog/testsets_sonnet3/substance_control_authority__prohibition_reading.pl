% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminalization of Drug Use/Possession (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition reading of the contested
 *   substance-control-authority kernel: the state's authority to criminalize
 *   drug use and possession, justified as protecting non-using third parties
 *   from drug-related crime and social disorder. The reading takes the
 *   standing criminalization arrangement as its referent, assessed by its own
 *   advocates' framing (public-order protection) against the metrics as they
 *   actually operate (concentrated, racially disparate incarceration with
 *   mounting enforcement infrastructure). Sibling readings —
 *   harm_reduction_reading and legalization_reading — are separate
 *   constraints with their own ε values and stakeholder structures; this
 *   story does not average or hedge across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.71).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.86).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminalization of Drug Use/Possession (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '9a3ec87b-2135-47f2-8b98-1de54ee1d24e').
narrative_ontology:cs_kernel_codification('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', distributed).
narrative_ontology:cs_authority_grounding('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', distributed).
narrative_ontology:cs_reading_relation('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', foundational, third_party_harm_justifies_criminalization).
narrative_ontology:cs_axiom_status(third_party_harm_justifies_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', third_party_harm_justifies_criminalization, instrumental).
narrative_ontology:cs_axiom('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', secondary, deterrence_proportionate_to_incarceration_scale).
narrative_ontology:cs_axiom_status(deterrence_proportionate_to_incarceration_scale, holdable).
narrative_ontology:cs_axiom_grounding('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', deterrence_proportionate_to_incarceration_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', police_power_public_order_doctrine).
narrative_ontology:cs_drift_state('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', post_mass_incarceration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a3ec87b-2135-47f2-8b98-1de54ee1d24e', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, carceral_state_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, prison_industry_contractors).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, residents_of_low_crime_neighborhoods).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, black_and_latino_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_defendants).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated_users).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, state_police_power_over_public_order).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_theory_of_criminal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce possession and use statutes through stops, searches, and arrests; receive budget, personnel, and asset-forfeiture resources tied to drug enforcement caseloads. Frame enforcement as protecting neighborhoods from crime and disorder associated with drug markets. Have institutional incentive to maintain the arrest pipeline that justifies their budget lines.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Courts, prosecutors, probation systems, and correctional institutions process the steady caseload that drug criminalization generates. This caseload volume justifies staffing levels, court budgets, and correctional infrastructure independent of whether incarceration reduces drug-related third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, carceral_state_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Private prison operators, phone-service contractors, and correctional supply vendors collect revenue proportional to incarcerated population. Drug offenses are a major, stable input to that population, giving these firms a direct financial stake in maintaining criminalization and high sentencing severity.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, prison_industry_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Face arrest, prosecution, and incarceration or supervision for possession/use regardless of whether their use harms third parties. Criminal records foreclose housing, employment, and voting for years afterward. Addiction and dependency mean exit from the conduct itself is often not a free choice, and the legal system offers no exit from consequences once charged.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Bear enforcement at rates substantially disproportionate to usage rates measured across racial groups, concentrated by policing patterns rather than by underlying behavior. Multi-generational effects follow from mass incarceration: family separation, wealth destruction, and community disinvestment that compound over decades.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, black_and_latino_communities, payer,
    powerless, generational, trapped, national).

% Cannot afford private counsel or bail, so plead guilty to possession charges under pressure regardless of the strength of evidence, accepting records and supervision terms that wealthier defendants avoid through better representation or diversion programs they can afford to access.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_defendants, payer,
    powerless, biographical, trapped, national).

% Absorb the economic and caregiving costs of a parent or breadwinner's incarceration for possession offenses — lost income, disrupted childcare, and the social stigma of a criminal record in the household — without having engaged in the conduct being punished.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, generational, trapped, national).

% Experience reduced visible drug-market activity and associated disorder (loitering, violence between dealers, property crime linked to acquisitive drug use) where enforcement is concentrated elsewhere, receiving a genuine localized coordination benefit from displacement of drug markets away from their communities.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, residents_of_low_crime_neighborhoods, beneficiary,
    moderate, biographical, mobile, local).

% Argue criminalization drives use underground, increases overdose risk by preventing safe-supply and testing access, and diverts public health funding into carceral infrastructure. Present at legislative hearings but structurally outvoted by the enforcement-budget coalition; their policy proposals rarely reach floor votes.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_advocates, excluded,
    organized, generational, constrained, national).

% Study incarceration outcomes, racial disparity data, and recidivism rates for drug offenses, publishing findings that inform (but rarely determine) legislative debate over criminalization versus alternative approaches.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces visible drug-market disorder and acquisitive crime in enforcement-favored areas by removing users and dealers from circulation and deterring use through threat of arrest — a genuine, if narrow, public-order coordination function for third parties who do not use drugs.
% TRANSFER_FUNCTION: Moves years of liberty, employment prospects, housing eligibility, and family stability from people who use drugs (concentrated among Black and Latino communities and the poor) to the budgets and caseloads of police, courts, and correctional/prison-industry actors, in exchange for a public-order benefit captured by residents and businesses in enforcement-favored neighborhoods.
% ABSENT_VOICES: People with active substance use disorders are rarely represented in the legislative process shaping the statutes that criminalize them; harm reduction advocates and formerly incarcerated people testify but hold little structural power against police unions and correctional-industry lobbying.
% DISAPPEARANCE_RATIONALE: If criminalization vanished overnight, arrest and incarceration flows tied to drug offenses would collapse, court and correctional caseloads would shrink substantially, prison-industry revenue tied to drug offenders would disappear, and millions of people currently under supervision or with active warrants would be released from that specific liability — while enforcement agencies would need to justify budgets on other grounds.
% FOUNDING_PROBLEM: Rising visible drug markets and associated violent and acquisitive crime in the mid-to-late 20th century, framed as requiring a criminal-law response to protect non-using third parties and preserve public order.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement leadership and some neighborhood associations attest the public-order problem remains live and criminalization is necessary. Independent public health researchers, sentencing commissions, and multiple government-commissioned racial-disparity audits (outside the enforcement and correctional-industry beneficiary set) find that measured reductions in drug-related crime from criminalization are modest relative to incarceration's scale and that enforcement intensity tracks policing patterns more than underlying use rates.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rose over the interval (0.45 to 0.71) as sentencing enhancements, mandatory minimums, and asset forfeiture regimes layered onto the original public-order rationale, shifting the arrangement from targeted deterrence toward volume-driven caseload generation. Theater ratio rose in parallel (0.20 to 0.42) as enforcement increasingly targeted low-level possession — cases with minimal public-order impact — to sustain arrest statistics and budget justifications rather than to address violent drug-market crime. Suppression is high and rising (0.68 to 0.86) because the arrangement depends on continuous, resource-intensive enforcement (stops, searches, prosecutions) rather than voluntary compliance; accessibility_collapse (0.58) is moderate rather than near-total because diversion programs and decriminalization movements in some jurisdictions represent partially surviving alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (law enforcement, prosecutors), the arrangement reads as legitimate public-order coordination funded by necessary caseload. From the payer seats (people who use drugs, their families, disparately-policed communities), the identical structure reads as extraction: liberty and opportunity converted into institutional budget and correctional-industry revenue with weak connection to actual third-party harm reduction. The engine computes this divergence from the structural power/exit data; the claimed_type (tangled_rope) is authored to reflect the divergence itself rather than resolve it in either seat's favor.
 *
 * DIRECTIONALITY LOGIC:
 *   Enforcement and correctional institutions and the prison-industry sector are structural beneficiaries — they collect budget, revenue, and caseload from the arrangement's continued operation, placing them near the beneficiary end of directionality regardless of nominal public-order purpose. People who use drugs, and disproportionately Black and Latino communities, sit at the target end: trapped exit options (addiction, criminal record foreclosure, geographic concentration of enforcement) and generational time horizons for community-level harm. Residents of low-crime neighborhoods receive a genuine, if narrow, coordination benefit and are coded as beneficiaries with mobile exit, distinguishing them from the institutional beneficiaries who administer the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visible drug-market disorder and associated crime) is contested as still live: enforcement agencies attest it persists, while independent audits find enforcement intensity has decoupled from underlying use and crime rates, tracking instead with policing patterns and caseload targets. This is the mandatrophy signature — an arrangement whose original mandate has partially atrophied into self-perpetuating institutional momentum (budget capture, correctional-industry revenue) while its public-order justification is retained as legitimating cover. The tangled_rope classification captures both halves: authentic (if narrow) coordination benefit for enforcement-favored neighborhoods, and asymmetric extraction concentrated on people who use drugs and communities of color, sustained by active, escalating enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does criminalization actually deter drug-related crime and disorder at a level proportionate to its enforcement and incarceration costs, or has the deterrent function substantially decoupled from crime outcomes while the enforcement apparatus persists on institutional momentum?',
    'Comparative analysis of crime and disorder rates in jurisdictions that have decriminalized possession versus matched jurisdictions retaining criminalization, controlling for enforcement intensity and socioeconomic covariates.',
    'If deterrence effect is weak relative to incarceration''s scale, the coordination-function claim is substantially cover for extraction, supporting reclassification toward snare; if deterrence effect is robust, the tangled_rope classification''s coordination component is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Whether criminalization''s deterrent function justifies its enforcement scale.').

omega_variable(
    racial_disparity_mechanism,
    'Is the racial disparity in enforcement a byproduct of race-neutral policing priorities responding to reported disorder, or a structural feature of how enforcement discretion and resource allocation have been exercised historically?',
    'Audit studies comparing self-reported drug use rates by race against arrest, charge, and sentencing rates, holding offense type constant, across multiple jurisdictions and time periods.',
    'If disparity is structural rather than incidental, the victim set (black_and_latino_communities) is not a side effect but a load-bearing feature of how the constraint is actually administered, sharpening the extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_mechanism, empirical, 'Whether enforcement disparity is incidental or structural to the arrangement.').

omega_variable(
    prohibition_kernel_framing_choice,
    'Two coherent framings of state authority over substances are in contest here: (a) the police-power framing, where the state''s authority derives from its obligation to prevent third-party harm from crime/disorder (this reading''s chosen frame), and (b) the bodily-autonomy framing, where the state''s authority over what individuals ingest is itself the primary constraint needing justification, with third-party harm as a secondary consideration. This story adopts framing (a) per the assigned reading; framing (b) would treat users'' loss of bodily autonomy as the base extraction rather than the derivative harm channel.',
    'This is not empirically resolvable — it depends on which value (public order vs. individual liberty) is treated as the baseline the state must justify departure from. Legislative and constitutional doctrine differ by jurisdiction on which baseline applies.',
    'Adopting framing (b) would likely elevate the extractiveness score further, since it would treat the criminalization of personal use (independent of any third-party harm shown) as extraction in itself rather than as a proportionate means to a third-party-protective end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prohibition_kernel_framing_choice, conceptual, 'Which normative baseline (public order vs. bodily autonomy) frames the constraint''s justification burden — routed here per Rule 2 rather than folded into ε.').

omega_variable(
    enforcement_theater_share,
    'What proportion of current possession arrests target conduct with demonstrable connection to third-party crime or disorder, versus arrests generated primarily to sustain caseload, budget justification, or statistical performance metrics?',
    'Case-level review of arrest narratives and charging decisions across a representative sample of jurisdictions, coded for presence of an identifiable third-party harm nexus.',
    'A high theater share would support the rising theater_ratio measurement and reinforce that criminalization''s coordination justification has become substantially decoupled from its stated third-party-protection function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_theater_share, empirical, 'How much current enforcement volume is disconnected from the stated third-party-protection rationale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__prohibition_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__prohibition_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(subs_tr_t32, substance_control_authority__prohibition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__prohibition_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__prohibition_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(subs_be_t32, substance_control_authority__prohibition_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__prohibition_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__prohibition_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(subs_su_t32, substance_control_authority__prohibition_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_authority kernel. prohibition_reading treats criminalization as the state's core mechanism for third-party protection via deterrence and incarceration (this file, ε=0.71, tangled_rope). harm_reduction_reading treats the same kernel as authority to accept use while minimizing harm through public health means (expected lower ε, rope-leaning, different beneficiary/victim structure — public health infrastructure as beneficiary, no incarceration-driven victim set). legalization_reading treats the kernel as authority to regulate licit commerce (expected different ε again, with tax revenue and regulated industry as beneficiaries and illicit-market participants as a shrinking victim set). Each reading is ε-invariant on its own terms; they are linked here for contamination-propagation analysis, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
