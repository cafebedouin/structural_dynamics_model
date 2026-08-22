% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminalization of Drug Use/Possession (Third-Party Protection Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story authors the prohibition reading of the substance control
 *   authority kernel: the state's claim to criminalize drug possession and
 *   use as a means of protecting third parties from drug-related crime and
 *   disorder. The reading treats the criminalized user as the mechanism
 *   (deterrence target) rather than the direct object of concern — the
 *   justification is public order, not the user's own welfare. Over the
 *   mid-to-late 20th century this reading hardened from a targeted
 *   public-order measure into a mass-incarceration apparatus whose
 *   enforcement intensity and extraction rose well beyond what the original
 *   disorder-suppression rationale required, while measured drug-use
 *   prevalence did not fall proportionately. This is a sibling of
 *   harm_reduction_reading and legalization_reading, which read the same
 *   underlying state authority differently — accepting use while minimizing
 *   harm, or regulating supply as legal commerce, respectively. Each reading
 *   has its own epsilon and its own victim/beneficiary structure; they are
 *   not merged here.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: primary target (powerless/trapped) — criminalized as the deterrence mechanism
 *   - low_income_minority_communities: concentrated target (powerless/trapped) — bears enforcement at disparate rates
 *   - law_enforcement_agencies: agenda-setter and institutional beneficiary (institutional/arbitrage) — administers and partly profits from enforcement scale
 *   - prosecutorial_apparatus: agenda-setter and institutional beneficiary (institutional/arbitrage) — charging discretion drives caseload-linked incentives
 *   - neighborhoods_seeking_order: genuine coordination beneficiary (organized/constrained) — the real disorder-reduction function this reading is built on
 *   - carceral_industry_contractors: incidental beneficiary (organized/arbitrage) — captures enforcement spending without playing a role in the stated rationale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.71).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminalization of Drug Use/Possession (Third-Party Protection Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'ebb3c337-23ad-4fb7-a5f1-880638dc7305').
narrative_ontology:cs_kernel_codification('ebb3c337-23ad-4fb7-a5f1-880638dc7305', distributed).
narrative_ontology:cs_authority_grounding('ebb3c337-23ad-4fb7-a5f1-880638dc7305', distributed).
narrative_ontology:cs_reading_relation('ebb3c337-23ad-4fb7-a5f1-880638dc7305', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebb3c337-23ad-4fb7-a5f1-880638dc7305', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('ebb3c337-23ad-4fb7-a5f1-880638dc7305', foundational, criminal_deterrence_protects_third_parties).
narrative_ontology:cs_axiom_status(criminal_deterrence_protects_third_parties, holdable).
narrative_ontology:cs_axiom_grounding('ebb3c337-23ad-4fb7-a5f1-880638dc7305', criminal_deterrence_protects_third_parties, empirically_contingent).
narrative_ontology:cs_axiom('ebb3c337-23ad-4fb7-a5f1-880638dc7305', foundational, user_liability_is_legitimate_deterrence_mechanism).
narrative_ontology:cs_axiom_status(user_liability_is_legitimate_deterrence_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ebb3c337-23ad-4fb7-a5f1-880638dc7305', user_liability_is_legitimate_deterrence_mechanism, instrumental).
narrative_ontology:cs_reference_frame('ebb3c337-23ad-4fb7-a5f1-880638dc7305', police_power_public_order_framework).
narrative_ontology:cs_drift_state('ebb3c337-23ad-4fb7-a5f1-880638dc7305', contemporary_mass_incarceration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ebb3c337-23ad-4fb7-a5f1-880638dc7305', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, carceral_industry_contractors).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, neighborhoods_seeking_order).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, prosecutorial_apparatus).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_minority_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, nonviolent_possession_defendants).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated_users).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, state_police_power_over_public_order).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_theory_of_criminal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face arrest, prosecution, and incarceration for possession or use, often for substance dependency that is a health condition. Criminal records that follow from conviction close off housing, employment, and licensing, compounding the original harm the law claims to prevent. Exit from the constraint requires either abstaining entirely or evading detection; neither is available to someone in active addiction without treatment access they frequently cannot obtain because of the same record the arrest created.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Bear enforcement at rates far exceeding usage-rate parity with other communities, concentrating arrests, stops, and incarceration geographically. Multi-generational effects include disrupted family structures, reduced household wealth, and diminished political voice from felony disenfranchisement. Cannot relocate out of over-policed jurisdictions without resources most residents lack.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_minority_communities, payer,
    powerless, generational, trapped, national).

% Charged and often incarcerated for possession alone, without any allegation of harm to a third party. Plea-bargaining pressure under mandatory minimums forces guilty pleas regardless of case merits. Their situation is the clearest test of whether the law is protecting third parties or punishing the user directly.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, nonviolent_possession_defendants, payer,
    powerless, biographical, trapped, national).

% Lose income, caregiving capacity, and household stability when a member is incarcerated for a possession offense. Children of incarcerated parents face measurable downstream harms the criminalization regime does not count among its costs.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, generational, trapped, national).

% Administer enforcement, set operational priorities within statutory limits, and receive budget, personnel, and asset-forfeiture revenue tied to drug enforcement activity. Their institutional survival and expansion are partly bound to the continuation of criminalized enforcement as a mission and funding stream.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary).

% Charge, plea-bargain, and sentence possession cases; caseload volume and conviction statistics that justify budgets and reelection for elected prosecutors depend substantially on drug caseloads. Discretion in charging is wide and largely unreviewed.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, prosecutorial_apparatus, beneficiary).

% Private and public prison operators, supervision-technology vendors, and drug-testing firms derive direct revenue from possession-driven incarceration and probation populations. They have no operational role in the third-party-protection rationale but capture a share of its enforcement spending.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, carceral_industry_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Residents near open drug markets experience real reductions in visible disorder, property crime, and violence when enforcement suppresses local dealing activity. This is the genuine coordination function the reading is built on; the benefit is real but concentrated and time-limited relative to the costs imposed on the criminalized population.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, neighborhoods_seeking_order, beneficiary,
    organized, biographical, constrained, local).

% Public health researchers and clinicians who would argue that criminalization drives users away from treatment and testing services, worsening overdose and disease transmission outcomes. They participate in policy debate but their evidence rarely displaces the enforcement-first framework once adopted.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_public_health_agencies, excluded,
    moderate, generational, constrained, national).

% Adjudicate challenges to sentencing disparity, search and seizure practices, and proportionality; can narrow or expand the enforcement apparatus but operate within the state's asserted police-power authority rather than questioning the reading itself.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, constitutional_and_public_health_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Suppresses visible drug markets and associated violence, property crime, and disorder in residential and commercial areas by removing dealers and deterring public use through the threat and application of criminal penalty.
% TRANSFER_FUNCTION: Moves liberty, income, and family stability from people who use or possess drugs (concentrated overwhelmingly among low-income and minority populations) to the general public's experience of order, and to the budgets and institutional scope of enforcement and carceral agencies.
% ABSENT_VOICES: People currently incarcerated for possession, and the public health/harm-reduction research community, are structurally outside the legislative and prosecutorial rooms where charging thresholds and sentencing schedules are set; their evidence on treatment efficacy and enforcement-driven harm rarely reaches the policy floor with equal standing to law-enforcement testimony.
% DISAPPEARANCE_RATIONALE: If criminal possession liability vanished overnight, police departments would lose a major share of stop, search, and arrest activity and associated budget justification; prosecutors would lose a large caseload category; incarcerated populations for possession offenses would be released; drug markets would likely shift toward the legal/regulatory arrangements described by the sibling readings. The scale of institutional and demographic rearrangement is large enough that 'world_unchanged' would misdescribe it.
% FOUNDING_PROBLEM: Early-to-mid 20th century concern that unregulated drug markets and use produced violent crime, public disorder, and social breakdown that ordinary civil and tort remedies could not address, requiring the state's police power to suppress supply and deter use directly.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and some community organizations attest the disorder problem remains live in areas with open drug markets. Independent criminological research, public health bodies (e.g., government-commissioned drug policy commissions), and formerly incarcerated advocacy organizations — outside the enforcement and carceral-industry beneficiary set — attest that decades of criminalization have not reduced use prevalence and that the arrangement now functions primarily as a distribution mechanism for punishment rather than a demonstrated disorder-reduction tool.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.71) and suppression (0.88) are both high and are authored separately: suppression reflects the raw coercive apparatus (arrest, prosecution, incarceration, asset forfeiture, supervision) required to maintain the regime, which is a structural fact independent of how effective the deterrence actually is. Extractiveness reflects how much value net flows away from the criminalized population relative to the disorder-reduction benefit delivered to third parties — this rose over the interval as sentencing severity, caseload volume, and carceral capacity expanded well past what marginal disorder reduction could justify, then plateaued as reform pressure emerged. Theater ratio (0.42) captures that a substantial share of enforcement activity (low-level possession sweeps, public consumption citations) functions more as visible activity than as a demonstrated disorder-reduction mechanism — the genuine third-party protection function (removing violent market activity) is real but is a minority share of aggregate enforcement volume.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (law enforcement, prosecutors), the arrangement is a functioning coordination mechanism suppressing real disorder and violence. From the payer seats (users, disparately-policed communities, nonviolent defendants), the same structure operates as a punitive extraction regime whose costs vastly exceed the local order gained, especially where the underlying conduct involves no third-party harm at all (simple possession). The engine's per-seat computation should reflect this divergence directly from the declared power/exit asymmetry, not from any story-level adjudication of who is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs and low-income/minority communities are the clearest targets: no exit exists once criminalized status attaches, and enforcement falls disproportionately by geography and race independent of usage-rate parity, which is the racial-disparity delta this reading structurally carries. Law enforcement and prosecutorial agencies sit as institutional agenda-setters with a secondary beneficiary interest — budget, headcount, and caseload-linked incentives are real even though their formal mandate is third-party protection, not revenue capture. Neighborhoods seeking order are genuine, if partial, beneficiaries: the coordination story is not pure cover, since visible market suppression is a real and locally valued outcome — this is what keeps the reading from being a pure snare and pushes it toward tangled_rope. Carceral industry contractors are incidental beneficiaries with no role in the stated rationale, which is a marker of extraction riding on the coordination function rather than constituting it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (violent, disorderly open drug markets) remains partially live in some localities, which prevents a clean mandatrophy verdict — hence 'contested' rather than 'dead.' But the enforcement apparatus has grown to address possession broadly, including conduct with no third-party harm nexus, well past what the disorder-suppression rationale supports. The tangled_rope classification is deliberately chosen over snare because a real, corroborated coordination benefit exists for a specific beneficiary (neighborhoods experiencing market suppression) alongside asymmetric extraction from a clearly named victim class — both gates required by the schema are met by the underlying facts, not tuned to produce the label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_kernel_reading_disambiguation,
    'Is criminalization the reading of state substance-control authority under evaluation here, as distinct from the harm-reduction and legalization readings of the same underlying kernel?',
    'Structural: this story is scoped explicitly to the prohibition_reading constraint_id; sibling readings are separate stories linked via network.affects_constraints, each with its own epsilon and stakeholder structure.',
    'Conflating readings would produce an incoherent averaged epsilon across mechanisms with very different victim sets and extraction profiles; keeping them separate preserves epsilon-invariance per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_kernel_reading_disambiguation, conceptual, 'This story is one of three sibling readings of the substance_control_authority kernel; disambiguation is structural, not empirical.').

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does criminalization actually deter drug-related crime and disorder at a rate proportionate to the enforcement costs and carceral harms it imposes, or has the deterrence effect been substantially exhausted while enforcement scale continued to grow?',
    'Comparative analysis of use-prevalence and disorder/crime trends across criminalization-intensity variation (cross-state, cross-national, decriminalization natural experiments), controlling for socioeconomic confounds.',
    'If deterrence efficacy is low relative to enforcement scale, the coordination function claimed for this reading is substantially exhausted and the classification should trend toward snare; if efficacy is robust, the tangled_rope classification''s coordination leg is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Whether the deterrence mechanism this reading depends on is still functioning at the scale enforcement has reached.').

omega_variable(
    racial_disparity_mechanism_ambiguity,
    'Is the racially disparate application of possession enforcement a contingent implementation failure correctable within the prohibition reading, or a structural feature of how criminalized drug enforcement is necessarily administered given resource-allocation and policing-pattern incentives?',
    'Track disparity trends across jurisdictions that have implemented explicit bias-reduction and resource-reallocation reforms within a still-criminalized framework; persistence of disparity despite reform effort would support the structural reading.',
    'If structural, the victim concentration on low_income_minority_communities is not a fixable defect of this reading but an inherent feature, strengthening the tangled_rope/asymmetric-extraction characterization; if contingent, reform could substantially lower measured extraction without changing the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_mechanism_ambiguity, empirical, 'Whether racial disparity in enforcement is fixable within prohibition or is structurally inherent to it.').

omega_variable(
    carceral_capture_of_enforcement_scale,
    'To what extent has the scale of possession-driven enforcement and incarceration been driven by carceral-industry and institutional-budget incentives independent of the underlying disorder-suppression need?',
    'Analysis of enforcement intensity correlation with private-prison contracts, asset-forfeiture revenue dependency, and departmental budget cycles versus local crime/disorder metrics.',
    'Strong correlation would support classifying a larger share of current extraction as rent-seeking by incidental beneficiaries riding the coordination function, rather than protection-function cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carceral_capture_of_enforcement_scale, empirical, 'Whether enforcement scale is driven by genuine disorder-suppression need or by incidental institutional/financial incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(subs_tr_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(subs_be_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(subs_su_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_authority kernel. harm_reduction_reading authors a different beneficiary/victim structure (users as recipients of public health services rather than as criminalized targets) and a substantially lower epsilon for the corresponding intervention mechanism. legalization_reading authors state authority as market regulation, with victims limited to those harmed by residual illicit-market activity rather than by criminalization itself. All three share the underlying kernel — that the state possesses some legitimate authority over drug use/markets — but diverge sharply in mechanism, epsilon, and who bears the cost. This reading (prohibition) is the one with by far the highest measured extraction and suppression of the three because incarceration is its enforcement mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
