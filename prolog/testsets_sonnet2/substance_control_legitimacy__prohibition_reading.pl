% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Criminalization of Substance Use as Moral-Duty-Based State Authority (Prohibition Reading)
 *   domain: public health policy / criminal justice / political economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of a contested kernel
 *   about the source and scope of state authority over substance use. Under
 *   this reading, substance use is treated as inherently harmful regardless
 *   of context or consent, and the state's moral duty to prevent harm is read
 *   as extending to criminal suppression of use and supply. This reading is
 *   authored as substantially extractive and coercively enforced: users and
 *   low-level participants in the drug economy become the constraint's
 *   structural victims via criminalization itself, carceral and forfeiture
 *   infrastructure captures the resulting institutional and financial gains,
 *   and the black market the prohibition creates generates a violence
 *   externality with no accounting inside the reading's own moral-duty
 *   justification. The sibling readings (harm_reduction_reading,
 *   legalization_reading) are NOT part of this story; they are separate
 *   constraints with their own ε and victim sets, linked here only through
 *   the kernel network.
 *
 * KEY AGENTS:
 *   - carceral_enforcement_agencies: agenda-setter and institutional beneficiary; administers and profits from continued criminalization
 *   - people_who_use_drugs: primary victim class created by the criminalization act itself
 *   - illicit_supply_networks: unintended beneficiary capturing the price premium prohibition manufactures
 *   - public_health_researchers: excluded analytical voice with contrary outcome evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.79).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Criminalization of Substance Use as Moral-Duty-Based State Authority (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public health policy / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '118f6397-b7fb-483c-a8a4-f085ae84c30f').
narrative_ontology:cs_kernel_codification('118f6397-b7fb-483c-a8a4-f085ae84c30f', distributed).
narrative_ontology:cs_authority_grounding('118f6397-b7fb-483c-a8a4-f085ae84c30f', extraction).
narrative_ontology:cs_interpretation_layer_present('118f6397-b7fb-483c-a8a4-f085ae84c30f').
narrative_ontology:cs_reading_relation('118f6397-b7fb-483c-a8a4-f085ae84c30f', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('118f6397-b7fb-483c-a8a4-f085ae84c30f', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('118f6397-b7fb-483c-a8a4-f085ae84c30f', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('118f6397-b7fb-483c-a8a4-f085ae84c30f', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('118f6397-b7fb-483c-a8a4-f085ae84c30f', foundational, state_moral_duty_requires_criminal_suppression).
narrative_ontology:cs_axiom_status(state_moral_duty_requires_criminal_suppression, holdable).
narrative_ontology:cs_axiom_grounding('118f6397-b7fb-483c-a8a4-f085ae84c30f', state_moral_duty_requires_criminal_suppression, deontological).
narrative_ontology:cs_reference_frame('118f6397-b7fb-483c-a8a4-f085ae84c30f', early_20th_century_moral_suppression_consensus).
narrative_ontology:cs_drift_state('118f6397-b7fb-483c-a8a4-f085ae84c30f', contemporary_public_health_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('118f6397-b7fb-483c-a8a4-f085ae84c30f', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, carceral_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, asset_forfeiture_beneficiary_departments).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, politicians_running_on_toughness).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, low_level_dealers).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, communities_subject_to_racialized_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, illicit_supply_networks).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_moral_duty_to_prevent_self_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set enforcement priorities, write departmental policy on which substances and neighborhoods to target, and administer the arrest-to-incarceration pipeline. Budget, staffing levels, and institutional prestige are tied to continued enforcement volume; the agency's own survival depends on the problem remaining framed as a criminal one rather than a medical one.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, carceral_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, carceral_enforcement_agencies, beneficiary).

% Contract with state and federal governments to house incarcerated people, a substantial share of whom are held on drug-possession or low-level distribution charges. Revenue scales directly with incarceration volume; lobbying activity supports maintaining criminal rather than civil or medical handling of substance use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, private_prison_operators, beneficiary,
    organized, biographical, arbitrage, national).

% Retain a share of cash, vehicles, and property seized during drug enforcement operations, often without requiring a conviction. This revenue stream funds equipment and operations directly, creating an institutional incentive to prioritize seizure-rich enforcement over public-health-oriented approaches.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, asset_forfeiture_beneficiary_departments, beneficiary,
    institutional, biographical, arbitrage, regional).

% Campaign on visible enforcement metrics (arrests, seizures, sentencing severity) as evidence of moral seriousness about harm prevention. Political capital accrues from appearing tough on substance use; softening the framing carries electoral risk regardless of public health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, politicians_running_on_toughness, beneficiary,
    powerful, biographical, mobile, national).

% Bear arrest, prosecution, incarceration, and lifelong collateral consequences (housing, employment, voting, family custody) for possession or use. Physical dependency and criminal records foreclose most conventional exits; the same act treated as a health condition under other readings is treated here as a punishable moral failure, converting them into the constraint's primary victim class.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Occupy the bottom of illicit supply chains, frequently users themselves, and absorb the harshest sentencing exposure while capturing the least economic benefit from the trade. Prohibition's price premium creates the market they operate in; enforcement targets them because they are the most visible and least resourced link.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, low_level_dealers, payer,
    powerless, biographical, trapped, national).

% Experience enforcement intensity disproportionate to rates of substance use relative to other communities, producing generational effects on family structure, wealth accumulation, and trust in state institutions. Some collective political organizing capacity exists, but enforcement discretion is exercised by agencies largely insulated from community input.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, communities_subject_to_racialized_enforcement, payer,
    moderate, generational, constrained, regional).

% Absorb the economic and caregiving costs of a member's incarceration — lost income, legal fees, disrupted childcare, housing instability — without having used any substance themselves. They have no direct standing to contest the enforcement decision that produced their situation.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, generational, trapped, national).

% Profit from the price premium and reduced competition that criminalization creates by foreclosing legal supply. Violence between networks over market share and enforcement risk is a direct externality of the prohibition structure, though this actor bears none of the reading's stated moral-duty justification.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, illicit_supply_networks, beneficiary,
    organized, biographical, arbitrage, continental).

% Produce evidence on overdose mortality, treatment efficacy, and comparative outcomes across criminalized versus decriminalized regimes. Their findings are frequently cited in academic and policy circles but carry limited weight in the legislative and enforcement decisions this reading actually authorizes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_researchers, excluded,
    moderate, generational, analytical, national).

% Adjudicate individual cases and periodically rule on sentencing proportionality or search-and-seizure practices, but operate within statutory frameworks set by the criminalization choice itself and cannot revisit the underlying moral-duty premise from the bench.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, sentencing_and_appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, carceral_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, state-enforced moral and legal standard against which substance use is judged harmful per se, coordinating law enforcement, courts, and corrections around a single uniform prohibition rather than a patchwork of local tolerance levels.
% TRANSFER_FUNCTION: Moves liberty, economic resources, and life-outcomes from people who use or distribute substances (and their families) to enforcement agencies, incarceration contractors, and politicians who convert enforcement activity into institutional budget and electoral capital; further moves untaxed profit margin to illicit supply networks who capture the price premium prohibition creates.
% ABSENT_VOICES: People who use drugs and low-level dealers are the parties most affected by charging and sentencing decisions but have no seat in setting enforcement priorities; public health researchers producing contrary outcome evidence are cited but structurally unable to alter the criminalization premise from outside the legislature.
% DISAPPEARANCE_RATIONALE: If criminalization vanished overnight, incarceration populations tied to drug offenses would collapse, forfeiture and prison-contract revenue streams would disappear, illicit-market price premiums (and much of the associated violence) would fall as supply normalized, and enforcement agencies built around this mission would need to redefine their function or shrink substantially.
% FOUNDING_PROBLEM: Early-20th-century concern that unregulated substance use produced addiction, family breakdown, and public disorder, and that the state had a moral obligation to prevent harm to individuals and communities by suppressing supply and use through criminal law.
% FOUNDING_PROBLEM_CORROBORATION: Enforcement agencies and elected officials who campaign on toughness attest the founding problem remains live and requires continued criminal enforcement. Independent public health researchers, several national commissions on drug policy, and comparative outcome data from decriminalized jurisdictions — sources outside the beneficiary set — attest that criminalization has not reduced use or harm relative to public-health alternatives and that its primary observable effect is the transfer described above, not harm prevention.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.79, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.79 at interval end) because the reading's own operation converts a health condition into a felony liability with lifelong collateral consequences, transferring liberty and economic capacity from users to enforcement, incarceration, and forfeiture beneficiaries. Suppression is authored higher still (0.88) because persistence depends on continuous active enforcement — surveillance, arrest, prosecution, incarceration — not on voluntary participation; the moral-duty framing is the justificatory surface, but the mechanism is coercive throughout. Theater ratio is moderate (0.42) and rising: a growing share of enforcement activity (high-profile seizures, mandatory-minimum sentencing theater) increasingly serves institutional and electoral signaling functions rather than measurable harm reduction, which is the Goodhart-drift signature this reading exhibits over the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Carceral agencies, private prison operators, forfeiture-beneficiary departments, and toughness-campaigning politicians sit near the full-beneficiary end: they collect budget, revenue, or political capital directly from continued enforcement volume and have institutional or electoral exit options (arbitrage/mobile) that let them shift strategy without personal cost. People who use drugs, low-level dealers, and their families sit near the full-target end: criminalization is imposed on them, exit is trapped (dependency, criminal record, poverty), and the harm the reading claims to prevent is substantially the harm the reading itself produces through incarceration and collateral consequence. Illicit supply networks occupy an unusual beneficiary position structurally created by the same prohibition that claims to suppress them — they profit from the very enforcement regime nominally targeting them, which is why the network externality (violence) is authored as part of this reading's extraction profile rather than treated as exogenous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated substance use producing addiction and disorder) is authored as contested rather than flatly dead or live: enforcement agencies and politicians attest it remains live, while independent public health evidence and national commissions — sources outside the beneficiary set — attest the mechanism no longer serves its stated function and instead sustains itself through institutional revenue capture. Declaring founding_problem_status as contested (rather than accepting the beneficiary-only attestation of 'live') prevents this reading from being mislabeled as pure functioning coordination when its own corroboration record shows a capture pattern: status=contested paired with disappearance_verdict=world_rearranges signals exactly the mismatch the R5 consumer is built to flag for downstream mandatrophy review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the prohibition reading, rather than the harm_reduction or legalization reading, the structurally correct account of what substance-control state authority IS, or is it one contestable framing among three live alternatives?',
    'Comparative outcome analysis across jurisdictions that have adopted each reading (criminalization, decriminalization-with-treatment, and regulated legalization), tracking mortality, incarceration, and public order metrics over matched time periods.',
    'If comparative evidence consistently favors non-criminalization outcomes on the reading''s own stated goal (harm prevention), the prohibition reading''s moral-duty justification is undermined even on its own terms, strengthening the case that its persistence is capture rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether prohibition is the structurally correct reading of state authority over substance use or one of three coexisting contested framings.').

omega_variable(
    inherent_harm_premise_validity,
    'Is substance use inherently harmful in a way that grounds a state moral duty to criminalize, independent of context, dosage, and legal status — or is the harm profile itself substantially produced by the criminalization regime (adulterated supply, overdose from unknown potency, incarceration trauma)?',
    'Comparative toxicological and mortality data between criminalized illicit markets and regulated/decriminalized markets for comparable substances.',
    'If harm is substantially iatrogenic to the prohibition regime rather than intrinsic to the substance, the reading''s foundational axiom (inherent harm grounding moral duty) loses empirical support, which under this schema''s engine logic bears on whether the axiom''s grounding_type claim should be treated as contested rather than settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_harm_premise_validity, empirical, 'Whether substance harm is intrinsic or substantially produced by the prohibition mechanism itself.').

omega_variable(
    carceral_beneficiary_capture_degree,
    'To what extent has the moral-duty justification become a legitimating cover for institutional revenue streams (private prisons, asset forfeiture, agency budgets) that would resist reform regardless of updated harm evidence?',
    'Track lobbying expenditure and legislative voting patterns of carceral-adjacent beneficiaries against contemporaneous harm-reduction evidence; a persistent negative correlation between quality of evidence and legislative responsiveness would indicate capture.',
    'High capture would reclassify the constraint''s persistence mechanism from moral-duty coordination toward the tangled_rope''s extraction pole, and would support piton-adjacent readings of specific enforcement subsystems (e.g., mandatory minimums) that persist mainly through inertia and institutional interest rather than active belief in efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carceral_beneficiary_capture_degree, empirical, 'Degree to which institutional revenue interests, rather than genuine harm-prevention belief, sustain the criminalization apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'substance control legitimacy' per the ε-invariance principle. Each reading (prohibition, harm_reduction, legalization) authors a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type from the same contested kernel. This reading (prohibition) is authored as tangled_rope with high extractiveness (0.79) and a criminalized victim class; the harm_reduction and legalization readings are separate files with their own metrics, linked here via affects_constraints rather than folded into this story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
