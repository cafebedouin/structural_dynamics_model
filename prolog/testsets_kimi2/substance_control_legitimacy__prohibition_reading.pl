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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibition Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint is the prohibition reading of the
 *   substance_control_legitimacy kernel, instantiated as a standing policy
 *   arrangement in which state authority is derived from a moral duty to
 *   prevent the inherent harm of substance use through criminalization. The
 *   regime operates through active carceral enforcement, producing
 *   identifiable victim populations among criminalized users and communities
 *   exposed to black-market violence. Sibling
 *   readingsâharm_reduction_reading and legalization_readingâcontest the
 *   same kernel but instantiate structurally distinct constraints with
 *   different beneficiary/victim profiles. The authored metrics describe a
 *   heavily extractive, actively enforced arrangement whose claimed
 *   harm-prevention function operates as cover for carceral and political
 *   extraction.
 *
 * KEY AGENTS:
 *   - criminalized_users: Primary target (powerless/trapped) â bear direct carceral extraction through incarceration, fines, and civil disability.
 *   - black_market_communities: Secondary target (powerless/constrained) â bear externalized violence and insecurity from illicit markets created by prohibition.
 *   - drug_enforcement_bureaucracy: Primary beneficiary/agenda-setter (institutional/constrained) â collects budget, mandate, and employment through enforcement.
 *   - prohibitionist_political_coalition: Political beneficiary (powerful/mobile) â derives moral authority and electoral support from the criminalization frame.
 *   - harm_reduction_advocates: Excluded voice (moderate/constrained) â would advance health-based alternatives but are marginalized from policy design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibition Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '56508df4-b13f-4e4f-b449-91dc83e76092').
narrative_ontology:cs_kernel_codification('56508df4-b13f-4e4f-b449-91dc83e76092', formalized).
narrative_ontology:cs_authority_grounding('56508df4-b13f-4e4f-b449-91dc83e76092', extraction).
narrative_ontology:cs_interpretation_layer_present('56508df4-b13f-4e4f-b449-91dc83e76092').
narrative_ontology:cs_reading_relation('56508df4-b13f-4e4f-b449-91dc83e76092', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_reading_relation('56508df4-b13f-4e4f-b449-91dc83e76092', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('56508df4-b13f-4e4f-b449-91dc83e76092', foundational, substance_use_inherently_immoral).
narrative_ontology:cs_axiom_status(substance_use_inherently_immoral, holdable).
narrative_ontology:cs_axiom_grounding('56508df4-b13f-4e4f-b449-91dc83e76092', substance_use_inherently_immoral, deontological).
narrative_ontology:cs_axiom('56508df4-b13f-4e4f-b449-91dc83e76092', foundational, state_duty_to_criminalize_harm).
narrative_ontology:cs_axiom_status(state_duty_to_criminalize_harm, holdable).
narrative_ontology:cs_axiom_grounding('56508df4-b13f-4e4f-b449-91dc83e76092', state_duty_to_criminalize_harm, deontological).
narrative_ontology:cs_reference_frame('56508df4-b13f-4e4f-b449-91dc83e76092', prohibitionist_moral_order).
narrative_ontology:cs_drift_state('56508df4-b13f-4e4f-b449-91dc83e76092', contemporary_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('56508df4-b13f-4e4f-b449-91dc83e76092', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, drug_enforcement_bureaucracy).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prohibitionist_political_coalition).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, criminalized_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, black_market_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers criminalization through arrests, prosecution, and incarceration; receives budgetary allocations, equipment, and institutional mandate tied to the prohibition regime; resists policy reform that would reduce its scope or funding.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, drug_enforcement_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, drug_enforcement_bureaucracy, beneficiary).

% Derives political authority and electoral support from framing substance use as moral failure requiring state punishment; benefits from a policy framework that conflates moral order with state power and criminal sanction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prohibitionist_political_coalition, beneficiary,
    powerful, biographical, mobile, national).

% Subjected to arrest, incarceration, fines, and permanent criminal records for substance use; unable to opt out of the legal regime; bear the direct costs of imprisonment, asset forfeiture, and restricted civil rights.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, criminalized_users, payer,
    powerless, immediate, trapped, national).

% Experience violence and instability resulting from illicit market territorial disputes created by prohibition; pay safety costs without voice in the policy design or enforcement priorities.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_communities, payer,
    powerless, immediate, constrained, local).

% Advocate for health-based approaches to substance use; structurally excluded from prohibitionist policy design, their expertise treated as incompatible with the moral-duty frame.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Uniform prohibition of psychoactive substances to prevent perceived individual and collective harm, delegating enforcement to a centralized state apparatus and criminalizing non-medical use.
% TRANSFER_FUNCTION: Moves bodily autonomy, wealth, and liberty from criminalized users to the carceral state via fines, asset forfeiture, and incarceration; moves safety and stability from communities into black-market violence externalities; moves political legitimacy to prohibitionist coalitions.
% ABSENT_VOICES: Harm reduction advocates, legalization proponents, and criminalized users are excluded from policy design; they would argue for decriminalization, regulated markets, or health-based approaches but are framed as morally compromised or deviance-enabling.
% DISAPPEARANCE_RATIONALE: Without criminalization, the drug enforcement bureaucracy loses its central mandate, prison populations contract, black-market supply chains collapse or transition to regulated markets, and the prohibitionist political coalition loses a primary source of moral authority.
% FOUNDING_PROBLEM: Widespread substance use in the early 20th century was associated with racialized moral panic, social disorder, and perceived threats to public health; state actors sought to suppress use and assert moral order through criminal law.
% FOUNDING_PROBLEM_CORROBORATION: Prohibitionist political coalitions and the drug enforcement bureaucracy attest the problem remains live, citing overdose deaths. Public health researchers, historians, and harm reduction advocates outside the benefiting parties attest that the founding moral panic has evolved into a health crisis that criminalization exacerbates; international comparisons from jurisdictions with decriminalization corroborate alternative approaches.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint channels wealth, liberty, and bodily autonomy from criminalized users into the carceral state via incarceration, asset forfeiture, and fines. Suppression is higher (0.88) because the regime depends on actively excluding alternatives such as decriminalization, safe supply, and legalization, and on punishing dissent. Theater ratio rises to 0.70 as the gap between claimed harm prevention and actual outcomes (mass incarceration, unregulated overdose supply, militarized enforcement theater) widens over the interval. Accessibility collapse is substantial (0.70) because legal alternatives are structurally barred, though some exist in other jurisdictions. Resistance is moderate (0.55) because reform movements have gained visibility but remain politically subordinate.
 *
 * PERSPECTIVAL GAP:
 *   The prohibitionist political coalition experiences the constraint as a legitimate expression of moral statecraft and public safety; the engine will compute a near-beneficiary classification for this seat. Criminalized users experience the identical constraint as arbitrary state violence and extraction; the engine will compute a full-target classification for this seat. The divergence is structural, not perspectival in the sense of mere opinionâit follows from the declared beneficiary/victim asymmetry and the radical difference in exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The drug enforcement bureaucracy and prohibitionist political coalition are structural beneficiaries: they collect budget, institutional mandate, and moral-political authority, placing them near the beneficiary end of directionality. Criminalized users are the primary targets: they bear incarceration, financial penalties, and civil restrictions with near-zero exit, placing them near the full-target end. Black-market communities are secondary targets suffering externalized violence. Harm reduction advocates are excluded rather than coordinated; their exclusion is necessary for the constraint's legitimating narrative to persist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâracialized moral panic and perceived disorderâhas been substantially contested by historical and public health analysis outside the benefiting parties. The enforcement apparatus has persisted and grown despite decades of evidence that criminalization exacerbates the harms it claims to prevent. This suggests mandatrophy (the mandate has outlived its functional justification), yet the constraint is not a piton because active beneficiaries continue to capture substantial extraction and actively suppress reform. The snare classification captures this: the coordination story is cover, and the persistence depends on coercion and concentrated beneficiary interest, not merely inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_reading_sibling_delta,
    'This constraint instantiates the prohibition reading of the substance_control_legitimacy kernel. How would the structural classification change if the harm_reduction_reading or legalization_reading were adopted instead?',
    'Compare victim/beneficiary structures across sibling constraint stories: harm_reduction removes users from the victim set and reframes them as coordinated health-system beneficiaries; legalization removes state enforcement and eliminates carceral extraction entirely.',
    'Under harm_reduction, classification shifts toward tangled_rope or rope as health coordination dominates; under legalization, extraction collapses toward a low-coercion market arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_reading_sibling_delta, conceptual, 'Structural delta between prohibition reading and sibling readings of the same kernel.').

omega_variable(
    carceral_net_harm_balance,
    'Does the prohibition regime prevent more harm through reduced use than it creates through incarceration, black-market violence, and unregulated supply?',
    'Comparative policy analysis across jurisdictions with varying prohibition intensity; longitudinal studies of overdose rates, incarceration rates, and violent crime before and after decriminalization or legalization.',
    'If the regime is net harmful, the coordination story is cover and snare classification is reinforced; if net preventive, tangled_rope classification becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carceral_net_harm_balance, empirical, 'Net harm prevention versus carceral and black-market harm creation.').

omega_variable(
    enforcement_mandatrophy,
    'Has the drug enforcement apparatus outlived the functional problem it was built to solve, persisting now as institutional self-preservation?',
    'Budget and employment trajectory of enforcement agencies relative to substance-use mortality trends; independent legislative hearings on enforcement efficacy.',
    'If the apparatus persists despite failing to reduce use or harm, the constraint exhibits piton-like theatrical maintenance in addition to snare extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mandatrophy, empirical, 'Whether enforcement persists by institutional inertia rather than problem-solving efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sub_ctrl_proh_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sub_ctrl_proh_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(sub_ctrl_proh_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(sub_ctrl_proh_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(sub_ctrl_proh_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.7).
narrative_ontology:measurement(sub_ctrl_proh_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(sub_ctrl_proh_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sub_ctrl_proh_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(sub_ctrl_proh_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(sub_ctrl_proh_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(sub_ctrl_proh_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(sub_ctrl_proh_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sub_ctrl_proh_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sub_ctrl_proh_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(sub_ctrl_proh_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(sub_ctrl_proh_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(sub_ctrl_proh_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(sub_ctrl_proh_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the prohibition reading of the substance_control_legitimacy kernel. Its sibling readings arise from contestation of the same kernel but instantiate structurally distinct constraints with different beneficiary/victim profiles and epsilon values. Per the epsilon-invariance principle, each reading is authored as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
