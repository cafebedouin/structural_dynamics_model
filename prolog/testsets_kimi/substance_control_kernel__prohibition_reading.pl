% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Substance Prohibition as Moral Transgression (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition_reading of the
 *   substance_control_kernel: the claim that substance use constitutes a
 *   moral transgression requiring state punishment to protect social order.
 *   Under this reading, substance users enter the criminal victim set, the
 *   criminal justice apparatus becomes the primary beneficiary of extraction
 *   via enforcement budgets and carceral expansion, and black market violence
 *   emerges as a secondary externality. The state exercises coercive rather
 *   than service-provision authority. The kernel is contested: sibling
 *   readings (harm_reduction, legalization) assign different structural roles
 *   to users and the state. This JSON authors ONLY the prohibition reading as
 *   a clean, epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - criminal_justice_apparatus: Agenda-setter and primary beneficiary (institutional/constrained) â administers prohibition and captures extraction
 *   - substance_users: Primary target (powerless/trapped) â bear criminalization, incarceration, and market violence
 *   - policed_communities: Secondary target (powerless/trapped) â bear concentrated enforcement externalities
 *   - public_health_sector: Excluded voice (organized/mobile) â possesses contradictory expertise but sidelined
 *   - reform_advocates: Excluded voice (organized/mobile) â push alternative readings from the legislative margins
 *   - independent_researchers: Analytical observer (analytical/analytical) â document outcome gaps without policy leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.84).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.91).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Prohibition as Moral Transgression (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5').
narrative_ontology:cs_kernel_codification('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', formalized).
narrative_ontology:cs_authority_grounding('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', extraction).
narrative_ontology:cs_interpretation_layer_present('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5').
narrative_ontology:cs_reading_relation('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', foundational, substance_use_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', substance_use_moral_transgression, deontological).
narrative_ontology:cs_axiom('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', secondary, punitive_state_preserves_social_order).
narrative_ontology:cs_axiom_status(punitive_state_preserves_social_order, holdable).
narrative_ontology:cs_axiom_grounding('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', punitive_state_preserves_social_order, instrumental).
narrative_ontology:cs_reference_frame('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', punitive_moral_order).
narrative_ontology:cs_drift_state('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', contemporary_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f401bf2e-e3cc-4b82-ae2b-4d7922d0f4a5', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, criminal_justice_apparatus).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, policed_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces substance prohibition through policing, prosecution, and incarceration. Captures operating budgets, employment, asset forfeiture revenue, and institutional authority from the criminalization of substance use. Framing prohibition as moral defense of social order justifies expanding carceral capacity and discretionary policing power.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminal_justice_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, criminal_justice_apparatus, beneficiary).

% Bear the direct costs of prohibition: criminal records, incarceration, fines, civil asset forfeiture, and exposure to black-market violence. Trapped by addiction, poverty, and the legal status of their conduct, with no licit exit from the constraint except total cessation under threat of punishment.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Bear the secondary externalities of prohibition: concentrated policing, black-market violence, property devaluation, family separation, and community destabilization. Geographically and economically trapped in enforcement zones, they pay the diffuse social cost of the state's punitive strategy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, policed_communities, payer,
    powerless, biographical, trapped, local).

% Possess evidence-based interventions and epidemiological data that contradict the moral-transgression framing, but are structurally excluded from policy design in the prohibition framework. Their expertise is subordinated to the enforcement apparatus's authority.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_sector, excluded,
    organized, generational, mobile, national).

% Advance decriminalization and legalization arguments based on human-rights and cost-benefit grounds. Structurally marginalized from legislative deliberation by the dominance of enforcement interests and moral-panic rhetoric.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, reform_advocates, excluded,
    organized, generational, mobile, national).

% Study the relationship between prohibition, substance-use prevalence, and social outcomes from outside the beneficiary apparatus. Their findings typically show weak or inverse correlations between punitive severity and use reduction, but these findings do not alter enforcement policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, independent_researchers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, criminal_justice_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to protect social order and collective moral fabric by deterring substance use through state punishment, incapacitating users via incarceration, and signaling societal condemnation of vice.
% TRANSFER_FUNCTION: Moves financial resources, human capital, and bodily autonomy from substance users and policed communities into the criminal justice apparatus via enforcement budgets, asset forfeiture, fines, and captive labor; concentrates moral-political authority in the state as arbiter of bodily conduct.
% ABSENT_VOICES: Substance users are criminalized and disenfranchised, excluded from policy design. The public health sector and harm-reduction advocates are sidelined. Affected communities are structurally absent from legislative deliberation. Reform advocates operate from the margins.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the criminal justice apparatus would lose a primary enforcement mandate and associated revenue streams, carceral populations would plummet, black markets would collapse or shift to licit commerce, policing patterns would reorganize, and social resources would redirect toward health or consumer frameworks.
% FOUNDING_PROBLEM: Perceived breakdown of social order and moral fabric due to substance use; perceived need for state capacity to condemn, deter, and punish vice in defense of collective morality.
% FOUNDING_PROBLEM_CORROBORATION: Enforcement agencies and political moral entrepreneurs assert the problem remains live. Independent criminologists, public health researchers, and international health bodies outside the beneficiary set contest that prohibition protects social order, citing stable or reduced drug-related harm under alternative regulatory regimes; no independent non-beneficiary corroboration supports the continued necessity of the punitive framing.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.84, 'kimi-k2.6', 'none', direct).

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
 *   Extraction is high (0.84 at interval end) because the apparatus captures fiscal and human resources from criminalized populations with minimal return in reduced use. Suppression is very high (0.91) because the constraint persists only through active policing, incarceration, and suppression of alternative regulatory frameworks; without enforcement, licit and illicit markets immediately reconfigure. Theater is moderate-high (0.62): the moral-transgression framing sustains enforcement rituals ( Drug War rhetoric, ceremonial sentencing) that are increasingly decoupled from measurable epidemiological outcomes. Accessibility collapse is high (0.78) because decriminalization and legalization alternatives are structurally excluded from mainstream policy discourse and legislative agendas despite international evidence. Resistance is substantial (0.72) due to persistent reform movements, affected-community organizing, and shifting public opinion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat experiences prohibition as necessary moral coordination defending civilization; the payer seats experience the identical structure as coercive extraction. The engine computes this divergence from the structural asymmetry in power, exit, and scope: the apparatus is institutional with generational horizons, while users are powerless and trapped in local scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The criminal justice apparatus sits near the beneficiary end: it sets the rules, administers enforcement, and collects the budgetary and authority rents. Substance users and policed communities sit near the full-target end: they pay through incarceration, violence, fines, and community destabilization with no reciprocal benefit. Excluded public health and reform seats are outside the active transfer but would register high directionality if structurally incorporated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims a coordination function (protecting social order) but its founding problem is contested and its disappearance would rearrange the world. The presence of a concentrated beneficiary (enforcement apparatus), an identifiable victim set (users and policed communities), and very high active enforcement prevent mislabeling this as a rope or mountain. The R5 genealogy (contested founding problem + world_rearranges disappearance) flags it as a mandate that may have outlived its original justification, but the continued extraction sustains it as a snare rather than a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the prohibition_reading of substance_control_kernel; the sibling harm_reduction_reading would remove users from the victim set and recast the state as service-provider; the sibling legalization_reading would eliminate the beneficiary role of the enforcement apparatus. Which structural element is dispositive: the victim set composition, the beneficiary identity, or the authority mode?',
    'Comparative policy analysis across jurisdictions holding different readings, tracking changes in victim sets and beneficiary flows when a jurisdiction switches readings.',
    'Determines whether the kernel is fundamentally about bodily autonomy, health, or social control, and which seat''s structural position drives classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Uncertainty about which structural element of the kernel is dispositive across readings.').

omega_variable(
    social_order_naturalness,
    'Is the social order that prohibition claims to protect a pre-political natural law or a constructed disciplinary target?',
    'Historical comparison of substance use regimes across cultures and eras; anthropological analysis of whether prohibition precedes or follows the enforcement apparatus.',
    'If social order is constructed post-hoc to justify the apparatus, the constraint is a false summit or snare; if pre-political, it approaches a rope or mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_order_naturalness, conceptual, 'Whether the protected social order is natural or constructed.').

omega_variable(
    enforcement_apparatus_extraction,
    'Does the criminal justice apparatus enforce prohibition because it benefits from the extraction, or does extraction follow from a genuine enforcement necessity?',
    'Budget and employment trajectory analysis of enforcement agencies correlated with decriminalization events; natural experiments where jurisdictions shifted readings.',
    'If budget trajectories track prohibition intensity independently of substance-use rates, the apparatus is a concentrated beneficiary and the constraint is extractive; if budgets track objective harm metrics, the coordination function may be genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_extraction, empirical, 'Whether enforcement intensity tracks institutional benefit or objective harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sc_prohibition_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sc_prohibition_tr_t5, substance_control_kernel__prohibition_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sc_prohibition_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(sc_prohibition_tr_t15, substance_control_kernel__prohibition_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(sc_prohibition_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sc_prohibition_tr_t25, substance_control_kernel__prohibition_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(sc_prohibition_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(sc_prohibition_tr_t35, substance_control_kernel__prohibition_reading, theater_ratio, 35, 0.55).
narrative_ontology:measurement(sc_prohibition_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(sc_prohibition_tr_t45, substance_control_kernel__prohibition_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement(sc_prohibition_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(sc_prohibition_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sc_prohibition_be_t5, substance_control_kernel__prohibition_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sc_prohibition_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sc_prohibition_be_t15, substance_control_kernel__prohibition_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(sc_prohibition_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(sc_prohibition_be_t25, substance_control_kernel__prohibition_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(sc_prohibition_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(sc_prohibition_be_t35, substance_control_kernel__prohibition_reading, base_extractiveness, 35, 0.8).
narrative_ontology:measurement(sc_prohibition_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(sc_prohibition_be_t45, substance_control_kernel__prohibition_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(sc_prohibition_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(sc_prohibition_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sc_prohibition_su_t5, substance_control_kernel__prohibition_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(sc_prohibition_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sc_prohibition_su_t15, substance_control_kernel__prohibition_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(sc_prohibition_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sc_prohibition_su_t25, substance_control_kernel__prohibition_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(sc_prohibition_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(sc_prohibition_su_t35, substance_control_kernel__prohibition_reading, suppression_requirement, 35, 0.87).
narrative_ontology:measurement(sc_prohibition_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(sc_prohibition_su_t45, substance_control_kernel__prohibition_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement(sc_prohibition_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three epsilon-invariant readings: prohibition_reading (high extraction, criminalization of users, enforcement apparatus as beneficiary), harm_reduction_reading (moderate extraction, users as patients, public health as beneficiary), and legalization_reading (low extraction, users as consumers, state as regulator). Each reading has a distinct beneficiary/victim structure and epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
