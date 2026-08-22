% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Prohibition Reading: State Criminalization of Drug Use/Possession for Third-Party Protection
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition reading of the
 *   substance_control_authority kernel: the claim that the state possesses
 *   legitimate authority to criminalize drug use and possession in order to
 *   protect third parties from drug-related crime and social disorder. Under
 *   this reading, drug users are cast as threats to public order rather than
 *   agents with autonomy, and the primary state response is carceral:
 *   policing, prosecution, and incarceration. The constraint is actively
 *   enforced through criminal law, with marked racial disparities in
 *   application and substantial public expenditure. It is claimed as a
 *   coordination mechanism (public safety) but operates with high asymmetric
 *   extraction from stigmatized and racialized populations.
 *
 * KEY AGENTS:
 *   - state_legislature: agenda setter (institutional/analytical) â defines criminal statutes and appropriates enforcement resources
 *   - law_enforcement_agencies: beneficiary (institutional/constrained) â receive budgets and forfeiture; institutional growth tied to prohibition
 *   - drug_users: primary target (powerless/trapped) â bear extraction via incarceration and criminal records
 *   - racialized_communities: structural target (moderate/identity_locked) â disparately policed and incarcerated despite similar usage rates
 *   - public_health_agencies: excluded (institutional/constrained) â possess counter-evidence but lack policy authority
 *   - drug_policy_reform_advocates: observer (organized/analytical) â document harms and argue for reclassification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.85).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Prohibition Reading: State Criminalization of Drug Use/Possession for Third-Party Protection").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '0a5c96df-5ead-45ec-b332-2f7951e1cd77').
narrative_ontology:cs_kernel_codification('0a5c96df-5ead-45ec-b332-2f7951e1cd77', formalized).
narrative_ontology:cs_authority_grounding('0a5c96df-5ead-45ec-b332-2f7951e1cd77', extraction).
narrative_ontology:cs_interpretation_layer_present('0a5c96df-5ead-45ec-b332-2f7951e1cd77').
narrative_ontology:cs_reading_relation('0a5c96df-5ead-45ec-b332-2f7951e1cd77', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a5c96df-5ead-45ec-b332-2f7951e1cd77', substance_control_authority__legalization_reading, forecloses).
narrative_ontology:cs_axiom('0a5c96df-5ead-45ec-b332-2f7951e1cd77', foundational, state_criminalization_for_public_safety).
narrative_ontology:cs_axiom_status(state_criminalization_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('0a5c96df-5ead-45ec-b332-2f7951e1cd77', state_criminalization_for_public_safety, conventional).
narrative_ontology:cs_axiom('0a5c96df-5ead-45ec-b332-2f7951e1cd77', foundational, deterrence_through_incarceration).
narrative_ontology:cs_axiom_status(deterrence_through_incarceration, holdable).
narrative_ontology:cs_axiom_grounding('0a5c96df-5ead-45ec-b332-2f7951e1cd77', deterrence_through_incarceration, empirically_contingent).
narrative_ontology:cs_reference_frame('0a5c96df-5ead-45ec-b332-2f7951e1cd77', carceral_public_safety_framework).
narrative_ontology:cs_drift_state('0a5c96df-5ead-45ec-b332-2f7951e1cd77', contemporary_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a5c96df-5ead-45ec-b332-2f7951e1cd77', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_public).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racialized_communities).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_theory).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, carceral_public_safety).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, state_police_power_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains criminal statutes that classify drug use and possession as offenses punishable by incarceration. Appropriates billions in enforcement and corrections funding. Frames this as an exercise of constitutional police power to protect public health and safety.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Receive statutory authority, budgets, staffing, and civil asset forfeiture powers tied to drug prohibition. Their institutional growth, equipment acquisitions, and employment levels depend on continued criminalization. Lobby legislatively against reform.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).

% Claimed beneficiaries of reduced drug-related crime and neighborhood disorder through deterrence. Experience varies sharply by geography and class; the justification narrative names them as the constraint's reason, though many receive neither measurable protection nor direct harm.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_public, beneficiary,
    organized, biographical, mobile, national).

% Subject to arrest, prosecution, incarceration, and lifelong criminal records for use or possession. Bear the direct costs of the constraint: loss of liberty, exposure to unregulated supply, economic marginalization, and social stigma. Exit requires total abstinence or successful evasion, both high-cost and unreliable.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Experience disproportionate enforcement, arrest, and incarceration rates despite similar drug-use prevalence across demographics. Policing practices concentrate on these neighborhoods, making race a structural predictor of exposure to the constraint's extraction. Exit is unavailable because racial identity is inescapable.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racialized_communities, payer,
    moderate, generational, identity_locked, national).

% Possess epidemiological evidence that criminalization increases overdose mortality and infectious disease, but are structurally sidelined by the criminal justice framing. Their preferred interventionsâtreatment, harm reductionâare legally and budgetarily subordinated to policing.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_agencies, excluded,
    institutional, biographical, constrained, national).

% Document racial disparities, enforcement costs, and deterrence failures. Argue that prohibition creates black-market violence and disorder that it claims to suppress. Provide the analytical seat that reads the constraint as carceral extraction rather than public safety coordination.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_policy_reform_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to solve a collective-action problem of public order by using threatened state punishment to deter drug consumption, thereby reducing theft, violence, and neighborhood disorder attributed to drug markets and intoxication.
% TRANSFER_FUNCTION: Transfers liberty, bodily autonomy, health security, and future economic opportunity from drug users and racialized communities to the state (via incarceration, fines, surveillance) and to law enforcement agencies (via budgets, asset forfeiture, employment), while promising diffuse safety benefits to the non-using public.
% ABSENT_VOICES: Drug users themselves are politically disenfranchised by felony status and stigma; public health officials who would prioritize treatment and harm reduction over punishment; and communities that experience enforcement as occupation rather than protection. These voices are excluded from legislative design but would contest the constraint's framing if admitted.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force immediate reorganization of policing priorities, prison systems, and budget flows. Drug markets would shift from black-market to regulatory or medical frameworks; public health agencies would gain jurisdiction; and millions of criminal records would become legally meaningless. The world rearranges because the constraint organizes vast institutional and carceral infrastructure around it.
% FOUNDING_PROBLEM: Uncoordinated drug use was perceived to generate externalitiesâaddiction-driven property crime, public intoxication and disorder, family breakdown, and contagious diseaseâthat voluntary private action and local norms could not contain, requiring state criminalization to protect non-consenting third parties.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement unions and some victim-advocacy groups corroborate the live-status narrative. However, public health researchers, post-decriminalization jurisdictions (Portugal, Oregon), and criminal justice reform organizations provide external corroboration that the founding problem is either misattributed to drug use per se or more effectively addressed by non-carceral interventions. Corroboration is split and politically polarized; no independent consensus exists.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint transfers liberty, health, and economic capacity from users to the carceral state, with incarceration as the primary mechanism. Suppression (0.85) is high because the arrangement depends on continuous policing, search, arrest, and imprisonment to persist; without active enforcement, the constraint collapses. Theater_ratio (0.45) reflects significant performative maintenance: 'tough on crime' rhetoric, drug-war pageantry, and racialized moral panic that exceed the measurable public-safety return. Accessibility_collapse (0.80) is high because legal alternatives to criminalization (regulation, decriminalization) are structurally excluded from legislative consideration in prohibitionist jurisdictions. Resistance (0.60) captures persistent but politically marginalized reform movements, civil liberties litigation, and public health opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state legislature) and beneficiary seat (law enforcement) experience the constraint as a legitimate exercise of police power producing public safety. The payer seats (drug users, racialized communities) experience it as unpredictable violence, loss of liberty, and intergenerational trauma. The engine computes this divergence from identical structural data: low directionality for the institutional beneficiaries, high directionality for the trapped targets. The third-party public sits in between, receiving a claimed benefit they did not ask for and cannot easily verify.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are third_party_public (claimed safety) and law_enforcement_agencies (budgets, forfeiture, institutional scope). Victims are drug_users (direct carceral extraction) and racialized_communities (disparate enforcement). Law enforcement has constrained exit: their institutional identity and funding depend on prohibition, but individual officers could leave. Drug users have trapped exit: ceasing use or avoiding detection is high-cost and unreliable. Racialized communities have identity_locked exit: race is not escapable, making the constraint inescapable regardless of behavior. These structural relationships drive the engine's directionality derivation without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) by insisting on the victim set and active enforcement, while preventing mislabeling as pure extraction (snare) by requiring the declared coordination function (third-party protection via deterrence). The mandatrophy questionâwhether the founding problem of drug-related disorder is still liveâis contested: the constraint persists partly because the carceral apparatus has become self-sustaining, but the reform movement argues the founding problem is better addressed by non-carceral means. The R5 genealogy interview records this contestation explicitly, routing mandatrophy resolution through corroboration rather than self-assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_contingency,
    'Does criminalization actually deter drug use and related crime, or does it primarily displace activity into black markets without reducing net harm?',
    'Cross-jurisdictional natural experiments comparing prohibition, decriminalization, and legalization regimes on overdose rates, property crime, and violent crime metrics.',
    'If deterrence is empirically negligible, the coordination function collapses and the constraint reads as snare; if partial, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_contingency, empirical, 'Empirical contingency of the deterrence premise underlying prohibition.').

omega_variable(
    racial_disparity_structural_or_incidental,
    'Are the documented racial disparities in enforcement an incidental artifact of policing patterns, or a structural feature that stabilizes the constraint politically?',
    'Historical analysis of legislative intent, enforcement targeting data, and public-opinion polling on race and drug policy.',
    'If structural, the extraction is identity-locked and the constraint''s persistence is tied to racialized social control; if incidental, reform may be easier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_structural_or_incidental, empirical, 'Whether racial disparity is structural or incidental to prohibition enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (threat of arrest/incarceration) or internalized (stigma, self-policing by users)?',
    'Post-decriminalization behavioral studies: if use patterns change dramatically when legal status shifts, suppression was primarily structural; if stigma persists unchanged, suppression is partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure because the target carries the constraint after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for drug users.').

omega_variable(
    reading_boundary_ambiguity,
    'Does the prohibition reading foreclose the legalization reading within a single legal framework, or do they merely coexist as alternative legislative programs?',
    'Comparative constitutional analysis: whether a jurisdiction can simultaneously hold criminal prohibition and regulated legal markets for the same substance without logical contradiction.',
    'If foreclosed, the readings are incommensurable kernels; if coexistent, they are policy options within a shared framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Logical relationship between prohibition and legalization readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__prohibition_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__prohibition_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(subs_tr_t32, substance_control_authority__prohibition_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__prohibition_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__prohibition_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(subs_be_t32, substance_control_authority__prohibition_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__prohibition_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__prohibition_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(subs_su_t32, substance_control_authority__prohibition_reading, suppression_requirement, 32, 0.88).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the prohibition reading of the substance_control_authority kernel. It is structurally distinct from the harm_reduction and legalization readings, which evaluate different claims about the same policy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
