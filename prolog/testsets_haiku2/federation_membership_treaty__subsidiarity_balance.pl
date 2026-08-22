% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Subsidiarity Balance: Proportionate Free Movement with National Interest Gates
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   The subsidiarity_balance reading of the federation membership treaty
 *   treats free movement as a presumptively lawful right constrained by
 *   proportionality rather than as either constitutive (integration_primary)
 *   or conditional on state consent (sovereignty_primary). Member states
 *   retain graduated authority to impose residency requirements, welfare
 *   contributions tests, labor-market protections, and public-health
 *   exceptions—but only when proportionately justified by legitimate national
 *   interests. The constraint's operation creates a graduated extraction
 *   structure: workers and employers benefit from mobility while bearing
 *   legitimate state restrictions; destination-state workers and welfare
 *   systems bear costs but retain some protective authority; member states
 *   preserve autonomy but face judicial review of proportionality. The
 *   reading sits structurally between its siblings: rejecting pure
 *   integration but also rejecting sovereignty absolutism.
 *
 * KEY AGENTS:
 *   - Mobile workers benefiting from proportionality-bounded movement rights
 *   - Employers accessing wider labor pools under mobility protections
 *   - Destination-state workers and welfare systems bearing extraction costs
 *   - Federation court/arbitrating authority enforcing proportionality doctrine
 *   - Integrationist coalition excluded by the proportionality framework
 *   - Sovereigntist coalition excluded by mandatory mobility rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.47).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Subsidiarity Balance: Proportionate Free Movement with National Interest Gates").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0').
narrative_ontology:cs_kernel_codification('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', formalized).
narrative_ontology:cs_authority_grounding('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', extraction).
narrative_ontology:cs_interpretation_layer_present('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0').
narrative_ontology:cs_reading_relation('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', foundational, proportionality_limits_both_extremes).
narrative_ontology:cs_axiom_status(proportionality_limits_both_extremes, holdable).
narrative_ontology:cs_axiom_grounding('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', proportionality_limits_both_extremes, deontological).
narrative_ontology:cs_axiom('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', foundational, domain_specific_thresholds_for_legitimacy).
narrative_ontology:cs_axiom_status(domain_specific_thresholds_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', domain_specific_thresholds_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', proportionality_limited_reciprocal_autonomy).
narrative_ontology:cs_drift_state('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', contemporary_federation_jurisprudence, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9c6dcbbe-ede6-400d-bcd2-9b3596ea94d0', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers_benefiting_cohort).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, employers_seeking_labor_mobility).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_state_preserving_autonomy).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, workers_in_protected_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, destination_state_welfare_budgets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, origin_state_labor_pool).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, origin_state_labor_pool).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can exercise free movement across member states to access employment opportunities, higher wages, or improved living conditions. The subsidiarity reading grants them a presumptive right to movement, though member states retain authority to impose proportionate restrictions on welfare access or labor-market entry. Their mobility is facilitated by treaty structures that require states to justify restrictions rather than justify openness.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers_benefiting_cohort, beneficiary,
    moderate, biographical, mobile, global).

% Access a wider labor pool and can relocate operations across member states without visa or work-permit friction. The constraint enables cross-border hiring and labor arbitrage while preserving sufficient national labor-market protection that states do not impose blanket foreign-worker bans. They benefit from mobility without facing the total market foreclosure that pure sovereignty reading would permit.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, employers_seeking_labor_mobility, beneficiary,
    powerful, generational, arbitrage, global).

% Face increased labor-market competition from mobile workers, wage pressure in sectors where free movement is permitted, and reduced capacity of their state to shield domestic employment through entry restrictions. The subsidiarity reading permits only proportionate restrictions, meaning protections are limited to narrow national-interest cases (public health, security, social cohesion) rather than blanket labor-market closure. They pay the cost of mobility through wage and employment effects.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, workers_in_protected_labor_markets, payer,
    moderate, biographical, constrained, national).

% Bear fiscal costs when mobile workers from poorer member states access social benefits, healthcare, or education systems in destination states with higher standards. The subsidiarity reading allows states to impose residency requirements and contributions tests, but does not permit them to categorically deny welfare to migrants. They absorb some costs of mobility and must distinguish legitimate social cohesion concerns from protectionist welfare exclusion.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, destination_state_welfare_budgets, payer,
    institutional, generational, constrained, national).

% Experiences brain drain when high-skilled workers exercise free movement to wealthier states, reducing human capital at home. Yet they also benefit from remittances, return migration of workers with new skills, and reduced domestic unemployment pressure. The subsidiarity constraint permits them to use training bonds or reciprocal recognition requirements to slow outflow, though not to block it entirely. They are both beneficiary (through mobility benefits) and payer (through emigration costs).
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, origin_state_labor_pool, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, origin_state_labor_pool, payer).

% Interprets and enforces the proportionality principle that defines the subsidiarity reading: judges whether member state restrictions on free movement are narrowly tailored to legitimate national interests or are pretextual protectionism. Maintains the boundary between permissible graduated constraint and impermissible blanket exclusion. Their case law and doctrinal development actively determines which restrictions are upheld and which are struck down, making the constraint's actual operation depend on their jurisprudence.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_court_or_arbitrating_authority, agenda_setter,
    institutional, generational, analytical, global).

% Advocates for treating free movement as constitutive and near-absolute (integration_primary reading), not as a constrained right subject to proportionality balancing. Excluded from the subsidiarity decision-making framework because their preferred reading has not prevailed in the federation's constitutional interpretation. They contest the legitimacy of the proportionality boundary itself and would remove or weaken state gates.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, integrationist_political_coalition, excluded,
    powerful, generational, mobile, global).

% Advocates for member state authority to impose restrictions on free movement to protect national labor markets and welfare systems (sovereignty_primary reading). Excluded from the subsidiarity framework because proportionality review is treated as the binding principle, not member state consent. They argue the constraint unfairly privileges mobility over state autonomy and would permit states to restrict movement more broadly.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sovereigntist_political_coalition, excluded,
    powerful, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, federation_court_or_arbitrating_authority).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework in which member states can participate in a single market with facilitated labor mobility while retaining graduated authority to protect domestic labor markets, welfare systems, and social cohesion through proportionate restrictions. Solves the coordination problem of reconciling market integration with legitimate member state protective interests—an alternative to pure open borders (which eliminates state autonomy) and pure sovereignty (which eliminates mobility).
% TRANSFER_FUNCTION: Transfers labor-market competition and fiscal costs from mobile workers and employers to destination-state workers and welfare systems, in exchange for states accepting legally enforceable mobility rights rather than discretionary restriction. The transfer is graduated: larger in high-mobility sectors and lighter in sectors where states can invoke proportionate national-interest exceptions.
% ABSENT_VOICES: Integrationist advocates (who would demand near-absolute mobility) and sovereigntist advocates (who would demand state discretion to restrict) are both excluded from the subsidiarity framework because it treats proportionality balancing—not either extreme—as the binding principle. Neither coalition has veto power over the proportionality doctrine, though both contest it in federation courts and legislative bodies.
% DISAPPEARANCE_RATIONALE: If the subsidiarity proportionality framework vanished, member states would either regress to sovereignty_primary reading (reimposing labor-market restrictions and welfare exclusions) or be pressured toward integration_primary reading (accepting near-absolute mobility). Employers would face unpredictability in hiring across borders; workers would experience either suddenly renewed barriers or suddenly unconstrained competition; federalism itself would shift toward either tighter integration or looser coordination. The constraint's disappearance would force a choice between its two sibling readings.
% FOUNDING_PROBLEM: Early federation members needed to integrate economic activity without triggering mass labor displacement or welfare-system collapse in lower-wage regions. Pure free movement threatened domestic labor markets and fiscal systems; pure sovereignty meant no real market integration. The proportionality principle emerged as a middle path: mobility is presumptively lawful, but states retain authority to invoke narrowly tailored exceptions for demonstrable national interests.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists document persistent wage effects and fiscal costs in destination states from unmanaged migration, validating the founding concern. Federation court doctrine confirms member states regularly invoke proportionality exceptions (residency requirements, contributions tests, public health provisions) to manage mobility effects. Independent analysts from both integrationist and sovereigntist traditions acknowledge the founding problem remains live: the tension between market gains and protective interests has not been resolved, only managed through the proportionality framework.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end), rising from 0.42 at start as enforcement machinery clarifies which restrictions are proportionate. The rise reflects accumulating case law that closes ambiguities about legitimate national interests: court rulings narrow the scope of permissible restrictions and establish clearer precedents for what counts as proportionate. Suppression is moderate (0.47) because the constraint suppresses both unrestricted mobility (states can impose proportionate gates) and blanket restrictions (courts strike down discriminatory closures). The suppression is symmetric rather than asymmetric: the framework actively constrains both extremes. Theater is moderate-high (0.42) because proportionality review involves substantial performative dispute—states frame labor protections as proportionate national interests while integrationists frame them as pretext; courts must perform boundary-drawing work. The measurements track a flattening trajectory after year 15: extractiveness and theater stabilize once the jurisprudence solidifies, suggesting the constraint has matured from contestation to internalization.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality principle creates asymmetric constraint relationships: mobile workers and employers are presumptively included and must justify exits (they have mobile exit options so low d); member states are presumptively constrained and must justify restrictions (they have constrained authority to exit, so moderate-high d); integrationists and sovereigntists are excluded entirely (they cannot shift the framework from within, so they advocate for reading replacement, not constraint navigation). The court maintains the boundary through repeated proportionality review, which is both a coordination function (clarifying what restrictions are permissible) and an extraction mechanism (restricting member state autonomy in ways they did not consent to).
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and employers are near beneficiaries (d toward 0.0) because they gain mobility presumptively protected by the framework, though constrained by proportionality exceptions. Workers in protected labor markets are targets (d toward 1.0) because they experience wage and employment costs without direct authority over the gates that constrain them. Destination-state welfare budgets are targets (d toward 0.8–0.9) because they bear fiscal costs and can impose only graduated restrictions. The federation court is the agenda-setter (d = analytical, no extraction) because it sets the proportionality doctrine. Integrationists and sovereigntists are structurally excluded and constrained—they would have high d if they had decision authority, but they are locked out of the framework itself, so their d is not computed in the constraint's operation (they appear in six_questions.absent_voices, not as active stakeholders).
 *
 * MANDATROPHY ANALYSIS:
 *   The subsidiarity_balance reading avoids false-mountain and false-rope classification by declaring the active enforcement it requires (requires_active_enforcement: true) and naming both beneficiaries and victims. It is tangled_rope, not snare, because the coordination function (reconciling mobility with state autonomy) is genuine and the constraint genuinely enables mobility it would not exist without—but the cost structure is asymmetric (destination-state workers and welfare systems bear extraction through wage/fiscal effects) and enforced (the court enforces the framework against member states that resist). A pure-snare reading would require that states have no genuine interest in mobility or that the proportionality doctrine is pure pretext; a pure-rope reading would require that suppression is symmetric and non-coercive. The tangled-rope classification holds because both the coordination and the asymmetric extraction are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_definition_contestation,
    'What constitutes a ''legitimate national interest'' sufficient to justify proportionate restrictions on free movement? Where is the boundary between proportionate protection and protectionist pretext?',
    'Federation court jurisprudence, comparative case law analysis, and empirical evidence of whether restrictions actually serve stated national interests or function as labor-market closure.',
    'If the proportionality boundary shifts toward stricter national-interest tests, extractiveness declines and the constraint moves toward integration_primary. If the boundary shifts toward permitting broader state exceptions, extractiveness rises and the constraint moves toward sovereignty_primary. The boundary is the reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_definition_contestation, conceptual, 'Whether the proportionality principle''s content is stable or contestable across cases and domains.').

omega_variable(
    symmetry_of_suppression_mechanisms,
    'Does the subsidiarity constraint suppress both unrestricted mobility AND blanket restrictions equally, or does it asymmetrically favor mobility over state closure?',
    'Audit of federation court decisions: what proportion strike down state restrictions vs. strike down expansive mobility claims? Analysis of which types of restrictions are upheld under proportionality review.',
    'If suppression is truly symmetric, the constraint is a genuine equilibrium. If court doctrine persistently favors mobility over state protection, the suppression is asymmetric and the constraint is a disguised integration-primary arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symmetry_of_suppression_mechanisms, empirical, 'Whether suppression operates equally on both mobility and state-protective directions or favors one.').

omega_variable(
    enforcement_sustainability,
    'Can the proportionality framework sustain enforcement when integrationist and sovereigntist coalitions contest the boundary with growing intensity, or does the framework decay under political pressure?',
    'Observation of federation court case outcomes, legislative attempts to override court doctrine, and political pressure on the court from excluded coalitions over the interval.',
    'If enforcement weakens, the constraint shifts toward sovereignty_primary (states ignore court doctrine). If enforcement hardens, it may shift toward integration_primary (courts expand mobility protection). Sustained proportionality indicates the reading remains live; collapse indicates reading replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Whether the subsidiarity framework''s enforcement capacity survives political contestation.').

omega_variable(
    graduated_constraint_domain_variation,
    'Are the thresholds for legitimate restrictions genuinely domain-specific (varying by sector and welfare type), or do they converge toward a single default that applies everywhere?',
    'Analysis of federation court doctrine and enforcement patterns: do proportionality thresholds differ across labor-market sectors, welfare systems, healthcare, education, and security domains, or is there a uniform approach?',
    'If thresholds are truly graduated by domain, the reading instantiates genuine subsidiarity (decentralized judgment of legitimate interests per context). If they collapse to uniform rules, the reading loses its distinguishing feature and converges toward integration_primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_constraint_domain_variation, empirical, 'Whether the constraint actually maintains domain-specific flexibility or applies uniform proportionality globally.').

omega_variable(
    kernel_reading_identity_under_contestation,
    'If the integrationist and sovereigntist readings gain political power and challenge the proportionality principle itself, can the subsidiarity reading maintain its identity or does it dissolve into one of its siblings?',
    'Longitudinal observation of federation constitutional interpretation and coalition power: does proportionality doctrine persist under political attack, shift toward integration, or toward sovereignty?',
    'If proportionality persists as a live reading, subsidiarity_balance remains a valid constraint. If it is abandoned, the kernel reverts to a two-reading structure (integration_primary vs. sovereignty_primary) and this constraint becomes historically obsolete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity_under_contestation, preference, 'Whether the subsidiarity reading survives as a distinct position or collapses under contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__subsidiarity_balance, theater_ratio, 5, 0.32).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.37).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__subsidiarity_balance, theater_ratio, 15, 0.41).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__subsidiarity_balance, theater_ratio, 20, 0.42).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__subsidiarity_balance, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 25, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.22).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested federation_membership_treaty kernel. Two sibling constraints instantiate the competing readings: integration_primary (free movement as constitutive, restrictions presumptively illegitimate) and sovereignty_primary (free movement as conditional on state consent, states retain closure authority). The three constraints form a kernel family linked by network.affects_constraints. Each reading has its own ε value, beneficiary/victim set, and type. The subsidiarity_balance reading's extractiveness (0.58) sits between the integration reading's low extraction (mobility presumptively costless to states) and sovereignty reading's high extraction (state authority presumptively costless to workers). Each reading represents a different structural reading of the same kernel, not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
