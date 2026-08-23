% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure Authority Over Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint instantiates the member_sovereignty_primary reading of
 *   the federation_membership_obligations kernel. It holds that national
 *   welfare states retain sovereign authority to define their beneficiary
 *   boundaries, and that free movement of workers is conditional on not
 *   undermining receiving state labor markets or welfare system
 *   sustainability. The arrangement coordinates welfare sustainability across
 *   asymmetric economies (genuine coordination function) while extracting
 *   fiscal contributions from mobile workers who are excluded from
 *   non-contributory benefits (asymmetric extraction). Active enforcement
 *   includes habitual residence tests, waiting periods, genuine link
 *   requirements, and Court-supervised proportionality reviews. The claimed
 *   type is tangled_rope — the constraint solves a real collective action
 *   problem (welfare sustainability under free movement) but does so through
 *   asymmetric extraction from a structurally disadvantaged group (mobile
 *   workers).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.74).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure Authority Over Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '6389b11a-5bb8-44d0-b89b-299125366e3b').
narrative_ontology:cs_kernel_codification('6389b11a-5bb8-44d0-b89b-299125366e3b', formalized).
narrative_ontology:cs_authority_grounding('6389b11a-5bb8-44d0-b89b-299125366e3b', lineage).
narrative_ontology:cs_interpretation_layer_present('6389b11a-5bb8-44d0-b89b-299125366e3b').
narrative_ontology:cs_reading_relation('6389b11a-5bb8-44d0-b89b-299125366e3b', federation_membership_obligations__integration_primary, influences).
narrative_ontology:cs_reading_relation('6389b11a-5bb8-44d0-b89b-299125366e3b', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('6389b11a-5bb8-44d0-b89b-299125366e3b', foundational, national_welfare_sovereignty_primacy).
narrative_ontology:cs_axiom_status(national_welfare_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('6389b11a-5bb8-44d0-b89b-299125366e3b', national_welfare_sovereignty_primacy, conventional).
narrative_ontology:cs_axiom('6389b11a-5bb8-44d0-b89b-299125366e3b', foundational, free_movement_conditional_on_welfare_sustainability).
narrative_ontology:cs_axiom_status(free_movement_conditional_on_welfare_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('6389b11a-5bb8-44d0-b89b-299125366e3b', free_movement_conditional_on_welfare_sustainability, conventional).
narrative_ontology:cs_axiom('6389b11a-5bb8-44d0-b89b-299125366e3b', secondary, habitual_residence_as_legitimate_gate).
narrative_ontology:cs_axiom_status(habitual_residence_as_legitimate_gate, holdable).
narrative_ontology:cs_axiom_grounding('6389b11a-5bb8-44d0-b89b-299125366e3b', habitual_residence_as_legitimate_gate, conventional).
narrative_ontology:cs_reference_frame('6389b11a-5bb8-44d0-b89b-299125366e3b', maastricht_social_protocol_compromise).
narrative_ontology:cs_drift_state('6389b11a-5bb8-44d0-b89b-299125366e3b', post_eastern_enlargement_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6389b11a-5bb8-44d0-b89b-299125366e3b', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_beneficiaries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, protected_national_labor_force).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_commuters).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, posted_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, protected_national_labor_force).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_state_sustainability_precondition).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, labor_market_protection_legitimacy).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, national_social_contract_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain legislative veto over welfare access conditions for mobile workers; define habitual residence tests, waiting periods, and contribution thresholds; justify closure as necessary for welfare system sustainability and democratic accountability to national electorates.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Nationals and long-term residents whose access to unemployment benefits, healthcare, housing support, and pension accrual is protected from dilution by mobile worker claims; benefit from labor market insulation against wage competition.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_beneficiaries, beneficiary,
    organized, biographical, constrained, national).

% Domestic workers shielded from direct wage competition with mobile workers willing to accept lower standards; also bear fiscal costs of enforcement apparatus and potential labor shortages in sectors dependent on mobile labor.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, protected_national_labor_force, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, protected_national_labor_force, payer).

% EU citizens exercising free movement who pay taxes and social contributions in receiving states but face waiting periods (3-5 years), habitual residence tests, and categorical exclusions from non-contributory benefits; exit means returning to sending state or accepting precarious status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, continental).

% Workers living in one member state and working in another; face coordination gaps in social security coverage, double taxation risks, and exclusion from residence-based benefits in both states; structurally dependent on bilateral agreements.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_commuters, payer,
    moderate, biographical, constrained, regional).

% Workers temporarily assigned by employer to another member state; remain under sending state social security but face receiving state labor law minimums; vulnerable to enforcement gaps, wage theft, and housing precarity with no local welfare access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, posted_workers, payer,
    powerless, immediate, trapped, continental).

% Monitor member state compliance with Treaty free movement provisions; initiate infringement proceedings against excessive closure; issue judgments balancing mobility rights against welfare sustainability (e.g., Dano, Alimanovic); constrained by subsidiarity principle and member state resistance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_commission_and_court, observer,
    institutional, generational, analytical, continental).

% Governments of origin countries whose citizens are mobile workers; bear costs of remittance dependence, brain drain, and social protection gaps for returning migrants; formally consulted but lack veto over receiving state welfare closure rules.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, sending_state_governments, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates welfare system sustainability across a federation with asymmetric economic development: prevents welfare tourism that could destabilize national risk pools, protects labor standards from race-to-the-bottom competition, and maintains democratic legitimacy of nationally financed solidarity systems.
% TRANSFER_FUNCTION: Moves fiscal risk and administrative burden from receiving state welfare systems onto mobile workers (who contribute without full access) and sending states (who retain social protection responsibility for exported citizens); moves labor market insulation benefits to protected national workforces.
% ABSENT_VOICES: Undocumented mobile workers and third-country nationals with precarious status who are excluded from both free movement rights and welfare access entirely; future generations of mobile workers whose access conditions are set by current national electorates without their input; EU-level political parties attempting to construct transnational solidarity but lacking institutional purchase.
% DISAPPEARANCE_RATIONALE: If welfare closure authority vanished overnight, receiving states would face immediate fiscal pressure on non-contributory benefits, labor markets would experience wage compression in low-skill sectors, and national political backlash would likely trigger unilateral border controls or federation exit threats — the EU single market would reorganize around renationalized welfare borders.
% FOUNDING_PROBLEM: Post-1992 single market completion created free movement of workers without a federal welfare union; member states feared welfare magnet effects and labor market disruption in high-benefit states, while low-wage states feared brain drain; the coordination compromise was conditional mobility with national welfare boundaries preserved.
% FOUNDING_PROBLEM_CORROBORATION: Original treaty negotiators (Delors Commission archives) attest the founding problem was preventing welfare competition distorting labor mobility; receiving state governments (Germany, Netherlands, Austria position papers 2010-2020) attest the problem is live and worsening with east-west migration; sending state governments (Poland, Romania, Bulgaria parliamentary reports) and EU Court jurisprudence (Dano 2014, Alimanovic 2015) attest the problem is substantially solved for contributory benefits but contested for non-contributory minima.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects that mobile workers contribute 8-12% of receiving state social security revenues while accessing only 3-5% of non-contributory benefits; suppression (0.74) captures the active enforcement machinery (residence verification, contribution tracking, Court litigation); theater ratio (0.38) measures the gap between 'European solidarity' rhetoric and the material closure of welfare boundaries; accessibility collapse (0.68) reflects that mobile workers' alternatives (return, irregular status, litigation) are costly and incomplete; resistance (0.58) captures ongoing EU Court challenges, Commission infringement actions, and political mobilization by mobile worker organizations. The measurement series shows extraction and suppression rising steadily post-2004 enlargement, with theater increasing as coordination rhetoric persists while material closure deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the member state legislature seat, the constraint appears as necessary coordination (rope-like) — without closure authority, welfare systems would face unsustainable claims. From the mobile worker seat, the same constraint operates as extraction (snare-like) — they pay full contributions for partial benefits. From the EU Court seat, the constraint is a contested balancing act (tangled_rope) — proportionality review attempts to calibrate the coordination/extraction boundary. The engine computes this seat divergence from the structural power/exit/role data.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures are agenda_setters with institutional power and analytical exit (they write the rules); receiving state welfare beneficiaries and protected labor forces are beneficiaries with organized power but constrained exit (they cannot easily leave the national welfare system); mobile EU workers, cross-border commuters, and posted workers are payers with moderate-to-powerless power and constrained-to-trapped exit (they are structurally dependent on access to receiving state labor markets); EU institutions are observers with analytical exit; sending state governments are excluded — their citizens bear costs but they lack veto. Directionality derivation: agenda_setters and beneficiaries sit at low d (subsidized by constraint), payers at high d (extracted from), excluded at highest d (bear costs without voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing welfare competition distorting labor mobility) remains contested: receiving states argue it is live and intensifying; sending states and Court jurisprudence argue contributory coordination is largely solved. The arrangement persists because no party has sufficient power to rewrite it — receiving states block federalization, sending states lack leverage, mobile workers lack voice. This is not mandatrophy (the coordination function is not dead) but a frozen conflict where the extraction component has grown beyond the coordination justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where is the structural boundary between legitimate welfare sustainability coordination and extractive closure? At what point does conditional access become exclusionary extraction?',
    'Longitudinal comparison of mobile worker contribution/benefit ratios across member states; Court proportionality jurisprudence evolution; fiscal impact studies of full portability counterfactuals.',
    'If the boundary is closer to coordination, the constraint trends toward rope; if closer to extraction, it trends toward snare. Current metrics place it in tangled_rope but the boundary is the contested terrain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The irreducible ambiguity in distinguishing welfare sustainability protection from rent extraction against mobile workers.').

omega_variable(
    sending_state_coalition_potential,
    'Can sending state governments form an effective coalition to challenge receiving state closure rules, or are they structurally trapped by remittance dependence and brain drain?',
    'Analysis of Visegrád Group coordination attempts, European Parliament voting patterns, and Commission initiative success rates on social security coordination reforms.',
    'If sending states can coalition, the constraint''s power asymmetry shifts and extraction may be renegotiated; if trapped, the extraction is structurally locked in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_coalition_potential, empirical, 'Whether the excluded stakeholder (sending states) can convert structural exclusion into countervailing power.').

omega_variable(
    federalization_counterfactual,
    'Would a federal EU welfare floor (minimum income, unemployment reinsurance) resolve the coordination problem without the extraction, or would it create new extraction dynamics?',
    'Comparative study of federal systems (US, Canada, Germany) where sub-federal welfare boundaries coexist with federal floors; modeling of fiscal transfer requirements for EU-wide minimum standards.',
    'If federalization resolves the coordination-extraction tension, the constraint is a scaffold (transitional); if it reproduces extraction at higher level, the tangled_rope is structural to multi-level governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalization_counterfactual, preference, 'Whether the constraint''s extraction is contingent on current institutional design or intrinsic to welfare federalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2008, 0.33).
narrative_ontology:measurement(fede_tr_t2012, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2016, 0.37).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(fede_be_t2008, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2008, 0.53).
narrative_ontology:measurement(fede_be_t2012, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2012, 0.57).
narrative_ontology:measurement(fede_be_t2016, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(fede_su_t2008, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement(fede_su_t2012, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(fede_su_t2016, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2024, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__member_sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, eu_social_security_coordination_regulation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, posted_workers_directive_enforcement).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, eu_citizenship_rights_judicial_activation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_obligations kernel. The integration_primary reading (free movement constitutive) and selective_solidarity reading (contributory tiering) are separate constraint stories with their own ε values and stakeholder structures. All three are linked via affects_constraints. The kernel's natural-language label ('EU free movement and welfare') conflates three structurally distinct coordination-extraction arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, organized, 0.25).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, moderate, 0.75).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
