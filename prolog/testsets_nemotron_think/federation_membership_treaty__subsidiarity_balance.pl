% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Subsidiarity-Balanced Free Movement in Federation Treaty
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story captures the 'subsidiarity_balance' reading of the
 *   federation membership treaty kernel. The treaty establishes free movement
 *   of persons as a fundamental right but subjects it to a proportionality
 *   principle: member states may restrict mobility only to the extent
 *   necessary to protect legitimate national interests (labor market
 *   functioning, welfare sustainability, public order). The reading claims
 *   this creates a graduated, domain-sensitive constraint structure — not a
 *   binary open/closed choice. Beneficiaries and victims shift by policy
 *   domain: in professional qualifications, mobile workers benefit and
 *   protectionist states pay; in welfare access, net-contributor states
 *   benefit and mobile citizens pay; in posted workers, low-wage states
 *   benefit and high-wage states pay. The constraint requires active
 *   enforcement (Court jurisprudence, infringement procedures, preliminary
 *   rulings) and suppresses both unrestricted mobility claims and blanket
 *   restriction attempts. The claimed type is tangled_rope: genuine
 *   coordination (single labor market) fused with asymmetric extraction
 *   (domain-varying beneficiaries/victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.42).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Subsidiarity-Balanced Free Movement in Federation Treaty").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '1a5273c4-bcb7-455f-b41b-7030256663eb').
narrative_ontology:cs_kernel_codification('1a5273c4-bcb7-455f-b41b-7030256663eb', formalized).
narrative_ontology:cs_authority_grounding('1a5273c4-bcb7-455f-b41b-7030256663eb', lineage).
narrative_ontology:cs_interpretation_layer_present('1a5273c4-bcb7-455f-b41b-7030256663eb').
narrative_ontology:cs_reading_relation('1a5273c4-bcb7-455f-b41b-7030256663eb', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('1a5273c4-bcb7-455f-b41b-7030256663eb', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('1a5273c4-bcb7-455f-b41b-7030256663eb', foundational, proportionality_balances_mobility_and_national_interest).
narrative_ontology:cs_axiom_status(proportionality_balances_mobility_and_national_interest, holdable).
narrative_ontology:cs_axiom_grounding('1a5273c4-bcb7-455f-b41b-7030256663eb', proportionality_balances_mobility_and_national_interest, conventional).
narrative_ontology:cs_axiom('1a5273c4-bcb7-455f-b41b-7030256663eb', secondary, policy_domain_variation_legitimate).
narrative_ontology:cs_axiom_status(policy_domain_variation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1a5273c4-bcb7-455f-b41b-7030256663eb', policy_domain_variation_legitimate, instrumental).
narrative_ontology:cs_reference_frame('1a5273c4-bcb7-455f-b41b-7030256663eb', proportionality_balanced_freedom_of_movement).
narrative_ontology:cs_drift_state('1a5273c4-bcb7-455f-b41b-7030256663eb', contemporary_polycrisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a5273c4-bcb7-455f-b41b-7030256663eb', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, integrationist_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers_cross_border).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federal_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, single_market_operators).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sovereigntist_member_states_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, welfare_systems_high_migration).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, non_mobile_citizens_perceived_competition).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, third_country_nationals_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, sovereigntist_member_states).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sovereigntist_member_states).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, mobile_workers_cross_border).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_principle).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, subsidiarity_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, graduated_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Net recipients of mobile labor and capital; benefit from deepened single market integration. Use proportionality doctrine to resist blanket restrictions while accepting narrow, justified constraints. Can leverage federal institutions and Court jurisprudence to shape the balance.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, integrationist_member_states, beneficiary,
    institutional, generational, arbitrage, continental).

% Experience net outflows of labor or inflows straining welfare systems. Invoke 'legitimate national interests' to justify restrictions on free movement (labor market protection, welfare access conditions). Constrained by treaty obligations and Court oversight; cannot unilaterally exit without severe economic/political cost.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sovereigntist_member_states, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, sovereigntist_member_states, payer).

% Exercise free movement rights for work/residence but face domain-specific barriers: professional qualification recognition, social security portability gaps, posted-worker directive limits, family reunification hurdles. Benefit from baseline mobility but pay compliance costs and suffer rights gaps that vary by policy domain. Professional and family ties lock them into the federation framework.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers_cross_border, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, mobile_workers_cross_border, beneficiary).

% Commission proposes legislation; Court adjudicates proportionality; Parliament co-legislates. They define the 'bounds of proportionality' through case law and secondary law. Their authority derives from treaty mandate to police the balance. They benefit from the constraint's persistence (institutional relevance) but do not directly extract rents.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% First-instance adjudicators of proportionality in concrete cases. Apply federal Court precedent but retain margin of appreciation in fact-finding. Their decisions collectively shape the operational boundary. Caught between federal supremacy and national constitutional identity claims.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, national_courts, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, national_courts, observer).

% Bear fiscal costs of mobile EU citizens accessing non-contributory benefits, healthcare, housing support. Use 'legitimate national interest' to justify residence tests, waiting periods, genuine link requirements. Constrained by Court's case-by-case proportionality review; cannot impose blanket exclusions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, welfare_systems_high_migration, payer,
    organized, biographical, constrained, national).

% Experience labor market competition and wage pressure from mobile workers in sectors with high intra-EU mobility (construction, hospitality, logistics, care). Politically mobilize for restrictions but lack individual exit; their voice channels through sovereigntist parties. Bear diffuse costs without organized representation at federal level.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, non_mobile_citizens_perceived_competition, payer,
    powerless, biographical, trapped, local).

% Subject to federation's external border regime; free movement rights do not extend to them. Would argue for mobility rights on human dignity/labor need grounds but are structurally excluded from the treaty's citizenship framework. Their exclusion is the flip side of the internal mobility right.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, third_country_nationals_excluded, excluded,
    powerless, biographical, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a single labor market across federation members by guaranteeing baseline mobility rights, while providing a structured mechanism (proportionality test) for members to address genuine domestic concerns (labor market disruption, welfare sustainability, public service capacity) without unraveling the market.
% TRANSFER_FUNCTION: Moves regulatory authority over mobility conditions from unilateral state control to a shared proportionality framework: states transfer the power to impose blanket restrictions, gain the right to impose justified constraints; mobile workers transfer unconditional mobility, gain legally enforceable baseline rights with domain-specific conditions; federal institutions gain adjudicatory authority over the balance.
% ABSENT_VOICES: Third-country nationals excluded from the mobility right entirely; non-mobile citizens in high-migration regions who bear competitive costs without representation in federal deliberation; future generations whose federation membership terms are set by current proportionality jurisprudence. They are absent because the treaty constituency is defined by member state nationals only.
% DISAPPEARANCE_RATIONALE: If the proportionality-balanced framework vanished overnight, two regimes would compete: integrationist states would push for unrestricted mobility (single market purity); sovereigntist states would impose unilateral restrictions. The single market would fragment into bilateral agreements or a two-tier mobility system. Mobile workers would lose enforceable baseline rights; federal institutions would lose core adjudicatory function. The federation's constitutional architecture would restructure.
% FOUNDING_PROBLEM: Post-war European integration required reconciling two imperatives: (1) a single market needs free factor mobility including labor; (2) member states would not surrender control over labor market regulation, welfare systems, and national identity to a supranational authority. The Treaty of Rome (1957) and subsequent treaties embedded free movement as a right but left its limits undefined, creating the proportionality gap this constraint now structures.
% FOUNDING_PROBLEM_CORROBORATION: Integrationist states and federal institutions attest the problem is live: new domains (digital labor platforms, posted workers, social security coordination) constantly generate fresh proportionality disputes. Sovereigntist states and national courts attest the founding problem is substantially solved for core domains (worker mobility, establishment) and the constraint now serves as a vehicle for federal mission creep. Academic federalism scholars (outside beneficiary sets) corroborate both: the original labor-market-integration problem is largely solved, but new governance problems (asymmetric shocks, demographic divergence) have emerged that the proportionality framework was not designed to address.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the constraint transfers regulatory authority and compliance costs asymmetrically across domains — it is not pure coordination. Suppression (0.55) is moderate: the constraint actively blocks both 'no restrictions ever' and 'any restriction allowed' positions through Court enforcement, but alternatives (derogations, safeguard clauses, enhanced cooperation) exist. Theater ratio (0.28) is low-moderate: the proportionality test is legally real and bites in case law, but a growing share of litigation serves to entrench federal judicial authority rather than resolve concrete mobility disputes. Accessibility collapse (0.48) is partial: states retain policy space within proportionality bounds; mobile workers retain core rights with domain-specific conditions. Resistance (0.52) is significant from both poles: integrationists resist any restriction as market fragmentation; sovereigntists resist proportionality review as sovereignty erosion.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seat types: from federal_institutions seat, the constraint appears as rope (coordination with minimal extraction); from mobile_workers and non_mobile_citizens seats, it computes as snare (extraction with constrained exit); from sovereigntist_states seat, it computes as tangled_rope (genuine coordination function they accept, plus extraction they contest). The claimed_type (tangled_rope) reflects the structural reality that all three experiences coexist — the constraint IS different things from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and federal institutions sit near beneficiary end (d ~0.15-0.25): they collect regulatory authority and single-market deepening. Sovereigntist states are dual-positioned: beneficiaries when invoking national interests (d ~0.35 for their protective measures), payers when constrained by Court (d ~0.65 for compliance costs). Mobile workers are identity-locked payers (d ~0.7): they bear compliance costs across domains, professional/family ties prevent exit, baseline rights are real but incomplete. Welfare systems and non-mobile citizens are trapped payers (d ~0.8-0.9): they bear diffuse costs with no federal representation. Third-country nationals are excluded (d not computed): the constraint's internal balance presupposes their external exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling single market with state sovereignty over labor/welfare) is contested: largely solved for 1957-era labor mobility, but the proportionality framework now governs domains (digital platforms, climate migration, demographic aging) it was not designed for. This mandatrophy — the constraint persisting beyond its original justification — is managed through doctrinal expansion (proportionality absorbing new fields) rather than treaty revision. The constraint is not a piton (theater without function) because the proportionality test still resolves live disputes; but it is not a pure rope because the beneficiary/victim asymmetry is structural and domain-varying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where does the ''subsidiarity_balance'' reading end and the ''integration_primary'' or ''sovereignty_primary'' readings begin? Is proportionality a distinct structural position or a continuum?',
    'Analyze Court case law clustering: do proportionality judgments form a coherent doctrinal category distinct from strict scrutiny (integration_primary) and margin-of-appreciation deference (sovereignty_primary)? Measure citation networks and outcome patterns.',
    'If proportionality is not a distinct reading but a continuum, this constraint story artificially reifies a gradient. The ε-invariance principle would require decomposing into domain-specific constraints rather than a single balance constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the three declared readings are structurally discrete or points on a continuum.').

omega_variable(
    domain_variation_extraction,
    'Does the beneficiary/victim variation by policy domain reflect genuine coordination differentiation, or does it mask a systematic extraction pattern (e.g., core states consistently benefit, periphery consistently pay)?',
    'Cross-domain extraction accounting: for each policy domain (professional qualifications, posted workers, social security coordination, family reunification, jobseeker access), map net benefit flows between state groups. Test for systematic core-periphery pattern.',
    'If systematic core-periphery extraction exists beneath domain variation, the constraint is a snare with coordination cover, not a tangled_rope. The claimed balance would be a false summit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_variation_extraction, empirical, 'Whether domain-varying beneficiaries/victims sum to systematic asymmetric extraction.').

omega_variable(
    proportionality_test_performativity,
    'Is the Court''s proportionality test (suitability, necessity, balancing) a genuine constraint on state power, or has it become a ritual that ratifies whatever restriction states propose while appearing to review?',
    'Longitudinal analysis of Court outcomes: proportion of state restrictions struck down vs. upheld over time; comparison of reasoning depth in upheld vs. struck cases; amicus participation patterns.',
    'If performative, theater_ratio is understated and the constraint drifts toward piton (theatrical maintenance of federal authority). If genuine, theater_ratio is accurate and the constraint remains a functioning tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_performativity, empirical, 'Whether proportionality adjudication is substantive review or theatrical ratification.').

omega_variable(
    mobile_worker_identity_lock_mechanism,
    'What specific identity-fusion mechanism binds mobile workers to the federation framework despite domain-specific rights gaps? Professional identity? Family relocation sunk costs? European citizenship identity?',
    'Survey/experimental work with mobile worker cohorts: measure willingness to accept rights restrictions vs. exit (return home, move to third country) under counterfactual scenarios. Decompose identity_locked into professional, relational, ideological, institutional components.',
    'If identity_locked is primarily professional/relational (sunk costs), exit_options could shift to ''constrained'' with policy changes (portable pensions, qualification recognition). If ideological (European citizenship), exit_options remains identity_locked regardless of policy. Changes directionality derivation for mobile_worker seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mobile_worker_identity_lock_mechanism, empirical, 'Decomposition of the identity_locked exit option for mobile workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_sub_bal_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fmt_sub_bal_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fmt_sub_bal_tr_t20, federation_membership_treaty__subsidiarity_balance, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fmt_sub_bal_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fmt_sub_bal_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.22).
narrative_ontology:measurement(fmt_sub_bal_tr_t50, federation_membership_treaty__subsidiarity_balance, theater_ratio, 50, 0.25).
narrative_ontology:measurement(fmt_sub_bal_tr_t60, federation_membership_treaty__subsidiarity_balance, theater_ratio, 60, 0.27).
narrative_ontology:measurement(fmt_sub_bal_tr_t70, federation_membership_treaty__subsidiarity_balance, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(fmt_sub_bal_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fmt_sub_bal_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(fmt_sub_bal_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(fmt_sub_bal_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(fmt_sub_bal_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(fmt_sub_bal_be_t50, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(fmt_sub_bal_be_t60, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(fmt_sub_bal_be_t70, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fmt_sub_bal_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fmt_sub_bal_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(fmt_sub_bal_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(fmt_sub_bal_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(fmt_sub_bal_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(fmt_sub_bal_su_t50, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(fmt_sub_bal_su_t60, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 60, 0.54).
narrative_ontology:measurement(fmt_sub_bal_su_t70, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, single_market_regulatory_framework).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, asylum_migration_management_regulation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, european_social_security_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_treaty kernel. The integration_primary reading (free movement as constitutive, restrictions presumptively illegitimate) and sovereignty_primary reading (free movement conditional on state consent) are sibling constraints. All three share the same treaty text but instantiate different constraints with different ε, beneficiaries, victims, and types. This reading claims the proportionality balance; the others claim priority of integration or sovereignty respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, institutional, 0.2).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, organized, 0.55).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, moderate, 0.7).
constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
