% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Tiered Free Movement Rights by Contribution History
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'selective_solidarity' reading of the
 *   contested kernel 'federation_membership_obligations'. The kernel concerns
 *   what obligations federation membership imposes on member states regarding
 *   mobile citizens' welfare access. This reading asserts that free movement
 *   rights are tiered by contribution history and economic activity status,
 *   with welfare access following a contributory principle rather than a
 *   citizenship principle. Mobile workers are bifurcated: employed workers
 *   retain full rights (coordination beneficiaries), while economically
 *   inactive workers face restricted access (extraction targets). The
 *   arrangement is actively enforced through Directive 2004/38, CJEU case law
 *   (Dano, Alimanovic), and national implementing legislation. The constraint
 *   presents as coordination (single market labor mobility) but operates with
 *   asymmetric extraction (cost-shifting to vulnerable mobile workers and
 *   net-recipient states).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Tiered Free Movement Rights by Contribution History").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '9ac7f844-e577-4ea8-9037-0e82374297ca').
narrative_ontology:cs_kernel_codification('9ac7f844-e577-4ea8-9037-0e82374297ca', formalized).
narrative_ontology:cs_authority_grounding('9ac7f844-e577-4ea8-9037-0e82374297ca', extraction).
narrative_ontology:cs_interpretation_layer_present('9ac7f844-e577-4ea8-9037-0e82374297ca').
narrative_ontology:cs_reading_relation('9ac7f844-e577-4ea8-9037-0e82374297ca', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('9ac7f844-e577-4ea8-9037-0e82374297ca', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('9ac7f844-e577-4ea8-9037-0e82374297ca', foundational, contributory_principle_over_citizenship_principle).
narrative_ontology:cs_axiom_status(contributory_principle_over_citizenship_principle, holdable).
narrative_ontology:cs_axiom_grounding('9ac7f844-e577-4ea8-9037-0e82374297ca', contributory_principle_over_citizenship_principle, empirically_contingent).
narrative_ontology:cs_axiom('9ac7f844-e577-4ea8-9037-0e82374297ca', secondary, labor_market_link_as_welfare_gateway).
narrative_ontology:cs_axiom_status(labor_market_link_as_welfare_gateway, holdable).
narrative_ontology:cs_axiom_grounding('9ac7f844-e577-4ea8-9037-0e82374297ca', labor_market_link_as_welfare_gateway, instrumental).
narrative_ontology:cs_reference_frame('9ac7f844-e577-4ea8-9037-0e82374297ca', post_maastricht_citizenship_equality).
narrative_ontology:cs_drift_state('9ac7f844-e577-4ea8-9037-0e82374297ca', post_dano_alimanovic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ac7f844-e577-4ea8-9037-0e82374297ca', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, eu_institutions).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, net_recipient_member_states).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_principle_primacy).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, labor_market_integration_as_citizenship_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers who move between member states and maintain continuous employment. They enjoy full free movement rights, unrestricted welfare access based on contributions, and can leverage labor mobility for career advancement. Their exit option is moving to another member state with better conditions — they are not locked into any single national system.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    organized, biographical, mobile, continental).

% Mobile workers who become unemployed, are in precarious work, or are outside the labor market (students, caregivers, retirees, job-seekers). Their free movement rights are restricted after initial periods — welfare access requires proof of genuine link to labor market, contribution history, or sufficient resources. They cannot easily exit the constraint because returning to origin state may mean worse conditions, and onward movement restarts waiting periods.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers, payer,
    powerless, immediate, constrained, continental).

% Wealthier member states (e.g., Germany, Netherlands, Nordic states) that receive net fiscal contributions from mobile workers. They set the agenda for tiered rights through Council negotiations and Court litigation, framing restrictions as protection of welfare sustainability. They benefit from selective inflow of productive workers while limiting welfare tourism. Their exit option is political — they could veto deeper integration but choose to shape rules instead.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributor_member_states, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, net_contributor_member_states, beneficiary).

% Less wealthy member states (e.g., Eastern and Southern EU) that send workers abroad and receive remittances but bear costs when workers return unemployed or when their own welfare systems must cover gaps. They resist restrictions on their citizens' rights but lack voting power to block them. Their exit is constrained — leaving the single market is economically prohibitive, but they cannot unilaterally change the tiered structure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_recipient_member_states, payer,
    moderate, biographical, constrained, continental).

% Commission, Court (CJEU), and Parliament that administer and adjudicate the tiered system. The Court's case law (Dano, Alimanovic, Commission v UK) established the contributory principle as limiting factor. They benefit from institutional authority to define 'genuine link' and 'unreasonable burden'. Their analytical seat means they observe the system's operation but are also its architects.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Citizens who do not exercise free movement but whose welfare contributions fund the systems that mobile workers access. They experience the constraint indirectly through political discourse on 'welfare tourism' and 'fairness'. Their analytical position is shaped by national media and party narratives rather than direct exposure to tiered rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, non_mobile_citizens, observer,
    organized, generational, analytical, national).

% Non-EU migrants subject to entirely separate, more restrictive regimes. They are structurally excluded from the free movement framework entirely — their situation shows the boundary of the contributory principle (which applies only to those already inside the mobility right). They would object to the two-tier citizenship but have no voice in EU legislative process.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, third_country_nationals, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor mobility across a heterogeneous federation by establishing a common framework for which mobile workers qualify for host-state welfare — preventing a race to the bottom where states compete to exclude, while enabling workers to move for employment without losing social protection.
% TRANSFER_FUNCTION: Transfers welfare cost risk from host member states to origin states and to mobile workers themselves: economically inactive mobile workers lose host-state welfare access after 3-6 months, shifting costs to their last state of employment or to their personal resources. Employed mobile workers' contributions subsidize the coordination infrastructure (portability, aggregation).
% ABSENT_VOICES: Third-country nationals permanently excluded from the free movement framework; economically inactive mobile workers who cannot organize across borders due to language barriers, precarious status, and short time horizons; civil society organizations advocating for universal residence-based welfare (largely excluded from Council working groups where tiered rules are negotiated).
% DISAPPEARANCE_RATIONALE: If tiered rights vanished overnight, either (a) host states would unilaterally restrict all mobile worker welfare access (returning to pre-1990s fragmentation), or (b) the Court would impose uniform citizenship-based access, triggering fiscal backlash and potential member state exits. The tiered system is the negotiated equilibrium that keeps the single market open for workers while containing fiscal exposure.
% FOUNDING_PROBLEM: The 1990s-2000s expansion of free movement jurisprudence (Martinez Sala, Grzelczyk, Bidar) extended equal treatment to economically inactive EU citizens, creating open-ended welfare liability for host states. The 2004/38 Directive and subsequent Court rulings (Dano, Alimanovic) were the response: re-anchoring welfare access in contribution history and labor market link to prevent 'benefit tourism' while preserving worker mobility.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and net-contributor states attest the problem is live — fiscal sustainability requires contributory limits. Net-recipient states, European Parliament social committees, and academic experts (e.g., Frans Pennings, Elspeth Guild) attest the problem is substantially solved — actual 'welfare tourism' is negligible, and restrictions disproportionately harm vulnerable mobile workers. No independent fiscal audit has quantified the net cross-border welfare flow attributable to economically inactive mobility.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the tiered system shifts welfare costs from host states to economically inactive mobile workers (who lose access after 3-6 months) and to net-recipient states (whose returning workers bear costs). The constraint is not pure extraction — employed mobile workers genuinely benefit from portability and aggregation of contributions — hence tangled_rope not snare. Suppression (0.58) is moderate: restrictions are legally enforced but alternatives exist (return to origin, onward movement, private insurance). Theater ratio (0.38) is significant: the 'welfare tourism' narrative justifies restrictions that affect few actual cases (studies show <1% of mobile citizens are economically inactive in host state). The measurement series on a shared grid shows extractiveness and suppression rising together from 1990-2024 as Court jurisprudence tightened.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (economically inactive mobile workers) experiences the constraint as a snare: coordination cover for extraction. The beneficiary seat (employed mobile workers) experiences it as a rope: genuine coordination they rely on. The agenda-setter seats (net-contributor states, EU institutions) experience it as scaffold: a transitional arrangement they hope to normalize. The engine will compute these divergences from the structural data — the claimed tangled_rope is the author's structural assessment, not a reconciliation of seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Employed mobile workers are structural beneficiaries (d ~0.2): they gain portability, aggregation, and equal treatment while employed. Economically inactive mobile workers are structural targets (d ~0.85): they bear the cost of restrictions, have constrained exit, and the contributory principle operates against them. Net-contributor states are agenda-setters with beneficiary position (d ~0.15): they shape rules and receive net fiscal gains. Net-recipient states are payers with constrained exit (d ~0.7): they bear costs of returning workers but cannot leave the system. EU institutions sit near analytical (d ~0.5) but with agenda-setter power — they administer the system they created. Third-country nationals are excluded entirely (trapped, d ~0.95) — the constraint's boundary defines them out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (open-ended welfare liability from 1990s jurisprudence) is contested as live vs. solved. If dead, the tiered system is mandatrophic — a response to a past crisis now maintained as rent-extraction from vulnerable workers. If live, it remains a genuine coordination mechanism. The corroboration split (Commission vs. Parliament/social experts) means the engine's mismatch detection (founding_problem_status=contested + disappearance_verdict=world_rearranges) will flag this for investigation without pre-judging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the selective_solidarity reading represent a distinct constraint from integration_primary and member_sovereignty_primary, or is it an unstable compromise that collapses into one sibling under pressure?',
    'Track CJEU case law trajectory: if Court consistently applies contributory principle as limiting factor (not exception), reading is distinct. If Court swings to either universal equal treatment (integration_primary) or broad member state discretion (member_sovereignty_primary), the reading collapses.',
    'If distinct, three-constraint family with separate ε values. If collapse, selective_solidarity is a transient interpretive position, not a structural constraint — its ε would be measurement artifact of transition period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether selective_solidarity is a stable kernel reading or transitional interpretive position').

omega_variable(
    fiscal_magnitude_of_tiering,
    'What is the actual fiscal magnitude of cross-border welfare flows from economically inactive mobile workers, relative to total welfare spending?',
    'Independent fiscal audit of member state data on non-contributory benefit claims by mobile EU citizens, disaggregated by duration of residence, prior contribution history, and reason for inactivity.',
    'If magnitude is negligible (<0.1% of welfare spending), the tiered system''s extractiveness is performative — theater_ratio understates the gap between justification and reality. If substantial, the contributory principle addresses a real fiscal coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_magnitude_of_tiering, empirical, 'Whether the extraction measured by the constraint corresponds to a real fiscal problem or a political narrative').

omega_variable(
    identity_lock_of_economically_inactive,
    'Are economically inactive mobile workers identity-locked to the federation (cannot conceive exit because EU citizenship is constitutive of their self-understanding) or merely constrained (exit is costly but imaginable)?',
    'Longitudinal survey of mobile workers who lose welfare access: do they pursue return to origin, onward movement, or political mobilization for rights restoration? Identity-locked agents mobilize; constrained agents exit.',
    'If identity-locked, their directionality d approaches 1.0 (full target) — the constraint extracts from agents who cannot structurally exit. If merely constrained, d ~0.7 — extraction is high but exit remains a real option.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_of_economically_inactive, empirical, 'Whether the primary extraction target is identity-locked to the federation or merely exit-constrained').

omega_variable(
    contributory_principle_as_extraction_cover,
    'Is the contributory principle a genuine coordination mechanism (solving adverse selection in welfare access) or a cover story for extracting from economically inactive mobile workers who are disproportionately from net-recipient states?',
    'Compare welfare access rules for economically inactive mobile workers vs. economically inactive non-mobile citizens in same host state. If rules are identical, contributory principle is genuine coordination. If mobile workers face stricter tests, it is targeted extraction.',
    'If cover story, the constraint is snare not tangled_rope — coordination function is pretext. If genuine, tangled_rope classification holds with asymmetric extraction as side-effect of coordination design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributory_principle_as_extraction_cover, conceptual, 'Whether the coordination function is genuine or pretextual').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership_obligations__selective_solidarity, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__selective_solidarity, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__selective_solidarity, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__selective_solidarity, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(fede_tr_t2014, federation_membership_obligations__selective_solidarity, theater_ratio, 2014, 0.33).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__selective_solidarity, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__selective_solidarity, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership_obligations__selective_solidarity, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__selective_solidarity, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__selective_solidarity, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__selective_solidarity, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(fede_be_t2014, federation_membership_obligations__selective_solidarity, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__selective_solidarity, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__selective_solidarity, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership_obligations__selective_solidarity, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__selective_solidarity, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__selective_solidarity, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__selective_solidarity, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(fede_su_t2014, federation_membership_obligations__selective_solidarity, suppression_requirement, 2014, 0.53).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__selective_solidarity, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__selective_solidarity, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, eu_social_security_coordination_regulation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, directive_2004_38_free_movement).

% DUAL FORMULATION NOTE:
% This constraint (selective_solidarity) is one of three readings of kernel 'federation_membership_obligations'. The ε values differ: integration_primary has low extractiveness (near 0.15, Mountain-like for workers), member_sovereignty_primary has high extractiveness (0.75+, Snare for mobile workers), selective_solidarity sits at 0.62 (Tangled Rope). The three stories form a constraint family linked by affects_constraints. The decomposition follows the BGS pattern: the kernel label 'free movement welfare obligations' conflates three structurally distinct claims with different ε, different victim/beneficiary structures, and different Court lineages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
