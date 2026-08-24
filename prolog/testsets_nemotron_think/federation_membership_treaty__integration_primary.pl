% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The integration-primary reading of the federation membership treaty holds
 *   that free movement of workers is a constitutive element of the single
 *   market, not a conditional privilege. Restrictions by member states are
 *   presumptively illegitimate and permitted only under narrow, strictly
 *   interpreted justifications (public policy, public security, public
 *   health). This reading has driven the Court of Justice's expansive case
 *   law (e.g., Bosman, Laval, Viking) and the Commission's enforcement
 *   strategy. The structural delta identifies mobile workers, capital owners,
 *   and EU institutions as beneficiaries; local labor markets, national
 *   welfare systems, and displaced native workers as bearing the costs; and
 *   national restriction attempts as the target of high suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.75).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '4f76039d-e9b1-4747-b3f2-40c8e287d2a8').
narrative_ontology:cs_kernel_codification('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', formalized).
narrative_ontology:cs_authority_grounding('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', lineage).
narrative_ontology:cs_interpretation_layer_present('4f76039d-e9b1-4747-b3f2-40c8e287d2a8').
narrative_ontology:cs_reading_relation('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', foundational, free_movement_constitutive_of_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_single_market, holdable).
narrative_ontology:cs_axiom_grounding('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', free_movement_constitutive_of_single_market, conventional).
narrative_ontology:cs_axiom('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', foundational, restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', restrictions_presumptively_illegitimate, conventional).
narrative_ontology:cs_reference_frame('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', single_market_integration_framework).
narrative_ontology:cs_drift_state('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', contemporary_enlargement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4f76039d-e9b1-4747-b3f2-40c8e287d2a8', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, capital_owners_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, single_market_operators).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, displaced_native_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, single_market_integration_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, four_freedoms_constitutional_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers who exercise free movement rights across member states. They gain access to wider labor markets, higher wages, and career mobility. Their exit option is strong — they can move to where opportunities exist — but they depend on the constraint's enforcement to keep borders open and rights portable.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Firms and capital owners who benefit from a larger, more flexible labor pool. They lobby for expansive free movement rules and against national restrictions. They can relocate capital or restructure supply chains if restrictions tighten, giving them arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, capital_owners_employers, beneficiary,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, capital_owners_employers, agenda_setter).

% EU-level institutions (Commission, Court of Justice, Parliament) that administer and enforce free movement rules. They set the agenda for integration, initiate infringement procedures against restrictive member states, and interpret the scope of justifications. They do not personally collect rents but their institutional mandate expands with integration.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, single_market_operators, agenda_setter,
    institutional, generational, analytical, continental).

% Subnational labor markets (regions, sectors) that absorb incoming workers. They experience wage pressure, displacement effects, and adjustment costs. They cannot easily exit the single market; their political representatives can seek derogations but face high legal and political barriers.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    organized, biographical, constrained, regional).

% National social protection systems (healthcare, unemployment, pensions) that must extend coverage to mobile workers under non-discrimination rules. They bear fiscal costs without full control over who enters. Exit would require treaty change or withdrawal — politically prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Workers in regions/sectors with high inflow of mobile labor who face direct competition, wage stagnation, or job loss. They lack mobility (skills, family ties, language) and political voice. Their exit options are minimal — they cannot easily move or change sectors.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, displaced_native_workers, payer,
    powerless, biographical, trapped, local).

% National governments that must transpose and enforce free movement directives. They face domestic political pressure to restrict but face infringement proceedings if they do. They set agendas in the Council but are constrained by qualified majority voting and Court rulings.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter).

% National and EU competition authorities that monitor whether free movement rules are applied proportionally and whether restrictions constitute disguised protectionism. They provide analytical input but do not set the integration agenda.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, competition_authorities, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single labor market across member states, allowing workers to move to where their skills are most valued and firms to access a continental talent pool, eliminating the fragmentation of national labor markets.
% TRANSFER_FUNCTION: Moves labor market access and welfare portability from national control to EU-level guarantee, transferring adjustment costs (wage pressure, fiscal burden, displacement) to local labor markets and national welfare systems, while transferring gains (mobility, arbitrage, scale) to mobile workers and capital owners.
% ABSENT_VOICES: Third-country nationals excluded from free movement rights; future generations who inherit the fiscal and demographic consequences; regions experiencing depopulation (brain drain) rather than immigration pressure — they are not represented in the Council or Parliament in proportion to their stake.
% DISAPPEARANCE_RATIONALE: If free movement as a constitutive right vanished overnight, national borders would reassert control over labor access, welfare systems would revert to national-only coverage, wage differentials would widen, and the single market would fragment into national markets with bilateral agreements. The EU's legal order would lose a pillar.
% FOUNDING_PROBLEM: Post-war European integration required a single market to lock in peace and prosperity; fragmented national labor markets were seen as barriers to economic efficiency and political convergence. Free movement was the mechanism to make the single market real for people, not just goods and capital.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and Parliament attest the problem is live (integration incomplete, barriers persist). Member state governments (especially post-2004 entrants) attest the founding problem is substantially solved and current pressures reflect new problems (wage divergence, welfare tourism) not contemplated originally. Academic literature (e.g., Scharpf, Streeck) corroborates the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the constraint transfers substantial adjustment costs to local systems while concentrating mobility gains on mobile workers and capital. Suppression (0.75) is high because the constraint's persistence depends on active legal enforcement — infringement proceedings, Court rulings striking down national measures, political pressure on governments — not voluntary compliance. Theater ratio (0.35) is moderate: the 'proportionality test' and 'genuine link' doctrines perform a justification ritual, but the Court's case law consistently narrows the space for national restrictions. Accessibility collapse (0.65) is significant because once a national restriction is challenged, the legal framework makes alternatives (derogations, safeguards) extremely difficult to sustain. Resistance (0.60) is substantial: member states regularly seek opt-outs, safeguard clauses, and political exemptions, but face high legal and reputational costs.
 *
 * PERSPECTIVAL GAP:
 *   From the EU institutions' seat (agenda_setter, analytical exit), the constraint is genuine coordination — the single market cannot function without free movement. From mobile workers' seat (beneficiary, mobile exit), it is a right that enables life projects. From local labor markets and welfare systems (payer, constrained exit), it is an imposed cost with no exit. From displaced native workers (payer, trapped exit), it is extraction without voice. The engine computes these divergent per-seat types from the structural data; the authored claim (tangled_rope) reflects the authoring seat's judgment that both coordination and asymmetric extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: mobile_workers (direct mobility gains), capital_owners_employers (labor arbitrage, scale), single_market_operators (institutional mandate expansion). Victims declared: local_labor_markets (wage pressure, adjustment), national_welfare_systems (fiscal externalities), displaced_native_workers (direct competition, no exit). Member_state_governments are dual: they administer the constraint (agenda_setter in Council) but bear political costs and face infringement risk (payer). The derivation chain assigns low d to beneficiaries, high d to victims, intermediate d to dual-positioned governments. No overrides needed — structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war market fragmentation) is contested as live vs. solved. The constraint shows extraction accumulation (base_extractiveness rising from 0.15 to 0.65) and enforcement intensification (suppression from 0.20 to 0.75) as the single market deepened and enlarged eastward. This is not pure mandatrophy — the coordination function remains real — but the extraction-to-coordination ratio has shifted. The classification as tangled_rope (not snare) preserves the coordination function while flagging the asymmetric extraction. A snare classification would deny the coordination reality; a rope classification would deny the extraction reality. Tangled rope is the honest structural description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the integration_primary reading logically foreclose the sovereignty_primary reading within a single constitutional framework, or do they coexist as competing principles in the EU treaties?',
    'Legal analysis of whether the treaties'' text and Court jurisprudence establish a hierarchy (free movement supreme) or a genuine balance (proportionality as open-ended). Empirical test: count how often national restrictions survive Court review vs. are struck down.',
    'If forecloses, the kernel has a structural contradiction that the Court resolves by hierarchy. If coexists_with, the kernel contains an irreducible tension that generates ongoing political conflict. If influences, integration_primary creates path dependency that makes sovereignty_primary harder to sustain over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between integration_primary and sovereignty_primary readings of the same kernel.').

omega_variable(
    extraction_coordination_boundary,
    'How much of the measured extractiveness (0.65) is the necessary cost of coordinating a continental labor market, and how much is asymmetric rent extraction by mobile workers/capital at the expense of stationary populations?',
    'Counterfactual modeling: simulate a single market with portable rights but national control over inflows (e.g., points-based systems). Compare welfare outcomes. Or: estimate the marginal cost of cross-border coordination (portability admin, recognition) vs. the transfer magnitude.',
    'If most extraction is coordination cost, the constraint leans toward rope. If most is asymmetric transfer, it leans toward snare. The tangled_rope claim asserts both are substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Whether the constraint''s extraction is separable into coordination overhead vs. asymmetric transfer.').

omega_variable(
    displaced_worker_coalition_potential,
    'Can displaced_native_workers (powerless, trapped) form effective coalitions with member_state_governments (institutional, constrained) to shift the constraint toward subsidiarity_balance, or does the institutional architecture block this?',
    'Political economy analysis of electoral cleavages, party systems, and Council voting dynamics. Track whether anti-free-movement parties gain influence and whether they translate that into treaty change or derogations.',
    'If coalitions form and succeed, the constraint may drift toward subsidiarity_balance or sovereignty_primary. If blocked, extraction accumulates and resistance radicalizes (exit from EU).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_coalition_potential, empirical, 'Political feasibility of victim-side coalition to renegotiate the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_integration_primary_tr_t1957, federation_membership_treaty__integration_primary, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(fmt_integration_primary_tr_t1973, federation_membership_treaty__integration_primary, theater_ratio, 1973, 0.12).
narrative_ontology:measurement(fmt_integration_primary_tr_t1986, federation_membership_treaty__integration_primary, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(fmt_integration_primary_tr_t1992, federation_membership_treaty__integration_primary, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(fmt_integration_primary_tr_t2004, federation_membership_treaty__integration_primary, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(fmt_integration_primary_tr_t2015, federation_membership_treaty__integration_primary, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(fmt_integration_primary_tr_t2024, federation_membership_treaty__integration_primary, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(fmt_integration_primary_be_t1957, federation_membership_treaty__integration_primary, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement(fmt_integration_primary_be_t1973, federation_membership_treaty__integration_primary, base_extractiveness, 1973, 0.2).
narrative_ontology:measurement(fmt_integration_primary_be_t1986, federation_membership_treaty__integration_primary, base_extractiveness, 1986, 0.28).
narrative_ontology:measurement(fmt_integration_primary_be_t1992, federation_membership_treaty__integration_primary, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(fmt_integration_primary_be_t2004, federation_membership_treaty__integration_primary, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fmt_integration_primary_be_t2015, federation_membership_treaty__integration_primary, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(fmt_integration_primary_be_t2024, federation_membership_treaty__integration_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fmt_integration_primary_su_t1957, federation_membership_treaty__integration_primary, suppression_requirement, 1957, 0.2).
narrative_ontology:measurement(fmt_integration_primary_su_t1973, federation_membership_treaty__integration_primary, suppression_requirement, 1973, 0.25).
narrative_ontology:measurement(fmt_integration_primary_su_t1986, federation_membership_treaty__integration_primary, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fmt_integration_primary_su_t1992, federation_membership_treaty__integration_primary, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(fmt_integration_primary_su_t2004, federation_membership_treaty__integration_primary, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement(fmt_integration_primary_su_t2015, federation_membership_treaty__integration_primary, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(fmt_integration_primary_su_t2024, federation_membership_treaty__integration_primary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint (integration_primary) is one of three readings of the federation_membership_treaty kernel. The sovereignty_primary reading treats state authority as primary and free movement as conditional; the subsidiarity_balance reading treats proportionality as the mediating principle. All three share the same treaty text but instantiate different constraints with different beneficiary/victim structures and extractiveness values. This reading has the highest extractiveness (0.65) and suppression (0.75) because it minimizes the space for national justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
