% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member-State Closure Authority over Federated Welfare Access
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Under this reading, the standing arrangement is: national welfare states
 *   retain authority to close their solidarity systems, and free movement is
 *   lawful but conditional — on worker or self-sufficient status, on not
 *   becoming an unreasonable burden, on passing residence and
 *   habitual-residence tests that member state legislatures write and revise
 *   at will. Mobile workers pay into host systems from day one but draw
 *   non-contributory support only behind the gate; the economically inactive
 *   are largely outside it altogether; even returning co-nationals pass
 *   through the same tests. The arrangement is actively enforced (residence
 *   verification, denial litigation, expulsion procedure) and actively
 *   defended (safeguard clauses, emergency-brake demands, benefit-tourism
 *   campaigns). KEY AGENTS (by structural relationship): -
 *   member_state_legislatures: Agenda-setter (institutional/constrained) —
 *   writes and vets the gate - receiving_state_welfare_systems: Primary
 *   beneficiary and day-to-day administrator (institutional/constrained) —
 *   collects contributions, defers liability - domestic_labor_forces and
 *   static_resident_contributors: Protected beneficiaries
 *   (organized/constrained) - intra_eu_mobile_workers: Primary target
 *   (moderate/constrained) — contributes immediately, draws conditionally -
 *   economically_inactive_movers: Sharpest target (powerless/trapped) —
 *   excluded outright - returning_nationals: Secondary target
 *   (moderate/constrained) — the gate applied to co-nationals -
 *   sending_state_communities: Excluded party (powerless/trapped) — bears the
 *   flow's other end with no seat - eu_commission_mobility_portfolio:
 *   Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.57).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.55).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.57).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member-State Closure Authority over Federated Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '92efa340-6865-4f4e-83d1-80abcd99b8f9').
narrative_ontology:cs_kernel_codification('92efa340-6865-4f4e-83d1-80abcd99b8f9', fixed_text).
narrative_ontology:cs_authority_grounding('92efa340-6865-4f4e-83d1-80abcd99b8f9', lineage).
narrative_ontology:cs_interpretation_layer_present('92efa340-6865-4f4e-83d1-80abcd99b8f9').
narrative_ontology:cs_reading_relation('92efa340-6865-4f4e-83d1-80abcd99b8f9', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('92efa340-6865-4f4e-83d1-80abcd99b8f9', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('92efa340-6865-4f4e-83d1-80abcd99b8f9', foundational, national_demoi_retain_welfare_closure_authority).
narrative_ontology:cs_axiom_status(national_demoi_retain_welfare_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('92efa340-6865-4f4e-83d1-80abcd99b8f9', national_demoi_retain_welfare_closure_authority, deontological).
narrative_ontology:cs_axiom('92efa340-6865-4f4e-83d1-80abcd99b8f9', foundational, solidarity_requires_bounded_reciprocity).
narrative_ontology:cs_axiom_status(solidarity_requires_bounded_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('92efa340-6865-4f4e-83d1-80abcd99b8f9', solidarity_requires_bounded_reciprocity, instrumental).
narrative_ontology:cs_reference_frame('92efa340-6865-4f4e-83d1-80abcd99b8f9', conferral_settlement_welfare_reserve).
narrative_ontology:cs_drift_state('92efa340-6865-4f4e-83d1-80abcd99b8f9', contemporary_citizenship_jurisprudence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('92efa340-6865-4f4e-83d1-80abcd99b8f9', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, static_resident_contributors).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, intra_eu_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, economically_inactive_movers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, returning_nationals).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, subsidiarity_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, bounded_reciprocity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the residence conditions, habitual-residence tests, and safeguard clauses that gate welfare access for arriving citizens of other member states. Retain formal veto authority over any extension of welfare access and exercise it through national implementing legislation. Bound by the treaty frame they cannot unilaterally leave, but they control the national gate and answer electorally to static constituencies.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Collect taxes and contributions from mobile workers from their first day of host-country employment while administering the residence and activity tests that defer or deny those same workers' access to non-contributory support. Their budgets are shielded from open-ended liability to newcomers; they bear the administrative cost of status verification and of defending denials in court.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_systems, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_systems, agenda_setter).

% Work inside the protected national labor market and perceive closure authority as a shield against wage-scale competition at the bottom of the market. Their electoral weight is the political engine of closure politics. They also draw on welfare systems partly financed by the contributions of the very movers the gate conditions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Long-term contributors whose accumulated benefit claims are insulated from dilution by newcomers. They pay nothing extra for the gate and support it as insurance on their own claims; their exposure to the arrangement is indirect and almost entirely positive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, static_resident_contributors, beneficiary,
    organized, biographical, constrained, national).

% Exercise free movement, pay host taxes and social contributions immediately, and accrue equal-treatment rights as workers, but encounter residence-condition and activity gates whenever they seek non-contributory support. Relocating to another member state does not exit the arrangement, since comparable conditionality applies at every destination, and returning home forfeits the accrued position, networks, and pension credits built abroad. Exit exists but at the price of the life already constructed.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, intra_eu_mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Moved without a job offer, or lost work after arriving, and are excluded from non-contributory benefits absent worker or self-sufficient status. They subsist on family transfers, charity, or savings while job-searching, face expulsion procedures if deemed an unreasonable burden, and have the fewest resources to absorb shocks or litigate. Their fiscal footprint is on average negligible, which does not alter their legal position.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, economically_inactive_movers, payer,
    powerless, immediate, trapped, continental).

% Co-nationals coming home after years of work abroad who find themselves subjected to the same right-to-reside and habitual-residence tests designed for foreigners. The closure logic bites its own members: their contributions were paid partly into foreign systems, and their re-entry is administratively suspect. Small in number and weakly organized, they rarely appear in the political conversation about the gate.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, returning_nationals, payer,
    moderate, biographical, constrained, national).

% Communities in lower-wage member states that lose working-age members to richer regions, bearing training costs, remittance dependence, and demographic strain. They have no seat in the receiving state's welfare-access decisions and would argue for portable contribution rights or compensating transfers; the gate is set entirely on the receiving side of the flow.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, sending_state_communities, excluded,
    powerless, generational, trapped, national).

% Polices member-state compliance with free-movement law, brings infringement actions against disproportionate residence tests, and publishes the mobility and fiscal statistics the debate runs on. Mediates continuously between its market-making mandate and member-state political sensitivity; adjudicates nothing finally but shapes what counts as a lawful gate.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_commission_mobility_portfolio, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, receiving_state_welfare_systems).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the political and fiscal viability of combining large-scale free movement with nationally organized, contribution-financed welfare: each national community keeps a gate on who draws its solidarity pool, containing fiscal externalities between jurisdictions and sustaining the domestic tax bargain that funds redistribution.
% TRANSFER_FUNCTION: Moves people and labor freely across member-state borders while moving welfare risk asymmetrically: mobile workers' taxes and contributions flow to host systems immediately, while their eligibility for non-contributory support is deferred behind residence and activity tests; protection flows to host treasuries, domestic labor forces, and incumbent contributor pools.
% ABSENT_VOICES: Sending-state communities bear the out-migration side of the flow and have no seat where the gate is set. Economically inactive movers acquire voice mainly retrospectively, through court cases brought after denial. Future cohorts of movers are bound by precedents set in litigation they were never party to.
% DISAPPEARANCE_RATIONALE: If closure authority vanished overnight and every mover gained unconditional equal welfare access, receiving-state welfare politics would rearrange within months: legislatures would rebuild gates in new legal form, or movement itself would be restricted, because the domestic tax bargain that funds welfare is constituted partly by the boundary. Arrangements demonstrably depend on the gate persisting in some form.
% FOUNDING_PROBLEM: Reconcile free movement and a single labor market with welfare states that are nationally organized and contribution-financed: prevent one jurisdiction's solidarity pool from becoming another's fiscal externality, and prevent mobility from dissolving the reciprocity between taxpayers that sustains national redistribution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the Court of Justice's own proportionality jurisprudence explicitly balances mobility rights against host-system protection, treating the tension as real; European Commission and OECD fiscal studies of intra-EU mobility engage the sustainability question empirically; sending-state governments object publicly to uncompensated out-migration. No major participant disputes that the tension exists — the parties dispute which principle should resolve it.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.57, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.57: movers transfer taxes and contributions immediately while their non-contributory access is deferred or denied, and the 'unreasonable burden' standard is administered restrictively despite average net-contribution findings for the mobile population; it is not higher because contributory benefits, labor-market protections, and equal-treatment rights as workers do flow. Suppression 0.55: enforcement machinery is real (verification, denial, expulsion threat) but movement itself is permitted and exit-at-cost exists, so the constraint coerces access terms rather than presence. Theater_ratio 0.36: labor-market protection has genuine content in specific exposed sectors, but a growing share of enforcement and rhetoric targets economically inactive movers whose measured fiscal footprint is negligible — principle defended more than loss prevented. Accessibility_collapse 0.42: alternatives persist (work, naturalize, relocate, return) but the unconditional-access alternative is legally foreclosed everywhere in the federation. Resistance 0.58: sustained CJEU litigation, Commission infringement actions, academic and civil-society challenge, and electoral contestation. Assumptions stated: the interval indexes years since the Maastricht entry into force (T0 = 1993, T30 = 2023); all measurement points are historical and marked observed. The series run on one shared time grid — every tracked metric is authored at every examined point. The trajectories rise through enlargement and crisis-era benefit-tourism politics, peak around the Brexit rupture (T24), and ease slightly afterward as emergency solidarity experiments and shifting political attention relaxed enforcement intensity; the non-monotonic tail is treated as a real moderation, not noise, and is not modeled as cyclical reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute an arrangement that preserves something valuable: from the legislature and welfare-system positions the gate is the load-bearing wall that keeps free movement politically survivable alongside national redistribution. The payer seats compute enforced second-class membership: from the mobile worker's position the same structure takes contributions upfront and rations recognition behind discretionary tests. The returning-national seat exposes the divergence most sharply — the sovereignty logic applied to co-nationals reveals that the gate was never about foreignness but about boundary maintenance itself. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: legislatures retain the veto they are declared to retain; welfare systems collect without matching liability; domestic labor forces and static contributors are insured at others' administrative expense. Payers sit at the high-d end: movers bear the transfer and the tests; inactive movers bear exclusion outright. No directionality overrides were needed, for one reason worth recording: mobile workers are authored with exit_options 'constrained', not 'mobile', because nominal mobility does not reach arbitrage-grade exit from this constraint — conditionality applies at every destination in the federation, and returning home forfeits the accrued position that made moving worthwhile. The derivation chain therefore places them near the target end without correction. The welfare systems carry a secondary agenda-setter role because they do not merely collect: they operate the tests through which the extraction is administered.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and the mandate has not outlived its function, so no mandatrophy resolution is declared. The tangled_rope claim guards against two opposite errors. Reading the gate as a pure snare ignores the possibility — carried by the closure_load_bearing_for_movement omega — that dismantling it would trigger movement restrictions harsher than the conditionality it replaces, destroying more mobility than it liberates. Reading it as a pure rope ignores the measurable asymmetry: a population that funds systems it cannot fully draw on, enforced by tests whose restrictive drift no legislature must vote on. Piton is ruled out structurally: enforcement is consequential for identifiable people, theater_ratio sits well below dominance, and the administrator (the legislature) bears real electoral stakes in maintaining the gate — this is not an inertial husk nobody profits from or hurts enough to fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_authority,
    'This constraint is one reading (member_sovereignty_primary) of the kernel federation_membership_obligations: the disagreement with the sibling readings (integration_primary, selective_solidarity) is located in the locus and foundation of welfare-closure authority — national legislative veto, constitutive mobility right, or individual contribution record. Which locus actually governs access in hard cases?',
    'Structural observation of which actor''s decision gates access when the readings collide: legislature-enacted residence tests upheld by courts (this reading holds), rights-adjudication overriding national gates (integration_primary prevails), or contributory tiering displacing discretionary closure (selective_solidarity prevails).',
    'Under integration_primary the victim set expands to all conditionality-bearing movers and epsilon rises sharply over the same referent; under selective_solidarity the legislature''s veto atrophies into a contribution formula and the payer set re-sorts by contribution history. This story''s classification holds only while national gates remain the operative mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_authority, conceptual, 'Kernel contest: which of three loci of closure authority governs federated welfare access.').

omega_variable(
    net_fiscal_impact_of_intra_eu_movers,
    'Are intra-EU mobile workers net fiscal contributors or net costs to receiving-state welfare systems, disaggregated by sector, region, and household type?',
    'Harmonized longitudinal administrative data linking individual contribution histories to benefit receipts across member states, replacing aggregate and anecdotal estimates.',
    'Widespread net-contribution findings would strip the sustainability rationale of empirical cover, raise theater_ratio, and push the computed type toward snare; concentrated net-cost pockets (child-benefit outflows, border regions) would partially vindicate the closure rationale and stabilize the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_fiscal_impact_of_intra_eu_movers, empirical, 'Whether the sustainability justification tracks measured fiscal reality.').

omega_variable(
    chilling_effect_unmeasured_suppression,
    'How much mobility and benefit take-up does the conditionality deter that formal legal analysis never observes — eligible movers who do not claim, workers who do not move, families that do not reunify?',
    'Take-up gap studies comparing eligible mobile populations against actual claim rates; natural experiments around abrupt rule changes in individual member states.',
    'Large chilling effects mean the authored suppression understates effective suppression: the constraint constrains behavior well beyond its enforcement surface, amplifying effective extraction for cautious movers who never appear in any denial statistic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_unmeasured_suppression, empirical, 'Unobserved behavioral suppression beyond the formal enforcement record.').

omega_variable(
    closure_load_bearing_for_movement,
    'Is national closure authority load-bearing for free movement''s political survival — would unconditional welfare access trigger movement restrictions harsher than the current conditionality, as the Brexit mobilization suggests?',
    'Comparative counterfactual analysis: jurisdictions that extended unconditional access and what followed, versus those that tightened; revealed electorate preferences when offered the trade explicitly.',
    'If load-bearing, a substantial share of the measured extraction is the price of the mobility itself and the coordination reading strengthens; if not load-bearing, the gate is rent preservation and the classification collapses toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_load_bearing_for_movement, conceptual, 'Counterfactual political sustainability of unconditional access alongside national welfare.').

omega_variable(
    unreasonable_burden_indeterminacy,
    'How much restrictive drift does the indeterminate ''unreasonable burden'' standard permit across administrations and over time, without any legislative decision?',
    'Cross-state audits of denial rates for comparable mover profiles; longitudinal tracking of administrative guidance and local practice.',
    'High cross-state variance would indicate the standard functions as a discretionary extraction valve: effective extraction exceeds the formal rule''s face value, and enforcement can intensify without any agenda-setter ever voting for it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unreasonable_burden_indeterminacy, empirical, 'Discretionary drift inside the vague sustainability standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t6, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(fede_tr_t6, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t18, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(fede_tr_t18, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t6, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(fede_be_t6, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t18, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(fede_be_t18, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 30, 0.57).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t6, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(fede_su_t6, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 12, 0.51).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t18, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 18, 0.57).
narrative_ontology:measurement_basis(fede_su_t18, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(fede_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% The colloquial label 'federation membership obligations' conflates three structurally distinct constraints and is decomposed per the epsilon-invariance rule into three linked stories. This file instantiates member_sovereignty_primary: closure authority held by national legislatures, movement conditional, victim set composed of conditionality-bearing movers, excluded inactive movers, and reverse-discriminated returning nationals. The sibling stories instantiate the same kernel with different epsilon referents-assessed-by-their-own-lights and different victim sets; they are separate files, not parameters of this one. Downstream coupling: each successful assertion of national closure in this reading shifts the legitimacy environment in which the selective_solidarity reading's contribution-tiering spreads, which is why this story carries an influences edge to that sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
