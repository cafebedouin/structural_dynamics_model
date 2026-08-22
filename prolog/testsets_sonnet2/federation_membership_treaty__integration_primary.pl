% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive Principle of the Single Market
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the integration_primary reading of the
 *   federation_membership_treaty kernel: free movement is treated as
 *   constitutive of the single market itself, not as a policy choice states
 *   extend or withdraw. Under this reading, any national restriction on labor
 *   mobility or welfare access bears the burden of proving narrow,
 *   proportionate justification — the default posture treats restriction as
 *   presumptively illegitimate. This produces a specific beneficiary/victim
 *   structure distinct from the sovereignty_primary and subsidiarity_balance
 *   readings of the same kernel: mobile workers, cross-border employers, and
 *   capital owners in receiving economies benefit from guaranteed access;
 *   domestic labor-market incumbents, low-wage workers, and national welfare
 *   systems bear the adjustment costs, with limited recourse because the
 *   legal presumption runs against their protective measures. Extraction and
 *   suppression are both authored as substantial and rising over the
 *   interval, reflecting the doctrine's consolidation through case law that
 *   has progressively narrowed the space for national exception.
 *
 * KEY AGENTS:
 *   - mobile_workers: primary beneficiary (moderate/mobile) — gains guaranteed continental access
 *   - cross_border_employers: beneficiary (powerful/arbitrage) — gains elastic labor supply
 *   - receiving_economy_capital_owners: beneficiary and co-agenda-setter (institutional/arbitrage)
 *   - local_labor_market_incumbents: primary target (powerless/trapped) — bears wage compression
 *   - national_welfare_systems: institutional target (institutional/constrained) — bears fiscal exposure
 *   - low_wage_domestic_workers: sharpest-end target (powerless/trapped)
 *   - supranational_court: agenda-setter (institutional/analytical) — administers and hardens the doctrine
 *   - restrictionist_member_states: excluded voice (organized/constrained) — objections legally subordinated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.74).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive Principle of the Single Market").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '2a85272b-623e-423b-a270-50dca5a3debf').
narrative_ontology:cs_kernel_codification('2a85272b-623e-423b-a270-50dca5a3debf', fixed_text).
narrative_ontology:cs_authority_grounding('2a85272b-623e-423b-a270-50dca5a3debf', lineage).
narrative_ontology:cs_interpretation_layer_present('2a85272b-623e-423b-a270-50dca5a3debf').
narrative_ontology:cs_reading_relation('2a85272b-623e-423b-a270-50dca5a3debf', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('2a85272b-623e-423b-a270-50dca5a3debf', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('2a85272b-623e-423b-a270-50dca5a3debf', foundational, free_movement_constitutive_of_market_unity).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_market_unity, holdable).
narrative_ontology:cs_axiom_grounding('2a85272b-623e-423b-a270-50dca5a3debf', free_movement_constitutive_of_market_unity, conventional).
narrative_ontology:cs_axiom('2a85272b-623e-423b-a270-50dca5a3debf', foundational, restriction_bears_burden_of_justification).
narrative_ontology:cs_axiom_status(restriction_bears_burden_of_justification, holdable).
narrative_ontology:cs_axiom_grounding('2a85272b-623e-423b-a270-50dca5a3debf', restriction_bears_burden_of_justification, instrumental).
narrative_ontology:cs_reference_frame('2a85272b-623e-423b-a270-50dca5a3debf', post_war_anti_fragmentation_commitment).
narrative_ontology:cs_drift_state('2a85272b-623e-423b-a270-50dca5a3debf', contemporary_consolidated_case_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a85272b-623e-423b-a270-50dca5a3debf', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, receiving_economy_capital_owners).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_market_incumbents).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, low_wage_domestic_workers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, single_market_constitutive_integration_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, free_movement_as_fundamental_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move across member-state borders to take up work without visas, quotas, or labor-market tests. The doctrine's presumption against restriction is what makes their mobility a right rather than a discretionary permission each destination state could withdraw. Their gain is direct and structural: wage arbitrage, career options, and legal standing enforceable against any state that tries to condition entry.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continental labor pool without needing to justify recruitment through national permit systems. The presumption-of-illegitimacy standard forces states to prove narrow tailoring before restricting labor supply, which functionally guarantees employers access to whichever national workforce is cheapest or most available at a given moment.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Capital-intensive sectors and their owners benefit from elastic labor supply that suppresses wage growth and lets investment flow to wherever the combined labor-and-capital mobility regime is most favorable. They also sit close to the agenda-setting apparatus that drafted and defends the constitutive-principle doctrine at the supranational court and commission level.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, receiving_economy_capital_owners, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, receiving_economy_capital_owners, agenda_setter).

% Workers in receiving regions face wage compression and job competition from an enlarged labor pool they had no vote on constituting. They cannot exit the labor market they're embedded in, and the doctrine's presumption against restriction means their national government's hands are largely tied on protective measures — any attempt is litigated as presumptively illegitimate.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_market_incumbents, payer,
    powerless, biographical, trapped, national).

% Social insurance and benefit systems designed around a national contributor base face fiscal pressure when free movement allows access with limited residency-based conditioning. They bear administrative and solvency costs while their capacity to impose eligibility restrictions is narrowly bounded by the same presumptive-illegitimacy test that governs labor restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Bear the sharpest end of wage competition in low-skill sectors where mobile workers concentrate. They have no meaningful exit — relocating to escape competition often means moving into the very labor markets that generated the pressure in the first place — and their political voice against liberalization is treated as the kind of protectionist interest the doctrine exists to override.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, low_wage_domestic_workers, payer,
    powerless, biographical, trapped, national).

% Adjudicates disputes between member states and the free-movement principle, consistently interpreting exceptions narrowly. It administers the presumption-of-illegitimacy standard as settled doctrine and has consistently strengthened it through case law expanding what counts as an unjustified restriction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, supranational_court, agenda_setter,
    institutional, civilizational, analytical, continental).

% Would prefer discretionary control over labor-market access and welfare eligibility calibrated to domestic conditions. Their objections are heard in political fora but structurally subordinated in legal ones: any national restriction bears the burden of proof against a doctrine that treats restriction as the thing needing justification.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, restrictionist_member_states, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes duplicated, fragmented national labor-market gatekeeping and lets a genuinely integrated continental market allocate labor to its most productive use, avoiding a race of overlapping permit bureaucracies and enabling firms and workers to plan across borders as though inside one jurisdiction.
% TRANSFER_FUNCTION: Moves labor-market rents from domestic incumbent workers (especially in low-wage and low-skill sectors) and fiscal capacity from national welfare systems, toward mobile workers who gain access and toward employers and capital owners who gain an elastic, wage-suppressing labor supply.
% ABSENT_VOICES: Domestic low-wage workers and restrictionist member-state electorates would object that they never consented to labor-market exposure at this scale, but their objections are treated procedurally as protectionist interests to be overridden rather than legitimate stakeholders in the coordination question; they are represented in politics but structurally weak in the adjudicating forum.
% DISAPPEARANCE_RATIONALE: If the constitutive-principle reading were abandoned overnight, member states would reassert labor-market and welfare conditioning, mobile-worker flows would reorganize around bilateral permit regimes, employers would lose guaranteed continental labor access, and the single market's legal architecture (built on this doctrine as foundational) would require wholesale renegotiation.
% FOUNDING_PROBLEM: Post-war economic integration required removing internal barriers to labor movement so that a genuine common market could function instead of a patchwork of protected national economies; free movement was built to prevent backsliding into economic nationalism that had contributed to prior continental conflict.
% FOUNDING_PROBLEM_CORROBORATION: The supranational court and integration-oriented institutions attest the founding problem remains live — that any relaxation risks re-fragmentation. Independent labor economists studying wage effects in receiving regions, and elected officials in restrictionist member states, attest that the original anti-fragmentation problem has been substantially solved and the doctrine now functions primarily to protect an entrenched liberalization regime against democratic reversal, a reading corroborated by referenda and parliamentary votes in several member states that were legally foreclosed rather than accommodated.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as substantial (0.58 at interval end) and rising because the doctrine, read this way, systematically transfers labor-market and fiscal capacity from national systems and domestic workers toward mobile workers and capital, with the transfer intensifying as case law narrows the space for exception. Suppression is authored higher still (0.74) because the presumption-of-illegitimacy standard is precisely a suppression mechanism: it does not merely permit free movement, it forecloses the political and legal tools states would otherwise use to restrict it. Theater ratio stays comparatively low (0.22) because the coordination function — an actually integrated labor market avoiding fragmented permit regimes — is real and substantially delivered, not merely performed; this is not a hollowed-out constraint, it is a live, actively defended one.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational court and capital-owner seats, this is coordination: a genuinely integrated market requires exactly this kind of constitutive commitment, and any weakening reopens fragmentation risk. From the local incumbent and welfare-system seats, the same structure is experienced as enforced extraction: their protective preferences are treated as presumptively illegitimate rather than as a legitimate policy interest to be weighed. The engine's per-seat computation should reflect this asymmetry directly from the power/exit data rather than from any narrative adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and employers derive low d (beneficiary end) because the constitutive-principle reading structurally guarantees them access regardless of destination-state preference. Local incumbents and low-wage domestic workers derive high d (target end) because they are trapped in labor markets whose composition they cannot control and whose protective adjustments are legally disfavored. National welfare systems sit institutional but constrained rather than trapped — they retain some administrative capacity but operate under a legal ceiling on eligibility conditioning that the doctrine imposes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing economic nationalism from re-fragmenting the continental market — was genuinely live at founding and is treated by the supranational court as still live today. But independent economists and several member-state electorates attest the problem has been substantially solved, and that the doctrine's continued expansion now serves primarily to insulate the liberalization regime from democratic reversal rather than to prevent renewed fragmentation. This is a founding_problem_status: contested case precisely because it is unresolved whether the mandate has outlived its function or whether relaxation would in fact trigger the fragmentation the doctrine was built to prevent — the mismatch between 'dead' (economist reading) and 'live' (court reading) is the diagnostic signal, not a resolved fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_framing_choice,
    'Is the correct framing of the free-movement clause that it constitutes the single market (this reading) or that it is a conditional grant states retain authority to limit (sovereignty_primary)?',
    'Textual and drafting-history analysis of the founding treaty, combined with tracking which framing the supranational court''s case law has actually entrenched over successive rulings versus which framing member-state ratification debates assumed.',
    'If the sovereignty_primary framing is correct, this story''s high suppression score is actually the signature of doctrinal overreach by the court beyond its textual mandate; if the integration_primary framing is correct, the suppression is faithful implementation of the treaty''s actual constitutive commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_framing_choice, conceptual, 'Whether the constitutive or conditional reading is the more textually and historically faithful account of the founding commitment.').

omega_variable(
    welfare_system_capacity_ambiguity,
    'How much genuine fiscal capacity do national welfare systems retain under the narrow-justification test, versus how much is nominal only?',
    'Empirical tracking of national eligibility-restriction attempts and their success/failure rate before the supranational court over the interval, distinguishing genuine judicial deference from uniformly narrow construction.',
    'If national restrictions are routinely upheld when narrowly tailored, the doctrine functions closer to tangled_rope (real coordination with bounded extraction); if restrictions are almost always struck down regardless of tailoring, the ''narrowly justified'' exception is largely theater and the constraint functions closer to snare for welfare systems specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_capacity_ambiguity, empirical, 'Whether the narrow-justification exception provides real or merely nominal protection to national welfare capacity.').

omega_variable(
    labor_market_incumbent_coalition_potential,
    'Could domestic low-wage workers and restrictionist member-state electorates form an effective coalition to force renegotiation of the constitutive-principle doctrine, despite being individually powerless within the adjudicating forum?',
    'Track whether cross-national populist or labor coalitions achieve treaty renegotiation, opt-outs, or binding political constraints on the court''s interpretive latitude over subsequent treaty revision cycles.',
    'If such coalitions succeed, the powerless-payer classification for domestic incumbents understates their long-run structural power; if such efforts consistently fail against the doctrine''s legal entrenchment, it confirms the asymmetry is durable rather than contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_incumbent_coalition_potential, empirical, 'Whether politically powerless payer groups can achieve coalition leverage against a legally entrenched doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__integration_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__integration_primary, theater_ratio, 16, 0.15).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__integration_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__integration_primary, theater_ratio, 32, 0.2).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__integration_primary, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__integration_primary, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__integration_primary, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__integration_primary, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__integration_primary, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__integration_primary, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__integration_primary, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__integration_primary, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language kernel 'federation membership treaty on free movement' (per the epsilon-invariance principle: the same clause read three ways yields three different beneficiary/victim structures and three different epsilon values, hence three stories, not one with a parameter). integration_primary authors the highest suppression and a substantial rising extractiveness because it treats restriction as presumptively illegitimate. sovereignty_primary is expected to author much lower suppression and different beneficiaries (member states, protected domestic labor markets). subsidiarity_balance sits between the two, with moderate suppression bounded by proportionality review. All three share the same underlying treaty text and supranational court but are structurally distinct constraints under this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
