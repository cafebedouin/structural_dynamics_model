% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Contributory Tiering of Federation Mobility and Welfare Access
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   In a multi-jurisdiction federation with unequal welfare systems, access
 *   to residence security and social benefits is conditioned on recorded
 *   contribution history and current economic activity rather than on
 *   membership alone. Workers who cross qualifying thresholds hold full
 *   rights; the economically inactive, recent arrivals, and people whose work
 *   produces no recorded contributions face restricted residence and closed
 *   benefit offices. The arrangement is administered through registration
 *   systems, habitual-residence testing, and benefit-office verification,
 *   enforced actively and litigated constantly. This story authors the
 *   standing arrangement as it operates — who it filters, who it protects,
 *   where its gains land — with metrics describing actual operation,
 *   independent of the claimed type.
 *
 * KEY AGENTS:
 *   - host_state_governments: Agenda-setter (institutional/arbitrage) — legislates qualifying periods and residence tests, redesigns the screen at will, collects electoral support from its perceived prudence
 *   - host_state_treasuries: Primary fiscal beneficiary (institutional/arbitrage) — receives avoided outlays as budget headroom and funds the enforcement apparatus
 *   - established_mobile_workers: Protected beneficiary (moderate/constrained) — holds full rights behind accumulated contribution history; exit would forfeit accruals
 *   - domestic_employers: Secondary beneficiary (organized/mobile) — draws a pre-filtered, employment-disciplined labor pool at negligible welfare cost
 *   - new_arrival_workers: Dual-positioned payer (moderate/constrained) — contributes from month one, locked out until thresholds are met, converts to full beneficiary afterward
 *   - economically_inactive_migrants: Primary target (powerless/trapped) — fails activity tests, loses residence security and benefit access together
 *   - informal_caregivers: Hidden target (powerless/trapped) — works full days in care labor that generates no recorded contributions
 *   - prospective_low_wage_movers: Excluded voice (powerless/constrained) — would move for work but has no seat where qualifying periods are set
 *   - supranational_mobility_courts: Analytical observer (institutional/analytical) — strikes down discriminatory test formulations; legislatures answer with narrower drafts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contributory Tiering of Federation Mobility and Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'c1d1a175-4286-4c7f-a633-c158d73ac1dc').
narrative_ontology:cs_kernel_codification('c1d1a175-4286-4c7f-a633-c158d73ac1dc', formalized).
narrative_ontology:cs_authority_grounding('c1d1a175-4286-4c7f-a633-c158d73ac1dc', lineage).
narrative_ontology:cs_interpretation_layer_present('c1d1a175-4286-4c7f-a633-c158d73ac1dc').
narrative_ontology:cs_reading_relation('c1d1a175-4286-4c7f-a633-c158d73ac1dc', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('c1d1a175-4286-4c7f-a633-c158d73ac1dc', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('c1d1a175-4286-4c7f-a633-c158d73ac1dc', foundational, entitlement_tracks_contribution_not_nationality).
narrative_ontology:cs_axiom_status(entitlement_tracks_contribution_not_nationality, holdable).
narrative_ontology:cs_axiom_grounding('c1d1a175-4286-4c7f-a633-c158d73ac1dc', entitlement_tracks_contribution_not_nationality, instrumental).
narrative_ontology:cs_axiom('c1d1a175-4286-4c7f-a633-c158d73ac1dc', secondary, economic_activity_as_movement_threshold).
narrative_ontology:cs_axiom_status(economic_activity_as_movement_threshold, holdable).
narrative_ontology:cs_axiom_grounding('c1d1a175-4286-4c7f-a633-c158d73ac1dc', economic_activity_as_movement_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('c1d1a175-4286-4c7f-a633-c158d73ac1dc', contributory_entitlement_framework).
narrative_ontology:cs_drift_state('c1d1a175-4286-4c7f-a633-c158d73ac1dc', post_enlargement_mass_mobility_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c1d1a175-4286-4c7f-a633-c158d73ac1dc', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, established_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, domestic_employers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, new_arrival_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, informal_caregivers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, new_arrival_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates qualifying periods, habitual-residence tests, and benefit-conditionality rules; can rewrite the screen at any session but faces electoral punishment from host publics for loosening it and litigation plus employer complaints for tightening it past court limits. Collects durable electoral support from voters who read the activity conditions as fiscal prudence.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, host_state_governments, beneficiary).

% Funds the benefit systems and the administrative machinery that screens claimants. Every application refused at a residence or contribution test is budget headroom retained; multi-year forecasts treat the screening yield as a planning input. Gains from the arrangement land here first, as avoided outlays.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_treasuries, beneficiary,
    institutional, generational, arbitrage, national).

% Moved jurisdictions years ago, worked continuously, and crossed every threshold; now hold secure residence and full benefit access. The same qualifying rules that exclude newcomers protect the value of what they have already paid in. Leaving for another jurisdiction would mean starting a new contribution clock and risking accrued entitlements, so staying put is the rational move even against better offers abroad.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, established_mobile_workers, beneficiary,
    moderate, biographical, constrained, continental).

% Hire from a labor pool that the activity conditions keep employment-oriented; turnover skews toward workers with reasons to stay attached to jobs. Bear almost none of the welfare cost the conditions offset, and press legislatures to retain or raise activity requirements whenever loosening is proposed.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, domestic_employers, beneficiary,
    organized, biographical, mobile, national).

% Arrive, register, and work immediately — paying taxes and contributions from the first month — but sit below qualifying-period lines for most benefits, sometimes for years. Their position flips to full access once thresholds are met, so the rule that excludes them now functions later as insurance they funded. Changing jobs or jurisdictions mid-climb can reset the clock.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, new_arrival_workers, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, new_arrival_workers, beneficiary).

% Job seekers, people between contracts, returning nationals, and students without work income. Activity and residence tests remove both benefit access and, in many cases, the right to remain; losing one typically triggers losing the other. Going back to the origin jurisdiction forfeits partial accruals and meets origin-side re-registration hurdles, so the available exits each carry heavy loss.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, immediate, trapped, continental).

% Provide childcare, eldercare, and household labor that makes other people's employment possible, but the recording rules generate no contribution history from this work. Classified alongside the genuinely idle despite working full days. No filing category exists through which their hours could become qualifying time.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, informal_caregivers, payer,
    powerless, biographical, trapped, continental).

% Would move for work if entry terms were known and stable; sit outside every legislature that sets qualifying periods in destination jurisdictions. Objections travel only through NGOs and litigation, never through a seat. Whether they would face the screen's restrictions is decided before they arrive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, prospective_low_wage_movers, excluded,
    powerless, immediate, constrained, continental).

% Hear challenges to residence tests and benefit conditionality against treaty guarantees of movement; strike down formulations judged discriminatory or disproportionate. Each ruling loosens the screen briefly; legislatures respond with narrower drafts that survive review, and the cycle repeats.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, supranational_mobility_courts, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem that arises when jurisdictions with unequal welfare systems share an open labor market: conditioning access on contribution keeps insurance pools actuarially sound and keeps open borders politically sustainable for host publics who would otherwise demand closure.
% TRANSFER_FUNCTION: Moves welfare-system access and residence security away from economically inactive mobile citizens, pre-threshold newcomers, and unpaid caregivers, toward contributing workers, employers, and host treasuries; the avoided cost lands as budget headroom and electoral credit.
% ABSENT_VOICES: Prospective movers from lower-wage regions have no seat in any legislature that sets qualifying periods; informal caregivers are present in the statistics but absent from the drafting rooms, since no ministry represents unpaid work; economically inactive migrants object only through NGOs and litigation after the rules bind them. Unanimity behind the contributory principle arises in rooms these parties never entered.
% DISAPPEARANCE_RATIONALE: If the contributory tiering vanished overnight, welfare access rules would reorganize around either citizenship-based access or restored border controls: treasuries would face uncapped cross-border claim exposure, governments would confront immediate closure politics, established workers' accrued protections would lose their distinguishing force, and millions of mobile citizens' residence and benefit positions would reset — the federation's mobility settlement would rearrange within a single political cycle.
% FOUNDING_PROBLEM: Post-war social insurance was built on distinguishing insured contributors from the general population to keep pools solvent, and later enlarged to a multi-state federation where unequal welfare generosity made unmanaged mobility a fiscal and political threat to the open-borders settlement itself.
% FOUNDING_PROBLEM_CORROBORATION: Comparative welfare-state scholarship and actuarial studies of cross-border claim patterns — sources outside the benefiting parties — corroborate that inter-jurisdictional free-riding pressure on unequal welfare pools is real and ongoing. The same external literature disputes the current calibration: labor economists and migrant-rights organizations attest that existing thresholds exceed any actuarially necessary filter. Corroboration therefore supports the founding problem while contesting the screen's calibration.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.62: the arrangement performs real filtering and insurance work, but a calibrated excess falls on groups whose exclusion outruns any fiscal risk they pose — informal caregivers above all, whose exclusion is an artifact of recording rules rather than measured cost. Suppression is authored at 0.62 as a raw structural property (unscaled by the engine's context dimensions, unlike extractiveness, which the engine scales by directionality and scope): the screen is held up by administrative law — tests, deregistration powers, documentation burdens — not by participant preference. Theater_ratio 0.30: the tests sort genuinely, but a growing share of administrative activity is ritualistic re-documentation of already-verified status. Accessibility_collapse 0.50: exits exist (return migration, self-sufficiency, informal work) but each forfeits accrued position. Resistance 0.55: continuous litigation and advocacy produce periodic judicial loosening, always answered by narrower legislative drafts. The three measurement series share one six-point grid across the thirty-year interval; trajectories rise monotonically — an enforcement-and-calibration ratchet, not a cycle — with terminal values matching the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The treasury and government seats experience the arrangement as solvency maintenance and political sustainability — from there it looks like the price of keeping internal borders open at all. The inactive-migrant and caregiver seats experience arbitrary forfeiture: work performed but unrecognized, residence lost alongside benefits. Established workers see earned protection; new arrivals see a deferred promise they are currently paying into. Same-nominal-status actors diverge sharply: two mobile citizens of identical nationality sit on opposite sides of a qualifying line, differentiated solely by contribution-record position. The engine computes these per-seat classifications from the declared structure; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place host_state_treasuries nearest the beneficiary pole (receives avoided outlays; funds enforcement as a routine budget line). Governments collect electoral support from the screen's perceived prudence — beneficiary by secondary role — though they also bear enforcement and litigation costs, holding them slightly off the pole. Established workers and employers derive low-to-moderate d from their beneficiary declarations, with workers' constrained exit (forfeitable accruals) pulling them back toward symmetry. Victims derive high d: inactive migrants and caregivers are trapped with no converting exit; new arrivals sit high but not maximal, because their payer role converts to beneficiary at threshold — a prospective subsidy the derivation reads as partial. No directionality overrides were needed: the beneficiary/victim plus exit data yields the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-jurisdictional free-riding threatening both insurance-pool solvency and the political viability of open internal borders — remains live, and is corroborated from outside the benefiting parties by comparative welfare-state scholarship and actuarial analyses of cross-border claim patterns. No mandatrophy resolution is declared. The tangled_rope classification guards against two misreadings: a pure-extraction reading would predict that removing the screen is costless, missing that the coordination half is load-bearing (removal proposals reliably trigger closure politics); a pure-coordination reading would erase the calibrated excess borne by caregivers and the inactive. The residual risk the omegas track is inversion: if actuarial audit shows thresholds far exceeding measured fiscal risk, the coordination story thins toward cover and the structure slides toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the selective_solidarity reading of the federation_membership_obligations kernel; how would the victim set and effective extraction change if a sibling reading were adopted instead?',
    'Observe which reading legislatures and courts operationalize: adoption of integration_primary extends full access to economically inactive citizens and dissolves this reading''s victim classes; adoption of member_sovereignty_primary re-keys exclusion to nationality, preserving the burden-shifting while changing who bears it.',
    'Under integration_primary this constraint''s epsilon collapses toward coordination cost; under member_sovereignty_primary the burden-shifting persists with a re-composed victim set and higher suppression. The disagreement is located in the membership criterion itself — citizenship, contribution, or national discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the membership-obligations kernel; sibling adoptions re-compose the victim set.').

omega_variable(
    threshold_calibration_vs_actuarial_need,
    'Do qualifying periods and activity conditions track the measured fiscal risk posed by short-residence and inactive claimants, or do they systematically exceed it?',
    'Actuarial audit comparing net fiscal contribution distributions by months-since-arrival against the thresholds actually imposed, using administrative tax and benefit microdata.',
    'Thresholds matched to risk support the coordination framing and lower effective extraction; systematic excess establishes rent layered onto coordination and pushes classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_vs_actuarial_need, empirical, 'Whether the screen''s calibration is actuarial or padded.').

omega_variable(
    care_work_recording_artifact,
    'Is informal caregivers'' exclusion a measurement artifact of contribution-recording rules, or deliberate design?',
    'Pilot programs crediting care periods as contribution-equivalent time, with before/after fiscal and take-up analysis.',
    'If artifact, the victim class shrinks under reform and the arrangement moves toward cleaner coordination; if design, the exclusion is confirmed as targeted burden placement on unpaid work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_work_recording_artifact, empirical, 'Artifact-versus-design status of the caregiver exclusion.').

omega_variable(
    sustainability_counterfactual,
    'Would unconditional welfare access for mobile citizens actually destabilize host welfare politics, or is the collapse scenario a preference dressed as necessity?',
    'Natural experiments from jurisdictions operating shorter or no residence conditions, tracking benefit take-up, fiscal balances, and anti-immigration voting over subsequent electoral cycles.',
    'If collapse fails to materialize, the coordination justification weakens and the screen reads as burden-shifting with a sustainability alibi; if strain appears, part of the measured extraction is the price of the mobility regime itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainability_counterfactual, conceptual, 'Counterfactual status of the political-sustainability justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t6, federation_membership_obligations__selective_solidarity, theater_ratio, 6, 0.15).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.19).
narrative_ontology:measurement(fede_tr_t18, federation_membership_obligations__selective_solidarity, theater_ratio, 18, 0.23).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.27).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t6, federation_membership_obligations__selective_solidarity, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(fede_be_t18, federation_membership_obligations__selective_solidarity, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t6, federation_membership_obligations__selective_solidarity, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(fede_su_t18, federation_membership_obligations__selective_solidarity, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'free movement versus welfare' decomposes into three structurally distinct readings of the federation_membership_obligations kernel, each with its own epsilon, victim set, and classification. This file is the selective_solidarity reading. integration_primary supplies the upstream mobility guarantee this reading tiers; member_sovereignty_primary supplies the closure authority this reading channels into contributory form. The stories are linked pairwise through network.affects_constraints so contamination analysis can trace how a shift in any one reading's legitimacy propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
