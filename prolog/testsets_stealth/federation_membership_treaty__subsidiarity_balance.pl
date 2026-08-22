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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Subsidiarity-Balanced Free Movement (Proportionality Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   In this reading of the federation membership treaty, free movement is
 *   real and judicially enforceable but bounded by proportionality: member
 *   states may restrict it where legitimate national interests require, and a
 *   supranational court polices the fit between restriction and aim. The
 *   result is a graduated structure whose winners and losers vary by policy
 *   domain — mobile citizens and cross-border employers gain enforceable
 *   access; states alternate between shield and loss depending on how their
 *   measures fare; domestic low-wage workers and sending regions bear
 *   adjustment costs they never consented to; third-country nationals sit
 *   outside the graduated tier entirely. Claim and metrics are independent
 *   authored facts: the claimed type reflects the structure I believe true (a
 *   genuine coordination service joined to asymmetric, domain-varying
 *   incidence, held up by active enforcement), and the metric values reflect
 *   the arrangement's observed operation. This file instantiates one reading
 *   of the contested kernel; the sibling readings are separate constraints,
 *   not hedges folded into this one.
 *
 * KEY AGENTS:
 *   - - supranational_court: Agenda setter (institutional/identity_locked) — administers proportionality review; its authority is fused with the reviewer role
 *   - - member_state_governments: Dual-positioned payer/beneficiary (institutional/constrained) — restrictions sometimes upheld, sometimes struck down
 *   - - mobile_union_citizens: Primary beneficiary (moderate/constrained) — enforceable access with residual friction
 *   - - cross_border_employers: Secondary beneficiary and receipt seat (powerful/arbitrage) — recruits into kept-open wage differentials
 *   - - low_wage_domestic_workers: Payer (powerless/trapped) — bears competition it did not consent to
 *   - - sending_region_communities: Payer (organized/trapped) — bears depopulation and skill drain
 *   - - third_country_nationals: Excluded voice (powerless/trapped) — outside the graduated tier, no standing
 *   - - federalism_legal_scholars: Analytical observer — maps domain-varying outcomes, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.48).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.5).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Subsidiarity-Balanced Free Movement (Proportionality Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, 'adb2097e-94bc-4787-b7c8-dc1edd6a8efc').
narrative_ontology:cs_kernel_codification('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', fixed_text).
narrative_ontology:cs_authority_grounding('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', lineage).
narrative_ontology:cs_interpretation_layer_present('adb2097e-94bc-4787-b7c8-dc1edd6a8efc').
narrative_ontology:cs_reading_relation('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', foundational, proportionate_national_interests_lawfully_constrain_mobility).
narrative_ontology:cs_axiom_status(proportionate_national_interests_lawfully_constrain_mobility, holdable).
narrative_ontology:cs_axiom_grounding('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', proportionate_national_interests_lawfully_constrain_mobility, conventional).
narrative_ontology:cs_axiom('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', foundational, restriction_legitimacy_is_adjudicated_not_self_declared).
narrative_ontology:cs_axiom_status(restriction_legitimacy_is_adjudicated_not_self_declared, holdable).
narrative_ontology:cs_axiom_grounding('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', restriction_legitimacy_is_adjudicated_not_self_declared, conventional).
narrative_ontology:cs_reference_frame('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', judicially_balanced_mobility_compact).
narrative_ontology:cs_drift_state('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', contemporary_enlargement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adb2097e-94bc-4787-b7c8-dc1edd6a8efc', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_union_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, low_wage_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sending_region_communities).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, third_country_nationals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, mutual_recognition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reviews member-state restrictions on cross-border movement against a proportionality standard: legitimate aim, suitability, necessity, overall balance. Its docket determines which restrictions survive. Its authority rests on holding the middle position between the mobility guarantee and state self-government; abandoning the reviewer role would dissolve the jurisdiction it exercises. It collects no revenue; what accrues to it is docket control and doctrinal precedence.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, supranational_court, agenda_setter,
    institutional, generational, identity_locked, continental).

% Adopt the restrictions the framework reviews: residence conditions for welfare access, public-sector employment rules, transitional labor-market controls for newer members. Restrictions that pass review stand and deliver the protective outcome sought; restrictions struck down cost compliance programs, litigation expense, and a measure of regulatory authority ceded to the reviewing court. The same instrument is therefore sometimes a shield and sometimes a loss. Exit would mean treaty withdrawal, priced in lost export access and structural funds.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, beneficiary).

% Hold an enforceable right to seek work and residence in other member states, with legal certainty about which obstacles are challengeable. They gain access to larger labor markets and coordinated social-security treatment. Their friction is residual: lawful waiting periods, credential recognition delays, and the risk that a host state's new restriction survives review. Every destination shares the same framework, so relocating does not escape it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_union_citizens, beneficiary,
    moderate, biographical, constrained, continental).

% Recruit across borders into wage differentials the framework keeps open, fill shortages in receiving regions, and post the same workers across jurisdictions under harmonized rules. If any single government turns hostile, hiring plans and capital move to a friendlier jurisdiction; that relocation option disciplines the review process from the employer side.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Compete with incoming workers in construction, agriculture, care, and food processing without having consented to the competition and without a seat in the proceedings that authorize it. Wage growth in their sectors lags regional averages. Skills, housing tenure, and family ties bind them to place, so their realistic response is individual coping rather than relocation. Their organizations are thin; their objections surface mainly through sectoral unions and municipal government.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, low_wage_domestic_workers, payer,
    powerless, immediate, trapped, national).

% Lose working-age residents to richer regions faster than they replace them: school closures, thinning tax bases, dependent populations left behind. Regional governments petition the same institutions that administer mobility for cohesion funds and derogations, and receive partial compensation at best. There is no exit from the union-wide framework short of a national withdrawal they do not control.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_region_communities, payer,
    organized, generational, trapped, regional).

% Live and work in the same territory under a separate, stricter permission regime: their movement between member states falls outside the graduated framework entirely, and long-term residence stays discretionary. They would object that a system calibrating freedom of movement for citizens while excluding them entrenches a two-tier labor market, but they hold standing in almost none of the forums where the calibration is argued.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Track how the balance shifts across policy domains and document where review outcomes track doctrine versus political salience. They publish comparative analyses, testify in reform debates, and map the counter-limit positions of national courts. They bear none of the arrangement's costs and collect none of its gains.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federalism_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of reconciling cross-border labor mobility with retained national regulatory competence: a shared adjudicative standard tells every member state in advance which restrictions will stand, replacing case-by-case bilateral trade conflict with one predictable test administered centrally.
% TRANSFER_FUNCTION: Moves adjudicative authority over mobility restrictions from national governments to the supranational court; moves labor supply across borders toward higher-wage regions; lands the adjustment costs of that movement on domestic low-wage workers and sending-region communities, and lands sovereignty losses on states whose measures fail review.
% ABSENT_VOICES: Third-country nationals are outside the framework altogether. Holders of the integration_primary reading would object that proportionality gives states too much room to obstruct; holders of the sovereignty_primary reading would object that external review strips democratically accountable choices. Each flank appears only as a litigant asking the reviewer to move toward its pole; neither accepts the middle as legitimate.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, every cross-border restriction becomes a bilateral dispute; mobility rights become uncertain pending negotiation; states re-impose controls while employers reroute hiring; the court loses its core docket and the mutual-recognition settlement unwinds into managed trade conflict.
% FOUNDING_PROBLEM: Post-war integration needed to stop economic nationalism from fragmenting the common market while preserving democratic legitimacy for national welfare regulation and labor-market protection; the proportionality balance was built to hold both commitments at once.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national constitutional courts' counter-limits jurisprudence attests the live tension from the state-sovereignty side; the comparative federalism literature documents it independently; parliamentary debates over mobility backstops recur in every enlargement. No corroborating source treats the founding problem as resolved.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness sits at 0.48 because the arrangement transfers real adjustment costs onto unconsenting parties (sectoral wage compression for domestic workers, depopulation for sending regions, ceded regulatory authority for losing states) while no seat captures monopoly rents and much of its operation is genuine dispute resolution that substitutes for bilateral conflict. Suppression is 0.50: the framework actively forecloses both unrestricted-mobility claims and blanket prohibition, which requires continuous enforcement capacity (infringement actions, preliminary rulings), but it leaves wide lawful accommodation space rather than closing exits outright. Theater is 0.27 — the balancing test is mostly operative, though recital-style reasoning grows as outcomes stabilize. Accessibility_collapse is 0.50: understanding the test kills blanket instruments but negotiated derogations and transitional controls persist. Resistance is 0.55: counter-limit jurisprudence, opt-outs, and legislative defiance of unpopular rulings are recurring. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. The three measurement series share one time grid (points 0, 6, 12, 18, 24, 30) so no metric is sampled against another metric's end-state. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: review machinery hardened through mid-interval landmark rulings, then eased slightly as legislatures codified restriction frameworks, lowering the marginal enforcement burden. Coalition note: the domestic workers' seat is weak organizationally, not numerically — coalition potential exists but is unrealized, which is recorded in the data rather than assumed away.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat the arrangement is the constitution of compromise — the thing that makes coexistence of market and welfare state adjudicable. From the mobile citizen's seat it is a protected right with friction at the edges. From the domestic worker's and sending region's seats it is a regime that authorized competition and departure they were never consulted on. From the state's seat it alternates between shield (upheld measures) and loss (struck-down measures) depending on the domain and the political moment. The engine computes these per-seat classifications from the structural data; the divergence between the court's view and the trapped payers' view is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: mobile citizens (moderate power, constrained exit) sit well toward the beneficiary end; cross-border employers (powerful, arbitrage-grade exit) sit nearest it. Declared victims derive high directionality: domestic workers (powerless, trapped) and sending regions (organized but trapped) sit near the full-target end, amplified modestly by the continental scope that makes verification of proportionality claims harder; third-country nationals, formally outside the framework, would sit furthest of all if admitted to the computation. Member-state governments are genuinely dual-positioned — payer when struck down, beneficiary when upheld — and the secondary_role declaration carries that duality rather than a directionality override, since no single power-atom override could express it without distorting the court's institutional seat. No directionality_overrides are authored: the derivation from roles, power, and exit options produces the right relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a common market with national self-government — is still live, so no mandatrophy resolution is declared and the R5 mismatch flag should not fire. The classification discipline guards both symmetrical errors: reading this as a pure rope ignores the unconsenting payers whose costs fund the settlement; reading it as a snare ignores the real coordination service (predictable centralized adjudication replacing bilateral trade conflict) and the absence of any seat capturing the proceeds. The tangled-rope claim keeps both facts in view simultaneously, and per-seat computation lets the engine show where the same structure operates as near-rope (for mobile citizens) and near-snare (for trapped payers) without forcing one verdict over all seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (subsidiarity_balance) of the federation_membership_treaty kernel; would instantiating integration_primary or sovereignty_primary instead produce a materially different epsilon, victim set, and computed type?',
    'Author the sibling reading files against the same referent and compare computed per-seat classifications; convergence would indicate the kernel is less contested than the reading split suggests.',
    'If the siblings compute different types, the kernel contest is substantive and cross-reading comparison becomes the primary analysis; if not, the three readings collapse into one constraint with rhetorical variation and the family should merge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame uncertainty: which reading of the membership treaty governs the standing arrangement.').

omega_variable(
    domain_varying_incidence,
    'Does the aggregate epsilon mask divergent per-policy-domain structures (labor mobility, welfare access, services, establishment) whose beneficiary and victim sets differ enough to warrant decomposition into separate stories?',
    'Code review outcomes by policy domain and compute the structural relationship separately for each domain using domain-specific winner and loser records.',
    'If domains diverge widely, this story should decompose into a family of per-domain constraints linked by network edges; the aggregate figure would then be averaging distinct constraints with distinct epsilons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_varying_incidence, empirical, 'Whether the graduated structure is one constraint or several domain-specific ones.').

omega_variable(
    equilibrium_or_ratchet,
    'Does the proportionality balance genuinely equilibrate, or does it ratchet toward expanded mobility with periodic sovereignty backlash that each subsequent ruling absorbs?',
    'Longitudinal coding of review outcomes by domain and political salience; test whether strike-down rates mean-revert around a stable ratio or trend in one direction.',
    'A ratchet would date a type transition toward heavier extraction that the flattening end-state series conceals; a genuine equilibrium supports treating the balanced arrangement as structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_or_ratchet, empirical, 'Stability of the judicial balance over the interval.').

omega_variable(
    tcn_exclusion_boundary,
    'Is third-country nationals'' exclusion part of this constraint''s structure, or a separate constraint (parallel permission regimes) that merely abuts it?',
    'Epsilon-invariance test: assess the citizen-mobility arrangement with and without the citizenship boundary; if the assessment is unchanged, the boundary belongs to a different story.',
    'If included, the victim set expands and effective extraction rises; if separate, this story''s victim set shrinks to internal payers and the parallel exclusion regime warrants its own file linked by network edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tcn_exclusion_boundary, conceptual, 'Scope boundary between the graduated citizen framework and the adjacent exclusion regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subsidiarity_balance_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t0, observed).
narrative_ontology:measurement(subsidiarity_balance_tr_t6, federation_membership_treaty__subsidiarity_balance, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t6, observed).
narrative_ontology:measurement(subsidiarity_balance_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t12, observed).
narrative_ontology:measurement(subsidiarity_balance_tr_t18, federation_membership_treaty__subsidiarity_balance, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t18, observed).
narrative_ontology:measurement(subsidiarity_balance_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t24, observed).
narrative_ontology:measurement(subsidiarity_balance_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(subsidiarity_balance_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(subsidiarity_balance_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t0, observed).
narrative_ontology:measurement(subsidiarity_balance_be_t6, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t6, observed).
narrative_ontology:measurement(subsidiarity_balance_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.44).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t12, observed).
narrative_ontology:measurement(subsidiarity_balance_be_t18, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 18, 0.46).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t18, observed).
narrative_ontology:measurement(subsidiarity_balance_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t24, observed).
narrative_ontology:measurement(subsidiarity_balance_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(subsidiarity_balance_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(subsidiarity_balance_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t0, observed).
narrative_ontology:measurement(subsidiarity_balance_su_t6, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t6, observed).
narrative_ontology:measurement(subsidiarity_balance_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t12, observed).
narrative_ontology:measurement(subsidiarity_balance_su_t18, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 18, 0.53).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t18, observed).
narrative_ontology:measurement(subsidiarity_balance_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t24, observed).
narrative_ontology:measurement(subsidiarity_balance_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(subsidiarity_balance_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, posted_workers_enforcement_regime).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, welfare_residence_conditionality).

% DUAL FORMULATION NOTE:
% Constraint family from the federation_membership_treaty kernel: one kernel, three readings emitted as separate files. The upstream reading (integration_primary) supplies the mobility guarantee this reading bounds; the downstream reading (sovereignty_primary) supplies the state-interest exceptions this reading adjudicates. Epsilon differs across the family because the referent — the standing arrangement under contest — is assessed through different burden-of-justification allocations; this file authors epsilon for the proportionality-balanced arrangement as the subsidiarity_balance reading assesses it, never for the flanking arrangements its siblings would install.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
