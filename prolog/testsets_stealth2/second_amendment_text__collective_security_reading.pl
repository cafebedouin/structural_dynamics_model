% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Collective-Security Reading of the Second Amendment (Militia-Clause Conditioning)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story authors ONE reading of the Second Amendment kernel: the
 *   collective-security reading, under which the prefatory militia clause
 *   conditions the right to arms on organized civic defense and thereby
 *   warrants state regulation of arms toward collective security. As an
 *   operative arrangement (mapped here onto roughly 1939-2008, with interval
 *   units of about 2.3 years, from United States v. Miller's militia-framing
 *   through the eve of District of Columbia v. Heller), the reading does real
 *   coordination work: it channels arms through accountable institutions and
 *   licenses screening that has documented protective effects. It
 *   simultaneously transfers discretion, fees, and compliance burdens to a
 *   state apparatus that did not bear them before, and it defines entire
 *   classes of claimants out of the framework. The claim and the metrics are
 *   authored independently: claimed_type records the structure I believe true
 *   (genuine coordination plus asymmetric transfer under active enforcement);
 *   the metrics record the arrangement's observed operation. KEY AGENTS (by
 *   structural relationship): - state_regulatory_apparatus: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) — holds the
 *   constitutional warrant, collects fees and approval discretion -
 *   judiciary_as_interpreter: Agenda-setter by doctrinal construction
 *   (institutional/constrained) — its precedents constitute the reading's
 *   operative force - organized_militia_institutions: Secondary beneficiary
 *   (institutional/constrained) — occupies the protected position the reading
 *   designates - communities_affected_by_gun_violence: Coordination-side
 *   beneficiary (moderate/constrained) — receives the security output,
 *   supplies political demand - individual_gun_owners: Primary target
 *   (moderate/constrained) — pays fees, waits, accepts exclusions, risks
 *   prosecution - firearms_retailers_dealers: Secondary target
 *   (moderate/constrained) — carries licensing, record-keeping, and
 *   inspection burdens - unorganized_militia_claimants: Excluded claimant
 *   (powerless/identity_locked) — defined out of the framework yet regulated
 *   by it - constitutional_scholars: Analytical observer
 *   (analytical/analytical) — supplies the arguments all sides borrow
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.6).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Collective-Security Reading of the Second Amendment (Militia-Clause Conditioning)").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'e413ac90-d738-4b63-bdd1-3dd14b0309db').
narrative_ontology:cs_kernel_codification('e413ac90-d738-4b63-bdd1-3dd14b0309db', fixed_text).
narrative_ontology:cs_authority_grounding('e413ac90-d738-4b63-bdd1-3dd14b0309db', practice).
narrative_ontology:cs_interpretation_layer_present('e413ac90-d738-4b63-bdd1-3dd14b0309db').
narrative_ontology:cs_reading_relation('e413ac90-d738-4b63-bdd1-3dd14b0309db', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e413ac90-d738-4b63-bdd1-3dd14b0309db', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('e413ac90-d738-4b63-bdd1-3dd14b0309db', foundational, militia_prefatory_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_prefatory_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('e413ac90-d738-4b63-bdd1-3dd14b0309db', militia_prefatory_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('e413ac90-d738-4b63-bdd1-3dd14b0309db', secondary, state_police_power_extends_to_arms).
narrative_ontology:cs_axiom_status(state_police_power_extends_to_arms, holdable).
narrative_ontology:cs_axiom_grounding('e413ac90-d738-4b63-bdd1-3dd14b0309db', state_police_power_extends_to_arms, instrumental).
narrative_ontology:cs_reference_frame('e413ac90-d738-4b63-bdd1-3dd14b0309db', militia_conditioned_civic_defense_compact).
narrative_ontology:cs_drift_state('e413ac90-d738-4b63-bdd1-3dd14b0309db', contemporary_post_heller_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e413ac90-d738-4b63-bdd1-3dd14b0309db', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_institutions).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, communities_affected_by_gun_violence).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_retailers_dealers).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unorganized_militia_claimants).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, prefatory_clause_limiting_force_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, police_power_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures draft permit, prohibition, and storage regimes justified as serving collective security; agencies administer applications, fees, and revocations; police execute compliance checks. The reading supplies the constitutional warrant for the whole apparatus: because the right exists to serve organized defense, arms circulation may be governed toward that end. Fee revenue funds administration, and approval discretion accumulates to issuing officials. Exit for the apparatus is adjustment rather than abandonment: rules can be rewritten and categories shifted wherever courts sustain the warrant.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts decide what the amendment protects and what it permits. Under this reading their precedents treat the militia clause as limiting: challenges premised on personal possession outside organized service fail, and regulatory statutes stand. Each ruling reconstitutes the arrangement. Departure requires overturning settled doctrine, which courts approach slowly and under intense political pressure, so the interpreter seat is bound by its own prior outputs.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, judiciary_as_interpreter, agenda_setter,
    institutional, generational, constrained, national).

% The National Guard and state-organized forces occupy the position the reading reserves for protected arms-bearing: weapons, training, and discipline flow through accountable command. The reading gives these bodies constitutional centrality as what 'well regulated Militia' presently denotes, and their budgets, personnel authority, and legal immunities ride on that designation. Their position is fixed by the state-federal command structure they operate inside.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, organized_militia_institutions, beneficiary,
    institutional, generational, constrained, national).

% Residents of areas with high firearm violence receive the security side of the arrangement: background checks, category prohibitions, and permit screening are offered to them as protection. They bear harms when enforcement fails and bear little of the compliance burden falling on owners and dealers. Their political support supplies the demand that keeps regulatory statutes enacted; relocating away from violence is costly and rarely chosen.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, communities_affected_by_gun_violence, beneficiary,
    moderate, biographical, constrained, regional).

% Own or seek firearms for purposes the reading does not privilege: hunting, collecting, personal defense. They pay application fees, wait out processing, accept category exclusions and storage mandates, and risk prosecution for violations. Compliance is the price of lawful possession; refusal means forfeiting ownership or facing charges. Organized advocacy represents them politically, but each owner faces the permitting desk alone. Relocation to permissive states is possible for some; most adapt.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Hold licenses, keep records, submit to inspections, and refuse sales flagged by the system. Compliance overhead scales with inventory and transaction volume and is passed partly to customers. Revocation of the license ends the business, so adherence is not optional; the dealer's livelihood sits inside the administrative perimeter the reading draws.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_retailers_dealers, payer,
    moderate, biographical, constrained, national).

% Assert membership in self-organized companies and claim the amendment protects their drills and equipment as militia service. The reading recognizes only state-organized bodies, so their claims find no doctrinal home, and several states separately criminalize unauthorized paramilitary activity. They stand outside the policy conversation that defines who counts as the militia, while their self-concept is constituted by the very civic-defense identity the reading assigns elsewhere; leaving the claim would mean abandoning that identity.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, unorganized_militia_claimants, excluded,
    powerless, biographical, identity_locked, national).

% Historians and legal academics publish on the clause's drafting, ratification, and nineteenth-century reception. They hold no decision power; their analyses supply arguments that every litigant borrows. Their stake is reputational and intellectual rather than material.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels armed capacity into organized, accountable bodies and subjects arms circulation to collective-security criteria, addressing the republican problem of arming citizens without either general disarmament or unaccountable private force.
% TRANSFER_FUNCTION: Moves regulatory discretion, fee revenue, and compliance costs (application fees, waiting periods, category exclusions, record-keeping burdens) from individual gun owners and dealers to the state regulatory apparatus; moves a security assurance to the broader public.
% ABSENT_VOICES: Owners whose sole justification is personal self-defense have no seat: the reading defines their preferred ground out of the framework before argument begins. Unorganized militia claimants likewise stand outside the room that decides who counts as the militia, though their activities are regulated. May-issue applicants in discretionary jurisdictions face decisions they cannot meaningfully appeal into the conversation.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, thousands of permit, prohibition, and storage statutes would lose their constitutional warrant and face immediate challenge; the regulatory apparatus would shrink or re-found itself on different legal ground; ownership patterns, dealer practices, and state-federal arms arrangements would all shift. The arrangement is load-bearing for the current allocation of authority over arms.
% FOUNDING_PROBLEM: Securing the early republic against invasion and insurrection, and against the danger of a standing federal army, by guaranteeing the states an armed citizen militia under local control.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and the Guard's own institutional record attest the founding problem's dissolution: the Militia Act of 1903 folded the state militias into a federally commanded National Guard, ending independent state armed capacity, and the standing army the clause guarded against became permanent. These sources sit outside the benefiting parties (the regulatory apparatus and organized militia institutions), which instead narrate a continuous 'collective security' purpose. No party outside the beneficiary set attests that the original militia-defense problem remains live.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.65 at interval end) reflects a real, growing transfer: permit fees, discretionary denial, category prohibitions, and dealer compliance overhead, decoupled in significant part from marginal security produced. Suppression (0.60) is the raw structural coercive force maintaining the arrangement — criminal penalties for violation, license revocation, unreviewable may-issue discretion — and is authored unscaled, since only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.40) captures the growing share of activity that is symbolic or arbitrary (grandfathered prohibitions, discretionary denials untethered to risk) alongside genuinely functional screening. Accessibility_collapse (0.45) is moderate: within the reading's frame, alternatives to regulation collapse, but the sibling readings and the political route remain open, so the frame does not totalize. Resistance (0.78) is high and rising — organized litigation, the Heller challenge itself, and persistent political contestation — because identifiable classes bear real costs and retain voice. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point. Suppression_requirement is authored as a series because the story specifically traces enforcement-capacity build-up: ATF expansion, the 1968 regime, computerized background-check infrastructure, and maturing permit bureaucracies — a rising enforcement trajectory, not a static picture. Base_extractiveness and theater_ratio rise monotonically with the accumulation of regulatory layers; no cyclical dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently from the same structure. From the regulatory apparatus and the bench, the arrangement is self-executing constitutional architecture: the warrant regenerates with each statute and each sustaining precedent, and adjustment (rewriting rules, shifting categories) is always available. From the owner's and dealer's positions, the same structure arrives as a queue, a fee schedule, a discretionary denial, and a criminal statute. The excluded seat experiences something starker: definitional erasure — its claim is not denied on the merits but rendered inarticulate, while its conduct is still regulated. The engine computes these per-seat classifications from the structural data; this commentary explains why they diverge without adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the state_regulatory_apparatus (warrant-holder and receipt-seat for fees and discretion), organized_militia_institutions (occupies the designated protected position), and communities_affected_by_gun_violence (receives the security output without bearing compliance costs, sitting nearer the symmetric range than the apparatus because enforcement failures harm them directly). Victim declarations drive high directionality for individual_gun_owners and firearms_retailers_dealers, whose exit is constrained (comply, relocate at cost, or litigate) rather than mobile. Unorganized_militia_claimants are declared victims despite their excluded conversational role: exclusion from recognition does not shield them from the arrangement's costs (prohibited activity, failed claims), and their identity_lock pushes them toward the full-target end. Constitutional_scholars take the analytical seat and feed no directional arithmetic. Larger spatial scope (national) modestly amplifies effective extraction for the constrained payer seats by making verification and uniform compliance harder; suppression remains unscaled by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The genealogy interview exposes the arrangement's mandate problem plainly. The founding problem — independent state armed capacity as counterweight to a standing army — died in 1903 when the Militia Act federalized the militia into the National Guard; corroborating testimony comes from military historians and the Guard's own command records, not from the arrangement's beneficiaries. The reading persisted by re-describing 'collective security' as general public-safety regulation, a purpose the founding generation would not recognize as the militia clause's function. The expected mismatch (founding_problem_status=dead with disappearance_verdict=world_rearranges) is the capture/zombie signature: the arrangement persists because the apparatus that benefits from it depends on it, not because the founding function needs it. The hybrid classification prevents both symmetrical errors: a pure-coordination framing would hide the apparatus's accumulating rents behind the genuine screening function, and a pure-extraction framing would erase the real security services that communities_affected_by_gun_violence actually receive and politically demand. The structure is both at once, held together by active enforcement — which is why requires_active_enforcement is declared and why the enforcement-build-up series matters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the collective_security_reading of kernel second_amendment_text; do the sibling readings (individual_right_reading, originalist_civic_virtue_reading) instantiate structurally different constraints, and where exactly is the disagreement located?',
    'Doctrinal analysis locating the dispute in the logical force assigned to the prefatory militia clause: this reading makes it conditional (the right exists only insofar as arms-bearing serves organized civic defense); the individual_right_reading makes the operative clause stand alone; the originalist_civic_virtue_reading reads the founding-era militia as the universal armed citizenry. Each sibling is a separate story with its own epsilon, beneficiary/victim sets, and classification.',
    'Under the individual_right_reading the state regulatory apparatus loses its constitutional warrant and becomes a constrained actor rather than a beneficiary; under the civic virtue reading the protected class shifts toward an armed citizen body and licensing regimes face stricter scrutiny. This story''s classification holds only for this reading and must not be averaged across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested constitutional kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    militia_function_obsolescence,
    'Does organized civic defense remain a live function on which a constitutional right could be conditioned, given the National Guard''s subordination to federal command?',
    'Institutional analysis of the Guard''s dual state-federal command structure, mobilization records, and whether any independent state-level armed capacity persists that the right could meaningfully condition upon.',
    'If the function is dead, the reading''s persistence is mandate-survival and its transfer component is unsanctioned by any live coordination purpose; if a residual form is alive, part of the measured transfer prices a real service and the coordination share is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_function_obsolescence, empirical, 'Whether the founding coordination function survives in any operational form.').

omega_variable(
    discretion_security_efficiency_gap,
    'How much of the regulatory discretion this reading licenses produces measurable collective-security outcomes versus arbitrary or symbolic administration (may-issue arbitrariness, grandfathered prohibitions with little effect)?',
    'Outcome studies of permitting regimes, harm data where regimes tightened or loosened, and audit studies of licensing discretion across issuing authorities.',
    'A wide gap raises effective extraction and pushes the computed type toward snare; a narrow gap enlarges the genuine coordination share and stabilizes the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_security_efficiency_gap, empirical, 'Share of licensed regulatory discretion that purchases real security.').

omega_variable(
    post_heller_state_level_persistence,
    'After federal repudiation of the collective-security-only frame (2008-2010), does this reading persist as an operative constraint at the state level, and with what remaining transfer?',
    'Survey of state constitutional provisions, state court doctrine, and surviving may-issue and prohibition regimes after the federal doctrinal shift.',
    'Persistence would drive theater_ratio upward (maintenance of a repudiated frame) while concentrating remaining transfer in holdout jurisdictions; full displacement would date this constraint''s death at the federal level and confine it to historical analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_heller_state_level_persistence, empirical, 'Sub-national survival of the reading after federal repudiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_security_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sa_collective_security_tr_t0, observed).
narrative_ontology:measurement(sa_collective_security_tr_t6, second_amendment_text__collective_security_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(sa_collective_security_tr_t6, observed).
narrative_ontology:measurement(sa_collective_security_tr_t12, second_amendment_text__collective_security_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(sa_collective_security_tr_t12, observed).
narrative_ontology:measurement(sa_collective_security_tr_t18, second_amendment_text__collective_security_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(sa_collective_security_tr_t18, observed).
narrative_ontology:measurement(sa_collective_security_tr_t24, second_amendment_text__collective_security_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(sa_collective_security_tr_t24, observed).
narrative_ontology:measurement(sa_collective_security_tr_t30, second_amendment_text__collective_security_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(sa_collective_security_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sa_collective_security_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(sa_collective_security_be_t0, observed).
narrative_ontology:measurement(sa_collective_security_be_t6, second_amendment_text__collective_security_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(sa_collective_security_be_t6, observed).
narrative_ontology:measurement(sa_collective_security_be_t12, second_amendment_text__collective_security_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(sa_collective_security_be_t12, observed).
narrative_ontology:measurement(sa_collective_security_be_t18, second_amendment_text__collective_security_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement_basis(sa_collective_security_be_t18, observed).
narrative_ontology:measurement(sa_collective_security_be_t24, second_amendment_text__collective_security_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(sa_collective_security_be_t24, observed).
narrative_ontology:measurement(sa_collective_security_be_t30, second_amendment_text__collective_security_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(sa_collective_security_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_security_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sa_collective_security_su_t0, observed).
narrative_ontology:measurement(sa_collective_security_su_t6, second_amendment_text__collective_security_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(sa_collective_security_su_t6, observed).
narrative_ontology:measurement(sa_collective_security_su_t12, second_amendment_text__collective_security_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(sa_collective_security_su_t12, observed).
narrative_ontology:measurement(sa_collective_security_su_t18, second_amendment_text__collective_security_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(sa_collective_security_su_t18, observed).
narrative_ontology:measurement(sa_collective_security_su_t24, second_amendment_text__collective_security_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(sa_collective_security_su_t24, observed).
narrative_ontology:measurement(sa_collective_security_su_t30, second_amendment_text__collective_security_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(sa_collective_security_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Second Amendment.' The single ratified text is one kernel, but it instantiates three structurally distinct constraints: this collective-security reading (militia clause conditions the right; state regulation warranted; epsilon ~0.65, tangled_rope-shaped), the individual_right_reading (operative clause stands alone; personal self-defense core; different beneficiary/victim sets and materially different epsilon), and the originalist_civic_virtue_reading (founding-era universal armed citizenry; citizen-soldier capacity protected). Each story carries its own epsilon, stakeholders, and classification; forcing one story to span all three would make epsilon observable-dependent, violating epsilon-invariance. Historically this reading defined the interpretive battlefield the siblings fought on; since 2008 the individual-right reading exerts reverse pressure on this one's shrinking domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
