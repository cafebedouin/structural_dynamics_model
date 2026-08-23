% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary - Balanced Contestation Reading
 *   domain: constitutional law/comparative constitutionalism/judicial review theory
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel: the
 *   balanced-contestation reading of the interpretive boundary over Israel's
 *   Basic Laws. On this reading both branches hold legitimate but bounded
 *   authority - the court interprets within its jurisdictional domain, the
 *   legislature retains amendment supremacy constrained by international
 *   obligations and judicial-independence norms - and the boundary exists
 *   only as something continuously negotiated. The claim/metric split is
 *   deliberate: claimed_type records the tangled-rope structure I believe
 *   structurally true (genuine mutual-checking coordination plus genuine
 *   domain-variable extraction, held together only by active enforcement),
 *   while the metrics record the arrangement's observed operation on one
 *   shared nine-point grid from 1992 to 2025. The epsilon referent is the
 *   standing arrangement under contest - the negotiated boundary itself as
 *   this reading sees it - never either sibling reading's preferred
 *   arrangement.
 *
 * KEY AGENTS:
 *   - israeli_supreme_court: agenda-setting institutional seat (institutional/identity_locked) - administers the interpretive line, collects doctrinal authority, absorbs retaliatory pressure
 *   - knesset_governing_coalition: agenda-setting institutional seat (institutional/constrained) - controls amendment and override levers, pays in struck-down legislation
 *   - rights_dependent_minorities: protected constituency (moderate/constrained) - receives domain-variable protection, thick in civilian domains and thin in security domains
 *   - occupied_territories_residents: heaviest cost-bearing seat (powerless/trapped) - lives under the deferentially reviewed military orders with no seat in the exchange
 *   - israeli_citizenry: near-symmetric diffuse seat (organized/constrained) - collects constitutional stability, pays crisis-cycle costs
 *   - opposition_parties: excluded seat (moderate/constrained) - outside the triadic negotiation until elections rotate them in
 *   - international_human_rights_bodies: external monitor (institutional/mobile) - supplies argument and reputational pressure, holds no internal lever
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) - documents the gap between declared balance and observed dominance patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.48).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary - Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional law/comparative constitutionalism/judicial review theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'ac3bea1c-6486-4d66-a680-c679634b5b0b').
narrative_ontology:cs_kernel_codification('ac3bea1c-6486-4d66-a680-c679634b5b0b', fixed_text).
narrative_ontology:cs_authority_grounding('ac3bea1c-6486-4d66-a680-c679634b5b0b', distributed).
narrative_ontology:cs_reading_relation('ac3bea1c-6486-4d66-a680-c679634b5b0b', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac3bea1c-6486-4d66-a680-c679634b5b0b', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ac3bea1c-6486-4d66-a680-c679634b5b0b', foundational, neither_branch_may_capture_interpretive_authority).
narrative_ontology:cs_axiom_status(neither_branch_may_capture_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('ac3bea1c-6486-4d66-a680-c679634b5b0b', neither_branch_may_capture_interpretive_authority, deontological).
narrative_ontology:cs_axiom('ac3bea1c-6486-4d66-a680-c679634b5b0b', foundational, legislative_supremacy_subject_to_real_constraints).
narrative_ontology:cs_axiom_status(legislative_supremacy_subject_to_real_constraints, holdable).
narrative_ontology:cs_axiom_grounding('ac3bea1c-6486-4d66-a680-c679634b5b0b', legislative_supremacy_subject_to_real_constraints, conventional).
narrative_ontology:cs_reference_frame('ac3bea1c-6486-4d66-a680-c679634b5b0b', balanced_branch_authority).
narrative_ontology:cs_drift_state('ac3bea1c-6486-4d66-a680-c679634b5b0b', post_2023_overhaul_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ac3bea1c-6486-4d66-a680-c679634b5b0b', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_citizenry).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, rights_dependent_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, occupied_territories_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws, decides which ordinary laws fall short of them, and sets the proportionality standards that define where the interpretive line sits in practice. Collects doctrinal authority and institutional prestige from each round of the exchange, and pays when coalitions respond with appointment delays, budget pressure, or override proposals. Its members' professional self-conception is fused with the guardianship role adopted after 1992; stepping back from it would require conceding three decades of doctrine was mistaken.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_supreme_court, beneficiary).

% Passes ordinary legislation, amends Basic Laws by simple majority, and disciplines the court through override bills, appointment composition, and selective compliance. Gains legislative latitude wherever the court defers and deterrence value from the standing threat of amendment. Pays when flagship legislation is struck down and when international partners treat non-compliance as reputational cost. Cannot leave the constitutional game without undermining the democratic legitimacy it claims.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalition, payer).

% Arab citizens, LGBTQ communities, religious minorities, and asylum seekers who rely on court invalidation for protection when governing coalitions are hostile. Protection is thick in civilian rights domains and thin in security and demographic domains, so what they receive depends heavily on which policy area the exchange touches. Organized advocacy gives them litigation capacity but no vote-blocking power over a determined coalition.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, rights_dependent_minorities, payer,
    moderate, biographical, constrained, national).

% Palestinians in the West Bank governed by military orders that the court reviews with marked deference. They hold no Knesset franchise and no seat in the branch-to-branch exchange, so the security-domain tilt of the arrangement lands on them with no offsetting channel. Exit means leaving home; staying means living under the deferentially reviewed orders.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, occupied_territories_residents, payer,
    powerless, generational, trapped, regional).

% Receives the everyday goods of a working constitutional order: predictable rules, peaceful transfers of power, and a check that runs in both directions between the elected and appointed arms of government. Bears the diffuse costs of recurring crisis cycles - polarization, protest mobilization, and the uncertainty each override attempt injects into pending policy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_citizenry, payer).

% Would contest both the court's doctrine and the coalition's override threats, but the ongoing negotiation runs among the sitting coalition, the court, and the executive. They hold no seat until elections rotate them into government, and their objections enter mainly through protest and litigation support rather than the bargaining table.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, opposition_parties, excluded,
    moderate, biographical, constrained, national).

% Treaty committees and transnational legal networks monitor compliance with the obligations cited in domestic litigation. Their findings supply arguments to court petitioners and reputational pressure on coalitions, but they command no enforcement lever inside the system and cannot move the line themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_bodies, observer,
    institutional, generational, mobile, global).

% Document the exchange's operation, compare it to weak-form review arrangements elsewhere, and track the gap between the declared balance and the observed dominance patterns across policy domains. Their analyses shape elite opinion but bind no actor.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates interpretive authority over the Basic Laws between an unelected court and an elected legislature: the court supplies rights-protective review and doctrinal continuity, the legislature supplies democratic legitimacy and amendment capacity, and the contested boundary lets each check the other without either capturing the constitutional text outright.
% TRANSFER_FUNCTION: Moves interpretive control and veto power over legislation between branches by policy domain: invalidation moves legislative output toward the court in rights-salient domains; deference and override threats move discretion back toward the executive in security and fiscal domains; the recurring contest itself moves public attention and legitimacy between the branches.
% ABSENT_VOICES: Opposition parties have no seat in the ongoing negotiation, which runs among the sitting coalition, the court, and the executive; they would object to both the court's doctrine and the coalition's override threats. Occupied-territories residents, governed by the deferentially reviewed military orders, lack both Knesset representation and any channel into the exchange; they would object most sharply to the security-domain tilt, and their objection is structurally unheard.
% DISAPPEARANCE_RATIONALE: If the negotiated boundary vanished overnight, the constitutional order would immediately rearrange around whichever reading captured the vacuum: pending invalidations would either become final or become void, coalition legislative programs would either accelerate or stall, and minority protection would either collapse to political tolerance or harden into court-final doctrine. Arrangements across legislation, litigation, and protest all currently presuppose the contested middle.
% FOUNDING_PROBLEM: When the 1992 Basic Laws created a higher-order legal layer, no settlement accompanied it on who interprets that layer. The founding problem: how to give an elected sovereign's statutes a rights-protective check without transferring final authority away from the electorate - how to bound both branches at once.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative constitutional scholars treat the Israeli arrangement as a live instance of the general weak-form-review problem; the 2023 mass-protest leadership explicitly framed the dispute as an unresolved founding question rather than a routine policy fight; and international treaty bodies continue to press the obligations half of the problem in periodic reviews. No participant claims the question is settled.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 because the burden is real but episodic and domain-variable: legislative programs absorb invalidations in rights-salient areas, occupied-territories residents absorb deference in security areas, and neither burden is systematic across all domains - the mid-range figure is the honest aggregate, with the bimodality risk carried by an omega. Suppression is 0.38: contestation is open and procedural (override bills, petitions, protest are all legal and frequent), but the game is closed - there is no constituent assembly, no third branch, no exit from the constitutional framework - and that closure is structural rather than internalized, except on the court's side where doctrinal self-commitment adds an internalized element. Theater is 0.25: most moves are functional (rulings, amendments, appointments), while symbolic override bills and sovereignty declarations supply a growing performative share that spiked in 2023. Accessibility_collapse is 0.45: understanding the boundary does not collapse alternatives, since amendment, supermajority-building, doctrinal litigation, and domain-shopping all remain workable. Resistance is 0.70 because contestation IS the operating mode of this arrangement - the boundary is never passively accepted by anyone. The three measurement series run on one shared nine-point grid so every metric is authored at every examined time point. The trajectory is cyclical rather than monotonic: gradual accumulation through the 2000s peak, adaptive relaxation through the 2010s as coalitions learned to avoid triggers, then the 2023 crisis spike (overhaul legislation, mass protest, wartime suspension) followed by partial settlement. The oscillation here is not itself the extraction mechanism - each crisis re-teaches both branches the other's limits, which has been stabilizing - but the 2023 values are a genuine enforcement-intensity event, not noise. Gain_flow is authored as diffuse on an affirmative check of every seat: gains land alternately on the two institutional seats depending on domain and period (the court converts deference and invalidation into doctrinal capital; the coalition converts override threats and deference into legislative latitude), and no seat durably captures the flow - the oscillation is the reading's defining feature. Fixing_cost is prohibitive: resolving the boundary requires either a constitutional rewrite or one side's decisive victory, and the 2023 attempt priced that cost publicly.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat the arrangement is legitimate guardianship under escalating political attack; from the coalition's seat it is an unelected veto imposed on an electoral mandate; from the rights-dependent minorities' seat it is a line of protection with variable thickness - robust in civilian domains, thin exactly where they are most exposed; from the occupied territories' seat the celebrated dialogue is largely invisible, because deference is the operative fact and they hold no seat in any branch. Same structure, four different experienced types. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The court is declared under beneficiaries (collects doctrinal authority) with exit identity_locked - its fusion with the post-1992 guardianship role means it cannot reposition without an identity break, which pulls its derived directionality up from the pure-beneficiary end because it also absorbs retaliation. The coalition is declared under beneficiaries with a payer secondary role - it collects deterrence and latitude but pays in struck-down legislation, and its constrained exit (it cannot abandon the constitutional game without destroying its own legitimacy claim) keeps it from arbitrage-grade relief. The citizenry sits near symmetric: genuine coordination benefit, diffuse crisis cost. Rights-dependent minorities sit near the target end, moderated by their litigation capacity; occupied-territories residents sit nearest the full-target end - powerless, trapped, and outside the negotiation entirely. No directionality_overrides are authored: the schema keys overrides on the power atom alone, and the two agenda-setting seats share the institutional atom, so any single override would mis-specify one of them along with the international monitors. The asymmetry between the two institutional seats is carried instead by their differing declarations and exit conditions, which the derivation chain reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - bounding both branches at once after 1992 created a higher-law layer without an interpreter settlement - is still live, so no mandatrophy is declared and none is due. The tangled-rope classification does the anti-mislabeling work in both directions: against the rope over-call (a pure-coordination reading would erase the domain-variable burdens falling on those with the least voice) and against the snare over-call (a pure-extraction reading would erase the genuine mutual checking each branch imposes on the other, which the 2023 crisis showed citizens will defend in the street). The decay omega tracks the one mandatrophy-relevant risk: if the dialogue decays into one-sided dominance, this arrangement becomes transitional scaffolding for a sibling reading's capture rather than a steady-state balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_reading_delta,
    'This constraint instantiates the balanced_contestation_reading of the basic_law_interpretive_boundary kernel; how would classification shift if practice consolidated under the judicial_supremacy_reading or the parliamentary_sovereignty_reading instead?',
    'Observe which reading captures practice across the next Basic Law amendment cycle: binding-and-unreviewed invalidation consolidates the supremacy reading; a successful override or amendment-by-simple-majority consolidates the sovereignty reading; continued alternation sustains this reading.',
    'Under the supremacy reading, extraction concentrates on legislative majorities and the court''s seat moves firmly to the beneficiary end; under the sovereignty reading, minority-protection extraction rises sharply and the court''s seat moves toward the payer end. Either consolidation replaces this constraint with a structurally different one rather than moving this one''s metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_reading_delta, conceptual, 'Committer structure: sibling readings of the same kernel instantiate different constraints with different epsilon values and victim structures.').

omega_variable(
    epsilon_domain_bimodality,
    'Does the aggregate extractiveness of 0.48 mask a bimodal profile - near-coordination in civilian rights domains and near-full extraction in security and occupation domains?',
    'Decompose the record by policy domain: tabulate invalidations, deferences, and standing-review outcomes separately for civilian rights, security, fiscal, and occupation-related matters.',
    'If bimodal, the single-story figure understates the burden on occupied-territories residents and overstates it in civilian domains; the constraint family should be decomposed into per-domain stories linked through the network, per the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_domain_bimodality, empirical, 'Whether one aggregate epsilon hides domain-separated regimes within the same arrangement.').

omega_variable(
    dialogue_or_dominance_drift,
    'Is the branch-to-branch exchange genuinely bidirectional, or is it drifting toward one-sided dominance - court deference accumulating through the 2010s, coalition override pressure peaking in 2023?',
    'Track invalidation rates, override-bill introductions, appointment-composition shifts, and Basic Law amendment frequency over the coming decade; sustained one-directional movement indicates decay of the balance.',
    'Sustained decay would recast the arrangement as transitional - a phase ending in one sibling reading''s capture - changing persistence expectations and the weight given to the enforcement measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dialogue_or_dominance_drift, empirical, 'Whether the balance is stable, oscillating, or decaying toward a sibling reading''s capture.').

omega_variable(
    international_obligations_binding_force,
    'Do the international obligations and judicial-independence norms that nominally bound the legislature exert real constraint, or are they rhetorical cover that costs a determined coalition nothing to ignore?',
    'Compare enacted legislation against treaty-body objections and measure whether cited obligations ever decided a legislative outcome; examine whether reputational costs altered any coalition''s behavior.',
    'If the obligations are non-binding in practice, the balance tilts toward the parliamentary-sovereignty reading and the extraction borne by rights-dependent constituencies rises; if binding, the declared balance is materially real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligations_binding_force, empirical, 'Whether the external half of the balance has operative force or is decorative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(basi_tr_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(basi_tr_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(basi_tr_t2012, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(basi_tr_t2017, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(basi_tr_t2021, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(basi_tr_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(basi_be_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(basi_be_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2006, 0.5).
narrative_ontology:measurement(basi_be_t2012, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2012, 0.46).
narrative_ontology:measurement(basi_be_t2017, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(basi_be_t2021, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2021, 0.4).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(basi_be_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(basi_su_t1995, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1995, 0.36).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(basi_su_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2006, 0.44).
narrative_ontology:measurement(basi_su_t2012, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(basi_su_t2017, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2017, 0.36).
narrative_ontology:measurement(basi_su_t2021, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2021, 0.34).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2023, 0.62).
narrative_ontology:measurement(basi_su_t2025, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the Basic Laws' covers three structurally distinct arrangements. This file instantiates the balanced-contestation reading only: bounded authority on both sides, epsilon mid-range and domain-variable. The judicial-supremacy reading (binding invalidation, court-final) and the parliamentary-sovereignty reading (simple-majority amendment and override, Knesset-final) are separate constraint stories with their own epsilon values and victim structures; all three are linked through network.affects_constraints as one constraint family. Influence runs through practice in both directions: each reading cites episodes generated under the others as evidence for its own account, and consolidation of any one reading rewrites the operating environment of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
