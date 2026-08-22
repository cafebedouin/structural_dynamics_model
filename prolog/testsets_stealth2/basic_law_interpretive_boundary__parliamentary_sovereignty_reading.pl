% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Basic Law Interpretive Boundary — Parliamentary Sovereignty Reading
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   Kernel-reading story: this file instantiates the
 *   parliamentary_sovereignty_reading of the contested
 *   basic_law_interpretive_boundary kernel. The arrangement under
 *   classification: the Knesset, as elected sovereign and continuing
 *   constituent authority dating to the 1950 Harari compromise, holds
 *   ultimate power to interpret and amend the Basic Laws by simple majority,
 *   including express power to override judicial review; judicial
 *   constitutional pronouncements are advisory pending legislative
 *   disposition. The epsilon referent is THIS arrangement, assessed by the
 *   reading's own lights: democratic self-rule converts most policy burden
 *   into self-government, leaving acknowledged extraction concentrated in the
 *   treaty-obligation friction the reading itself exempts from override and
 *   in the unentrenched exposure of electoral minorities pending coalition
 *   realignment. Per the epsilon-invariance decomposition, the colloquial
 *   label 'who decides what the Basic Laws mean' covers three structurally
 *   distinct claims; the sibling readings (judicial_supremacy_reading:
 *   judicial invalidation binds the Knesset; balanced_contestation_reading:
 *   bounded mutual authority) are separate constraint files with their own
 *   epsilon, beneficiary structures, and classifications, linked through the
 *   network family. KEY AGENTS (by structural relationship): -
 *   knesset_governing_coalitions: agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — administers the boundary it sits inside -
 *   majority_electorate_blocs: beneficiaries (organized/mobile) — receive
 *   policy alignment without institutional counterweight -
 *   electoral_minority_groups: primary targets (moderate/constrained) — bear
 *   unblockable costs with no institutional recourse -
 *   individual_rights_claimants: primary targets (powerless/trapped) — lost
 *   their only domestic forum - supreme_court_justices: excluded seat
 *   (institutional/identity_locked) — review function denied, professional
 *   identity fused to it - opposition_parties: dual-positioned payers
 *   (organized/constrained) — bear costs now, may wield identical power later
 *   - treaty_partner_states_and_bodies: observers of the one external
 *   constraint the reading concedes - comparative_constitutional_scholars:
 *   analytical observers — outside corroboration for the genealogy
 *
 * KEY AGENTS:
 *   - knesset_governing_coalitions: agenda_setter (institutional/arbitrage) — sets and enforces the interpretive boundary by majority vote
 *   - majority_electorate_blocs: beneficiary (organized/mobile) — policy rents flow to winning coalitions' supporters
 *   - electoral_minority_groups: payer (moderate/constrained) — chronic losers with no institutional veto point
 *   - individual_rights_claimants: payer (powerless/trapped) — the judicial forum is closed by design
 *   - supreme_court_justices: excluded (institutional/identity_locked) — assert review, hold none under this reading
 *   - opposition_parties: payer with secondary beneficiary position (organized/constrained) — position tracks proximity to power
 *   - treaty_partner_states_and_bodies: observer (institutional/analytical) — the conceded external constraint
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — genealogy corroboration from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.53).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Basic Law Interpretive Boundary — Parliamentary Sovereignty Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'a452160e-3eaa-4b2d-bee6-14edd0bdd828').
narrative_ontology:cs_kernel_codification('a452160e-3eaa-4b2d-bee6-14edd0bdd828', fixed_text).
narrative_ontology:cs_authority_grounding('a452160e-3eaa-4b2d-bee6-14edd0bdd828', practice).
narrative_ontology:cs_reading_relation('a452160e-3eaa-4b2d-bee6-14edd0bdd828', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a452160e-3eaa-4b2d-bee6-14edd0bdd828', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('a452160e-3eaa-4b2d-bee6-14edd0bdd828', foundational, constituent_authority_vested_in_elected_chamber).
narrative_ontology:cs_axiom_status(constituent_authority_vested_in_elected_chamber, holdable).
narrative_ontology:cs_axiom_grounding('a452160e-3eaa-4b2d-bee6-14edd0bdd828', constituent_authority_vested_in_elected_chamber, conventional).
narrative_ontology:cs_axiom('a452160e-3eaa-4b2d-bee6-14edd0bdd828', foundational, no_external_veto_on_legislative_will).
narrative_ontology:cs_axiom_status(no_external_veto_on_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('a452160e-3eaa-4b2d-bee6-14edd0bdd828', no_external_veto_on_legislative_will, deontological).
narrative_ontology:cs_reference_frame('a452160e-3eaa-4b2d-bee6-14edd0bdd828', knesset_continuing_constituent_authority).
narrative_ontology:cs_drift_state('a452160e-3eaa-4b2d-bee6-14edd0bdd828', contemporary_judicial_reform_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a452160e-3eaa-4b2d-bee6-14edd0bdd828', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majority_electorate_blocs).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_minority_groups).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, individual_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, opposition_parties).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, opposition_parties).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes ordinary legislation and Basic Law amendments by simple majority, interprets Basic Laws through legislation, and can enact override clauses nullifying adverse rulings. Constrained only by coalition arithmetic and by the treaty commitments the reading itself exempts from override. Any institutional limit that inconveniences a majority can be rewritten by that majority, which is what exit looks like from this seat: rule-changing rather than rule-leaving.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions, agenda_setter,
    institutional, biographical, arbitrage, national).

% Voting blocs whose preferences become law without institutional counterweight when their side wins: draft arrangements, settlement policy, judicial appointment composition, and sectoral funding flow as coalition deliverables. They collect policy alignment without administering the boundary themselves and can shift allegiance between elections, which keeps their position comfortable rather than captive.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majority_electorate_blocs, beneficiary,
    organized, biographical, mobile, national).

% Citizens — Arab citizens, left-liberal constituencies, and other blocs chronically outside coalitions — whose votes rarely translate into governing power. They bear policies no domestic institution lets them block: their remedies are street protest, appeal to treaty bodies, or waiting for coalition realignment that historically seldom arrives. Emigration is the only permanent exit and carries existential cost for those with deep roots in the country.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_minority_groups, payer,
    moderate, generational, constrained, national).

% Litigants whose recourse was the Supreme Court's review of legislation conflicting with Basic Laws. Under this reading their petitions yield advisory opinion at most, confirmable or dismissible by the same majority that produced the legislation. There is no second domestic forum; each case terminates where the coalition's will terminates, and relief arrives only if an unrelated electoral wave rescues them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, individual_rights_claimants, payer,
    powerless, immediate, trapped, national).

% Assert authority, consolidated since the mid-1990s, to invalidate legislation violating Basic Laws. Under this reading their rulings bind no one pending legislative disposition — they are reclassified as advisory. Their professional identity is constituted by the review function the reading denies; exit means resignation or accommodation, and they continue issuing rulings the governing coalition publicly labels provisional.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, excluded,
    institutional, generational, identity_locked, national).

% Blocked from policy now, aware that the same simple-majority machinery will be theirs whenever they next win. Their stance on the boundary tracks proximity to power more than principle: they resist override clauses while excluded and quietly preserve the option of using them in government. Their costs are real but discounted by the expectation of reversal.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, opposition_parties, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, opposition_parties, beneficiary).

% Counterparties to the conventions the reading itself concedes bind the sovereign. They monitor compliance, issue findings, and attach diplomatic consequences, but cannot legislate domestically. They matter to this story because the treaty carve-out is the one place the reading admits the Knesset is a target rather than a source of authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, treaty_partner_states_and_bodies, observer,
    institutional, generational, analytical, global).

% Map the Israeli dispute onto weak-form and strong-form review typologies, document the Harari-compromise genealogy, and testify in hearings. They collect nothing and pay nothing; their function in this story is outside corroboration — the founding problem and its status are attested from seats that do not benefit from the arrangement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single terminal decision point for constitutional interpretation: disputed questions of Basic Law meaning are settled by the elected chamber's vote, avoiding inter-branch deadlock, giving voters a clear locus of responsibility, and letting a state without an agreed constitution produce authoritative law continuously.
% TRANSFER_FUNCTION: Moves interpretive authority and veto power over fundamental law from courts — and from the minorities who would invoke them — to whichever coalition commands a temporary Knesset majority; concretely, it moves policy outcomes (draft obligations, land policy, rights protections, judicial composition) from losing groups to winning ones, with treaty obligations the sole reverse flow.
% ABSENT_VOICES: Permanent electoral minorities would object and are present only as voters who almost never swing coalition formation; future generations would object and are seated nowhere, since simple-majority revisability means no one speaks for them; the judiciary would object and is present but reclassified as advisory. All three objections are registered outside the room — in protest, in treaty findings, in dissenting opinions labeled provisional.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if ultimate interpretive and amendatory authority relocated to the courts or a constituent body — governance would rearrange immediately: governing coalitions would lose unilateral control of their legislative agenda, pending and recent legislation would become vulnerable to invalidation, the treaty-compliance calculus would shift, the 2023-2025 protest and counter-mobilization architecture would lose its object, and every sectoral arrangement currently sustained by majority arithmetic would face an institution with independent veto power.
% FOUNDING_PROBLEM: The new state, deadlocked between religious and secular visions of a written constitution, adopted the 1950 Harari compromise: defer constitution-making to the Knesset itself, acting as a continuing constituent authority legislating chapter-by-chapter Basic Laws, with the chamber retaining ultimate interpretive authority over what it had enacted. The founding problem was how to make authoritative fundamental law without an agreed higher-order text, answered by locating all constituent and interpretive authority in the elected chamber.
% FOUNDING_PROBLEM_CORROBORATION: The vacuum-filling origin is corroborated from outside the beneficiary set: legal-historical scholarship documents the Harari compromise, and the Supreme Court's own 1990s jurisprudence attests that the constitutional vacuum persisted for decades. Whether the problem remains LIVE is attested mainly by the governing coalition itself, which cites the absence of an agreed constitution; opposition legal briefs and academic commentary from outside the benefiting parties overwhelmingly attest the original problem is solved and that the arrangement now functions as majority convenience — that asymmetry in corroboration is itself signal.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.22, reading-indexed: by this reading's own lights the arrangement is substantially non-extractive — costs imposed on losing voters are legitimate democratic outcomes subject to electoral correction — with residual extraction acknowledged only in the treaty carve-out and the accumulating, visible burden on groups that never enter coalitions. Suppression (0.53) is structural, not physical: the arrangement works by closing the judicial alternative forum, not by coercing bodies; the electoral channel remains open, which caps suppression below snare levels. Theater ratio (0.38) rises across the interval because the sovereignty claim increasingly detaches from practice — after 1995 the Knesset legislates in full knowledge that rulings may nullify ordinary legislation, then re-legislates or threatens override, making portions of the sovereignty performance rather than operation; the 2023-2025 dip reflects partial reassertion through the reasonableness-limitation episode. Accessibility collapse (0.55): once the arrangement is understood, the domestic judicial alternative is closed, but electoral turnover, treaty bodies, and extra-parliamentary mobilization persist as partial alternatives. Resistance (0.78) is among the highest recorded for a constitutional allocation: mass protest, reservist refusal threats, business-sector intervention, and unified opposition all met the arrangement's recent consolidation. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine. All three tracked series run on one shared seven-point grid (1948, 1968, 1992, 1995, 2005, 2023, 2025) so no metric row borrows an end-state value from another. The claimed type (tangled_rope) is stated from structural analysis — genuine coordination function (terminal decision point, accountability, avoidance of inter-branch deadlock) plus asymmetric extraction (unentrenched minority exposure through the same simple-majority rule) plus active enforcement (override machinery) — independently of these metric values; where per-seat computed types diverge from the claim, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the coalition seat, the arrangement is near-pure coordination it built and can exit by rewriting (arbitrage-grade exit, subsidized directionality) — a rope. From the individual rights claimant seat, the same structure is enforced extraction with no exit — approaching snare. From the electoral minority seat, extraction is real but buffered by the electoral-correction hope, landing at tangled territory. The Court seat experiences the constraint as exclusion of its constitutive function — neither collecting nor paying in the transfer sense, but bearing an identity-level cost the transfer metrics do not capture. Opposition parties straddle: today's payers are tomorrow's potential agenda-setters, which is precisely why they resist entrenchment solutions that would bind their own future majorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: knesset_governing_coalitions and majority_electorate_blocs sit near the beneficiary pole (d near 0), with the coalition pushed further toward subsidy by its arbitrage exit — it can amend any Basic Law that inconveniences it. Victims push the opposite way: individual_rights_claimants (trapped, powerless) sit nearest the full-target pole; electoral_minority_groups (constrained, moderate) sit slightly inside them because electoral turnover and emigration preserve thin alternatives. The Court is excluded rather than coordinated — its exclusion is the enforcement object itself, mirroring how rival payment networks function in marketplace constraints. Treaty bodies observe the single carve-out where the reading concedes the sovereign is a target: the directionality of the coalition seat inverts locally on treaty matters, which is why epsilon cannot approach zero even on the reading's own account.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Labeling the arrangement a snare fails because the coordination function is genuine: democratic decision-making requires a terminal point, and the reading supplies one with real accountability properties — pure-extraction framing would erase the legitimate majoritarian core that distinguishes this from predatory structures. Labeling it a rope fails because the same simple-majority rule that empowers winners strips losers of recourse with no entrenchment floor — asymmetric extraction rides the coordination channel. Tangled rope holds both facts. On obsolescence: the founding problem (authoritative law without an agreed constitution) is largely resolved as a vacuum problem, but the arrangement persists because fixing is prohibitive for the only fixer — no majority entrenches limits on itself. Watch the piton drift path: if enforcement activity continues converting into theater (re-legislation cycles, override threats never executed) while the closure function grows more contested, the constraint migrates toward maintained performance rather than operative sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the basic_law_interpretive_boundary kernel correctly locates the boundary — this parliamentary sovereignty reading, the judicial supremacy reading, or the balanced contestation reading?',
    'A settled constituent process, entrenched constitutional text, or durable political settlement that fixes the override question; until then, track enacted override clauses and Court responses.',
    'If the judicial supremacy reading prevails, this constraint''s beneficiary and victim sets invert: the coalition becomes the target of binding invalidation and rights claimants become subsidized beneficiaries. Classification for every seat recomputes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling readings are separate constraints with different epsilon and victim sets.').

omega_variable(
    treaty_carve_out_extent,
    'How extensively do international treaty obligations actually constrain the sovereign under this reading, given that the reading itself exempts them from override?',
    'Track instances where coalition legislation collides with ratified treaty commitments (occupation-related obligations, rights conventions) and whether the Knesset overrides, complies, or reinterprets.',
    'Sets the floor of extraction the reading itself acknowledges: a hard treaty constraint keeps epsilon above zero and gives external bodies leverage; a porous carve-out collapses the residual toward the reading''s near-zero claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_carve_out_extent, empirical, 'Extent of the one external constraint the parliamentary sovereignty reading concedes.').

omega_variable(
    entrenchment_flexibility_valence,
    'Is simple-majority revisability of Basic Laws a coordination feature (democratic flexibility, error correction) or the primary extraction vector (transient majorities rewriting fundamentals affecting those who cannot block them)?',
    'Comparative analysis of weak-form versus strongly entrenched constitutional systems on minority-outcome stability measures across electoral cycles.',
    'If flexibility dominates, the coalition seat computes nearer rope; if revision tracks majority convenience at minority expense, the tangled_rope reading hardens toward snare at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_flexibility_valence, conceptual, 'Valence of the absence of entrenchment: coordination flexibility versus extraction channel.').

omega_variable(
    minority_exit_realism,
    'Do electoral minority groups possess realistic exit — electoral turnover, coalition realignment, emigration — or is their coded exit option generously overstated?',
    'Longitudinal data on coalition formation (how often minority-preferred blocs enter governments), emigration rates among affected populations, and protest-to-policy conversion rates.',
    'If exit is thinner than coded, payer-seat directionality moves toward the full-target pole and effective extraction at those seats rises sharply; if turnover is real, the electoral-correction defense of the reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_realism, empirical, 'Whether constrained exit accurately describes electoral minorities'' alternatives.').

omega_variable(
    enforcement_counterfactual_baseline,
    'Is active enforcement intrinsic to this reading''s arrangement, or is the current enforcement demand an artifact of the judicial supremacy reading''s consolidation — would the arrangement self-execute under a deferential court as it did before 1995?',
    'Compare weak-form review jurisdictions (New Zealand, pre-HRA United Kingdom) where legislative supremacy operates without override machinery against the Israeli enforcement build-up.',
    'If enforcement is artifact, requires_active_enforcement is contingent on the sibling contest and the constraint''s type is unstable across the kernel dispute; if intrinsic, the tangled_rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_counterfactual_baseline, empirical, 'Counterfactual enforcement need of the arrangement absent a rival reading''s consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t1948, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1968, 0.1).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t1968, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.14).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t1992, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t1995, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t2005, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t2023, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_tr_t2025, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.08).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t1948, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1968, 0.1).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t1968, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.12).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t1992, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t1995, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2005, 0.17).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t2005, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2023, 0.21).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t2023, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_be_t2025, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2025, 0.22).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.05).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t1948, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1968, 0.07).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t1968, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.12).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t1992, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t1995, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t2005, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t2005, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t2023, observed).
narrative_ontology:measurement(blib_parliamentary_sovereignty_su_t2025, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2025, 0.53).
narrative_ontology:measurement_basis(blib_parliamentary_sovereignty_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'who decides what the Basic Laws mean' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (parliamentary_sovereignty_reading) authors low reading-indexed epsilon (~0.22) for its arrangement — democratic self-rule legitimating majoritarian outcomes, residual extraction confined to treaty friction and unentrenched minority exposure. The judicial_supremacy_reading file authors epsilon for a counter-majoritarian guardianship arrangement with its own beneficiary structure (rights claimants subsidized, coalitions targeted). The balanced_contestation_reading file authors intermediate epsilon for a bounded mutual-authority arrangement. Upstream/downstream: the Harari-compromise genealogy (this reading's lineage) is cited as historical evidence by both siblings, so this story influences the others' legitimacy conditions even where it forecloses one of them logically. All three files cross-link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
