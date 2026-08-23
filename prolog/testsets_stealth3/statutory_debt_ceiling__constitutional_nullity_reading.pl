% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutional Nullity (Section 4 Supersession Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel and authors
 *   only that reading. The kernel is the statutory debt ceiling: the
 *   aggregate legal limit on federal borrowing first consolidated in 1939.
 *   The reading instantiated here, the constitutional nullity reading, holds
 *   that the ceiling possesses no operative legal force: Fourteenth Amendment
 *   Section 4 guarantees the validity of the public debt and forbids
 *   questioning it, so a statute that authorizes obligations through
 *   appropriations and then bars the borrowing needed to honor them is void,
 *   and Treasury executes borrowing as appropriations require. On this
 *   reading the standing arrangement (the statute plus the recurring
 *   authorization ritual around it) extracts nothing, coerces nothing, and
 *   persists as ceremony maintained by institutional inertia. The claim and
 *   the metrics are authored independently: claimed_type states the structure
 *   this reading asserts (an atrophied, theatrically maintained remnant); the
 *   metrics describe the arrangement's actual operation as this reading
 *   assesses it. This file belongs to a three-story constraint family
 *   decomposing the colloquial label 'debt ceiling'; the sibling files
 *   instantiate the coordination-scaffold and extraction-snare readings, and
 *   the family linkage is recorded in network.affects_constraints. No other
 *   reading's epsilon is averaged, hedged, or described here. KEY AGENTS (by
 *   structural relationship): - congress: agenda-setter and incidental
 *   beneficiary (institutional/mobile) -- writes, suspends, and ritually
 *   re-authorizes the limit; collects symbolic fiscal-oversight credit from a
 *   limit that does not bind - treasury_department: incidental beneficiary
 *   with payer residue (institutional/constrained) -- executes borrowing per
 *   appropriations; absorbs planning costs during standoff windows -
 *   bondholders_and_money_markets: payer (organized/mobile) -- absorb
 *   episodic repricing risk that reverses on each suspension -
 *   federal_program_agencies: payer (moderate/constrained) -- carry recurring
 *   contingency-planning costs for delays that never arrive -
 *   leverage_faction_legislators: payer (organized/identity_locked) -- invest
 *   political capital in a leverage strategy this reading holds void -
 *   general_public: payer (powerless/trapped) -- bears diffuse attention and
 *   uncertainty costs of the recurring ritual - judicial_branch: excluded
 *   (institutional/analytical) -- the seat able to confirm or deny the
 *   nullity claim abstains from adjudicating it - future_taxpayers: excluded
 *   (powerless/trapped) -- inherit the debt stream the ritual decorates;
 *   unrepresented in it - constitutional_legal_scholarship: observer
 *   (analytical/analytical) -- develops and contests the Section 4
 *   supersession argument
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.03).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.06).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutional Nullity (Section 4 Supersession Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b').
narrative_ontology:cs_kernel_codification('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', formalized).
narrative_ontology:cs_authority_grounding('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', lineage).
narrative_ontology:cs_interpretation_layer_present('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b').
narrative_ontology:cs_reading_relation('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', foundational, section_four_public_debt_validity_supreme).
narrative_ontology:cs_axiom_status(section_four_public_debt_validity_supreme, holdable).
narrative_ontology:cs_axiom_grounding('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', section_four_public_debt_validity_supreme, conventional).
narrative_ontology:cs_axiom('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', secondary, borrowing_follows_appropriations_necessarily).
narrative_ontology:cs_axiom_status(borrowing_follows_appropriations_necessarily, holdable).
narrative_ontology:cs_axiom_grounding('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', borrowing_follows_appropriations_necessarily, instrumental).
narrative_ontology:cs_reference_frame('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', section_four_debt_validity_supremacy).
narrative_ontology:cs_drift_state('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', contemporary_brinkmanship_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bfd116ed-165f-4e9c-ab04-ac8a3cbb1d6b', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_money_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, federal_program_agencies).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, leverage_faction_legislators).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, section_four_public_debt_validity_supremacy).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, appropriations_governed_issuance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the statutes that set an aggregate limit on federal borrowing, and periodically suspends or raises it. Each cycle it debates, stages votes, and passes suspension measures, acquiring visible ownership of fiscal decisions at minimal cost. It could delete the limit by ordinary legislation at any time and has not; the recurring ritual continues under its control.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress, beneficiary).

% Runs federal cash operations and debt auctions, issuing whatever securities appropriations require. During standoff windows it deploys accounting maneuvers, drafts contingency plans, and studies emergency authorities it has never invoked. It gains uninterrupted market access so long as the limit does not bind, and it absorbs the planning burden whenever the ritual intensifies.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, payer).

% Holds Treasury securities priced continuously in global markets. During standoff windows prices wobble, insurance against short-term default ticks up, and auction calendars draw scrutiny; each suspension restores prior levels. Exposure can be reduced quickly by selling or hedging, so the burden arrives as transient repricing rather than lasting loss.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_money_markets, payer,
    organized, biographical, mobile, global).

% Operates programs that depend on scheduled federal disbursements. Each standoff cycle they draft furlough plans, prioritize payments, and rehearse cash-rationing procedures; the rehearsals have so far never been executed. They cannot decline to prepare, since a missed cycle would be catastrophic, so the planning cost recurs regardless of outcome.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_program_agencies, payer,
    moderate, biographical, constrained, national).

% Campaigns to use the borrowing limit as bargaining leverage in budget negotiations, staging confrontations timed to the X-date. Its members' political identities and coalition standing are built around this strategy. Each round ends in suspension with the sought concessions diminished or absent, yet abandoning the instrument would mean dissolving the identity the strategy sustains, so the investment repeats.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, leverage_faction_legislators, payer,
    organized, biographical, identity_locked, national).

% Follows recurring headlines about possible default, absorbs the anxiety and attention costs of each manufactured deadline, and forms opinions on outcomes it cannot influence. No household has experienced an actual missed federal payment. Exit from the polity's fiscal arrangements is not available to individuals.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, general_public, payer,
    powerless, biographical, trapped, national).

% Receives petitions and litigations touching the borrowing limit, including challenges premised on the Fourteenth Amendment's public-debt clause, and has consistently declined to adjudicate them. Its abstention leaves the legal question open and keeps it outside a conversation it alone could conclude.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, judicial_branch, excluded,
    institutional, generational, analytical, national).

% Will service interest on the debt that appropriations law creates. No mechanism represents them in the authorization ritual, and the limit neither restricts nor releases the issuance they will inherit; their stake is registered nowhere in the recurring debate.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Publishes the competing analyses: arguments that the public-debt clause renders the borrowing limit legally superseded, and counter-arguments defending the limit's validity and Congress's authority over borrowing. Neither camp's view has been adopted by any adjudicating institution, so the scholarship documents the dispute without settling it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_legal_scholarship, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None presently operable. The arrangement's original procedural purpose -- consolidating borrowing authority so the Treasury need not obtain per-issue approval -- is superseded: appropriations law governs issuance, and on this reading Section 4 forecloses any limiting function. Stated without evaluation: the only activity the arrangement still organizes is its own recurring authorization ritual.
% TRANSFER_FUNCTION: Nothing material moves through the arrangement under this reading: no funds, work, or obligations transfer by virtue of the limit, because it does not bind. Residual movements are episodic and reputational -- political capital and public attention are consumed during standoff windows, transient risk premia appear and reverse in Treasury markets, and symbolic fiscal-oversight credit accrues to the body that votes.
% ABSENT_VOICES: The judiciary is the loudest absent voice: it holds the authority to confirm or deny the nullity claim and has systematically declined every invitation to adjudicate it, leaving the conversation about the limit's force to proceed without the only participant empowered to settle it. Future taxpayers who will service the debt are likewise unrepresented in the ritual. Bond-market participants are present only through pricing, never as seats.
% DISAPPEARANCE_RATIONALE: On this reading the limit already fails to bind: Treasury borrows as appropriations require, every standoff resolves in suspension before any default point, and no arrangement depends on the limit's operation. Deleting the statute overnight would end the ritual and its episodic uncertainty collateral; issuance, payments, auction mechanics, and market function would continue unchanged. Only the ceremony and its noise disappear.
% FOUNDING_PROBLEM: When federal debt finance became continuous, Congress consolidated its scattered per-issue bond limits into a single aggregate ceiling (1939) so the Treasury could manage issuance without returning for approval on each offering -- a procedural delegation of borrowing mechanics that preserved a nominal congressional checkpoint.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Congressional Research Service histories of the ceiling document the per-issue-approval burden the 1917/1939 revisions removed, and fiscal-history scholarship on the Liberty Loan Acts independently describes the consolidation's original purpose. Notably, no external source attests the founding problem as still live; the corroborating record attests it as historical, which is itself the signal that the mandate has outlived its function.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.03 because this reading holds the limit legally inoperative: no transfer moves through it, suspensions always precede any default point, and the residual figure registers micro-frictions (systems that reference the limit, scoring conventions) rather than extraction. Suppression is 0.06: nothing is coerced, and the small figure reflects the normative expectation that Congress act before the X-date, not structural force. Theater_ratio is 0.78, the story's center of gravity: authorization votes that do not bind, brinkmanship choreography that resolves in suspension, and suspension acts packaged under unrelated policy names are performance; the residual functional share is the record-keeping and auction mechanics that would occur identically without the limit. Accessibility_collapse is 0.10 because understanding the nullity collapses no alternative -- the alternative (borrowing per appropriations) is already the operative reality. Resistance is 0.25 and attaches to the nullity CLAIM rather than to the arrangement's operation: courts decline to confirm it, a legislative faction insists the limit binds, and scholarship contests it; the arrangement itself, being inoperative, provokes almost no resistance. The three measurement series share one eight-point grid (1939-2025) as the alignment rule requires. Extractiveness stays near the floor throughout, with a small 2011 elevation (0.09) that this reading attributes to standoff collateral -- transient risk-premium and planning noise that reversed on resolution -- not to binding transfer; an author of the snare reading for the same calendar years would draw a sharply different curve, which is the family's point. Suppression_requirement is authored because the story traces enforcement-capacity decay: mild normative force in the routine-raise era decaying into open ceremony, with a brief 2011 uptick when leverage was attempted and failed. The trajectories are monotonic rather than cyclical; the oscillation seen in headline attention is a property of the political news cycle, not of the arrangement's operation, and is not modeled as an extraction mechanism here.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the congress seat the arrangement is a ritual it owns: it sets and suspends the limit, and the exercise yields visible ownership of fiscal decisions at near-zero cost. From the treasury seat the same arrangement is background weather: an obligation to manage around episodic standoffs, borne by an actor that cannot leave its post. Among same-power institutional actors, congress and treasury diverge on exit, not rank: congress can repeal the statute outright (mobile), while treasury must operate inside whatever framework the other branches recognize (constrained) -- constraint-specific factors, not global standing, drive the difference. The payer seats divide by exit quality: mobile bondholders reprice and move on; trapped citizens simply endure the attention economy of the ritual. The leverage-faction seat is the sharpest divergence: its members' political identity is fused to the leverage strategy, so they keep investing in an instrument this reading holds void -- identity-lock, not incapacity, explains the persistence of their participation. The judicial seat's abstention means no seat's experience validates the limit as binding; the divergence that matters is between those who perform the arrangement and those who pay its noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Congress sits near the beneficiary end (declared beneficiary, agenda-setter, mobile exit): the arrangement subsidizes its symbolic oversight role. Treasury sits low-to-moderate: declared incidental beneficiary (unimpeded market access) with a payer residue (standoff management), and constrained exit pulls it off the pure-beneficiary pole. The declared victims derive high directionality scaled by exit: trapped citizens sit nearest the full-target end, constrained agencies next, and mobile bondholders are damped toward symmetry because repricing lets them shed exposure quickly. The leverage faction, though not listed in the victims array, is a payer seat with identity-locked exit and correspondingly high directionality -- it bears the arrangement's costs in the currency of wasted political capital. The decisive structural fact is that epsilon is near zero, so high directionality multiplies against almost nothing: every seat's effective extraction is negligible, which is the arithmetic signature of a legally inoperative shell maintained as ceremony. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- consolidating borrowing authority so Treasury need not seek per-issue approval -- is dead: appropriations law now governs issuance comprehensively, and on this reading Section 4 removes any residual limiting function. External corroboration (CRS histories, fiscal-history accounts of the 1917/1939 revisions) attests the problem as historical; no outside source attests it as live. The classification prevents mislabeling in both directions: calling the arrangement a snare would credit extraction this reading holds never occurs (the leverage faction's threats do not execute); calling it a rope would credit a coordination function that is superseded (nothing is coordinated that appropriations law does not already govern). The piton claim names the residue precisely: a function atrophied to zero, maintained theatrically, with no seat capturing material value. The receipt surface sharpens the open question: gain_flow is authored 'diffuse' (no seat captures; congress's take is a reputational byproduct of the ritual, not received transfer, and bondholder losses are transient mark-to-market noise that reverses on resolution) and fixing_cost 'cheap' (repeal is a single statute imposing no operational adjustment, since Treasury already borrows per appropriations). Cheap-plus-diffuse reads as transient neglect rather than entrenched piton -- which is exactly this reading's live prediction: the arrangement should dissolve into formal obsolescence. Whether it does, or persists indefinitely as ceremony, is carried by the formal_obsolescence_vs_indefinite_theater omega rather than resolved by tuning the type claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (constitutional_nullity_reading) of the statutory_debt_ceiling kernel; where exactly is the disagreement among the three declared readings located, and what would adopting a sibling change structurally?',
    'Locate the disputed element: whether the statutory ceiling possesses operative legal force given Fourteenth Amendment Section 4. A court adjudication, an authoritative published Treasury or DOJ legal doctrine, or formal repeal would resolve which reading describes the standing arrangement.',
    'Adopting the coordination_scaffold_reading would assign the ceiling a live coordination function and nonzero baseline extraction; adopting the extraction_snare_reading would assign high epsilon with identified victims and enforcement dependence. This file''s zero-extraction, high-theater profile is valid only within the nullity reading; cross-reading epsilon comparison is meaningless by construction because the readings instantiate different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of three readings of the debt-ceiling kernel; the disagreement is located in operative legal force.').

omega_variable(
    nullity_unconfirmed_by_courts,
    'No court has affirmed Section 4 nullity and Treasury officially disclaims unilateral borrowing action. Does any authoritative adjudication confirm the voidness this reading asserts?',
    'A courtroom resolution of a Zivotovsky-style challenge, or an explicit published Treasury/DOJ doctrine adopting Section 4 supremacy as operative authority.',
    'Confirmation collapses the theater (theater_ratio approaches its ceiling before obsolescence) and hardens this reading; denial revives the operative-force readings and invalidates this file''s near-zero epsilon assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nullity_unconfirmed_by_courts, empirical, 'Institutional confirmation status of the nullity claim.').

omega_variable(
    legal_voidness_vs_political_potency,
    'Does constitutional voidness extinguish political leverage, or can a legally void ceiling retain threat credibility, and thus extraction capacity, because participants discount the nullity claim?',
    'Observe concession outcomes in standoff windows under conditions where both sides know the Section 4 backstop exists; game-theoretic analysis of credible threats known to be void.',
    'If potency survives voidness, effective extraction is nonzero during standoff windows and this reading''s zero-delta understates chi; the hybrid (void in law, potent in politics) would sit between this file and the snare sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_voidness_vs_political_potency, conceptual, 'Whether legal nullity entails political impotence.').

omega_variable(
    formal_obsolescence_vs_indefinite_theater,
    'Will the void ceiling be formally repealed or lapse into desuetude, or persist indefinitely as ceremonial infrastructure?',
    'Legislative tracking: repeal bill introductions, lengthening suspension durations, quiet omission of ceiling language from suspension acts, and CRS/GAO treatment of the limit''s status.',
    'Repeal removes the arrangement from the corpus (resolving the cheap-plus-diffuse receipt cell as transient neglect); indefinite persistence entrenches the piton profile and keeps the theater''s collateral costs on the ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_obsolescence_vs_indefinite_theater, empirical, 'Trajectory of a legally void but ritually maintained statute.').

omega_variable(
    retroactive_voidness_scope,
    'Does this reading hold the ceiling void from enactment (Section 4 dates to 1868 and precedes every ceiling statute) or void only from the point the supremacy argument gains institutional acceptance?',
    'Doctrinal analysis of the nullity literature''s temporal claims, compared against how the sibling scaffold reading dates the ceiling''s functional life.',
    'Retroactive voidness strengthens this reading''s foreclosure of the scaffold sibling (no era in which the ceiling validly coordinated); prospective-only voidness weakens foreclosure toward coexistence and would admit a historical coordination ledger into this constraint''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_voidness_scope, conceptual, 'Temporal scope of the asserted nullity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1939, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_nullity_tr_t1939, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1939, 0.35).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1939, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1955, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1955, 0.4).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1955, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1971, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1971, 0.48).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1971, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1985, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1985, 0.58).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1985, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1997, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1997, 0.62).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1997, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.75).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2011, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2019, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2019, 0.72).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2019, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.78).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sdc_nullity_be_t1939, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1939, 0.08).
narrative_ontology:measurement_basis(sdc_nullity_be_t1939, observed).
narrative_ontology:measurement(sdc_nullity_be_t1955, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1955, 0.07).
narrative_ontology:measurement_basis(sdc_nullity_be_t1955, observed).
narrative_ontology:measurement(sdc_nullity_be_t1971, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1971, 0.07).
narrative_ontology:measurement_basis(sdc_nullity_be_t1971, observed).
narrative_ontology:measurement(sdc_nullity_be_t1985, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1985, 0.06).
narrative_ontology:measurement_basis(sdc_nullity_be_t1985, observed).
narrative_ontology:measurement(sdc_nullity_be_t1997, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1997, 0.06).
narrative_ontology:measurement_basis(sdc_nullity_be_t1997, observed).
narrative_ontology:measurement(sdc_nullity_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.09).
narrative_ontology:measurement_basis(sdc_nullity_be_t2011, observed).
narrative_ontology:measurement(sdc_nullity_be_t2019, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2019, 0.05).
narrative_ontology:measurement_basis(sdc_nullity_be_t2019, observed).
narrative_ontology:measurement(sdc_nullity_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.03).
narrative_ontology:measurement_basis(sdc_nullity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sdc_nullity_su_t1939, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1939, 0.18).
narrative_ontology:measurement_basis(sdc_nullity_su_t1939, observed).
narrative_ontology:measurement(sdc_nullity_su_t1955, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1955, 0.16).
narrative_ontology:measurement_basis(sdc_nullity_su_t1955, observed).
narrative_ontology:measurement(sdc_nullity_su_t1971, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1971, 0.15).
narrative_ontology:measurement_basis(sdc_nullity_su_t1971, observed).
narrative_ontology:measurement(sdc_nullity_su_t1985, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1985, 0.13).
narrative_ontology:measurement_basis(sdc_nullity_su_t1985, observed).
narrative_ontology:measurement(sdc_nullity_su_t1997, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1997, 0.11).
narrative_ontology:measurement_basis(sdc_nullity_su_t1997, observed).
narrative_ontology:measurement(sdc_nullity_su_t2011, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement_basis(sdc_nullity_su_t2011, observed).
narrative_ontology:measurement(sdc_nullity_su_t2019, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2019, 0.08).
narrative_ontology:measurement_basis(sdc_nullity_su_t2019, observed).
narrative_ontology:measurement(sdc_nullity_su_t2025, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2025, 0.06).
narrative_ontology:measurement_basis(sdc_nullity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'debt ceiling' covers three structurally distinct claims that this family separates per the epsilon-invariance principle: (1) this file -- the limit as a constitutionally void remnant whose legal force is superseded by Fourteenth Amendment Section 4, extracting nothing and persisting as ceremony; (2) statutory_debt_ceiling__coordination_scaffold_reading -- the limit as a procedural coordination mechanism with a live or historically live coordination function; (3) statutory_debt_ceiling__extraction_snare_reading -- the limit as a weaponized boundary through which a legislative minority extracts concessions under default threat. The readings assign radically different epsilon to the same calendar years (this file: near-zero throughout; the snare reading: spiking in standoff years), which is precisely why they are separate constraints rather than one constraint with a measurement parameter. This file links both siblings. The upstream/downstream citation pattern runs from the scaffold reading's historical account (cited as evidence the limit once worked) into the other two readings' disputes about its present force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
