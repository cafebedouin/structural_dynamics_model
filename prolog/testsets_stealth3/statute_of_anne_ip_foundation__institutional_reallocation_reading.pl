% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne (1710) — Institutional Reallocation Reading: Guild-to-Author Rights Transfer
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   In 1710 Parliament enacted the Statute of Anne, vesting exclusive rights
 *   in printed books first in authors for limited terms, transferable by
 *   assignment, with compulsory registration at Stationers' Hall and
 *   penalties for unauthorized reprinting. This story instantiates the
 *   institutional_reallocation_reading of the statute_of_anne_ip_foundation
 *   kernel: the statute's decisive fact was that it reallocated existing
 *   rights — dissolving the Stationers' Company's perpetual corporate
 *   monopoly and changing who occupied the institutional space, with rights
 *   flowing through authors to the booksellers who bought assignments. KEY
 *   AGENTS (by structural relationship): parliament_legislature authored the
 *   reallocation and retains amendment power (institutional/arbitrage);
 *   london_booksellers receive the assigned rights and the arrangement's
 *   gains accrue to them (organized/arbitrage); working_authors are the
 *   vesting point of the reallocation (moderate/mobile);
 *   stationers_company_monopoly_holders lost the corporate monopoly
 *   (organized/identity_locked); perpetual_stock_shareholders saw their
 *   perpetual claims extinguished (powerful/constrained);
 *   provincial_reprint_trades were criminalized outsiders
 *   (organized/constrained); reading_public receives deferred access
 *   (powerless/constrained); westminster_enforcement_courts define the
 *   registry's legal meaning (institutional/constrained). Family
 *   decomposition note: this file authors epsilon only for the reallocation
 *   arrangement as this reading assesses it (referent: the standing 1710-1774
 *   rights regime seen as an occupancy change); the
 *   conceptual_emergence_reading file authors its own epsilon for the
 *   concept-creation claim, and the entangled_event_reading file a single
 *   epsilon over the undecomposed event. Sibling edges run through
 *   network.affects_constraints; no epsilon is averaged or hedged across
 *   readings in this file.
 *
 * KEY AGENTS:
 *   - - parliament_legislature: Agenda setter (institutional/arbitrage) — authored the reallocation and retains amendment power
 *   - - london_booksellers: Primary beneficiary (organized/arbitrage) — receives assigned rights; the arrangement's gains accrue here
 *   - - working_authors: Nominal first holders (moderate/mobile) — the vesting point through which rights pass
 *   - - stationers_company_monopoly_holders: Primary target (organized/identity_locked) — dissolved corporate monopoly
 *   - - perpetual_stock_shareholders: Secondary target (powerful/constrained) — extinguished perpetual claims
 *   - - provincial_reprint_trades: Criminalized outsider (organized/constrained) — suppressed reprint channel
 *   - - reading_public: Diffuse beneficiary (powerless/constrained) — term expiry and price ceilings
 *   - - westminster_enforcement_courts: Enforcement interpreter (institutional/constrained) — defines the registry's legal meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.62).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) — Institutional Reallocation Reading: Guild-to-Author Rights Transfer").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'ed2d29f3-31ab-439d-8338-ae298f9a588b').
narrative_ontology:cs_kernel_codification('ed2d29f3-31ab-439d-8338-ae298f9a588b', formalized).
narrative_ontology:cs_authority_grounding('ed2d29f3-31ab-439d-8338-ae298f9a588b', lineage).
narrative_ontology:cs_interpretation_layer_present('ed2d29f3-31ab-439d-8338-ae298f9a588b').
narrative_ontology:cs_reading_relation('ed2d29f3-31ab-439d-8338-ae298f9a588b', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed2d29f3-31ab-439d-8338-ae298f9a588b', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('ed2d29f3-31ab-439d-8338-ae298f9a588b', foundational, print_rights_preexisted_the_statute).
narrative_ontology:cs_axiom_status(print_rights_preexisted_the_statute, holdable).
narrative_ontology:cs_axiom_grounding('ed2d29f3-31ab-439d-8338-ae298f9a588b', print_rights_preexisted_the_statute, empirically_contingent).
narrative_ontology:cs_axiom('ed2d29f3-31ab-439d-8338-ae298f9a588b', foundational, decisive_effect_was_occupancy_change).
narrative_ontology:cs_axiom_status(decisive_effect_was_occupancy_change, holdable).
narrative_ontology:cs_axiom_grounding('ed2d29f3-31ab-439d-8338-ae298f9a588b', decisive_effect_was_occupancy_change, empirically_contingent).
narrative_ontology:cs_axiom('ed2d29f3-31ab-439d-8338-ae298f9a588b', secondary, assignment_routed_rights_to_sellers).
narrative_ontology:cs_axiom_status(assignment_routed_rights_to_sellers, holdable).
narrative_ontology:cs_axiom_grounding('ed2d29f3-31ab-439d-8338-ae298f9a588b', assignment_routed_rights_to_sellers, empirically_contingent).
narrative_ontology:cs_reference_frame('ed2d29f3-31ab-439d-8338-ae298f9a588b', guild_monopoly_to_author_property_reallocation).
narrative_ontology:cs_drift_state('ed2d29f3-31ab-439d-8338-ae298f9a588b', post_donaldson_becket_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed2d29f3-31ab-439d-8338-ae298f9a588b', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, working_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, perpetual_stock_shareholders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, provincial_reprint_trades).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1710 statute after the licensing regime lapsed: set the fourteen and twenty-one year terms, retained compulsory entry of copies at Stationers' Hall, prohibited imported reprints, and answered the Stationers' monopoly-restoration petitions with an author-vested alternative. Retains the power to amend or repeal; the 1735 perpetual-copyright bill passed the Commons here before rejection in the Lords.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% The London wholesale booksellers who petitioned for trade protection in 1705-1710 and then purchased authors' rights under the new terms. They finance editions, hold assigned copyrights as trade assets, and from the 1730s campaign for perpetual terms to protect their portfolios. The arrangement's gains accrue here: assignment converts author-vested rights into seller-held property, and their position inside the new frame lets them buy, sell, and collateralize rights freely.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers, agenda_setter).

% The class into which exclusive rights were first vested: they can now sell compositions as property and receive lump sums from booksellers, but few hold the bargaining power to retain rights or share in ongoing returns, and most sign outright assignments at first publication. Patronage, periodical writing, and outright sale remain the practical alternatives open to them.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, working_authors, beneficiary,
    moderate, biographical, mobile, national).

% The Company's leadership and members whose corporate privilege — perpetual rights over copies entered at the Hall under the old licensing regime — the statute dissolved. The Company keeps the registry function but loses the income stream and the legal basis of its monopoly. Its officers spend decades defending the old perpetual claims in court and in Parliament, treating the corporate custody of the trade's order as constitutive of who they are.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly_holders, payer,
    organized, generational, identity_locked, national).

% Booksellers holding shares in pre-1710 perpetual copyrights — the English Stock and old copy assignments. The statute grants their holdings a twenty-one year grace term, after which their titles fall to competitors unless terms are extended. Their wealth and parliamentary access nearly carry the 1735 perpetuity bill; the 1774 court decision finally extinguishes the perpetual claim. Their capital is sunk in the shares, leaving litigation and lobbying as the main levers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, perpetual_stock_shareholders, payer,
    powerful, generational, constrained, national).

% Scottish and Irish printers who built a reprint trade on the post-1695 vacuum; the statute's infringement penalties and import prohibition criminalize their principal line of business. Edinburgh and Dublin shops defy enforcement for decades, supplying cheap editions to English readers through smuggling channels while lacking access to the London market's legitimate channels.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, provincial_reprint_trades, payer,
    organized, biographical, constrained, regional).

% Buyers of books: nominally protected during terms by statutory price ceilings (loosely enforced) and promised unrestricted access once terms expire. Cheap contraband reprints compete with authorized editions throughout the period. The access that term expiry creates arrives here, diffusely and late, and the seat has no organization to press its interests during the term years.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    powerless, generational, constrained, national).

% The London courts, Chancery foremost, that adjudicate infringement actions, ownership disputes over registered copies, and — decisively in 1774 — the question of whether the statutory terms exhaust the right. Their rulings define what the Hall's registry entries legally mean, and they operate inside the bounds of the enacted text and accumulated precedent.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, westminster_enforcement_courts, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced the collapsed licensing regime with a uniform, enforceable frame for the book trade: fixed ownership terms, a compulsory registry operated as before at Stationers' Hall, defined transfer of rights by assignment, and court-enforced infringement penalties. It solved the post-1695 problem that no legal basis existed anywhere in the kingdom for controlling unauthorized reprinting.
% TRANSFER_FUNCTION: Moves exclusive printing-and-vending rights, and the returns attached to them: vested first in authors for limited terms, then by assignment to booksellers. Simultaneously it extinguishes the Stationers' Company's perpetual corporate privilege over existing copies, transferring occupancy of the rights-space from guild to author-to-seller chains. During terms it moves readers' payments to rights-holders; after terms it releases works to unrestricted copying.
% ABSENT_VOICES: Working authors supplied the statute's rhetoric but held no drafting seat; provincial Scottish and Irish printers, criminalized by the import and infringement provisions, were entirely outside the London consultation; readers and booksellers unconnected to the Company had no representation. Parliamentary journals record the incorporated trade's petitions dominating the submissions Parliament received.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand every assignment contract, void the registry's legal meaning, and return the trade to the post-1695 condition of contested piracy and guild privilege. The entire literary-property market — ownership chains, copyrights held as collateral, term-based edition pricing — is built on the statute's frame and would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695 the London book trade had no legal protection against unauthorized reprinting, and the Stationers' pressed for restoration of their monopoly; Parliament needed a settlement that secured the trade while answering the anti-monopoly critique that had killed the Act's renewal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: contemporaneous parliamentary journals and the Lords' 1774 debate record attest both the trade-protection origin and the learning-encouragement justification; modern book-trade historiography independently reconstructs the 1695-1710 lobbying sequence. No account of the founding problem rests solely on testimony from within the bookseller beneficiary set.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement redistributes rather than maximally takes — it destroyed one return stream (corporate perpetuity) while constructing another (term-limited assignable copyrights that concentrate in seller hands), and it embeds a genuine release valve in term expiry. The temporal series shows the characteristic arc: extraction climbs as booksellers consolidate assignments and press perpetuity bills (peak near t=32-40, the height of the 1730s-1750s campaign to convert the grace term into restored perpetuity), then eases after the 1774 decision confirms statutory limits. Suppression 0.62: criminalized reprinting, the import prohibition, and compulsory registration are raw structural properties of the arrangement, authored unscaled — adjusting for power or scope is the engine's computation, not the author's. Theater_ratio 0.30: the encouragement-of-learning framing progressively covers trade-rent defense, but the registry, courts, and term machinery do real work throughout. Accessibility_collapse 0.45: alternatives persist — contraband channels, patronage, litigation, parliamentary appeal — so understanding the arrangement does not close the option set. Resistance 0.60: Scottish defiance, six decades of perpetuity litigation, and the near-passage of the 1735 bill show sustained active opposition. Claimed type tangled_rope is authored from the reading's own structure — a genuine coordination frame carrying an asymmetric reallocation with named losers — independently of the metric values, which are authored from the descriptive record.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently from the same statute. From the london_booksellers seat the arrangement is a property frame they helped design and profit inside; from the stationers_company_monopoly_holders seat it is the dissolution of a corporate inheritance they regard as constitutive — their identity_locked exit reflects institutional identity fusion, where the Company's self-concept as custodian of the trade's order was bound to perpetual corporate rights, making exit unthinkable and producing sixty years of litigation rather than adaptation. The same-level contrast is sharp: london_booksellers and provincial_reprint_trades sit at the same organized power in the same trade, yet the London seat holds arbitrage-grade mobility inside the new frame while the provincial seat is criminalized outside it — the statute, not global standing, differentiates their exits. Inter-institutionally, parliament authored, the courts reinterpreted (settling the limits question in 1774 against the shareholders' reading), and the Company administered a registry it no longer profited from.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: london_booksellers (declared beneficiary, arbitrage exit) sit near the beneficiary end; stationers_company_monopoly_holders, perpetual_stock_shareholders, and provincial_reprint_trades (declared victims, trapped-to-constrained exits) sit near the target end, with identity lock and sunk capital pushing the first two toward full-target; reading_public sits near symmetric (deferred access against term-time price exposure); parliament and the courts sit near-symmetric as administrators. One override is authored: the moderate seat (working_authors, the only moderate-power agent in this story) is overridden to d=0.32. The derivation would read declared-beneficiary plus mobile exit as near-full beneficiary (d roughly 0.1), but the vesting point is transient — rights pass through authors to sellers within months of publication, and the realized benefit is the lump sum, not a standing subsidy. The override encodes that conduit position; without it the engine would overweight the author seat's subsidization.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Calling the statute a pure rope ignores that its coordination frame carried an asymmetric reallocation with identifiable losers — the Company's dissolved monopoly, the shareholders' extinguished claims, the criminalized provincial trades. Calling it a snare ignores the genuine post-1695 coordination problem it solved and the public-domain accrual written into its terms. The founding problem is contested rather than dead: trade protection and learning encouragement were fused at founding and pulled apart over the interval, and the parties still dispute which problem the arrangement actually solves. The mandate has transformed rather than expired — by 1774 the limits question was settled judicially while the occupancy question (who durably holds the reallocated rights) remained live — so mandatrophy is not declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the statute_of_anne_ip_foundation kernel — the institutional_reallocation_reading. Which structural facts do the sibling readings relocate, and where exactly does the disagreement sit?',
    'Compare the compiled sibling stories: conceptual_emergence_reading relocates the decisive fact onto concept creation (the beneficiary set shifts toward the learning public and the Stationers'' loss becomes incidental), while entangled_event_reading refuses the decomposition and authors a single epsilon over both dimensions. The disagreement is located in the decisive-fact question: occupancy change (this file) versus concept creation versus inseparability.',
    'If the entangled framing is adopted, this file''s epsilon and the emergence file''s epsilon measure one constraint twice and the family must merge; if the emergence framing dominates, the Stationers''-monopoly victim declaration loses force and the computed classification moves toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the Statute-of-Anne kernel this story instantiates and where sibling readings diverge.').

omega_variable(
    preexisting_rights_extent,
    'How extensive were the Stationers'' pre-1710 rights in fact — durable corporate property, or licensing-dependent privileges that died with the Licensing Act?',
    'Systematic analysis of Stationers'' Hall entrance registers and Licensing Act records: continuity of claimed rights across the 1695-1710 gap, and whether post-lapse trade conduct treated old copies as owned property.',
    'If pre-existing rights were thin or licensing-contingent, the reallocation reading''s referent weakens — the statute approaches creation rather than reallocation, the framing shifts toward the emergence sibling, and the Stationers''-as-target declaration loses force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preexisting_rights_extent, empirical, 'Whether guild print rights genuinely pre-existed as property for the statute to reallocate.').

omega_variable(
    author_benefit_or_conduit,
    'Were working authors genuine net gainers from the vesting, or rhetorical instruments through which bookseller interests secured a new property form?',
    'Author-contract and earnings records for 1710-1770: assignment timing, lump-sum levels relative to manuscript-market alternatives, and whether any authors with bargaining power retained rights or shared in ongoing returns.',
    'If authors were pure conduits, they leave the beneficiary set, their directionality moves toward the target end, and the arrangement reads as bookseller capture wearing authorial dress — pushing computed types toward snare at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_benefit_or_conduit, empirical, 'Whether the author-vesting step delivered real benefit or served as transfer cover.').

omega_variable(
    reader_price_incidence,
    'Did readers bear materially elevated prices during copyright terms, or did statutory price ceilings and contraband competition hold consumer costs near competitive levels?',
    'Price series for authorized versus contraband editions, 1710-1774, controlling for production-cost trends.',
    'If term-time prices were materially inflated, reading_public belongs in the victim set as well and effective extraction widens beyond the trade-internal reallocation; if not, the public seat stays purely beneficiary and the extraction remains intra-trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_price_incidence, empirical, 'Whether the reading public bore term-time costs or only enjoyed deferred access gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soa_inst_reallocation_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(soa_inst_reallocation_tr_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(soa_inst_reallocation_tr_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(soa_inst_reallocation_tr_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(soa_inst_reallocation_tr_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(soa_inst_reallocation_tr_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(soa_inst_reallocation_tr_t48, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 48, 0.29).
narrative_ontology:measurement(soa_inst_reallocation_tr_t56, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 56, 0.3).
narrative_ontology:measurement(soa_inst_reallocation_tr_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 64, 0.3).

% Extraction over time
narrative_ontology:measurement(soa_inst_reallocation_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(soa_inst_reallocation_be_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(soa_inst_reallocation_be_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(soa_inst_reallocation_be_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(soa_inst_reallocation_be_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(soa_inst_reallocation_be_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(soa_inst_reallocation_be_t48, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 48, 0.61).
narrative_ontology:measurement(soa_inst_reallocation_be_t56, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 56, 0.59).
narrative_ontology:measurement(soa_inst_reallocation_be_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 64, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soa_inst_reallocation_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(soa_inst_reallocation_su_t8, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(soa_inst_reallocation_su_t16, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(soa_inst_reallocation_su_t24, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(soa_inst_reallocation_su_t32, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(soa_inst_reallocation_su_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(soa_inst_reallocation_su_t48, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 48, 0.63).
narrative_ontology:measurement(soa_inst_reallocation_su_t56, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(soa_inst_reallocation_su_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 64, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the statute_of_anne_ip_foundation kernel label into three structurally distinct claims, per the epsilon-invariance principle: this file carries the institutional dimension (who occupied the rights-space before and after 1710, and where the gains landed); conceptual_emergence_reading carries the conceptual dimension (whether copyright as a limited regulatory tool was newly created); entangled_event_reading carries the refusal of decomposition (one event, one epsilon). Each file keeps a single stable epsilon; the sibling edges document the family so contamination and merger analyses can traverse it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, moderate, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
