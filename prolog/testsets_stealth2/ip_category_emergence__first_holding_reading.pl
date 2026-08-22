% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Statute of Anne Settlement: Occupancy Shift in the Rights-Holder Set (First-Holding Reading)
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   When the Licensing Act lapsed in 1695, the Stationers' Company's
 *   centuries-old monopoly over printing lost its statutory footing. The
 *   Statute of Anne (1710) rebuilt the book trade's legal order on a new
 *   occupancy: copy vested first in authors for limited terms, with a
 *   terminal date promising every work back to the common stock. This story
 *   instantiates the first_holding_reading of the ip_category_emergence
 *   kernel: what 1710 marks is a membership shift in an already-occupied
 *   legitimate claimant set — the enforcement beneficiary changed from the
 *   guild to statutorily vested authors and, through the assignment market
 *   that absorbed the vesting almost immediately, to the London booksellers
 *   who had lobbied for the statute. The metric profile is authored
 *   independently of the claim: the arrangement carried real coordination
 *   (secure titles, expanded output, per-work sunset) while rents
 *   concentrated in the assignee trade and suppression machinery was
 *   repurposed rather than dismantled. The epsilon referent is the standing
 *   post-1710 arrangement as this reading sees it — not the pre-1710 guild
 *   order and not any endorsed reform.
 *
 * KEY AGENTS:
 *   - - london_booksellers: primary beneficiary and de facto agenda-shaper (powerful/constrained) — collects assigned terms and term-length rents, lobbied the statute into shape, financed the perpetual-copyright litigation
 *   - - stationers_company: enforcement administrator with residual beneficiary position (organized/identity_locked) — register, policing, and trade discipline persist under new masters
 *   - - vested_authors: nominal first occupant of the rights-holder seat (moderate/constrained) — real gains for some, immediate assignment for most
 *   - - lump_sum_authors: bear the assignment discount (powerless/constrained) — upside on successful works accrues to the term-holder
 *   - - book_buying_public: diffuse price-bearers (powerless/constrained) — single authorized price during each term
 *   - - scottish_provincial_printers: excluded rivals working jurisdictional friction (organized/arbitrage) — prosecuted as pirates, litigating for decades
 *   - - public_domain_readers: deferred beneficiaries of the term-limit promise (powerless/constrained) — represented only by the sunset arithmetic
 *   - - parliament_of_england: enacting agenda-setter (institutional/constrained) — authored the bargain, amendable only against organized trade resistance
 *   - - historical_jurisprudence_scholars: analytical observer (analytical/analytical) — reconstructs actual occupancy from registers and case files
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.63).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.58).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne Settlement: Occupancy Shift in the Rights-Holder Set (First-Holding Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'fe588b0b-d5d0-4187-af2e-5216b5b4f967').
narrative_ontology:cs_kernel_codification('fe588b0b-d5d0-4187-af2e-5216b5b4f967', formalized).
narrative_ontology:cs_authority_grounding('fe588b0b-d5d0-4187-af2e-5216b5b4f967', lineage).
narrative_ontology:cs_interpretation_layer_present('fe588b0b-d5d0-4187-af2e-5216b5b4f967').
narrative_ontology:cs_reading_relation('fe588b0b-d5d0-4187-af2e-5216b5b4f967', ip_category_emergence__thinkability_reading, forecloses).
narrative_ontology:cs_reading_relation('fe588b0b-d5d0-4187-af2e-5216b5b4f967', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('fe588b0b-d5d0-4187-af2e-5216b5b4f967', foundational, claimant_set_preexisted_1710).
narrative_ontology:cs_axiom_status(claimant_set_preexisted_1710, holdable).
narrative_ontology:cs_axiom_grounding('fe588b0b-d5d0-4187-af2e-5216b5b4f967', claimant_set_preexisted_1710, conventional).
narrative_ontology:cs_axiom('fe588b0b-d5d0-4187-af2e-5216b5b4f967', foundational, enforcement_beneficiary_swap_is_the_mark).
narrative_ontology:cs_axiom_status(enforcement_beneficiary_swap_is_the_mark, holdable).
narrative_ontology:cs_axiom_grounding('fe588b0b-d5d0-4187-af2e-5216b5b4f967', enforcement_beneficiary_swap_is_the_mark, empirically_contingent).
narrative_ontology:cs_reference_frame('fe588b0b-d5d0-4187-af2e-5216b5b4f967', stationers_occupied_claimant_set).
narrative_ontology:cs_drift_state('fe588b0b-d5d0-4187-af2e-5216b5b4f967', post_donaldson_1774, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('fe588b0b-d5d0-4187-af2e-5216b5b4f967', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, london_booksellers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, stationers_company).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, vested_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, public_domain_readers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, book_buying_public).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, scottish_provincial_printers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, lump_sum_authors).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, limited_term_supersedes_perpetual_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Statute of Anne in 1710 after the Licensing Act lapsed in 1695 and the book trade was left without a statutory order. Drafted the settlement as a bargain: fourteen-year terms for new works, a twenty-one-year tail for books already in print, copy vested in authors first, deposits owed to designated libraries. Can alter the settlement only through new legislation, which requires assembling majorities against the organized London trade's persistent lobbying.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament_of_england, agenda_setter,
    institutional, generational, constrained, national).

% Wholesale and retail booksellers of London who purchase copyrights from authors, finance editions, and control the trade's distribution channels. They lobbied for the statute once the old licensing order collapsed, bought up vested terms rapidly through assignment, and financed the decades-long litigation campaign to extend their holdings beyond the statutory limits. Their capital is sunk in copyright stock; leaving the trade means writing off that stock.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, london_booksellers, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, london_booksellers, agenda_setter).

% The City livery company that registers copy-titles, polices unauthorized printing, and supplies the trade's disciplinary machinery. Stripped of its statutory monopoly in 1710, it re-entered the settlement as registrar and enforcement agent for whichever rights-holders the register records, collecting fees and keeping primacy in the trade. Its charter, hall, and offices are built around governing printing; relinquishing that role would dissolve the institution itself.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, stationers_company, beneficiary).

% Writers in whom the statute vests the copy for a first term. Some negotiate advances and gain leverage they never had under the guild order; most sell their terms outright to booksellers shortly after publication for lump sums priced by the trade. Withholding a manuscript is their principal lever, and distribution runs through the booksellers' channels regardless of who holds the term.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, vested_authors, beneficiary,
    moderate, biographical, constrained, national).

% Authors who assign their entire term for a one-time payment scaled to the purchaser's sales estimate. When a work outperforms the estimate, the upside accrues to the holder of the term; the author cannot repurchase the copy or share in later editions. Alternative channels — patronage, subscription, self-financed printing — reach a fraction of the trade's market.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, lump_sum_authors, payer,
    powerless, biographical, constrained, national).

% Readers who pay the authorized price for books during the copyright term. Substitutes exist at the margin — secondhand copies, libraries, abridgments, editions smuggled past the trade's prosecutors — but a new title in demand is available only at the single authorized price until its term expires.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, book_buying_public, payer,
    powerless, immediate, constrained, national).

% Printers in Edinburgh, Glasgow, and Aberdeen who reprint English-market works cheaply. The statute's reach into Scotland was contested for decades, and they worked the friction: printing for the domestic market, shipping into England when enforcement slackened, and litigating the copy's status in Scottish courts. Prosecution as pirates is the recurring cost of running that trade.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, scottish_provincial_printers, payer,
    organized, biographical, arbitrage, national).

% Later readers and printers who gain free access to works as terms expire. The statute promises every title a terminal date after which anyone may print it; cheap collected editions of expired works appeared within a generation. They act through no organization and are represented in the settlement only by the term-limit arithmetic itself.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_domain_readers, beneficiary,
    powerless, generational, constrained, national).

% Legal historians and intellectual-property theorists who reconstruct the 1710 settlement from parliamentary journals, the Stationers' registers, assignment contracts, and case files. They document who actually held and enforced the copy, and their accounts feed modern disputes over what the statute established.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, historical_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, london_booksellers).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the Licensing Act lapsed in 1695, publishing had no enforceable title system: no secure basis for financing editions, no reliable registry of who held what, and no terminal rule converting exclusivity back into common access. The settlement solves these once, centrally: transferable time-limited copy-titles, a register for proving them, and a predictable date on which every title enters the common stock.
% TRANSFER_FUNCTION: Moves monopoly rents on newly printed books — from readers paying above-cost authorized prices during the term, and from would-be rival printers kept out by prosecution — to whoever holds the copy-title: nominally the author at vesting, in practice overwhelmingly the London booksellers who purchased assignments.
% ABSENT_VOICES: Readers and non-London printers had no seat at the drafting table. Authors were spoken for by the booksellers who purchased their terms; the 'encouragement of learned men' preamble was drafted by trade interests. Scottish printers enter the record chiefly as prosecuted pirates. Future readers are represented only by the term-limit clause — no one argued their seat in 1709-1710.
% DISAPPEARANCE_RATIONALE: Overnight removal forces the trade to reorganize around either a re-chartered guild privilege or open reprint competition: edition financing collapses without enforceable titles, London's price umbrella vanishes, Scottish reprinting legalizes immediately, and author compensation migrates to patronage and subscription models. Every seated party's position depends on the arrangement existing.
% FOUNDING_PROBLEM: With the Licensing Act lapsed in 1695, the book trade had no statutory order: the Stationers' monopoly rested on expired authority, copy-titles were enforceable only through trade custom, and investment in editions was insecure. The statute was built to restore an enforceable title system while answering the monopoly objection through limited terms and author-vesting.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: parliamentary debate records from 1705-1710 show both monopoly critics and trade spokesmen treating post-1695 disorder as real; Scottish judicial opinions and the 1774 Donaldson v Becket arguments attest the enforcement dispute was genuine; modern legal-history scholarship (Feather, Rose, Deazley) reconstructs the founding problem from registers and case files independent of any beneficiary's account.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.63: the settlement's rents decoupled from service cost as the assignment market concentrated terms in a few London houses, peaking near 0.70 on the eve of Donaldson v Becket and easing after 1774 when the perpetual-copyright pretense died and terms began expiring in volume. Suppression (0.58 at end) is structural, not interpersonal: unauthorized reprinting was criminalized, and distribution ran only through channels the trade controlled. The suppression_requirement series deliberately tracks enforcement-capacity change — the Stationers' machinery was repurposed at t0 (0.50), the booksellers' litigation and prosecution campaign ratcheted through mid-century (0.66 at t48), and enforcement normalized downward after 1774 (0.58) once the maximal-suppression project failed. Theater rises from 0.30 to roughly 0.45 as author-vesting became progressively more performative — celebrated in the preamble, absorbed by assignment in practice — dipping slightly at t64 when the 1774 decision forced the arrangement's actual function into the open. Accessibility_collapse sits at 0.55: alternatives (piracy, import, secondhand, libraries) persisted but were legally foreclosed at the margin; resistance at 0.60 reflects sixty years of organized Scottish arbitrage, bookseller petitioning, and parliamentary contest. All three series share one seven-point grid (t=0,12,24,36,48,60,64) so no metric row is sampled against another's scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the london_booksellers seat the settlement is the salvage of their trade — coordination they paid for and administer, closer to a rope they run. From the scottish_provincial_printers seat the same structure is enforced exclusion with a prosecution apparatus attached. The two author seats split: vested_authors with bargaining power experienced a genuine new lever, while lump_sum_authors experienced a discount on their own upside. The book_buying_public seat registers a price umbrella with no organized voice. Parliament's seat holds the bargain as struck. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: london_booksellers and stationers_company sit near the beneficiary end despite their agenda-setting power (collecting, not bearing); public_domain_readers sit low-d but powerless and temporally deferred — their benefit arrives only at term expiry, a timing asymmetry the direction captures even though magnitude is muted. Victims derive high d: book_buying_public and lump_sum_authors near full target with constrained exit; scottish_provincial_printers high d but damped by arbitrage-grade exit (jurisdictional friction they actively exploited). One override is authored: vested_authors (the only moderate-power seat) would derive near-full-beneficiary d from the vesting declaration alone, but the assignment conduit routed most of the vested term to purchasers within months of publication — their realized position is mixed, protection for some and upside-loss for others — so d is corrected to 0.38.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (an enforceable title order for the book trade) remains live in direct lineage — modern copyright descends from this settlement, and the R5 status is authored live with external corroboration. The tangled_rope classification earns its keep here against two mislabelings. Reading the settlement as pure coordination (the booksellers' own account, and the celebratory Whig account of authorial emancipation) erases the named victims: readers paying the umbrella price, Scottish printers under prosecution, authors discounted at assignment. Reading it as pure extraction erases the real coordination: edition financing did expand output dramatically, titles were provable and transferable, and the per-work sunset delivered the public domain it promised — cheap collected editions of expired works appeared within a generation. The theater_ratio tracks precisely the component that tempts the emancipation misreading: vesting-in-authors as legitimating display versus vesting-as-conduit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_first_holding,
    'This story instantiates the first_holding_reading of kernel ip_category_emergence: that 1710 marks a change in who legitimately occupies the rights-holder position. Which structural elements of the 1710 event does this reading treat as load-bearing, and what would the sibling readings relocate?',
    'Comparative compilation of the three sibling stories'' epsilon referents, beneficiary/victim sets, and enforcement-beneficiary declarations, with convergence and divergence mapped across the family.',
    'If the occupancy-change elements (Stationers'' displacement, assignment routing, enforcement-beneficiary swap) carry the classification, this story stands as authored; if the category-emergence elements dominate, the thinkability_reading''s structure governs and this story''s deltas become secondary color.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position_first_holding, conceptual, 'Committer structure: this story is one reading of the ip_category_emergence kernel, not the kernel entire.').

omega_variable(
    pre1710_coherence_contest,
    'Did a coherent, enforceable claimant structure over printed copies exist before 1710 (as this reading''s occupancy-change premise requires), or did ownable expression become legally coherent only with the statute (the thinkability_reading''s premise)?',
    'Archive work on pre-1710 enforcement: Stationers'' register entries acted upon in court, equity''s treatment of authorial manuscript property, and trade-custom recognition of first publishers.',
    'Evidence of coherent pre-1710 claimancy stabilizes this reading and hardens its foreclosure relation to the thinkability_reading; its absence collapses this reading into a variant of category emergence and merges the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre1710_coherence_contest, empirical, 'Whether the claimant set this reading says changed membership actually pre-existed the statute.').

omega_variable(
    assignment_conduit_share,
    'What share of the vested author-term was transferred to booksellers by assignment, and how quickly after publication?',
    'Assignment contracts, Stationers'' register transfer entries, and author earnings records for the 1710-1740 publication cohorts.',
    'A high, fast conduit share pushes theater_ratio above the authored 0.45 and drives the arrangement toward pure extraction; a low share keeps the author-protection function substantive and supports the coordination half of the tangled reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assignment_conduit_share, empirical, 'How much of the author-vesting was protection versus legitimating conduit to trade ownership.').

omega_variable(
    seam_independence_status,
    'Are first-holding and thinkability formally independent dimensions of the 1710 event, or is their apparent sequence an artifact of temporal framing (the M4/M5 collapse test the seam reading runs)?',
    'Formal analysis: construct counterfactuals in which membership shifts without category emergence and vice versa; test each against the historical record for joint satisfiability.',
    'If the dimensions collapse into one, this story and the thinkability_reading merge and the family reduces to two stories; if they are independent, all three readings stand as distinct constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seam_independence_status, conceptual, 'Whether the seam reading''s collapse test dissolves the distinction this story is built on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__first_holding_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(ip_c_tr_t0, observed).
narrative_ontology:measurement(ip_c_tr_t12, ip_category_emergence__first_holding_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(ip_c_tr_t12, observed).
narrative_ontology:measurement(ip_c_tr_t24, ip_category_emergence__first_holding_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(ip_c_tr_t24, observed).
narrative_ontology:measurement(ip_c_tr_t36, ip_category_emergence__first_holding_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement_basis(ip_c_tr_t36, observed).
narrative_ontology:measurement(ip_c_tr_t48, ip_category_emergence__first_holding_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement_basis(ip_c_tr_t48, observed).
narrative_ontology:measurement(ip_c_tr_t60, ip_category_emergence__first_holding_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(ip_c_tr_t60, observed).
narrative_ontology:measurement(ip_c_tr_t64, ip_category_emergence__first_holding_reading, theater_ratio, 64, 0.45).
narrative_ontology:measurement_basis(ip_c_tr_t64, observed).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__first_holding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(ip_c_be_t0, observed).
narrative_ontology:measurement(ip_c_be_t12, ip_category_emergence__first_holding_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(ip_c_be_t12, observed).
narrative_ontology:measurement(ip_c_be_t24, ip_category_emergence__first_holding_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(ip_c_be_t24, observed).
narrative_ontology:measurement(ip_c_be_t36, ip_category_emergence__first_holding_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(ip_c_be_t36, observed).
narrative_ontology:measurement(ip_c_be_t48, ip_category_emergence__first_holding_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(ip_c_be_t48, observed).
narrative_ontology:measurement(ip_c_be_t60, ip_category_emergence__first_holding_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(ip_c_be_t60, observed).
narrative_ontology:measurement(ip_c_be_t64, ip_category_emergence__first_holding_reading, base_extractiveness, 64, 0.63).
narrative_ontology:measurement_basis(ip_c_be_t64, observed).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__first_holding_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(ip_c_su_t0, observed).
narrative_ontology:measurement(ip_c_su_t12, ip_category_emergence__first_holding_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(ip_c_su_t12, observed).
narrative_ontology:measurement(ip_c_su_t24, ip_category_emergence__first_holding_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(ip_c_su_t24, observed).
narrative_ontology:measurement(ip_c_su_t36, ip_category_emergence__first_holding_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement_basis(ip_c_su_t36, observed).
narrative_ontology:measurement(ip_c_su_t48, ip_category_emergence__first_holding_reading, suppression_requirement, 48, 0.66).
narrative_ontology:measurement_basis(ip_c_su_t48, observed).
narrative_ontology:measurement(ip_c_su_t60, ip_category_emergence__first_holding_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(ip_c_su_t60, observed).
narrative_ontology:measurement(ip_c_su_t64, ip_category_emergence__first_holding_reading, suppression_requirement, 64, 0.58).
narrative_ontology:measurement_basis(ip_c_su_t64, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The colloquial label 'what IP's emergence marks in 1710' decomposes into three structurally distinct claims per the epsilon-invariance principle: a membership shift in an occupied claimant set (this story), the birth of ownable expression as a coherent category (ip_category_emergence__thinkability_reading), and a meta-reading testing the independence of the first two (ip_category_emergence__synchronic_diachronic_seam). Each carries its own epsilon, beneficiary/victim structure, and classification. Upstream/downstream: the thinkability_reading's category-birth claim is typically cited as background for this occupancy story, while the seam reading interrogates both; all three are mutually linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, moderate, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
