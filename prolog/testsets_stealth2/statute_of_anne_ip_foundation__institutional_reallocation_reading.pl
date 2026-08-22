% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne — Institutional Reallocation of Literary Property (1710–1774)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   When the Statute of Anne received royal assent in 1710 it did not, on
 *   this reading, conjure a new kind of property out of nothing: it re-cut an
 *   existing institutional space. Exclusive rights in printed books had long
 *   been held — registered in the Stationers' Company's ledger, traded among
 *   its members, defended through its institutions — and the statute changed
 *   who occupied that space and on what tenure. Perpetual guild claims became
 *   14-year statutory terms vested first in authors, alienable by assignment
 *   to the bookseller houses that had always been the trade's financiers. The
 *   occupied set changed: authors entered as nominal first holders, the
 *   London houses consolidated as effective holders through cheap assignment
 *   purchases, incumbents' perpetual claims were extinguished at term
 *   boundaries, and reprint traders outside the combine became enforcement
 *   targets. Gains demonstrably pool in the bookseller houses; losses fall on
 *   displaced incumbents, prosecuted reprinters, and term-time readers. KEY
 *   AGENTS (by structural relationship): - parliament_legislature: Agenda
 *   setter (institutional/mobile) — enacted the reallocation, sets and resets
 *   terms, collects tied duties - london_booksellers_publishers: Concentrated
 *   beneficiary via assignment (powerful/constrained) — receives the pooled
 *   gains, runs enforcement and lobbying - authors_as_first_term_holders:
 *   Nominal first holders and conduits (moderate/constrained) — hold terms
 *   briefly, sell onward - stationers_company_perpetual_claimants: Displaced
 *   incumbent victim (organized/identity_locked) — lost perpetual claims,
 *   retained the registry - provincial_scottish_reprint_printers:
 *   Enforced-upon competitor victim (organized/constrained) - reading_public:
 *   Diffuse payer and deferred beneficiary (powerless/constrained) -
 *   irish_reprint_trades: Excluded arbitrageur outside the statute's
 *   jurisdiction (moderate/arbitrage) - common_law_jurists: Analytical
 *   adjudicator of the arrangement's boundary (institutional/analytical) This
 *   story is one of three linked readings of the same founding statute; its
 *   epsilon is indexed to this reading's referent — the standing post-1710
 *   allocation as the reallocation account sees it — not to the sibling
 *   readings' referents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.55).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.44).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne — Institutional Reallocation of Literary Property (1710–1774)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'fd8395ec-e5a6-4097-a85f-16157303763a').
narrative_ontology:cs_kernel_codification('fd8395ec-e5a6-4097-a85f-16157303763a', fixed_text).
narrative_ontology:cs_authority_grounding('fd8395ec-e5a6-4097-a85f-16157303763a', lineage).
narrative_ontology:cs_interpretation_layer_present('fd8395ec-e5a6-4097-a85f-16157303763a').
narrative_ontology:cs_reading_relation('fd8395ec-e5a6-4097-a85f-16157303763a', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd8395ec-e5a6-4097-a85f-16157303763a', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('fd8395ec-e5a6-4097-a85f-16157303763a', foundational, printing_rights_pre_existed_statute).
narrative_ontology:cs_axiom_status(printing_rights_pre_existed_statute, holdable).
narrative_ontology:cs_axiom_grounding('fd8395ec-e5a6-4097-a85f-16157303763a', printing_rights_pre_existed_statute, empirically_contingent).
narrative_ontology:cs_axiom('fd8395ec-e5a6-4097-a85f-16157303763a', foundational, occupancy_change_is_essential_delta).
narrative_ontology:cs_axiom_status(occupancy_change_is_essential_delta, holdable).
narrative_ontology:cs_axiom_grounding('fd8395ec-e5a6-4097-a85f-16157303763a', occupancy_change_is_essential_delta, instrumental).
narrative_ontology:cs_reference_frame('fd8395ec-e5a6-4097-a85f-16157303763a', stationers_guild_property_baseline).
narrative_ontology:cs_drift_state('fd8395ec-e5a6-4097-a85f-16157303763a', contemporary_book_history_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fd8395ec-e5a6-4097-a85f-16157303763a', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers_publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_first_term_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_perpetual_claimants).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, provincial_scottish_reprint_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, incentive_justifies_limited_term).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, registration_confers_enforceable_title).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute after the lapsing of the Licensing Act left the book trade without a legal foundation for exclusive publication and after trade petitions warned of ruinous open reprinting. Fixed the term lengths (14 years for new books, 21 for existing stock), vested the term in authors first, retained power to extend or repeal, and considered then rejected repeated extension bills through the 1730s and 1760s. Collects stamp and book duties tied to the registered trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Purchase assigned terms from authors, finance large editions, and police the market through prosecutions in Chancery and at common law, proving title through the Stationers' Hall registry. Led the lobbying campaign for perpetual rights from the 1730s to 1774 and the litigation line that produced Millar v Taylor. Assignment prices undershoot term value and retail prices are set by a small group of London houses, so the arrangement's income pools here rather than with the authors who first hold the terms. Capital is sunk in copyrights and stock, making redeployment slow.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers_publishers, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers_publishers, agenda_setter).

% Hold the statutory term first and sell or license it to booksellers; before 1710 they sold manuscripts outright with no residual claim. Their gain depends on competitive bidding among purchasers and on renewal if alive at term end. Most lack bargaining leverage against the London houses, sign assignments early, and receive a fraction of term value; the alternative channel of patronage publication is thin.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_first_term_holders, beneficiary,
    moderate, biographical, constrained, national).

% Members holding perpetual registration entries in the Company's ledger watched those entries lose statutory recognition at term boundaries, while the Company itself retained the registry and search functions the new system relies on. Individually, many members bought assigned copyrights and stayed in the trade; institutionally, the Company defended its members' old claims through litigation and decades of lobbying for restoration of perpetual right. The Company's self-conception is fused with custody of the nation's copy registry, so abandoning the space it lost was never a live institutional option even as individual members adapted.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_perpetual_claimants, payer,
    organized, generational, identity_locked, national).

% Reprint popular London titles cheaply for domestic and export markets; exposed to forfeiture of sheets and fines when enforcement reaches them, and shut out of the registry-based title chain that legitimizes London holdings. Mounted coordinated litigation in the Scottish courts that ended in Hinton v Donaldson and fed the appeal decided in 1774. Relocating means joining or submitting to the London system, so remaining outside it is the only viable posture.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, provincial_scottish_reprint_printers, payer,
    organized, biographical, constrained, regional).

% Buys books at prices set during exclusive terms and waits for terms to lapse for cheaper editions. Gains a growing stock of out-of-term works after 1774 and, indirectly, more financed new publications. Has no seat in the trade's councils; its preferences surface only through market behavior, library formation, and occasional pamphleteering.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, payer,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary).

% Operate under the Dublin parliament, outside the Westminster statute's jurisdiction, and ship cheap reprints into Britain. Their competition is cited in enforcement petitions and lobbying, but they hold no seat in the arrangements being designed or renegotiated and bear no obligation to the terms they undercut.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, irish_reprint_trades, excluded,
    moderate, biographical, arbitrage, regional).

% Adjudicate whether exclusive right in printed books subsists independently of the statute: Mansfield's bench found for perpetual common-law right in Millar v Taylor, the Scottish courts rejected it, and the House of Lords divided before reversing course in 1774. Holds no economic stake in the outcome; its rulings define the outer boundary of what the statutory terms protect.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, common_law_jurists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_booksellers_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, registrable, time-limited title to newly printed books, making capital-intensive editions financiable and giving the trade a common enforcement path through the courts instead of private combination among the old incumbency.
% TRANSFER_FUNCTION: Moves exclusive-market income from book buyers and would-be reprinters to term holders during each term; because terms are assignable and are purchased early and cheaply, the bulk of that income accrues to London bookseller houses rather than to the authors who first hold the rights.
% ABSENT_VOICES: Irish reprint traders (outside the statute's jurisdiction), provincial and Scottish retailers dependent on cheap reprints, ordinary readers, and authors without access to the London trade had no seat in drafting or enforcement design; their objections survive only in petitions, pamphlets, and courtroom argument.
% DISAPPEARANCE_RATIONALE: Without the 1710 reallocation the post-1695 open-trade condition persists: no liquid market in literary property, no assignment-based financing model, no registry-anchored title chain, and no term-limited public domain feeding cheap editions after 1774. The Anglo-American copyright lineage and its particular beneficiary structure (authors first, publishers by assignment) do not come into existence in this shape.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695 the Stationers' registrations lost legal force; booksellers feared ruinous open reprinting and petitioned for restored title security, while reformers sought a limited, purpose-bound grant that would encourage learned men to write rather than restore a guild monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is split along the two halves of the founding story. The encouragement-of-learning rationale is attested from outside the benefiting parties by contemporary pamphleteers (including Defoe's essays on author remuneration) and in parliamentary debate. The trade-ruin crisis is attested almost entirely by the petitioning booksellers themselves and is disputed in the submissions of Scottish and dissenting printers; no fully disinterested contemporary voice attests the crisis half, which is itself signal about whose problem the statute was written to solve.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   The claimed type is tangled_rope on structural grounds independent of the metrics: the arrangement solves a real coordination problem (uniform registrable title making capital-intensive editions financiable, a common enforcement path replacing private combination) while operating asymmetric transfer through the same structure (term-time premiums from readers, foreclosed reprint competition, gains pooling in the purchasing houses). Metrics are authored descriptively, not tuned toward any verdict. Base extractiveness ends at 0.55: real rents, bounded by short terms and a replenishing public stock. Suppression (0.44 at end) is a raw structural input — the engine scales only extractiveness — reflecting statutory penalties, forfeiture, and registry-based prosecution. Theater climbs from 0.15 to 0.49 across the interval as the encouragement-of-learning preamble is progressively recruited to defend perpetual rent (the 1735–1769 extension bills and the common-law litigation line), then falls back to 0.40 once the 1774 decision re-grounds the arrangement on limited terms. All three tracked series share one eight-point grid (1710–1774) so no metric is sampled against another's gaps. Suppression_requirement is tracked because this story's enforcement arc is genuinely dynamic — machinery built up through the 1752–1769 litigation offensive, then collapsing after the Scottish courts and the Lords rejected the perpetual claim. The rising base_extractiveness series is deliberate data for extraction-accumulation detection, not drift to be smoothed away.
 *
 * PERSPECTIVAL GAP:
 *   Payer and beneficiary seats compute differently from identical structural inputs. The displaced incumbents experienced 1710 as expropriation of customary property, yet their exit was identity-locked: the Company's self-conception had fused with custody of the nation's copy registry, so the institution could not simply vacate the space it had lost. Individually, members dissolved that lock over generations by buying assigned copyrights — which is why the victim seat attenuates rather than hardens (see the stationer_disposition omega); had the identity frame broken early, the incumbency would read as a compensated loser rather than a durable injured class. The London houses experienced the same statute as legalization of a better tenure than the one they lost: perpetual claims of uncertain validity exchanged for watertight terms they could purchase wholesale. Authors experienced a new saleable asset, but the receipt record shows the asset pooling downstream. Same-level lateral divergence: London houses and Scottish reprinters stood at comparable trade standing, differentiated by registry access (title proof ran through Stationers' Hall), capital depth for litigation, and jurisdictional reach — factors specific to this arrangement, not global standing, determined who could exit. Readers experienced the whole architecture as a distant abstraction until out-of-term editions arrived.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (london_booksellers_publishers, authors_as_first_term_holders) derive low d toward the beneficiary end; declared victims (stationers_company_perpetual_claimants, provincial_scottish_reprint_printers) derive high d toward the target end, with the incumbency pushed further by identity-locked exit. The reading_public seat carries both roles, so its derived directionality is unstable and a single override pins it slightly target-side (powerless → d 0.62): readers pay term-time premiums with no seat-level compensation inside the arrangement, their return arriving only through public-stock replenishment outside it. Authors' derived low d flatters them — structurally they are conduits through whom value passes to the purchasing houses; the gain_flow field records where receipts actually land, and the author_capture_rate omega tracks whether the derivation should be corrected. Receipt is not benefit: the houses hold beneficiary role and capture; the authors hold beneficiary role without capturing. Suppression enters the computation unscaled; only extractiveness is amplified by directionality and national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification guards against two symmetric misreadings. A romance of 1710 as a pure gift to authors and learning misses that the same instrument re-concentrated the trade in fewer hands with stronger title than the guild ever held. An indictment of the arrangement as pure predation misses the financing function without which the large editions, the author advances, and the eventual public stock do not appear. The mandatrophy trajectory is visible in the measurements: the founding problem — restoring title security after the 1695 lapse — was substantively solved within the first decade, and what persisted was steady-state management of a rent-bearing allocation, with the justification layer growing theatrical (theater_ratio 0.15 → 0.49) until the courts stripped the theatrical overlay in 1774 and re-founded the arrangement on its limited-term core. The arrangement survived its original mandate by mutation rather than inertia; correlating theater_ratio against the litigation and lobbying calendar is what distinguishes that mutation from zombie persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading correctly characterizes the founding delta of kernel statute_of_anne_ip_foundation: conceptual creation, entangled dual change, or institutional reallocation?',
    'Comparative adjudication across the three linked family stories: pre/post-1710 instruments (Stationers'' register entries, surviving assignment contracts, parliamentary journals) tested against each reading''s predicted signature; the entangled reading stands if neither dimension alone accounts for the record.',
    'Beneficiary/victim structure flips across readings: under conceptual_emergence the beneficiary is the learning-public and no Stationers'' victim is named; under entangled_event the two dimensions fuse and per-seat classification blends both; the publisher-via-assignment capture structure documented here holds only under institutional_reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This story instantiates one of three rival readings of the same founding statute; sibling readings instantiate different constraints with different epsilon and different beneficiary structures.').

omega_variable(
    pre_statute_rights_property_status,
    'Did transferable proprietary rights in printed copies genuinely exist before 1710, or only trade custom without recognized property status?',
    'Archival reconstruction of pre-1695 copy transactions, registration entries, and chancery records treating ''copies'' as alienable assets.',
    'If no pre-existing property is found, this story''s foundational axiom fails and the account collapses toward the emergence reading; if property existed, the statute reads as re-titling an existing space and the reallocation account stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_statute_rights_property_status, empirical, 'Whether the reallocation reading''s premise of pre-existing rights survives archival scrutiny.').

omega_variable(
    author_capture_rate,
    'What share of term value did authors capture versus the bookseller houses that bought assignments, and how did that share vary with author standing?',
    'Surviving assignment contracts and author account ledgers compared against retail term revenue for matched titles.',
    'High capture keeps authors in the beneficiary set; low capture demotes them to conduits, concentrates receipt further on london_booksellers_publishers, and pushes computed severity toward the snare boundary at the payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(author_capture_rate, empirical, 'Whether the authors'' first-holder position translated into captured value or passed straight through to assignees.').

omega_variable(
    public_domain_replenishment_rate,
    'How quickly did expiring terms convert into a usable cheap-classics stock that repays the reading public''s term-time premium?',
    'Price and edition counts for out-of-term works before and after the 1774 decision, benchmarked against in-term pricing for the same titles.',
    'Slow replenishment means the coordination payoff never reaches readers and the arrangement drifts toward pure extraction at the reader seat; fast replenishment stabilizes the mixed coordination-plus-transfer structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_replenishment_rate, empirical, 'Whether the term-limited design delivered its promised public-side return.').

omega_variable(
    stationer_disposition_transient_or_permanent,
    'Was the incumbents'' displacement a permanent expropriation or a transitional reshuffle absorbed as old firms bought assigned copyrights and re-entered the new system?',
    'Trace incumbent firms'' copyright portfolios across 1710–1740 trade sales and auction registers.',
    'If absorbed, the victim seat is transient and the episode reads as consensual reorganization with a compensated loser class; if permanent, a durable loser persists and structural resistance stays fueled across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationer_disposition_transient_or_permanent, empirical, 'Persistence question for the displaced-incumbent seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anne_inst_reallocation_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1710, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1719, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1719, 0.18).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1719, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.26).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1730, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1739, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1739, 0.36).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1739, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1752, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1752, 0.43).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1752, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1760, 0.46).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1760, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1769, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1769, 0.49).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1769, observed).
narrative_ontology:measurement(anne_inst_reallocation_tr_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1774, 0.4).
narrative_ontology:measurement_basis(anne_inst_reallocation_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(anne_inst_reallocation_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.45).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1710, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1719, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1719, 0.5).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1719, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.57).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1730, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1739, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1739, 0.61).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1739, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1752, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1752, 0.65).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1752, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1760, 0.67).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1760, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1769, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1769, 0.69).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1769, observed).
narrative_ontology:measurement(anne_inst_reallocation_be_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1774, 0.55).
narrative_ontology:measurement_basis(anne_inst_reallocation_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(anne_inst_reallocation_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.38).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1710, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1719, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1719, 0.46).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1719, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.58).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1730, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1739, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1739, 0.63).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1739, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1752, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1752, 0.7).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1752, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1760, 0.73).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1760, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1769, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1769, 0.75).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1769, observed).
narrative_ontology:measurement(anne_inst_reallocation_su_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1774, 0.44).
narrative_ontology:measurement_basis(anne_inst_reallocation_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Statute of Anne founded copyright' decomposes per the epsilon-invariance principle into three stories — conceptual_emergence (referent: the new regulatory category for learning; expect low epsilon, learning-public beneficiary, no Stationers' victim), entangled_event (referent: the undivided founding event; refuses per-seat separation of the two dimensions), and this institutional_reallocation story (referent: the post-1710 allocation of existing rights; publisher-via-assignment capture, displaced incumbents as victims). Each member carries its own epsilon, beneficiaries, victims, and type; each declares links to the others in its own affects_constraints. Downstream citation pressure runs from the emergence account (invoked as evidence of benign founding intent) into this one, which must show capture beyond benign re-titling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
