% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Copyright as Limited Regulation for Learning (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the conceptual_emergence_reading of the kernel
 *   statute_of_anne_ip_foundation: the claim that the 1710 statute's
 *   signature achievement was making a new legal category thinkable,
 *   copyright as a limited regulatory instrument for learning, with perpetual
 *   property in texts displaced from the center of the law. On this reading
 *   the beneficiary of the arrangement is public learning (readers and
 *   authors), and the party that bore its founding cost is the
 *   perpetual-monopoly interest embodied by the London booksellers, whose
 *   claimed common-law perpetuity the statute extinguished and whose
 *   sixty-year campaign to restore it failed at Donaldson v Becket in 1774,
 *   the interval's endpoint. The claim and the metrics are authored
 *   independently: the reading claims a tangled_rope (genuine coordination
 *   with asymmetric, term-bounded extraction), while the metric series
 *   honestly records extraction, theater, and enforcement rising together as
 *   the booksellers converted limited terms into de facto perpetuity through
 *   assignments and evergreening, peaking around Millar v Taylor (t=59) and
 *   falling back after Donaldson (t=64).
 *
 * KEY AGENTS:
 *   - parliament_of_great_britain: agenda-setter (institutional/mobile) - enacted the scheme and alone can reshape it
 *   - london_booksellers_stationers: dual-positioned operator (powerful/constrained) - lost the perpetual claim, gained the operable limited monopoly, collects the rents
 *   - authors_and_learned_men: nominal beneficiary (moderate/constrained) - gained a saleable right, mostly sold it onward
 *   - reading_public: dual-positioned beneficiary-payer (powerless/constrained) - pays during terms, inherits the public domain after
 *   - scottish_and_irish_reprint_trades: excluded competitor (organized/mobile) - supplies cheap copies from outside the bargain
 *   - common_law_courts_and_house_of_lords: adjudicating observer with agenda-setting effect (institutional/analytical) - fixed the arrangement's meaning at Millar v Taylor and Donaldson v Becket
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.4).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.31).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Copyright as Limited Regulation for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e704d322-33e8-4075-a90f-c158b7e1eed6').
narrative_ontology:cs_kernel_codification('e704d322-33e8-4075-a90f-c158b7e1eed6', fixed_text).
narrative_ontology:cs_authority_grounding('e704d322-33e8-4075-a90f-c158b7e1eed6', lineage).
narrative_ontology:cs_interpretation_layer_present('e704d322-33e8-4075-a90f-c158b7e1eed6').
narrative_ontology:cs_reading_relation('e704d322-33e8-4075-a90f-c158b7e1eed6', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e704d322-33e8-4075-a90f-c158b7e1eed6', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('e704d322-33e8-4075-a90f-c158b7e1eed6', foundational, copyright_is_constitutively_limited_regulation_for_learning).
narrative_ontology:cs_axiom_status(copyright_is_constitutively_limited_regulation_for_learning, holdable).
narrative_ontology:cs_axiom_grounding('e704d322-33e8-4075-a90f-c158b7e1eed6', copyright_is_constitutively_limited_regulation_for_learning, instrumental).
narrative_ontology:cs_axiom('e704d322-33e8-4075-a90f-c158b7e1eed6', secondary, perpetual_property_in_texts_is_illegitimate).
narrative_ontology:cs_axiom_status(perpetual_property_in_texts_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('e704d322-33e8-4075-a90f-c158b7e1eed6', perpetual_property_in_texts_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('e704d322-33e8-4075-a90f-c158b7e1eed6', copyright_as_limited_regulation_for_learning).
narrative_ontology:cs_drift_state('e704d322-33e8-4075-a90f-c158b7e1eed6', post_donaldson_v_becket, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e704d322-33e8-4075-a90f-c158b7e1eed6', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_and_learned_men).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers_stationers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers_stationers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Debated and enacted the 1710 statute after the Licensing Act lapsed in 1695, leaving the book trade without an enforceable legal foundation. Retains power throughout the interval to lengthen terms, shorten them, add new subject matter, or let the scheme decay; it is petitioned continuously by booksellers seeking longer or perpetual terms and by their opponents seeking cheaper books, and it amends the scheme only rarely.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_of_great_britain, agenda_setter,
    institutional, generational, mobile, national).

% The London trade that had run the Stationers' register and claimed a perpetual common-law right in the copies it held. The statute replaced that claim with fourteen-year terms, destroying the legal basis of its oldest asset, and the trade spent sixty years litigating and lobbying to restore perpetuity. In the meantime it adapted: it bought authors' rights outright, dominated the assignment market, and operated the limited monopoly as its core business. Its capital, warehouses, and unsold stock are locked into the book trade; leaving means liquidating the business it dominates.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers_stationers, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers_stationers, payer).

% Gain for the first time a saleable, enforceable, time-limited right in their own writings, an alternative to patronage and to whatever terms booksellers offered informally. Most sell the right to a bookseller for a lump sum at or shortly after publication, so the protection reaches them mainly through the purchase price. They depend on bookseller capital to reach print at all, and individually they have little leverage over terms.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_and_learned_men, beneficiary,
    moderate, biographical, constrained, national).

% Buys books during active terms at prices set by rights-holders, and receives each work free of exclusive control when its term ends. It has no organized voice in Parliament or the courts; its interest in cheap books and a growing stock of unprotected works is invoked rhetorically by every faction and argued directly by none. Access to cheaper current titles runs through imported Irish reprints and, after mid-century, Scottish editions of contested legality.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, payer).

% Print and import editions of English works outside the statute's effective reach, openly in Dublin and, after the Union, in Edinburgh under disputed legality. Would compete openly on price if admitted to the statutory bargain; instead it operates at the edge of enforcement, is sued and enjoined when caught, and reorganizes or relocates when pressed. Its competitive pressure is a standing fact the London trade litigates against for the whole interval.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, scottish_and_irish_reprint_trades, excluded,
    organized, biographical, mobile, continental).

% Adjudicate what the statute's words mean and whether any common-law right survives alongside it. King's Bench in Millar v Taylor (1769) finds a perpetual right at common law; the House of Lords in Donaldson v Becket (1774) reverses course and confines the right to the statutory terms. Their rulings fix the operative meaning of the arrangement for every other seat, and they hear argument from all factions while collecting nothing from the trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, common_law_courts_and_house_of_lords, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, common_law_courts_and_house_of_lords, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers_stationers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the appropriability problem of book publishing after the collapse of state licensing: without an exclusive right, reprinters undercut the fixed costs of composition and printing, so the statute creates a temporary, transferable exclusive right that finances publication while guaranteeing that every covered work enters the public domain on a known schedule.
% TRANSFER_FUNCTION: Moves monopoly rents from book buyers to rights-holders during each active term, overwhelmingly to the booksellers who purchased assignments, in exchange for financing publication; and moves each work itself, at term expiry, from exclusive control into the common stock.
% ABSENT_VOICES: The reading public would object to term-length rent if it could organize, but it is dispersed, unenfranchised, and represented only rhetorically. Future readers of not-yet-written works, provincial and dissenting-academy libraries priced out of new titles, and authors who had already signed away their rights for lump sums are all absent from Westminster lobbying and courtroom argument; the cheapest-book constituency appears in the record only as an argument made by others.
% DISAPPEARANCE_RATIONALE: If the statutory scheme vanished overnight, the trade reverts to contested informal registration and litigation over customary rights, patronage and commission publishing expands to finance respectable works, the cheap reprint sector grows or goes underground, and the category of a work entering the public domain on a schedule never organizes the auction, library, and reprint markets that grew up around it. The book economy reorganizes around whoever can physically control copies.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695 the book trade had no enforceable legal foundation: the Stationers' register conveyed no right cognizable at law, unauthorized reprinting threatened the trade's capital, and authors held nothing they could sell. The statute was built to give the trade a lawful footing and to give learned men an inducement to write.
% FOUNDING_PROBLEM_CORROBORATION: The 1709-1710 parliamentary journals and the statute's own preamble attest the legal vacuum from outside any single trade faction, and modern economic analysis of public goods in publishing corroborates that the appropriability problem for composed texts remains real, though economic historians dispute how large it was in the eighteenth century. No attesting source sits inside the benefiting trades alone.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.40 at interval end: during any active term buyers paid well above marginal cost and the booksellers captured most of the rent through assignments, but the terms were short, the subject matter narrow, and every grant self-expired, so the reading's own lights place the standing arrangement at moderate, bounded extraction. Suppression is 0.31 and is authored as a raw structural property, unscaled by power or scope: enforcement ran through private infringement actions and injunctions aimed chiefly at the reprint trades, while patronage publishing, importing, and post-term reprinting remained fully available alternatives. Theater_ratio is 0.27: the encouragement-of-learning preamble was substantially functional (title output and the public-domain stock both grew), but a growing minority of the rhetoric served bookseller rent-seeking, especially the claim that perpetuity itself was the learning incentive. Accessibility_collapse is deliberately low at 0.25: the reading's own thesis is that the statute ADDED a point to the conceptual space rather than eliminating rivals, and the record agrees, since patronage publishing, Irish reprinting, and the perpetual-property claim all stayed live for the entire interval. Resistance is 0.55: organized, capitalized, and sustained across six decades of litigation and lobbying. The temporal series share one grid (t = 0, 12, 24, 36, 48, 59, 64) so every metric is authored at every examined point; the suppression_requirement series is included because the story specifically tracks enforcement-capacity dynamics, the booksellers' litigation machine building toward 1769 and decaying after 1774. has_sunset_clause is true because per-grant expiries are constitutive of the design, but the arrangement is not a scaffold: the institution was built to persist indefinitely, with each work's monopoly, not the scheme itself, marked for transition.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the bookseller seat the arrangement is the framework of its livelihood: a right it operates, litigates over, and nearly lost twice, experienced as earned property under siege. From the reader seat it is a bounded toll that always ends, tolerable precisely because it expires. From the author seat it is ambivalent emancipation: a first-ever saleable right that arrives bundled with dependence on bookseller capital. From the excluded reprinter seat it is a pure enforcement wall with no coordinating content at all. The powerless reader seat also carries the coalition question: individually voiceless, the reading public's interest was structurally incapable of self-organization in this interval, which is why its benefit arrives mediated by factions arguing on its behalf rather than by its own pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: readers and authors sit toward the beneficiary end, the booksellers sit ambiguously because they appear in both arrays, and the reprint trades appear in neither array despite bearing the sharpest enforcement pressure. Three overrides correct places where the derivation would err. The powerful atom is overridden to d=0.30 because the booksellers are dual-listed (they lost perpetuity, gained the operable monopoly, and collect the rents as the gain_flow seat), which a single-array derivation cannot resolve. The organized atom is overridden to d=0.70 because the reprint trades hold the excluded role with no beneficiary or victim declaration, so the canonical fallback would misplace them near symmetry when they are in fact the enforcement's principal targets. The powerless atom is overridden to d=0.35 because the reading public's secondary payer role pulls it off a clean beneficiary derivation, while the reading's own lights keep public learning on the subsidized side of the ledger.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, the post-1695 legal vacuum, was solved almost immediately, and the institution persisted and expanded past its solution; the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and correctly finds no zombie flag, because the appropriability problem the arrangement solves recurs with every new work rather than having been permanently discharged. The classification work the reading performs cuts both ways. Without the coordination function visible, the arrangement collapses into a publisher cartel story and reads as pure extraction; without the extraction visible, it romanticizes into disinterested public-interest legislation. The tangled_rope claim holds both halves: the same structure that finances publication transfers rents to the assignment-buying trade, and the per-work sunset that bounds the extraction is the design feature the booksellers spent sixty years trying to abolish. The measurement arc guards against mistaking the 1774 settlement for the arrangement's steady state: the mid-interval peak shows what the structure becomes when the limit principle weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separability_contest,
    'Is the statute''s signature achievement an isolable conceptual emergence (this reading), a mere reallocation of rights among existing occupants (institutional_reallocation_reading), or an inseparable fusion of the two (entangled_event_reading)? This story is one reading of kernel statute_of_anne_ip_foundation; the committer structure lives here rather than in the constraint body.',
    'Comparative historiographic and doctrinal adjudication: counterfactual tests of whether the category copyright-as-limited-regulation was available absent the specific reallocation and vice versa, plus citation-pattern analysis showing whether post-1774 judges reasoned from the conceptual frame independently of occupant interests.',
    'If the entangled reading wins, this constraint merges with its siblings and a per-reading epsilon becomes ill-defined; if the reallocation reading wins, the beneficiary and victim structure shifts to occupant-based accounting and the measured extraction rises, since the same rents would be counted against unchanged rights rather than against a new category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability_contest, conceptual, 'Which reading of the Statute of Anne kernel correctly locates the structural delta.').

omega_variable(
    conceptual_emergence_contingency,
    'Did limited-term copyright become thinkable because print-market economics made it structurally necessary, or was it a contingent political compromise that could plausibly have failed, as it nearly did when King''s Bench found a perpetual common-law right in 1769?',
    'Comparative jurisdiction history: jurisdictions without an analogous statute, and the near-miss counterfactual of Millar v Taylor standing unreversed, tested against whether trade structure converged on limited terms regardless.',
    'If the emergence was structurally necessary, the arrangement trends mountain-like over long horizons with falling resistance; if contingent, its persistence depends on continued enforcement and the 1774 settlement remains revisable, keeping the tangled_rope classification load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_emergence_contingency, empirical, 'Whether the conceptual emergence was inevitable or a fragile political settlement.').

omega_variable(
    rhetorical_vs_operational_beneficiary,
    'Did the learning benefit accrue to the reading public as the preamble claims, or did the learning rationale function as cover for transferring the Stationers'' informal monopoly into a legally safer bookseller cartel?',
    'Price and output data for 1710-1774 (book prices, title counts, edition runs, assignment terms) compared against the counterfactual trade under continued informal registration, with attention to how much of the rent reached authors versus assignment-holding booksellers.',
    'If bookseller capture dominates, effective extraction rises sharply and the classification drifts toward pure extraction with the learning preamble as theater; if the public benefit is real and large, the tangled_rope reading holds with the current bounded-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_vs_operational_beneficiary, empirical, 'Whether the declared beneficiary captured the gains or the operating trade did.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 36, 0.23).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t59, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 59, 0.33).
narrative_ontology:measurement(soanne_conceptual_emergence_tr_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 64, 0.27).

% Extraction over time
narrative_ontology:measurement(soanne_conceptual_emergence_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 36, 0.41).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 48, 0.46).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t59, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 59, 0.53).
narrative_ontology:measurement(soanne_conceptual_emergence_be_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 64, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(soanne_conceptual_emergence_su_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 36, 0.34).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 48, 0.4).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t59, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 59, 0.48).
narrative_ontology:measurement(soanne_conceptual_emergence_su_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 64, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the statute_of_anne_ip_foundation kernel per the epsilon-invariance principle: the colloquial label 'what the Statute of Anne did' conflates a conceptual claim (a new category became thinkable, this file), an institutional claim (rights moved between occupants, institutional_reallocation_reading), and a claim about the event's unity (entangled_event_reading). Each member carries its own epsilon, beneficiary/victim structure, and classification. This reading's epsilon is lower than an occupant-focused accounting would yield, because it measures the arrangement as a bounded regulatory innovation rather than as a transfer among pre-existing rights; the upstream/downstream ordering runs from this file toward the reallocation reading, whose occupant-based accounting is what the conceptual frame made expressible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, powerful, 0.3).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, organized, 0.7).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
