% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne Arrangement (Entangled-Event Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as a single entangled act: in one
 *   document, limited-term literary property became thinkable AND the holding
 *   of printing rights changed hands — from the Stationers' customary
 *   perpetual claims to a market of trade-held, author-titled, time-limited
 *   copyrights. This story instantiates the entangled_event_reading of the
 *   statute_of_anne_ip_foundation kernel; the sibling readings
 *   (conceptual_emergence_reading, institutional_reallocation_reading) are
 *   separate constraints with their own epsilon values and are not described
 *   inside this one. Under this reading the beneficiary structure is
 *   irreducibly ambiguous — authors nominal, booksellers practical — and the
 *   standing casualty is conceptual clarity itself: because the act fused the
 *   two dimensions, three centuries of dispute over what copyright IS traces
 *   to the fusion. Claim/metric independence is preserved: the claimed type
 *   is tangled_rope because the reading holds a genuine coordination
 *   achievement and asymmetric, actively enforced extraction in one
 *   structure; the metrics are authored from the arrangement's observed
 *   operation, not reconciled to the claim.
 *
 * KEY AGENTS:
 *   - london_booksellers: Practical beneficiary and day-to-day administrator (organized/constrained) — collects the arrangement's proceeds and runs its enforcement
 *   - authors: Nominal beneficiary with practical cost-bearing (moderate/constrained) — named first in the text, paid at the trade's offer
 *   - stationers_company: Institutional residue of the prior regime (institutional/identity_locked) — keeps the registry, the searchers, and the fees
 *   - reading_public: Diffuse cost-bearer with term-end gains (powerless/constrained)
 *   - provincial_scottish_printers: Enforcement target with jurisdictional exit (organized/arbitrage)
 *   - irish_booksellers: Excluded reprinter, never consulted (organized/arbitrage)
 *   - parliament: Rule-setter that moved on after enactment (institutional/mobile)
 *   - ip_legal_historians: Analytical observer — sees the full entangled structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.66).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.55).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne Arrangement (Entangled-Event Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'c5c7ea75-5f65-4f2c-8e4a-3dd494f780af').
narrative_ontology:cs_kernel_codification('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', formalized).
narrative_ontology:cs_authority_grounding('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', lineage).
narrative_ontology:cs_interpretation_layer_present('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af').
narrative_ontology:cs_reading_relation('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', foundational, conceptual_and_institutional_change_inseparable).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', conceptual_and_institutional_change_inseparable, empirically_contingent).
narrative_ontology:cs_axiom('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', secondary, beneficiary_ambiguity_structurally_builtin).
narrative_ontology:cs_axiom_status(beneficiary_ambiguity_structurally_builtin, holdable).
narrative_ontology:cs_axiom_grounding('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', beneficiary_ambiguity_structurally_builtin, empirically_contingent).
narrative_ontology:cs_reference_frame('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', unitary_foundational_enactment).
narrative_ontology:cs_drift_state('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', post_revisionist_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5c7ea75-5f65-4f2c-8e4a-3dd494f780af', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, london_booksellers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, provincial_scottish_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute after the Licensing Act's lapse left the book trade without a legal settlement. Fixed the terms — fourteen-year forward terms renewable once if the author lived, twenty-one years for books already in print, ninepence sheet-price ceilings, deposit copies to five libraries, registration at Stationers' Hall — and retained power to amend or repeal. Its attention moved on quickly after passage; day-to-day operation passed to the trade and the courts.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% The wholesale houses of Fleet Street and Paternoster Row. Administered the arrangement in practice: entered titles at Stationers' Hall, bought copyrights from authors outright, financed editions, employed the Company's searchers, and brought infringement suits. The twenty-one-year backstop preserved the exclusivity of their existing stock; the forward terms supplied fresh inventory bought at the trade's own offered price. Holdings passed down within firms, and firm credit rested on the copyright ledger. Their collective lobbying drove the mid-century campaign to stretch terms toward perpetuity.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, london_booksellers, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, london_booksellers, beneficiary).

% The City livery company that had governed printing since 1557. It lost its statutory licensing monopoly in 1695 and its customary perpetual-property claims in 1710, yet kept the register, the hall, the searchers, and the fee income. Its officers ran the daily machinery of the new regime because no other administrative apparatus existed. The Company's corporate identity was bound up with governing the trade; surrendering that role would have hollowed out its purpose.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, agenda_setter).

% Writers who for the first time owned a sellable fourteen-year property in their new works. In practice most sold their rights outright to booksellers soon after composition, for lump sums sized by the trade's offer rather than the work's performance, and few lived to exercise the renewal contingent on their survival. The statute named them first in its title and preamble, and the author petitions of mid-century — several drafted by the booksellers themselves — supplied the public case for longer terms.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, authors, payer).

% Buyers of books. During a title's term they paid the ceiling prices the statute set and met fewer competing editions; after expiry they gained cheap multitudes of reprints. They had no voice in the statute's making and no organization; their interest in cheap books was invoked rhetorically by every faction while remaining unrepresented.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, reading_public, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, reading_public, beneficiary).

% Edinburgh and Glasgow printing houses outside the London trade's compact. They reprinted English works where the statute's reach was disputed, undercut London prices, and bore the enforcement side: suits, seizures, and the campaigns of the 1750s that carried Scottish reprints before Westminster courts. Their working exit was jurisdictional — printing beyond the effective reach of English enforcement until union-era rulings narrowed it.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, provincial_scottish_printers, payer,
    organized, biographical, arbitrage, regional).

% Dublin's reprint trade, permanently outside Westminster's copyright jurisdiction and outside the negotiation that produced the statute. They supplied cheap English-language editions to Ireland and the colonies and would have opposed any settlement extending London's exclusivity over their markets; they were simply never consulted.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, irish_booksellers, excluded,
    organized, biographical, arbitrage, continental).

% Scholars reconstructing what the 1710 act did — from Patterson, Feather, Rose, and Deazley onward. They read the drafting record, the Commons journals, the trade ledgers, and the case law, and they see the whole board: the conceptual invention, the institutional handover, and the fact that the two arrived fused in a single document.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, ip_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, london_booksellers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled the book trade after the 1695 lapse of the Licensing Act: replaced unenforceable customary perpetual claims and open piracy with centrally registered, time-limited exclusive rights, made edition investment recoverable inside a fixed window, released old works into reprint competition at term's end, and capped sheet prices during term.
% TRANSFER_FUNCTION: Moves exclusive-market proceeds toward whoever holds the registered title: from unauthorized reprinters to title-holders by enforcement; from authors to purchasing booksellers at the moment of sale; from readers to the trade through term-length price ceilings; and, via the twenty-one-year backstop, from the pre-1710 public domain into the hands of the incumbent stock-owners.
% ABSENT_VOICES: Irish booksellers were outside the jurisdiction and outside the room entirely; Scottish reprinters had no seat at Westminster; readers were unorganized and unrepresented; rank-and-file authors spoke chiefly through petitions drafted by the very houses that bought their rights. Each would have objected to a different element — the backstop, the trade-run registry, the ceilings, the purchase-only design.
% DISAPPEARANCE_RATIONALE: If the 1710 settlement vanished overnight, the book trade would revert to the post-1695 condition it was built to escape: contested customary claims, unfinanceable editions, and litigation without a registry. Every downstream development — the term-and-renewal structure, the author-sale market, the case law running through Donaldson v Becket, and eventually modern copyright — presupposes the arrangement, so the world rearranges around whatever replaces it.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695, the Stationers' perpetual copyright lost its statutory cover, piracy spread, edition finance collapsed, and Parliament needed a settlement that encouraged learning without restoring pre-publication censorship or confirming perpetual trade monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the Commons Journals and drafting papers of 1706-1710 record the problem independently of the trade's testimony; Defoe's contemporaneous essays on the trade press the same grievance from a printer's seat; and the modern historiography (Patterson, Feather, Rose, Deazley) reconstructs the founding problem from archives the beneficiaries did not control. Whether the problem remains live is precisely what the parties dispute.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the twenty-one-year backstop transferred pre-1710 stock's exclusivity wholesale to incumbents, the purchase system routed the new rights through trade-offered lump sums, and term-length ceilings sustained elevated prices — offset by a real incentive effect on new composition and by term-end release into reprint competition. Suppression 0.55: registration at Stationers' Hall gated protection, the Company's searchers and the courts pursued reprinters, but enforcement was jurisdictionally leaky (Scotland until the 1750s, Ireland throughout). Theater_ratio 0.42: the learning-encouragement preamble was operative at the outset (caps, deposits, term limits) but a growing share of the arrangement's public face — the mid-century author petitions, the poor-writer rhetoric of the perpetual-right campaign — performed protection while the trade consolidated holdings; the ratio peaks at the campaign's height and eases slightly after 1774 forced the rhetoric and the legal reality back into contact. Accessibility_collapse 0.55: once the statute was understood, the statutory route dominated legitimate publishing, but patronage, import, and offshore reprinting persisted as partial alternatives. Resistance 0.6: Scottish and Irish reprinting, the perpetual-right litigation culminating in Donaldson v Becket, and recurring author discontent. The suppression_requirement series tracks a real enforcement-capacity arc — Company custom, then Chancery injunctions, then registration-backed litigation intensifying to its 1760s peak, then collapsing after the 1774 decision removed the perpetual claim's legal basis — which is why it is authored rather than left static. All three series share one time grid (nine points, eight-year steps, 1710-1774). Coalition note: the Scottish trade acted collectively against London enforcement and partially succeeded, so the organized payer seat carries real coalition leverage despite the arrangement's design.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the bookseller seat the arrangement is a settlement its members built, administer, and hold capital in — coordination-forward, with the backstop experienced as earned continuity. From the author seat the same document is a title held on paper and a cheque cashed early — the nominal/practical gap this reading places at the structure's center. From the reader seat it is prices during term and abundance after. From the Scottish printer seat it is a border with courts behind it. From the historian seat it is one act doing two things at once. The engine derives these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Booksellers sit nearest the beneficiary end: they collect the proceeds and run the machinery, and although their exit is constrained (their capital is the copyright ledger), their structural position is that of the arrangement's operator-collector. Authors are declared beneficiaries — the statute names them — but the derivation from that declaration alone would place them far toward the subsidized end, which is exactly what this reading denies: the purchase system transferred most of the new right's value at the moment of sale. The override sets the moderate-power seat (authors, uniquely) to d=0.5, encoding the nominal/practical divergence the reading asserts. Readers are declared cost-bearers but gain term-end abundance and price caps; the override sets the powerless seat (reading_public, uniquely) to d=0.6 rather than the near-full-target value a bare victim declaration would yield. Scottish printers derive near-full-target directionality with arbitrage damping; Irish printers are excluded rather than coordinated — their exclusion is part of what the settlement stabilized. Parliament sits near-symmetric as steward: it set the terms and bore little of either flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding settlement's specific mandate — calibrate terms to 1710 print economics and compensate incumbents for lost perpetual claims via a twenty-one-year backstop — was spent by the 1770s: the backstop lapsed, Donaldson v Becket rejected perpetuity, and the arrangement's center of gravity shifted to a general term-limited-rights principle no longer tied to the founding bargain. Mandatrophy is therefore resolved: the mandate outlived its original function, but the institution transformed rather than atrophied into performance — theater_ratio rose toward mid-century yet the coordination core (registered, time-limited rights financing editions) stayed live, which is why this resolves as transformation rather than piton decay. The entangled reading is what prevents misclassification at both ends: a pure conceptual reading scores the statute as a near-rope learning instrument and misses the backstop rents; a pure reallocation reading scores it as a near-snare rent handover and misses the genuine settlement of a coordination crisis. Holding both dimensions in one structure is the tangled_rope signature, and the ambiguous beneficiary pair is the mechanism that keeps the hybrid stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_entangled_event,
    'This constraint instantiates the entangled_event_reading of the statute_of_anne_ip_foundation kernel. Which structural facts would change if a sibling reading were adopted instead?',
    'Cross-read the sibling stories (statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading) against this one: convergence of beneficiary and victim sets indicates the disagreement is emphasis within one constraint; divergence indicates the readings instantiate different constraints with different epsilon.',
    'Under the conceptual_emergence_reading the beneficiary set clarifies toward the reading public and the cause of learning, and epsilon falls; under the institutional_reallocation_reading the victim set clarifies toward the Stationers'' Company and the beneficiary ambiguity dissolves. This reading''s ambiguous-beneficiary structure and its conceptual-clarity casualty are the delta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_entangled_event, conceptual, 'Committer-frame routing: which kernel, which reading, what the siblings would change structurally.').

omega_variable(
    nominal_versus_practical_author_gain,
    'Did authors gain net from the statute, or did the purchase system transfer most of the new right''s value to the booksellers who bought it?',
    'Trade ledgers and probate accounts of the major houses (Tonson, Lintot, Millar), together with author-earnings reconstructions covering 1710-1774, compared against the counterfactual of pre-1710 patronage and lump-sum copy sales.',
    'If authors net-gained, the beneficiary declaration stands and the overridden d=0.5 relaxes toward the beneficiary end; if the trade captured the surplus, authors belong among the cost-bearers outright, the victim set widens, and the arrangement shifts toward the snare end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_versus_practical_author_gain, empirical, 'Whether the nominal beneficiary realized the benefit the text assigns her.').

omega_variable(
    separability_of_dimensions_counterfactual,
    'Was the fusion of conceptual and institutional change in the 1710 act necessary, or could Parliament have enacted term-limited rights without handing administration to the incumbent trade?',
    'Counterfactual reconstruction from the drafting record: compare the 1706-1710 bill drafts and the defeated alternatives (perpetual-property bills, compulsory-licensing schemes, a crown-controlled registry) for administrable designs that bypassed Stationers'' Hall and the purchase-only market.',
    'If a clean separation was administratively available, the entangled reading weakens and the statute decomposes toward the two sibling stories; if every viable legislative path ran through the trade''s existing machinery, the fusion is structural and this reading stands as the correct description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_dimensions_counterfactual, conceptual, 'Whether the entanglement was contingent on the legislative moment or necessary to any settlement.').

omega_variable(
    conceptual_clarity_as_cost_bearer,
    'The reading names conceptual clarity as the casualty of the fusion — is that a real cost borne by identifiable parties, or a rhetorical summary of dispersed interpretive labor?',
    'Trace the downstream doctrinal and legislative labor attributable to the ambiguity: the Tonson v Collins and Donaldson v Becket litigation chain, the 1774 resolution''s aftermath, nineteenth-century codification debates, and the recurring modern dispute over copyright''s purpose.',
    'If the ambiguity generated measurable litigation, repeated relitigation, and doctrinal instability, the cost is real and belongs in the extraction account; if it was absorbed cheaply by the legal system, the victim declaration overstates and epsilon falls toward the rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_clarity_as_cost_bearer, conceptual, 'Status of the reading''s distinctive claim that the victim is conceptual clarity.').

omega_variable(
    enforcement_attribution_company_custom,
    'How much of the measured suppression was created by the statute versus inherited from the Stationers'' Company''s pre-1710 policing customs?',
    'Compare enforcement volumes, methods, and personnel before and after 1710 using the Company courtbooks, hall records, and Chancery suit filings across the interval.',
    'If most suppressive force predates the statute, the arrangement''s own suppression is lower than authored and the 1710 act reads closer to a reallocation of an existing coercive apparatus than a new one; if enforcement intensified under the new terms as authored, the trajectory stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_attribution_company_custom, empirical, 'Attribution of suppressive force between the statute and the inherited Company machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anne_entangled_tr_t0, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anne_entangled_tr_t8, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(anne_entangled_tr_t16, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(anne_entangled_tr_t24, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(anne_entangled_tr_t32, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(anne_entangled_tr_t40, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(anne_entangled_tr_t48, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(anne_entangled_tr_t56, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 56, 0.45).
narrative_ontology:measurement(anne_entangled_tr_t64, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 64, 0.42).

% Extraction over time
narrative_ontology:measurement(anne_entangled_be_t0, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(anne_entangled_be_t8, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(anne_entangled_be_t16, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(anne_entangled_be_t24, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(anne_entangled_be_t32, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(anne_entangled_be_t40, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(anne_entangled_be_t48, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(anne_entangled_be_t56, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 56, 0.74).
narrative_ontology:measurement(anne_entangled_be_t64, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 64, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(anne_entangled_su_t0, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anne_entangled_su_t8, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(anne_entangled_su_t16, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(anne_entangled_su_t24, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(anne_entangled_su_t32, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(anne_entangled_su_t40, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(anne_entangled_su_t48, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 48, 0.76).
narrative_ontology:measurement(anne_entangled_su_t56, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 56, 0.79).
narrative_ontology:measurement(anne_entangled_su_t64, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 64, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Statute of Anne founded copyright' conflates a conceptual claim (a new kind of limited right came into existence) with an institutional claim (occupancy of the printing-rights space changed hands). Per the epsilon-invariance principle these are separate stories: the conceptual_emergence_reading carries low epsilon (a regulatory instrument for learning, negligible extraction), the institutional_reallocation_reading carries moderate epsilon (a rent handover with an efficiency rationale), and this entangled_event_reading carries the highest epsilon of the family (0.66) because the fusion preserves both the backstop rents and the beneficiary ambiguity, and names conceptual clarity as the standing casualty. The upstream member (conceptual_emergence_reading) is typically cited as evidence within the other two, since the learning-serving conceptual story is what legitimates the arrangement whose operation the other readings assess. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, moderate, 0.5).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
