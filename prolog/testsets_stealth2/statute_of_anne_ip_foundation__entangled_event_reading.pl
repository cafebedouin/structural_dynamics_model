% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Statute of Anne: The Fused Conceptual-Institutional Grant (Entangled Event Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is the founding act of Anglo-American
 *   copyright, and this story instantiates the entangled_event_reading of the
 *   statute_of_anne_ip_foundation kernel: the act fused two changes in one
 *   instrument — copyright became thinkable (a limited statutory right
 *   sourced in the author, replacing perpetual charter monopoly) and was
 *   first held (the trade re-occupied the new structure through assignments
 *   within a generation) — and the two dimensions cannot be cleanly
 *   disentangled. The ε referent is the standing arrangement under contest:
 *   the statutory grant structure as it actually operated, 1710–1774,
 *   assessed by this reading's own lights, which sees the innovation and the
 *   capture as one structure and names conceptual clarity as the casualty,
 *   because every later attempt to attribute the act's effects must first
 *   undo a fusion the act itself performed. This is one of three linked
 *   readings of the same kernel: the conceptual_emergence_reading authors low
 *   ε over the same referent (a liberating regulatory innovation); the
 *   institutional_reallocation_reading authors moderate ε (a transfer of
 *   existing rights between holders); this reading authors ε = 0.62
 *   (innovation and capture fused, plus an epistemic cost the clean readings
 *   do not price). The readings are separate constraint stories linked by
 *   network.affects_constraints; the contest between them is carried in omega
 *   variables, not inside this constraint.
 *
 * KEY AGENTS:
 *   - parliament_of_1709: agenda setter (institutional/arbitrage) — drafted and enacted the fused instrument; can amend terms but cannot touch one dimension without re-opening the other
 *   - working_authors: nominal beneficiary, practical assignor (moderate/constrained) — named source of right; monetized only by selling to the trade
 *   - bookseller_publishers: practical beneficiary (organized/constrained) — bought assignments, consolidated holdings, financed the perpetual-right litigation, collected the income
 *   - reading_public: payer with no seat (powerless/trapped) — bore maintained prices; the 'learning' the preamble invoked
 *   - stationers_company: registry-administrator (institutional/constrained) — lost the charter monopoly, kept the Hall registration on which the statutory right depended
 *   - scottish_irish_reprint_trades: suppressed alternative (organized/constrained) — two generations of litigation resistance, culminating in Donaldson
 *   - westminster_courts: interpretive agenda setter (institutional/constrained) — Millar v Taylor (1769) and Donaldson v Beckett (1774) set the arrangement's operative content
 *   - conceptual_clarity: non-agent casualty — the epistemic victim of the fusion; cannot speak, litigate, or exit
 *   - legal_historians: analytical observer — sees the full structure and authors the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.62).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne: The Fused Conceptual-Institutional Grant (Entangled Event Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'a80dee45-dd83-4420-a332-05a8e4fef324').
narrative_ontology:cs_kernel_codification('a80dee45-dd83-4420-a332-05a8e4fef324', fixed_text).
narrative_ontology:cs_authority_grounding('a80dee45-dd83-4420-a332-05a8e4fef324', lineage).
narrative_ontology:cs_interpretation_layer_present('a80dee45-dd83-4420-a332-05a8e4fef324').
narrative_ontology:cs_reading_relation('a80dee45-dd83-4420-a332-05a8e4fef324', statute_of_anne_ip_foundation__conceptual_emergence_reading, influences).
narrative_ontology:cs_reading_relation('a80dee45-dd83-4420-a332-05a8e4fef324', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_axiom('a80dee45-dd83-4420-a332-05a8e4fef324', foundational, conceptual_institutional_dimensions_inseparable).
narrative_ontology:cs_axiom_status(conceptual_institutional_dimensions_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('a80dee45-dd83-4420-a332-05a8e4fef324', conceptual_institutional_dimensions_inseparable, empirically_contingent).
narrative_ontology:cs_axiom('a80dee45-dd83-4420-a332-05a8e4fef324', secondary, grant_beneficiary_constitutively_ambiguous).
narrative_ontology:cs_axiom_status(grant_beneficiary_constitutively_ambiguous, holdable).
narrative_ontology:cs_axiom_grounding('a80dee45-dd83-4420-a332-05a8e4fef324', grant_beneficiary_constitutively_ambiguous, empirically_contingent).
narrative_ontology:cs_reference_frame('a80dee45-dd83-4420-a332-05a8e4fef324', fused_conceptual_institutional_act).
narrative_ontology:cs_drift_state('a80dee45-dd83-4420-a332-05a8e4fef324', contemporary_historiography, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a80dee45-dd83-4420-a332-05a8e4fef324', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, working_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, bookseller_publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, scottish_irish_reprint_trades).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, working_authors).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, statutory_limited_term_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, authorial_personhood_in_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1710 statute in a single instrument that both named the author as the source of a new limited printing right and settled the book trade's rights structure after the Licensing Act lapsed. It can amend terms and did so under pressure, but it cannot adjust one dimension of the act without re-opening the other; every later revision re-litigates the whole settlement.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament_of_1709, agenda_setter,
    institutional, generational, arbitrage, national).

% Are named in the statute as the persons from whom the printing right flows, for a fourteen-year term renewable if they live to see it end. They hold no printing capital and no retail channel, so the right is monetizable only by assignment to the London booksellers, typically for a lump sum; the asset's long-term appreciation passes at sale. Their gain is the existence of the salable right; their cost is where its value comes to rest.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, working_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, working_authors, payer).

% Are the London trade that bought assignments of author copyrights, consolidated them into stable holdings passed down through firms and partnerships, financed the litigation that sought to make the right perpetual at common law, and set the prices at which books sold during the terms. Their capital and market position are the arrangement's practical substance; exit would mean liquidating the asset base that constitutes the trade itself.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, bookseller_publishers, beneficiary,
    organized, generational, constrained, national).

% Buys the books whose prices the trade maintains during the statutory terms, and is the 'learning' the preamble invokes without ever seating it. Cheap lawful alternatives — Scottish reprints, imported editions — were suppressed by the statute and its enforcement; forgoing books is the only remaining option. Its scheduled gain, the public domain, arrives only on the term clock and only for works whose holders do not renew.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, reading_public, payer,
    powerless, generational, trapped, national).

% Lost its charter monopoly when the statute passed but kept the registration function: entry at Stationers' Hall was a condition of the statutory right, so the Company's machinery remained the gate through which every claim passed. Its member booksellers are the trade that collects; the Company itself persists on the fees and the relevance the registration role confers. Its charter identity is fused with administering the trade it once monopolized.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, beneficiary).

% Printed cheap reprints of English works and built a substantial trade on the price gap the statutory terms created. The statute and subsequent enforcement aimed at closing that gap; the trade fought back through two generations of litigation and parliamentary agitation, culminating in the House of Lords' 1774 decision. Its options throughout were fight, comply, or leave the trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, scottish_irish_reprint_trades, payer,
    organized, biographical, constrained, national).

% Adjudicated what the statute's words meant in practice: Chancery granted and refused injunctions against reprinting, the common-law courts heard the booksellers' claim that the right predated and outlasted the statute, and the House of Lords in 1774 rejected the perpetual claim and affirmed the statutory terms. Its rulings, not the text alone, set the arrangement's operative content across the interval.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, westminster_courts, agenda_setter,
    institutional, generational, constrained, national).

% The analytical casualty of the act's form: because the conceptual and institutional changes were enacted in one instrument, no later analysis can attribute the statute's effects to one dimension without counterfactual reconstruction, and the resulting ambiguity was exploitable — the booksellers argued from the author's natural property against the statutory term for sixty years. This entry is a non-agent kept for narrative completeness; it cannot speak, litigate, or exit, and it is excluded from the engine's directional arithmetic.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).

% Study the act and its aftermath from outside the period's bargaining: they reconstruct the parliamentary record, the assignment market, and the litigation, and they author the competing accounts — conceptual emergence, institutional reallocation, and the entangled reading instantiated in this story. They collect nothing and pay nothing; their stake is analytic.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, bookseller_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute replaced the Stationers' indefinite charter monopoly with a fixed-term, registrable, transferable exclusive printing right: a predictable asset the trade could finance, a registration and deposit record, and a scheduled entry of works into the public domain — settled once, by statute, for the whole print market.
% TRANSFER_FUNCTION: Moves exclusive printing rights — and the income they carry — nominally from the Crown-charter settlement to authors for limited terms, and practically into the hands of the London bookseller-publishers who bought assignments; moves costs to readers (prices maintained through the terms) and to the Scottish and Irish reprint trades (their market suppressed by statute and enforcement).
% ABSENT_VOICES: The reading public had no seat: 'the encouragement of learning' was spoken in the preamble by the very trade that priced the books. The Scottish and Irish reprint trades were excluded by jurisdiction and enforcement and could object only as litigants. Future readers — the public-domain beneficiaries — had no voice in term design. Their absence is what let the beneficiary ambiguity stand unexamined for sixty years.
% DISAPPEARANCE_RATIONALE: If the statute vanished in 1710, the Stationers' charter monopoly and common-law property claims would have re-filled the space: the trade's asset structure, the assignment market, the scheduled public domain, and the entire Anglo-American copyright lineage that traces to this act would not exist in their actual form. The world rearranges around whichever settlement — charter, common law, or none — re-emerged.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695 the print trade had no settled legal basis: the Stationers' Company enforced an effective perpetual monopoly through its charter and common-law claims, unlicensed printing proliferated, and neither authors nor the public held any recognized position. The statute was built to end the monopoly, settle printing rights for limited terms, and encourage learning.
% FOUNDING_PROBLEM_CORROBORATION: The House of Lords in Donaldson v Beckett (1774) attests from outside the beneficiary set that the perpetual-right claim failed and the statutory settlement governs; the Scottish booksellers' two generations of litigation attest the cost of the suppression the settlement required; modern legal-historical scholarship (Rose, Feather, Deazley, Johns) attests the partial resolution from the analytical seat. No attestation exists from the reading public itself — the seat the preamble invoked — because it never held one; that silence is itself signal.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claim — tangled_rope — is what the entangled reading holds structurally true: the arrangement has a genuine coordination core (fixed terms, registration, deposit, a scheduled public domain, a financeable asset) AND asymmetric extraction (the nominal author-grant was captured at assignment; readers paid trade-maintained prices; the cheaper reprint alternative was suppressed), held together by active enforcement through Stationers' Hall, the courts, and a fifty-year litigation campaign. Extractiveness 0.62: the trade captured most of the income while the term-limit machinery remained real and the 1774 decision re-affirmed it. Suppression 0.58: enforcement is structural — registration conditions, injunctions, prosecutions — not interpersonal. Theater 0.55: the author-centered framing performed a legitimacy the trade had already re-occupied, while the machinery underneath stayed functional. The measurement series share one grid (1710–1774, eight points, every tracked metric at every point). suppression_requirement is authored because enforcement capacity is this story's dynamic: it ratchets up through the common-law campaign to its Millar v Taylor peak (0.78, 1769) and collapses at Donaldson (0.58, 1774) when the Lords rejected perpetual right — enforcement decay, not extraction decay, is what moved. gain_flow is authored bookseller_publishers: the income demonstrably accrued to the London trade — the affirmative receipt claim this reading's ambiguity thesis predicts. fixing_cost is authored prohibitive: because the structure is fused, any parliamentary correction re-opened both dimensions at once against an organized trade, at a cost to the fixer exceeding the diffuse reader benefit; the 1774 decision shows the courts could prune a branch, not that Parliament could have re-rooted the tree cheaply.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the bookseller seat the arrangement is a settled asset market it financed, litigated for, and defends — coordination it helped build. From the author seat it is a nominal grant captured at assignment — a right wearing the author's name whose value goes elsewhere. From the reader seat it is maintained prices and suppressed alternatives with no voice in the room. From the analytical seat the finding is the fusion itself: no historical seat sees the whole, and the seat that names the whole (this reading) is not a historical party at all. The engine computes per-seat types from the structural data; that divergence is the measurement, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   bookseller_publishers derive near-full-beneficiary d from the beneficiary declaration plus organized power. reading_public and scottish_irish_reprint_trades derive near-full-target d from the victim declarations — readers trapped inside maintained prices, reprint trades facing the statute's coercive edge. working_authors are the override case: the derivation would read pure beneficiary from the grant clause (low d), but the assignment market made their net position symmetric — nominal gain, practical capture — so a directionality override pins the moderate atom at d = 0.5, the reading's 'authors nominal, publishers practical' rendered as arithmetic. conceptual_clarity is authored as a victim but carries agent: false, so it is excluded from the d-to-chi derivation: the epistemic casualty cannot enter the arithmetic, which is itself an instance of the reading's thesis — the fusion's victim is the one party that cannot litigate its way out. The agenda seats (parliament_of_1709, stationers_company, westminster_courts) take power-atom fallbacks; note that the schema's coarse institutional atom cannot separate Parliament from the Company — a small structural echo of the reading's claim that the act's institutional dimension resists clean separation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding act is routinely mythologized as a pure origin — either a mountain-like birth of a natural institution or a clean public-regarding bargain. The entangled reading blocks both mislabels. Treating it as pure coordination erases the capture (the assignment consolidation, the suppressed reprints, the sixty-year fight for perpetual right); treating it as pure extraction erases the real coordination (terms, registration, the scheduled public domain the Lords protected in 1774) and the post-decision price falls and trade expansion. tangled_rope holds both, and the R5 interview locates the mandatrophy question precisely: the founding problem — ending the charter monopoly, encouraging learning — is contested, solved in law and re-formed in practice through private holdings. The arrangement persists because each dimension keeps the other alive: the author-framing legitimates the trade's asset, and the trade's enforcement gives the framing force. founding_problem_status = contested with disappearance_verdict = world_rearranges produces no dead-mandate mismatch flag, correctly: this arrangement runs on a live, disputed mandate, not a dead one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_separability_dispute,
    'Can the statute''s conceptual and institutional dimensions be cleanly attributed (as each sibling reading attempts), or are they constitutively fused as this reading holds?',
    'Counterfactual archival analysis of the 1709-10 parliamentary bargain: would the trade settlement have held without the author-source clause, and would the author-right have been enacted without the trade settlement; plus historiographic convergence testing across the three readings'' evidence bases.',
    'Resolution toward a clean reading dissolves this story''s distinctive structure into the sibling''s: conceptual emergence would lower epsilon and clarify the beneficiary; pure reallocation would shift the victim set toward the displaced monopolists and reframe the grant as transfer rather than creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_separability_dispute, conceptual, 'The kernel''s central contest: separability of the act''s two dimensions.').

omega_variable(
    nominal_vs_practical_beneficiary,
    'Did working authors net-gain under the statutory grant (making the beneficiary ambiguity rhetorical), or did the assignment market capture the upside (making it structural)?',
    'Assignment-price series and author earnings against the trade''s resale and licensing values, 1710-1774; lump-sum market analysis of copyright sales and the conger''s consolidation record.',
    'If authors net-gained, their directionality drops toward the beneficiary end and the extraction asymmetry rests on readers and reprint trades alone; if net-captured, effective extraction is higher than authored and the tangled_rope leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_vs_practical_beneficiary, empirical, 'Whether the beneficiary ambiguity is rhetorical or structural.').

omega_variable(
    epistemic_victim_status,
    'Does ''conceptual clarity'' bear genuine structural victimhood, or is it a framing artifact of this reading — did the fusion have exploitable consequences, or only retrospective ones?',
    'Test the exploitation record directly: the perpetual-copyright litigation argued the conceptual dimension (the author''s natural property) against the institutional dimension (the statutory term) for sixty years; if the fusion generated no such exploitable ambiguity in its own time, the victim set shrinks to the actor victims.',
    'If the epistemic casualty is real, this reading''s victim set is genuinely tri-partite and its epsilon premium over the clean readings is justified; if not, this story converges toward the institutional_reallocation_reading''s structure and conceptual_clarity should be retired from the victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_victim_status, conceptual, 'Whether the reading''s distinctive victim is structural or rhetorical.').

omega_variable(
    term_extension_recurrence,
    'Does the entanglement recur at each subsequent term-extension episode — the conceptual dimension (authors'' due) re-invoked to extend the institutional grant — and if so, does the long-run trajectory bend toward snare?',
    'Compare the 1814, 1842, 1911, 1956, 1988 and EU-harmonization term debates for author-framing deployed in trade-favoring extensions; code each episode for fusion-exploitation (author-natural-right arguments attached to publisher-favoring outcomes).',
    'Recurrence would push the constraint''s long-run classification toward snare (the framing as a recurring extraction instrument rather than a one-time founding condition); its absence would support tangled_rope settling toward rope after the 1774 settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_extension_recurrence, empirical, 'Whether the fusion is a one-time founding condition or a recurring extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soa_entangled_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.35).
narrative_ontology:measurement_basis(soa_entangled_tr_t1710, observed).
narrative_ontology:measurement(soa_entangled_tr_t1720, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1720, 0.42).
narrative_ontology:measurement_basis(soa_entangled_tr_t1720, observed).
narrative_ontology:measurement(soa_entangled_tr_t1730, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1730, 0.46).
narrative_ontology:measurement_basis(soa_entangled_tr_t1730, observed).
narrative_ontology:measurement(soa_entangled_tr_t1740, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1740, 0.5).
narrative_ontology:measurement_basis(soa_entangled_tr_t1740, observed).
narrative_ontology:measurement(soa_entangled_tr_t1750, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1750, 0.55).
narrative_ontology:measurement_basis(soa_entangled_tr_t1750, observed).
narrative_ontology:measurement(soa_entangled_tr_t1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1760, 0.58).
narrative_ontology:measurement_basis(soa_entangled_tr_t1760, observed).
narrative_ontology:measurement(soa_entangled_tr_t1769, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1769, 0.62).
narrative_ontology:measurement_basis(soa_entangled_tr_t1769, observed).
narrative_ontology:measurement(soa_entangled_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.55).
narrative_ontology:measurement_basis(soa_entangled_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(soa_entangled_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement_basis(soa_entangled_be_t1710, observed).
narrative_ontology:measurement(soa_entangled_be_t1720, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1720, 0.55).
narrative_ontology:measurement_basis(soa_entangled_be_t1720, observed).
narrative_ontology:measurement(soa_entangled_be_t1730, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1730, 0.58).
narrative_ontology:measurement_basis(soa_entangled_be_t1730, observed).
narrative_ontology:measurement(soa_entangled_be_t1740, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1740, 0.6).
narrative_ontology:measurement_basis(soa_entangled_be_t1740, observed).
narrative_ontology:measurement(soa_entangled_be_t1750, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1750, 0.63).
narrative_ontology:measurement_basis(soa_entangled_be_t1750, observed).
narrative_ontology:measurement(soa_entangled_be_t1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1760, 0.65).
narrative_ontology:measurement_basis(soa_entangled_be_t1760, observed).
narrative_ontology:measurement(soa_entangled_be_t1769, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1769, 0.7).
narrative_ontology:measurement_basis(soa_entangled_be_t1769, observed).
narrative_ontology:measurement(soa_entangled_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.62).
narrative_ontology:measurement_basis(soa_entangled_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(soa_entangled_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.45).
narrative_ontology:measurement_basis(soa_entangled_su_t1710, observed).
narrative_ontology:measurement(soa_entangled_su_t1720, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1720, 0.5).
narrative_ontology:measurement_basis(soa_entangled_su_t1720, observed).
narrative_ontology:measurement(soa_entangled_su_t1730, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1730, 0.55).
narrative_ontology:measurement_basis(soa_entangled_su_t1730, observed).
narrative_ontology:measurement(soa_entangled_su_t1740, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1740, 0.6).
narrative_ontology:measurement_basis(soa_entangled_su_t1740, observed).
narrative_ontology:measurement(soa_entangled_su_t1750, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1750, 0.66).
narrative_ontology:measurement_basis(soa_entangled_su_t1750, observed).
narrative_ontology:measurement(soa_entangled_su_t1760, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1760, 0.72).
narrative_ontology:measurement_basis(soa_entangled_su_t1760, observed).
narrative_ontology:measurement(soa_entangled_su_t1769, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1769, 0.78).
narrative_ontology:measurement_basis(soa_entangled_su_t1769, observed).
narrative_ontology:measurement(soa_entangled_su_t1774, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1774, 0.58).
narrative_ontology:measurement_basis(soa_entangled_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (statute_of_anne_ip_foundation), three readings, three stories, linked by affects_constraints in both directions. The epsilon values differ by construction over the shared referent (the standing arrangement, the statute's grant structure as operated): conceptual_emergence_reading authors low epsilon (a liberating regulatory innovation); institutional_reallocation_reading authors moderate epsilon (a transfer of existing rights); this reading authors 0.62 because it holds the innovation and the capture to be one structure and prices the epistemic casualty the clean readings do not. reading_relations from this story to both siblings are authored as influences rather than coexists_with or forecloses: this reading affirms the content of both sibling accounts while denying their isolability, so it does not logically eliminate either, but its articulation changes what each clean attribution must now defend (separability) — a legitimacy-condition change without resolution. The siblings are upstream in evidential establishment; this reading is articulated against both and consumes their evidence bases as raw material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
