% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Statute of Anne: Institutional Reallocation of Printing Rights (1710-1842)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   England's book trade entered the eighteenth century without an
 *   enforceable rights framework: the Licensing Act lapsed in 1695, the
 *   Stationers' Company's register-based monopoly lost its statutory
 *   foundation, and piratical reprinting spread. The Statute of Anne (1710)
 *   answered the vacuum by vesting exclusive rights in new books first in
 *   their authors, for fourteen-year terms renewable once, with registration
 *   at Stationers' Hall and infringement actions for enforcement. This story
 *   instantiates the institutional_reallocation_reading of that event: the
 *   statute's operative structure was the transfer of an existing
 *   institutional space, the printing-rights regime the Stationers' Company
 *   had occupied, to a new holder class. Authors held the new right nominally
 *   and first; London bookseller-publishers held it effectively, buying
 *   assignments at lump-sum discounts and rebuilding concentrated copyright
 *   libraries in statutory form. The dispossessed party is the Stationers'
 *   Company, which lost its perpetual monopoly while retaining the
 *   registration office. The claimed type and the metrics are authored
 *   independently: the claim is tangled_rope; the metrics describe what the
 *   arrangement actually did across 1710-1842, and any divergence between
 *   claim and computed type is the measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - london_bookseller_publishers: effective rights-holder and primary beneficiary (institutional / constrained) — receives assignments, assembles copyright libraries, captures the regime's gains
 *   - stationers_company: dispossessed prior occupant (institutional / identity_locked) — loses the perpetual register monopoly, retains the statutory registration office
 *   - manuscript_authors: nominal first-vesting class (moderate / constrained) — holds the right first, assigns it almost immediately
 *   - parliament: agenda setter (institutional / arbitrage) — enacted the terms and retains amendment and repeal power
 *   - reading_public: diffuse beneficiary-payer (powerless / constrained) — pays term-length monopoly prices, gains post-term reprints
 *   - scottish_reprinters: excluded trade (organized / trapped) — outside the grant, principal enforcement target until 1774
 *   - courts_of_westminster: analytical observer (institutional / analytical) — fixed the ceiling the arrangement operates under (Donaldson v Becket, 1774)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.7).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Printing Rights (1710-1842)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '204c4dc8-d881-4629-ac95-033760a68722').
narrative_ontology:cs_kernel_codification('204c4dc8-d881-4629-ac95-033760a68722', formalized).
narrative_ontology:cs_authority_grounding('204c4dc8-d881-4629-ac95-033760a68722', lineage).
narrative_ontology:cs_interpretation_layer_present('204c4dc8-d881-4629-ac95-033760a68722').
narrative_ontology:cs_reading_relation('204c4dc8-d881-4629-ac95-033760a68722', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('204c4dc8-d881-4629-ac95-033760a68722', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('204c4dc8-d881-4629-ac95-033760a68722', foundational, rights_reallocation_not_creation).
narrative_ontology:cs_axiom_status(rights_reallocation_not_creation, holdable).
narrative_ontology:cs_axiom_grounding('204c4dc8-d881-4629-ac95-033760a68722', rights_reallocation_not_creation, empirically_contingent).
narrative_ontology:cs_axiom('204c4dc8-d881-4629-ac95-033760a68722', secondary, author_vesting_serves_assignee_interests).
narrative_ontology:cs_axiom_status(author_vesting_serves_assignee_interests, holdable).
narrative_ontology:cs_axiom_grounding('204c4dc8-d881-4629-ac95-033760a68722', author_vesting_serves_assignee_interests, empirically_contingent).
narrative_ontology:cs_reference_frame('204c4dc8-d881-4629-ac95-033760a68722', stationers_occupied_printing_space).
narrative_ontology:cs_drift_state('204c4dc8-d881-4629-ac95-033760a68722', statute_repeal_1842, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('204c4dc8-d881-4629-ac95-033760a68722', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_bookseller_publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, manuscript_authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, manuscript_authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% London wholesale booksellers and printing-house proprietors who lobbied for the statute after the Licensing Act lapsed in 1695. They acquire exclusive rights in new titles by buying assignments from authors, assemble large copyright libraries, and enforce those rights through Stationers' Hall registration and infringement actions. Their trade dynasties span generations and their capital and parliamentary access let them shape renewal legislation in 1739, 1814, and 1842. Leaving the book trade would mean abandoning their entire capital base.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_bookseller_publishers, beneficiary,
    institutional, generational, constrained, national).

% The City of London livery company that operated the register and had held the de facto perpetual printing monopoly under the licensing system. The statute stripped the Company's corporate monopoly while leaving it the statutory registration office at Stationers' Hall. Its members lost the rent stream from perpetual titles but kept their trade roles as printers and distributors. The Company's institutional identity is fused with its register function; it cannot abandon that role without ceasing to be itself.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, identity_locked, national).

% Writers of new works in whose name the statute vests exclusive rights first. Most sell the entire right to a bookseller for a one-time payment before or at publication; a few prominent authors such as Pope and Gay negotiated directly and profitably. Their livelihoods depend on the London trade's distribution network, and no alternative channel to readers exists in this period.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, manuscript_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, manuscript_authors, payer).

% The Westminster legislature that enacted the statute, fixed its fourteen-year terms, and retains power to amend, extend, or repeal it. It responds to organized trade lobbying, to the learning-encouragement rhetoric of the preamble, and later to renewal campaigns; it restructured the terms in 1739, 1814, and 1842, each time at low cost to itself.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Book buyers and readers, diffuse and unorganized. During the statutory term they pay whatever the rights-holder charges for a new title; after expiry they gain cheap reprints as the title enters common use. They benefit from the promise of new works the regime encourages and bear the term-length pricing the regime permits. A specific in-term title is obtainable from no lawful alternative source.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, payer).

% Edinburgh and Glasgow printing houses whose trade model reprinted English titles without assignment. They sit outside the statutory grant and were its principal enforcement targets through customs seizures and Chancery actions until the 1774 settlement. Their domestic market offered no equivalent base of original publishing to retreat into.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, scottish_reprinters, excluded,
    organized, biographical, trapped, national).

% Chancery, King's Bench, and on appeal the House of Lords, which adjudicated what the reallocation meant: whether the statutory term exhausted the right or left a perpetual common-law copyright beneath it (Tonson v Collins, Millar v Taylor, Donaldson v Becket). They take no side in the trade's gains and losses; they fix the ceiling the arrangement operates under.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, courts_of_westminster, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, london_bookseller_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the Licensing Act lapsed in 1695, the book trade had no uniform, enforceable framework for exclusive rights in texts: investment in new works was insecure, piracy was rampant, and the trade's informal register system had lost legal force. The statute solved this with a single regime of fixed terms, registration at Stationers' Hall, and infringement actions, coordinating publishers, authors, and courts around one enforceable rights structure.
% TRANSFER_FUNCTION: Moves exclusive commercial rights in new texts first to authors and then, overwhelmingly, to London bookseller-publishers via lump-sum assignment; moves term-length pricing power to rights-holders; and strips the Stationers' Company's perpetual register monopoly, transferring occupation of the rights space from the Company to the assignment-buying trade.
% ABSENT_VOICES: Scottish printers were outside the grant and absent from the 1709-1710 negotiation, yet bore the enforcement that followed. Unorganized readers had no seat; no one at the table represented the price-paying public. Authors were present only through the booksellers who claimed to speak for them. The bargain was struck between parliamentary leadership and London trade interests.
% DISAPPEARANCE_RATIONALE: If the statute and its reallocation vanished overnight, the trade would revert to the post-1695 condition: no enforceable exclusive rights, renewed parliamentary attempts at a licensing settlement, Scottish and provincial reprinting expanding immediately, and the Stationers' Company attempting to reassert register control. The institutional space would be re-occupied by some arrangement, since printing rights are always held by someone, but it would not re-arrange into this arrangement without the statute's specific terms.
% FOUNDING_PROBLEM: The lapse of the Licensing Act in 1695 left English printing without any statutory foundation: the Stationers' Company's register monopoly lost legal force, piracy spread, and Parliament faced simultaneous pressure from the trade for a new rights settlement and from reformers against perpetual monopoly. The statute was built to fill that vacuum on new terms: limited, author-vested, registered, enforceable.
% FOUNDING_PROBLEM_CORROBORATION: The Stationers' Company's own 1695-1710 petitions for a new licensing act corroborate the vacuum, attested from the dispossessed party rather than the beneficiary set. Contemporary parliamentary journals record the learning-encouragement rationale. The Donaldson litigation record (1774) shows both sides stipulating that the statute was passed to secure a limited term after the licensing collapse. Later historiography from outside the benefiting parties (Patterson 1968; Feather 1980) corroborates the bargain structure. No source in the contest disputes that the vacuum was the occasion; what remains contested is whether the statute's essence was the vacuum's cure or the occupant change this reading emphasizes.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Base extractiveness is 0.70 at the referent arrangement as it stood at interval end: a time-limited exclusive-rights regime whose gains flowed via assignment to a concentrated London trade — below the old perpetual monopoly's level in its early decades, rising as copyright libraries consolidated, checked at Donaldson (1774) when the House of Lords confirmed the statutory ceiling, rising again as term extensions accumulated to the 1842 act. Suppression is 0.58 as a raw structural property, unscaled by power or scope: the regime required registration formalities, infringement litigation, customs seizures against Scottish and foreign reprints, and a decades-long litigation campaign, but it did not need to seal exits the way a purely coercive arrangement would, because term expiry and the post-Donaldson ceiling left lawful space outside the rights-holders' control. Theater is 0.48: the author-first vesting and the learning-encouragement preamble were real but partly ornamental from the start, since the operative bargain ran through assignment, and theatricality peaked when the trade argued perpetual common-law copyright beneath the statutory term (Millar v Taylor, 1769) before Donaldson stripped that position. Accessibility collapse is 0.45: alternatives persisted for decades (Scottish reprinting, term expiry, imported books) and were suppressed rather than never existing. Resistance is 0.55: the Stationers' petitioned for licensing restoration, Scottish presses defied enforcement for two generations, the trade itself resisted the statutory ceiling through the perpetual-copyright campaign, and reading interests resisted term extension through the 1814 and 1842 debates. The three measurement series share one time grid (t = 0, 22, 44, 64, 88, 110, 132 of a 1710-1842 interval) so no metric is ever sampled against another metric's end-state; the Donaldson hinge at t=64 is the interval's structural event, not noise. Identity-lock note: the stationers_company seat is bound by institutional identity fusion — the Company has become its register function — so its resistance to the reallocation persisted long after the economic loss was sunk; if the Company had reconstituted as a pure trade association, its exit would have loosened and the persistence of its opposition, though not its directionality, would have changed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the london_bookseller_publishers seat the arrangement is a hard-won legal foundation: trade chaos replaced by enforceable, transferable rights they built the lobbying coalition to obtain — low directionality, coordination-dominant. From the stationers_company seat the same statute is expropriation: a centuries-old institutional space stripped from its occupant by a bargain struck over its head — high directionality, identity-locked exit. From the manuscript_authors seat it is a near-symmetric honor: a real but immediately alienable right (override d = 0.45). From the reading_public seat it is slightly target-side: new works exist, but in-term titles carry monopoly prices (override d = 0.6). Parliament experiences a settled policy instrument it can restructure at will; the courts experience a ceiling to police. Same statute, different constraint per seat; the engine computes the divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (london_bookseller_publishers, manuscript_authors) derive low directionality; the victim declaration (stationers_company) derives high directionality. Two overrides correct derivations the declarations alone get wrong. First, manuscript_authors are declared beneficiaries because the statute vests in them first, but the derivation would place them deep in beneficiary territory while the assignment market made their realized position near-symmetric — lump-sum sale before publication, no participation in the right's appreciation — so d is overridden to 0.45 at power atom 'moderate'. Second, reading_public is not declared a victim because this reading's structural delta locates the harm in the Stationers' dispossession, but the derivation from their incidental beneficiary status would place them deep on the beneficiary side while in-term monopoly pricing puts them slightly target-side, so d is overridden to 0.6 at power atom 'powerless'. london_bookseller_publishers and stationers_company need no overrides: the declarations match their structural positions. Parliament and the courts carry no beneficiary or victim declaration; Parliament holds the agenda-setter seat (directionality from the power-atom fallback) and the courts hold the analytical seat. Scope is national throughout: the statute governed Great Britain, and larger-scope verification costs are the engine's to apply.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a workable rights regime after the 1695 licensing collapse, and the breaking of perpetual monopoly — was substantially solved within a generation of enactment. The arrangement then persisted for over a century and expanded past its founding terms: term extensions in 1739, 1814, and 1842, and the entrenchment of the copyright libraries. Authoring founding_problem_status as 'dead' against disappearance_verdict 'world_rearranges' produces exactly the mismatch the engine cross-checks against the computed theater and piton paths: the arrangement's original mandate is gone but the world is still organized around it. The tangled_rope claim prevents mislabeling in both directions. A pure extraction reading would erase the genuine coordination function — without the statute the trade had no enforceable foundation at all, and the post-1695 piracy vacuum was real. A pure coordination reading would erase the capture — the assignment market reconstituted concentrated publisher control in statutory form and dispossessed the prior occupant. Both halves are structural: enforcement machinery holds the arrangement together, and identifiable parties sit on each side of the transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel statute_of_anne_ip_foundation — the institutional_reallocation_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Historiographical adjudication across the sibling files: statute_of_anne_ip_foundation__conceptual_emergence_reading and statute_of_anne_ip_foundation__entangled_event_reading are authored as separate constraints with their own epsilon values and beneficiary structures; comparing the three files against the evidentiary record (Stationers'' Register, parliamentary journals, the Donaldson litigation) resolves which structural description each body of evidence supports.',
    'The conceptual_emergence_reading would re-describe this constraint as a newly created limited regulatory concept with the learning public as beneficiary and lower extraction; the entangled_event_reading would refuse this reading''s separable institutional epsilon and require a joint concept-plus-institution epsilon over an inseparable event. The disagreement is located in whether the statute''s operative structure is occupant-change within a continuous institutional space (this reading), creation of a new conceptual category (conceptual reading), or an inseparable composite (entangled reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel reading this constraint instantiates, what siblings would change, and where the disagreement sits.').

omega_variable(
    assignment_capture_depth,
    'How completely did the assignment market capture the author-vested rights at enactment — was first vesting substantively meaningful for any authors, or uniformly a conduit to the bookseller trade?',
    'Stationers'' Register assignment records and author-contract scholarship: the share of new works assigned before or at publication, the lump-sum discount against the right''s realized value, and the size of the minority (Pope, Gay) who bargained directly and profitably.',
    'If capture was near-uniform, this reading''s structural delta holds exactly — the new holder class is the publishing trade and the author beneficiary declaration is nominal; if meaningful author bargaining was common, authors hold genuine beneficiary directionality and the override toward symmetry (d = 0.45) is too high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assignment_capture_depth, empirical, 'Depth of assignment-market capture of the author-vested right.').

omega_variable(
    stationers_baseline_naturalness,
    'Was the pre-1710 Stationers'' monopoly a state-constructed privilege or an emergent trade order that the register system stabilized?',
    'Historiography of the Licensing Acts and the Company''s regulatory functions: the balance between crown-granted monopoly (the 1557 charter, the 1662 act) and guild self-governance that predated and outlasted state backing.',
    'If the monopoly was state-constructed privilege, the reallocation reads as a transfer between privilege-holders and the victim''s position is weaker; if it was an emergent functional order, the statute expropriated a working institution and the victim declaration carries more weight, shifting the arrangement''s effective extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_baseline_naturalness, empirical, 'Constructed-privilege versus emergent-order character of the dispossessed baseline.').

omega_variable(
    term_limit_sincerity,
    'Was the fourteen-year term a genuine sunset commitment at enactment, or a bargaining position the drafters always expected to extend?',
    'Parliamentary debates and lobbying records from the 1735 renewal campaigns through the 1842 act: whether extension was contested as a departure from the founding bargain or pursued as its continuity.',
    'If the limit was insincere from the start, the arrangement is closer to a rebranded perpetual monopoly and the early-interval measurements understate its extraction; if sincere, the drift toward life-based terms is later capture and the early-interval measurements understate its initial coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_limit_sincerity, empirical, 'Sincerity of the statutory term limit at enactment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 132).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anne_institutional_reallocation_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t22, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 22, 0.3).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t44, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 44, 0.38).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 64, 0.44).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t88, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 88, 0.36).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t110, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 110, 0.42).
narrative_ontology:measurement(anne_institutional_reallocation_tr_t132, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 132, 0.48).

% Extraction over time
narrative_ontology:measurement(anne_institutional_reallocation_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(anne_institutional_reallocation_be_t22, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 22, 0.55).
narrative_ontology:measurement(anne_institutional_reallocation_be_t44, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 44, 0.6).
narrative_ontology:measurement(anne_institutional_reallocation_be_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 64, 0.66).
narrative_ontology:measurement(anne_institutional_reallocation_be_t88, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 88, 0.6).
narrative_ontology:measurement(anne_institutional_reallocation_be_t110, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 110, 0.64).
narrative_ontology:measurement(anne_institutional_reallocation_be_t132, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 132, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(anne_institutional_reallocation_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anne_institutional_reallocation_su_t22, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 22, 0.5).
narrative_ontology:measurement(anne_institutional_reallocation_su_t44, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 44, 0.58).
narrative_ontology:measurement(anne_institutional_reallocation_su_t64, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 64, 0.65).
narrative_ontology:measurement(anne_institutional_reallocation_su_t88, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 88, 0.52).
narrative_ontology:measurement(anne_institutional_reallocation_su_t110, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 110, 0.55).
narrative_ontology:measurement(anne_institutional_reallocation_su_t132, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 132, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, licensing_act_1662_printing_monopoly).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Statute of Anne created copyright' covers three structurally distinct claims and is decomposed into a three-story family: this file (institutional reallocation — who occupied the rights space before and after 1710), the conceptual_emergence_reading (what new concept the statute introduced), and the entangled_event_reading (the claim that the two dimensions cannot be separated). Each story carries its own epsilon, beneficiaries, and victims. The upstream licensing regime (licensing_act_1662_printing_monopoly) is the institutional baseline this reading reallocates from. The readings are linked via affects_constraints rather than forced into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, moderate, 0.45).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
