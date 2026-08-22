% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: First Statutory Author Holding (Statute of Anne 1710) — Shift from Stationers' Monopoly to Author-as-Rights-Holder
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The Statute of Anne (1710) shifted the legitimate claimant set for IP
 *   protection from the Stationers' Company (a royal-charter monopoly of
 *   printers/booksellers) to authors as statutory rights-holders. This
 *   reading emphasizes the membership change in the occupied set: who holds
 *   the enforcement claim. The constraint is a tangled rope because it
 *   simultaneously coordinates (solves the problem of incentivizing 'learned
 *   men to compose and write useful books' via a limited monopoly) and
 *   extracts (the monopoly rent is captured by assignees — initially
 *   Stationers via assignment contracts, later publishers — while
 *   unenfranchised creators and colonial subjects are excluded from the
 *   claimant set). The Stationers' monopoly was a snare; the statutory
 *   author-holding is a tangled rope that inherits extraction through
 *   assignment markets.
 *
 * KEY AGENTS:
 *   - statutory_authors: Primary beneficiary (moderate/constrained) — granted limited monopoly but often assign it
 *   - stationers_company_monopolists: Primary victim (powerful/trapped) — lost royal charter monopoly but captured statutory rights via assignment
 *   - parliamentary_legislature: Agenda setter (institutional/generational) — enacted the statute as public bargain
 *   - emerging_publishers: Secondary beneficiary (organized/mobile) — new entrants leveraging statutory framework
 *   - unenfranchised_creators: Victim (powerless/trapped) — domestic creators without access to registration/assignment
 *   - colonial_subject_creators: Victim (powerless/trapped) — imperial subjects excluded from statutory protection
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across 1710-1774
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.68).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.72).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "First Statutory Author Holding (Statute of Anne 1710) — Shift from Stationers' Monopoly to Author-as-Rights-Holder").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '6f40cee6-f7de-448d-b89c-a2dbb5d7950f').
narrative_ontology:cs_kernel_codification('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', formalized).
narrative_ontology:cs_authority_grounding('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', lineage).
narrative_ontology:cs_interpretation_layer_present('6f40cee6-f7de-448d-b89c-a2dbb5d7950f').
narrative_ontology:cs_reading_relation('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', foundational, author_as_legitimate_statutory_claimant).
narrative_ontology:cs_axiom_status(author_as_legitimate_statutory_claimant, holdable).
narrative_ontology:cs_axiom_grounding('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', author_as_legitimate_statutory_claimant, conventional).
narrative_ontology:cs_axiom('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', foundational, limited_monopoly_as_public_bargain).
narrative_ontology:cs_axiom_status(limited_monopoly_as_public_bargain, holdable).
narrative_ontology:cs_axiom_grounding('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', limited_monopoly_as_public_bargain, instrumental).
narrative_ontology:cs_reference_frame('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', stationers_monopoly_regime).
narrative_ontology:cs_drift_state('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', donaldson_v_beckett_1774, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6f40cee6-f7de-448d-b89c-a2dbb5d7950f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, parliamentary_legislature).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, emerging_publishers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company_monopolists).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unenfranchised_creators).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, colonial_subject_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, stationers_company_monopolists).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, author_natural_right_in_labor).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, limited_monopoly_as_public_bargain).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, learning_encouragement_via_statutory_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Granted a 14+14 year statutory monopoly in their works by the 1710 Act. In practice, most authors must assign this right to publishers/booksellers to reach markets — the assignment is the price of publication. Their exit from the assignment relationship is constrained by the necessity of distribution networks controlled by the same intermediaries who held the pre-1710 monopoly.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% Lost their royal charter monopoly (1709 expiry) and the perpetual common-law copyright claim (rejected in Donaldson v Beckett 1774). However, they captured the new statutory regime by leveraging their control of the Stationers' Register, distribution networks, and capital to become the primary assignees of authors' statutory rights. They bear the cost of the formal monopoly loss but extract via contractual capture of the new right.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company_monopolists, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, stationers_company_monopolists, beneficiary).

% Enacted the Statute of Anne (1710) as a public bargain: limited monopoly to authors/purchasers in exchange for 'encouragement of learning.' Parliament sets the term, scope, and enforcement machinery. It extracts legitimacy and regulatory authority, not direct rent. Its exit is analytical — it can amend the statute but does so within the framework it created.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliamentary_legislature, agenda_setter,
    institutional, generational, analytical, national).

% New entrants (e.g., London booksellers outside the Stationers' Company, Scottish and Irish reprint publishers) who leverage the statutory framework to compete with the Stationers. They benefit from the shift from perpetual common-law monopoly (Stationers' claim) to limited statutory term — the term limit creates a public domain they can exploit. Their exit is mobile: they can enter/exit the trade based on statutory terms.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, emerging_publishers, beneficiary,
    organized, biographical, mobile, national).

% Domestic creators (artisans, illustrators, translators, abridgers, oral performers) whose works fall outside the statutory categories or who lack access to registration/assignment infrastructure. They bear the suppression of the constraint (cannot freely copy/build upon protected works) without accessing its benefit (no enforceable claim). Their exit is trapped — the constraint's categories exclude them by design.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unenfranchised_creators, payer,
    powerless, biographical, trapped, national).

% Creators in British colonies (India, Caribbean, North America pre-1783) subject to British copyright law via imperial extension but excluded from the claimant set — colonial works are not protected in Britain, British works are enforced in colonies. Their identity as imperial subjects locks them into the extraction relation: they pay for British books via monopoly pricing but cannot claim protection for their own works. Exit requires imperial dissolution.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, colonial_subject_creators, payer,
    powerless, biographical, identity_locked, global).

% Sees the full structural trajectory from Stationers' monopoly (snare) to statutory author-holding (tangled rope) to modern copyright (snare/tangled_rope hybrid). Occupies no material position in the constraint; evaluates the occupancy shift, the coordination/extraction fusion, and the kernel contest across readings.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, stationers_company_monopolists).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of incentivizing creation and dissemination of 'useful books' by granting a limited, tradeable monopoly — replacing the Stationers' perpetual monopoly (which suppressed competition and dissent) with a time-limited statutory right that creates a public domain at term end.
% TRANSFER_FUNCTION: Moves monopoly rent from readers/purchasers (via monopoly pricing) to the statutory rights-holder (author or assignee). In practice, the rent flows: readers → publishers/booksellers (via price) → authors (via royalty/assignment fee, typically small fraction). The Stationers capture the rent via assignment contracts; emerging publishers capture via reprint rights after term expiry.
% ABSENT_VOICES: Unenfranchised domestic creators (artisans, oral performers, illustrators) and colonial subject creators — both excluded from the legitimate claimant set by statutory category definitions and imperial administration. They would object to the monopoly pricing they pay and the exclusion they suffer, but they are not in the legislative conversation (1710) or the judicial conversation (1769-1774).
% DISAPPEARANCE_RATIONALE: If the statutory author-holding constraint vanished overnight (reverting to Stationers' perpetual common-law claim or open reprinting), the book trade would reorganize: Stationers would reassert perpetual monopoly via common law; authors would lose even nominal rights; colonial extraction would intensify; the public domain would collapse. The 1774 Donaldson v Beckett rejection of common-law perpetual copyright shows the world *did* rearrange when the constraint's boundary was litigated.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual monopoly (via royal charter and common-law claim) suppressed competition, censored dissent, and blocked 'learned men' from controlling their works. The founding problem was: how to encourage learning without granting a perpetual monopoly to a chartered company?
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary preamble (1710) attests the problem is live (encouragement of learning). Stationers' petitions (1709-1739) attest the problem is dead (their monopoly was the solution). Scottish Enlightenment publishers (e.g., Foulis brothers, 1740s) and colonial petitioners (e.g., 1773 Massachusetts copyright petition) attest the problem is contested — the statutory solution created new exclusions. Independent corroboration: economic historians (Ronan Deazley, Isabella Alexander) show the 'learning encouragement' justification was strategically deployed against Stationers' opposition, not a neutral policy goal.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.68: the 14+14 year monopoly term (renewable once) grants substantial rent; suppression 0.72: enforcement requires active litigation, registration, and seizure of 'pirated' sheets — the constraint collapses alternatives (common-law perpetual copyright, open reprinting) through state coercion; theater_ratio 0.41: the 'encouragement of learning' preamble performs public benefit while assignment markets channel rent to intermediaries; accessibility_collapse 0.58: common-law perpetual copyright (argued by Stationers in Millar v Taylor) was a live alternative until Donaldson v Beckett (1774) rejected it; resistance 0.61: Stationers resisted via litigation, petitioning, and contractual capture; unenfranchised creators resisted via piracy and petitioning. The 50-year measurement grid (1710-1760) captures the transition from Stationers' capture to publisher-dominated assignment.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary seat: genuine coordination (public bargain for learning). From the author seat: coordination promise, extraction reality (assignment markets). From the Stationers seat: extraction (loss of monopoly) partially offset by capture (assignment contracts). From unenfranchised/colonial seats: pure extraction (exclusion from claimant set). The engine computes per-seat χ from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   statutory_authors: d ~0.3 (beneficiary — granted right but constrained by assignment necessity); stationers_company_monopolists: d ~0.8 (victim of formal monopoly loss but partial beneficiary via assignment capture — net target of the statutory shift); parliamentary_legislature: d ~0.1 (agenda setter, extracts legitimacy not rent); emerging_publishers: d ~0.25 (beneficiary — new market access); unenfranchised_creators: d ~0.9 (victim — excluded from claimant set, no exit); colonial_subject_creators: d ~0.95 (victim — structural exclusion, identity-locked via imperial subjecthood). The engine derives d from these declarations plus exit_options and power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (encouraging learned composition via limited monopoly) is contested — the monopoly term expanded from 14+14 to life+70, the claimant set expanded to corporate entities, and the 'learning' justification now covers entertainment software. The constraint persists because the agenda_setter (legislature) benefits from the regulatory capture equilibrium; fixing is prohibitive for any single actor. This is not a piton — the function (rent allocation) is active, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_statutory_origin_ambiguity,
    'Does the 1710 Statute of Anne recognize a pre-existing natural right of authors in their works, or does it create a new statutory right ex nihilo?',
    'Historical analysis of parliamentary debates (1709-1710), Stationers'' petitions, and contemporary juridical commentary (e.g., Millar v Taylor 1769 vs Donaldson v Beckett 1774). If natural-right language is performative (strategic rhetoric to overcome Stationers'' opposition) rather than doctrinal, the constraint is statutory construction; if doctrinal, it reflects a mountain-like natural law claim.',
    'If natural right is recognized, the constraint has mountain-like immunity from revision (extraction is the price of acknowledging a pre-existing claim). If statutory creation, the constraint is a tangled rope from inception — coordination (learning encouragement) fused with extraction (monopoly grant to authors/publishers). This delta changes ε by ~0.15-0.20 and flips the false_summit_mountain gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_statutory_origin_ambiguity, conceptual, 'Whether author''s right is discovered (natural) or enacted (statutory) — the core framing ambiguity of the first_holding_reading.').

omega_variable(
    stationers_monopoly_continuity,
    'To what extent did the Stationers'' Company retain de facto control over the book trade after 1710 through contractual assignment of authors'' new statutory rights?',
    'Economic history of the 1710-1774 period: assignment rates, contract terms, Stationers'' Register continuity, and the 1737-1739 ''Great Copyright'' litigation. Measure the fraction of statutory copyrights effectively controlled by Stationers via assignment within 5 years of first publication.',
    'High continuity (>70% assignment to Stationers) means the beneficiary shift is nominal — the constraint remains a snare for authors (extraction by Stationers via assignment). Low continuity means the statutory author-holding is genuine coordination. This omega directly modulates the beneficiary/victim structure and effective extraction for author_seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stationers_monopoly_continuity, empirical, 'Whether the Stationers'' monopoly persisted through contractual capture of the new statutory right.').

omega_variable(
    kernel_reading_boundary,
    'Is the first_holding_reading structurally foreclosed by the thinkability_reading, or do they occupy independent analytical planes?',
    'Formal analysis of the kernel''s occupational set: does ''author entered legitimate claimant set'' (first_holding) logically require ''ownable expression became legally coherent'' (thinkability), or can the membership shift occur without the category emergence? Test via counterfactual: if ownable expression was already coherent pre-1710, does the 1710 shift still constitute a constraint?',
    'If foreclosed, the first_holding_reading is a subset of thinkability and should not be a separate constraint story. If coexists_with, both are valid kernel readings with different ε referents. If influences, thinkability creates the semantic space that first_holding populates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this reading and the thinkability_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_first_holding_tr_t0, ip_category_emergence__first_holding_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ip_first_holding_tr_t5, ip_category_emergence__first_holding_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ip_first_holding_tr_t10, ip_category_emergence__first_holding_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ip_first_holding_tr_t20, ip_category_emergence__first_holding_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(ip_first_holding_tr_t35, ip_category_emergence__first_holding_reading, theater_ratio, 35, 0.38).
narrative_ontology:measurement(ip_first_holding_tr_t50, ip_category_emergence__first_holding_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(ip_first_holding_be_t0, ip_category_emergence__first_holding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ip_first_holding_be_t5, ip_category_emergence__first_holding_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ip_first_holding_be_t10, ip_category_emergence__first_holding_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ip_first_holding_be_t20, ip_category_emergence__first_holding_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ip_first_holding_be_t35, ip_category_emergence__first_holding_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(ip_first_holding_be_t50, ip_category_emergence__first_holding_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ip_first_holding_su_t0, ip_category_emergence__first_holding_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ip_first_holding_su_t5, ip_category_emergence__first_holding_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ip_first_holding_su_t10, ip_category_emergence__first_holding_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(ip_first_holding_su_t20, ip_category_emergence__first_holding_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(ip_first_holding_su_t35, ip_category_emergence__first_holding_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement(ip_first_holding_su_t50, ip_category_emergence__first_holding_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__first_holding_reading, 0.15).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, statutory_copyright_term_extension).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, assignment_market_capture).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, colonial_copyright_imposition).

% DUAL FORMULATION NOTE:
% Kernel ip_category_emergence decomposes into three readings with distinct ε referents: thinkability (category emergence, ε≈0.35, mountain-like), first_holding (occupancy change, ε≈0.68, tangled rope), synchronic_diachronic_seam (framing test, ε≈0.15, rope). This reading (first_holding) is downstream of thinkability (thinkability creates the coherent category that first_holding populates) and influences statutory_copyright_term_extension (the 1710 membership shift enables later term expansions by establishing author-as-rights-holder as the legitimate claimant).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, moderate, 0.3).
constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, powerful, 0.8).
constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, institutional, 0.1).
constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, organized, 0.25).
constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
