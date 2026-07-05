% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Statute of Anne (1710) as Entangled Conceptual-Institutional Event
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the entangled-event reading of the Statute of
 *   Anne kernel: the claim that the statute's conceptual innovation
 *   (copyright as an author-vested, term-limited right rather than a
 *   perpetual trade privilege) and its institutional reallocation (moving
 *   formal legal control from the Stationers' Company toward
 *   authors-as-nominal-holders, captured practically by publishers via
 *   assignment) occurred in a single, inseparable legislative act. Unlike the
 *   conceptual_emergence_reading (which treats the new idea of copyright as
 *   the primary event) or the institutional_reallocation_reading (which
 *   treats the redistribution of an existing right as primary), this reading
 *   refuses to assign either dimension causal or temporal priority — the same
 *   statutory text did both at once, and fifty-plus years of subsequent
 *   litigation (Millar v Taylor, Donaldson v Becket) attempting to separate
 *   them is read here as evidence of the entanglement rather than as a
 *   solvable puzzle. Extraction is measured as moderate-high because
 *   publishers captured most of the practical value through routine
 *   assignment contracts even though the statute's nominal beneficiary was
 *   the author; the rising theater_ratio over the interval tracks the growing
 *   gap between the statute's author-centered rhetoric and its
 *   publisher-dominated practice.
 *
 * KEY AGENTS:
 *   - authors_nominal: nominal statutory holder, practical non-operator
 *   - publishers_practical: practical capturer of the new right via assignment, also its institutional architect and defender
 *   - provincial_booksellers: payer bearing renewed exclusivity costs
 *   - public_domain_clarity: non-agent casualty of the unresolved conceptual/institutional entanglement
 *   - parliament_1710: drafting body that produced the entangled text
 *   - later_courts_and_historians: analytical seat attempting retrospective separation and failing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne (1710) as Entangled Conceptual-Institutional Event").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '5dace719-78f2-492c-9c08-798e1fee120e').
narrative_ontology:cs_kernel_codification('5dace719-78f2-492c-9c08-798e1fee120e', fixed_text).
narrative_ontology:cs_authority_grounding('5dace719-78f2-492c-9c08-798e1fee120e', lineage).
narrative_ontology:cs_interpretation_layer_present('5dace719-78f2-492c-9c08-798e1fee120e').
narrative_ontology:cs_reading_relation('5dace719-78f2-492c-9c08-798e1fee120e', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dace719-78f2-492c-9c08-798e1fee120e', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('5dace719-78f2-492c-9c08-798e1fee120e', foundational, conceptual_and_institutional_change_are_inseparable_in_this_act).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_are_inseparable_in_this_act, holdable).
narrative_ontology:cs_axiom_grounding('5dace719-78f2-492c-9c08-798e1fee120e', conceptual_and_institutional_change_are_inseparable_in_this_act, conventional).
narrative_ontology:cs_axiom('5dace719-78f2-492c-9c08-798e1fee120e', secondary, beneficiary_status_is_structurally_ambiguous_not_merely_unresolved).
narrative_ontology:cs_axiom_status(beneficiary_status_is_structurally_ambiguous_not_merely_unresolved, holdable).
narrative_ontology:cs_axiom_grounding('5dace719-78f2-492c-9c08-798e1fee120e', beneficiary_status_is_structurally_ambiguous_not_merely_unresolved, conventional).
narrative_ontology:cs_reference_frame('5dace719-78f2-492c-9c08-798e1fee120e', stationers_perpetual_trade_privilege).
narrative_ontology:cs_drift_state('5dace719-78f2-492c-9c08-798e1fee120e', donaldson_v_becket_1774, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5dace719-78f2-492c-9c08-798e1fee120e', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_clarity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, provincial_booksellers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Named in the statute's preamble as the rightful holders of a new statutory right in their works for a term of years. In practice most authors lacked the capital, distribution networks, or bargaining leverage to exploit this right themselves and assigned it to publishers near-immediately upon sale of a manuscript. The statute makes them the nominal origin point of the new entitlement without making them its practical operator.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal, beneficiary,
    moderate, biographical, constrained, national).

% Members and successors of the former Stationers' Company monopoly, now operating as assignees of author rights under the new statutory frame. They lobbied for the statute's passage, shaped its drafting, and captured the practical economic benefit of the new right through standard-form assignment contracts, while also becoming the entity that must litigate and lobby to defend the right's scope (Donaldson v Becket, term extensions).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, agenda_setter).

% Booksellers and printers outside the London trade who previously could reprint older works once informal Stationers' custom lapsed. The new statutory term (even though shorter than perpetual common-law claims asserted by London publishers) reintroduces formal legal exclusivity they must now clear or risk suit, raising their cost of stocking texts and constraining their trade relative to the pre-statute ambiguity some had begun exploiting.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, provincial_booksellers, payer,
    moderate, biographical, constrained, regional).

% Not an actor but the casualty of the entangled event: because conceptual and institutional change happened in the same act, no one can say cleanly whether a given work's status derives from a newly recognized abstract right or from a reallocated concrete privilege. This ambiguity persists into later doctrine (the perpetual-common-law-copyright argument litigated for over fifty years) and denies later readers, courts, and historians a clean baseline for what counts as 'in the commons.'
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_clarity, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_clarity).

% Drafted and passed the statute in response to lobbying from the book trade and public concern about the Stationers' perpetual monopoly. Framed the act using the vocabulary of both a new right ('the author... shall have the sole right') and continuation of registration/deposit machinery inherited from the Stationers' regime, without legislative clarity on which framing was primary — producing the entanglement this reading identifies.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament_1710, agenda_setter,
    institutional, generational, analytical, national).

% Judges in Millar v Taylor and Donaldson v Becket, and subsequent legal historians, attempted retrospectively to separate the statute's conceptual innovation from its institutional reallocation in order to answer whether common-law copyright survived the statute. Their difficulty in doing so cleanly is itself evidence for this reading's claim that the two dimensions were never separable at the moment of enactment.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, later_courts_and_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, dateable legal event that simultaneously supplies book-trade actors with a workable, time-limited exclusivity mechanism (solving the trade's need for predictable enforcement after the Licensing Act lapsed) and supplies the legal system with a named holder-category (the author) around which future doctrine could organize — one act discharging two coordination problems at once.
% TRANSFER_FUNCTION: Moves formal legal entitlement from an unbounded, informally-policed trade custom (Stationers' perpetual internal copy-right) to a bounded statutory term nominally vested in authors but functionally captured by publishers via assignment; simultaneously moves conceptual ground from 'copy is a trade privilege' to 'copy is authored,' a shift that is not separable from the transfer of who holds the entitlement because both changes are encoded in the same fourteen words of statutory text.
% ABSENT_VOICES: Readers, later scholars, and the public who bear the cost of the resulting doctrinal ambiguity are not party to the 1710 negotiation at all; provincial and colonial printers whose trade the London-centered statute constrains had negligible voice in Parliament relative to the London book trade's lobbying apparatus.
% DISAPPEARANCE_RATIONALE: If the entangled reading were somehow undone — if it turned out the conceptual and institutional dimensions actually were separable events — legal historiography would need to identify which came first and assign priority accordingly; under the entangled reading itself, no such separation is available, so 'disappearance' of this reading would not change the historical record but would change which downstream doctrinal arguments are even askable. Whether the world of copyright doctrine 'rearranges' therefore depends on which sibling reading one adopts, which is why this question is contested rather than settled by this reading alone.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual, internally-enforced printing monopoly had lost its formal legal backing after the Licensing Act expired in 1695, leaving the book trade without a mechanism to prevent unauthorized reprinting and Parliament without a settled rationale for granting one.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g. Ronan Deazley, Mark Rose) writing well outside the interests of either the modern publishing industry or authors' guilds attest that the specific trade-collapse problem of 1710 was resolved within a generation, while the statute's entangled conceptual/institutional language continued to generate litigation (Donaldson v Becket, 1774) for reasons unrelated to the original trade-collapse problem — corroboration that the founding problem itself is dead even though the instrument built to solve it persists in a different form.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, contested).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that a genuine coordination function existed (replacing an expired, unenforceable trade monopoly with a workable legal mechanism) but was substantially captured by publishers rather than flowing to the nominal beneficiaries (authors). Suppression (0.42) is moderate: enforcement required litigation and registration machinery but did not involve strong coercive apparatus comparable to censorship-era licensing. Theater_ratio rises across the interval (0.20 to 0.40) as the statute's author-centered justificatory language increasingly diverges from a practice dominated by standard-form assignment to publishers — a widening performance gap rather than a change in the underlying legal mechanism. Accessibility_collapse (0.60) reflects that alternative arrangements (perpetual trade custom, unregulated reprinting) became substantially foreclosed once the statutory term took hold, though not totally, since provincial reprinting persisted at the margins and common-law copyright claims were litigated for decades. Resistance (0.55) reflects sustained legal contestation (five decades of litigation over whether common-law copyright survived the statute) rather than passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status is deliberately declared as ambiguous per the reading's structural delta: authors are nominal beneficiaries (named right-holders) while publishers are practical beneficiaries (actual economic capturers via assignment) — both are listed under base_properties.beneficiaries because the entangled reading treats this ambiguity itself as structural, not as a modeling gap to be resolved. Provincial booksellers are victims because the reintroduced formal exclusivity constrains a trade practice they had begun to exploit under the post-1695 ambiguity. Public_domain_clarity is listed as a non-agent victim (agent: false) because it is a casualty-of-structure (a doctrinal good) rather than an actor with interests, consistent with the rule that non-actors marked agent:false are excluded from directionality computation while remaining narratively present.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the Stationers' loss of legal backing for their trade monopoly after 1695) is dead — that specific trade-collapse crisis was resolved within a generation. Yet the entangled instrument built to solve it persists, its terms extended and its conceptual vocabulary redeployed for purposes (perpetual common-law copyright arguments, later copyright term extensions) unconnected to the original crisis. Classifying this as tangled_rope rather than snare or piton is deliberate: the coordination function (a workable, bounded alternative to an unenforceable perpetual monopoly) was real at the founding moment, and the extraction (practical capture by publishers of a nominally author-vested right) rides on that same structure rather than replacing it — exactly the tangled_rope signature of coordination and extraction sharing one mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_dimensions,
    'Can the conceptual innovation (copyright as author-vested, limited right) and the institutional reallocation (transfer of practical control from Stationers to authors/publishers) be given independent causal or temporal priority, or are they genuinely a single inseparable event?',
    'Close textual-historical analysis of the statute''s drafting history (committee records, competing draft bills) to determine whether the conceptual framing was settled before or after the institutional allocation question, or whether the drafters themselves treated them as one question throughout.',
    'If separable with clear priority, this reading collapses into whichever of the sibling readings (conceptual_emergence_reading or institutional_reallocation_reading) captures the temporally prior dimension, and this story should be retired or reclassified as a restatement of that sibling. If genuinely inseparable, this reading''s ambiguous-beneficiary structure and tangled_rope classification remain the most accurate available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_dimensions, conceptual, 'Whether the two dimensions of the 1710 statute are analytically separable or genuinely entangled — the central committer question this reading answers in the negative.').

omega_variable(
    beneficiary_ambiguity_resolution,
    'Is the author/publisher beneficiary ambiguity a genuine structural feature of the statute (both benefit, in different registers, by design or by accident of drafting) or an artifact of incomplete historical data about who actually captured economic value in the first decades after 1710?',
    'Archival analysis of assignment contracts and Stationers'' Register entries from 1710-1740 to determine what share of statutory rights were retained by authors versus assigned to publishers, and on what terms.',
    'If archival data show near-universal, near-immediate assignment to publishers, the ''ambiguous beneficiary'' framing understates publisher capture and the story''s extractiveness score may be conservative. If a meaningful minority of authors retained and exploited rights directly, the ambiguity is more genuinely structural than a publisher-capture story would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_resolution, empirical, 'Whether the nominal/practical beneficiary split is a real structural ambiguity or reflects incomplete data on early assignment practice.').

omega_variable(
    kernel_framing_choice,
    'Given that the entangled_event_reading, conceptual_emergence_reading, and institutional_reallocation_reading each produce different classifications and beneficiary structures from the same statutory text, what evidence should guide a reader toward preferring one framing over the others?',
    'None fully dispositive; the choice tracks disciplinary priors (legal doctrinal historians tend toward institutional_reallocation_reading; intellectual historians of ideas tend toward conceptual_emergence_reading; historians of legal drafting practice tend toward entangled_event_reading). Cross-disciplinary synthesis work (e.g. comparing how each reading handles the Donaldson v Becket outcome) could narrow but not eliminate the framing choice.',
    'Selecting entangled_event_reading yields an ambiguous, dual beneficiary structure and a tangled_rope classification; selecting either sibling yields a cleaner beneficiary/victim structure and potentially a different classification (e.g. institutional_reallocation_reading may classify closer to snare if the reallocation is read as pure extraction from the Stationers; conceptual_emergence_reading may classify closer to rope if the new conceptual tool is read as pure coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Framing under-determination across the three sibling readings of the kernel, and what would justify choosing among them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.2).
narrative_ontology:measurement(stat_tr_t1721, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1721, 0.25).
narrative_ontology:measurement(stat_tr_t1732, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1732, 0.28).
narrative_ontology:measurement(stat_tr_t1743, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1743, 0.32).
narrative_ontology:measurement(stat_tr_t1754, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1754, 0.35).
narrative_ontology:measurement(stat_tr_t1765, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1765, 0.38).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.4).
narrative_ontology:measurement(stat_be_t1721, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1721, 0.46).
narrative_ontology:measurement(stat_be_t1732, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1732, 0.5).
narrative_ontology:measurement(stat_be_t1743, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1743, 0.53).
narrative_ontology:measurement(stat_be_t1754, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1754, 0.55).
narrative_ontology:measurement(stat_be_t1765, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1765, 0.57).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.3).
narrative_ontology:measurement(stat_su_t1721, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1721, 0.33).
narrative_ontology:measurement(stat_su_t1732, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1732, 0.35).
narrative_ontology:measurement(stat_su_t1743, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1743, 0.37).
narrative_ontology:measurement(stat_su_t1754, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1754, 0.39).
narrative_ontology:measurement(stat_su_t1765, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1765, 0.41).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1774, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the statute_of_anne_ip_foundation kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: conceptual_emergence_reading treats the statute as primarily introducing a new regulatory concept (copyright as limited tool, not perpetual property); institutional_reallocation_reading treats it as primarily redistributing an existing privilege from the Stationers' Company to authors; this entangled_event_reading treats the two dimensions as inseparable in a single legislative act, producing a distinctly ambiguous beneficiary structure (authors nominal, publishers practical) and identifying loss of conceptual clarity as a diffuse victim not named by either sibling reading. All three are linked via affects_constraints to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
