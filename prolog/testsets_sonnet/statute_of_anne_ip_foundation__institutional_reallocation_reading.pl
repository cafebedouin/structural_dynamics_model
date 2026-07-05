% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne (1710) — Institutional Reallocation of Copy-Right from Stationers to Authors
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the institutional-reallocation reading of the
 *   Statute of Anne kernel: the statute is read as a transfer of an existing
 *   institutional position (who is 'first holder' of the right to print) from
 *   the Stationers' Company's guild-based perpetual register to individual
 *   authors under a statutory term. This is NOT a claim about a new
 *   conceptual category of property (that is the sibling
 *   conceptual_emergence_reading) nor a claim that the conceptual and
 *   institutional dimensions are inseparable (the sibling
 *   entangled_event_reading). Here the occupied legal SLOT is treated as the
 *   stable unit of analysis, and the statute's operation is: same slot, new
 *   occupant. The coordination function (settling who may lawfully print) is
 *   real; the extraction is that assignment machinery lets publishers
 *   re-capture much of what was nominally reallocated to authors, while the
 *   former guild monopolists lose their perpetual claim outright and litigate
 *   for six decades to try to recover it.
 *
 * KEY AGENTS:
 *   - stationers_company_perpetual_copy_holders: primary victim (organized/trapped) — loses the perpetual institutional monopoly the statute displaces
 *   - commercial_publishers_via_assignment: primary beneficiary (organized/arbitrage) — re-occupies most of the practical position via assignment contracts
 *   - prominent_established_authors: nominal beneficiary (moderate/constrained) — gains formal occupancy of the first-right slot but often assigns it away immediately
 *   - unassigned_and_minor_authors: secondary victim (powerless/trapped) — receives the formal right without the leverage to exploit it
 *   - parliament: agenda_setter (institutional/analytical) — the actor whose statutory choice performs the reallocation
 *   - legal_historians_and_courts: analytical observer — adjudicates and later analyzes whether the reallocation was genuine or partial
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) — Institutional Reallocation of Copy-Right from Stationers to Authors").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'dc746a82-5c72-4191-9f22-e56d6d52a496').
narrative_ontology:cs_kernel_codification('dc746a82-5c72-4191-9f22-e56d6d52a496', fixed_text).
narrative_ontology:cs_authority_grounding('dc746a82-5c72-4191-9f22-e56d6d52a496', lineage).
narrative_ontology:cs_interpretation_layer_present('dc746a82-5c72-4191-9f22-e56d6d52a496').
narrative_ontology:cs_reading_relation('dc746a82-5c72-4191-9f22-e56d6d52a496', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc746a82-5c72-4191-9f22-e56d6d52a496', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('dc746a82-5c72-4191-9f22-e56d6d52a496', foundational, first_right_occupancy_is_a_transferable_institutional_slot).
narrative_ontology:cs_axiom_status(first_right_occupancy_is_a_transferable_institutional_slot, holdable).
narrative_ontology:cs_axiom_grounding('dc746a82-5c72-4191-9f22-e56d6d52a496', first_right_occupancy_is_a_transferable_institutional_slot, conventional).
narrative_ontology:cs_axiom('dc746a82-5c72-4191-9f22-e56d6d52a496', secondary, stationers_perpetual_claim_was_a_genuine_preexisting_entitlement).
narrative_ontology:cs_axiom_status(stationers_perpetual_claim_was_a_genuine_preexisting_entitlement, overridden).
narrative_ontology:cs_axiom_grounding('dc746a82-5c72-4191-9f22-e56d6d52a496', stationers_perpetual_claim_was_a_genuine_preexisting_entitlement, empirically_contingent).
narrative_ontology:cs_reference_frame('dc746a82-5c72-4191-9f22-e56d6d52a496', guild_perpetual_registry_occupancy).
narrative_ontology:cs_drift_state('dc746a82-5c72-4191-9f22-e56d6d52a496', post_donaldson_v_beckett, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('dc746a82-5c72-4191-9f22-e56d6d52a496', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, prominent_established_authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_perpetual_copy_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, unassigned_and_minor_authors).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_as_first_rights_holder_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held perpetual, self-perpetuating copy-rights under the Stationers' Company register for over a century, controlling entry to the print trade through guild membership. The statute strips the perpetual claim, substituting a fixed term (14 years, renewable once) vested first in authors. Their institutional monopoly position — not any individual book — is what is extracted from; they lobbied for the statute's passage hoping it would re-entrench their control, then litigated for decades (culminating in Donaldson v Beckett, 1774) to recover a perpetual common-law copyright the statute had foreclosed.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_perpetual_copy_holders, payer,
    organized, generational, trapped, national).

% Former Stationers who reorganize as ordinary commercial actors: they cannot hold the initial statutory right (it vests in the author) but they contract to acquire it via assignment, and their capital, distribution networks, and legal sophistication mean assignment functions as a near-total practical transfer. They administer and lobby to shape enforcement of the new registration and term regime, effectively re-occupying much of the institutional space they nominally lost.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment, agenda_setter).

% Gain formal, named first title to their own work for the first time in statute — a real reallocation of legal personhood in the institutional register. In practice, most negotiate from weak bargaining positions and assign the right immediately to a publisher for a lump sum, capturing only a fraction of the value the new occupied-position nominally confers on them.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, prominent_established_authors, beneficiary,
    moderate, biographical, constrained, national).

% Authors without market leverage or literacy in the new legal instrument often fail to register or negotiate favorable assignment terms, effectively receiving the formal right without the practical means to exploit it. They bear the cost of a reallocation that names them but does not equip them, while publishers capture the operational benefit.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, unassigned_and_minor_authors, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, unassigned_and_minor_authors, excluded).

% Enacts the statute, choosing to vest the first right in authors rather than renewing the Stationers' perpetual register, and sets the term-limited structure and registration/enforcement machinery (Stationers' Hall registration retained, penalties for unauthorized printing). Institutionally, Parliament is the actor whose decision determines who occupies the legal position — the reallocation is its structural act.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Litigate and later analyze whether the statute displaced a preexisting common-law perpetual copyright or created rights de novo — the institutional-reallocation question is precisely what Donaldson v Beckett (1774) adjudicates, ruling that the statutory term extinguished any surviving common-law perpetual right and settling (for this jurisdiction) which class occupies the position going forward.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_historians_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends destructive uncertainty over who may print a given text by installing one clear, registrable, term-limited holder of first right, replacing an opaque guild-internal perpetual claim with a public, time-bounded, transferable entitlement — solving a real coordination problem in print-trade allocation.
% TRANSFER_FUNCTION: Moves the formal occupancy of the 'first holder' legal position from the Stationers' Company (guild-based, perpetual) to individual authors (statute-based, term-limited); in practice, subsequent assignment machinery moves most of the operative economic value from authors back to commercial publishers, while unassigned authors and the former guild monopolists both bear net losses relative to their prior positions.
% ABSENT_VOICES: The reading public and downstream users of the work (who would benefit from works entering the public domain sooner) are not party to the reallocation negotiation at all; the statute's term limits nominally serve them but they have no seat in the Stationers-versus-authors-versus-Parliament contest the statute actually resolves.
% DISAPPEARANCE_RATIONALE: If this specific reallocation act were undone, the Stationers' Company's perpetual registry claim would presumptively survive (as it argued in Donaldson v Beckett), the print trade's occupied legal position would revert to guild-based perpetual holding, and the entire subsequent architecture of authorial-first-right copyright — the basis for the assignment contracts publishers rely on today — would lose its founding legal instrument.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual, guild-internal copy-right register was becoming unenforceable and increasingly contested as printing spread beyond London and guild control weakened; Parliament needed a public, statutory mechanism to settle who could lawfully print what, for how long, replacing an informal monopoly arrangement whose legitimacy was eroding.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside both the publishing trade and the authors' guild tradition (e.g., analyses cited in Donaldson v Beckett itself, and later scholarship such as Patterson's 'Copyright in Historical Perspective') attest that the specific 1710 guild-succession problem was resolved by the 1774 ruling confirming the statute displaced any perpetual claim; the institutional-reallocation function the statute performed is now complete and dead as a live problem, even though the term-limited-rights architecture it installed persists for entirely different (subsequently added) reasons.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises modestly but steadily over the interval (0.35 to 0.58) as assignment practice matures and publishers develop standard contracts that route most of the statutory author-right back into commercial hands — this is rent re-concentration riding on a real coordination function, hence tangled_rope rather than pure rope or pure snare. Suppression is moderate and slowly falling (0.50 to 0.42) as the Stationers' litigation option (culminating in Donaldson v Beckett) provides a genuine, if ultimately unsuccessful, resistance channel — the constraint is actively contested, not merely imposed. Theater ratio is low and slowly rising (0.15 to 0.28): registration formalities at Stationers' Hall persist as compliance ritual even as the substantive guild monopoly they once serviced has been extinguished.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat, this is a clean institutional correction: a decaying guild monopoly replaced by a clear, publicly registrable individual right. From the Stationers' seat, this is expropriation of a property interest they had held and traded for a century — hence six decades of litigation attempting to establish a surviving common-law perpetual right. From the unassigned author's seat, the formal reallocation is real but practically hollow: they are named as first holder of a position they lack the capital or knowledge to exploit, so the practical occupant remains a commercial publisher, just a different one than before.
 *
 * DIRECTIONALITY LOGIC:
 *   Stationers' Company monopolists are declared victims: they held a concentrated, tradeable institutional asset (perpetual registry right) that the statute directly extinguishes, and their exit options are trapped (the guild structure itself is what is being dismantled). Commercial publishers are declared beneficiaries via assignment: though they cannot be the initial statutory holder, their capital and legal position let them functionally re-occupy the position through contract, giving them arbitrage-grade exit and low effective directionality. Prominent authors sit closer to beneficiary but with constrained exit, since formal right does not guarantee bargaining power. Unassigned and minor authors are payers despite nominally being the class the statute elevates — the formal reallocation without practical means to exploit it produces a directionality closer to the target end than the naive reading of 'authors benefit' would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling who lawfully occupies the first-right position after the Stationers' registry became unenforceable) is dead as of Donaldson v Beckett (1774): the reallocation this specific statute performed is complete and judicially confirmed. The tangled_rope classification prevents mislabeling the statute's persistent legal architecture as pure extraction (it did solve the coordination problem of who holds first right) or as pure benign coordination (the assignment machinery that grew up around it means the reallocated position drifted back toward concentrated commercial capture within decades) — both readings are true simultaneously, at different points in the assignment chain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preexisting_common_law_right_ambiguity,
    'Did the Stationers'' Company (or authors, via the Stationers as intermediary) hold a genuine preexisting property right that the statute reallocated, or did the statute instead create the first legally cognizable right of this kind, meaning there was no prior ''slot'' to reallocate?',
    'This is precisely the question Donaldson v Beckett (1774) attempted to resolve, ruling 4-4 (with the deciding vote against) that no perpetual common-law copyright survived the statute — but the closeness of the vote and continuing historical debate (e.g., Patterson vs. later revisionist historians) means the underlying question is not fully closed even by that ruling.',
    'If no preexisting right existed, the institutional_reallocation_reading''s victim declaration (Stationers'' Company as victim of expropriation) weakens substantially, and the conceptual_emergence_reading becomes the more structurally accurate account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preexisting_common_law_right_ambiguity, conceptual, 'Whether a genuine preexisting entitlement was reallocated or a new entitlement was created de novo — the central kernel-level ambiguity this reading takes a side on.').

omega_variable(
    assignment_capture_magnitude,
    'What fraction of the economic value of the newly reallocated author right was actually captured by publishers via assignment, versus retained by authors, across the 1710-1774 period?',
    'Archival analysis of surviving publisher contracts and payment records (where extant) comparing lump-sum assignment payments to subsequent publisher revenues from the same works.',
    'A high capture fraction strongly supports the tangled_rope classification (coordination function real, but extraction re-concentrates); a low capture fraction would push the classification closer to a genuine rope with authors as durable beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(assignment_capture_magnitude, empirical, 'Empirical magnitude of publisher re-capture via assignment, underlying the extractiveness trend.').

omega_variable(
    sibling_reading_selection_basis,
    'Is the choice to treat this kernel primarily through the institutional-reallocation lens (rather than conceptual-emergence or entangled-event) itself contestable, given that all three readings are defensible from different evidentiary bases?',
    'Cross-reading comparison: examine whether legal doctrine (Donaldson v Beckett reasoning), economic historiography (guild-decline literature), and conceptual history (emergence of ''the author'' as legal category, per Foucault/Rose) converge or diverge on which framing best fits the primary sources.',
    'If conceptual-emergence evidence dominates, the victim declaration here (Stationers'' Company) would need revision toward a framing with no directly analogous prior right; if entangled-event evidence dominates, no single ε value should be treated as primary at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_basis, conceptual, 'Whether the reading selected for this story is the most defensible of the three sibling framings, and what would change if it were not.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1720, 0.18).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.2).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1740, 0.22).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1750, 0.24).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1760, 0.26).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1774, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.35).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1720, 0.42).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.48).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1740, 0.52).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1760, 0.56).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.5).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1720, 0.5).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.47).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1740, 0.45).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1750, 0.44).
narrative_ontology:measurement(stat_su_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1760, 0.43).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1774, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the statute_of_anne_ip_foundation kernel, each instantiating a structurally distinct claim about what the Statute of Anne did. institutional_reallocation_reading treats the statute as transferring an existing occupied legal slot from the Stationers' Company to authors (victim: Stationers; beneficiary: publishers via assignment). conceptual_emergence_reading treats the statute as creating a new conceptual category (copyright as limited regulatory tool) with no prior occupied slot to reallocate. entangled_event_reading treats institutional and conceptual change as one inseparable event, refusing to assign a single ε to either dimension alone. The three stories share no single ε — they are linked via network edges, not merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
