% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Statute of Anne (1710) — Institutional Reallocation of Copyright from Stationers to Authors
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the institutional-reallocation reading of the
 *   Statute of Anne kernel: the 1710 Act is read as a transfer of an existing
 *   right-type — the exclusive-printing entitlement previously administered
 *   as the Stationers' Company's internal guild property — to a new nominal
 *   holder class (authors), from whom publishers immediately reacquired the
 *   economic substance via assignment. On this reading the statute did not
 *   invent a new legal object; it relocated who occupied a pre-existing
 *   structural slot. This is a distinct constraint from the sibling readings:
 *   the conceptual-emergence reading treats the statute as creating a
 *   genuinely new regulatory category (limited-term copyright as
 *   public-learning instrument, ε structured around a very different
 *   beneficiary/victim map centering the public domain), and the
 *   entangled-event reading treats institutional and conceptual change as
 *   inseparable. Per the ε-invariance principle these are authored as three
 *   separate constraint files sharing a kernel, not one story with a hidden
 *   parameter.
 *
 * KEY AGENTS:
 *   - stationers_company_incumbents: primary victim (organized/constrained) — loses perpetual guild-administered monopoly
 *   - publisher_assignees: primary beneficiary via assignment (organized/arbitrage) — reoccupies the economic position through contract rather than guild status
 *   - author_class_nominal: nominal beneficiary (moderate/constrained) — formally named first holder, frequently passes the right through immediately
 *   - crown_licensing_apparatus: excluded predecessor institution (institutional/trapped) — the lapsed licensing regime with no seat at the reallocation table
 *   - legal_historians: analytical observer — traces the registry and contract record to test the institutional-transfer hypothesis against sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) — Institutional Reallocation of Copyright from Stationers to Authors").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '4cc5eb6c-ab00-484d-b7d8-9f8c7c875048').
narrative_ontology:cs_kernel_codification('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', fixed_text).
narrative_ontology:cs_authority_grounding('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', lineage).
narrative_ontology:cs_interpretation_layer_present('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048').
narrative_ontology:cs_reading_relation('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', foundational, rights_are_transferable_slots_not_novel_kinds).
narrative_ontology:cs_axiom_status(rights_are_transferable_slots_not_novel_kinds, holdable).
narrative_ontology:cs_axiom_grounding('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', rights_are_transferable_slots_not_novel_kinds, conventional).
narrative_ontology:cs_axiom('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', secondary, trade_economic_position_persists_across_formal_holder_change).
narrative_ontology:cs_axiom_status(trade_economic_position_persists_across_formal_holder_change, holdable).
narrative_ontology:cs_axiom_grounding('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', trade_economic_position_persists_across_formal_holder_change, empirically_contingent).
narrative_ontology:cs_reference_frame('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', guild_administered_perpetual_entry_right).
narrative_ontology:cs_drift_state('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', post_1774_donaldson_v_becket, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4cc5eb6c-ab00-484d-b7d8-9f8c7c875048', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publisher_assignees).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held a perpetual, guild-enforced entry-right (the 'stationer's copyright') in the register, controlling who could print any given title in perpetuity, independent of authorship. The statute stripped this and replaced it with a fixed, terminable term vested first in authors. The Company's registry apparatus, enforcement mechanisms, and monopoly rents built over a century were displaced by statutory design; their institutional position could not simply be reasserted once Parliament reallocated the underlying right.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_incumbents, payer,
    organized, generational, constrained, national).

% Former stationers and new booksellers who could now acquire the author's statutory right by assignment, typically at the moment of publication contracting, often for a lump sum. They lobbied for and helped draft the statute anticipating this outcome: the occupied institutional slot changed from 'stationer by guild membership' to 'assignee by private contract,' and they moved quickly to reoccupy the economic position vacated by the Company's collapse, this time via the market in author's rights rather than guild privilege.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publisher_assignees, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, publisher_assignees, agenda_setter).

% Named as the first holders of the new statutory right, which functioned as formal institutional recognition — the reallocated slot bore their name. In practice most authors lacked the capital or market position to withhold assignment, and quickly transferred the right to publishers under standard contracting pressure, so the nominal reallocation to authors was frequently a one-step pass-through to the very class it was ostensibly reallocated away from (the trade).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal, beneficiary,
    moderate, biographical, constrained, national).

% The prior licensing-act regime (lapsed 1695) had tied the Stationers' monopoly to state press-control interests. This apparatus had no seat in the 1710 reallocation debate; the statute's design proceeded from parliamentary and trade-lobby negotiation without formal reconstitution of press-licensing concerns, though print regulation interests shadowed the debate.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, crown_licensing_apparatus, excluded,
    institutional, generational, trapped, national).

% Examine surviving Stationers' Company registers, parliamentary debate records, and early assignment contracts to trace whether the 1710 reallocation functioned primarily as an institutional transfer of an existing right-type (this reading) versus the creation of a genuinely new legal object (the sibling conceptual-emergence reading).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the succession problem left by the 1695 lapse of press licensing: some mechanism was needed to allocate exclusive printing rights once the Crown-backed Stationers' registry lost statutory backing. The statute assigns that allocative function to a term-limited right vested first in the author, coordinating who may print a given work without reviving guild-administered perpetual entry.
% TRANSFER_FUNCTION: Moves the *institutional slot* of rights-holder — the position from which printing exclusivity is asserted and monopoly rents collected — from the Stationers' Company's guild-administered register to a market in individually assignable, term-limited author rights. Economic value largely continues flowing to the trade (via assignment), but the formal occupant of the rights-holding position changes.
% ABSENT_VOICES: The Stationers' Company as an institution had representation in the lobbying that produced the statute, but the guild's *perpetual* claim had no defenders in the final text — Parliament simply declined to renew licensing-act protections. Individual jobbing printers outside the Company, and readers/the public domain interest in eventual expiration, are largely absent from the recorded negotiation, which was dominated by established London booksellers.
% DISAPPEARANCE_RATIONALE: If the statutory reallocation were reversed and the pre-1710 guild-perpetual-entry system reinstated, the entire assignment-and-licensing market publishers built around statutory authors' rights would need to reorganize around guild registry rules again; conversely, absent any reallocation at all (the 1695 lapse simply continuing), printing would have reverted to an unregulated commons — either counterfactual visibly rearranges who can print what and on what terms.
% FOUNDING_PROBLEM: The 1695 expiration of the Licensing Act left no statutory basis for exclusive printing rights; the Stationers' Company's perpetual internal copyright had operated for over a century under state licensing but had no independent legal foundation once licensing lapsed, creating a vacuum the trade wanted refilled with enforceable exclusivity.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the book trade (drawing on Parliamentary records and the Stationers' own registers) attest that the specific vacuum — unenforceable printing exclusivity after 1695 — was resolved definitively by the 1710 statute and subsequent case law (settled by Donaldson v Becket, 1774, which foreclosed the trade's later claim to a surviving perpetual common-law copyright); the trade's own later litigation position, seeking to reassert perpetual control via a *different* legal theory, is not independent corroboration since it is the benefiting party reasserting the problem's liveness for its own advantage.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 — moderate-high, not extreme — because the reallocation reading holds that value continues flowing largely to the trade (now via assignment contracts rather than guild entry-fees); what changed is the formal occupant of the rights-holding slot, not necessarily the ultimate economic beneficiary. Suppression starts elevated (0.6) reflecting the pre-1710 guild enforcement apparatus, drops sharply at the moment of statutory reallocation (0.35-0.4) as guild enforcement is displaced by a lighter, court-administered term system, then drifts back up modestly as publishers' assignment practices harden into their own de facto enforcement norms. Theater ratio rises across the interval (0.15 to 0.35 by mid-century) as the nominal 'author's right' increasingly functions as a formality quickly assigned away — the naming ceremony persists while the substantive economic position is captured by the same trade actors who held it before, now via contract rather than charter.
 *
 * PERSPECTIVAL GAP:
 *   From the Stationers' Company's seat, the statute is a straightforward expropriation of an existing property interest — the perpetual entry-right simply vanishes by legislative fiat. From publisher-assignees' seat (many of them former or current Company members), the reallocation is a manageable transition: the underlying economic position is reoccupied within a single publishing cycle via standard assignment contracting. From the nominal author-beneficiary seat, the reallocation is largely symbolic — formal vesting followed immediately by economically compelled transfer. The engine should compute a tangled-rope or even snare-leaning result for the Company's seat (concentrated loss, no coordination benefit to them) against something closer to a rope or scaffold reading for publishers (a genuine coordination problem — succession after the 1695 licensing lapse — resolved in their favor).
 *
 * DIRECTIONALITY LOGIC:
 *   Publisher_assignees derive a low-to-symmetric directionality: they both benefit from the new right (arbitrage exit — can restructure contracting practices freely) and helped engineer its design, placing them near the beneficiary end. Author_class_nominal is a beneficiary in name only; their constrained exit options (limited bargaining power against established booksellers) push their effective directionality toward the target end despite nominal beneficiary status — this divergence between nominal role and structural directionality is itself the analytical point of the institutional-reallocation reading. Stationers_company_incumbents are unambiguous targets: organized but with only constrained exit (the guild could not simply relocate its monopoly elsewhere once Parliament acted), so directionality sits high, i.e. this is where extraction is concentrated on this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (post-1695 licensing vacuum) is authored as dead — resolved definitively, per legal-historical corroboration outside the trade, by the 1710 Act and cemented by Donaldson v Becket (1774). The classification therefore does not need to treat the ongoing operation of assignment-based copyright as still solving that specific 1690s succession crisis; what persists afterward is a different function (routine allocation of exclusivity in a mature print market), which is why theater_ratio is authored as rising — later invocations of the 'author's right' as solving an urgent institutional vacuum are increasingly performative once the vacuum itself has been closed for decades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_creation_locus,
    'Did the Statute of Anne primarily relocate an existing right-type between institutional occupants (this reading), or did it create a legally novel object (limited-term, author-vested, public-learning-purposed copyright) that had no true predecessor in the Stationers'' perpetual entry-right (the sibling conceptual_emergence_reading)?',
    'Close doctrinal comparison of the legal character of the pre-1710 stationer''s copyright (an internal guild/trade-property right, unenforceable at common law outside the Company) against the statutory right (a term-limited, universally justiciable entitlement) — if the two are held to be different in kind rather than merely different in holder, the reallocation framing understates the change.',
    'If the conceptual-emergence framing is correct, this story''s beneficiary/victim map and its comparatively high ε (reflecting continued trade capture) may overstate the extraction, since a genuinely novel public-learning-oriented right would have a different — lower extraction, public-benefit-centered — structural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_vs_creation_locus, conceptual, 'Whether the statute is best modeled as institutional transfer or conceptual creation — the central contest between this reading and its primary sibling.').

omega_variable(
    author_beneficiary_pass_through_extent,
    'What fraction of statutory rights vested nominally in authors were assigned to publishers immediately upon or before publication, versus retained by authors for meaningful economic benefit?',
    'Archival analysis of surviving 18th-century publishing contracts and Stationers'' Register post-1710 assignment entries to quantify how often and how quickly authorial vesting converted to publisher assignment.',
    'A high pass-through rate strengthens the reading that ''author'' was primarily a formal occupant label with publishers as the true structural beneficiary (raising confidence in the tangled_rope classification); a low pass-through rate would support treating authors as genuine independent beneficiaries and would lower the effective extraction directed at the nominal author class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_beneficiary_pass_through_extent, empirical, 'Empirical extent to which the nominal author-beneficiary was a pass-through to publisher assignees.').

omega_variable(
    stationers_company_natural_law_framing_ambiguity,
    'Is the pre-1710 Stationers'' Company monopoly itself best understood as a constructed extractive arrangement (making its 1710 loss a corrective reallocation) or as the then-prevailing ''natural'' order of the print trade (making its loss a genuine expropriation of a settled institutional position)?',
    'Historical analysis of whether contemporaries treated the guild''s perpetual entry-right as a customary/quasi-natural property interest or as a state-granted monopoly privilege understood at the time to be revocable.',
    'If contemporaries widely understood the Company''s right as a revocable state grant, the victim characterization of stationers_company_incumbents is weaker (they held a privilege, not a natural entitlement); if it was understood as settled property, the victim characterization and the story''s ε for their seat should be read as more severe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stationers_company_natural_law_framing_ambiguity, conceptual, 'Whether the displaced Stationers'' monopoly was itself naturalized property or an acknowledged revocable state privilege, bearing on how severely its loss should be weighted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1690, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1690, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1690, 0.15).
narrative_ontology:measurement(stat_tr_t1703, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1703, 0.2).
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.25).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.32).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1774, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t1690, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1690, 0.5).
narrative_ontology:measurement(stat_be_t1703, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1703, 0.62).
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.58).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1750, 0.6).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1690, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1690, 0.6).
narrative_ontology:measurement(stat_su_t1703, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1703, 0.35).
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.4).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.45).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1774, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the statute_of_anne_ip_foundation kernel, decomposed per the eps-invariance principle because the natural-language label 'the Statute of Anne's foundational significance' conflates structurally distinct claims with different ex values: institutional_reallocation_reading (this file, eps=0.58, tangled_rope, victim=Stationers) treats the statute as a transfer of an existing right between occupants; conceptual_emergence_reading treats it as creation of a genuinely new regulatory category with a different, lower-eps, public-benefit-centered structure; entangled_event_reading declines to decompose the conceptual and institutional dimensions at all and should be read as a distinct claim about inseparability rather than a synthesis of the other two. All three link to each other via affects_constraints; none is a summary or average of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
