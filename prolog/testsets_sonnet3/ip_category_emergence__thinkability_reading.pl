% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: Emergence of 'Ownable Expression' as a Coherent Legal Category (Statute of Anne, 1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story authors the THINKABILITY reading of the ip_category_emergence
 *   kernel: the claim that 1710 (the Statute of Anne) marks the point at
 *   which 'ownable expression' — a copy-right in the text itself, severable
 *   from the physical book and from guild-privilege occupancy — became a
 *   coherent legal category at all. This is a conceptual-space claim, not an
 *   occupancy claim: the emphasis is on the vocabulary and reasoning
 *   apparatus becoming available to courts (Millar v Taylor 1769 argued the
 *   new category through to a common-law property claim; Donaldson v Beckett
 *   1774 curtailed it to the statutory term), not on which specific parties
 *   first held the new entitlement. The sibling reading first_holding_reading
 *   asks the occupancy question (who entered the legitimate claimant set) and
 *   is a structurally distinct constraint with its own ε — it is NOT part of
 *   this story. The sibling synchronic_diachronic_seam asks whether the
 *   thinkability/occupancy distinction is even formally separable or an
 *   artifact of how the history is narrated diachronically; that too is a
 *   separate constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.42).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.38).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Emergence of 'Ownable Expression' as a Coherent Legal Category (Statute of Anne, 1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'fbac7f3a-8b37-4b27-a841-906d8812892c').
narrative_ontology:cs_kernel_codification('fbac7f3a-8b37-4b27-a841-906d8812892c', formalized).
narrative_ontology:cs_authority_grounding('fbac7f3a-8b37-4b27-a841-906d8812892c', lineage).
narrative_ontology:cs_interpretation_layer_present('fbac7f3a-8b37-4b27-a841-906d8812892c').
narrative_ontology:cs_reading_relation('fbac7f3a-8b37-4b27-a841-906d8812892c', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbac7f3a-8b37-4b27-a841-906d8812892c', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('fbac7f3a-8b37-4b27-a841-906d8812892c', foundational, expression_is_a_severable_ownable_kind).
narrative_ontology:cs_axiom_status(expression_is_a_severable_ownable_kind, holdable).
narrative_ontology:cs_axiom_grounding('fbac7f3a-8b37-4b27-a841-906d8812892c', expression_is_a_severable_ownable_kind, conventional).
narrative_ontology:cs_axiom('fbac7f3a-8b37-4b27-a841-906d8812892c', foundational, copy_right_is_conceptually_distinct_from_guild_privilege).
narrative_ontology:cs_axiom_status(copy_right_is_conceptually_distinct_from_guild_privilege, holdable).
narrative_ontology:cs_axiom_grounding('fbac7f3a-8b37-4b27-a841-906d8812892c', copy_right_is_conceptually_distinct_from_guild_privilege, conventional).
narrative_ontology:cs_reference_frame('fbac7f3a-8b37-4b27-a841-906d8812892c', guild_privilege_occupancy_regime).
narrative_ontology:cs_drift_state('fbac7f3a-8b37-4b27-a841-906d8812892c', post_statute_of_anne_common_law_extension, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('fbac7f3a-8b37-4b27-a841-906d8812892c', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, commercial_publishers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, named_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, book_trade_capital_holders).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, unlicensed_printers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, public_domain_reprinters).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, oral_and_folk_tradition_transmitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbied Parliament for a statutory replacement for the collapsing Stationers' Company perpetual copy-right after 1695, and administer the new registration and term system that emerged. They shift their rent-collection basis from guild membership to statutory title, retaining effective control of the trade while adopting the new conceptual vocabulary that makes 'a copy right' a transferable legal object distinct from a physical book or a guild seat.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, commercial_publishers, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, commercial_publishers, beneficiary).

% Gain, for the first time, a recognized (if term-limited and largely assignable-away) legal claim to their own composed expression, independent of guild membership. In practice most authors immediately sign the new right to a publisher for a lump sum, so the category's existence benefits them mainly as a bargaining chip rather than as a durable holding.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, named_authors, beneficiary,
    moderate, biographical, constrained, national).

% Previously operated in a genuinely contested zone where reprinting was a guild-monopoly violation, not a violation of an abstract 'right in the expression itself.' Once the category of ownable expression becomes legally coherent, their reprinting is recharacterized as infringement of a conceptual object rather than a trade-privilege breach, closing off argument strategies that were previously available (e.g., 'the guild's privilege lapsed, therefore printing is free').
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, unlicensed_printers, payer,
    moderate, biographical, constrained, national).

% Small booksellers and provincial printers who relied on the ambiguity of whether expression could be owned at all to reprint older or foreign texts cheaply. The new category, once stabilized, gives courts a vocabulary to extend claims of ownership over expression even where no living guild privilege exists, narrowing their operating space over subsequent decades of litigation (e.g., Millar v Taylor, Donaldson v Beckett).
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain_reprinters, payer,
    powerless, biographical, trapped, national).

% Ballad-singers, storytellers, and unlettered transmitters of collectively-authored material have no fixed, individually-authored text to register and so cannot access the new category at all. The conceptual architecture that makes expression ownable is built around fixed, attributable, individually-composed text — a structural precondition that quietly excludes forms of cultural production that do not fit that mold, with no forum in which this exclusion was argued.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, oral_and_folk_tradition_transmitters, excluded,
    powerless, generational, trapped, local).

% Reconstruct whether 1710 represents a genuine conceptual innovation (a new kind of thing became thinkable as property) or merely a relabeling of a pre-existing occupancy dispute (guild members vs. non-members) in new vocabulary. Their disagreement is the subject of the sibling readings this story is deliberately not adjudicating.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, commercial_publishers).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, portable, and increasingly standardized legal vocabulary — 'a copy right in the work' — that lets courts, publishers, and authors transact over expression without needing to invoke guild membership, royal charter, or trade-privilege lineage. This solves a genuine coordination problem: prior to a shared category, disputes over reprinting had to be litigated ad hoc against whatever local, expiring, or contested privilege applied.
% TRANSFER_FUNCTION: Moves the entitlement to control reproduction of a text from an occupancy-based claim (guild membership, royal patent) to a category-based claim (authorship of fixed expression), which publishers then immediately re-concentrate through near-universal assignment contracts — the conceptual gain nominally vests in authors but flows economically to publishers and, over subsequent litigation, hardens into a durable exclusionary claim usable against print-trade competitors who previously operated in genuinely open conceptual territory.
% ABSENT_VOICES: Oral tradition-bearers, folk composers, and collective/anonymous authorship communities have no seat in the 1710 debate and no path into the new category as structured — fixed, attributable, individual authorship is a precondition the statute does not argue for, it simply assumes, and no party in Parliament or the courts raises the exclusion.
% DISAPPEARANCE_RATIONALE: If the conceptual category of ownable expression had never stabilized in 1710, subsequent print-trade disputes would have continued to be litigated as occupancy/privilege conflicts (who holds the franchise) rather than as infringement of an abstract entitlement in the text itself — the entire subsequent architecture of copyright doctrine, including its extension to non-print media, presupposes that expression-as-such is the kind of thing that can be owned. Remove the category and courts have no vocabulary for that claim; they revert to guild-privilege or unfair-competition reasoning.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual licensing monopoly collapsed with the lapse of the Licensing Act in 1695, leaving no legal mechanism to prevent piracy of new books once guild privilege no longer governed the trade; publishers needed some replacement basis for exclusivity, and Parliament needed a framework that did not simply restore the unpopular pre-1695 licensing/censorship regime.
% FOUNDING_PROBLEM_CORROBORATION: Publishers and their trade historians attest the category was necessary to prevent the print trade's collapse into unrecoverable piracy. Independent legal historians outside the book trade (e.g., work tracing the Statute of Anne's drafting history and the subsequent Battle of the Booksellers) corroborate that a genuine coordination gap existed in 1695-1710, but also document that the same historians dispute whether what emerged in 1710 was a new conceptual category at all, or simply the same publisher-occupancy interest relabeled in individual-rights language to survive Parliament's refusal to restore licensing — that dispute is exactly what the sibling readings of this kernel exist to carry.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).
:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and rises across the interval as the conceptual category gets progressively deployed in litigation to extend exclusivity claims beyond what the coordination problem (preventing chaotic unlicensed reprinting after 1695) actually required — the peak at 1769 (Millar v Taylor asserting a perpetual common-law property in expression) followed by partial correction at 1774 (Donaldson v Beckett confirming the statutory, term-limited right) shows the category's extractive potential being periodically checked by courts, which is why the metric dips rather than monotonically rising. Suppression tracks the same arc: enforcement of the new category (against unlicensed printers invoking the old open-privilege arguments) intensifies through the 1760s and is partially relaxed once Donaldson clarifies the term-limited nature of the right. Theater ratio is modest throughout — the category does real coordination work (a shared vocabulary for adjudicating reprinting disputes) but an increasing share of litigation activity by 1769 is publishers performing a perpetual-property argument that the courts ultimately reject.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial publishers are the structural beneficiaries and agenda-setters: they lobbied for the statute, administer registration, and — critically — immediately re-absorb the nominal author's right through standard assignment contracts, so the conceptual gain accrues to them economically even though it is authored as vesting in authors. Named authors sit closer to beneficiary but with much lower effective capture, since most sign away the new right promptly; the category's chief value to them is negotiating leverage, not durable holding. Unlicensed printers and public domain reprinters are targets: the same conceptual innovation that solves publishers' coordination problem closes off argument space they previously had (that print was open because guild privilege had lapsed). Oral/folk tradition transmitters are excluded rather than victimized in the direct-extraction sense — the category's architecture (fixed, attributable, individually-composed text) is simply not built to include them, a structural exclusion with no forum where it was contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing the print trade's collapse into unpoliceable piracy after the 1695 lapse of licensing) was genuinely live in 1710 and arguably remains partially live in some form even today, which is why founding_problem_status is authored as contested rather than dead — but the specific CONCEPTUAL architecture that emerged (perpetual, expression-based property, per the publishers' 1769 argument in Millar v Taylor) was checked by Donaldson v Beckett in 1774 precisely because courts recognized that the coordination function did not require indefinite exclusivity. This is a clean example of why claim and metric independence matters: the category is CLAIMED here as tangled_rope (genuine coordination function — a shared vocabulary for adjudicating reprinting — bundled with asymmetric extraction that had to be actively checked by courts), and the metrics independently show that extraction rose specifically where enforcement outran the coordination need, then partially self-corrected. A pure snare reading would miss the genuine coordination the category solved in 1695-1710; a pure rope reading would miss that publishers used the same category to attempt indefinite rent extraction fifty years later.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_occupancy_priority,
    'Did the ''thinkability'' of ownable expression genuinely precede and cause the occupancy shift (authors becoming legitimate claimants), or is the conceptual-emergence narrative a retrospective gloss imposed on what was really a fight over who could invoke an existing kind of privilege?',
    'Close textual analysis of the Statute of Anne''s drafting history and contemporaneous pamphlet literature (e.g., the Battle of the Booksellers pamphlets) to determine whether drafters and litigants argued in terms of a genuinely new kind of object (severable expression) or merely extended existing occupancy vocabulary (privilege, franchise) to new holders (named authors) without conceptual innovation.',
    'If occupancy priority is correct, this thinkability_reading constraint may be a redescription of first_holding_reading rather than a genuinely distinct claim, which would collapse two nodes of the kernel network into one and change how contamination propagates between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_vs_occupancy_priority, conceptual, 'Whether the conceptual-emergence claim is causally/logically prior to, or merely a relabeling of, the occupancy-change claim.').

omega_variable(
    synchronic_diachronic_collapse_risk,
    'Is the thinkability/occupancy distinction (M4/M5 in the sibling synchronic_diachronic_seam framing) a real formal distinction with independent truth conditions, or does it collapse under a synchronic (structure-at-a-time) analysis into a single occupancy fact merely narrated diachronically as two events?',
    'Formal model-theoretic test proposed by the synchronic_diachronic_seam sibling reading: check whether the thinkability claim and the first-holding claim can vary independently across counterfactual legal histories (i.e., whether one could be true without the other in some coherent alternative timeline).',
    'If the distinction collapses, this story''s independent ε and the first_holding_reading sibling''s independent ε would need reconciliation rather than remaining genuinely separate constraints — though per the ε-invariance principle, until that formal collapse is demonstrated, the two are authored as separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synchronic_diachronic_collapse_risk, conceptual, 'Whether thinkability and first-holding are formally independent axes or a temporal-framing artifact of a single occupancy fact.').

omega_variable(
    exclusion_of_non_textual_authorship,
    'Is the exclusion of oral/folk/collective-authorship traditions from the new ownable-expression category an incidental byproduct of the fixed-text requirement, or a structural feature that made the category politically and administratively tractable for Parliament and courts in 1710?',
    'Comparative legal history: examine whether contemporaneous debates (Parliamentary records, pamphlet literature) ever raised the question of unfixed or collectively-authored material, and whether the fixation requirement was defended on administrative grounds when challenged in later centuries (e.g., early 20th-century folklore-copyright debates).',
    'If the exclusion was a deliberate tractability choice rather than an oversight, the category''s coordination function is narrower than usually claimed — it solves the coordination problem only for a subset of cultural production, and the boundary itself does extractive work by defining what counts as ownable at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_non_textual_authorship, empirical, 'Whether the category''s exclusion of non-fixed/collective authorship was incidental or structurally necessary to the category''s emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1695, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1695, ip_category_emergence__thinkability_reading, theater_ratio, 1695, 0.1).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__thinkability_reading, theater_ratio, 1730, 0.18).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__thinkability_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(ip_c_tr_t1769, ip_category_emergence__thinkability_reading, theater_ratio, 1769, 0.25).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.22).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1695, ip_category_emergence__thinkability_reading, base_extractiveness, 1695, 0.22).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.3).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__thinkability_reading, base_extractiveness, 1730, 0.36).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__thinkability_reading, base_extractiveness, 1750, 0.4).
narrative_ontology:measurement(ip_c_be_t1769, ip_category_emergence__thinkability_reading, base_extractiveness, 1769, 0.45).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1695, ip_category_emergence__thinkability_reading, suppression_requirement, 1695, 0.2).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.3).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__thinkability_reading, suppression_requirement, 1730, 0.38).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__thinkability_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(ip_c_su_t1769, ip_category_emergence__thinkability_reading, suppression_requirement, 1769, 0.48).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ip_category_emergence kernel. thinkability_reading (this story) authors the conceptual-space claim: a new kind of legally coherent object (ownable expression) came into existence in 1710. first_holding_reading authors the occupancy-change claim: authors entered the legitimate claimant set for exclusivity over reproduction. synchronic_diachronic_seam authors the meta-level claim that these two may be formally identical (M4/M5 collapse) rather than genuinely independent axes. Each carries its own ε, beneficiaries, and stakeholders per the ε-invariance principle; they are linked here rather than merged because measuring 'IP emergence' by conceptual-space criteria versus occupancy criteria versus formal-independence criteria yields different extraction profiles and different victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
