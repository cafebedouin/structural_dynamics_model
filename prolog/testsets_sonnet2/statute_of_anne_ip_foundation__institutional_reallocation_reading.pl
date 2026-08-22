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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Statute of Anne (1710) — Institutional Reallocation Reading: Rights Transferred from Stationers' Company to Authors
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story reads the Statute of Anne (1710) purely as an institutional
 *   reallocation event: an existing kind of entitlement (control over
 *   reprinting) that previously sat with the Stationers' Company as a guild
 *   moved, by statute, to sit initially with individual authors. On this
 *   reading the interesting fact is not that a new conceptual category of
 *   property was invented but that the occupied institutional slot changed
 *   hands — from a closed guild registry to a formally open, author-first
 *   vesting rule that publishers rapidly recaptured through assignment
 *   contracts. This is one of three linked readings of the same kernel
 *   (statute_of_anne_ip_foundation): the conceptual_emergence_reading holds
 *   that the statute created copyright as a genuinely new regulatory category
 *   rather than reallocating a pre-existing property right; the
 *   entangled_event_reading holds the conceptual and institutional dimensions
 *   are inseparable in a single event. This reading's epsilon is authored
 *   specifically for the reallocation transaction and its downstream
 *   assignment dynamics — not for the conceptual novelty claim, which the
 *   sibling story addresses on its own terms with its own epsilon.
 *
 * KEY AGENTS:
 *   - stationers_company_incumbents: primary target of the reallocation (organized/trapped) — loses the perpetual monopoly position
 *   - commercial_publishers_via_assignment: structural beneficiary via post-vesting assignment (organized/arbitrage) — reconstitutes the extraction position under new formal cover
 *   - author_class_nominal_holders: formal beneficiary of first-holder status but practically constrained (powerless/constrained) — the named class whose elevation is real on paper and thin in practice
 *   - parliament: agenda-setter who designed the reallocation (institutional/analytical)
 *   - reading_public: incidental beneficiary of the fixed-term structure (moderate/constrained) — no seat in the transaction
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
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) — Institutional Reallocation Reading: Rights Transferred from Stationers' Company to Authors").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '4d4939dd-2f67-4d62-9f66-ee6dd5579127').
narrative_ontology:cs_kernel_codification('4d4939dd-2f67-4d62-9f66-ee6dd5579127', fixed_text).
narrative_ontology:cs_authority_grounding('4d4939dd-2f67-4d62-9f66-ee6dd5579127', extraction).
narrative_ontology:cs_interpretation_layer_present('4d4939dd-2f67-4d62-9f66-ee6dd5579127').
narrative_ontology:cs_reading_relation('4d4939dd-2f67-4d62-9f66-ee6dd5579127', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d4939dd-2f67-4d62-9f66-ee6dd5579127', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('4d4939dd-2f67-4d62-9f66-ee6dd5579127', foundational, entitlement_preexisted_and_merely_transferred).
narrative_ontology:cs_axiom_status(entitlement_preexisted_and_merely_transferred, holdable).
narrative_ontology:cs_axiom_grounding('4d4939dd-2f67-4d62-9f66-ee6dd5579127', entitlement_preexisted_and_merely_transferred, conventional).
narrative_ontology:cs_axiom('4d4939dd-2f67-4d62-9f66-ee6dd5579127', secondary, occupied_institutional_slot_is_the_unit_of_analysis).
narrative_ontology:cs_axiom_status(occupied_institutional_slot_is_the_unit_of_analysis, holdable).
narrative_ontology:cs_axiom_grounding('4d4939dd-2f67-4d62-9f66-ee6dd5579127', occupied_institutional_slot_is_the_unit_of_analysis, conventional).
narrative_ontology:cs_reference_frame('4d4939dd-2f67-4d62-9f66-ee6dd5579127', stationers_company_perpetual_registry_monopoly).
narrative_ontology:cs_drift_state('4d4939dd-2f67-4d62-9f66-ee6dd5579127', donaldson_v_becket_1774, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d4939dd-2f67-4d62-9f66-ee6dd5579127', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal_holders).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, authorial_first_holding_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held a perpetual, self-renewing entry-based monopoly on printing under royal charter and licensing acts for over 150 years before 1710. The statute stripped their perpetual claim, replacing it with a fixed statutory term running from first publication and vesting the initial entitlement in authors rather than the guild. Their registry, their enforcement apparatus against unlicensed printing, and their capital investment in existing copies were all devalued by the reallocation. They lobbied heavily against the bill and for its renewal on old terms, and lost.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_incumbents, payer,
    organized, generational, trapped, national).

% Former Stationers and new booksellers alike quickly discovered that authors, lacking distribution capital and market access, would assign their newly vested rights back to publishers for a lump sum or modest royalty almost immediately after the statute took effect. The formal holder of first title changed; the practical occupant of the commercial position — collecting from reprints, controlling catalogs, enforcing against piracy — reconstituted itself within a generation. They administer the assignment contracts and lobby for extensions of the statutory term.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, commercial_publishers_via_assignment, agenda_setter).

% Named as the first holders of the new statutory right — a genuine and unprecedented formal elevation from the pre-1710 position where they held nothing enforceable against publishers. In practice, most authors had no capital to print, distribute, or litigate infringement themselves, so the right functioned mainly as a bargaining chip to be sold at the point of first sale to a publisher. They benefit from the formal recognition and from marginally improved bargaining leverage relative to the old system, but bear the practical cost of having a right they mostly cannot exploit without assigning it away.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal_holders, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_class_nominal_holders, payer).

% Benefits incidentally from the statute's fixed-term structure (works eventually enter the public domain, unlike the Stationers' de facto perpetual control) and from increased competition among publishers no longer bound to a single company's registry. Does not administer or receive assignment revenue, and has no seat in the negotiation between authors and publishers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    moderate, generational, constrained, national).

% Drafted and enacted the statute, choosing to vest the initial entitlement in authors rather than renew the Stationers' charter or leave the space unregulated. Balanced Stationers' lobbying, bookseller interests, and public concern about the printing monopoly. Could have structured the reallocation differently (e.g., vesting in the public domain immediately, or in a licensing board) but chose the author-first model.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Printers outside London and outside the Stationers' registry who operated in the gray zone before 1710 hoped the end of Stationers' perpetual control would open printing to wider competition. The statute's assignment dynamics reconcentrated commercial control in a new set of London booksellers-turned-publishers; the provincial printers had no seat in the 1710 negotiation and their interests do not appear in the statute's text.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, unlicensed_provincial_printers, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of who is entitled to control and profit from a printed work when the prior guild-monopoly registry system collapsed politically — the statute supplies a clear, enforceable, time-bounded rule for who holds the initial claim, replacing an expired and contested licensing regime.
% TRANSFER_FUNCTION: Moves formal first-holder status from the Stationers' Company (guild-based, perpetual, registry-mediated) to individual authors (statutory, term-limited, print-of-first-publication-mediated); moves practical commercial control from the old guild membership to a reconstituted class of publishers who acquire the author's right by assignment shortly after vesting.
% ABSENT_VOICES: Provincial and unlicensed printers who were not party to the Stationers' registry and had no formal voice in the 1710 negotiation; also largely absent are readers/the public, whose interest in eventual public-domain entry was invoked rhetorically in debate but who held no seat at the table.
% DISAPPEARANCE_RATIONALE: If the reallocation were undone and the Stationers' perpetual entry-based monopoly were restored, the entire chain of author-to-publisher assignment contracts, the fixed statutory term, and the modern publishing contract structure built on 'first vesting in the author' would need to be rebuilt on a different institutional foundation — the guild registry and its licensing hierarchy would resume as the operative mechanism.
% FOUNDING_PROBLEM: The Licensing of the Press Act had lapsed in 1695, leaving no clear statutory basis for controlling reprinting; the Stationers' Company wanted its perpetual registry-based monopoly restored, while Parliament and reform-minded voices wanted a bounded, author-anchored alternative that would not simply hand permanent control back to a single guild.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining Stationers' Company records and parliamentary debate transcripts (outside both the Stationers' and the modern publishing industry's interest) attest that the specific institutional problem — an unregulated post-1695 reprinting scramble threatening the book trade's capital investments — was resolved by the 1710s and that the statute's author-vesting structure has since been maintained primarily because it now underwrites the assignment-based publishing contract industry, not because the original scramble persists.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at a moderate-high 0.58 for the standing post-statute arrangement (not for the initial 1710 moment, which was more purely reallocative and less extractive — see the temporal series). The rising trajectory across 1710-1774 reflects a documented empirical pattern: as assignment contracts became standard practice, publishers reconstituted much of their pre-1710 commercial control under the new formal structure, and litigation (culminating in Donaldson v Becket, 1774) increasingly turned on whether the statute's term limits could be evaded by claiming a perpetual common-law copyright. Suppression is moderate (0.42) — the statute itself is not coercive in the way a guild licensing regime was, but enforcement of the new statutory term against unauthorized reprinting, and the practical pressure on authors to assign rights immediately, both carry real structural force. Theater ratio is modest and rising (0.10 to 0.30) as the 'protects authors' framing increasingly covers what is functionally a publisher-administered assignment market.
 *
 * DIRECTIONALITY LOGIC:
 *   Stationers' Company incumbents are the clearest victims under this reading: they lose a valuable, self-renewing entitlement and cannot exit the change (trapped, organized power but defeated in Parliament). Commercial publishers, though formally displaced as the FIRST holder, structurally reoccupy the beneficiary position through assignment — this is why 'commercial_publishers_via_assignment' rather than 'stationers_company_incumbents' is named as a primary beneficiary: the institutional slot persisted even though its formal occupant nominally changed. Authors sit in a genuinely mixed position — real formal elevation (hence beneficiary role) but practically constrained exit (hence also payer, in the sense of bearing the cost of a right they cannot exploit without immediate assignment).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unregulated post-1695 reprinting scramble with no clear statutory basis for control — was substantially resolved within a decade or two of enactment. What persists past that point is not the original coordination problem but a reconstituted extraction structure (publisher control via assignment) wearing the author-protection justification. This is exactly the kind of divergence the classification is meant to catch: reading this constraint as a stable Rope (mere coordination fix) would miss the documented recapture dynamic; reading it as a pure Snare would miss the genuine and unprecedented formal gain authors received relative to their pre-1710 position (they held literally nothing enforceable before). Tangled Rope status requires both a real coordination function (replacing an expired licensing vacuum with clear rules) and asymmetric extraction (Stationers' incumbents pay, publishers recapture) — both are present and both are load-bearing to the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_conceptual_novelty_locus,
    'Is the Statute of Anne''s significant historical effect best located in WHO held the right (institutional reallocation) or WHAT KIND of right was created (conceptual emergence of copyright as a regulatory tool distinct from property)? Different legal historians locate the statute''s importance in different places.',
    'Comparative doctrinal analysis of how courts treated the statute in the intervening decades — if courts and litigants (e.g., in Donaldson v Becket) argued primarily about WHO holds the right and for how long, that supports the reallocation reading; if the argument centered on whether a genuinely new kind of entitlement existed at all (distinct from common-law literary property), that supports the conceptual emergence reading.',
    'If the reallocation framing is correct, the coordination/extraction analysis here holds; if the conceptual emergence framing is correct instead, the relevant beneficiary/victim structure and epsilon would need to be assessed against the older sibling story instead, which analyzes the novel-category claim on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_vs_conceptual_novelty_locus, conceptual, 'Whether the statute''s structurally significant feature is the transfer of an existing right or the creation of a new kind of right — routed to the sibling constraint rather than resolved within this story.').

omega_variable(
    assignment_recapture_speed,
    'How quickly and how completely did publishers recapture practical control via assignment after 1710 — was the author''s formal first-holder position ever a meaningfully independent bargaining position, or was recapture near-instantaneous and near-total from the outset?',
    'Archival analysis of surviving 18th-century publishing contracts and Stationers'' Register entries in the years immediately following 1710, tracking the time lag and terms of author-to-publisher assignments.',
    'If recapture was near-instantaneous and near-total, the extractiveness value at t=1710 should be revised upward and the temporal series flattened rather than rising — the tangled-rope structure would have been present from the statute''s first operative year rather than emerging gradually.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assignment_recapture_speed, empirical, 'Whether publisher recapture of commercial control was immediate or gradual, affecting the shape of the temporal extractiveness series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(stat_tr_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1731, 0.2).
narrative_ontology:measurement(stat_tr_t1745, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1745, 0.26).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1760, 0.29).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1774, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.35).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1720, 0.42).
narrative_ontology:measurement(stat_be_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1731, 0.5).
narrative_ontology:measurement(stat_be_t1745, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1745, 0.55).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1760, 0.57).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.3).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1720, 0.33).
narrative_ontology:measurement(stat_su_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1731, 0.36).
narrative_ontology:measurement(stat_su_t1745, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1745, 0.4).
narrative_ontology:measurement(stat_su_t1760, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1760, 0.41).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1774, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the statute_of_anne_ip_foundation kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with an observable-dependent classification. institutional_reallocation_reading treats the statute as a transfer of an existing entitlement between institutional occupants (Stationers' Company -> authors -> publishers via assignment), with epsilon authored around the recapture dynamic. conceptual_emergence_reading treats the statute as creating a genuinely new regulatory category (bounded copyright for learning) with a distinct epsilon focused on the novelty claim's own contested status. entangled_event_reading treats the two dimensions as inseparable within a single event and authors its own epsilon accordingly. All three should be read together; none is the 'correct' single account of the statute — each instantiates a different structurally precise claim under the same colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
