% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: IP Category Emergence — Thinkability of Ownable Expression (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story isolates the THINKABILITY claim within the contested 'IP marks
 *   category emergence' kernel: in 1710, with the Statute of Anne, ownable
 *   expression became a legally coherent conceptual category for the first
 *   time — a genuinely new object of legal thought distinct from a printer's
 *   guild privilege over a physical copy. This is a claim about conceptual
 *   space gaining a point, not a claim about who thereby came to hold
 *   anything. The sibling readings (first_holding_reading: who became a
 *   legitimate claimant; synchronic_diachronic_seam: whether the two are
 *   actually distinct or a temporal-framing artifact) are separate
 *   constraints, not alternative measurements of this one. ε here is low and
 *   stable because the emergence of a category is itself a low-extraction
 *   event — a coordination gain in the space of arguable claims — even though
 *   its downstream deployment (in the first_holding reading) is where
 *   extraction, if any, would concentrate.
 *
 * KEY AGENTS:
 *   - print_trade_reformers: petition for the new statutory frame (organized/constrained) — set the agenda for the category's emergence
 *   - individual_authors: gain a conceptual slot they previously lacked (moderate/constrained) — beneficiaries of the new thinkable space
 *   - stationers_company: lose their exclusive framing vocabulary (organized/constrained) — bear the conceptual displacement cost
 *   - legal_theorists_of_authorship: gain a durable object of theorization (analytical/analytical) — observe and benefit from the category's long tail
 *   - future_claimants_under_the_category: absent from the founding moment, bound by its later boundaries (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.28).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.22).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence — Thinkability of Ownable Expression (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'e8652d99-3d5e-4cac-a607-98e1bafadc40').
narrative_ontology:cs_kernel_codification('e8652d99-3d5e-4cac-a607-98e1bafadc40', distributed).
narrative_ontology:cs_authority_grounding('e8652d99-3d5e-4cac-a607-98e1bafadc40', distributed).
narrative_ontology:cs_reading_relation('e8652d99-3d5e-4cac-a607-98e1bafadc40', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('e8652d99-3d5e-4cac-a607-98e1bafadc40', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('e8652d99-3d5e-4cac-a607-98e1bafadc40', foundational, expression_is_a_distinct_conceptual_object).
narrative_ontology:cs_axiom_status(expression_is_a_distinct_conceptual_object, holdable).
narrative_ontology:cs_axiom_grounding('e8652d99-3d5e-4cac-a607-98e1bafadc40', expression_is_a_distinct_conceptual_object, conventional).
narrative_ontology:cs_axiom('e8652d99-3d5e-4cac-a607-98e1bafadc40', foundational, category_emergence_precedes_and_is_severable_from_occupancy).
narrative_ontology:cs_axiom_status(category_emergence_precedes_and_is_severable_from_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('e8652d99-3d5e-4cac-a607-98e1bafadc40', category_emergence_precedes_and_is_severable_from_occupancy, conventional).
narrative_ontology:cs_reference_frame('e8652d99-3d5e-4cac-a607-98e1bafadc40', guild_privilege_over_printed_copies).
narrative_ontology:cs_drift_state('e8652d99-3d5e-4cac-a607-98e1bafadc40', statute_of_anne_enactment, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e8652d99-3d5e-4cac-a607-98e1bafadc40', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, print_trade_reformers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, individual_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_theorists_of_authorship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, stationers_company).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, expression_as_conceptually_distinct_object).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, copy_right_as_novel_legal_kind).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition Parliament for a statutory scheme after the Licensing Act lapses; they push the conceptual reframe from 'trade privilege over a printed object' to 'right in an expressive work' because the old guild-privilege vocabulary no longer gives them a legal hook to argue from. They do not yet hold a stable legal category — they are trying to make one thinkable so it can later be claimed.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, print_trade_reformers, agenda_setter,
    organized, generational, constrained, national).

% Before 1710 authors have no vocabulary in which to assert a claim over their own text independent of a printer's privilege; the emergence of the category gives them, for the first time, a conceptual slot to occupy, even before any court has decided who actually fills it. Their situation changes because the space of thinkable claims changes, not because any specific dispute is yet resolved in their favor.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, individual_authors, beneficiary,
    moderate, biographical, constrained, national).

% Loses its monopoly on the sole available vocabulary for talking about rights in printed matter — 'copy right' becomes conceivable as something an author could hold, not only something a guild member registers. This does not yet cost the Stationers a specific holding, but it costs them the exclusive conceptual frame they had used to structure the trade.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, stationers_company, payer,
    organized, generational, constrained, national).

% Gain a new conceptual object to theorize — 'ownable expression' as distinct from ownable physical copies or trade privileges. This category becomes the basis for centuries of subsequent doctrine (originality, fixation, the idea/expression distinction) that could not have been formulated in pre-1710 vocabulary.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_theorists_of_authorship, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, legal_theorists_of_authorship, observer).

% Composers, translators, later photographers and software authors who will eventually contest what counts as an ownable expressive work are not present in 1710 and have no voice in how the category's boundaries get set; the category's shape at emergence constrains what they will later be able to argue at all.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, future_claimants_under_the_category, excluded,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared vocabulary — a legally coherent category of 'ownable expression' distinct from ownership of a physical copy or a printer's trade privilege — so that courts, legislators, and disputants can refer to the same kind of thing when arguing about rights in text.
% TRANSFER_FUNCTION: This reading does not itself move money, work, or holdings between named parties; it moves conceptual capacity — the category of claims that can be coherently asserted at all — from a state where 'right in expression as such' is not a thinkable legal object to a state where it is. Downstream first-holding disputes (a separate reading) are what actually reassign occupancy.
% ABSENT_VOICES: Later claimant classes (composers, dramatists, and eventually authors in media forms unimagined in 1710) have no representation in the category's initial formation; the category's boundaries are set by print-trade disputants and legislators responding to a narrow crisis, not by anticipation of who will later need to fit inside it.
% DISAPPEARANCE_RATIONALE: If the conceptual category of ownable expression had never become thinkable in 1710, the entire subsequent architecture of copyright doctrine — originality, the idea/expression dichotomy, moral rights, derivative-work claims — would have no vocabulary to be built in; disputes over printed text would remain framed exclusively as trade-privilege or property-in-the-physical-copy disputes, a materially different legal universe.
% FOUNDING_PROBLEM: The lapse of the Licensing Act (1695) left the print trade with no legal mechanism to prevent unauthorized reprinting, and the existing guild-privilege framework could not be revived or extended by Parliament in its old form; a new conceptual basis for controlling reproduction of text was needed.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (e.g. scholarship on the Statute of Anne's drafting history and the Stationers' own petitions) attest that the pre-1710 vocabulary genuinely lacked a category for authorial right in expression as opposed to trade privilege in copies; this is corroborated independently of the authors and theorists who benefited from the category's creation, via parliamentary records and printers' guild archives predating the reform.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.28 by 1730) because this reading tracks only the conceptual-space event — the appearance of a new legally coherent category — not any subsequent enforcement or rent extraction under that category, which belongs to the first_holding reading. Suppression is low-moderate (0.22): the old guild-privilege framing does not vanish by force, it becomes one option among others as a genuinely new option opens up. Theater ratio stays low and rises only slightly (0.08 to 0.15) reflecting some ceremonial legislative process around the Statute's passage, but no performative substitution of function. Accessibility collapse is authored moderately high (0.62): once the category exists, arguing a dispute in pre-1710 vocabulary becomes progressively harder to sustain — the new conceptual frame displaces the old one in legal discourse even though it does not coercively suppress it.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' seat experiences category emergence as loss of interpretive monopoly even though no specific holding is yet reassigned to them; the theorists' and future authors' seats experience the same event as pure conceptual gain. The engine should register this asymmetry as a coordination-dominant profile with a small, real distributional shadow — not as extraction, because nothing is yet being transferred, only what CAN be argued.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (print_trade_reformers, individual_authors, legal_theorists) are declared without a victim group because this reading, unlike its first_holding sibling, does not allocate a contested resource between a winner and a loser — it opens a conceptual slot. The Stationers Company is authored as payer not victim: their loss is the loss of exclusive vocabulary control, not extraction of a good they held. This is why claimed_type is rope rather than tangled_rope — no active enforcement extracts from an identified victim at the category-emergence layer itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the post-1695 vocabulary gap for controlling reproduction of text) is corroborated as live at the time of the Statute of Anne by independent parliamentary and guild-archive records, not solely by the beneficiaries who profited from the new category. Because this reading tracks the category's emergence rather than its ongoing use, mandatrophy is not yet applicable — the relevant question (whether the category still serves its founding function centuries later) belongs to a downstream reading of the kernel, not to this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_emergence_vs_retrospective_narrative,
    'Did the category of ''ownable expression'' genuinely become newly thinkable in 1710, or is this a retrospective narrative imposed by later doctrine reading backward onto a more continuous, gradual shift in print-trade argumentation?',
    'Close textual analysis of print-trade petitions and parliamentary debate transcripts from 1690-1710 for evidence of genuinely novel argumentative moves versus continuity with existing guild-privilege rhetoric merely relabeled.',
    'If the shift is genuinely discontinuous, the thinkability_reading stands as a distinct, real event; if continuous, this reading and synchronic_diachronic_seam''s collapse hypothesis would be strongly supported, undermining the kernel''s three-way decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_emergence_vs_retrospective_narrative, conceptual, 'Whether 1710 marks a genuine conceptual discontinuity or a narrative overlay on gradual legal drift.').

omega_variable(
    thinkability_first_holding_independence,
    'Is the thinkability of a legal category logically and temporally separable from the question of who first occupies it as a claimant, or do the two necessarily coincide such that treating them as separate constraints double-counts one historical event?',
    'Comparative analysis against other legal category emergences (e.g., corporate personhood, privacy torts) where thinkability and first-occupancy are more clearly staggered in time, to test whether the 1710 case is unusual in collapsing the two.',
    'If the M4/M5 collapse test (synchronic_diachronic_seam) resolves toward collapse, this reading and first_holding_reading should be merged into a single constraint rather than treated as siblings — the current three-file decomposition would be revised to two or one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thinkability_first_holding_independence, conceptual, 'Whether the thinkability and first-holding readings of the kernel are genuinely distinct constraints or an artifact of temporal framing.').

omega_variable(
    beneficiary_selection_bias_in_naming,
    'Because the beneficiaries named here (reformers, authors, theorists) are also the parties who most clearly articulated and preserved the historical record of this conceptual shift, is the apparent ''emergence'' partly an artifact of whose arguments survived in the archive?',
    'Search for surviving Stationers'' Company internal records or opposition pamphlets that would show whether the old guild-privilege framing was actively defended as still coherent, rather than conceding the new category''s coherence.',
    'If strong contemporaneous opposition existed arguing the new category was incoherent nonsense, this weakens the claim that the category was straightforwardly and consensually ''legally coherent'' by 1710 and would lower confidence in the emerges-cleanly narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_selection_bias_in_naming, empirical, 'Whether the archival record of category emergence is skewed toward its proponents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1690, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__thinkability_reading, theater_ratio, 1690, 0.08).
narrative_ontology:measurement(ip_c_tr_t1698, ip_category_emergence__thinkability_reading, theater_ratio, 1698, 0.09).
narrative_ontology:measurement(ip_c_tr_t1706, ip_category_emergence__thinkability_reading, theater_ratio, 1706, 0.11).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.12).
narrative_ontology:measurement(ip_c_tr_t1718, ip_category_emergence__thinkability_reading, theater_ratio, 1718, 0.14).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__thinkability_reading, theater_ratio, 1730, 0.15).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__thinkability_reading, base_extractiveness, 1690, 0.12).
narrative_ontology:measurement(ip_c_be_t1698, ip_category_emergence__thinkability_reading, base_extractiveness, 1698, 0.15).
narrative_ontology:measurement(ip_c_be_t1706, ip_category_emergence__thinkability_reading, base_extractiveness, 1706, 0.19).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.22).
narrative_ontology:measurement(ip_c_be_t1718, ip_category_emergence__thinkability_reading, base_extractiveness, 1718, 0.25).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__thinkability_reading, base_extractiveness, 1730, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__thinkability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.03).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Part of a three-story decomposition of the 'IP marks category emergence in 1710' kernel. thinkability_reading (this file) tracks the conceptual-space event — a new legal category becoming coherent — with low, stable ε. first_holding_reading tracks the occupancy event — authors entering the legitimate claimant set — which carries its own distinct ε and beneficiary/victim structure (the Stationers Company's lost exclusivity is more directly implicated there). synchronic_diachronic_seam is the meta-level constraint testing whether the first two are genuinely independent claims or a temporal-framing artifact of a single underlying shift (the M4/M5 collapse test). All three are linked via network.affects_constraints; none should be read as alternative measurements of one constraint — per the ε-invariance principle, they are three distinct constraints sharing a historical moment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
