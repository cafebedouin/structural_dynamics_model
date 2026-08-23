% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation (Scholarly Standard Reading)
 *   domain: religious/textual_criticism/theology
 *
 * SUMMARY:
 *   The revisable_translation_reading treats the 1611 KJV as a landmark human
 *   translation superseded by better manuscripts (Sinaiticus, Vaticanus,
 *   papyri) and advances in linguistic knowledge. It establishes scholarly
 *   textual criticism as the legitimate authority for Bible translation,
 *   making translation selection a matter of consumer choice among competing
 *   versions. The constraint coordinates around critical texts (NA/UBS) and
 *   peer-reviewed translation committees. Extraction occurs via the
 *   publishing industry's control of copyrighted modern translations (NIV,
 *   ESV, NLT, CSB, etc.), which generate recurring revenue through revision
 *   cycles. Suppression is low — KJV remains widely available, and no one is
 *   prevented from using it — but the revisable reading's institutional
 *   dominance (seminaries, mainline denominations, major publishers)
 *   marginalizes the KJV-only position.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.35).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.15).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation (Scholarly Standard Reading)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '3e39abde-d1f9-41d7-a9b4-6585216af6ed').
narrative_ontology:cs_kernel_codification('3e39abde-d1f9-41d7-a9b4-6585216af6ed', fixed_text).
narrative_ontology:cs_authority_grounding('3e39abde-d1f9-41d7-a9b4-6585216af6ed', expertise).
narrative_ontology:cs_interpretation_layer_present('3e39abde-d1f9-41d7-a9b4-6585216af6ed').
narrative_ontology:cs_reading_relation('3e39abde-d1f9-41d7-a9b4-6585216af6ed', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('3e39abde-d1f9-41d7-a9b4-6585216af6ed', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('3e39abde-d1f9-41d7-a9b4-6585216af6ed', foundational, translation_revisable_by_scholarship).
narrative_ontology:cs_axiom_status(translation_revisable_by_scholarship, holdable).
narrative_ontology:cs_axiom_grounding('3e39abde-d1f9-41d7-a9b4-6585216af6ed', translation_revisable_by_scholarship, empirically_contingent).
narrative_ontology:cs_axiom('3e39abde-d1f9-41d7-a9b4-6585216af6ed', secondary, scholarly_authority_legitimate).
narrative_ontology:cs_axiom_status(scholarly_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3e39abde-d1f9-41d7-a9b4-6585216af6ed', scholarly_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('3e39abde-d1f9-41d7-a9b4-6585216af6ed', scholarly_translation_standard).
narrative_ontology:cs_drift_state('3e39abde-d1f9-41d7-a9b4-6585216af6ed', contemporary_scholarly_consensus, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3e39abde-d1f9-41d7-a9b4-6585216af6ed', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, bible_publishing_industry).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, mainline_denominations).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, general_christian_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, general_christian_consumers).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, textual_criticism_improves_translation).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, scholarly_consensus_legitimate_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce critical editions, translate modern versions, and set textual standards through peer review and academic institutions. Their authority derives from methodological rigor and manuscript evidence. They gain professional standing and institutional influence from the revisable translation norm.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars, agenda_setter,
    organized, generational, mobile, global).

% Control copyright, production, and distribution of modern translations (NIV, ESV, NLT, etc.). Profit from recurring revision cycles and new edition releases. The revisable translation norm creates a sustainable market for updated products. Consumers pay retail prices; publishers capture the margin.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, bible_publishing_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Adopt and authorize modern translations for liturgy and education. Gain congregational relevance and scholarly credibility. Denominational committees select translations, effectively setting default options for millions of adherents.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, mainline_denominations, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, mainline_denominations, agenda_setter).

% Purchase Bibles for personal and church use. Benefit from clearer, more accurate translations based on better manuscripts. Pay retail prices that include publisher margins and translation licensing fees. Can choose among many versions (low suppression), but must buy some version.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, general_christian_consumers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, general_christian_consumers, beneficiary).

% Hold the exclusive_inspiration_reading: KJV is divinely preserved and inerrant. Their position is structurally excluded by the revisable reading's premises (manuscript evidence trumps tradition). They maintain separate institutions, publishers, and educational networks. Exit from their position would require abandoning core identity commitments.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_only_adherents, excluded,
    organized, generational, identity_locked, global).

% Study manuscript transmission as a historical/philological discipline without theological commitment. Provide independent verification of textual claims. Their work undergirds the scholarly consensus but they do not participate in the ecclesiastical authority structure.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, textual_critics_outside_faith, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of providing Christians with translations that accurately reflect the best available manuscript evidence and linguistic understanding, replacing the single fixed 1611 text with a self-correcting scholarly process.
% TRANSFER_FUNCTION: Moves money from Bible purchasers (consumers, churches) to publishers and translation license-holders; moves epistemic authority from ecclesiastical tradition to academic textual criticism; moves cultural capital from KJV-only communities to mainline/evangelical institutions using modern versions.
% ABSENT_VOICES: KJV-only adherents are structurally excluded — their reading is logically foreclosed by the revisable translation premise. They would object that manuscript evidence is subordinate to divine preservation, but they have no seat in the scholarly/denominational apparatus that authorizes modern translations. Pre-1881 KJV-only tradition has no living institutional representation in the revisable framework.
% DISAPPEARANCE_RATIONALE: If the revisable translation norm vanished, the publishing industry would lose its revision-cycle revenue model; denominations would revert to a single authorized text (likely KJV or a frozen modern version); academic biblical scholars would lose their gatekeeping role over translation; consumers would lose meaningful choice among versions. The entire modern Bible economy would reorganize around a fixed text.
% FOUNDING_PROBLEM: By the late 19th century, the KJV's textual basis (Textus Receptus) was known to be inferior to newly discovered manuscripts (Sinaiticus, Vaticanus, papyri), and its English had become archaic. The Revised Version (1881) and subsequent translations were built to solve: how to give English readers a Bible reflecting the best manuscript evidence and current language.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the prefaces of every major modern translation (RV, ASV, RSV, NIV, ESV, etc.), by the formation of the International Greek New Testament Project, and by the near-universal adoption of critical texts (NA/UBS) in seminaries across theological spectra — including conservative evangelical institutions that reject KJV-onlyism. No significant scholarly body disputes that the manuscript situation has improved since 1611.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).
:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects publisher margins on copyrighted translations and the recurring revenue model of periodic revisions — consumers pay repeatedly for updated editions. Suppression (0.15) is low because the KJV is never banned; alternatives proliferate. Theater ratio (0.12) is low because scholarly work is genuine: critical editions are produced, manuscripts are collated, translations are peer-reviewed. Accessibility collapse (0.25) is modest: the KJV remains fully accessible, but its cultural dominance has collapsed. Resistance (0.45) is moderate: KJV-only movement persists as a distinct subculture with its own institutions, rejecting the scholarly premise.
 *
 * PERSPECTIVAL GAP:
 *   From the scholar/denomination seat, this is a rope: genuine coordination around better texts, low coercion, net benefit. From the consumer seat, it's a rope with a toll: better product but recurring cost. From the KJV-only seat (excluded), the revisable reading is a snare that displaced their text — but they are not a seated stakeholder in this constraint. The engine will compute per-seat types from the structural data; the gap between agenda_setter (rope) and payer (tangled_rope if extraction felt acutely) is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars and denominational committees are agenda_setters (they define standards and authorize translations) with institutional power and generational horizons — directionality near beneficiary end (d ≈ 0.15). Publishers are beneficiaries with arbitrage-grade exit (they can pivot to other markets) — d ≈ 0.1. Consumers are payers (they fund the system) but also beneficiaries (they get better translations) with mobile exit (can choose any version or none) — d ≈ 0.5. KJV-only adherents are excluded with identity_locked exit — they cannot adopt the revisable reading without identity rupture; their directionality is not computed by this reading's engine since they're not seated in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (KJV's archaic language and inferior manuscript base) remains live: manuscripts continue to be discovered (e.g., P.Oxy. 5345, 2019), language continues to evolve, and textual criticism continues to refine the critical text. The revisable reading has not become a piton — it still solves a live coordination problem. However, the publishing industry's extraction layer (copyrighted translations, revision cycles driven by market rather than manuscript discoveries) shows mandatrophy signals: the coordination function (accurate text) could be served by public-domain translations (e.g., NET, Berean, WEB), but the institutional apparatus favors proprietary versions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the revisable_translation_reading a single coherent constraint, or does it conflate distinct claims (textual criticism improves translations / scholars are legitimate arbiters / publishers should control modern versions)?',
    'Decompose per the ε-invariance principle: if measuring extractiveness via publisher margins yields high ε but measuring via textual accuracy yields low ε, split into separate constraint stories (e.g., scholarly_textual_standard vs. proprietary_translation_market) linked via network.affects_constraints.',
    'If decomposed, the scholarly coordination function may classify as rope (low extraction) while the publishing market classifies as tangled_rope (coordination + extraction). The current single story risks masking the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the revisable reading is one constraint or a family requiring decomposition.').

omega_variable(
    publisher_extraction_ambiguity,
    'Does the publishing industry''s profit from modern translations constitute extraction from consumers, or the coordination cost of producing and distributing reliable translations?',
    'Compare publisher margins on copyrighted translations (NIV, ESV) vs. public-domain equivalents (NET, WEB, Berean). If margins significantly exceed production/distribution costs, the surplus is extraction. Regulatory or market data on Bible publishing economics would resolve.',
    'If extraction, the constraint is tangled_rope (coordination + asymmetric extraction). If coordination cost, it remains rope. Affects claimed_type and mandatrophy assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(publisher_extraction_ambiguity, empirical, 'Whether publisher revenue represents rent extraction or necessary coordination overhead.').

omega_variable(
    kjv_only_victim_status,
    'Are KJV-only adherents victims of the revisable reading''s dominance (cultural displacement, loss of institutional support), or merely excluded participants in a pluralistic market?',
    'Trace institutional history: did mainline denominations actively suppress KJV-only institutions (seminary closures, ordination requirements, missionary society policies), or did KJV-only groups voluntarily separate? Documentary evidence from 1880-1950 denominational records.',
    'If active suppression occurred, KJV-only adherents are victims and the revisable reading''s suppression score is understated. If voluntary separation, they are excluded but not victimized by this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kjv_only_victim_status, empirical, 'Whether KJV-only cultural displacement constitutes victimization under the revisable reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression (0.15) structural (genuine market pluralism) or partially internalized (KJV-only adherents believe their position is indefensible in scholarly terms)?',
    'Post-exit suppression trajectory: if KJV-only adherents maintain high confidence and institutional vitality despite scholarly consensus, suppression is structural (they''re not internally suppressed). If they show identity erosion, internalized suppression may be present.',
    'If internalized, effective suppression is higher than measured — the constraint''s cultural dominance operates partly through epistemic marginalization, not just market forces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded KJV-only adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 1881, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_revisable_tr_t1881, kjv_text_1611__revisable_translation_reading, theater_ratio, 1881, 0.05).
narrative_ontology:measurement(kjv_revisable_tr_t1901, kjv_text_1611__revisable_translation_reading, theater_ratio, 1901, 0.07).
narrative_ontology:measurement(kjv_revisable_tr_t1952, kjv_text_1611__revisable_translation_reading, theater_ratio, 1952, 0.08).
narrative_ontology:measurement(kjv_revisable_tr_t1978, kjv_text_1611__revisable_translation_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(kjv_revisable_tr_t1995, kjv_text_1611__revisable_translation_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(kjv_revisable_tr_t2011, kjv_text_1611__revisable_translation_reading, theater_ratio, 2011, 0.12).
narrative_ontology:measurement(kjv_revisable_tr_t2025, kjv_text_1611__revisable_translation_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(kjv_revisable_be_t1881, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1881, 0.15).
narrative_ontology:measurement(kjv_revisable_be_t1901, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1901, 0.18).
narrative_ontology:measurement(kjv_revisable_be_t1952, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1952, 0.22).
narrative_ontology:measurement(kjv_revisable_be_t1978, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(kjv_revisable_be_t1995, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement(kjv_revisable_be_t2011, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2011, 0.33).
narrative_ontology:measurement(kjv_revisable_be_t2025, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(kjv_revisable_su_t1881, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1881, 0.3).
narrative_ontology:measurement(kjv_revisable_su_t1901, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1901, 0.25).
narrative_ontology:measurement(kjv_revisable_su_t1952, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1952, 0.2).
narrative_ontology:measurement(kjv_revisable_su_t1978, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(kjv_revisable_su_t1995, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement(kjv_revisable_su_t2011, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2011, 0.13).
narrative_ontology:measurement(kjv_revisable_su_t2025, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__revisable_translation_reading, 0.02).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, modern_translation_copyright_regime).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, biblical_textual_criticism_standard).

% DUAL FORMULATION NOTE:
% The kjv_text_1611 kernel decomposes into three readings with distinct ε values: exclusive_inspiration_reading (ε ≈ 0.05, mountain-claimed but FSM candidate), functional_equivalence_reading (ε ≈ 0.2, rope), revisable_translation_reading (ε ≈ 0.35, rope/tangled_rope). The revisable reading's scholarly authority claim structurally enables the proprietary translation market (modern_translation_copyright_regime). The functional_equivalence reading coexists by accepting the revisable reading's scholarly premise but adding a literary/historical valuation of the KJV.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, organized, 0.15).
constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, institutional, 0.1).
constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
