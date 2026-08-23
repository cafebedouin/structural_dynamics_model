% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Thinkability of IP as Ownable Expression (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story captures the thinkability_reading of the ip_category_emergence
 *   kernel: the claim that 1710 marks the moment when 'ownable expression'
 *   became a legally coherent category — a conceptual point in normative
 *   space that did not exist before. Pre-1710 disputes (e.g., Millar v
 *   Taylor, Donaldson v Beckett) lacked the vocabulary of 'intellectual
 *   property'; they fought over guild privilege, royal prerogative, and
 *   common-law literary property as a natural right. The 1710 Statute of Anne
 *   deployed 'copy right' as a distinct statutory category, not a guild
 *   privilege. This reading emphasizes the *gain of a conceptual coordinate*
 *   — the category itself — rather than a transfer of holdings
 *   (first_holding_reading) or a framing artifact
 *   (synchronic_diachronic_seam). The constraint is claimed as mountain
 *   because the category's emergence is treated as a historical fact: once
 *   the conceptual space has the point, it cannot be un-gained. But
 *   beneficiaries exist (legal professionals, publishers, state), triggering
 *   FSM evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.12).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.08).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Thinkability of IP as Ownable Expression (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'f15f6da8-4a11-491c-ad75-61123b21dcdd').
narrative_ontology:cs_kernel_codification('f15f6da8-4a11-491c-ad75-61123b21dcdd', formalized).
narrative_ontology:cs_authority_grounding('f15f6da8-4a11-491c-ad75-61123b21dcdd', lineage).
narrative_ontology:cs_interpretation_layer_present('f15f6da8-4a11-491c-ad75-61123b21dcdd').
narrative_ontology:cs_reading_relation('f15f6da8-4a11-491c-ad75-61123b21dcdd', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('f15f6da8-4a11-491c-ad75-61123b21dcdd', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('f15f6da8-4a11-491c-ad75-61123b21dcdd', foundational, conceptual_space_gained_point_1710).
narrative_ontology:cs_axiom_status(conceptual_space_gained_point_1710, holdable).
narrative_ontology:cs_axiom_grounding('f15f6da8-4a11-491c-ad75-61123b21dcdd', conceptual_space_gained_point_1710, conventional).
narrative_ontology:cs_axiom('f15f6da8-4a11-491c-ad75-61123b21dcdd', secondary, pre_1710_disputes_lacked_ip_vocabulary).
narrative_ontology:cs_axiom_status(pre_1710_disputes_lacked_ip_vocabulary, holdable).
narrative_ontology:cs_axiom_grounding('f15f6da8-4a11-491c-ad75-61123b21dcdd', pre_1710_disputes_lacked_ip_vocabulary, empirically_contingent).
narrative_ontology:cs_reference_frame('f15f6da8-4a11-491c-ad75-61123b21dcdd', pre_statutory_privilege_regime).
narrative_ontology:cs_drift_state('f15f6da8-4a11-491c-ad75-61123b21dcdd', contemporary_digital_copyright_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f15f6da8-4a11-491c-ad75-61123b21dcdd', '2026-08-10T14:30:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_professionals).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publishers_guild).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, state_chancery).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, authors).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, readers_public).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, expression_has_ontological_weight).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, authorship_creates_proprietary_interest).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, common_law_accommodates_intangible_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a new doctrinal category to litigate, advise upon, and build professional authority around. The emergence of 'literary property' creates billable expertise where none existed. Exit means abandoning a growing practice area; arbitrage-grade exit is available by pivoting to adjacent fields.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_professionals, beneficiary,
    institutional, generational, arbitrage, national).

% The Stationers' Company and successor publisher coalitions acquire a legally enforceable vocabulary to replace expiring royal printing privileges. They can now claim 'property' rather than 'privilege' — a stronger, more durable, and more transferable right. Exit from the new category is constrained because their business model reorganizes around it.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, publishers_guild, beneficiary,
    organized, biographical, constrained, national).

% The Court of Chancery and parliamentary legislature acquire a new jurisdictional hook: equity can now police 'literary property' as a species of intangible asset. This expands chancery's reach without new statute. The state does not 'exit' its own jurisdictional expansions.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, state_chancery, agenda_setter,
    institutional, generational, analytical, national).

% Writers gain a conceptual foothold to claim ownership of their works apart from patron or printer. But they also bear the cost of enforcing a right that exists only in courts they cannot easily access. Mobile exit: they can write without claiming the right, or publish anonymously.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, authors, payer).

% The reading public faces higher prices and restricted circulation as the new category enables publisher monopolies over 'copy right'. They have no organized voice in 1710 and no exit from the print market. Trapped by literacy needs and no alternative distribution.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, readers_public, payer,
    powerless, immediate, trapped, national).

% Analyze the category emergence from outside the constraint. They do not collect rents or bear costs from the 1710 deployment; they trace its genealogical consequences for modern IP systems.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable conceptual category — 'literary property' / 'copy right' — that allows courts, publishers, and authors to coordinate expectations about control over printed expression without relying on ad hoc royal privileges or guild customs. Solves the coordination problem of 'what counts as a protectable work' and 'who may authorize reproduction'.
% TRANSFER_FUNCTION: Moves control over reproduction decisions from the Crown/guild privilege system (where the Stationers' Company held a monopoly on printing) to a property-like right vested notionally in authors but practically exercised by publishers through assignment. The transfer is from privilege-holders to property-holders; the public pays via restricted access.
% ABSENT_VOICES: The reading public (trapped payers) and dissenting jurists who argued literary property was a common-law fiction (e.g., Lord Camden's later dissent in Donaldson v Beckett) were not in the room when the 1710 Act was drafted. The Act's preamble centers 'encouragement of learned men', not public access.
% DISAPPEARANCE_RATIONALE: If the thinkability of 'ownable expression' vanished overnight, the entire edifice of modern copyright — statutory regimes, international treaties, digital rights management, fair use doctrines — would lose its conceptual foundation. Courts would revert to privilege-based or contract-only regulation of printing. The world of cultural production would rearrange fundamentally.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual printing monopoly (guaranteed by royal charter) expired in 1695. The ensuing chaos — unregulated printing, authorial destitution, publisher infighting — created demand for a new legal category that could replace guild privilege with a property right enforceable in common law courts.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — replacing the expired Stationers' monopoly — is historically dead (the guild system never returned). Legal historians (Rose, Patterson, Deazley) corroborate from outside the beneficiary set that the 1710 Act solved a specific historical crisis, not a perennial coordination need. The beneficiaries (publishers, legal profession) continue to invoke 'encouragement of authors' as a live problem, but the original crisis is gone.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__thinkability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low at the origin (0.08) because the 1710 Act was narrow: 14+14 years, registration required, no performance/display rights. The measurement series shows extraction accumulating over three centuries as the category expanded (term extensions, subject matter expansion, digital enforcement). Theater ratio rises as performative 'author encouragement' rhetoric persists while actual extraction shifts to corporate rightsholders and platform intermediaries. Suppression requirement escalates from minimal (common-law courts policing a few printers) to maximal (global DRM, notice-and-takedown, border enforcement). The 1710 snapshot shows mountain-like metrics; the 2024 snapshot shows snare-like metrics. This is the T17 accumulation trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the legal professional / state chancery seat, the category emergence is a mountain: a conceptual fact that organizes law. From the reader_public seat, the same category's *consequences* are extractive and suppressive — but the category's thinkability itself is not what extracts; it's the institutional apparatus built *on* the category. The engine will compute per-seat types from this structural asymmetry. The claimed_type (mountain) reflects the 1710 snapshot; the measurements show the constraint's *operation* drifting toward extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal professionals and state chancery are structural beneficiaries: they gain jurisdiction, doctrine, and professional authority without bearing enforcement costs. Publishers_guild are beneficiaries who reorganize their business model around the new category — constrained exit because the category becomes the industry's foundation. Authors are dual-positioned: they gain a conceptual claim (beneficiary) but bear enforcement costs they cannot afford (payer) — mobile exit via anonymity or non-publication. Readers_public are pure payers: trapped by literacy needs, no organized voice, extraction via monopoly pricing. Legal_historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (replacing the Stationers' monopoly) is dead — the guild system is gone. Yet the category persists and expands. This is mandatrophy: a coordination solution (replace privilege with property) that outlived its problem and became an extraction platform. The thinkability_reading captures the *origin* of the category; the accumulation trajectory shows the mandatrophy. The FSM candidate status (mountain with beneficiaries) is deliberate: the category presents as natural/conceptual law but identifiable beneficiaries exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_category_vs_constructed_tool,
    'Is the ''thinkability of ownable expression'' a discovered conceptual necessity (like a mathematical object) or a constructed legal tool that serves identifiable interests?',
    'Counterfactual legal history: if the Stationers'' monopoly had been renewed in 1695, would the category still have emerged? If yes, it tracks a natural conceptual need; if no, it is a contingent institutional invention.',
    'If natural, the mountain claim holds and FSM is a false positive; if constructed, the beneficiaries are the architects and the category is a tangled_rope or snare from origin. FSM detection would be validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_category_vs_constructed_tool, conceptual, 'Whether the IP category is a natural conceptual coordinate or a constructed instrument').

omega_variable(
    category_vs_regime_conflation,
    'Does the extraction measured in the 2024 snapshot belong to the *category itself* (thinkability) or to the *regime built atop it* (term extensions, DMCA, TRIPS)?',
    'Decompose the constraint: separate ''the category exists'' (this story) from ''the regime operates thus'' (descendant constraints in the network). Measure extraction at each layer.',
    'If extraction belongs to the regime, this story''s mountain claim at t=1710 is valid and the accumulation is downstream contamination. If the category *inherently* generates the regime, the mountain claim fails at origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_vs_regime_conflation, conceptual, 'Whether extraction accrues to the category or its institutional superstructure').

omega_variable(
    reading_relations_foreclosure_test,
    'Does the thinkability_reading''s core premise (category emergence at 1710) logically foreclose the first_holding_reading (author-as-holder emergence at 1710) within a single framework?',
    'Test whether a single legal framework can simultaneously hold: (a) the category ''literary property'' emerged in 1710, AND (b) the author became a legitimate rights-holder in 1710. If both can be true in one framework, they coexist; if (a) makes (b) incoherent or vice versa, foreclosure.',
    'If forecloses, the kernel has mutually exclusive readings — only one can be the ''true'' structural move. If coexists_with, the kernel sustains a genuine interpretive dispute. The synchronic_diachronic_seam reading predicts coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_foreclosure_test, conceptual, 'Structural relationship between thinkability and first-holding readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.02).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.03).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1842, ip_category_emergence__thinkability_reading, theater_ratio, 1842, 0.05).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1911, ip_category_emergence__thinkability_reading, theater_ratio, 1911, 0.08).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1976, ip_category_emergence__thinkability_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t1998, ip_category_emergence__thinkability_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(ip_cat_emerg_think_tr_t2024, ip_category_emergence__thinkability_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(ip_cat_emerg_think_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.08).
narrative_ontology:measurement(ip_cat_emerg_think_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.15).
narrative_ontology:measurement(ip_cat_emerg_think_be_t1842, ip_category_emergence__thinkability_reading, base_extractiveness, 1842, 0.22).
narrative_ontology:measurement(ip_cat_emerg_think_be_t1911, ip_category_emergence__thinkability_reading, base_extractiveness, 1911, 0.35).
narrative_ontology:measurement(ip_cat_emerg_think_be_t1976, ip_category_emergence__thinkability_reading, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(ip_cat_emerg_think_be_t1998, ip_category_emergence__thinkability_reading, base_extractiveness, 1998, 0.62).
narrative_ontology:measurement(ip_cat_emerg_think_be_t2024, ip_category_emergence__thinkability_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ip_cat_emerg_think_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.05).
narrative_ontology:measurement(ip_cat_emerg_think_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.12).
narrative_ontology:measurement(ip_cat_emerg_think_su_t1842, ip_category_emergence__thinkability_reading, suppression_requirement, 1842, 0.25).
narrative_ontology:measurement(ip_cat_emerg_think_su_t1911, ip_category_emergence__thinkability_reading, suppression_requirement, 1911, 0.42).
narrative_ontology:measurement(ip_cat_emerg_think_su_t1976, ip_category_emergence__thinkability_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(ip_cat_emerg_think_su_t1998, ip_category_emergence__thinkability_reading, suppression_requirement, 1998, 0.72).
narrative_ontology:measurement(ip_cat_emerg_think_su_t2024, ip_category_emergence__thinkability_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.02).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, statutory_copyright_expansion_1842).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, berne_convention_1886).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, digital_millennium_copyright_act_1998).

% DUAL FORMULATION NOTE:
% Kernel ip_category_emergence decomposes into three readings: thinkability_reading (category emergence), first_holding_reading (author-as-holder emergence), synchronic_diachronic_seam (independence/framing artifact). All three share the 1710 origin but differ on what structurally emerged. This reading emphasizes the conceptual coordinate; first_holding emphasizes the claimant set; seam reading denies the distinction. Network edges reflect downstream contamination: the category enables the regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, institutional, 0.15).
constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, organized, 0.25).
constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, moderate, 0.45).
constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
