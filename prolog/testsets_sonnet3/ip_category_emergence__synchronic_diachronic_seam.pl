% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic/Diachronic Seam Test for IP Category Emergence (M4/M5 Collapse Test)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the third reading of the ip_category_emergence
 *   kernel: rather than asserting that 1710 marks category emergence
 *   (thinkability_reading) or occupancy change (first_holding_reading), it
 *   tests whether these two claims are formally independent (M4) or collapse
 *   into a single event under temporal reframing (M5). The test itself
 *   becomes a doctrinal instrument — its administration by formalist jurists
 *   and its selective invocation by modern litigators constitute a
 *   coordination/extraction hybrid: the coordination function is genuine
 *   (someone must adjudicate whether the kernel's structure is authentic or
 *   spurious), but the adjudication outcome systematically advantages parties
 *   positioned to invoke whichever collapse result they need, at the expense
 *   of historically voiceless claimant populations and public-domain
 *   advocates whose arguments depend on axis-separability persisting as a
 *   live possibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.58).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.42).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic/Diachronic Seam Test for IP Category Emergence (M4/M5 Collapse Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '7cbdba73-19d5-4aa5-bc95-c931022fbd7f').
narrative_ontology:cs_kernel_codification('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', distributed).
narrative_ontology:cs_authority_grounding('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', expertise).
narrative_ontology:cs_interpretation_layer_present('7cbdba73-19d5-4aa5-bc95-c931022fbd7f').
narrative_ontology:cs_reading_relation('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', foundational, category_emergence_and_occupancy_change_are_formally_severable_claims).
narrative_ontology:cs_axiom_status(category_emergence_and_occupancy_change_are_formally_severable_claims, holdable).
narrative_ontology:cs_axiom_grounding('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', category_emergence_and_occupancy_change_are_formally_severable_claims, conventional).
narrative_ontology:cs_axiom('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', secondary, temporal_framing_can_generate_spurious_independence_between_co_occurring_events).
narrative_ontology:cs_axiom_status(temporal_framing_can_generate_spurious_independence_between_co_occurring_events, holdable).
narrative_ontology:cs_axiom_grounding('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', temporal_framing_can_generate_spurious_independence_between_co_occurring_events, empirically_contingent).
narrative_ontology:cs_reference_frame('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', bipartite_kernel_structure_hypothesis).
narrative_ontology:cs_drift_state('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', contemporary_doctrinal_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cbdba73-19d5-4aa5-bc95-c931022fbd7f', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, doctrinal_formalists_seeking_clean_kernel_tests).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, modern_ip_maximalist_litigators).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, historical_authors_precluded_from_1710_claimant_set).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, public_domain_advocates_relying_on_thin_originality_history).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, ip_kernel_structural_authenticity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal theorists and appellate jurists who want a decision procedure that determines whether 'IP came into existence' and 'someone first held IP' are the same event or two logically severable claims. They construct and administer the M4/M5 collapse test, deciding which historical facts count as evidence for independence versus artifact, and their scholarly and judicial authority is enhanced whichever way the test resolves, since either answer becomes a citable structural finding.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, doctrinal_formalists_seeking_clean_kernel_tests, agenda_setter,
    institutional, civilizational, analytical, national).

% Contemporary counsel and rights-holding entities who benefit from whichever collapse-test outcome yields the most expansive account of when protectable interests can be said to have existed. They do not administer the test but selectively cite its results to argue for earlier or broader claimant sets in disputes over authorship priority, work-for-hire timing, and retroactive protection scope.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, modern_ip_maximalist_litigators, beneficiary,
    organized, biographical, arbitrage, national).

% Historical figures (deceased, cannot participate) whose works predate or straddle 1710 and whose status as rights-holders is retroactively adjudicated by whichever answer the seam test yields. If the test collapses thinkability and first-holding into one event, their exclusion from the legitimate claimant set becomes doctrinally overdetermined rather than a contingent historical accident open to reinterpretation; they cannot contest a classification that operates on facts about them long after the fact.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_authors_precluded_from_1710_claimant_set, payer,
    powerless, civilizational, trapped, national).

% Advocates and scholars who argue for narrow, historically contingent readings of when protectable categories emerged, so as to keep public domain boundaries wide. If the seam test resolves toward artifact (the two axes always co-occur, so there was never a real independence to exploit), their argument that occupancy and category emergence can be pulled apart to justify narrower retroactive claims loses its evidentiary basis, constraining the doctrinal moves available to them in ongoing term-extension and orphan-works litigation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, public_domain_advocates_relying_on_thin_originality_history, payer,
    moderate, generational, constrained, national).

% The actual historical parties present at the 1710 Statute of Anne moment, whose contemporaneous understanding of whether a new legal category was emerging versus whether an existing occupancy right was merely being formalized would bear directly on the seam question, but who left no systematic testimony addressing exactly this distinction and cannot be consulted.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, eighteenth_century_publishers_and_stationers, excluded,
    powerless, civilizational, trapped, national).

% Scholars who examine parallel category-emergence moments across jurisdictions (droit d'auteur, patent priority regimes) to test whether the thinkability/first-holding split is a general structural feature of legal category formation or an artifact specific to how Anglo-American IP historiography frames 1710.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, doctrinal_formalists_seeking_clean_kernel_tests).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared analytical procedure (the M4/M5 collapse test) that lets courts, scholars, and litigants determine whether two candidate readings of the same historical moment — 'a new ownable category became thinkable' and 'someone first legitimately held the resulting right' — are logically independent claims or the same claim described twice under different temporal framing. This resolves otherwise endless disputes about which reading of 1710 should govern a given doctrinal question.
% TRANSFER_FUNCTION: Moves interpretive authority over historical IP claimant status from open historical contestation toward whichever institutional body administers and certifies the collapse test's result; downstream, it moves litigation leverage toward parties who can invoke the test's finding (formalists and modern maximalists) and away from parties whose claims depend on the two axes remaining separable or contingent (public domain advocates, and retroactively, historical authors who fall on the wrong side of whichever line the test draws).
% ABSENT_VOICES: The eighteenth-century publishers, stationers, and unrecognized authors who lived through the actual 1710 moment cannot speak to whether they experienced it as one event or two, and no contemporaneous source disambiguates this for them. Historical authors excluded from the claimant set have no standing in a test conducted three centuries after the fact about their own legal status.
% DISAPPEARANCE_RATIONALE: If the collapse test vanished, doctrinal formalists would lose a load-bearing tool for adjudicating retroactive-claimant disputes, and litigators on both sides of term-extension and originality-threshold cases would lose a citable structural authority; but historians would continue debating the substantive 1710 question by other means (case-by-case historical argument), so whether 'the world rearranges' turns on whether one thinks the formal test is doing real analytical work or merely providing a veneer of rigor over what remains an interpretive judgment call — hence contested rather than settled either way.
% FOUNDING_PROBLEM: Two rival readings of IP category emergence (thinkability vs. first-holding) produced incompatible claimant-set implications, and no procedure existed to determine whether the disagreement reflected a genuine formal independence between the two axes or was merely an artifact of describing one event synchronically versus diachronically.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians attest that structurally analogous seam questions arise in other jurisdictions' category-formation histories and that no consensus exists on whether the axes are genuinely independent; this corroboration comes from outside the beneficiary set (neither formalists administering the test nor litigators invoking its results), but even this outside corroboration is limited to confirming the question is real, not resolving which answer is correct.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial-but-moderate (0.58) because the test's function is genuinely analytical (resolving a real formal ambiguity) even as its downstream deployment in litigation extracts interpretive leverage for whichever party's position the collapse result favors. Theater ratio rises to 0.5 by interval end because as the test becomes institutionally entrenched, an increasing share of its invocation is performative citation-of-authority rather than fresh analytical work — courts and scholars cite 'the M4/M5 test resolved X' as settled rather than re-litigating the formal question each time. Suppression (0.42) reflects that alternative framings (treating the seam question as permanently open, or as irrelevant to doctrine) are not fully foreclosed but are increasingly disfavored once the test achieves citation-authority. Accessibility collapse is moderate (0.4) — historians and litigants can still argue the seam is undecidable, but doing so carries increasing doctrinal cost.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist agenda-setter seat, the test is a piece of rigorous analytical apparatus resolving a genuine ambiguity about kernel structure. From the payer seats — historical authors and public domain advocates — the same apparatus operates as a mechanism that retroactively forecloses argumentative space they depend on, dressed in the language of formal necessity. The engine should compute these as diverging per-seat classifications from the same structural data, which is the point: the seam test's own authority is exactly the kind of thing a seam test cannot adjudicate about itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Doctrinal formalists sit near the beneficiary end: they administer the test and gain scholarly/judicial authority regardless of outcome. Modern IP maximalist litigators are secondary beneficiaries — they do not run the test but harvest its results opportunistically, giving them organized power and arbitrage-grade exit (they can cite the test when favorable and argue procedural irrelevance when not). Historical authors are maximally targeted: trapped by death and by the passage of time, they cannot contest a retroactive classification of their own status. Public domain advocates are targets by a different mechanism — the test's collapse-toward-artifact answer removes an argumentative resource they rely on, even though no party is 'enforcing' anything against them directly; their constrained exit reflects that they must now work within a narrower doctrinal space.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than mountain or rope) prevents two mislabelings: treating the collapse test as pure neutral coordination (which would erase the asymmetric benefit flowing to formalists and maximalist litigators who gain authority/leverage regardless of the test's actual answer) and treating it as pure extraction (which would erase the genuine, non-trivial formal question the test answers — whether category-emergence and occupancy-change really are independent axes is a legitimate structural question about legal kernels, not merely a pretext). The coordination function is real: without some procedure, the kernel's authenticity-versus-spuriousness question would remain permanently undecidable and every downstream doctrinal dispute would re-litigate first principles. The extraction is also real: the procedure's administration and selective invocation systematically transfers interpretive authority away from historically voiceless and public-domain-aligned parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_collapse_undecidability,
    'Is the apparent independence between category-emergence (thinkability) and occupancy-change (first-holding) at 1710 a genuine formal property of the two claims, or does it evaporate once the synchronic description (what became legally possible) and the diachronic description (who first held it) are correctly disambiguated as descriptions of one event?',
    'Comparative analysis across multiple jurisdictions'' category-formation moments (patent priority regimes, droit d''auteur, trade secret doctrine) to test whether thinkability and first-holding vary independently in at least one documented case; if they always co-occur across every tested case, M5 (artifact) is favored, if even one clean counter-case exists, M4 (independence) is favored.',
    'If M4 (independence) is corroborated, the kernel has authentic bipartite structure and the sibling readings (thinkability_reading, first_holding_reading) are genuinely distinct constraints deserving separate doctrinal treatment. If M5 (artifact) is corroborated, the two sibling readings collapse into a single underlying claim described at different temporal grains, undermining the practice of treating them as independently contestable in litigation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_collapse_undecidability, conceptual, 'Whether the kernel''s two-axis structure is authentic or a temporal-framing illusion.').

omega_variable(
    test_administration_capture,
    'Does the institutional body administering the M4/M5 collapse test have a structural interest in one outcome over the other, independent of the test''s actual formal merits?',
    'Track citation patterns and outcome distributions across cases where the test is invoked; if outcomes correlate with litigant power/resources rather than with case-specific historical facts, this suggests capture rather than neutral adjudication.',
    'If capture is confirmed, the tangled_rope classification''s extraction component is understated in this story and the constraint drifts toward snare; if no capture is found, the coordination function is more dominant than the current metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(test_administration_capture, empirical, 'Whether the test''s administering institutions have outcome-independent neutrality.').

omega_variable(
    retroactive_standing_problem,
    'Can a formal test about the structure of a historical legal kernel legitimately determine present-day claimant status for parties (historical authors) who have no way to contest the test''s application to facts about them?',
    'This is a normative/procedural question about retroactive adjudication rather than an empirical one; resolution depends on jurisprudential commitments about whether historical facts can be settled by present-day formal analysis without due process for the historically implicated parties.',
    'If retroactive standing is held illegitimate regardless of the test''s formal correctness, the extraction from historical authors is irreducible and the constraint cannot be fully rehabilitated by getting M4/M5 ''right.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_standing_problem, preference, 'Whether retroactive formal adjudication of historical claimant status is legitimate at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ip_c_tr_t8, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ip_c_tr_t16, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 16, 0.41).
narrative_ontology:measurement(ip_c_tr_t24, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 24, 0.45).
narrative_ontology:measurement(ip_c_tr_t32, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 32, 0.48).
narrative_ontology:measurement(ip_c_tr_t40, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ip_c_be_t8, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ip_c_be_t16, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(ip_c_be_t24, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(ip_c_be_t32, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(ip_c_be_t40, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__synchronic_diachronic_seam, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__synchronic_diachronic_seam, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, first_holding_reading).

% DUAL FORMULATION NOTE:
% This story is the third member of the ip_category_emergence kernel family. thinkability_reading and first_holding_reading each assert a substantive reading of the 1710 moment; this story tests the meta-question of whether their difference is structurally real (M4) or an artifact of temporal framing (M5). If M5 is ultimately favored, the sibling readings' independent ε values would need reconsideration as potentially describing the same underlying constraint from two temporal angles — but per the ε-invariance principle, that reconsideration belongs in those stories' own files, not folded into this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
