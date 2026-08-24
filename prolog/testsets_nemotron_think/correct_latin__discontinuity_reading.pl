% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Correct Latin as Classical Textual Standard (Discontinuity Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The discontinuity reading of 'correct Latin' declares a sharp rupture
 *   between Classical Latin (the standard) and medieval Latin (corrupt
 *   deviation). It originates in Renaissance humanist invective against
 *   'barbarous' medieval Latin and is institutionalized through the
 *   philological apparatus of textual criticism: critical editions
 *   reconstruct a Classical archetype by emending medieval manuscript
 *   transmission. The constraint coordinates scholarly communication around a
 *   fixed canonical corpus but extracts by excluding medieval Latinity from
 *   legitimacy, delegitimizing living Latin practices, and concentrating
 *   editorial authority in Classical philologists. The measurement series
 *   tracks the constraint's intensification from humanist polemic (1350)
 *   through the age of print and critical editing (1550-1850) to modern
 *   disciplinary consolidation (2025).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.72).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.78).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Correct Latin as Classical Textual Standard (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de').
narrative_ontology:cs_kernel_codification('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', fixed_text).
narrative_ontology:cs_authority_grounding('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', lineage).
narrative_ontology:cs_interpretation_layer_present('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de').
narrative_ontology:cs_reading_relation('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', foundational, classical_texts_as_sole_legitimate_standard).
narrative_ontology:cs_axiom_status(classical_texts_as_sole_legitimate_standard, holdable).
narrative_ontology:cs_axiom_grounding('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', classical_texts_as_sole_legitimate_standard, conventional).
narrative_ontology:cs_axiom('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', foundational, medieval_latin_as_corrupt_deviation).
narrative_ontology:cs_axiom_status(medieval_latin_as_corrupt_deviation, holdable).
narrative_ontology:cs_axiom_grounding('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', medieval_latin_as_corrupt_deviation, conventional).
narrative_ontology:cs_axiom('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', secondary, reconstruction_from_textual_symbols).
narrative_ontology:cs_axiom_status(reconstruction_from_textual_symbols, holdable).
narrative_ontology:cs_axiom_grounding('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', reconstruction_from_textual_symbols, empirically_contingent).
narrative_ontology:cs_reference_frame('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', classical_textual_standard).
narrative_ontology:cs_drift_state('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', contemporary_philological_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a7f1f19-7cb9-4685-a289-a1b8e6d4a9de', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, modern_classicists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_editors).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, living_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_textual_traditions).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_latin_as_fixed_standard).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_reconstruction_over_living_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the Classical Latin standard through textual criticism, critical editions, and academic curricula. Their professional authority derives from exclusive competence in reconstructing the 'pure' Classical form from ancient manuscripts. They control editorial standards, grant funding, and academic appointments.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, classical_philologists, beneficiary).

% Historical architects of the discontinuity: they declared medieval Latin 'barbarous' and launched the program of purifying Latin by returning to Classical models. Their textual searches, editorial principles, and educational reforms created the institutional infrastructure that modern philology inherits. They cannot exit — they are the founding generation.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, renaissance_humanists, agenda_setter,
    institutional, biographical, constrained, continental).

% Inherit the Classical standard as the definition of their discipline. Their research, teaching, and professional identity are organized around the texts and forms validated by the discontinuity reading. They benefit from the clarity and prestige of a fixed canonical corpus. Exit means leaving the field or switching to reception studies.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, modern_classicists, beneficiary,
    organized, biographical, mobile, global).

% Produce the critical editions that materialize the discontinuity: they select manuscripts, emend 'corrupt' medieval transmissions, and reconstruct the Classical archetype. Their editorial choices instantiate the constraint in every published text. They are bound by the methodological consensus of their guild.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_editors, agenda_setter,
    organized, biographical, constrained, global).

% Study and defend medieval Latin as a legitimate, evolved form of the language. Their field is structurally defined as 'post-Classical' and therefore secondary. They must constantly justify their object of study against the presumption that it is deviation. Their exit is constrained by institutional housing (often separate departments) and the legitimacy premium attached to Classical philology.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_scholars, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, medieval_latin_scholars, excluded).

% Use Latin as a living spoken/written language (conventicles, oral pedagogy, neo-Latin composition). Their practice demonstrates continuity of transmission but is dismissed as 'artificial' or 'modern' by the discontinuity standard. Their identity is fused to the language's vitality; exit means abandoning the very continuity they embody.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, living_latin_practitioners, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, living_latin_practitioners, excluded).

% The vast corpus of medieval Latin texts — legal, theological, scientific, literary — that the discontinuity reading classifies as 'corrupt' witnesses to a millennium of continuous Latinity. These texts cannot speak for themselves; their legitimacy is adjudicated by the philological framework that declares them derivative and degenerate.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_textual_traditions, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(correct_latin__discontinuity_reading, medieval_textual_traditions).

% Analyze Latin's historical development as a natural language continuum from Old Latin through Classical to medieval and Romance. They see the discontinuity as a prescriptive ideology, not a linguistic fact. Their seat is analytical: they describe the constraint's operation without being organized by it.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, descriptive_linguists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, stable Latin standard for international scholarly communication, textual editing, and canonical citation across centuries — a shared reference point that does not drift with living usage.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional prestige from medieval Latin traditions (living transmission, diverse regional forms, continuous textual culture) to Classical philologists and textual editors who control the reconstructed standard. The transfer runs through editorial practice: every critical edition that emends medieval transmission toward a Classical archetype enacts the extraction.
% ABSENT_VOICES: Medieval scribes, authors, and readers who transmitted and transformed Latin continuously for a millennium — they are the excluded originators of the very texts philologists edit. Also absent: the Romance languages themselves, which are living proof of Latin's continuous evolution but are structurally separated from 'Latin proper' by the discontinuity.
% DISAPPEARANCE_RATIONALE: If the discontinuity constraint vanished, medieval Latin would be reintegrated as a legitimate phase of Latinity; textual editing would shift from reconstructing a lost archetype to documenting a continuous manuscript tradition; living Latin movements would gain institutional parity; the Classical/medieval boundary would become a scholarly convention rather than an ontological rupture.
% FOUNDING_PROBLEM: Renaissance humanists encountered a Latin they judged barbarous — bureaucratic, scholastic, syntactically degraded — and needed a pure standard for their cultural program of reviving antiquity. The discontinuity reading solved this by positing a Golden Age Latin preserved only in ancient texts, making the medieval millennium a parenthesis of corruption.
% FOUNDING_PROBLEM_CORROBORATION: Humanist correspondence (Petrarch, Valla, Erasmus) attests the founding judgment of medieval Latin as corrupt. Modern codicology and palaeography (e.g., Bischoff, Reynolds & Wilson) corroborate that medieval manuscripts show systematic linguistic evolution, not random corruption — the 'problem' was a stylistic judgment, not a textual fact. No corroboration exists outside the humanist/philological tradition for the claim that medieval Latin is structurally deviant rather than evolved.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers epistemic authority from a millennium of continuous Latin transmission to a reconstructed standard controlled by a specialist guild. Suppression (0.78) is higher still because the constraint's persistence depends on active editorial emendation, curricular exclusion, and the structural separation of 'Classical' from 'medieval' philology. Theater ratio (0.45) reflects that the philological apparatus (stemmatics, emendation, critical apparatus) is technically real but increasingly serves to enforce the rupture rather than discover it. Accessibility collapse (0.82) is near-mountain level: once the discontinuity premise is accepted, medieval forms cannot be legitimate without contradicting the constraint's core axiom. Resistance (0.55) is moderate: medievalists, neo-Latinists, and living Latin communities contest the exclusion but operate from structurally disadvantaged positions.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist's seat, the constraint is genuine coordination: a stable standard enables cumulative scholarship. From the medievalist's seat, it is enforced exclusion: their object of study is defined as derivative. From the living Latin practitioner's seat, it is identity suppression: their vitality proves the continuity the constraint forbids. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and textual editors are structural beneficiaries (d ~ 0.15): they control the standard, collect the prestige, and their professional existence depends on the constraint. Renaissance humanists are founding agenda-setters (d ~ 0.1) — they cannot exit the constraint they created. Modern classicists are beneficiaries with mobile exit (d ~ 0.25): they inherit the standard but could shift to reception studies. Medieval Latin scholars are payers with constrained exit (d ~ 0.8): their field is defined by the constraint's exclusion. Living Latin practitioners are payers with identity-locked exit (d ~ 0.9): their practice embodies the continuity the constraint denies. Medieval textual traditions are trapped non-agent payers (d = 1.0): they cannot advocate for themselves. Descriptive linguists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Renaissance need for a pure Classical standard) is contested: humanists attest it was real; codicology shows medieval Latin was evolved, not corrupt. The constraint persists because it now coordinates a global scholarly infrastructure (editions, curricula, citations) that would be costly to reorganize — but the coordination function is inseparable from the extraction that privileges Classical philology over medieval Latinity. This is tangled_rope, not pure snare, because the standard genuinely enables cross-temporal scholarly communication; it is not pure rope because the coordination is achieved by declaring a living tradition illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_vs_evolution,
    'Is the Classical/medieval Latin boundary a genuine linguistic rupture or a prescriptive ideology imposed on a continuous evolution?',
    'Comparative analysis of manuscript transmission chains: if medieval manuscripts show gradual, systematic evolution from Classical models without intervention, the rupture is ideological; if they show abrupt degradation requiring external reconstruction, the rupture has empirical grounding.',
    'If ideological, the constraint is a snare disguised as coordination; if empirical, the coordination function has a genuine linguistic basis and the extraction is the price of a real standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_vs_evolution, empirical, 'Whether the declared discontinuity corresponds to a linguistic fact or a humanist judgment.').

omega_variable(
    editorial_agency_in_reconstruction,
    'How much of the ''reconstructed Classical Latin'' is discovered in the texts versus imposed by editorial conventions?',
    'History of critical editions: track how emendation principles change across generations (e.g., Lachmannian stemmatics vs. best-text editing vs. digital collation) and whether the ''Classical archetype'' converges or diverges.',
    'If the reconstructed standard shifts with editorial fashion, the constraint''s coordination function is partly theatrical — the standard is a moving target stabilized by institutional consensus, not textual evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(editorial_agency_in_reconstruction, conceptual, 'The degree to which the constraint''s coordination object is editorially constructed.').

omega_variable(
    living_latin_as_counterevidence,
    'Does the modern living Latin movement (spoken Latin, neo-Latin composition) falsify the discontinuity claim by demonstrating continuous transmitability?',
    'Sociolinguistic study of living Latin communities: if they achieve fluency and creative capacity using medieval and Renaissance Latin as models, the claim that only ancient texts preserve ''correct'' Latin is empirically challenged.',
    'If living Latin demonstrates continuity, the constraint''s accessibility_collapse is artificially maintained by institutional suppression rather than linguistic necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_latin_as_counterevidence, empirical, 'Whether contemporary practice undermines the discontinuity''s empirical premise.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the discontinuity reading''s core premise (rupture) logically foreclose the continuity reading in any single scholarly framework, or do they coexist as competing paradigms?',
    'Examine whether any scholar or institution simultaneously holds both readings as valid for different purposes (e.g., textual criticism vs. historical sociolinguistics) without contradiction.',
    'If foreclosure holds, the kernel is genuinely contested with no synthesis possible; if coexistence holds, the readings occupy different analytical levels and the kernel''s contestation is structural, not logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between discontinuity and continuity readings of the correct_latin kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1350, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_disc_tr_t1350, correct_latin__discontinuity_reading, theater_ratio, 1350, 0.2).
narrative_ontology:measurement(correct_latin_disc_tr_t1450, correct_latin__discontinuity_reading, theater_ratio, 1450, 0.28).
narrative_ontology:measurement(correct_latin_disc_tr_t1550, correct_latin__discontinuity_reading, theater_ratio, 1550, 0.35).
narrative_ontology:measurement(correct_latin_disc_tr_t1650, correct_latin__discontinuity_reading, theater_ratio, 1650, 0.4).
narrative_ontology:measurement(correct_latin_disc_tr_t1750, correct_latin__discontinuity_reading, theater_ratio, 1750, 0.42).
narrative_ontology:measurement(correct_latin_disc_tr_t1850, correct_latin__discontinuity_reading, theater_ratio, 1850, 0.44).
narrative_ontology:measurement(correct_latin_disc_tr_t1950, correct_latin__discontinuity_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(correct_latin_disc_tr_t2025, correct_latin__discontinuity_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(correct_latin_disc_be_t1350, correct_latin__discontinuity_reading, base_extractiveness, 1350, 0.35).
narrative_ontology:measurement(correct_latin_disc_be_t1450, correct_latin__discontinuity_reading, base_extractiveness, 1450, 0.52).
narrative_ontology:measurement(correct_latin_disc_be_t1550, correct_latin__discontinuity_reading, base_extractiveness, 1550, 0.65).
narrative_ontology:measurement(correct_latin_disc_be_t1650, correct_latin__discontinuity_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(correct_latin_disc_be_t1750, correct_latin__discontinuity_reading, base_extractiveness, 1750, 0.7).
narrative_ontology:measurement(correct_latin_disc_be_t1850, correct_latin__discontinuity_reading, base_extractiveness, 1850, 0.72).
narrative_ontology:measurement(correct_latin_disc_be_t1950, correct_latin__discontinuity_reading, base_extractiveness, 1950, 0.71).
narrative_ontology:measurement(correct_latin_disc_be_t2025, correct_latin__discontinuity_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_disc_su_t1350, correct_latin__discontinuity_reading, suppression_requirement, 1350, 0.4).
narrative_ontology:measurement(correct_latin_disc_su_t1450, correct_latin__discontinuity_reading, suppression_requirement, 1450, 0.55).
narrative_ontology:measurement(correct_latin_disc_su_t1550, correct_latin__discontinuity_reading, suppression_requirement, 1550, 0.68).
narrative_ontology:measurement(correct_latin_disc_su_t1650, correct_latin__discontinuity_reading, suppression_requirement, 1650, 0.72).
narrative_ontology:measurement(correct_latin_disc_su_t1750, correct_latin__discontinuity_reading, suppression_requirement, 1750, 0.75).
narrative_ontology:measurement(correct_latin_disc_su_t1850, correct_latin__discontinuity_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(correct_latin_disc_su_t1950, correct_latin__discontinuity_reading, suppression_requirement, 1950, 0.77).
narrative_ontology:measurement(correct_latin_disc_su_t2025, correct_latin__discontinuity_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.05).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, latin_textual_criticism_standards).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, medieval_latin_legitimacy).

% DUAL FORMULATION NOTE:
% Part of the correct_latin constraint family. The discontinuity reading (this story) declares rupture and external reconstruction; the continuity reading declares living transmission; the hybrid reading declares partial continuity with targeted textual correction. The three readings share the kernel 'correct Latin' but instantiate structurally distinct constraints with different ε, beneficiaries, and victims. This reading's editorial infrastructure (critical editions, stemmatics) is the downstream pressure on the hybrid reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__discontinuity_reading, institutional, 0.12).
constraint_indexing:directionality_override(correct_latin__discontinuity_reading, organized, 0.78).
constraint_indexing:directionality_override(correct_latin__discontinuity_reading, moderate, 0.88).
constraint_indexing:directionality_override(correct_latin__discontinuity_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
