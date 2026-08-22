% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading of Correct Latin: Medieval Latin as Legitimate Evolved Classical Latin
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the contested
 *   'correct Latin' kernel: correctness is conferred by unbroken living
 *   transmission, so medieval Latin is not a corruption of Classical Latin
 *   but its legitimate evolved descendant, on the same logic that makes
 *   contemporary spoken languages legitimate despite differing from their
 *   ancestral forms. This reading is generated as a single, clean,
 *   ε-invariant constraint; it does not describe or adjudicate the
 *   discontinuity or hybrid readings, which are separate constraint stories
 *   linked via network.affects_constraints. Extraction here is comparatively
 *   low and mostly reputational/institutional (whose textual tradition gets
 *   treated as authoritative) rather than material.
 *
 * KEY AGENTS:
 *   - medieval_latin_scribal_communities: beneficiary (moderate/constrained) — their historical usage is validated as legitimate rather than treated as error
 *   - monastic_and_ecclesiastical_institutions: beneficiary/agenda_setter (institutional/constrained) — continuous liturgical use grounds their linguistic authority
 *   - medieval_latin_specialists: beneficiary (moderate/mobile) — modern field whose object of study is validated by this reading
 *   - students_trained_on_classicist_norms: payer (powerless/constrained) — their classroom standard is demoted to one historical register among several
 *   - classical_philology_establishment: excluded — holds the rival discontinuity reading, not represented in this constraint's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.38).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.42).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading of Correct Latin: Medieval Latin as Legitimate Evolved Classical Latin").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, 'd4501c39-3e6f-457b-b7c1-eec227fafb0f').
narrative_ontology:cs_kernel_codification('d4501c39-3e6f-457b-b7c1-eec227fafb0f', distributed).
narrative_ontology:cs_authority_grounding('d4501c39-3e6f-457b-b7c1-eec227fafb0f', practice).
narrative_ontology:cs_interpretation_layer_present('d4501c39-3e6f-457b-b7c1-eec227fafb0f').
narrative_ontology:cs_reading_relation('d4501c39-3e6f-457b-b7c1-eec227fafb0f', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d4501c39-3e6f-457b-b7c1-eec227fafb0f', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('d4501c39-3e6f-457b-b7c1-eec227fafb0f', foundational, continuous_practice_constitutes_correctness).
narrative_ontology:cs_axiom_status(continuous_practice_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('d4501c39-3e6f-457b-b7c1-eec227fafb0f', continuous_practice_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('d4501c39-3e6f-457b-b7c1-eec227fafb0f', foundational, no_ontological_rupture_between_classical_and_medieval_stages).
narrative_ontology:cs_axiom_status(no_ontological_rupture_between_classical_and_medieval_stages, holdable).
narrative_ontology:cs_axiom_grounding('d4501c39-3e6f-457b-b7c1-eec227fafb0f', no_ontological_rupture_between_classical_and_medieval_stages, empirically_contingent).
narrative_ontology:cs_reference_frame('d4501c39-3e6f-457b-b7c1-eec227fafb0f', unbroken_transmission_confers_legitimacy).
narrative_ontology:cs_drift_state('d4501c39-3e6f-457b-b7c1-eec227fafb0f', post_19th_century_philological_professionalization, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d4501c39-3e6f-457b-b7c1-eec227fafb0f', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_latin_scribal_communities).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, monastic_and_ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, vernacular_romance_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_latin_specialists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, students_trained_on_classicist_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically, these were the copyists, chancery clerks, and monastic writers who used Latin as a living working language across centuries. Under this reading, their usage counts as legitimate Latin rather than corruption, which retroactively validates the forms they actually produced rather than treating their entire textual output as an error record to be corrected against.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_latin_scribal_communities, beneficiary,
    moderate, civilizational, constrained, continental).

% The Church transmitted Latin as a working liturgical and administrative language for a millennium. The continuity reading treats that unbroken institutional practice as constitutive of correctness itself, which grounds ecclesiastical Latin's authority in continuous use rather than in fidelity to a reconstructed ancient standard the Church did not originate.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, monastic_and_ecclesiastical_institutions, beneficiary,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, monastic_and_ecclesiastical_institutions, agenda_setter).

% Scholars tracing the emergence of French, Italian, Spanish, etc. from Latin depend on medieval Latin being treated as a real, evolving linguistic stage rather than a degraded copy of Classical forms. The continuity reading supplies the unbroken chain their historical linguistics requires; without it, their object of study is redescribed as centuries of mistake rather than of change.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_romance_philologists, beneficiary,
    moderate, generational, mobile, continental).

% Academics whose careers are built on editing, teaching, and interpreting medieval Latin texts on their own terms. This reading validates their entire disciplinary object as a legitimate stage of the language, protecting the field's status against a framing that would treat its primary sources as corrupted evidence needing correction to a prior standard.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_latin_specialists, beneficiary,
    moderate, biographical, mobile, global).

% Students who learned Latin through a Classical-normed curriculum (grammar rules, prose style, case usage keyed to Cicero and Caesar) encounter medieval texts whose usage departs from what they were taught to treat as correct. Under the continuity reading their classroom standard is demoted from 'correct Latin' to one historical register among several, which can undercut the authority of the norms they invested years mastering and complicates assessment of what error even means in medieval-text coursework.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, students_trained_on_classicist_norms, payer,
    powerless, biographical, constrained, national).

% The tradition of textual scholarship grounded in reconstructing and privileging the Classical corpus is not part of this reading's authority structure; its normative claim that medieval forms are deviations to be corrected against ancient attestation has no purchase here. This community would object that the continuity reading dissolves a meaningful distinction they consider central to the discipline, but this constraint does not include their framework.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_philology_establishment, excluded,
    institutional, civilizational, mobile, global).

% Linguists studying language change as a general phenomenon can use the continuity reading's data (uninterrupted transmission of a living language across a millennium) as a case study in gradual diachronic change, independent of which normative reading of 'correctness' wins the philological dispute.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, historical_linguistics_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent standard for what counts as correct Latin across a millennium-plus span of continuous use, allowing scribes, clerks, liturgists, and later scholars to treat their actual working language as legitimate rather than perpetually measuring it against an external, increasingly distant ancient benchmark.
% TRANSFER_FUNCTION: Moves normative authority away from reconstructed Classical philology and toward the institutions and communities that carried Latin forward in continuous practice — chiefly the Church and medieval scribal culture — and by extension toward the modern scholarly field built on treating their output as legitimate primary evidence rather than error to be corrected.
% ABSENT_VOICES: The Classical philology establishment, which holds that correctness is anchored in the ancient textual record and that medieval divergence is corruption, is not represented in this reading's framework; its objection — that dissolving the Classical/medieval boundary erases a meaningful and defensible standard — belongs to the discontinuity_reading constraint, not this one.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned, medieval Latin scholarship would lose its claim to be studying a legitimate stage of the language rather than a corpus of errors; curricula, editions, and the self-understanding of an entire subfield are organized around the premise that continuous transmission confers legitimacy. Ecclesiastical Latin's institutional authority claim would also need to be re-grounded on different terms.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century philology needed a framework for treating the vast medieval Latin textual record (charters, chronicles, liturgy, scholastic writing) as linguistically and historically meaningful rather than as a millennium of mistakes to be filtered out en route to reconstructing Classical norms.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists working on Romance language emergence (a field with no institutional stake in either medieval Latin studies or Church authority) independently corroborate that treating Latin as a continuously evolving language, rather than a fixed standard with a corrupt aftermath, better fits the attested diachronic evidence — this is a corroboration from outside the directly benefiting communities, though it does not resolve the normative question of which register counts as 'correct.'
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).
:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.38 at interval end) because the continuity reading redistributes scholarly and institutional legitimacy rather than material resources — the main cost borne by students_trained_on_classicist_norms is confusion about which norm is authoritative and diminished authority of previously learned rules, not direct extraction. Suppression is moderate (0.42): the reading does not coercively suppress the discontinuity view (it remains a live, published, taught alternative), but institutional endorsement of continuity within certain fields (medieval studies departments, ecclesiastical Latin instruction) does create real friction for holding the rival view within those institutions. Accessibility collapse is moderate-low (0.35) reflecting that the discontinuity and hybrid readings remain fully available and actively defended elsewhere in the discipline — this is not a case where alternatives have vanished. Resistance is moderate-high (0.55) because the classical philology establishment actively contests this reading's core premise in ongoing scholarly debate.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval Latin scribal communities, ecclesiastical institutions, Romance philologists, and medieval Latin specialists are declared beneficiaries: the continuity reading validates their historical practice or professional object of study, conferring legitimacy without requiring them to justify departures from a Classical standard. Students trained on Classicist norms are the payer group: they bear the cost of having their learned standard reframed as one historical stage rather than the timeless correct form, which can undercut confidence and complicate pedagogy. The classical philology establishment is excluded rather than victimized — they hold a structurally opposed reading (discontinuity) that simply is not this constraint's framework; their objection belongs to the sibling story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treating the medieval Latin textual record as linguistically meaningful rather than as an error record) remains live: medieval Latin scholarship continues as an active field with real evidentiary stakes. There is no zombie-mandate pattern here — the founding_problem_status is 'live' and corroborated by an outside discipline (Romance historical linguistics), so the continuity reading is not persisting past its function; it is an ongoing, contested normative position rather than a decayed one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_empirical_status,
    'Is the linguistic transition from Classical to medieval Latin better modeled as continuous gradual drift (supporting this reading) or as a documentable rupture correlated with the collapse of formal education infrastructure in late antiquity (supporting the discontinuity reading)?',
    'Corpus-linguistic analysis of dated texts across the 3rd-9th centuries CE tracking rate and clustering of morphological/syntactic change, compared against independent evidence of educational institution continuity or collapse in the same regions and periods.',
    'Evidence of smooth, unclustered change across the period would support treating the transition as evolution proper (this reading); evidence of a sharp discontinuity correlated with institutional collapse would support the sibling discontinuity reading and weaken this constraint''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_rupture_empirical_status, empirical, 'Whether the historical linguistic record shows gradual drift or a documentable rupture.').

omega_variable(
    authority_grounding_is_normative_or_descriptive,
    'Is ''continuous living practice confers correctness'' a descriptive claim about how languages actually change, or a normative/political claim about which institutions (the Church, medieval universities) get to inherit Classical prestige?',
    'Compare how the continuity criterion is applied elsewhere in historical linguistics (e.g., is Vulgar Latin-to-Romance transition treated the same way) versus whether it is invoked selectively to defend specific institutional authority claims about medieval Latin''s status.',
    'If purely descriptive and applied consistently, this reading is a neutral linguistic framework; if selectively invoked to protect ecclesiastical or academic institutional authority, the beneficiary structure is closer to interest-driven than to disinterested scholarship, which would push the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_is_normative_or_descriptive, conceptual, 'Whether the continuity criterion is a neutral descriptive principle or a normatively loaded institutional defense.').

omega_variable(
    reading_framing_alternative,
    'Could this constraint alternatively be framed around the kernel of ''linguistic legitimacy'' broadly rather than ''correct Latin'' specifically, changing which axioms are foundational?',
    'Compare classification outcomes if the kernel were reframed around general historical-linguistic legitimacy criteria (applicable to any language''s diachronic stages) versus the Latin-specific correctness dispute as authored here.',
    'The Latin-specific framing (as authored) ties this reading tightly to the medieval/Classical philological dispute and its specific institutional stakeholders; a general-legitimacy framing would broaden beneficiaries to all historical linguistics and likely reduce the extraction/contest signal, since the normative stakes specific to Latin philology would not carry over.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_alternative, conceptual, 'Alternative framing of the kernel at the level of general linguistic legitimacy versus Latin-specific correctness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t20, correct_latin__continuity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(corr_tr_t40, correct_latin__continuity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(corr_tr_t60, correct_latin__continuity_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(corr_tr_t80, correct_latin__continuity_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(corr_tr_t100, correct_latin__continuity_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t20, correct_latin__continuity_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(corr_be_t40, correct_latin__continuity_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(corr_be_t60, correct_latin__continuity_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(corr_be_t80, correct_latin__continuity_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(corr_be_t100, correct_latin__continuity_reading, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the correct_latin kernel. correct_latin__discontinuity_reading claims correctness is anchored in the Classical textual record and treats medieval Latin as corruption requiring reconstruction (high extraction directed at medieval Latin's legitimacy claims); correct_latin__hybrid_reading claims partial continuity with targeted textual correction. Each story authors its own ε, beneficiaries, victims, and claimed_type from its own reading's premises; none averages over the others. This story (continuity_reading) is the most permissive of the three toward medieval usage and consequently authors the lowest extraction level among the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
