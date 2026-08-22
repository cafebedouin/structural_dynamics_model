% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical-Ecclesiastical Latin Correctness Standard
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the
 *   classical_latin_standard kernel: correct Latin requires Classical
 *   grammatical fidelity as its spine, but recognizes a bounded set of
 *   post-Classical ecclesiastical and technical vocabulary as legitimate
 *   development rather than corruption. This is the position
 *   institutionalized by the Vatican's Latin office and most university Latin
 *   faculties — neither the reconstruction_reading's discontinuous return to
 *   purely Classical sources nor the continuity_reading's acceptance of the
 *   full span of natural medieval drift. The hybrid position draws a boundary
 *   — Classical grammar plus an accepted technical lexicon, everything else
 *   marked as barbarism — and that boundary is where this constraint's
 *   extraction is located.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.42).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical-Ecclesiastical Latin Correctness Standard").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '85bb1b2b-db07-4199-b1ae-7db30c5d919f').
narrative_ontology:cs_kernel_codification('85bb1b2b-db07-4199-b1ae-7db30c5d919f', distributed).
narrative_ontology:cs_authority_grounding('85bb1b2b-db07-4199-b1ae-7db30c5d919f', lineage).
narrative_ontology:cs_interpretation_layer_present('85bb1b2b-db07-4199-b1ae-7db30c5d919f').
narrative_ontology:cs_reading_relation('85bb1b2b-db07-4199-b1ae-7db30c5d919f', classical_latin_standard__continuity_reading, influences).
narrative_ontology:cs_reading_relation('85bb1b2b-db07-4199-b1ae-7db30c5d919f', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('85bb1b2b-db07-4199-b1ae-7db30c5d919f', foundational, classical_grammar_as_spine_with_technical_accommodation).
narrative_ontology:cs_axiom_status(classical_grammar_as_spine_with_technical_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('85bb1b2b-db07-4199-b1ae-7db30c5d919f', classical_grammar_as_spine_with_technical_accommodation, conventional).
narrative_ontology:cs_axiom('85bb1b2b-db07-4199-b1ae-7db30c5d919f', foundational, bounded_post_classical_legitimacy).
narrative_ontology:cs_axiom_status(bounded_post_classical_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('85bb1b2b-db07-4199-b1ae-7db30c5d919f', bounded_post_classical_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('85bb1b2b-db07-4199-b1ae-7db30c5d919f', counter_reformation_curial_latin_standard).
narrative_ontology:cs_drift_state('85bb1b2b-db07-4199-b1ae-7db30c5d919f', contemporary_vatican_latin_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('85bb1b2b-db07-4199-b1ae-7db30c5d919f', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_latinists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, vatican_curial_latin_office).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_scholarly_editors).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, seminary_and_university_latin_faculty).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_influenced_writers).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, self_taught_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, regional_medieval_dialect_traditions).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, domain_specific_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues style guides and adjudicates correctness for ecclesiastical Latin (encyclicals, canon law, liturgy), ruling that Classical syntax and morphology govern the language's spine while post-Classical ecclesiastical vocabulary (baptismus, trinitas, and comparable coinages) is admitted as legitimate technical extension. It sets the boundary between accepted development and rejected barbarism and revises that boundary as needed.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vatican_curial_latin_office, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Produce critical editions and philological commentary judged by Classical textual standards; the hybrid standard validates their disciplinary authority over the Classical core while leaving the ecclesiastical and technical vocabularies to a separate but parallel expert authority, so their gatekeeping role over 'proper' syntax is undisturbed.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_scholarly_editors, beneficiary,
    institutional, generational, arbitrage, global).

% Teach and examine Latin using the hybrid standard: students must master Classical grammar but are also taught the accepted technical/ecclesiastical lexicon as legitimate. Their curricular authority and credentialing power depend on the standard's continued institutional recognition; abandoning it in either direction (pure Classical or pure vernacular-continuity) would devalue their specific expertise.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, seminary_and_university_latin_faculty, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, seminary_and_university_latin_faculty, agenda_setter).

% Write Latin shaped by regional medieval and early-modern usage that falls outside both the Classical core and the officially sanctioned technical exceptions. Their forms are marked as barbarisms or errors in institutional review, journal publication, and examination, even where their usage reflects centuries of continuous regional practice. They can revise toward the hybrid norm at real cost to their existing competence, or remain outside institutionally recognized correctness.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_influenced_writers, payer,
    moderate, biographical, constrained, regional).

% Learn Latin outside formal institutional pipelines, often from devotional, popular, or online sources that do not clearly separate accepted ecclesiastical extensions from unsanctioned medieval or modern drift. They bear the cost of the hybrid standard's complexity without institutional access to the authoritative list of what counts as legitimate development versus barbarism, and their errors carry more social cost than an insider's identical usage.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, self_taught_latin_practitioners, payer,
    powerless, biographical, trapped, local).

% The body of Medieval Latin usage patterns (regional spelling, syntax simplifications, vocabulary innovations outside the ecclesiastical-technical set) that the hybrid standard does not recognize as legitimate development. As a non-agent linguistic tradition, it cannot advocate for itself; its continuities are preserved only in manuscripts and specialist philology, not in the living correctness standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, regional_medieval_dialect_traditions, excluded,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__hybrid_reading, regional_medieval_dialect_traditions).

% Study the historical development of Latin across all periods without needing to adjudicate correctness for institutional purposes. They can document how the hybrid standard's boundary between 'legitimate ecclesiastical development' and 'barbarism' was itself drawn by historically contingent institutional decisions rather than by any linguistic-internal criterion.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, comparative_philologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single institutionally usable correctness standard that lets ecclesiastical, legal, and scholarly Latin users communicate across centuries and regions without either abandoning Classical grammatical discipline or being unable to name post-Classical realities (sacraments, offices, disciplines) for which Classical Latin had no vocabulary.
% TRANSFER_FUNCTION: Moves recognition, institutional legitimacy, and access to credentialed positions (teaching posts, editorial authority, curial appointments) toward those whose Latin conforms to the hybrid norm, and away from those whose Latin reflects either unsanctioned regional medieval drift or self-taught eclectic acquisition — regardless of the historical continuity or communicative adequacy of the excluded forms.
% ABSENT_VOICES: Speakers and writers of regional medieval Latin dialects have no formal advocate in the standard-setting process; their usage patterns are documented by philologists after the fact but are not represented when the curial office or university faculties decide which post-Classical forms count as legitimate technical development versus barbarism.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, ecclesiastical and academic institutions would lose their shared basis for judging Latin competence; correctness would either revert to pure philological reconstruction (reconstruction_reading) or open fully to living-tradition continuity (continuity_reading), each of which would redistribute credentialing authority and reclassify large bodies of currently-marked 'barbarism' as either newly illegitimate or newly legitimate.
% FOUNDING_PROBLEM: Post-Classical institutions (the Church, universities, chanceries) needed to write about concepts Classical Latin had no words for, while resisting the perception that their Latin had degenerated into unintelligible or low-status vernacular drift — the hybrid standard was built to let them claim Classical prestige while still naming their own world.
% FOUNDING_PROBLEM_CORROBORATION: Comparative philologists outside the curial and university system corroborate that the underlying tension (naming new institutional realities while claiming continuity with Classical prestige) remains active in ongoing Vatican Latin neologism committees and university Latin pedagogy debates; however, these same observers note that many excluded 'barbarisms' are linguistically indistinguishable in kind from the accepted ecclesiastical coinages, suggesting the boundary is maintained more for institutional gatekeeping than for any principled linguistic criterion — a reading not corroborated by the beneficiary institutions themselves.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.48) because the hybrid standard does real coordination work (letting institutions name post-Classical realities without abandoning Classical prestige) while also delegitimizing a substantial body of regional medieval usage that is linguistically no different in kind from the sanctioned exceptions. Suppression starts higher (0.5) when the boundary between accepted and rejected post-Classical forms was less institutionally settled, and eases slightly (0.42) as the accepted technical lexicon becomes conventionalized and fewer disputes arise at the margin. Theater ratio rises modestly (0.18 to 0.3) as more of the standard's maintenance becomes the ritual reaffirmation of an already-settled lexicon rather than active adjudication of genuinely contested cases.
 *
 * PERSPECTIVAL GAP:
 *   From the curial office and faculty seats, the hybrid standard looks like principled discipline: Classical rigor plus necessary technical accommodation. From the vernacular-influenced writer or self-taught practitioner seat, the same boundary looks arbitrary and exclusionary, since their forms are often structurally identical to the sanctioned ecclesiastical exceptions but happen to fall outside the institutionally recognized list. The engine's per-seat computation should register this asymmetry directly from the power/exit differentials rather than from any claim either side makes about its own legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (curial office, scholarly editors, faculty) hold low d: they author the boundary, and their existing competence already sits inside it. Vernacular-influenced writers and self-taught practitioners hold high d: their usage falls on the wrong side of a boundary they did not draw and often cannot fully learn, since the boundary is maintained by institutions they have limited access to. Regional medieval dialect traditions are marked non-agent and excluded rather than victimized as an agent, since the tradition itself cannot bear costs — the living writers and readers who rely on it do.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (naming post-Classical institutional realities while claiming Classical continuity) remains live, which prevents blanket dismissal of the hybrid standard as pure inertial extraction — it is not a piton. But the corroboration gap matters: only the beneficiary institutions attest that the current boundary between accepted ecclesiastical coinage and rejected barbarism is principled; outside philological observers see the boundary as gatekeeping without a clean linguistic criterion. This keeps the constraint classified as tangled_rope rather than rope — genuine coordination function, but riding alongside an extraction that a fully corroborated coordination-only reading would not carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_principled_or_arbitrary,
    'Is the line the hybrid standard draws between ''legitimate ecclesiastical/technical development'' and ''barbarism'' grounded in a linguistically principled criterion, or is it an institutionally contingent list that happens to track which forms powerful users already employ?',
    'Comparative structural analysis of accepted coinages (e.g. trinitas, baptismus) against rejected regional forms of the same period and morphological type, checking whether any linguistic-internal feature (regularity of derivation, productivity, semantic transparency) distinguishes them, or whether the only distinguishing feature is which institution used the form first.',
    'If no principled linguistic criterion is found, the hybrid reading''s coordination claim is substantially weaker than it appears and the constraint moves further toward pure gatekeeping (higher effective extraction); if a real criterion exists, the tangled_rope classification is well-supported and the coordination component is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_principled_or_arbitrary, conceptual, 'Whether the accepted/rejected post-Classical boundary is linguistically principled or institutionally contingent.').

omega_variable(
    kernel_reading_selection_basis,
    'This constraint is one of three readings (continuity, hybrid, reconstruction) of a single contested kernel — what determines which institutions adopt which reading, and is the hybrid reading''s dominance in Church and university contexts itself a function of institutional power rather than superior linguistic argument?',
    'Historical-institutional analysis of when and why the Vatican Latin office and major universities converged on hybrid criteria rather than either sibling reading, tracing whether the convergence tracks argument quality or tracks which institutions had standard-setting power at key historical junctures (e.g. humanist philology''s rise, Counter-Reformation Latin policy).',
    'If institutional power rather than linguistic argument explains hybrid_reading''s dominance, this reframes the constraint as a settled outcome of an inter-institutional contest rather than a linguistically neutral compromise, raising its effective extraction relative to the sibling readings it displaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the hybrid reading''s institutional dominance reflects linguistic merit or historical institutional power.').

omega_variable(
    self_taught_practitioner_access_asymmetry,
    'Do self-taught practitioners lack access to the specific list of sanctioned technical/ecclesiastical vocabulary that would let them conform to the hybrid standard, and if so, is this an incidental information gap or a structural feature that preserves institutional gatekeeping?',
    'Survey of available public documentation on sanctioned post-Classical Latin vocabulary (curial style guides, standard reference grammars) versus what is accessible to learners outside institutional Latin programs.',
    'If the sanctioned list is genuinely hard to access outside institutions, suppression for powerless practitioners is higher than the story''s aggregate suppression score suggests, and the effective extraction borne by that seat specifically is understated by a single scalar suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_taught_practitioner_access_asymmetry, empirical, 'Whether information asymmetry about the sanctioned vocabulary list constitutes structural suppression for self-taught practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__hybrid_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__hybrid_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__hybrid_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__hybrid_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_reading member of the classical_latin_standard kernel family (three siblings: continuity_reading, hybrid_reading, reconstruction_reading). Each reading is authored as an independent ε-invariant constraint with its own beneficiary/victim structure per the ε-invariance principle: continuity_reading has the smallest victim set and lowest suppression (nearly all organic drift legitimized); reconstruction_reading has the largest victim set and highest suppression (nearly all medieval drift rejected); hybrid_reading sits between, with a reduced but non-trivial victim set (only 'barbarisms' outside a sanctioned technical lexicon) and moderate extractiveness. The three are linked via affects_constraints because institutional adoption of one reading directly affects the resource availability and legitimacy conditions of the others — e.g., the Vatican's adoption of hybrid_reading structurally disadvantages continuity_reading in ecclesiastical contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
