% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy / Orton-Gillingham Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint is the structured literacy / Orton-Gillingham reading of
 *   the literacy_acquisition_kernel: reading acquisition requires explicit,
 *   systematic, cumulative, multisensory instruction spanning phonological
 *   awareness through comprehension, developed originally for students with
 *   dyslexia but increasingly mandated universally through 'science of
 *   reading' state legislation and licensure reform. As legislative mandates
 *   and certification regimes have matured (roughly the last two decades,
 *   intensifying sharply in the last several years), the constraint has moved
 *   from an intervention-tier practice for identified dyslexic students to a
 *   broad teacher-certification and curriculum-procurement requirement, with
 *   corresponding growth in extraction on general education teachers and
 *   district budgets. The claim is authored as tangled_rope: there is a
 *   genuine coordination function (dyslexic students demonstrably benefit
 *   from explicit systematic instruction; a real cognitive-science problem
 *   was being ignored under prior instructional defaults) coexisting with
 *   asymmetric extraction (certification bodies and publishers capture rents
 *   from an expanding mandate whose universal, non-dyslexic-targeted scope is
 *   less firmly evidenced than its dyslexia-targeted scope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.48).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy / Orton-Gillingham Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '5cef6155-f0da-46bf-bc6e-4a71860bda96').
narrative_ontology:cs_kernel_codification('5cef6155-f0da-46bf-bc6e-4a71860bda96', formalized).
narrative_ontology:cs_authority_grounding('5cef6155-f0da-46bf-bc6e-4a71860bda96', expertise).
narrative_ontology:cs_interpretation_layer_present('5cef6155-f0da-46bf-bc6e-4a71860bda96').
narrative_ontology:cs_reading_relation('5cef6155-f0da-46bf-bc6e-4a71860bda96', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('5cef6155-f0da-46bf-bc6e-4a71860bda96', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('5cef6155-f0da-46bf-bc6e-4a71860bda96', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('5cef6155-f0da-46bf-bc6e-4a71860bda96', foundational, explicit_multisensory_cumulative_instruction_required_for_all_learners).
narrative_ontology:cs_axiom_status(explicit_multisensory_cumulative_instruction_required_for_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('5cef6155-f0da-46bf-bc6e-4a71860bda96', explicit_multisensory_cumulative_instruction_required_for_all_learners, empirically_contingent).
narrative_ontology:cs_axiom('5cef6155-f0da-46bf-bc6e-4a71860bda96', secondary, dyslexia_targeted_protocol_generalizes_universally).
narrative_ontology:cs_axiom_status(dyslexia_targeted_protocol_generalizes_universally, holdable).
narrative_ontology:cs_axiom_grounding('5cef6155-f0da-46bf-bc6e-4a71860bda96', dyslexia_targeted_protocol_generalizes_universally, empirically_contingent).
narrative_ontology:cs_reference_frame('5cef6155-f0da-46bf-bc6e-4a71860bda96', og_dyslexia_intervention_specialty_practice).
narrative_ontology:cs_drift_state('5cef6155-f0da-46bf-bc6e-4a71860bda96', post_science_of_reading_legislative_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5cef6155-f0da-46bf-bc6e-4a71860bda96', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, dyslexic_students).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, og_curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_training_budgets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on explicit, systematic, multisensory instruction to acquire decoding skills that do not develop through incidental exposure. Where structured literacy is implemented with fidelity, reading failure rates drop substantially. Cannot self-advocate for the pedagogy they need; entirely dependent on adults adopting it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, dyslexic_students, beneficiary,
    powerless, biographical, trapped, local).

% Required to complete lengthy, often costly specialized certification (sometimes 60-150 hours) in structured literacy/Orton-Gillingham methodology on top of existing certification and workload. Many trained in whole-language or balanced-literacy teacher-preparation programs must retrain mid-career. Exit means leaving the profession or working in districts that do not mandate the retraining; neither is a low-cost option.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, regional).

% Certify practitioners, accredit training programs, and lobby for legislative mandates requiring structured literacy training in teacher preparation and licensure. Collect certification fees and control the credential pipeline; benefit directly from expanding the mandate's scope.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies, beneficiary).

% Sell structured literacy curricula, decodable texts, assessment kits, and teacher-training materials to districts mandated or incentivized to adopt the approach. Revenue scales directly with the breadth of the mandate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, og_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Face state-level 'science of reading' legislation requiring structured literacy adoption without commensurate funding for retraining staff, purchasing new curricula, or hiring reading specialists. Must reallocate scarce budget from other programs or seek waivers where waivers still leave the underlying legal requirement in place.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_training_budgets, payer,
    moderate, biographical, constrained, regional).

% Advocate, often through dyslexia-specific parent organizations, for structured literacy mandates after experiencing their children's reading failure under whole-language or balanced-literacy instruction. Gain leverage collectively (through advocacy groups) that they lack individually against school systems.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    powerless, biographical, constrained, local).

% Built careers and instructional identities around meaning-first approaches now being legislatively displaced. Their pedagogical judgment is treated as presumptively invalid under 'science of reading' statutes; they are rarely consulted in the design of retraining mandates that target their existing practice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_and_balanced_literacy_practitioners, excluded,
    moderate, biographical, trapped, national).

% Conduct and synthesize the empirical literature on reading acquisition (meta-analyses, RCTs, cognitive science of dyslexia). Their findings are cited by all sides but are frequently compressed into policy soundbites that outrun the actual evidentiary nuance, especially regarding universal (non-dyslexic) applicability.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_science_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional practice around empirically-grounded methods for how the reading brain acquires decoding skill, particularly for students whose neurology makes incidental phonics acquisition unreliable — solving a real problem of reading failure that whole-language approaches left unaddressed for a substantial minority of learners.
% TRANSFER_FUNCTION: Moves training time, certification cost, and curriculum-adoption budget from general classroom teachers and under-resourced districts to credentialing bodies and curriculum publishers, while moving instructional benefit (reduced reading failure, earlier intervention) to dyslexic and struggling readers and validation to their parent-advocates.
% ABSENT_VOICES: Whole-language and balanced-literacy practitioners whose pedagogical training and professional identity are being legislatively overridden are rarely at the table when 'science of reading' statutes are drafted; general education teachers facing new certification burdens are consulted less than credentialing organizations and advocacy groups that lobby for the mandate.
% DISAPPEARANCE_RATIONALE: If structured literacy mandates and certification requirements vanished overnight, districts would revert to prior teacher-preparation defaults (often whole-language or balanced-literacy influenced), dyslexia-specific intervention would again depend on private tutoring or family advocacy rather than systemic policy, and the credentialing/publishing infrastructure built around Orton-Gillingham training would lose its primary revenue and legitimacy source.
% FOUNDING_PROBLEM: A substantial fraction of children, disproportionately those with dyslexia, were failing to learn to read under meaning-emphasis and incidental-phonics classroom approaches, and this failure was frequently misattributed to the child rather than to instructional mismatch with how decoding skill is actually acquired.
% FOUNDING_PROBLEM_CORROBORATION: Independent reading-science researchers outside the Orton-Gillingham credentialing and publishing ecosystem corroborate that explicit, systematic phonics instruction produces better decoding outcomes for at-risk readers (converging meta-analytic evidence, e.g. National Reading Panel-descended literature). However, those same researchers are more divided and more cautious than credentialing bodies about whether the FULL structured-literacy protocol (multisensory techniques, specific OG sequencing, universal application beyond phonics) is independently validated versus the phonics component alone — the corroboration supports the founding problem but does not fully corroborate the specific structured-literacy solution as distinct from phonics_reading.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48, rising from 0.28) tracks the expansion of the mandate from targeted dyslexia intervention to universal teacher certification requirements — the coordination benefit to dyslexic students is real and roughly constant, but the extraction on general education teachers and district budgets has grown as legislative mandates broadened scope without proportional broadening of the evidentiary case for universal (non-targeted) application. Suppression (0.42, rising from 0.20) reflects growing legislative and licensure enforcement — states increasingly require structured literacy certification for licensure renewal, foreclosing the option of continuing to practice under alternative pedagogical training. Theater ratio is moderate-low (0.22) — most of the activity is genuine instructional retraining, not pure performance, though a growing share is compliance documentation and credential accumulation with looser connection to classroom practice change. Resistance (0.55) is substantial because displaced whole-language/balanced-literacy practitioners and their teacher-preparation programs actively contest the mandate's universal scope, distinct from a genuine mountain which would meet almost no organized resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Dyslexic students are the clearest structural beneficiaries — they are powerless and trapped in the sense of complete instructional dependency, but the constraint operates FOR them, giving them low directionality toward extraction despite their powerlessness (the constraint subsidizes rather than extracts from them). Credentialing bodies and curriculum publishers are organized beneficiaries with arbitrage-grade exit — they can expand into new jurisdictions and shift curricular products as the mandate evolves, and they profit directly from mandate breadth. General education teachers and under-resourced districts are the clear payers: moderate power, constrained exit (leaving the profession or moving districts is costly), and they bear the certification/training/procurement burden without proportional say in mandate design. Parents of struggling readers are powerless individually but gain effective directionality-shifting leverage through organized advocacy — this is noted but not overridden, since the underlying individual-parent power atom remains powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — children, especially dyslexic children, failing to learn to read under instructional approaches mismatched to how decoding skill is acquired — remains substantially live per independent reading-science research; this blocks a straightforward mandatrophy verdict. What is contested is whether the SOLUTION has outrun its warrant: the phonics-and-explicit-instruction core is well corroborated, but the full structured-literacy protocol's universal application to non-dyslexic students, and its expansion into a broad certification-and-procurement regime, is less independently corroborated and shows the signature of extraction (credentialing and publishing revenue scaling with mandate breadth) riding on a genuine underlying coordination need. The tangled_rope classification is designed to hold both facts at once rather than resolve them into either 'pure coordination' or 'pure extraction.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_reading_or_phonics_variant,
    'Is structured_literacy_reading a genuinely distinct kernel reading from phonics_reading, or is it a more elaborate, more heavily institutionalized VARIANT of the same underlying phonics-first premise — differing in degree of protocol specification and credentialing infrastructure rather than in kind?',
    'Comparative analysis of the two readings'' core axioms: if structured_literacy''s claims about multisensory technique and universal (non-dyslexia-specific) applicability are empirically dissociable from phonics_reading''s decoding-before-comprehension claim (i.e., the multisensory and universality components could be false while phonics-first remains true, or vice versa), they are structurally distinct. If every empirical test of structured literacy''s added components collapses into a test of the phonics-first claim, it is a phonics_reading variant with added credentialing overhead.',
    'If a variant, the extraction attributable to specialized certification and OG-branded curricula should be understood as extraction layered onto phonics_reading''s coordination function rather than an independent coordination function of its own — this would push toward reclassifying part of this reading''s ε as attributable to the phonics kernel rather than a genuinely separate structured-literacy coordination problem. If genuinely fourth, the multisensory/cumulative/universal-application claims constitute an independent empirical program deserving separate evidentiary standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_reading_or_phonics_variant, conceptual, 'Whether structured literacy is a distinct kernel reading or a heavily institutionalized phonics_reading variant.').

omega_variable(
    universal_applicability_evidentiary_gap,
    'Is the evidence base for structured literacy''s benefit to DYSLEXIC students (strong, well-replicated) being used to license its UNIVERSAL mandate for all students (including those without reading difficulty), where the evidentiary case is thinner?',
    'Meta-analytic comparison of effect sizes for structured literacy interventions in identified-dyslexic populations versus general, non-at-risk populations; examination of whether legislative mandates distinguish tiered/targeted intervention from universal Tier-1 classroom mandate.',
    'If the universal mandate is substantially unsupported relative to the targeted-intervention evidence, the extraction on general education teachers and districts (certification and procurement costs applied system-wide) is disproportionate to the demonstrated coordination benefit, strengthening the tangled_rope reading over a pure rope reading. If well-supported, the coordination function is broader than assumed and extraction is more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_evidentiary_gap, empirical, 'Whether universal application evidence matches the strength of dyslexia-targeted intervention evidence.').

omega_variable(
    credentialing_capture_trajectory,
    'Are structured-literacy credentialing bodies and curriculum publishers actively lobbying to broaden legislative mandate scope beyond what the evidentiary case supports, in ways that expand their own revenue and market position (regulatory capture dynamic), or is mandate expansion driven primarily by independent legislative and parent-advocacy pressure?',
    'Tracing legislative lobbying records, credentialing-body funding sources, and the sequence of mandate expansion relative to publication of supporting versus null/mixed research findings.',
    'If capture-driven, the beneficiary status of credentialing bodies and publishers should be read as more extractive and less incidental than a pure downstream-beneficiary reading suggests, potentially warranting a directionality override upward for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_capture_trajectory, empirical, 'Whether mandate expansion reflects regulatory capture by credentialing/publishing interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints in the literacy_acquisition_kernel family, each instantiating a distinct reading of how reading acquisition works and what instruction it requires. structured_literacy_reading shares its phonics-first, explicit-instruction premise with phonics_reading (influences relationship — the phonics evidence base is often cited as foundational support for structured literacy's broader claims) but adds a heavier institutional/credentialing layer and a universal-applicability claim not present in the narrower phonics reading. It directly contradicts whole_language_reading's premise that explicit decoding instruction is unnecessary (forecloses relationship). It coexists in policy discourse with balanced_literacy_reading as a competing but not strictly incompatible synthesis claim, since balanced literacy also incorporates phonics instruction, just embedded differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
