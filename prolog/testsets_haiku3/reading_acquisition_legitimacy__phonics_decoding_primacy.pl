% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-First Decoding Primacy in Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.71).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-First Decoding Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '1658a3d9-c7c3-4084-9f3e-2c958c84f721').
narrative_ontology:cs_kernel_codification('1658a3d9-c7c3-4084-9f3e-2c958c84f721', formalized).
narrative_ontology:cs_authority_grounding('1658a3d9-c7c3-4084-9f3e-2c958c84f721', extraction).
narrative_ontology:cs_interpretation_layer_present('1658a3d9-c7c3-4084-9f3e-2c958c84f721').
narrative_ontology:cs_reading_relation('1658a3d9-c7c3-4084-9f3e-2c958c84f721', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('1658a3d9-c7c3-4084-9f3e-2c958c84f721', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('1658a3d9-c7c3-4084-9f3e-2c958c84f721', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('1658a3d9-c7c3-4084-9f3e-2c958c84f721', foundational, decoding_is_reading_foundation).
narrative_ontology:cs_axiom_status(decoding_is_reading_foundation, holdable).
narrative_ontology:cs_axiom_grounding('1658a3d9-c7c3-4084-9f3e-2c958c84f721', decoding_is_reading_foundation, empirically_contingent).
narrative_ontology:cs_axiom('1658a3d9-c7c3-4084-9f3e-2c958c84f721', foundational, alphabetic_principle_requires_explicit_instruction).
narrative_ontology:cs_axiom_status(alphabetic_principle_requires_explicit_instruction, holdable).
narrative_ontology:cs_axiom_grounding('1658a3d9-c7c3-4084-9f3e-2c958c84f721', alphabetic_principle_requires_explicit_instruction, empirically_contingent).
narrative_ontology:cs_reference_frame('1658a3d9-c7c3-4084-9f3e-2c958c84f721', alphabetic_principle_requires_explicit_systematic_instruction).
narrative_ontology:cs_drift_state('1658a3d9-c7c3-4084-9f3e-2c958c84f721', contemporary_empirical_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1658a3d9-c7c3-4084-9f3e-2c958c84f721', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_phonics_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, remediation_specialists).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, early_whole_language_adopters).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, meaning_centered_educators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_balanced_approach).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, fluent_meaning_makers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_necessity).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_instruction_efficacy).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, phonological_awareness_prerequisite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teachers and reading specialists trained in explicit, systematic phonics instruction. They benefit from policy shifts that mandate phonics-first curricula, fund phonics-aligned materials, and treat phonics decoding competence as the primary success metric. They set the agenda by leading professional development, authoring evidence reviews, and advising policy bodies on what 'evidence-based' reading instruction means.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_phonics_practitioners, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_phonics_practitioners, agenda_setter).

% Educational publishers who produce decodable readers, explicit phonics sequences, and diagnostic decoding assessments. They profit directly from curricula mandates requiring phonics-aligned materials. Their market share grows when policy designates phonics as the legitimate core of reading instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Reading specialists, dyslexia interventionists, and special-education providers trained in structured literacy protocols. They benefit from the legitimacy accorded to systematic phonics approaches and from the early identification systems that funnel children into their services. Their professional identity and career stability anchor to the phonics-first framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, remediation_specialists, beneficiary,
    moderate, biographical, constrained, national).

% Teachers trained under whole-language or constructivist literacy frameworks who treated meaning-making and immersion in authentic text as primary. They face pressure to retrain, repudiate their pedagogy as 'not evidence-based,' and implement phonics sequences that contradict their pedagogical beliefs. Their professional identity and curriculum materials become obsolete.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_whole_language_adopters, payer,
    moderate, biographical, constrained, national).

% Reading educators whose professional identity is grounded in the conviction that reading is comprehension and meaning-construction, not decoding mechanics. They experience phonics-first mandates as delegitimizing their life's work and forcing them to teach in ways they believe harm children's love of reading. Exit would require abandoning both their career orientation and their foundational epistemology of literacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, meaning_centered_educators, payer,
    moderate, biographical, identity_locked, national).

% Teachers trained in balanced literacy models that integrate phonics with guided reading, literature circles, and meaning-centered activities. They are caught between the competing legitimacy claims: phonics-first mandates require narrowing their practice and eliminating the elements they see as essential to engagement and comprehension development.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_balanced_approach, payer,
    moderate, biographical, constrained, national).

% Children identified as 'at-risk' or 'struggling' in early reading. They benefit from early, explicit decoding instruction if their difficulty is phonological; they pay if their struggle stems from comprehension, vocabulary, or engagement (meaning-construction routes) that this reading de-emphasizes. They are trapped in a system that reorganizes around decoding assessments and has reduced access to rich literature and authentic comprehension experiences.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers, payer).

% Children who develop reading fluency and comprehension through immersion in rich text and meaning-centered scaffolding. Under phonics-first mandates, their pathway is disrupted: they spend classroom time on mechanical decoding drills they have already internalized, reducing time for complex texts, discussion, and authentic comprehension work.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, fluent_meaning_makers, payer,
    powerless, biographical, trapped, local).

% Cognitive scientists, psycholinguists, and literacy researchers across multiple methodologies. They observe and investigate the legitimacy claims: whether explicit phonics instruction is necessary and sufficient for all children, whether the alphabetic principle requires explicit systematic instruction, whether decoding and comprehension are independently trainable, and whether early identification via decoding assessments predicts long-term reading success.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_researchers, observer,
    organized, generational, analytical, global).

% State education departments, legislative reading committees, and district adoption boards that set curriculum standards and materials approval requirements. They enforce the phonics-first reading by mandating phonics-aligned curricula, prescribing decodable text sequences, and conditioning funding on phonics fidelity measures. They are the enforcement machinery that sustains the constraint.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_mandating_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Professional organizations, university programs, and grassroots networks that defend whole-language and meaning-centered literacy instruction. They would argue that decoding is subordinate to meaning-construction, that authentic literature is essential to motivation and comprehension development, and that phonics-first sequences damage children's reading engagement. Their voice is structurally excluded from policy-making tables where phonics primacy is now assumed.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocacy_networks, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, systematized framework for teaching the alphabetic principle: instead of leaving decoding to incidental discovery or ad-hoc practice, explicit sequential phonics coordinates instruction across schools, districts, and teacher training programs so that all children receive the same foundational skill in the same order. Coordinates early identification of decoding difficulty through standardized phonological-awareness and phonics-screening assessments.
% TRANSFER_FUNCTION: Transfers classroom time, curriculum authority, and professional discretion from teachers trained in meaning-centered or balanced approaches to publishers, structured-literacy trainers, and phonics-adherent instructional sequences. Moves resources (funding, textbook adoption, teacher development) toward phonics-aligned materials and away from authentic literature collections. Transfers prestige and legitimacy from educators who prioritize comprehension and engagement to those who prioritize decoding automaticity.
% ABSENT_VOICES: Literacy researchers who document reading failure rates in phonics-heavy curricula, especially for children whose primary difficulty is comprehension or vocabulary rather than decoding; educators from whole-language traditions who would argue that decoding is better acquired through meaning-centered immersion; teachers whose classroom observation data shows disengagement and reduced comprehension under phonics-first narrowing; children themselves, who experience the constraint as reduced access to books they want to read.
% DISAPPEARANCE_RATIONALE: If phonics-first mandates disappeared, teachers would reintegrate balanced approaches; publishers would resume printing authentic literature and meaning-centered basals; teacher-training programs would broaden their pedagogy; classroom time for complex texts and comprehension discussion would expand; the incentives driving early decoding assessment and remediation tracking would diminish. The entire architecture of reading instruction would shift back toward integrating decoding with meaning-making rather than privileging decoding as foundational.
% FOUNDING_PROBLEM: In the late 20th century, the 'whole language' movement, which treated reading as a natural acquisition process emergent from immersion in meaningful text, began losing traction when longitudinal studies showed persistent reading failure in early elementary grades, especially for disadvantaged children. The founding problem: how to ensure all children acquire basic decoding competence reliably and early, rather than waiting for it to 'emerge' naturally from rich-reading immersion.
% FOUNDING_PROBLEM_CORROBORATION: Explicit-phonics advocates cite large-effect-size meta-analyses (Scarborough & Brady, NRP) showing phonics instruction accelerates decoding acquisition. Whole-language and balanced-literacy researchers counter that decoding gains do not persist when reading engagement and comprehension support are removed, and that the NRP meta-analysis conflates intervention (phonics instruction) with comparison conditions (often minimal/no instruction) rather than comparing phonics to other active reading instruction. Longitudinal outcome studies are cited by both sides with contradictory interpretations. No consensus corroborating authority outside the phonics-advocacy camp attests that the founding problem persists as stated.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_sufficiency_ambiguity,
    'Is explicit phonics instruction sufficient to produce reading comprehension and engagement, or is it necessary but insufficient—requiring integration with meaning-centered instruction, vocabulary building, and literature exposure?',
    'Longitudinal randomized controlled trials comparing phonics-only curricula to balanced or meaning-centered curricula, measured on long-term reading comprehension, reading volume, and reading engagement through grade 6+. Post-intervention measurement (children after exposure) to assess retention and transfer.',
    'If phonics is sufficient, the constraint is justified: decoding IS the primary reading skill and systematic phonics the legitimate core of instruction. If insufficient, the constraint is extractive: it privileges one necessary component over others, reducing classroom time for essential comprehension and engagement work. The classification could shift from tangled_rope (justified extraction + coordination) to snare (pure extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_sufficiency_ambiguity, empirical, 'Whether decoding automaticity is sufficient for reading comprehension and sustained engagement, or requires integration with meaning-making instruction.').

omega_variable(
    identity_locked_suppression_mechanism,
    'For meaning-centered educators whose professional identity is anchored to the belief that reading is comprehension and meaning-construction, is the suppression of alternative pedagogies structural (external mandates, materials unavailable, evaluation tied to phonics fidelity) or internalized (the educator has accepted the phonics-first framing as legitimate science)?',
    'Post-mandate narrative interviews and career-trajectory analysis: educators who exit the profession or transfer to non-mandated contexts, educators who retrain and integrate phonics while maintaining meaning-centered elements, educators who remain and perform phonics compliance while privately sustaining alternative approaches. Measurement of belief-change and identity-shift across the interval.',
    'If suppression is primarily structural, the constraint''s effective suppression is partially reversible by mandate removal. If internalized, the constraint carries suppression with it into alternative career contexts—the educator has absorbed the legitimacy claim and doubts their own pedagogy even outside the mandated system. The constraint''s effective extraction is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_mechanism, empirical, 'Whether suppression of whole-language and balanced approaches is externally enforced or internalized by educators as loss of legitimate authority.').

omega_variable(
    alternative_reading_pathways,
    'Does the constraint''s decoding-first sequencing work equally well for children whose primary reading difficulty is phonological (where explicit phonics is optimized) versus comprehension, vocabulary, or engagement (where meaning-centered routes may be more efficient)?',
    'Subgroup analysis in literacy intervention studies, stratified by reading-difficulty profile at baseline (phonological vs. comprehension vs. vocabulary). Measurement of skill acquisition rate and long-term outcome by profile and instructional approach.',
    'If decoding-first is equally effective across profiles, the constraint is justified as one-size-fits-all instruction. If effectiveness varies by profile (phonics-optimized for phonological difficulty; meaning-centered faster for vocabulary/comprehension difficulty), the constraint is extractive for non-phonological readers—it forces them through an inefficient pathway and uses up classroom time that could be allocated to their specific difficulty. The constraint would be revealed as fitting some readers well and harming others'' learning trajectories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_pathways, empirical, 'Whether phonics-first sequencing is equally effective for all reading-difficulty profiles or whether subgroups learn faster through alternative pathways.').

omega_variable(
    kernel_reading_vs_phonics_reading_divergence,
    'The contested literacy kernel has four readings (whole_language_meaning_primacy, balanced_literacy_integration, structured_literacy_remediation, and this one: phonics_decoding_primacy). This reading instantiates the claim that reading IS decoding, and the alphabetic principle requires explicit systematic instruction. But does the empirical evidence support this reading, or do alternative readings (that decoding emerges through meaningful engagement, or that the kernel should privilege vulnerable learners'' needs) better predict long-term outcomes?',
    'Meta-analytic synthesis of reading-outcome studies (reading comprehension, reading volume, reading engagement, reading fluency) comparing instruction organized around each of the four readings. Mediation analysis to identify which reading''s core premises (decoding primacy, meaning-centered immersion, balance, or structured progression) predict long-term success. Historical analysis of which reading has empirical evidence primarily from its own advocates versus corroboration from independent researchers.',
    'If phonics_decoding_primacy reading shows strongest long-term outcome evidence across all populations, the constraint is justified as evidence-based coordination. If alternative readings show equal or stronger outcomes for some populations (e.g., meaning-centered for children with strong motivation, structured-literacy for students with severe phonological deficits), or if the reading''s evidence base is primarily self-authored by its advocates, the constraint is revealed as sustained by institutional inertia and resource-capture rather than empirical warrant. The classification could shift from tangled_rope to piton (inertia-driven persistence with theater maintenance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_phonics_reading_divergence, empirical, 'Whether the phonics_decoding_primacy reading''s core empirical claims are corroborated by independent researchers or primarily by advocates of the reading itself.').

omega_variable(
    kernel_reading_legitimacy_competition,
    'This constraint instantiates one reading of the literacy-instruction kernel. The sibling readings (whole_language_meaning_primacy, balanced_literacy_integration, structured_literacy_remediation) represent genuinely alternative legitimate framings of how reading is acquired and how instruction should be organized. Does the current policy-enforced primacy of the phonics_decoding_primacy reading reflect genuine empirical superiority, or does it reflect resource-capture and institutional entrenchment by the phonics-advocacy community?',
    'Historical analysis of the network of researchers, publishers, and policy advisors who promoted the phonics_decoding_primacy reading from 2000-present. Funding-source analysis for literacy research (which reading''s research is funded by publishers, federal grants, advocacy organizations). Analysis of policy-committee composition and whose voices were included/excluded in reading-standards adoption. Comparison of effect-size estimates and study quality across readings'' evidence bases.',
    'If the phonics reading''s primacy reflects genuine empirical superiority, the constraint is justified and the extraction is coordination cost. If it reflects institutional capture (publishers profiting from phonics materials, remediation specialists benefiting from early-identification systems, phonics-trained policy advisors excluding alternative voices), the constraint is a snare—coordination is the cover story; extraction is the function. The classification could shift from tangled_rope to snare, and mandatrophy would be confirmed (founding problem is contested, enforcement is theater, beneficiaries are captured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy_competition, conceptual, 'Whether the phonics_decoding_primacy reading''s policy dominance reflects empirical warrant or institutional capture and resource-concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(read_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(read_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(read_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.06).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested literacy-acquisition kernel. The kernel codifies how reading is understood (decoding vs. meaning-making vs. integration) and how legitimate instruction must be organized. Four sibling readings instantiate different answers to the kernel question; this story represents the phonics_decoding_primacy reading. The sibling constraints (whole_language_meaning_primacy, balanced_literacy_integration, structured_literacy_remediation) are not part of this story—they are separate stories with their own ε, stakeholders, and types. The network links indicate structural influence: this reading's policy dominance affects the legitimacy conditions and resource availability for sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
