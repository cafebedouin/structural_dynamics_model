% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'whole language' approach to reading
 *   instruction, which prioritizes immersing children in authentic literature
 *   and fostering a love of reading, with the belief that decoding skills
 *   will emerge naturally. It is a specific reading of the broader
 *   'reading_acquisition_legitimacy' kernel, emphasizing meaning-making over
 *   explicit phonics. The constraint is actively enforced through teacher
 *   training, curriculum adoption, and policy advocacy, despite mounting
 *   empirical evidence for the necessity of systematic phonics instruction
 *   for many learners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.6).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.7).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'd8db1957-c5b2-4d41-9e6d-2077bedc1f6a').
narrative_ontology:cs_kernel_codification('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', implicit).
narrative_ontology:cs_authority_grounding('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', lineage).
narrative_ontology:cs_interpretation_layer_present('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a').
narrative_ontology:cs_reading_relation('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_axiom('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', foundational, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', reading_is_meaning_making, deontological).
narrative_ontology:cs_axiom('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', foundational, decoding_emerges_naturally).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally, holdable).
narrative_ontology:cs_axiom_grounding('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', decoding_emerges_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', child_centered_holistic_literacy).
narrative_ontology:cs_drift_state('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d8db1957-c5b2-4d41-9e6d-2077bedc1f6a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers_whole_language).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_lacking_phonics_training).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_lacking_phonics_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, often holding positions in teacher education, curriculum development, and policy-making. Their professional identity is deeply tied to this pedagogical philosophy, making alternative approaches difficult to adopt.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Profit from the sale of authentic literature sets, leveled readers, and teacher guides aligned with whole language principles. They benefit from the continued adoption of this approach in schools and districts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers_whole_language, beneficiary,
    organized, biographical, mobile, national).

% Are immersed in literature without explicit, systematic decoding instruction, often failing to develop foundational reading skills. They bear the cost of delayed literacy acquisition, impacting academic performance and future opportunities.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Observe their children's difficulties and often seek external tutoring or alternative educational settings, incurring significant financial and emotional costs. Their options are limited by school district policies and available resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Are trained in whole language methods and may lack the pedagogical tools or confidence to implement explicit phonics instruction. They may experience professional identity conflict if required to adopt different methods, even if they see students struggling.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_lacking_phonics_training, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_lacking_phonics_training, beneficiary).

% Conduct research demonstrating the efficacy of systematic phonics instruction, but their findings are often marginalized or dismissed by whole language proponents in policy and practice. They advocate for evidence-based literacy policies.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_researchers, excluded,
    powerful, generational, analytical, global).

% Are tasked with setting literacy standards and curriculum guidelines. They navigate conflicting pedagogical philosophies and political pressures, often seeking to balance different approaches or respond to public outcry over reading outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, education_policymakers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes reading for meaning and immersion in authentic texts, aiming to foster a love of reading and natural language acquisition.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design towards a meaning-first approach, often at the expense of explicit, systematic decoding instruction, impacting student outcomes and teacher training.
% ABSENT_VOICES: Phonics researchers and advocates for structured literacy are often excluded from curriculum design and policy-making discussions, despite a strong evidence base for explicit decoding instruction. Their voices would highlight the costs borne by struggling readers.
% DISAPPEARANCE_RATIONALE: If the whole language meaning primacy constraint vanished, educational institutions would rapidly shift towards more explicit and systematic phonics instruction, curriculum materials would change, and teacher training would be reformed. This would significantly alter literacy outcomes for many children.
% FOUNDING_PROBLEM: Traditional phonics instruction was often seen as dry, decontextualized, and stifling to a love of reading, leading to a desire for more engaging, holistic, and meaning-centered approaches to literacy.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem of disengaged readers is still live. However, a broad consensus among cognitive scientists and reading researchers, along with longitudinal studies of reading outcomes, corroborates that the founding problem of 'stifling a love of reading' is largely superseded by the problem of insufficient decoding skills, and that the whole language approach itself creates new problems for many learners.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the opportunity cost borne by struggling readers who do not acquire decoding skills efficiently under this model, and the financial/emotional costs to their families. Suppression (0.7) is high due to the institutional entrenchment of whole language ideology in teacher education and curriculum, making it difficult for teachers or districts to adopt alternative, evidence-based methods without significant resistance. The theater ratio (0.4) reflects that while fostering a love of reading is a genuine goal, a significant portion of the effort goes into defending the pedagogical approach against scientific evidence, rather than solely focusing on effective literacy outcomes for all children. The slight decrease in extractiveness, suppression, and theater ratio by 2020 reflects the increasing pressure from the 'Science of Reading' movement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, this is a 'rope' that coordinates a humane, child-centered approach to literacy. From the perspective of struggling readers and their families, it operates as a 'snare' that traps children in ineffective methods. The engine's classification as a 'tangled_rope' reflects this hybrid nature, acknowledging both the coordination function (fostering engagement with literature) and the asymmetric extraction (costs to struggling learners).
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and aligned curriculum publishers are clear beneficiaries, as their professional identities and economic models are tied to this approach. Struggling readers and their parents are victims, bearing the direct costs of ineffective instruction. Teachers trained in whole language are also payers, as they may lack the tools to help all students and face professional challenges if the paradigm shifts, but they also benefit from the established pedagogical framework. Phonics researchers are excluded, as their evidence challenges the core tenets of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_emergence_naturalness,
    'Does decoding truly ''emerge naturally'' for all children through immersion in authentic literature, or is explicit instruction necessary for a significant portion of learners?',
    'Longitudinal studies comparing literacy outcomes of children taught exclusively via whole language vs. those receiving explicit phonics instruction, particularly for children with varying cognitive profiles.',
    'If explicit instruction is found necessary for many, the ''natural emergence'' claim becomes a cover story, increasing the constraint''s effective extractiveness and suppression, pushing it closer to a Snare. If natural emergence is broadly true, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoding_emergence_naturalness, empirical, 'Empirical validity of the ''natural emergence'' hypothesis for decoding skills.').

omega_variable(
    pedagogical_identity_lock,
    'To what extent is the persistence of whole language pedagogy driven by genuine belief in its efficacy versus professional identity lock-in among educators and policymakers?',
    'Surveys and qualitative studies of educators'' willingness to adopt evidence-based practices that contradict their initial training, even when presented with compelling data on student outcomes.',
    'If identity lock-in is a primary driver, the suppression metric is higher due to internalized barriers to change, and the constraint''s persistence is more resistant to empirical challenge, indicating a stronger ''piton'' or ''snare'' element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_lock, conceptual, 'Role of professional identity in maintaining whole language pedagogy despite contradictory evidence.').

omega_variable(
    curriculum_market_capture,
    'Is the market for literacy curricula genuinely competitive, or is it captured by publishers aligned with specific pedagogical ideologies (e.g., whole language) that resist evidence-based shifts?',
    'Analysis of curriculum adoption patterns, market share of different pedagogical approaches, and lobbying efforts by publishers in educational policy-making.',
    'If market capture is significant, the ''beneficiary'' role of curriculum publishers is amplified, and the constraint''s extractiveness is higher due to reduced competition and suppressed alternatives for schools seeking different materials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curriculum_market_capture, empirical, 'Degree of market capture in literacy curriculum publishing by whole language aligned entities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'reading_acquisition_legitimacy' kernel, each representing a distinct pedagogical approach to literacy. They are linked as a constraint family because they compete for adoption in educational policy and practice, influencing each other's legitimacy and resource allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
