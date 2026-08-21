% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'whole language' approach to reading
 *   acquisition, which posits that decoding skills emerge implicitly from
 *   meaningful engagement with authentic texts, rather than through explicit
 *   phonics instruction. It is one reading of the broader
 *   'reading_acquisition_mechanism' kernel. The approach gained significant
 *   traction in the late 20th century, leading to widespread adoption in
 *   educational systems, despite mounting evidence from cognitive science
 *   supporting explicit phonics. The structural delta for this reading
 *   includes low initial instructional cost (no systematic sequence), high
 *   long-term remediation cost for struggling readers, maximized teacher
 *   autonomy, and disproportionate harm to struggling readers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '346b6354-e77a-40d2-9bcd-653814f72060').
narrative_ontology:cs_kernel_codification('346b6354-e77a-40d2-9bcd-653814f72060', implicit).
narrative_ontology:cs_authority_grounding('346b6354-e77a-40d2-9bcd-653814f72060', practice).
narrative_ontology:cs_interpretation_layer_present('346b6354-e77a-40d2-9bcd-653814f72060').
narrative_ontology:cs_reading_relation('346b6354-e77a-40d2-9bcd-653814f72060', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('346b6354-e77a-40d2-9bcd-653814f72060', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('346b6354-e77a-40d2-9bcd-653814f72060', foundational, meaning_first_decoding_emerges).
narrative_ontology:cs_axiom_status(meaning_first_decoding_emerges, holdable).
narrative_ontology:cs_axiom_grounding('346b6354-e77a-40d2-9bcd-653814f72060', meaning_first_decoding_emerges, conventional).
narrative_ontology:cs_axiom('346b6354-e77a-40d2-9bcd-653814f72060', secondary, authentic_texts_are_primary).
narrative_ontology:cs_axiom_status(authentic_texts_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('346b6354-e77a-40d2-9bcd-653814f72060', authentic_texts_are_primary, conventional).
narrative_ontology:cs_reference_frame('346b6354-e77a-40d2-9bcd-653814f72060', natural_language_acquisition_model).
narrative_ontology:cs_drift_state('346b6354-e77a-40d2-9bcd-653814f72060', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('346b6354-e77a-40d2-9bcd-653814f72060', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, publishers_of_authentic_texts).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, often through teacher training programs, curriculum development, and academic publications. They benefit from the philosophical alignment with constructivist pedagogy and the autonomy it grants teachers. Their professional identity is deeply tied to this approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the emphasis on using 'authentic' literature rather than phonics-specific readers, leading to higher sales of trade books and diverse classroom libraries. They are less invested in the pedagogical debate itself but profit from its curricular implications.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, publishers_of_authentic_texts, beneficiary,
    organized, biographical, mobile, national).

% Disproportionately harmed by the lack of explicit decoding instruction, often failing to acquire foundational reading skills. They bear the long-term costs of illiteracy, including academic failure and limited life opportunities. Their exit options are extremely limited, often requiring expensive private tutoring or specialized intervention.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Bear the emotional and financial burden of their children's reading difficulties, often seeking and paying for supplemental instruction outside the school system. They are constrained by school district policies and the limited availability of effective alternatives.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, immediate, constrained, local).

% Often trained in whole language methods and expected to implement them, which can lead to frustration and burnout when students fail to thrive. They benefit from the pedagogical autonomy but pay the cost in student outcomes and professional stress. Their career progression is tied to adherence to district-mandated curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, beneficiary).

% Conduct research on reading acquisition, often finding strong evidence for the necessity of explicit phonics instruction. They observe the outcomes of different pedagogical approaches and provide evidence-based critiques, but their influence on policy can be slow and indirect.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes holistic meaning-making and integrates reading with other language arts, fostering a love of reading through authentic literature.
% TRANSFER_FUNCTION: Transfers pedagogical authority and autonomy to teachers, allowing them to follow student interests and use diverse texts, while implicitly transferring the burden of decoding skill acquisition to the student, particularly those with less prior literacy exposure.
% ABSENT_VOICES: Neuroscientists and educational psychologists whose research on the cognitive mechanisms of reading acquisition strongly supports explicit phonics instruction are often marginalized in pedagogical discourse, their findings dismissed as reductionist or irrelevant to classroom practice.
% DISAPPEARANCE_RATIONALE: If the whole language approach vanished overnight, educational policy and teacher training would rapidly shift towards more explicit, systematic phonics instruction. Curriculum materials would change, and the focus on 'authentic texts' as the primary acquisition mechanism would be replaced by a more balanced approach, fundamentally altering how reading is taught and learned.
% FOUNDING_PROBLEM: Traditional phonics instruction was often rote, decontextualized, and failed to engage students with the joy of reading, leading to a generation of 'word callers' who could decode but not comprehend.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem is still live, arguing that overemphasis on phonics stifles comprehension and motivation. Cognitive scientists and many parents of struggling readers attest that while engagement is important, the core problem of decoding for many students was not adequately addressed by whole language, and that the pendulum swung too far, creating new problems for foundational skill acquisition. Independent educational research and longitudinal studies from outside the benefiting parties support the view that the founding problem was partially addressed but new, significant problems were created.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it places a significant, often unacknowledged, burden on struggling readers to 'discover' decoding rules, leading to academic failure and the need for costly remediation. Suppression (0.70) is high due to the institutional inertia and ideological commitment within educational faculties that actively resisted evidence-based critiques and suppressed alternative pedagogical approaches. The theater ratio (0.40) reflects that while the stated goal is fostering a love of reading, a substantial portion of the effort goes into defending the pedagogical philosophy against scientific evidence, rather than genuinely optimizing for universal reading acquisition. The initial lower extractiveness and suppression reflect the early, more genuinely experimental phase of the approach, which then hardened into an entrenched position.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, the approach is a 'rope' that fosters holistic literacy and a love of reading, with any 'extraction' being a necessary cost of a richer educational experience. From the perspective of struggling readers and cognitive scientists, it operates as a 'snare' or 'tangled rope,' extracting significant costs in literacy outcomes due to a flawed pedagogical premise, sustained by institutional suppression of evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and publishers of authentic texts are beneficiaries, gaining professional influence and market share, respectively. Struggling readers and their parents are clear victims, bearing the direct costs of inadequate instruction. Early career teachers are in a dual role, benefiting from pedagogical autonomy but paying the cost in student outcomes and professional stress. Cognitive scientists act as observers, providing critical evidence but often excluded from policy-making.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling it as a pure coordination mechanism (Rope) by highlighting the asymmetric extraction from struggling readers and the active enforcement required to maintain the pedagogical paradigm against scientific evidence. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, albeit often unrealized, coordination function of fostering engagement with literature. The founding problem (rote phonics) was real, but the solution created new, significant problems, leading to a 'contested' status for the founding problem and a 'world_rearranges' verdict for its disappearance, indicating its constructed and impactful nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_implicit_decoding,
    'Does robust empirical evidence support the claim that decoding skills emerge implicitly from exposure to authentic texts for the majority of learners, particularly those at risk for reading difficulties?',
    'Meta-analyses of randomized controlled trials comparing explicit phonics instruction to whole language or balanced literacy approaches, with a focus on decoding outcomes for diverse learner populations.',
    'If evidence strongly refutes implicit decoding, the constraint''s ''coordination'' function (fostering love of reading) would be re-evaluated as a cover for pedagogical extraction, shifting classification towards Snare. If some evidence supports it for a subset of learners, it would refine the understanding of who benefits and who is harmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_validity_of_implicit_decoding, empirical, 'The scientific validity of the core pedagogical premise.').

omega_variable(
    pedagogical_ideology_vs_scientific_evidence,
    'To what extent does adherence to the whole language approach stem from a philosophical commitment to constructivist pedagogy and teacher autonomy, rather than from an evidence-based assessment of reading acquisition mechanisms?',
    'Qualitative studies of teacher training programs and educational policy debates, analyzing the arguments used to defend whole language in the face of contradictory scientific evidence. Examination of professional identity formation among educators.',
    'If ideological commitment is the primary driver, the ''suppression'' metric would be understood as largely internalized within the educational community, making the constraint more resilient to external empirical challenges and harder to dislodge. This would reinforce its Tangled Rope nature, highlighting the identity-locked beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_ideology_vs_scientific_evidence, conceptual, 'The role of ideology in maintaining the pedagogical approach.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine reading of the ''reading_acquisition_mechanism'' kernel, or a distinct, independent constraint?',
    'Analysis of the core premises: if the ''whole language'' approach fundamentally redefines the mechanism of acquisition in a way that is irreconcilable with other readings (e.g., phonics), it is a distinct constraint. If it merely emphasizes different aspects of a shared underlying mechanism, it is a reading.',
    'If it is a distinct constraint, it would be re-evaluated as a standalone entity, potentially altering its network relationships and the interpretation of its ''coexistence'' with other pedagogical approaches. If it is a reading, the analysis of its relations to sibling readings (phonics_reading, balanced_literacy_reading) would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifies whether ''whole language'' is a reading or a separate constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, curriculum_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, teacher_training_accreditation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel. Its structural properties and classification are distinct from sibling readings like 'phonics_reading' and 'balanced_literacy_reading', which represent alternative pedagogical approaches to the same core problem of how children learn to read. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
