% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading of the Literacy Acquisition Kernel
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the 'balanced literacy' reading of the
 *   literacy_acquisition_kernel: the claim that systematic phonics
 *   instruction and meaningful text engagement are complementary components
 *   of a single well-designed instructional program, and that the reading
 *   wars were a false dichotomy resolved by combining both. This is a
 *   distinct constraint from the phonics_reading, whole_language_reading, and
 *   structured_literacy_reading siblings — each of those instantiates a
 *   different beneficiary/victim structure and a different epsilon. The
 *   balanced-literacy reading's distinguishing structural feature is that its
 *   coordination function (ending public-facing method polarization) is real,
 *   but its beneficiary set (publishers who monetize periodic curriculum
 *   churn, schools of education protecting institutional continuity, literacy
 *   coaches protecting professional standing) captures value regardless of
 *   whether the underlying instructional synthesis is actually delivered with
 *   fidelity in classrooms. The persistent presence of three-cueing materials
 *   inside programs marketed as 'balanced' is the empirical signature that
 *   some 'balance' implementations are whole language with a phonics veneer
 *   rather than genuine synthesis — this is exactly the uncertainty the omega
 *   variables below are built to hold open rather than resolve by fiat.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.38).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading of the Literacy Acquisition Kernel").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'c002312c-0a5a-4a1a-9a15-a707d79e67e8').
narrative_ontology:cs_kernel_codification('c002312c-0a5a-4a1a-9a15-a707d79e67e8', distributed).
narrative_ontology:cs_authority_grounding('c002312c-0a5a-4a1a-9a15-a707d79e67e8', distributed).
narrative_ontology:cs_reading_relation('c002312c-0a5a-4a1a-9a15-a707d79e67e8', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('c002312c-0a5a-4a1a-9a15-a707d79e67e8', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('c002312c-0a5a-4a1a-9a15-a707d79e67e8', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('c002312c-0a5a-4a1a-9a15-a707d79e67e8', foundational, complementarity_of_decoding_and_meaning).
narrative_ontology:cs_axiom_status(complementarity_of_decoding_and_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c002312c-0a5a-4a1a-9a15-a707d79e67e8', complementarity_of_decoding_and_meaning, empirically_contingent).
narrative_ontology:cs_axiom('c002312c-0a5a-4a1a-9a15-a707d79e67e8', secondary, reading_wars_dichotomy_is_false).
narrative_ontology:cs_axiom_status(reading_wars_dichotomy_is_false, holdable).
narrative_ontology:cs_axiom_grounding('c002312c-0a5a-4a1a-9a15-a707d79e67e8', reading_wars_dichotomy_is_false, conventional).
narrative_ontology:cs_created_at('c002312c-0a5a-4a1a-9a15-a707d79e67e8', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, district_literacy_coaches).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_early_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell 'balanced literacy' curriculum packages and re-badge existing whole-language-adjacent materials with added phonics components. Benefit from being able to market to both camps simultaneously and from periodic curriculum adoption cycles as districts chase the current consensus.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Train new teachers under a balanced-literacy framework that lets faculty retain much of an existing whole-language-oriented syllabus while adding a phonics module, avoiding the reputational cost of admitting the prior framework underperformed. Set state certification standards that require exposure to 'balanced' methods.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education, agenda_setter).

% Administer professional development and select classroom materials under the balanced-literacy banner. Their career standing depends on the framework's continued institutional legitimacy; abandoning it for structured literacy would require admitting past guidance was insufficient.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, district_literacy_coaches, agenda_setter,
    moderate, biographical, constrained, regional).

% Implement whatever 'balance' the district's chosen materials specify, often with inconsistent phonics scope-and-sequence and no explicit decoding progression. Bear the burden of reconciling contradictory guidance in real time with limited training in phonemic awareness diagnostics. Cannot unilaterally switch programs without administrative approval.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Sit in classrooms where phonics instruction is present but often unsystematic and insufficiently cumulative, layered alongside contextual-guessing strategies inherited from whole language (e.g., picture cues, three-cueing). Without systematic decoding instruction they can fall behind and the 'balance' framing delays identification of the specific deficit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_early_readers, payer,
    powerless, biographical, trapped, local).

% Require the most systematic, explicit, cumulative phonics instruction to compensate for weak phonological processing. A 'balanced' approach that treats phonics as one ingredient among several, delivered inconsistently, is least protective for exactly this population; they cannot select their own instructional method.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Study national assessment data (e.g., NAEP-style reading scores) and meta-analyses of instructional method efficacy. Some read balanced literacy as a genuine synthesis under contested implementation fidelity; others read it as whole language's institutional continuation under new branding, citing the persistence of three-cueing materials inside 'balanced' programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_researchers, observer,
    analytical, generational, analytical, global).

% Rarely have visibility into which specific instructional components their child's classroom actually delivers under the 'balanced' label, or in what proportion; advocacy groups (e.g. dyslexia parent coalitions) that have pushed for structured literacy legislation are the main channel through which this voice reaches policy, but individual parents in most districts are not consulted on method selection.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents_of_early_readers, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework that lets a school district adopt a single coherent-sounding instructional policy instead of choosing between two polarized, politically charged camps — reducing the coordination cost of committing publicly to either 'pure phonics' or 'pure whole language.'
% TRANSFER_FUNCTION: Moves curriculum-adoption revenue and professional-development contracts toward publishers and consultants who can rebrand existing materials as 'balanced,' and moves instructional-fidelity risk onto classroom teachers and, ultimately, onto students whose decoding skill depends on how systematically phonics is actually delivered inside the 'balance.'
% ABSENT_VOICES: Individual parents of early readers and the students themselves have no seat in curriculum-adoption decisions; dyslexia-advocacy coalitions partially represent this voice at the legislative level but are not present in day-to-day district method selection, where the balanced framing is set and re-set.
% DISAPPEARANCE_RATIONALE: If 'balanced literacy' as an institutional label disappeared overnight, districts would have to choose explicitly between structured/systematic phonics-first approaches and something else — publishers argue this would be chaotic and costly to re-train for; structured-literacy advocates argue it would simply force honesty about which components are actually being delivered, and that little of substance would be lost because the 'balance' was already inconsistently implemented.
% FOUNDING_PROBLEM: The 1990s-2000s 'reading wars' pitted whole language against phonics-first instruction in bitter, polarized public and academic conflict; balanced literacy was proposed to end the conflict by asserting both camps were partially right and could be integrated.
% FOUNDING_PROBLEM_CORROBORATION: Curriculum publishers and schools of education attest the synthesis is real and functioning. Independent reading researchers analyzing national assessment trends (e.g. persistent proportions of students below basic reading benchmarks) and dyslexia-advocacy organizations outside the balanced-literacy institutional apparatus attest that 'balance' in practice has often meant token or inconsistent phonics instruction layered onto a whole-language-descended core, with three-cueing strategies persisting in materials marketed as balanced — an outside-the-beneficiary reading that the founding problem (polarization) was resolved rhetorically but not instructionally.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) rather than high because the coordination function is genuinely operative in well-implemented programs — some districts do deliver systematic, cumulative phonics alongside rich text engagement, and those implementations look close to a rope. But the label is loosely enough specified, and adoption-cycle economics reward publishers enough, that low-fidelity implementations extract adoption/PD revenue without delivering the systematic phonics component dyslexic and struggling readers specifically need. Theater ratio rises over the interval (0.30 to 0.48) tracking the accumulating gap between the rhetoric of 'balance' (stable across implementations) and actual instructional fidelity (increasingly variable and, in many audited districts, phonics-light) as documented in post-2019 'Sold a Story'-era investigative reporting. Suppression is comparatively low (0.38) because the reading does not require aggressively suppressing alternatives — its persistence rests more on institutional inertia, credentialing requirements, and publisher lock-in than active enforcement, though districts requiring balanced-literacy-branded materials in teacher certification create real (if moderate) suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and schools of education sit near the beneficiary end: they capture adoption and certification revenue and reputational continuity regardless of classroom-level fidelity, and their exit options are effectively arbitrage (repackage under whatever label wins next). District coaches are a secondary beneficiary/agenda-setter whose career capital is invested in the framework's continued legitimacy. Classroom teachers are payers with constrained exit — they implement centrally-selected materials without authority to switch programs. Struggling early readers and dyslexic students are the clearest targets: powerless, trapped (a six-year-old cannot select their own reading program), and specifically harmed when the 'balance' in their classroom under-delivers the systematic phonics component their reading development actually requires. Parents are excluded from the loop that selects the label their children will be taught under.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — polarized, politically weaponized reading-instruction debate — is genuinely dead in the sense that a synthesis position is now academically defensible and widely adopted; but the arrangement's persistence as an institutional label does not guarantee the synthesis is actually delivered, and 'balanced literacy' can function as a mandatrophied survival of whole-language's institutional position (same schools of education, same coaches, same publisher relationships) wearing a phonics-inclusive name. The tangled_rope classification captures this: a real coordination function (ending public dichotomy-framing) coexists with asymmetric extraction (publishers and institutions capture value from label continuity independent of classroom fidelity, at the expense of the specific populations — struggling and dyslexic readers — who need the systematic component most when it is diluted).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine third theoretical position that integrates systematic phonics with meaningful text engagement, or is it institutionally whole language''s continuation under a new label that added minimal, unsystematic phonics content to satisfy political pressure without changing core practice (e.g., retained three-cueing strategies)?',
    'Audit of actual classroom materials and instructional minutes labeled ''balanced literacy'' across a representative district sample: measure whether phonics instruction meets structured-literacy fidelity criteria (explicit, systematic, cumulative, diagnostic) versus incidental/embedded exposure, and whether three-cueing or similar whole-language-descended strategies remain present.',
    'If audits show systematic fidelity, this reading is closer to a rope (real synthesis, moderate extraction from adoption economics only). If audits show persistent whole-language core practices with token phonics, this reading is closer to a snare wearing coordination language, and the tangled_rope classification understates victim harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, empirical, 'Whether balanced literacy delivers genuine instructional synthesis or rebrands whole language.').

omega_variable(
    kernel_reading_multiplicity_or_collapse,
    'Does the literacy_acquisition_kernel genuinely support four distinct readings (phonics, whole_language, structured_literacy, balanced_literacy), or does balanced_literacy collapse into whole_language once implementation fidelity is accounted for, leaving only three genuinely distinct positions?',
    'Compare the axiom sets and reading_relations across all four sibling stories; if balanced_literacy''s foundational axiom (complementarity_of_decoding_and_meaning) is empirically indistinguishable in practice from whole_language''s core commitment once ''unsystematic phonics inclusion'' is treated as noise rather than signal, the kernel may only support three readings.',
    'If balanced_literacy collapses into whole_language, this story should be understood as documenting a contested boundary case rather than a stable fourth reading — future corpus work might merge or explicitly mark this reading as unstable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity_or_collapse, conceptual, 'Whether balanced literacy is a stable fourth kernel reading or an unstable variant of whole language.').

omega_variable(
    beneficiary_capture_independent_of_fidelity,
    'Do curriculum publishers and schools of education benefit from the balanced-literacy label regardless of whether classroom-level instructional fidelity is high or low, such that their incentive to defend the label is decoupled from measurable student outcomes?',
    'Track publisher revenue and school-of-education program enrollment/certification-standard changes against independent reading-outcome data (e.g., state or national assessment trends) over the same period; decoupling would appear as revenue/enrollment stability or growth despite flat or declining independent outcome measures.',
    'If decoupled, the beneficiary structure is closer to pure extraction riding on the coordination label; if outcome-linked, the beneficiaries'' interests are more aligned with genuine synthesis and the tangled_rope''s coordination component is stronger relative to its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_independent_of_fidelity, empirical, 'Whether publisher and institutional benefit from the label tracks or is independent of student outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the colloquial 'reading wars' / literacy_acquisition_kernel debate. Each sibling reading is authored as a separate file with its own epsilon, beneficiary/victim structure, and claimed_type, per the ε-invariance principle: phonics_reading and structured_literacy_reading emphasize systematic decoding-first instruction (differing chiefly in scope of application and diagnostic tradition); whole_language_reading rejects explicit decoding instruction as necessary; balanced_literacy_reading (this story) claims to integrate the phonics and whole-language positions but is itself contested as either genuine synthesis or institutional continuation of whole_language practice under new branding. The influences edge to whole_language_reading reflects that balanced_literacy's political success created downstream legitimacy pressure making pure whole_language framing harder to defend publicly, without foreclosing it as a position some practitioners and scholars still hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
