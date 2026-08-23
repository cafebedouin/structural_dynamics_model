% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Instructional Regime (Integrated Phonics + Authentic Texts)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   In the mid-1990s, after four decades of pendulum war between phonics
 *   drills and whole-language immersion, Anglophone schooling settled on a
 *   compromise branded balanced literacy: explicit sound-letter instruction
 *   admitted as a component, authentic literature kept as the organizing
 *   spine, the two joined in 'integrated practice.' The constraint analyzed
 *   here is that settlement as an operating instructional regime. The
 *   referent for every authored number is the regime as actually implemented
 *   in adopting districts, judged by this reading's own lights — which
 *   endorse integration and therefore weigh the regime by its fidelity to it.
 *   The claimed type is authored independently of the metrics: the reading
 *   holds that the arrangement genuinely coordinates the profession around a
 *   workable synthesis while its operation concentrates real costs on
 *   students who needed systematic code instruction and never received it.
 *   The sibling readings — phonics_reading, which makes systematic
 *   instruction foundational, and whole_language_reading, which expects
 *   decoding to emerge implicitly — are separate files in this kernel family,
 *   linked through network.affects_constraints; each carries its own epsilon
 *   over its own instantiation. KEY AGENTS (by structural relationship): -
 *   teacher_preparation_faculties: agenda-setting seat
 *   ([institutional]/[identity_locked]) — certifies the method, staffs its
 *   journals, reproduces its lineage - leveled_text_publishers:
 *   agenda-setting seat that also collects ([institutional]/[arbitrage]) —
 *   sells the benchmark kits and leveled libraries the regime runs on -
 *   balanced_literacy_pd_consultants: beneficiary ([organized]/[constrained])
 *   — method-specific training income - district_curriculum_offices:
 *   beneficiary with cost exposure ([organized]/[constrained]) — runs
 *   adoptions, absorbs fallout - classroom_teachers: dual-positioned
 *   implementer ([organized]/[constrained]) — planning relief on one side,
 *   blame on the other - struggling_readers: primary target
 *   ([powerless]/[trapped]) — instruction withheld, exit impossible -
 *   dyslexic_students: most-exposed target ([powerless]/[trapped]) — the
 *   implicit-code route is unavailable to them - literate_home_students:
 *   incidental beneficiary ([powerless]/[constrained]) — home capital
 *   substitutes for missing instruction - parents_of_struggling_readers:
 *   paying advocates ([organized]/[constrained]) — buy privately what school
 *   did not teach - remediation_taxpayers: deferred payers
 *   ([moderate]/[constrained]) — fund the downstream tiers -
 *   state_science_of_reading_legislatures: analytical observer acting on
 *   evidence ([institutional]/[analytical]) — statutory remedies resetting
 *   adoption
 *
 * KEY AGENTS:
 *   - teacher_preparation_faculties: agenda-setting seat ([institutional]/[identity_locked]) — certifies the method, staffs its journals, reproduces its lineage
 *   - leveled_text_publishers: agenda-setting seat that also collects ([institutional]/[arbitrage]) — sells the benchmark kits and leveled libraries the regime runs on
 *   - balanced_literacy_pd_consultants: beneficiary ([organized]/[constrained]) — method-specific training income
 *   - district_curriculum_offices: beneficiary with cost exposure ([organized]/[constrained]) — runs adoptions, absorbs fallout
 *   - classroom_teachers: dual-positioned implementer ([organized]/[constrained]) — planning relief on one side, blame on the other
 *   - struggling_readers: primary target ([powerless]/[trapped]) — instruction withheld, exit impossible
 *   - dyslexic_students: most-exposed target ([powerless]/[trapped]) — the implicit-code route is unavailable to them
 *   - literate_home_students: incidental beneficiary ([powerless]/[constrained]) — home capital substitutes for missing instruction
 *   - parents_of_struggling_readers: paying advocates ([organized]/[constrained]) — buy privately what school did not teach
 *   - remediation_taxpayers: deferred payers ([moderate]/[constrained]) — fund the downstream tiers
 *   - state_science_of_reading_legislatures: analytical observer acting on evidence ([institutional]/[analytical]) — statutory remedies resetting adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Instructional Regime (Integrated Phonics + Authentic Texts)").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'b6638aa3-dd9f-4402-80c6-58ae3900128e').
narrative_ontology:cs_kernel_codification('b6638aa3-dd9f-4402-80c6-58ae3900128e', distributed).
narrative_ontology:cs_authority_grounding('b6638aa3-dd9f-4402-80c6-58ae3900128e', lineage).
narrative_ontology:cs_interpretation_layer_present('b6638aa3-dd9f-4402-80c6-58ae3900128e').
narrative_ontology:cs_reading_relation('b6638aa3-dd9f-4402-80c6-58ae3900128e', reading_acquisition_mechanism__phonics_reading, influences).
narrative_ontology:cs_reading_relation('b6638aa3-dd9f-4402-80c6-58ae3900128e', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('b6638aa3-dd9f-4402-80c6-58ae3900128e', foundational, integrated_code_meaning_instruction_necessary).
narrative_ontology:cs_axiom_status(integrated_code_meaning_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b6638aa3-dd9f-4402-80c6-58ae3900128e', integrated_code_meaning_instruction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b6638aa3-dd9f-4402-80c6-58ae3900128e', secondary, authentic_literature_motivational_primacy).
narrative_ontology:cs_axiom_status(authentic_literature_motivational_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b6638aa3-dd9f-4402-80c6-58ae3900128e', authentic_literature_motivational_primacy, instrumental).
narrative_ontology:cs_reference_frame('b6638aa3-dd9f-4402-80c6-58ae3900128e', progressive_integration_equilibrium).
narrative_ontology:cs_drift_state('b6638aa3-dd9f-4402-80c6-58ae3900128e', post_science_of_reading_legislation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6638aa3-dd9f-4402-80c6-58ae3900128e', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, leveled_text_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_pd_consultants).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_offices).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, literate_home_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, remediation_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_offices).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Graduate schools of education that design pre-service reading-methods coursework descended from the progressive lineage of Dewey, Goodman, and Clay, accredit the knowledge base entering classrooms, and staff the journals and conferences where reading pedagogy is adjudicated. Faculty reputations, course catalogs, doctoral pipelines, and enrollment revenue are built on this tradition; changing foundations would mean disavowing decades of published work and retraining the professoriate.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_faculties, agenda_setter,
    institutional, generational, identity_locked, national).

% Publishing houses that author and license benchmark assessment kits, leveled book libraries, and bundled early-grades curricula purchased on district adoption cycles. Revenue scales with the installed base of teachers trained in the framework's routines; catalog pivots toward rival methodologies are possible but costly. Conference sponsorship and author speaking circuits keep adoption networks warm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, leveled_text_publishers, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, leveled_text_publishers, beneficiary).

% Independent trainers, coaches, and institute faculty selling professional-development days, coaching contracts, and certification courses in guided reading, mini-lessons, and running records. Income is method-specific and client relationships renew only while districts keep the adopted framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_pd_consultants, beneficiary,
    organized, biographical, constrained, national).

% Offices that run textbook-adoption committees, allocate English-arts budgets, and answer to school boards and state reviews for literacy results. A ready-made framework lowers procurement risk and decision burden; the same offices absorb board criticism, press inquiries, and parent organizing when scores stagnate.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_offices, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_offices, payer).

% Implement daily instruction inside the adopted framework using its lesson templates, leveled texts, and assessments. The framework supplies planning shortcuts, predictable classroom rhythms, and professional community; teachers also field parental anger, carry self-doubt when students fail to read, and many quietly supplement with sound-work their materials lack.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, beneficiary).

% Students who do not assemble the alphabetic code under meaning-first instruction. Instructional minutes go to context- and picture-based guessing strategies and to leveled texts matched to memorized sight words rather than to a systematic sound-letter progression. They cannot choose their curriculum or leave the room, and the gap usually surfaces only after years, once intervention is costlier and confidence has eroded.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Students whose difficulty with written words reflects a neurobiological difference that makes implicit code acquisition unreliable, and for whom explicit, systematic, cumulative sound-letter teaching is broadly regarded as indispensable. Under a regime that minimizes such teaching and trains guessing from pictures and context, they fall furthest fastest and arrive late to special-education evaluation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, national).

% Students from print-rich homes who piece together decoding largely irrespective of instructional method, through nightly reading and family support. Literature-centered classrooms fit them comfortably; they furnish the visible success stories cited for the approach, and their families seldom need to contest anything.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literate_home_students, beneficiary,
    powerless, biographical, constrained, national).

% Adults who discover a child cannot decode, purchase private evaluations and tutoring at hourly rates to supply the missing instruction, and organize into advocacy networks when schools attribute the gap to maturation or home life. Ways out run through tutoring spending, private or home schooling, or litigation, each expensive.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, national).

% Fund the downstream tiers: Title I services, summer programs, special-education placements, and adult basic-education systems that take in students the early instruction missed. The bill arrives years later, spread thin across budgets, and is rarely traced back to the instructional decision that produced it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, remediation_taxpayers, payer,
    moderate, generational, constrained, national).

% Since 2019, dozens of legislatures have enacted statutes requiring evidence-based reading instruction, banning cue-based word-guessing, funding teacher retraining, and auditing adopted curricula. They hear cognitive-science testimony in committee, condition program funding, and thereby reset what districts are permitted to buy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, state_science_of_reading_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, leveled_text_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the literacy-instruction ecosystem around one shared framework: a common curriculum vocabulary, a training pipeline that turns novices into credentialed practitioners, standard assessment instruments, and a purchasing market that lets thousands of districts adopt without designing instruction from scratch. It also performed a one-time diplomatic function: ending a forty-year intramural war that had made coherent policy impossible.
% TRANSFER_FUNCTION: Moves instructional time away from systematic sound-letter work toward meaning-making activity; moves district and federal funds to publishers, trainers, and institutes on adoption cycles; and moves explanatory responsibility for reading failure from the method to children and families (readiness, maturation, home environment).
% ABSENT_VOICES: Struggling readers themselves — children cannot testify before adoption committees and are described in third person in the literature that governs them. Cognitive scientists outside colleges of education were long absent from the rooms where method was decided; parents arrived only after organizing; and classroom teachers who doubted the method lacked a channel until statutes gave them one. They stood outside the faculty conference circuit and the publisher-funded adoption networks where the consensus formed.
% DISAPPEARANCE_RATIONALE: Thousands of districts' curriculum guides, assessment calendars, and professional-development contracts reference the framework; a publishing segment and a training industry are sized to it; teacher licenses were earned inside it. Overnight removal would strand adoptions, invalidate benchmark workflows, idle trainer networks, and force a scramble toward whichever rival framework state law permits — a wholesale rearrangement of the literacy economy, not a quiet return to some natural default.
% FOUNDING_PROBLEM: The reading wars: recurring pendulum collapse between decontextualized phonics drills (blamed for joyless, comprehension-poor instruction) and whole-language immersion (which left measurable cohorts unable to decode). Schools needed a settlement that let them teach both code and meaning without reopening civil war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: the National Reading Panel (2000) and subsequent meta-analytic work attest that the code-instruction side of the problem remained live and unresolved under the compromise; Jeanne Chall's histories documented the war cycle from outside the movement; and the findings sections of state science-of-reading statutes recite the unresolved-problem record verbatim. The benefiting faculties dispute the characterization, attributing failure to implementation rather than design — which is precisely why corroboration is cited from outside their set.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.58: the regime delivers genuine value to a majority of entrants (engagement, vocabulary, comprehension work, and adequate code for the easily-taught) while systematically shorting the minority who needed explicit instruction — a large, identifiable cohort with lifelong stakes, plus a deferred remediation bill. Suppression 0.58 is authored as a raw structural property — certification gates, adoption lock-in, publisher bundling, professional stigma against 'drill and kill' — and is deliberately unscaled; the engine applies scope and directionality modifiers only to extractiveness. Theater 0.45: benchmark assessments and running records consume hours while revealing little about which sound-letter links a child lacks, and 'balanced' functions as rhetorical solvent absorbing any critique; guided reading and read-alouds nonetheless do real work, keeping theater just under the halfway line. Accessibility collapse 0.5: structured-literacy alternatives exist and are procurable, but reaching them requires surmounting retraining, procurement, and identity friction, so alternatives are heavily discounted rather than sealed off. Resistance 0.7: unusually high for a pedagogical norm — investigative journalism, dyslexia advocacy, researcher rebuttal, and since 2019 statute after statute. Temporal series share one grid (1995-2025, eight points) per the alignment rule, every tracked metric authored at every point. Base_extractiveness rises through 2020 as the evidence gap widens (an accumulation signature worth abductive investigation), then eases as statutes bite. Suppression_requirement is authored because this story genuinely tracks enforcement-capacity change: a 1995-2020 ratchet (credentialing consolidation, adoption hardening) followed by statutory decay after 2020. Theater peaks where the brand sits farthest from practice, then dips as districts rebrand under legal pressure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute different types from identical structural data. From the faculties' seat the regime is a professional equilibrium they staff, certify, and defend — coordination dominates and costs stay invisible behind attributional habits (readiness, maturation). From the struggling reader's seat the same regime is instruction that did not happen, with no exit and no voice. Same-power divergence is structural, not noise: literate-home and struggling students share the powerless atom but differ in effective exit, because family capital substitutes for the missing instruction in one case and not the other; consultants and classroom teachers share the organized atom yet sit at opposite ends of the flow, one collecting training fees, the other collecting blame. The engine computes these divergent classifications from power, exit, and declared position; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Named beneficiaries derive directionality toward the subsidy end: deepest for identity-locked faculties (exit would dissolve their scholarly selves, so the arrangement subsidizes them maximally), near-extreme for arbitrage-capable publishers (regulatory threat notwithstanding, they can pivot catalogs and today collect the largest receipts), moderate for constrained consultants and district offices. Named victims derive directionality toward the target end, deepest for trapped students who cannot leave the classroom, shallower for taxpayers whose costs arrive diffuse and deferred. Dual-positioned agents (teachers: payer primary with beneficiary secondary; district offices the reverse) should land mid-range from their paired declarations. The observing legislature sits outside the flow with analytical exit. Continental-scale adoption markets with nationally varying enforcement raise verification difficulty, which modestly amplifies effective extraction on the target side — that is the engine's arithmetic, not an authored adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate was mediation: stop the pendulum war by integrating code and meaning. That mandate succeeded as a settlement and then outlived itself — the apparatus (adoption cycles, institutes, benchmark kits) now reproduces interests rather than negotiating peace. Classifying this as tangled_rope protects both halves of that truth: a pure-snare reading would deny the real coordination and the documented value where integration was faithful (including the motivational dividend of authentic text); a pure-rope reading would launder cohort harm into 'implementation noise.' The mandatrophy question is routed to the omega variables rather than settled by fiat: implementation-fidelity collapse and faculty identity-versus-incentive lock determine whether the surviving structure is salvageable coordination or interest maintenance. The R5 interview records the founding problem as contested, and the mismatch consumer reads status against the world_rearranges verdict — both sides of the dispute are live here, so no zombie flag is asserted by construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file is one reading (balanced_literacy_reading) of kernel reading_acquisition_mechanism; what would change structurally if the same regime were evaluated under a sibling reading''s premises?',
    'Cross-file comparison within the kernel family: instantiate phonics_reading and whole_language_reading as their own constraints and diff victim sets, epsilon, and enforcement structure.',
    'Under phonics_reading the victim set widens (every student denied systematic instruction) and epsilon rises; under whole_language_reading the phonics-deficit victim category dissolves entirely and the regime reads as faithful implementation. The disagreement lives on the systematicity axis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing record: one reading of the reading-acquisition kernel; sibling readings are separate files.').

omega_variable(
    implementation_fidelity_collapse,
    'Are the measured costs a property of balanced literacy as designed, or of typical implementations collapsing toward whole language for want of specified systematicity?',
    'Compare high-fidelity integrated sites (a specified phonics scope-and-sequence inside authentic-text practice) against typical implementations on decoding and comprehension outcomes, holding demographics constant.',
    'If fidelity rescues outcomes, the authored epsilon belongs largely to implementation failure and the constraint migrates toward rope; if no fidelity level rescues decoding, doctrine-level responsibility is confirmed and the tangled-rope reading sharpens toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_collapse, empirical, 'Whether harm tracks the doctrine or its degraded implementations.').

omega_variable(
    integration_necessity_dose_response,
    'Is integrated practice genuinely necessary for acquisition (a real coordination function), or is ''integration'' a coalition-maintenance formula that lets incompatible camps share a brand?',
    'Dose-response designs varying phonics explicitness and authenticity of text independently, measuring interaction effects on acquisition.',
    'A genuine interaction effect supports the coordination-function claim and holds the classification in tangled_rope; a null interaction converts integration into packaging and pushes the read toward pure extraction with a coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_necessity_dose_response, empirical, 'Authenticity of the integration premise versus brand management.').

omega_variable(
    faculty_identity_vs_incentive_lock,
    'Does the professoriate''s defense of the framework rest on identity fusion with the progressive lineage, or on material dependence on the training and adoption economy?',
    'Observe defense intensity after revenue channels are severed by statute and lawsuit: if publication, hiring, and accreditation defense persist without income, identity dominates.',
    'Identity-dominated lock predicts cultural persistence after legal removal (suppression outliving its machinery); incentive-dominated lock predicts rapid normalization once statutes reroute money — materially different decay curves for the enforcement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faculty_identity_vs_incentive_lock, empirical, 'Source of the agenda-setters'' attachment to the framework.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (certification gates, adoption lock-in, procurement bundling) or internalized (practitioner conviction that explicit instruction damages comprehension, carried after the barriers fall)?',
    'Belief and practice surveys of retrained cohorts after statutory mandates: does avoidance of systematic instruction persist where no gate remains?',
    'An internalized component keeps effective suppression elevated after structural removal and slows measured recovery; a purely structural profile predicts faster convergence once gates open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression measure.').

omega_variable(
    authority_grounding_frame_ambiguity,
    'Is the regime''s authority structure lineage-grounded (legitimacy inherited through the Dewey-Goodman-Clay transmission chain and interpreted by the professoriate) or extraction-grounded (authority sustained by preventing revision of the kernel)?',
    'Test whether interpretive authority survives loss of revenue and statutory displacement: a lineage survives as tradition even when displaced; an extraction structure collapses when the rents close.',
    'An extraction reading would reclassify the interpretive structure as capture-maintaining and sharpen the snare-side diagnostics; the lineage reading adopted here treats current defense as traditionalist resistance with capture as an emergent symptom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_frame_ambiguity, conceptual, 'Two defensible framings of who adjudicates the kernel and why.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balanced_literacy_tr_t1995, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(balanced_literacy_tr_t2000, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(balanced_literacy_tr_t2005, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(balanced_literacy_tr_t2010, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(balanced_literacy_tr_t2015, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(balanced_literacy_tr_t2020, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement(balanced_literacy_tr_t2023, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2023, 0.49).
narrative_ontology:measurement(balanced_literacy_tr_t2025, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(balanced_literacy_be_t1995, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(balanced_literacy_be_t2000, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(balanced_literacy_be_t2005, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(balanced_literacy_be_t2010, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(balanced_literacy_be_t2015, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(balanced_literacy_be_t2020, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(balanced_literacy_be_t2023, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2023, 0.61).
narrative_ontology:measurement(balanced_literacy_be_t2025, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(balanced_literacy_su_t1995, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement(balanced_literacy_su_t2000, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(balanced_literacy_su_t2005, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(balanced_literacy_su_t2010, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement(balanced_literacy_su_t2015, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(balanced_literacy_su_t2020, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(balanced_literacy_su_t2023, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2023, 0.64).
narrative_ontology:measurement(balanced_literacy_su_t2025, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% 'How children learn to read' is a colloquial label covering three structurally distinct claims that differ on one axis — whether decoding instruction must be explicit and systematic, implicitly emergent, or nominally included but unspecified. Per the epsilon-invariance principle the label decomposes into a three-file family: phonics_reading (strongest empirical warrant: National Reading Panel 2000, Ehri et al. meta-analyses), balanced_literacy_reading (this file; the unspecified-systematicity compromise), and whole_language_reading (the implicit-emergence claim). Upstream-downstream: phonics findings are cited as the legitimizing 'phonics component' of balanced programs while their systematicity demand is diluted — the upstream claim lends evidence warmth to the downstream brand; balanced literacy in turn shelters whole language institutionally. Each file authors its own epsilon over the regime its reading instantiates; the epsilons differ because each reading weighs the same classroom reality by its own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
