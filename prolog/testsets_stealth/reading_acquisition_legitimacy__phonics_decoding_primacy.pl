% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics Mandate: Decoding Primacy in Reading Instruction
 *   domain: education policy/cognitive science
 *
 * SUMMARY:
 *   This story instantiates the phonics_decoding_primacy reading of the
 *   reading_acquisition_legitimacy kernel as it operates in contemporary
 *   education policy: a legitimacy standard holding that reading IS decoding
 *   and that legitimate instruction makes the alphabetic principle explicit,
 *   enforced through state science-of-reading statutes, approved-curriculum
 *   lists, universal K-3 screening mandates, teacher-preparation
 *   accreditation requirements, and third-grade retention gates. The
 *   arrangement solves a genuine coordination failure — instructional quality
 *   previously depended on individual teacher philosophy, and struggling
 *   readers were identified too late — while the same mandate machinery
 *   channels compliance spending to curriculum publishers, screening vendors,
 *   and certification providers, delegitimizes a generation of
 *   meaning-first-trained teachers, and attaches retention risk to children's
 *   benchmark scores. The epsilon referent is the standing arrangement under
 *   contest — the decoding-primacy mandate regime as it actually operates,
 *   assessed by this reading's own lights: the pedagogical core (explicit,
 *   sequenced decoding instruction) registers as coordination even from
 *   inside this reading, while vendor capture, compliance costs, and
 *   retention harm register as extraction. The claimed type and the metrics
 *   are independent authored facts: the claim states the structure I believe
 *   true; the metrics describe the regime's operation as I believe
 *   descriptively accurate. Sibling readings (whole_language_meaning_primacy,
 *   balanced_literacy_integration, structured_literacy_remediation) are
 *   separate constraints with their own epsilon and party structures; see
 *   network.dual_formulation_note. The measurement interval T0-T25 maps 2000
 *   (National Reading Panel report) through 2025.
 *
 * KEY AGENTS:
 *   - state_literacy_mandate_authorities: agenda setter (institutional/constrained) — enacts and enforces the decoding-primacy legitimacy standard through statute, approved lists, screening mandates, and accreditation
 *   - phonics_curriculum_publishers: primary monetary beneficiary (organized/arbitrage) — receives the largest stream of compliance spending
 *   - reading_assessment_vendors: secondary monetary beneficiary (organized/arbitrage) — recurring license revenue from universal screening mandates
 *   - structured_literacy_training_providers: secondary monetary beneficiary (organized/arbitrage) — sells the retraining the mandates require
 *   - students_with_dyslexia: primary substantive beneficiary (powerless/trapped) — early identification and explicit instruction they previously went without
 *   - early_grade_students: dual-positioned payer and beneficiary (powerless/trapped) — receive the instruction, bear retention-gate risk and curriculum narrowing
 *   - whole_language_trained_teachers: primary payer (moderate/identity_locked) — professional practice and identity delegitimized by the standard
 *   - school_districts: payer (institutional/constrained) — bears procurement, licensing, and retraining costs
 *   - balanced_literacy_publishers: payer (organized/arbitrage) — loses the market its programs previously dominated
 *   - dyslexia_parent_advocacy_networks: beneficiary (organized/constrained) — collects policy wins and services for their children
 *   - literacy_research_community: analytical observer (institutional/analytical) — adjudicates the evidence base the standard claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics Mandate: Decoding Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education policy/cognitive science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, 'f076106b-4421-4612-9f19-5a3ee88dd25f').
narrative_ontology:cs_kernel_codification('f076106b-4421-4612-9f19-5a3ee88dd25f', formalized).
narrative_ontology:cs_authority_grounding('f076106b-4421-4612-9f19-5a3ee88dd25f', expertise).
narrative_ontology:cs_interpretation_layer_present('f076106b-4421-4612-9f19-5a3ee88dd25f').
narrative_ontology:cs_reading_relation('f076106b-4421-4612-9f19-5a3ee88dd25f', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('f076106b-4421-4612-9f19-5a3ee88dd25f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('f076106b-4421-4612-9f19-5a3ee88dd25f', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('f076106b-4421-4612-9f19-5a3ee88dd25f', foundational, decoding_requires_explicit_instruction).
narrative_ontology:cs_axiom_status(decoding_requires_explicit_instruction, holdable).
narrative_ontology:cs_axiom_grounding('f076106b-4421-4612-9f19-5a3ee88dd25f', decoding_requires_explicit_instruction, empirically_contingent).
narrative_ontology:cs_axiom('f076106b-4421-4612-9f19-5a3ee88dd25f', foundational, decoding_is_primary_constraint_on_comprehension).
narrative_ontology:cs_axiom_status(decoding_is_primary_constraint_on_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('f076106b-4421-4612-9f19-5a3ee88dd25f', decoding_is_primary_constraint_on_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('f076106b-4421-4612-9f19-5a3ee88dd25f', secondary, early_texts_must_be_decodable).
narrative_ontology:cs_axiom_status(early_texts_must_be_decodable, holdable).
narrative_ontology:cs_axiom_grounding('f076106b-4421-4612-9f19-5a3ee88dd25f', early_texts_must_be_decodable, instrumental).
narrative_ontology:cs_reference_frame('f076106b-4421-4612-9f19-5a3ee88dd25f', explicit_alphabetic_instruction_standard).
narrative_ontology:cs_drift_state('f076106b-4421-4612-9f19-5a3ee88dd25f', contemporary_mandate_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f076106b-4421-4612-9f19-5a3ee88dd25f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_assessment_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_training_providers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, school_districts).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, early_grade_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, early_grade_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexia_parent_advocacy_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and administer science-of-reading statutes: define approved curriculum lists, require universal K-3 decoding screeners, condition teacher-preparation accreditation on explicit-phonics coursework, and attach promotion to benchmark performance. Collect compliance documentation and political credit for literacy gains; can amend or repeal the regime but face an organized coalition of advocacy groups, vendors, and districts with sunk investments.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_literacy_mandate_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Previously failed to learn to read under meaning-first instruction and were identified only after years of failure. Universal screening now flags them in kindergarten or first grade, and explicit, cumulative decoding instruction reaches them at all. They cannot exit public schooling and their literacy depends entirely on the quality of the instruction the mandate requires.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Sell decodable-text programs and re-branded structured literacy curricula into a market created by approved-list statutes and district adoption cycles. They receive the largest single stream of compliance spending; product lines pivot as standards shift, as the same firms pivoted out of whole-language era products.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% License the universal K-3 screening and progress-monitoring tools that statutes now require, on recurring per-student contracts tied to mandated testing windows. Their benchmark design influences which students get flagged for intervention.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_assessment_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Sell the teacher certification and retraining coursework (structured-literacy institutes, multisensory certification) that statutes and district compliance plans now require. Revenue scales directly with the size of the workforce the mandates reach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_training_providers, beneficiary,
    organized, biographical, arbitrage, national).

% Trained in meaning-first pedagogy; their instructional methods are now statutorily delegitimized and reclassified as the cause of reading failure. They must retrain on their own or district time, teach from mandated sequences, decodable texts, and scripts, and accept that their professional expertise no longer counts. The only full exit is leaving the profession; retraining is experienced as repudiation of a career's worth of craft.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers, payer,
    moderate, biographical, identity_locked, national).

% Must purchase from approved curriculum lists, license screeners, fund mass retraining, and document implementation fidelity. Budgets shift from discretionary enrichment toward compliance; noncompliance risks funding conditions and intervention-status labels. They cannot exit state oversight.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, school_districts, payer,
    institutional, generational, constrained, national).

% Receive explicit decoding instruction they need and previously often went without, but also bear retention-gate risk under third-grade reading laws and spend early-grade instructional time on phonics routines and decodable texts at the expense of read-alouds, content knowledge, and voluntary reading. They hold no seat in the legislative and procurement processes that set their curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_grade_students, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, early_grade_students, beneficiary).

% Built market-leading literature-rich programs over three decades; approved-list exclusion and shifting district adoptions collapse their revenue base. They respond by re-issuing programs with added phonics components, conceding the legitimacy standard they previously contested in order to keep the market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_publishers, payer,
    organized, biographical, arbitrage, national).

% Organized parent coalitions that drafted and lobbied the mandate wave after their children failed under the prior regime. They collect policy wins, services for their children, and standing in the policy conversation; their advocacy is mission-locked to their children's school years.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexia_parent_advocacy_networks, beneficiary,
    organized, biographical, constrained, national).

% Produces and disputes the evidence base the standard claims: meta-analyses of phonics effects, moderator studies, critiques of over-generalization from small effects. Its internal disputes determine what counts as settled science, though it holds no enforcement power and is cited selectively by both the mandate coalition and its opponents.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_research_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, explicit, sequenced standard for early reading instruction: a common teacher knowledge base, common K-3 screening benchmarks, and a common instructional progression from grapheme-phoneme correspondences to connected text. This solves a real coordination failure in which instructional quality depended on individual teacher philosophy and struggling readers were identified too late for intervention to work.
% TRANSFER_FUNCTION: Moves curriculum, assessment, and training spending from school district and state budgets to curriculum publishers, screening vendors, and certification providers; moves instructional time and professional autonomy from teachers to mandated sequences and scripts; moves grade-retention risk onto students who miss decoding benchmarks; and moves literacy itself — the intended transfer — to previously failing readers.
% ABSENT_VOICES: Early-grade students have no seat in the legislative and procurement processes that set their instruction. Teachers defending meaning-making practice enter mainly as defendants in hearings. Developmental and motivation researchers critical of universal mandates sit outside the advocacy coalition that drafted the statutes. Balanced-literacy researchers were excluded from approved-list criteria. The statutes were drafted with advocacy organizations and vendor input, not with these seats present.
% DISAPPEARANCE_RATIONALE: If the decoding-primacy legitimacy standard and its mandate apparatus vanished overnight, screening mandates, approved curriculum lists, preparation accreditation requirements, and retention gates would lose their legal warrant; districts would revert to philosophically heterogeneous instruction; the vendor complex built on compliance spending would lose its market; and identification of struggling readers would again depend on individual teacher judgment, with dyslexic students again found late.
% FOUNDING_PROBLEM: Widespread early reading failure, concentrated among disadvantaged and dyslexic students, produced by instruction that never made the alphabetic principle explicit, compounded by teacher-preparation programs that did not teach the reading science.
% FOUNDING_PROBLEM_CORROBORATION: NAEP long-term trend and state assessment data — independent of the vendor and advocacy complex — attest persistent reading failure at scale, and peer-reviewed early-intervention research attests the late-identification problem the screening mandates address. Vendor, publisher, and training-provider attestations of the problem are excluded as self-interested; no corroboration is drawn from the benefiting parties.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62, 'stealth/ox-alpha', 'none', direct).

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

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the compliance economy built on the mandate wave: approved-list curriculum adoption, recurring screening licenses, and mandatory retraining are decoupled from marginal instructional value even where the instruction itself works. Suppression (0.65) is a raw structural property, unscaled — only extractiveness is engine-scaled by directionality and scope — and reflects statutory compulsion: teachers must comply or leave, districts must buy from lists or risk intervention labels, and the excluded alternative survives only at the margins of private and home schooling. Theater (0.35) reflects the growing share of mandate activity that is label compliance — materials purchased, screeners administered, data filed — without changed classroom practice. Accessibility_collapse (0.55): the alternative does not vanish under scrutiny; it is delegitimized in procurement and accreditation while persisting in classrooms and non-public settings. Resistance (0.55): organized but losing — unions, holdout districts, and a minority research faction contest the standard while the advocacy-vendor coalition and outcome evidence carry the policy tide. All measurement series run on one shared grid (T0=2000 through T25=2025, six points, all three metrics at every point) so no series is backfilled from another's end state. The trajectories rise monotonically: this is an enforcement ratchet, not a cycle — statutes accumulate, screening windows expand, retraining mandates reach deeper into the workforce, and suppression_requirement is tracked precisely because enforcement machinery is the dynamic being built. The oscillation-free rise is itself diagnostic of a constraint consolidating rather than decaying.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the vendor seats the arrangement is market creation: approved lists convert pedagogy into procurement, and each new statute expands the customer base — a subsidy-like experience. From the whole-language-trained teacher seat the same structure is professional dispossession: the identity-lock mechanism fuses career investment, preparation training, and self-concept as a teacher of readers to a pedagogy the standard reclassifies as the cause of reading failure; exit is retraining experienced as repudiation, or leaving the profession. If that identity frame broke — retraining reframed as extension rather than repudiation — the seat's exit would shift from identity_locked toward constrained and measured resistance would fall without any structural change. From the dyslexic-student seat the arrangement is rescue: screening finds them in kindergarten instead of third grade, and explicit instruction reaches them at all. Early-grade students are dual-positioned and the engine reads both declarations. Districts pay and comply; state authorities set the agenda and collect political credit; the research community observes analytically. A potential payer coalition (unions, holdout publishers, dissenting researchers) exists but is unstable because the publisher wing defected by re-branding its programs to the new standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive directionality. phonics_curriculum_publishers, reading_assessment_vendors, and structured_literacy_training_providers (organized power, arbitrage exit) sit near the beneficiary end — the arrangement subsidizes them. students_with_dyslexia (powerless, trapped) also sits near the beneficiary end: the arrangement's benefits concentrate on them despite their lack of power. whole_language_trained_teachers (moderate, identity_locked) and school_districts (institutional, constrained) sit near the target end; identity lock and constrained exit push the teacher seat toward the full-target pole. early_grade_students appear in the victims array and carry a secondary beneficiary role, so the derivation reads both. No directionality_overrides are authored: overrides key on the power atom and could not separate the two powerless student seats from each other, and the derived values from declarations plus exit options capture every other seat's relationship correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reading failure produced by instruction that never made the alphabetic principle explicit — is live, corroborated outside the beneficiary set, so no mandatrophy is declared. The tangled-rope classification prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the documented equity gains — early identification and explicit instruction for students the previous regime failed; reading it as pure coordination would launder vendor capture, mandatory-retraining rents, and retention harm as pedagogy. The hybrid holds because coordination and extraction run through the same structures: the approved list that guarantees instructional quality also guarantees publishers a captive market; the screening mandate that finds dyslexic students also licenses vendor revenue; the retraining requirement that builds the teacher knowledge base also bills the workforce it certifies. Drift watch: if implementation theater entrenches while the pedagogical core holds, the compliance layer drifts piton-ward; if the evidence-scope omega resolves toward targeted-only efficacy, the universal mandate loses warrant and the arrangement narrows toward a transitional shape; if vendor rents are competed away while mandates hold, the arrangement drifts toward pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the phonics_decoding_primacy reading of the reading_acquisition_legitimacy kernel: is decoding primacy the correct account of what reading is and what makes instruction legitimate, and how would each sibling reading restructure this constraint''s parties if it governed instead?',
    'Resolved only by which reading a jurisdiction adopts (statute, preparation accreditation, approved lists) and by long-horizon evidence accumulation across readings; not resolvable within this story. The sibling stories carry their own epsilon, beneficiaries, victims, and axioms.',
    'Under whole_language_meaning_primacy the harmed set relocates (students denied meaning-rich instruction become the injured party; literature publishers benefit); under structured_literacy_remediation the benefiting set narrows to the most vulnerable learners and intervention vendors; under balanced_literacy_integration both the coordination and the capture distribute across the hybrid. The disagreement is located in the definitional premise — reading IS decoding versus reading IS meaning-making — which fixes each reading''s victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the reading-acquisition-legitimacy kernel; sibling readings would relocate the party structure.').

omega_variable(
    mandate_extraction_separability,
    'Is the capture by the vendor complex (curriculum lock-in via approved lists, recurring screening licenses, mandatory retraining fees) separable from the genuine instructional and early-identification functions the mandates fund?',
    'Procurement and cost-benchmark analysis: compare mandated prices against competitively tendered equivalents; natural experiments from states with open-list or district-choice procurement.',
    'If separable, the arrangement is coordination with attached rents and the receipt surface is the capture story; if inseparable, part of the measured extraction is quality-assurance cost and the arrangement sits nearer pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_extraction_separability, empirical, 'Whether vendor rents ride on, or are constitutive of, the mandate''s coordination function.').

omega_variable(
    evidence_scope_generalization,
    'Does the meta-analytic support for systematic phonics generalize from kindergarten-through-first-grade and struggling readers to universal mandates covering all students across the early grades?',
    'Moderator analyses and long-term trials comparing universal implementation against targeted intervention for at-risk readers only.',
    'If effects concentrate in struggling readers, universal mandates over-burden instructional time for typical readers, the coordination function narrows to early identification plus intervention, and the arrangement loses warrant for its universal reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_scope_generalization, empirical, 'Scope of the evidence base relative to the mandate''s universal coverage.').

omega_variable(
    implementation_theater_trajectory,
    'Is the rising theater ratio transition lag (districts mid-adoption, materials purchased before practice changes) or entrenched compliance performance (labels and paperwork without instructional change)?',
    'Classroom-observation fidelity audits and outcome-convergence checks across adoption cohorts.',
    'If lag, theater should decline as adoption matures and the mandate layer stabilizes; if entrenched, the compliance layer drifts toward inertial maintenance while the pedagogical core holds — a piton-forming layer on a live arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_theater_trajectory, empirical, 'Whether measured compliance theater is transitional or structural.').

omega_variable(
    retention_gate_net_effect,
    'Do third-grade retention gates — the sharpest cost attached to students who miss decoding benchmarks — produce durable literacy gains or net harm to retained students?',
    'Longitudinal cohort studies of retained versus promoted students with comparable baseline scores, beyond the short-horizon gains reported in early state evaluations.',
    'If net harm, the harmed-party structure hardens and the burden on the student seat rises; if durable gains, the retention cost is a contested but functional component of the identification mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retention_gate_net_effect, empirical, 'Whether the retention gate is a net cost or a costly component of early identification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 5, 0.15).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 10, 0.19).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.24).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.3).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 25, 0.35).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The natural-language label 'evidence-based reading instruction' covers structurally distinct claims that this corpus holds apart as one constraint family under the reading_acquisition_legitimacy kernel: decoding primacy (this file — universal explicit phonics as the legitimacy standard, with a vendor-complex receipt surface), whole_language_meaning_primacy (immersion; a different harmed set — students denied meaning-rich instruction — and different beneficiaries), balanced_literacy_integration (hybrid; coordination and capture both distributed), and structured_literacy_remediation (vulnerability-first; the benefiting set narrowed to the most vulnerable learners). Each reading has its own epsilon and party structure; the family is linked through affects_constraints so legitimacy shifts and contamination propagate across readings — this reading's mandate victories change balanced literacy's resource and legitimacy conditions without logically eliminating it, while logically excluding whole-language practice within any single adopting framework. Sibling files should carry reciprocal notes and link back to this constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
