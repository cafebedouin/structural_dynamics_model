% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition (Natural Emergence Claim)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The whole-language reading of the literacy acquisition kernel claims that
 *   reading emerges naturally from meaningful engagement with connected text,
 *   that phonics skills develop incidentally through exposure, and that
 *   explicit decoding instruction is unnecessary and potentially harmful to
 *   motivation. This reading dominated English-speaking education systems
 *   from the 1980s through early 2000s. The structural delta reveals
 *   asymmetric extraction: teacher professional identity and autonomy are
 *   preserved (low extractiveness on teachers), but students without
 *   print-rich home environments — who depend on school for systematic code
 *   instruction — bear high extractiveness as their reading failure is
 *   attributed to 'lack of engagement' rather than instructional omission.
 *   The constraint requires active enforcement through curriculum mandates,
 *   teacher preparation standards, and material adoption policies that
 *   exclude systematic phonics programs.
 *
 * KEY AGENTS:
 *   - teacher_professional_identity: Primary beneficiary (organized/identity_locked) — professional self-concept constituted through 'natural' methods; exit threatens identity
 *   - students_without_home_literacy_support: Primary victim (powerless/trapped) — depend entirely on school for code instruction; no exit from compulsory schooling
 *   - students_with_dyslexia_risk: Secondary victim (powerless/trapped) — biologically require explicit systematic instruction; whole-language assumes universal natural emergence
 *   - students_with_home_literacy_support: Incidental beneficiary (moderate/constrained) — home environment compensates for school's instructional gaps
 *   - phonics_advocates: Excluded (organized/constrained) — structural exclusion from curriculum decisions; would object if present
 *   - reading_science_researchers: Observer (analytical/analytical) — provide empirical challenge to natural-emergence axiom
 *   - district_administrators: Agenda_setter (institutional/biographical) — adopt/maintain whole-language curricula; professional identity aligned with teacher autonomy narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.58).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, mountain).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition (Natural Emergence Claim)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).
domain_priors:emerges_naturally(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '239188eb-0a37-4e59-8214-414ace7844fd').
narrative_ontology:cs_kernel_codification('239188eb-0a37-4e59-8214-414ace7844fd', distributed).
narrative_ontology:cs_authority_grounding('239188eb-0a37-4e59-8214-414ace7844fd', practice).
narrative_ontology:cs_interpretation_layer_present('239188eb-0a37-4e59-8214-414ace7844fd').
narrative_ontology:cs_reading_relation('239188eb-0a37-4e59-8214-414ace7844fd', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('239188eb-0a37-4e59-8214-414ace7844fd', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('239188eb-0a37-4e59-8214-414ace7844fd', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('239188eb-0a37-4e59-8214-414ace7844fd', foundational, reading_emerges_naturally_from_meaningful_engagement).
narrative_ontology:cs_axiom_status(reading_emerges_naturally_from_meaningful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('239188eb-0a37-4e59-8214-414ace7844fd', reading_emerges_naturally_from_meaningful_engagement, empirically_contingent).
narrative_ontology:cs_axiom('239188eb-0a37-4e59-8214-414ace7844fd', foundational, explicit_decoding_instruction_harms_reading_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harms_reading_motivation, holdable).
narrative_ontology:cs_axiom_grounding('239188eb-0a37-4e59-8214-414ace7844fd', explicit_decoding_instruction_harms_reading_motivation, empirically_contingent).
narrative_ontology:cs_axiom('239188eb-0a37-4e59-8214-414ace7844fd', secondary, teacher_professional_judgment_supersedes_scripted_programs).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_supersedes_scripted_programs, holdable).
narrative_ontology:cs_axiom_grounding('239188eb-0a37-4e59-8214-414ace7844fd', teacher_professional_judgment_supersedes_scripted_programs, deontological).
narrative_ontology:cs_reference_frame('239188eb-0a37-4e59-8214-414ace7844fd', whole_language_natural_emergence).
narrative_ontology:cs_drift_state('239188eb-0a37-4e59-8214-414ace7844fd', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('239188eb-0a37-4e59-8214-414ace7844fd', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_professional_identity).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_autonomy_advocates).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, students_with_home_literacy_support).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, literacy_as_natural_language_acquisition).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, meaning_based_reading_philosophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teachers' professional self-concept is constituted through 'natural' child-centered methods; explicit phonics is experienced as deskilling and identity threat. Teacher preparation programs, professional organizations, and peer networks reinforce this identity. Leaving the approach means abandoning professional self-understanding — not just changing pedagogy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_professional_identity, beneficiary,
    organized, biographical, identity_locked, national).

% Administrators, union leaders, and curriculum directors who defend teacher professional judgment against 'scripted' programs. They set agenda by adopting whole-language curricula, blocking phonics mandates, and framing opposition as anti-teacher. They benefit from the constraint's protection of professional authority but face accountability pressure when reading scores decline.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_autonomy_advocates, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, teacher_autonomy_advocates, beneficiary).

% Children from low-print homes who enter school without letter knowledge, phonemic awareness, or book experience. They depend entirely on school for systematic code instruction. Under whole language, they receive 'authentic literature' they cannot decode, are told to 'use context cues,' and fall further behind each year. No exit: compulsory attendance, no alternative instruction available, parents lack resources for tutoring.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, biographical, trapped, local).

% Children with neurobiological differences requiring explicit, systematic, cumulative phonics instruction. Whole language's 'natural emergence' assumption means their needs are invisible — they are not 'ready' or 'engaged' enough. They experience the constraint as total extraction: years of failure, misattribution to motivation, delayed intervention. Trapped in same system with no specialized instruction until (if ever) identified.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_risk, payer,
    powerless, biographical, trapped, local).

% Children from print-rich homes who enter school with letter knowledge, phonemic awareness, and thousands of hours of read-aloud experience. They acquire decoding incidentally through the 'meaningful engagement' the constraint provides. They benefit from the literature-rich environment and suffer minimal extraction — their home environment compensates for the school's instructional gaps.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_home_literacy_support, beneficiary,
    moderate, biographical, constrained, local).

% Researchers, dyslexia organizations, and parent advocates who argue for explicit systematic phonics. They are structurally excluded from curriculum adoption committees, teacher preparation standards, and major professional conferences. Their exclusion is functional: the constraint's enforcement machinery (curriculum mandates, materials adoption) exists to keep phonics out. They would object if present; their absence is not accidental.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, phonics_advocates, excluded,
    organized, biographical, constrained, national).

% Cognitive scientists, psycholinguists, and educational researchers studying reading acquisition. They provide the empirical challenge to the natural-emergence axiom (neuroimaging, eye-tracking, intervention studies). They do not collect from or pay into the constraint; their seat sees the full structure across all readings of the kernel.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_science_researchers, observer,
    analytical, generational, analytical, global).

% Superintendents, curriculum directors, and school boards who adopt and maintain whole-language or balanced-literacy curricula. They face competing pressures: teacher professional identity (organized, identity_locked) vs. reading score accountability (state mandates, parent pressure). Their biographical horizon means they may rotate before consequences fully materialize.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, teacher_professional_identity).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects teacher professional autonomy against scripted, basal-reader approaches; centers meaning and engagement in literacy instruction; provides literature-rich environments for all students.
% TRANSFER_FUNCTION: Moves instructional time, cognitive load, and long-term reading outcomes from students without home literacy support (who need explicit code instruction) to teacher professional identity (which collects autonomy and status from 'natural' methods). The transfer is not monetary but developmental: vulnerable students pay with reading failure; teachers collect professional coherence.
% ABSENT_VOICES: Students without home literacy support and students with dyslexia risk are structurally excluded from curriculum decisions — they are children with no political voice. Their parents (often low-income, limited English, or both) are excluded from adoption committees. Phonics advocates (researchers, dyslexia organizations) are excluded from teacher preparation and major conferences. The constraint's enforcement machinery exists to maintain these exclusions.
% DISAPPEARANCE_RATIONALE: If whole-language mandates vanished overnight, teacher preparation would need restructuring, curriculum adoptions would shift to evidence-based programs, students without home support would receive explicit phonics, and reading outcomes for vulnerable populations would improve (per Mississippi, England, Florida reforms). Teacher professional identity would face acute crisis — the constraint's disappearance rearranges the professional world.
% FOUNDING_PROBLEM: 1970s-80s basal readers produced 'word-callers' who decoded fluently but comprehended poorly; skills-first approaches were seen as killing motivation and meaning; children needed authentic literacy experiences like those in print-rich homes.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language proponents (Goodman, Smith, Calkins) attest the founding problem is LIVE — comprehension crisis persists. Reading scientists (Adams, Moats, Seidenberg, Castles) and dyslexia advocates (International Dyslexia Association) attest the founding problem is DEAD for decoding — the crisis now is children NOT learning to decode at all. State policy shifts (38+ US states with science-of-reading laws 2019-2024) corroborate the shifted-problem reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, ExtMetricName, E),
    domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(literacy_acquisition_kernel__whole_language_reading),
    narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.65 reflects substantial extraction from vulnerable students (directionality ~0.9 for powerless/trapped) while teacher seat experiences near-zero extraction (directionality ~0.1 for organized/identity_locked beneficiary). Suppression 0.58: structural (curriculum mandates excluding phonics) plus internalized (teacher belief that phonics harms motivation). Theater_ratio 0.42: performative 'literacy-rich environments' and 'authentic literature' rituals persist while core decoding instruction is omitted. Accessibility_collapse 0.71: for students without home support, alternatives collapse completely — no other path to decoding. Resistance 0.62: sustained challenge from reading science, dyslexia advocacy, and policy shifts (Mississippi, England, US states post-2019).
 *
 * PERSPECTIVAL GAP:
 *   From teacher_professional_identity seat (organized, identity_locked, beneficiary): the constraint is genuine coordination — it protects professional judgment against scripted programs, centers meaning, and trusts children's natural learning. From students_without_home_literacy_support seat (powerless, trapped, payer): the same structure is extraction — they pay with reading failure while the system attributes it to their 'lack of engagement.' The engine computes this divergence from the structural data: same constraint, opposite types across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher professional identity is beneficiary: collects professional autonomy, status, and identity coherence from the constraint; identity_locked exit means they cannot leave without losing self-concept (d ≈ 0.15). Students without home literacy support are victims: bear reading failure, remedial costs, lifetime opportunity loss; trapped exit (compulsory schooling, no alternative instruction) means d ≈ 0.95. Students with dyslexia risk similarly trapped with higher per-capita extraction. District administrators are agenda_setters with institutional power but biographical horizon — they administer the constraint but face electoral/accountability pressure (d ≈ 0.45). Phonics advocates are excluded — their exclusion IS the enforcement mechanism (d ≈ 0.8 for organized/constrained).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s-80s): 'skills-based basal readers produce word-callers who decode without comprehension; children need authentic literacy experiences.' This problem was LIVE when whole language emerged. By 2000s, the problem shifted: 'children are not learning to decode at all.' The arrangement persists (mandatrophy_unresolved) because teacher identity is fused with the approach — fixing it requires admitting the founding solution created new victims. The mandatrophy is not resolved; the constraint survives by rebranding as 'balanced literacy' while retaining the core axiom (decoding emerges naturally).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_pedagogy,
    'Is the claim that reading emerges naturally from meaningful text engagement a genuine cognitive universal, or a constructed pedagogical theory that benefits teacher professional identity while extracting from students lacking print-rich home environments?',
    'Cross-cultural and SES-stratified longitudinal studies comparing reading outcomes under whole-language vs. explicit instruction, controlling for home literacy environment; neuroimaging of reading acquisition pathways in diverse populations.',
    'If natural law, the constraint is a mountain with near-zero extraction for all; if constructed, the asymmetric extraction on vulnerable students reclassifies it as tangled_rope or snare via FSM, and the beneficiary declaration (teacher professional identity) becomes the extraction anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_pedagogy, empirical, 'Whether whole-language emergence is a cognitive universal or a professional-identity-serving construction').

omega_variable(
    kernel_reading_whole_language,
    'This constraint is the whole_language_reading of the literacy_acquisition_kernel. How does this reading''s structural relationship to the kernel differ from its sibling readings (phonics_reading, balanced_literacy_reading, structured_literacy_reading), and what specific structural elements do the readings disagree on?',
    'Structural comparison of each reading''s beneficiary/victim sets, directionality profiles, and claimed_type against the shared referent (the standing arrangement of literacy instruction). The disagreement is located on: (1) whether decoding instruction is necessary (this reading: unnecessary/harmful; siblings: necessary), (2) who bears the cost of ''natural'' emergence (this reading: no one; siblings: students without home support), (3) whether teacher autonomy is a coordination benefit or an extraction shield.',
    'Clarifies that ε is reading-indexed: this reading authors low ε (natural emergence), while sibling readings author higher ε for the same referent. The engine computes per-reading classifications from each reading''s own structural data. The kernel_id links them for contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_whole_language, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel').

omega_variable(
    suppression_mechanism_curriculum_mandates,
    'Is the suppression of explicit phonics instruction under whole-language regimes structural (curriculum mandates, teacher preparation programs, material adoption policies) or internalized (teachers'' belief that phonics harms motivation, professional identity fused with ''natural'' methods)?',
    'Post-policy-change suppression trajectory: in jurisdictions that mandated explicit phonics (e.g., Mississippi 2013, England 2006), measure whether whole-language practices persist via teacher belief/identity mechanisms after structural barriers are removed.',
    'If internalized, the constraint''s effective suppression is higher than policy measures suggest — teachers carry the suppression with them, explaining persistence of ''balanced literacy'' hybrids that retain whole-language assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_curriculum_mandates, empirical, 'Structural vs. internalized suppression of explicit decoding instruction').

omega_variable(
    cs_framing_teacher_practice_vs_student_outcome,
    'Does the commitment-system structure of this reading ground authority in teacher practice community (practice) or in the empirical claim about natural emergence (expertise)? The two framings yield different cs_pattern classifications.',
    'Analyze whether whole-language professional development, teacher education texts, and curriculum guides cite teacher craft wisdom or cognitive science as the warrant for ''natural emergence''. If practice, authority_grounding=practice and interpretation_layer absorbs drift; if expertise, axiom_overriding drift from reading science directly challenges legitimacy.',
    'Practice framing makes the reading resilient to empirical challenge (drift absorbed by interpretation layer); expertise framing makes it vulnerable to axiom_overriding foreclosure. This determines whether cs_structure.drift_state.direction is ''practice_drift'' or ''axiom_overriding''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_teacher_practice_vs_student_outcome, conceptual, 'CS-framing under-determination: practice vs. expertise authority grounding for whole-language kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lawr_tr_t1985, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(lawr_tr_t1985, observed).
narrative_ontology:measurement(lawr_tr_t1995, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement_basis(lawr_tr_t1995, observed).
narrative_ontology:measurement(lawr_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(lawr_tr_t2000, observed).
narrative_ontology:measurement(lawr_tr_t2005, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement_basis(lawr_tr_t2005, observed).
narrative_ontology:measurement(lawr_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement_basis(lawr_tr_t2010, observed).
narrative_ontology:measurement(lawr_tr_t2015, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(lawr_tr_t2015, observed).
narrative_ontology:measurement(lawr_tr_t2020, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement_basis(lawr_tr_t2020, observed).
narrative_ontology:measurement(lawr_tr_t2024, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(lawr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(lawr_be_t1985, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement_basis(lawr_be_t1985, observed).
narrative_ontology:measurement(lawr_be_t1995, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(lawr_be_t1995, observed).
narrative_ontology:measurement(lawr_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(lawr_be_t2000, observed).
narrative_ontology:measurement(lawr_be_t2005, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement_basis(lawr_be_t2005, observed).
narrative_ontology:measurement(lawr_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement_basis(lawr_be_t2010, observed).
narrative_ontology:measurement(lawr_be_t2015, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(lawr_be_t2015, observed).
narrative_ontology:measurement(lawr_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(lawr_be_t2020, observed).
narrative_ontology:measurement(lawr_be_t2024, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(lawr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(lawr_su_t1985, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement_basis(lawr_su_t1985, observed).
narrative_ontology:measurement(lawr_su_t1995, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement_basis(lawr_su_t1995, observed).
narrative_ontology:measurement(lawr_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement_basis(lawr_su_t2000, observed).
narrative_ontology:measurement(lawr_su_t2005, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement_basis(lawr_su_t2005, observed).
narrative_ontology:measurement(lawr_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(lawr_su_t2010, observed).
narrative_ontology:measurement(lawr_su_t2015, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(lawr_su_t2015, observed).
narrative_ontology:measurement(lawr_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(lawr_su_t2020, observed).
narrative_ontology:measurement(lawr_su_t2024, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(lawr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, teacher_preparation_program_accreditation).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, state_english_language_arts_standards).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, dyslexia_screening_mandates).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four constraint stories (whole_language_reading, phonics_reading, balanced_literacy_reading, structured_literacy_reading) linked by network.affects_constraints. This reading (whole_language) claims natural emergence (mountain) but shows asymmetric extraction on vulnerable students — FSM candidate. The phonics and structured_literacy readings claim explicit instruction necessity with different beneficiary/victim structures. The balanced_literacy reading attempts coordination hybrid but inherits whole-language's extraction on vulnerable students. All four share the referent (literacy instruction arrangement) but author different ε and structural data per reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, organized, 0.15).
constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
