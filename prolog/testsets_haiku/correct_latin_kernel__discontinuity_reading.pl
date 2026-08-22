% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical Latin Discontinuity Constraint (Reconstruction as Symbolic Reoccupation)
 *   domain: intellectual/historical/linguistic
 *
 * SUMMARY:
 *   Classical Latin and Medieval Latin represent a contested boundary in
 *   historical linguistics and intellectual authority. The discontinuity
 *   reading treats them as structurally distinct systems: Classical Latin is
 *   a fixed historical object to be recovered from ancient texts; Medieval
 *   Latin is a corruption or aberration requiring symbolic reoccupation back
 *   toward the classical standard. This reading dominates Latin philology
 *   through institutional control (universities, editorial boards,
 *   peer-review gatekeeping). The constraint benefits classical authorities
 *   and harms the medieval tradition by rendering medieval texts
 *   interpretively subordinate and alternative recovery methodologies
 *   professionally marginal. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination function: unified system provision) but authored
 *   with high extractiveness and suppression metrics, revealing the gap
 *   between the coordinating function and the extractive operation. The
 *   engine computes per-seat classifications; the authoring claim states what
 *   the discontinuity reading itself believes.
 *
 * KEY AGENTS:
 *   - classical_philology_establishment: institutional beneficiary, sets the agenda, controls canonical texts and peer-review standards
 *   - traditional_grammar_authorities: institutional beneficiary, authority depends on unified classical system framework
 *   - medieval_textual_tradition: powerless victim, trapped under classical corrective lens, cannot speak in own linguistic voice
 *   - alternative_recovery_methodologies: moderate payer/excluded, systematic marginalization in peer-review and funding
 *   - students_and_learners: powerless victim, identity-locked into classical-only framework via pedagogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical Latin Discontinuity Constraint (Reconstruction as Symbolic Reoccupation)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "intellectual/historical/linguistic").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '5fb2922a-450e-4aa0-b8e3-0f975bf2b20e').
narrative_ontology:cs_kernel_codification('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', fixed_text).
narrative_ontology:cs_authority_grounding('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', extraction).
narrative_ontology:cs_interpretation_layer_present('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e').
narrative_ontology:cs_reading_relation('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', foundational, medieval_forms_as_corruptions).
narrative_ontology:cs_axiom_status(medieval_forms_as_corruptions, holdable).
narrative_ontology:cs_axiom_grounding('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', medieval_forms_as_corruptions, empirically_contingent).
narrative_ontology:cs_axiom('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', foundational, classical_standard_as_recovery_metric).
narrative_ontology:cs_axiom_status(classical_standard_as_recovery_metric, holdable).
narrative_ontology:cs_axiom_grounding('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', classical_standard_as_recovery_metric, conventional).
narrative_ontology:cs_axiom('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', secondary, symbolic_reoccupation_methodologically_necessary).
narrative_ontology:cs_axiom_status(symbolic_reoccupation_methodologically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', symbolic_reoccupation_methodologically_necessary, instrumental).
narrative_ontology:cs_reference_frame('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', classical_latin_as_unified_standard).
narrative_ontology:cs_drift_state('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', contemporary_genomic_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fb2922a-450e-4aa0-b8e3-0f975bf2b20e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philology_establishment).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, traditional_grammar_authorities).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_textual_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, alternative_recovery_methodologies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, students_and_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, philological societies, editorial boards controlling canonical texts and peer-review standards for Latin studies. They define 'correct' Latin by reference to Classical authors (Cicero, Virgil, Livy) and treat medieval variants as corruptions. Their authority derives from institutional control of textual transmission and credentialing of Latinists. They benefit from maintaining the discontinuity framework because it justifies their expertise and resource control.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philology_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Grammarians, lexicographers, commentators whose reference works and authority rest on describing a unified Classical system. Treating Medieval Latin as separate would require wholesale revision and would fragment their authoritative framework. They benefit from maintaining the discontinuity constraint because their professional status and scholarship depend on its validity. Exit would require cognitive reorientation and loss of prestige.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, traditional_grammar_authorities, beneficiary,
    institutional, generational, constrained, global).

% The body of medieval Latin texts (theological, legal, administrative, liturgical documents, 5th-15th centuries). Systematically devalued as corruptions rather than understood on their own linguistic terms. Scribal intent is rendered invisible. Texts cannot speak in their own voice; every reading is mediated through a Classical corrective lens. Trapped because the texts themselves cannot resist or exit; they are subject to emendation and reinterpretation by classical authorities.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_textual_tradition, payer,
    powerless, civilizational, trapped, global).

% Linguists, codicologists, historians who reconstruct medieval Latin from internal evidence (manuscript variants, scribal practices, institutional patterns) without classical templates. Systematically marginalized in peer-reviewed Latin philology. Their methodology and research are excluded from prestige journals and hiring ladders. They pay through reduced professional resources and visibility; they are excluded because their approach challenges the discontinuity reading's framework.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, alternative_recovery_methodologies, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, alternative_recovery_methodologies, excluded).

% Learners of Latin are taught a single unified classical system via classical texts and classical grammar rules. Medieval texts are excluded or presented as degenerate. Learners internalize the premise that 'proper' Latin is the classical form. Exit from this framing requires cognitive reorientation and identity disruption (admitting the language taught as 'the' language was only one historical state). Identity-locked because the learned framework becomes part of professional and linguistic identity.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, students_and_learners, payer,
    powerless, biographical, identity_locked, global).

% Codicologists, palaeographers, manuscript scholars who handle medieval texts directly and observe internal linguistic patterns that classical philology cannot explain. Their expertise is devalued in Latin philology despite their asymmetric knowledge. They remain observers rather than decision-makers because classical text-criticism still dominates the prestige hierarchy.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, manuscript_specialists, observer,
    moderate, generational, constrained, global).

% The texts of Cicero, Virgil, Livy, etc. treated as the fixed, recoverable standard for 'real Latin'. Vindicated as the authoritative instantiation of correct Latin by the discontinuity reading's framework. This non-agent entry acknowledges that the texts themselves collect no benefit but that the constraint's operation treats them as the fixed reference point.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_authors_corpus, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__discontinuity_reading, classical_authors_corpus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_philology_establishment).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, teachable, reference-able system of Latin grammar and lexicon by anchoring all Latin to a fixed classical standard; enables communication across time and institutions about Latin texts and meanings via a common metalanguage and framework.
% TRANSFER_FUNCTION: Moves institutional authority, prestige, and research resources from medieval linguistic specialists toward classical philologists; subordinates medieval textual understanding and alternative recovery methodologies to classical correction paradigms; channels pedagogy, funding, and hiring decisions toward classical texts and away from medieval sources.
% ABSENT_VOICES: Medieval scribes and authors cannot defend their linguistic choices in the framework; codicologists and palaeographers lack decision-making power in Latin philology peer-review and hiring; comparative linguists trained in evolutionary frameworks are excluded from prestige positions; alternative readings of medieval text populations remain unpublished or relegated to marginal venues outside classical-philology journals.
% DISAPPEARANCE_RATIONALE: If the discontinuity constraint vanished, Latin philology would bifurcate: Classical Latin would be studied as a historical-linguistic system to be recovered from its own texts; Medieval Latin would be studied as a distinct linguistic system with internal coherence, phonological rules, and lexical-syntactic patterns. Pedagogies would fragment (Classical track, Medieval track, or hybrid). Research resources would redistribute toward medieval specialists. Textual criticism would employ different methodologies for medieval texts (internal manuscript evidence, scribal-practice analysis, codicological dating) rather than forcing them into classical templates. The institutional landscape would reorganize around two distinct Latin systems rather than one.
% FOUNDING_PROBLEM: After the fall of the Western Roman Empire, Latin usage fragmented across regions and centuries, producing texts (legal, liturgical, administrative, theological) whose forms diverged substantially from classical models. Renaissance and Early Modern scholars needed to recover the structure of Classical Latin from fragmentary and sometimes corrupted medieval copies, and to authenticate genuine ancient works against medieval forgeries. The classical standard solved this problem by providing a fixed reference point against which textual authority could be adjudicated and authenticity could be determined.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists and linguists trained in the Neogrammarian tradition attest the founding problem remains live: Medieval Latin still deviates substantially from Classical norms; Classical reference remains necessary to recover authentic ancient texts and prevent conflation of classical and medieval forms; the problem is ongoing because every new medieval manuscript poses the same authentication challenge. Codicologists, medieval historians, and comparative linguists (e.g., grammaticalization theorists) attest the founding problem has been solved or superseded: Medieval texts can be analyzed using internal methods without classical reference; manuscript genealogy and codicological dating are more reliable authentication methods than classical conformity; treating Medieval as a linguistic system in its own right actually improves understanding of both Medieval and Classical Latin; the classical standard now actively obscures rather than illuminates medieval linguistic structures.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers interpretive authority and prestige from medieval specialists to classical authorities; suppression (0.71) is higher because the constraint actively excludes and marginalizes alternative recovery methodologies that would treat Medieval Latin as a distinct system. Theater ratio (0.42) is moderate-rising: the coordination function (unified system provision) is real and genuine, but an increasing share of enforcement activity defends classical authority against empirical challenges rather than serving the coordination function itself. The measurement series shows extractiveness and suppression both rising from t0 to approximately t10-15, then plateauing — a pattern consistent with institutional entrenchment and normalization of the classical standard as 'natural' rather than requiring active defense. Accessibility collapse (0.63) is moderate: alternatives (Medieval-linguistic methodology, internal recovery approaches) remain conceptually imaginable but are institutionally invisible, creating structural collapse of alternatives within the peer-review landscape. Resistance (0.59) reflects substantial pushback from codicologists, comparative linguists, and medieval historians, though this resistance is largely invisible in classical-philology journals and hiring decisions.
 *
 * PERSPECTIVAL GAP:
 *   From the classical-philology establishment seat, the constraint solves a genuine problem (recovering authentic Classical texts from medieval copies) and provides real coordination value (unified reference system for teaching and scholarship). From the medieval-textual-tradition seat, the constraint is primarily extractive: medieval meanings are systematically subordinated to classical correction; medieval scribal intent is rendered invisible; medieval Latin exists only as a problem to be fixed, not as a linguistic system to be understood. From the alternative-methodology seat, the constraint is suppressive: legitimate scholarly approaches are excluded from prestige journals and hiring decisions; institutional rewards flow to classical conformity, not methodological innovation. The engine should compute tangled_rope at the classical-establishment seat (genuine coordination benefit, concentrated) but snare at the medieval-tradition and alternative-methodology seats (extraction with suppression, no direct coordination benefit). This divergence reflects asymmetric structural positions, not disagreement about the world.
 *
 * DIRECTIONALITY LOGIC:
 *   The discontinuity reading itself posits that Classical and Medieval Latin are distinct linguistic systems, with Classical treated as the correct/authoritative one and Medieval as a departed/corrupted variant. This posture generates asymmetric directionality: classical authorities benefit from the constraint's operation (d near 0.0 for the classical_philology_establishment, full beneficiary); medieval tradition and alternative methodologies bear extraction costs (d near 1.0 for medieval_textual_tradition, full target). Students experience moderate asymmetry (d ~0.65): they are constrained by the classical-only pedagogy but also receive coordination benefits (unified system, teachable grammar). The constraint's beneficiary structure (classical authorities) differs from its victim structure (medieval tradition, alternative methods) by institutional power, prestige, and resource access. This power differential is sustained by the discontinuity reading's authority grounding (institutional inheritance, canonical text control) rather than by empirical dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested': classical philologists argue the founding problem remains live (Medieval texts still corrupt; Classical authentication still necessary), while codicologists and medieval historians argue it is dead (Medieval texts can be analyzed internally; Classical correction actually obscures medieval structure). The disappearance_verdict is 'world_rearranges': if the discontinuity constraint vanished, Latin philology would bifurcate into two distinct linguistic systems with different methods, pedagogies, and prestige hierarchies — the world would restructure. This (dead_founding_problem, world_rearranges) mismatch is a mandatrophy signal: the constraint persists not because the founding problem remains urgent but because institutional beneficiaries maintain the apparatus. The theater_ratio trajectory (0.28 → 0.42) shows rising performative maintenance relative to functional problem-solving, consistent with a constraint that has solved its original purpose but persists through inertia and institutional investment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_incommensurability,
    'Are Classical and Medieval Latin incommensurable linguistic systems, or do they share a common underlying structure that evolved over time?',
    'Comparative analysis of core morphological paradigms (noun declension, verb conjugation) across texts from 1st century BCE through 12th century CE, examining whether Medieval forms can be derived from Classical forms via regular sound changes and analogical extension, or whether they represent wholesale structural rupture.',
    'If derivable via regular processes, the discontinuity reading is superseded by a continuity or hybrid reading; if structurally incommensurable, the discontinuity reading is vindicated and Medieval Latin becomes a separate object of study.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, empirical, 'Whether the discontinuity between Classical and Medieval Latin is fundamental or apparent.').

omega_variable(
    reconstruction_methodology_adequacy,
    'Can Medieval Latin be adequately described using internal, manuscript-based evidence alone, without reference to Classical templates?',
    'Attempt to construct a complete phonological, morphological, and syntactic description of 9th-century Latin based only on manuscript variants, scribal practices, and internal patterns, without consulting Classical grammars. Test whether the resulting description is internally consistent and predictive.',
    'If internally adequate, alternative methodologies are epistemically valid and suppression of medieval-linguistic analysis is institutional gatekeeping; if inadequate, classical standard retains epistemic justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_methodology_adequacy, empirical, 'Whether medieval-linguistic reconstruction is methodologically independent of classical reference.').

omega_variable(
    authority_grounding_shift,
    'Is the discontinuity reading''s dominance in Latin philology grounded in superior explanatory power, or in institutional inheritance and prestige hierarchies?',
    'Longitudinal citation analysis of Latin scholarship and peer-review patterns: do rejections cite empirical problems or category dismissals? What proportion of prestige goes to classical conformity vs. methodological innovation?',
    'If institutional inheritance dominates, the constraint operates as extraction disguised as coordination; if empirical power dominates, the classical standard retains scientific justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_shift, conceptual, 'Whether discontinuity reading dominance reflects epistemic merit or gatekeeping.').

omega_variable(
    kernel_reading_committer_uncertainty,
    'Is this constraint the discontinuity_reading instantiation, or do the authored metrics suggest a different reading (continuity or hybrid) is more structurally accurate?',
    'Cross-reading comparison: if Medieval forms are internally coherent and derivable via regular change (empirical finding), the continuity_reading becomes the correct structural description. If Medieval and Classical are truly incommensurable (empirical finding), discontinuity_reading is correct. If some parts (morphology) are continuous and others (syntax/lexicon) required recovery, hybrid_reading is correct. The committer frame declares which reading THIS constraint instantiates; alternative readings are OTHER constraints.',
    'The reading chosen determines ε (discontinuity reading has high ε for extraction from medieval tradition; continuity reading would have lower ε for internal evolution; hybrid reading would have layered ε). Adopting a different reading would create a different constraint with different beneficiaries, victims, and founding problems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_uncertainty, conceptual, 'Committer frame: which kernel reading is instantiated here?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__discontinuity_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__discontinuity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__discontinuity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__discontinuity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__discontinuity_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__discontinuity_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__discontinuity_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__discontinuity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__discontinuity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__discontinuity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__discontinuity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__discontinuity_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__discontinuity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__discontinuity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__discontinuity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__discontinuity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__discontinuity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(corr_su_t30, correct_latin_kernel__discontinuity_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% correct_latin_kernel is a contested kernel with three structurally distinct readings. This constraint (discontinuity_reading) treats Classical and Medieval Latin as incommensurable systems requiring symbolic reoccupation. Sibling reading continuity_reading treats Medieval as evolved Classical; hybrid_reading treats parts as continuous and parts as requiring recovery. Each reading is a distinct constraint with distinct ε, beneficiaries, victims. They coexist as live positions in scholarly debate; none logically forecloses the others. The readings are linked via network.affects_constraints (this constraint influences both siblings; both siblings influence this constraint) to enable contamination propagation and cross-reading consistency analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
