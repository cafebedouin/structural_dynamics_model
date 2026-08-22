% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Classical Latin Standard (Reconstruction Reading)
 *   domain: humanities/philology/commitment_system
 *
 * SUMMARY:
 *   The reconstruction reading of the classical_latin_standard kernel asserts
 *   that Correct Latin is the Classical form recoverable only through
 *   philological archaeology, requiring discontinuous return to textual
 *   sources and rejection of medieval developments as corruption. This
 *   reading emerges from early modern humanism and consolidates through the
 *   16th-18th centuries as humanist scholars recover manuscripts, establish
 *   comparative methods, and institutionalize philological training as the
 *   sole legitimate path to correctness. The reading directly conflicts with
 *   the continuity reading held by institutional clergy (who defend living
 *   transmission and functional adequacy of medieval forms) and coexists-with
 *   the hybrid reading (which argues for domain-specific standards). The
 *   reconstruction reading's core claim—that textual archaeology reveals
 *   objective truth unavailable to practitioners—is the hinge on which it
 *   extracts gatekeeping authority from medieval institutional users.
 *   Extractiveness rises measurably over the interval as humanist methods
 *   consolidate in universities and as practitioners' medieval Latin becomes
 *   systematically delegitimized. Theater ratio rises as well, indicating
 *   that an increasing share of enforcement activity is devoted to defending
 *   the textual-authority framework itself rather than practical
 *   coordination.
 *
 * KEY AGENTS:
 *   - humanist_philologists: institutional gatekeepers of correctness (power=institutional, exit=arbitrage); set the agenda and control the methods
 *   - medieval_clerical_practitioners: institutional users whose practiced competence is retroactively declared corrupt (power=powerful, exit=constrained); bear the extraction as delegitimization
 *   - ecclesiastical_scribes: skilled practitioners with identity-locked exit (power=moderate, exit=identity_locked); face retraining or exclusion
 *   - classical_education_institutions: universities and elite schools (power=institutional, exit=mobile); benefit from unified curriculum and gatekeeping prestige
 *   - continuity_advocates: excluded defenders of living practice (power=powerful, exit=constrained); represent the sibling reading's perspective
 *   - hybrid_theorists: excluded proposers of domain-specific standards (power=moderate, exit=constrained); represent a middle position between reconstruction and continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.79).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.84).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Standard (Reconstruction Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "humanities/philology/commitment_system").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '2a78cd38-aa3a-4166-bae7-7db5d2945227').
narrative_ontology:cs_kernel_codification('2a78cd38-aa3a-4166-bae7-7db5d2945227', fixed_text).
narrative_ontology:cs_authority_grounding('2a78cd38-aa3a-4166-bae7-7db5d2945227', extraction).
narrative_ontology:cs_interpretation_layer_present('2a78cd38-aa3a-4166-bae7-7db5d2945227').
narrative_ontology:cs_reading_relation('2a78cd38-aa3a-4166-bae7-7db5d2945227', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a78cd38-aa3a-4166-bae7-7db5d2945227', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('2a78cd38-aa3a-4166-bae7-7db5d2945227', foundational, discontinuous_recovery_from_antiquity_is_epistemic_truth).
narrative_ontology:cs_axiom_status(discontinuous_recovery_from_antiquity_is_epistemic_truth, holdable).
narrative_ontology:cs_axiom_grounding('2a78cd38-aa3a-4166-bae7-7db5d2945227', discontinuous_recovery_from_antiquity_is_epistemic_truth, empirically_contingent).
narrative_ontology:cs_axiom('2a78cd38-aa3a-4166-bae7-7db5d2945227', foundational, medieval_forms_are_corruption_not_adaptation).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruption_not_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('2a78cd38-aa3a-4166-bae7-7db5d2945227', medieval_forms_are_corruption_not_adaptation, empirically_contingent).
narrative_ontology:cs_reference_frame('2a78cd38-aa3a-4166-bae7-7db5d2945227', classical_textual_authority).
narrative_ontology:cs_drift_state('2a78cd38-aa3a-4166-bae7-7db5d2945227', post_humanist_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a78cd38-aa3a-4166-bae7-7db5d2945227', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_education_institutions).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_clerical_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_scribes).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, practitioners_of_vulgar_latin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the reconstruction of Classical Latin through textual scholarship, manuscript examination, and comparative analysis. Set the standard for correct form by recovering texts and establishing philological methods as the sole legitimate authority. Gate access to the 'true' Latin through mastery of these methods, establishing themselves as necessary intermediaries between texts and practitioners.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Universities and elite schools benefit from the reconstruction standard: it provides a unified curriculum framework grounded in objective textual authority rather than local practice variation. Certification of students as 'correctly trained' becomes possible because there is now a single standard to teach against. Their prestige and gatekeeping power increase.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_education_institutions, beneficiary,
    institutional, generational, mobile, continental).

% Institutional clergy who have practiced Latin as a living, transmuted language through centuries of use and adaptation. Their Latin—developed for legal documents, theological argument, liturgy, and administration—is now retroactively declared incorrect because it incorporates medieval developments, phonetic shifts, and functional innovations. Their authority as practitioners is delegitimized; continued use of forms they learned requires now defending them against charges of corruption.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_clerical_practitioners, payer,
    powerful, generational, constrained, continental).

% Professional copyists and document-writers whose mastery was built on scriptural tradition, ecclesiastical convention, and workplace practice passed through apprenticeship. The reconstruction standard retroactively declares their practiced competence 'medieval corruption.' To remain legitimate, they must abandon transmission-learned expertise and retrain in philological methods controlled by a different institutional class. Their identity as skilled practitioners is severed from their work.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, ecclesiastical_scribes, payer,
    moderate, biographical, identity_locked, continental).

% Non-elite speakers and writers using Latin for practical purposes—commerce, administration, daily communication. Their Latin is furthest from Classical norms and thus most vulnerable to the reconstruction standard's delegitimization. They lack institutional standing to contest the standard and cannot afford the education required to master Classical form. Their exclusion from 'correct' Latin-speaking becomes complete.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, practitioners_of_vulgar_latin, payer,
    powerless, biographical, trapped, local).

% Classical texts recovered from antiquity become the sole legitimate repository of correct form. The existence and interpretation of these texts grounds authority; their reconstruction is the vindicated path to truth. The constraint elevates manuscript evidence as the only valid evidence.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_texts, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__reconstruction_reading, classical_texts).

% Institutional defenders of living Latin practice who argue that natural linguistic development is legitimate and that medieval innovations serve real communicative functions. They would argue for practice-based authority, gradual legitimate change, and the validity of non-Classical forms for non-Classical purposes. They are excluded from the definitional conversation that settles what 'correct' means.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, continuity_advocates, excluded,
    powerful, generational, constrained, continental).

% Scholars and clerics who argue for domain-specific correctness: Classical form for literary imitation, but legitimate medieval developments for technical and ecclesiastical domains. They propose that correctness is context-dependent and that multiple standards can coexist. This position is excluded by the reconstruction standard's universalizing framing.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, hybrid_theorists, excluded,
    moderate, generational, constrained, continental).

% Agents who fabricate or alter texts to match the reconstruction standard's demands, or who selectively present evidence to support particular reconstructions. They exploit the system's dependence on textual authority and the scarcity of multiple independent manuscripts for verification.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, manuscript_forgers, observer,
    moderate, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified normative framework for Latin use across politically fragmented territories and competing institutional authorities. By anchoring correctness in recovered Classical texts, the standard enables certification of proficiency independent of local or monastic practice variation—anyone trained in philological methods can judge correctness against the same textual standard.
% TRANSFER_FUNCTION: Transfers authority from practitioners (clergy, scribes, institutional users) to humanist philologists and the educational institutions that employ them. Practitioners cede judgment of correctness to trained experts who alone can read manuscripts, apply comparative philology, and pronounce on authentic forms. Economic resources flow toward humanist centers of learning; institutional prestige concentrates among those who control the philological methodology.
% ABSENT_VOICES: Living practitioners of medieval and ecclesiastical Latin—clerics, scribes, document-writers, non-elite Latin speakers—are excluded from the conversation that defines correctness. Their objection would be that discontinuous rupture with centuries of practice destroys communicative continuity, that medieval innovations serve real institutional needs, and that practice-based authority is legitimate. Their exclusion is enforced by the reconstruction standard's claim that only philological expertise (not practice) can judge correctness.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished, practitioners would continue using medieval and ecclesiastical Latin forms; institutional documents would circulate in forms now declared 'incorrect'; educational standards would fragment back into local/monastic practice variation. The humanist gatekeeping class would lose the source of its authority and prestige. The neat separation between 'correct Classical form' and 'corrupt medieval drift' would dissolve into continuous practice.
% FOUNDING_PROBLEM: The fragmentation of Latin across territories and institutions after Rome's fall created mutual unintelligibility and erosion of Classical standards. No single authority existed to judge correctness; clergy in different regions developed different forms; scribal practice drifted. The founding problem is the loss of unified correctness after the rupture of Empire and continuous transmission.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists attest the problem is perpetual—every generation's Latin threatens to drift further from Classical norms unless continuously recovered through textual archaeology. Medieval clerical defenders and continuity advocates attest the problem was partly solved by living practice itself: medieval Latin became functional, stable, and communicatively adequate for its purposes. The founding problem's status depends on whether one reads the goal as perpetual recovery of a lost standard (humanist reading) or functional adequacy in practice (practitioner reading).
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as tangled_rope because it combines genuine coordination (unified standard enabling cross-institutional certification) with asymmetric extraction (gatekeeping authority concentrated in humanist philologists). Extractiveness (0.79) is high because the standard's persistence depends on suppressing practice-based authority and creating gatekeeping barriers that did not exist before. Suppression (0.84) is higher still because medieval practitioners must continuously defend their forms against charges of corruption, and institutional exclusion (via educational gatekeeping) makes exit costly. Theater ratio rises from 0.08 to 0.42 over the interval: early humanist recovery work is genuine textual scholarship, but as the standard consolidates, an increasing share of enforcement activity is devoted to defending the framework itself—declaring medieval forms corrupt, training new generations to reject practice authority, performing the authority of the texts. The shared time grid ensures every metric is authored at every examined point; the interval (0–25) represents the early modern period from early humanist recovery to consolidated institutional control.
 *
 * PERSPECTIVAL GAP:
 *   The reconstruction reading's humanist seats (philologists, universities) compute the constraint as genuine coordination addressing the foundational problem (fragmentation of Latin standards after Rome). Their d-value is low (~0.2, beneficiary range): they collect authority without running a trap. The medieval practitioner seats (clergy, scribes) compute the same structure very differently: they experience it as enforced delegitimization of their institutional competence and are locked into defending their practice against external judgment. Their d-value is high (~0.8, target range): they are the constraint's primary targets. The excluded continuity advocates and hybrid theorists occupy d~0.5 (symmetric pressure) because the reconstruction reading's suppression affects them equally regardless of whether they formally defend their position. The engine computes per-seat classification from power + exit + this d-distribution; divergence is expected and diagnostic of the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (humanist_philologists, classical_education_institutions) have high institutional power and arbitrage exit (humanists can move between universities or into private scholarship; universities can absorb the standard); their d-values are low (~0.15–0.25), placing them fully in the beneficiary range. Victims (medieval_clerical_practitioners, ecclesiastical_scribes) have either powerful institutional position but constrained exit (clergy tied to institutional roles) or moderate power with identity-locked exit (scribes whose professional identity fuses with the now-delegitimized practice). Their d-values are high (~0.75–0.85), placing them in the target range. The excluded seats (continuity_advocates, hybrid_theorists) have constrained or mobile exit but powerful/moderate power; they sit ~0.50 because they face suppression without having chosen to be targets, and their exclusion removes them from the conversation that determines correctness. Practitioners_of_vulgar_latin have powerless status and trapped exit; their d~0.95 marks them as the fully trapped extraction target, though they are less articulate in historical records than the clerical classes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmentation of Latin standards after Rome's fall) is legitimately real and not manufactured by the reconstruction reading. However, the reconstruction reading's claim that discontinuous recovery to Classical form is the only solution to fragmentation is contestable. The continuity reading argues that functional adequacy through practice (not textual fidelity) solved fragmentation: medieval clergy developed stable, mutually intelligible forms sufficient for institutional purposes. The founding problem's status is therefore contested—humanists read it as perpetually live (each generation risks new drift), while practitioners read it as substantially solved by the time humanism recovers texts. The reconstruction reading's mandatrophy analysis hinges on this contest: if the founding problem is still live and discontinuous recovery is the best solution, the constraint serves its original function and mandatrophy is not resolved. If the founding problem was solved by practice and the reading now pursues gatekeeping instead of problem-solving, mandatrophy is resolved—the founding problem is dead but the constraint persists. The omega on discontinuity plausibility directly probes this question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_vs_practice_authority,
    'Is textual fidelity to recovered Classical sources the only legitimate basis for correctness, or is practice-based authority (transmission, functional adequacy, institutional consistency) equally valid?',
    'Historical examination of how correctness was actually adjudicated: did practitioners defer to texts, or did texts defer to established practice? Were both systems ever treated as equivalent?',
    'If practice authority is structural, the reconstruction standard is imposing a novel authority hierarchy that delegitimizes centuries of institutional practice. The suppression and extractiveness would be confirmed as active enforcement of a contested claim, not discovery of an objective standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_vs_practice_authority, conceptual, 'Whether correctness is grounded in textual archaeology or practice-based institutional authority.').

omega_variable(
    discontinuity_vs_continuity_plausibility,
    'Is discontinuous recovery from antiquity actually possible without introducing philological reconstruction errors? Were medieval forms truly ''drift'' away from Classical norms, or adaptive development for different communicative purposes?',
    'Comparative linguistic analysis: do medieval forms show systematic drift from Classical patterns, or do they show systematic transformation serving specific institutional needs? Can the reconstruction standard actually recover an unambiguous Classical standard, or does it impose one by selective manuscript choice?',
    'If medieval forms are adaptive rather than corrupt, the reconstruction reading''s narrative of decline-and-recovery is false, and the constraint is pure extractive gatekeeping dressed as restoration. If discontinuous recovery is impossible (no unambiguous Classical standard exists in the manuscript record), the entire reading collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_vs_continuity_plausibility, empirical, 'Whether medieval Latin is corruption or adaptation; whether discontinuous recovery is actually possible.').

omega_variable(
    kernel_reading_contest,
    'What is the structural relationship between this reconstruction reading and the continuity and hybrid readings of the same classical_latin_standard kernel? Does each reading''s core premise foreclose the others, or do they coexist as live alternatives held by different institutional factions?',
    'Examine whether a single institutional actor (e.g. a university) or interpretive authority could coherently hold more than one reading simultaneously, or whether holding one reading requires rejecting the others'' fundamental premises.',
    'If readings coexist (different clergy defend continuity, different humanists defend reconstruction, some theorists defend hybridity), the kernel is genuinely contested and the reconstruction reading is one option among others. If this reading forecloses the others (logical contradiction at the framework level), the constraint is the winning side of a settled dispute. If it influences without foreclosing (creates pressure against the others), the dynamics are asymmetric suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship among readings of the classical_latin_standard kernel.').

omega_variable(
    gatekeeping_class_extraction,
    'How much of the measured extractiveness comes from the genuine coordination function (unified standard enabling cross-institutional certification) versus the gatekeeping effect (humanist philologists controlling access to legitimacy)?',
    'Counterfactual: could a unified Classical standard be administered through different institutional arrangements (e.g. clergy-controlled councils rather than humanist-controlled universities) without reducing the coordination benefit?',
    'If the coordination function is separable from humanist gatekeeping, the constraint shows high excess extraction beyond coordination cost. If gatekeeping is structural to how the standard is maintained, some extraction is coordination cost. The measured extractiveness (0.79) probably conflates both; the omega names the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_class_extraction, empirical, 'How much extraction is coordination cost versus excess gatekeeping.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of medieval forms structural (external barriers, legal/institutional exclusion from legitimate use) or internalized (practitioners internalize the shame of ''corruption'' and suppress their own practice)?',
    'Post-suppression trajectory: if medieval practitioners abandon their forms because of external enforcement, suppression decays when enforcement relaxes. If practitioners internalize stigma and avoid medieval forms even without enforcement, suppression persists. Track whether 16th-century humanist pressure physically excludes medieval Latin from contexts, or whether practitioners voluntarily abandon it.',
    'If internalized, the measured suppression (0.84) understates the constraint''s hold—practitioners carry the suppression with them even in contexts where enforcement is absent. If structural, the suppression depends on continuous enforcement machinery; decay of humanist institutional power would reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of medieval forms is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(clas_tr_t5, classical_latin_standard__reconstruction_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clas_tr_t10, classical_latin_standard__reconstruction_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(clas_tr_t15, classical_latin_standard__reconstruction_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__reconstruction_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(clas_tr_t25, classical_latin_standard__reconstruction_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clas_be_t5, classical_latin_standard__reconstruction_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(clas_be_t10, classical_latin_standard__reconstruction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(clas_be_t15, classical_latin_standard__reconstruction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__reconstruction_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(clas_be_t25, classical_latin_standard__reconstruction_reading, base_extractiveness, 25, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(clas_su_t5, classical_latin_standard__reconstruction_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(clas_su_t10, classical_latin_standard__reconstruction_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(clas_su_t15, classical_latin_standard__reconstruction_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__reconstruction_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(clas_su_t25, classical_latin_standard__reconstruction_reading, suppression_requirement, 25, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three constraint stories: reconstruction_reading (this constraint), continuity_reading, and hybrid_reading. Each story carries distinct ε values (reconstruction is high at 0.79, continuity is moderate, hybrid is intermediate) reflecting different structural relationships to the founding problem of fragmented Latin standards. The readings are not alternative measurements of one constraint—they instantiate genuinely different constraint structures grounded in different authority premises. The network edges record that reconstruction affects both sibling readings by creating pressure against their legitimacy (institutional channeling of authority toward humanist methods, educational gatekeeping that favors reconstruction over continuity/hybrid frameworks). All three readings share the same kernel codification (fixed_text: Classical manuscripts) but different authority_grounding values (reconstruction=extraction from the humanist seat; continuity=practice; hybrid=distributed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, powerful, 0.78).
constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, moderate, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
