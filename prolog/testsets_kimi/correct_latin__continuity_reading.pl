% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading of Correct Latin: Medieval as Evolved Classical
 *   domain: historical/linguistic/intellectual
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the contested
 *   kernel 'correct_latin': the claim that legitimate Latin is the form
 *   transmitted through continuous living practice from Classical antiquity
 *   through the medieval period, with no rupture between the two. Under this
 *   reading, medieval Latin is not a corrupt deviation to be reconstructed
 *   away, but the legitimate evolved state of the language. The philological
 *   establishment (universities, journals, editorial boards) enforces this
 *   framework through hiring, peer review, and curriculum design, expanding
 *   the legitimate object of study to include medieval forms while
 *   downgrading classical reconstructionist approaches. This is a kernel
 *   reading: the natural-language phrase 'correct Latin' conflates three
 *   structurally distinct normative commitments (continuity, discontinuity,
 *   hybrid), each of which is authored as a separate constraint story.
 *
 * KEY AGENTS:
 *   - Philological establishment (agenda_setter): Universities, academies, and editorial boards that certify 'correct Latin' and enforce continuity-framework standards through institutional gatekeeping.
 *   - Medievalist scholars (beneficiary/organized): Gain scholarly legitimacy, curriculum space, and career pathways when medieval Latin is treated as evolved Classical rather than corrupt deviation.
 *   - Classical reconstructionists (payer/organized): Bear the cost of diminished methodological authority and reduced institutional priority for textual-reconstruction approaches.
 *   - Text-critical scholars (excluded/moderate): Marginalized voices who treat medieval forms as corruptions to be emended toward Classical norms; largely absent from mainstream philological discourse.
 *   - Latin students (dual beneficiary-payer): Receive a unified curricular tradition but may lose exposure to rigorous Classical normativity.
 *   - Historical linguists (observer/analytical): Outside the philological power structure, they study the debate as a sociolinguistic phenomenon without institutional stake in its outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.52).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.42).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading of Correct Latin: Medieval as Evolved Classical").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical/linguistic/intellectual").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '9926c08a-61f3-410e-a549-a4d82b83c8cb').
narrative_ontology:cs_kernel_codification('9926c08a-61f3-410e-a549-a4d82b83c8cb', implicit).
narrative_ontology:cs_authority_grounding('9926c08a-61f3-410e-a549-a4d82b83c8cb', practice).
narrative_ontology:cs_interpretation_layer_present('9926c08a-61f3-410e-a549-a4d82b83c8cb').
narrative_ontology:cs_reading_relation('9926c08a-61f3-410e-a549-a4d82b83c8cb', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('9926c08a-61f3-410e-a549-a4d82b83c8cb', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('9926c08a-61f3-410e-a549-a4d82b83c8cb', foundational, unbroken_historical_continuity).
narrative_ontology:cs_axiom_status(unbroken_historical_continuity, holdable).
narrative_ontology:cs_axiom_grounding('9926c08a-61f3-410e-a549-a4d82b83c8cb', unbroken_historical_continuity, empirically_contingent).
narrative_ontology:cs_axiom('9926c08a-61f3-410e-a549-a4d82b83c8cb', foundational, medieval_forms_normatively_legitimate).
narrative_ontology:cs_axiom_status(medieval_forms_normatively_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9926c08a-61f3-410e-a549-a4d82b83c8cb', medieval_forms_normatively_legitimate, conventional).
narrative_ontology:cs_reference_frame('9926c08a-61f3-410e-a549-a4d82b83c8cb', continuous_practice_authority).
narrative_ontology:cs_drift_state('9926c08a-61f3-410e-a549-a4d82b83c8cb', contemporary_philological_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9926c08a-61f3-410e-a549-a4d82b83c8cb', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medievalist_scholars).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classical_reconstructionists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, text_critical_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, latin_students).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, latin_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the standards of Latin scholarship through university curricula, peer-reviewed journals, and academic hiring. Certifies which forms and methodologies count as legitimate, integrating medieval Latin into the mainstream canon while maintaining gatekeeping functions against discontinuity-framed approaches.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, philological_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Study medieval Latin texts and language under a framework that grants their object of study full legitimacy as evolved Classical Latin. Benefit from expanded curricular presence, dedicated publication venues, and institutional recognition that their period is not a corruption but a valid phase of the language.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medievalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Specialize in recovering Classical Latin norms through textual criticism and historical reconstruction. Their methodological priority is downgraded under the continuity framework, which treats medieval manuscript readings as potentially legitimate rather than corruptions to be emended; they face shrinking institutional space for pure Classical normativity.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_reconstructionists, payer,
    organized, biographical, constrained, global).

% Approach Latin texts with a priority on reconstructing authorial or Classical originals, often treating medieval manuscript variants as errors. Their perspective is increasingly marginalized in mainstream philological discourse, though it persists in specialized text-editing subdisciplines.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, text_critical_scholars, excluded,
    moderate, biographical, constrained, global).

% Study Latin within a curriculum that presents the language as a continuous tradition from antiquity through the Middle Ages. They gain a unified field of study but may encounter less rigorous Classical normativity than in a reconstructionist program; they do not choose the framework but inherit it from institutional design.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, latin_students, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, latin_students, payer).

% Study the historical development of Latin and Romance languages from an analytical standpoint outside the normative debates of philology. They observe the continuity-discontinuity contest as a sociolinguistic and institutional phenomenon without being structured by its authority claims.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, historical_linguists, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, medievalist_scholars).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents balkanization of Latin studies into disconnected Classical and Medieval specializations by providing a single normative framework that grants medieval forms the same legitimacy as Classical forms, enabling unified curricula, shared journals, and cross-period scholarly communication.
% TRANSFER_FUNCTION: Moves authority to define 'correct Latin' from text-critical classical reconstructionism to continuous-practice medievalist and historical philology; moves curricular space, hiring lines, and editorial priority toward integrative approaches that include medieval developments.
% ABSENT_VOICES: Text-critical scholars who treat medieval forms as corruptions to be emended back toward Classical norms; tradition-exclusive Classicists who reject medieval developments as methodologically illegitimate; these voices survive in specialized text-editing circles but are largely absent from mainstream philological discourse and major journal editorial boards.
% DISAPPEARANCE_RATIONALE: If the continuity framework disappeared, Latin curricula would split into separate Classical and Medieval tracks with no shared standard of correctness, editorial practices in major journals would revert to privileging Classical conjecture over manuscript fidelity for medieval texts, and the institutional unity of the field would fragment as period-specialization boundaries hardened.
% FOUNDING_PROBLEM: The nineteenth-century fragmentation of Latin studies into isolated Classical and Medieval specializations, with medieval Latin treated as unworthy of serious philological attention or as merely corrupt data requiring reconstruction back to an idealized Classical norm.
% FOUNDING_PROBLEM_CORROBORATION: Medievalist historians and early twentieth-century philologists corroborate the existence of the institutional split. However, classical philologists outside the benefiting parties dispute that fragmentation was the core problem, arguing instead that the real issue was insufficient rigor in textual criticism; independent historians of philology document the split but debate whether continuity was the necessary remedy.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the continuity framework genuinely solves a coordination problem (preventing balkanization of Latin studies) but asymmetrically transfers methodological authority and resources from classical reconstructionists to medievalist scholars. Suppression (0.42) reflects active though not overwhelming enforcement: discontinuity-framed work faces higher barriers in mainstream venues but survives in specialized text-critical niches. Theater ratio (0.32) captures the performative dimensionâsome continuity claims function more to legitimate medievalist institutional expansion than to describe actual historical linguistic process. Accessibility collapse (0.60) is relatively high because once a scholar is trained in the continuity framework, the discontinuity view appears methodologically naive. Resistance (0.35) is moderate: classical philologists continue to resist but are losing institutional ground. The temporal series show extraction rising as the framework became dominant mid-interval, then slightly stabilizing as counter-arguments persist.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (philological establishment) experiences the constraint as necessary coordination that keeps the field unified and prevents destructive periodization wars. The payer seats (classical reconstructionists) experience the same structure as extraction that downgrades their methodological commitments and restricts their access to mainstream legitimacy. Medievalist beneficiaries experience it as earned recognition. The engine computes this divergence from the structural asymmetry in exit options and role declarationsânot from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Medievalist_scholars are the structural beneficiaries: the constraint subsidizes their field's legitimacy and resource base (d near 0.0). Classical_reconstructionists and text_critical_scholars are the structural targets: the constraint extracts methodological authority from them and redirects it (d near 1.0). Latin_students sit near symmetric: they gain curricular unity but lose Classical rigor. The philological_establishment is an administrative beneficiary with constrained exit: they enforce the framework because their institutional legitimacy depends on maintaining a unified discipline.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents two errors: (1) treating it as pure rope would ignore the asymmetric extraction from classical scholars whose authority is diminished; (2) treating it as pure snare would ignore the genuine coordination functionâwithout some continuity assumption, Latin studies would fragment into non-communicating Classical and Medieval specializations. The mandatrophy risk here is obsolescence: if the founding problem (19th-century balkanization) is solved, the framework should wither, but it persists because it now serves as a resource-allocation mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_empirical_or_normative,
    'Is the continuity between Classical and Medieval Latin an empirically demonstrable historical linguistic fact, or a normative scholarly construction that reclassifies medieval forms as legitimate?',
    'Comparative historical-linguistic analysis independent of philological institutional commitments; sociological study of how scholarly norms allocate legitimacy across period boundaries.',
    'If the continuity is primarily normative, the constraint''s extraction is higher than its coordination function suggests; if empirical, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_empirical_or_normative, conceptual, 'Whether the continuity claim is empirical fact or normative construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the dominance of the continuity reading maintained by structural institutional gatekeeping or by internalized scholarly consensus that makes discontinuity unthinkable?',
    'Tracking acceptance rates of discontinuity-framed submissions in major journals and hiring outcomes for classical reconstructionists before and after explicit policy shifts.',
    'If internalized, effective suppression exceeds structural measures because scholars self-censor; if purely structural, removing gatekeepers might restore pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of discontinuity readings.').

omega_variable(
    reform_internal_vs_external_framing,
    'Does the continuity reading''s insistence on internal adjustment foreclose necessary external textual reconstruction for periods where manuscript transmission is demonstrably corrupt?',
    'Case studies of editorial practice where continuous-practice assumptions produced demonstrably inferior texts compared to stemmatic reconstruction.',
    'If internal adjustment systematically produces worse texts, the continuity reading''s coordination function is undermined and its extraction increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_internal_vs_external_framing, empirical, 'Whether internal-reform methodology suffices for corrupt transmission streams.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_cont_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(correct_latin_cont_tr_t20, correct_latin__continuity_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(correct_latin_cont_tr_t40, correct_latin__continuity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(correct_latin_cont_tr_t60, correct_latin__continuity_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(correct_latin_cont_tr_t80, correct_latin__continuity_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(correct_latin_cont_tr_t100, correct_latin__continuity_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(correct_latin_cont_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(correct_latin_cont_be_t20, correct_latin__continuity_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(correct_latin_cont_be_t40, correct_latin__continuity_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(correct_latin_cont_be_t60, correct_latin__continuity_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(correct_latin_cont_be_t80, correct_latin__continuity_reading, base_extractiveness, 80, 0.53).
narrative_ontology:measurement(correct_latin_cont_be_t100, correct_latin__continuity_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_cont_su_t0, correct_latin__continuity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(correct_latin_cont_su_t20, correct_latin__continuity_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(correct_latin_cont_su_t40, correct_latin__continuity_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(correct_latin_cont_su_t60, correct_latin__continuity_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(correct_latin_cont_su_t80, correct_latin__continuity_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(correct_latin_cont_su_t100, correct_latin__continuity_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'correct_latin'. The kernel decomposes into at least three structurally distinct constraints because the natural-language label 'correct Latin' conflates competing normative claims about the relationship between Classical and medieval forms. This reading (continuity) treats medieval Latin as legitimate evolved Classical Latin; the discontinuity reading treats medieval Latin as corrupt deviation; the hybrid reading treats medieval Latin as transmitted but correctable via textual evidence. Each reading has a different Îµ, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
