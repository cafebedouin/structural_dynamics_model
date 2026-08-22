% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 Abrogating Universal Jihad Obligation
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   This constraint instantiates the abrogating_universal reading of the
 *   quran_9_5_scope kernel: Quran 9:5 ('Then, when the sacred months have
 *   passed, slay the polytheists wherever you find them...') is held to
 *   abrogate (nasikh) all prior verses enjoining patience, peace, or
 *   non-aggression toward non-Muslims, establishing universal offensive jihad
 *   as a permanent, individual or communal legal obligation until polytheists
 *   convert or submit. The reading is claimed as natural law by its adherents
 *   (divine command) but operates as a constructed extractive constraint: it
 *   creates a global victim category (all non-Muslims outside treaty),
 *   confers categorical legitimacy on expansionist violence, and suppresses
 *   all alternative hermeneutic or political frameworks. The claim is a
 *   theological mountain; the metrics describe a snare.
 *
 * KEY AGENTS:
 *   - expansionist_movements: Primary beneficiary/agenda_setter (organized/global/mobile exit) â administers violence and collects submission
 *   - non_muslim_populations: Primary target (powerless/global/constrained exit) â bears extraction through violence, subjugation, or coerced conversion
 *   - peaceful_coexistence_advocates: Excluded voice (moderate/global/constrained exit) â suppressed alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Abrogating Universal Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/theological").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '3e7a155c-4813-449a-9532-a41a361b3694').
narrative_ontology:cs_kernel_codification('3e7a155c-4813-449a-9532-a41a361b3694', fixed_text).
narrative_ontology:cs_authority_grounding('3e7a155c-4813-449a-9532-a41a361b3694', lineage).
narrative_ontology:cs_interpretation_layer_present('3e7a155c-4813-449a-9532-a41a361b3694').
narrative_ontology:cs_reading_relation('3e7a155c-4813-449a-9532-a41a361b3694', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('3e7a155c-4813-449a-9532-a41a361b3694', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('3e7a155c-4813-449a-9532-a41a361b3694', foundational, universal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3e7a155c-4813-449a-9532-a41a361b3694', universal_offensive_jihad_obligation, theological).
narrative_ontology:cs_axiom('3e7a155c-4813-449a-9532-a41a361b3694', foundational, total_abrogation_peaceful_verses).
narrative_ontology:cs_axiom_status(total_abrogation_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('3e7a155c-4813-449a-9532-a41a361b3694', total_abrogation_peaceful_verses, theological).
narrative_ontology:cs_reference_frame('3e7a155c-4813-449a-9532-a41a361b3694', classical_expansionist_jurisprudence).
narrative_ontology:cs_drift_state('3e7a155c-4813-449a-9532-a41a361b3694', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e7a155c-4813-449a-9532-a41a361b3694', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, abrogation_naskh_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, classical_jihad_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim divine mandate to enforce universal offensive jihad under the abrogating reading of 9:5. Administer violence, territorial expansion, and subjugation of non-Muslim populations, collecting jizya, booty, and political submission as the fruits of conquest. Their legitimacy depends on maintaining the abrogation framework as eternally binding.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_movements, agenda_setter,
    organized, generational, mobile, global).

% Categorized as legitimate military targets unless they convert to Islam, submit to dhimmi status with jizya, or secure a temporary treaty of protection. Bear the costs of violence, displacement, institutionalized second-class status, and systematic exclusion from political equality under the abrogating legal framework.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, immediate, constrained, global).

% Muslim and non-Muslim scholars and activists who advocate for permanent peaceful coexistence, pluralism, or contextual limitation of 9:5. Their theological and political arguments are delegitimized and suppressed by the abrogation logic, which rules their readings out of bounds a priori.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, peaceful_coexistence_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the external relations of the Muslim polity by subordinating all non-treaty non-Muslims to a single tripartite legal status â conversion, submission, or legitimate warfare â eliminating interpretive pluralism over the use of force against non-believers.
% TRANSFER_FUNCTION: Moves political submission, territorial control, and material wealth (jizya, booty, productive assets) from non-Muslim populations to expansionist Muslim polities; moves theological authority from contested interpretive traditions to the abrogation doctrine itself.
% ABSENT_VOICES: Contextualist scholars, progressive Muslim theologians, and non-Muslim political actors who reject the abrogation framework and advocate for permanent peaceful coexistence are systematically excluded from the authoritative interpretive community; their arguments are ruled out by the abrogation logic itself.
% DISAPPEARANCE_RATIONALE: If the abrogating obligation vanished, the legal justification for offensive jihad would collapse, non-Muslim populations would no longer be categorically legitimate targets, coexistence frameworks would re-emerge as viable theological options, and expansionist movements would lose their primary divine mandate for territorial conquest.
% FOUNDING_PROBLEM: The early Muslim polity faced treaty-breaking polytheist tribes in Medina who threatened the community's survival; classical jurists later required a framework to regulate relations between an expanding empire and subject non-Muslim populations.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists within the Hanbali and Shafi'i traditions attest the problem as still live (disbelief as ongoing casus belli). Modern Islamic international law scholars, human rights bodies, and contextualist theologians from outside the beneficiary set attest the founding problem is historically specific and no longer operative.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint authorizes first-strike seizure of life, liberty, and property from an entire class defined by religious identity, with no upper bound. Suppression is higher still (0.90) because the constraint's persistence depends on actively excluding contextual, progressive, and peaceful alternative readings from legitimacy, not on voluntary adherence. Theater_ratio is moderate (0.45): the elaborate abrogation apparatus (usul al-fiqh, classical tafsir, isnad chains) performs scholarly legitimacy while the underlying operation is territorial extraction. Accessibility_collapse is high (0.80) because once the abrogation frame is accepted, peaceful alternatives collapse almost entirely within that hermeneutic. Resistance is substantial (0.70) because targeted populations fight back, modern states reject the obligation, and rival Islamic readings contest it.
 *
 * PERSPECTIVAL GAP:
 *   The expansionist_movements seat experiences the constraint as divine coordination â a unifying legal mandate that eliminates factional disagreement and channels communal energy. The non_muslim_populations seat experiences the identical structure as pure extraction with a theological veneer. The peaceful_coexistence_advocates seat experiences it as an enforced silence. The engine computes this divergence from the structural data: same constraint, radically different directionality and effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist_movements are declared beneficiaries and agenda_setters; their exit is mobile (they could adopt a different reading), yielding a low directionality value and low/negative effective extraction â the constraint subsidizes their political project. Non_muslim_populations are declared victims (payers); their exit is constrained (conversion or submission are costly and identity-altering), yielding a high directionality and amplified effective extraction. Peaceful_coexistence_advocates are excluded rather than coordinated; their suppression is the condition for the constraint's stability.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy and victim/beneficiary declaration, this constraint could be misread as a Tangled Rope: it does coordinate the Muslim polity around a single legal norm, and classical jurists framed it as solving the problem of pagan treaty-breaking. However, the founding problem is contested or dead (the specific 7th-century tribal threat is gone), the coordination function has atrophied into perpetual extraction, and the victim set is global and categorical rather than situational. Classifying it as Snare prevents the coordination story from masking the extraction. If it were a genuine Tangled Rope, the victims would be limited to active combatants and the beneficiaries would include the coordinated community as a whole, not primarily the expansionist wing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_kernel_reading_stability,
    'Is this constraint one stable reading of Quran 9:5, or does the abrogating universal interpretation collapse into a different constraint under textual and historical critique?',
    'Comparison with sibling readings contextual_defensive and progressive_synthesis: if the abrogation claim cannot be sustained textually or historically, the constraint dissolves into a time-bound political directive or a narrow defensive exception.',
    'If the abrogation claim fails, the constraint''s victim set shrinks to nil and it reclassifies as a historical artifact or narrow coordination rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_kernel_reading_stability, conceptual, 'Reading stability under textual and historical critique').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of coexistence frameworks enforced by structural power (state violence, institutional control) or internalized theological conviction (self-policing by believers who accept abrogation as divine)?',
    'Post-liberation trajectory: if suppression of peaceful alternatives persists in the absence of state enforcement, the constraint is partially internalized; if it collapses immediately upon regime change, it was structural.',
    'Internalized suppression raises effective extraction because targets carry the constraint after external exit; structural suppression indicates pure snare dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in theological enforcement').

omega_variable(
    theological_cover_political_extraction,
    'Does the abrogation doctrine represent a genuine theological commitment to divine command, or is it a hermeneutic technology for legitimizing political expansion?',
    'Historical analysis of abrogation theory''s development: if nasikh/mansukh was systematized primarily under imperial expansion, the theological frame is cover for extraction.',
    'If primarily political, the claimed type remains snare; if genuinely theological, it might shift toward tangled_rope (coordinating believers around divine duty with extraction as side-effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_cover_political_extraction, empirical, 'Theological authenticity versus political instrumentalization of abrogation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.25).
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.3).
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t40, quran_9_5_scope__abrogating_universal, theater_ratio, 40, 0.35).
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t60, quran_9_5_scope__abrogating_universal, theater_ratio, 60, 0.38).
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t80, quran_9_5_scope__abrogating_universal, theater_ratio, 80, 0.42).
narrative_ontology:measurement(quran_9_5_scope_ab_tr_t100, quran_9_5_scope__abrogating_universal, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(quran_9_5_scope_ab_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(quran_9_5_scope_ab_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(quran_9_5_scope_ab_be_t40, quran_9_5_scope__abrogating_universal, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(quran_9_5_scope_ab_be_t60, quran_9_5_scope__abrogating_universal, base_extractiveness, 60, 0.8).
narrative_ontology:measurement(quran_9_5_scope_ab_be_t80, quran_9_5_scope__abrogating_universal, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(quran_9_5_scope_ab_be_t100, quran_9_5_scope__abrogating_universal, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(quran_9_5_scope_ab_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(quran_9_5_scope_ab_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(quran_9_5_scope_ab_su_t40, quran_9_5_scope__abrogating_universal, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(quran_9_5_scope_ab_su_t60, quran_9_5_scope__abrogating_universal, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(quran_9_5_scope_ab_su_t80, quran_9_5_scope__abrogating_universal, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(quran_9_5_scope_ab_su_t100, quran_9_5_scope__abrogating_universal, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is the abrogating_universal reading of the quran_9_5_scope kernel. Sibling readings instantiate structurally distinct constraints from the same verse due to epsilon-invariance: different empirical claims, different victim sets, different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
