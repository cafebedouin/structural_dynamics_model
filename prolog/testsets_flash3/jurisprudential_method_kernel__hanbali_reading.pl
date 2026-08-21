% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's jurisprudential
 *   methodology, which emphasizes strict textual literalism from the Qur'an,
 *   Hadith, and Companion opinions, while largely rejecting analogical
 *   reasoning (qiyas) and juristic preference (istihsan) as illegitimate
 *   innovations (bid'ah). It asserts that only unanimous consensus (ijma) is
 *   a valid secondary source. This reading is one of several competing
 *   methodologies within Islamic jurisprudence, each forming a distinct
 *   constraint on legal derivation. The high extractiveness reflects the
 *   intellectual and practical costs imposed on jurists and communities
 *   seeking more flexible interpretations.
 *
 * KEY AGENTS:
 *   - textualist_scholars: Agenda-setter (institutional/identity_locked) — gains authority from strict adherence.
 *   - conservative_religious_authorities: Beneficiary (institutional/constrained) — leverages rigidity for social control.
 *   - rationalist_jurists: Payer (moderate/constrained) — intellectually limited, work devalued.
 *   - customary_practice_advocates: Payer (powerless/trapped) — local norms delegitimized.
 *   - lay_muslims_seeking_flexibility: Payer (powerless/constrained) — experience rigidity in daily life.
 *   - other_madhhab_scholars: Excluded (institutional/mobile) — implicitly challenged by Hanbali claims of purity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.75).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, snare).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'd7cad809-d29e-40ee-b9b6-43df48eda7f2').
narrative_ontology:cs_kernel_codification('d7cad809-d29e-40ee-b9b6-43df48eda7f2', fixed_text).
narrative_ontology:cs_authority_grounding('d7cad809-d29e-40ee-b9b6-43df48eda7f2', lineage).
narrative_ontology:cs_interpretation_layer_present('d7cad809-d29e-40ee-b9b6-43df48eda7f2').
narrative_ontology:cs_reading_relation('d7cad809-d29e-40ee-b9b6-43df48eda7f2', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('d7cad809-d29e-40ee-b9b6-43df48eda7f2', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7cad809-d29e-40ee-b9b6-43df48eda7f2', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('d7cad809-d29e-40ee-b9b6-43df48eda7f2', foundational, textual_literalism_is_primary).
narrative_ontology:cs_axiom_status(textual_literalism_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('d7cad809-d29e-40ee-b9b6-43df48eda7f2', textual_literalism_is_primary, deontological).
narrative_ontology:cs_axiom('d7cad809-d29e-40ee-b9b6-43df48eda7f2', foundational, qiyas_istihsan_are_bidah).
narrative_ontology:cs_axiom_status(qiyas_istihsan_are_bidah, holdable).
narrative_ontology:cs_axiom_grounding('d7cad809-d29e-40ee-b9b6-43df48eda7f2', qiyas_istihsan_are_bidah, theological).
narrative_ontology:cs_reference_frame('d7cad809-d29e-40ee-b9b6-43df48eda7f2', early_salaf_practice).
narrative_ontology:cs_drift_state('d7cad809-d29e-40ee-b9b6-43df48eda7f2', contemporary_global_islam, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d7cad809-d29e-40ee-b9b6-43df48eda7f2', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, conservative_religious_authorities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_advocates).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, lay_muslims_seeking_flexibility).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, purity_of_early_islam_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for strict adherence to the literal text of the Qur'an and Hadith, and the opinions of the Companions. They gain authority and legitimacy by presenting themselves as guardians of the 'pure' tradition, rejecting innovation (bid'ah) in legal methodology. Their careers and social standing are often tied to this interpretive stance.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the Hanbali method's emphasis on literalism, as it provides clear, often rigid, answers that align with their conservative social and political agendas. This method reduces interpretive ambiguity, which they can leverage to maintain control and suppress dissent, even if they do not actively develop the jurisprudence themselves.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, conservative_religious_authorities, beneficiary,
    institutional, generational, constrained, national).

% Are intellectually constrained by the Hanbali method's rejection of analogical reasoning (qiyas) and juristic preference (istihsan). Their ability to address novel legal issues or adapt to changing social contexts is severely limited, often leading to their marginalization or accusations of bid'ah. Their scholarly work is devalued by this constraint.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Represent local traditions and practices that may not have direct textual basis but have evolved over time. The Hanbali method often invalidates these practices as 'innovation,' leading to social friction and the suppression of local legal autonomy. They bear the cost of having their established norms delegitimized.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_advocates, payer,
    powerless, biographical, trapped, local).

% Experience the Hanbali method as a rigid legal system that offers little room for adaptation to modern life or personal circumstances. They may find themselves in conflict with legal rulings that do not account for contemporary realities, leading to a sense of alienation or difficulty in practicing their faith in a way that feels relevant.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslims_seeking_flexibility, payer,
    powerless, immediate, constrained, local).

% Scholars from other schools (Hanafi, Maliki, Shafii) who employ broader interpretive tools are often dismissed or criticized by Hanbali adherents. While they have their own established traditions, the Hanbali method's claims of purity and textual fidelity implicitly challenge their legitimacy, even if they are not directly governed by it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, other_madhhab_scholars, excluded,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for deriving law, reducing interpretive variance by strictly limiting sources to foundational texts and early community consensus, thereby coordinating legal practice around a shared, literalist understanding.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and evolving social contexts to a fixed body of sacred texts and early opinions, concentrating power in those who master and transmit these texts literally.
% ABSENT_VOICES: Rationalist jurists, advocates for contextual interpretation, and those whose customary practices are invalidated are structurally excluded from the Hanbali discourse; they would argue for the necessity of reason and adaptation in legal derivation, but their methods are deemed illegitimate 'innovation'.
% DISAPPEARANCE_RATIONALE: If the Hanbali method's strictures vanished, Islamic jurisprudence would immediately diversify, with a resurgence of analogical reasoning, juristic preference, and contextual interpretation. Legal systems in regions currently influenced by Hanbali thought would undergo significant reform, leading to a more flexible and adaptable body of law.
% FOUNDING_PROBLEM: The early Islamic community faced a proliferation of opinions and methods for deriving law, leading to perceived inconsistencies and a lack of unified legal guidance, particularly as the empire expanded and encountered diverse customs.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and conservative authorities within the Hanbali tradition continue to assert that the problem of interpretive deviation and innovation remains live, requiring strict adherence to their methodology. However, scholars from other schools and independent legal historians argue that the problem has evolved, and the Hanbali method's rigidity now creates new problems rather than solving the original one, with corroboration from historical legal debates and contemporary social challenges.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the Hanbali method severely limits the tools available for legal reasoning, forcing jurists into a narrow interpretive frame and invalidating alternative approaches. Suppression (0.75) is also high, as the method actively labels broader reasoning as 'innovation' (bid'ah), which carries significant religious and social stigma, effectively suppressing intellectual dissent. The theater ratio is low (0.20) because the method is genuinely applied, though its claims of absolute textual purity may have a performative aspect in asserting authority. Accessibility collapse is moderate (0.60) as alternative methods exist but are actively delegitimized, and resistance is moderate (0.40) due to ongoing debates within Islamic legal philosophy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of textualist scholars, this method is a necessary 'rope' for preserving the purity of Islamic law, ensuring consistency and preventing deviation. From the perspective of rationalist jurists and those advocating for customary practice, it operates as a 'snare,' extracting intellectual freedom and adaptability, and suppressing legitimate interpretive diversity. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative religious authorities are beneficiaries (low d) as their authority and worldview are reinforced by the method's rigidity. Rationalist jurists, customary practice advocates, and lay Muslims seeking flexibility are targets (high d) as they bear the costs of intellectual constraint, delegitimization, and practical inflexibility. Other madhhab scholars are excluded, as their methods are implicitly challenged, but they operate within their own established frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali method's persistence is rooted in its claim to preserve the 'pure' tradition, a mandate that remains 'live' for its adherents. However, the high and increasing extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest that the method may have drifted from its original coordination function (reducing interpretive chaos) to an extractive mechanism that benefits specific scholarly and religious authorities by suppressing alternative legal thought. The classification as a Snare, despite its historical claims of coordination, prevents mislabeling it as a benign 'rope' or 'mountain' of tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_qiyas_istihsan,
    'Is analogical reasoning (qiyas) and juristic preference (istihsan) a legitimate tool for deriving Islamic law, or is it an illegitimate innovation (bid''ah)?',
    'Theological and philosophical debate, historical analysis of early Islamic legal practice, and the acceptance or rejection of these methods by a broad consensus of contemporary Islamic scholars.',
    'If deemed legitimate, the Hanbali method''s high extractiveness on rationalist jurists would be reclassified as unjustified, potentially shifting the constraint towards a Snare. If confirmed as bid''ah, the Hanbali method''s suppression would be seen as justified, reinforcing its internal coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_qiyas_istihsan, conceptual, 'The fundamental disagreement over the role of human reason in legal derivation.').

omega_variable(
    unanimous_consensus_scope,
    'What constitutes ''unanimous consensus'' (ijma) and how broadly must it be applied to be valid?',
    'Historical and methodological studies of ijma, and the practical application of this principle in diverse legal contexts. Does it require consensus of all scholars, or only a specific community?',
    'A narrow definition of ijma would further restrict legal sources, increasing extractiveness. A broader definition might allow for more flexibility, reducing the constraint''s suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimous_consensus_scope, empirical, 'Ambiguity in the scope and application of unanimous consensus as a legal source.').

omega_variable(
    identity_lock_of_textualist_scholars,
    'To what extent is the ''identity_locked'' exit option for textualist scholars a genuine structural constraint versus a chosen ideological stance?',
    'Sociological studies of scholarly communities, analysis of career paths and institutional incentives, and examination of scholars who have transitioned between jurisprudential schools.',
    'If primarily ideological, the ''identity_locked'' status would reflect a preference rather than a structural trap, potentially lowering the computed effective extraction for this seat. If genuinely structural (e.g., loss of livelihood, social ostracization), it reinforces the high effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_textualist_scholars, empirical, 'Distinguishing structural from ideological identity lock for textualist scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 300, 0.75).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 900, 0.83).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 900, 0.73).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'jurisprudential_method_kernel', each representing a major school of Islamic legal thought. They are linked as a constraint family, with each reading influencing or coexisting with the others through their distinct methodological claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
