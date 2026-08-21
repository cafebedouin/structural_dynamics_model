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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning
 *   domain: islamic_jurisprudence/legal_philosophy
 *
 * SUMMARY:
 *   This constraint is the Hanbali reading of the jurisprudential method
 *   kernel, emphasizing textual literalism and rejecting analogical reasoning
 *   (qiyas) and juristic preference (istihsan) as bid'ah (innovation) that
 *   corrupt the kernel. It accepts only unanimous consensus (ijma') as a
 *   valid secondary source. Sibling readings (Hanafi, Maliki, Shafii) offer
 *   alternative approaches to legal derivation, particularly regarding the
 *   role of human reason and local practice. The Hanbali reading's high
 *   extractiveness and suppression reflect its active delegitimization of
 *   these alternative methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.75).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "islamic_jurisprudence/legal_philosophy").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '1de8ba9c-808f-4218-a90f-fc5ec4ec7746').
narrative_ontology:cs_kernel_codification('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', fixed_text).
narrative_ontology:cs_authority_grounding('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', lineage).
narrative_ontology:cs_interpretation_layer_present('1de8ba9c-808f-4218-a90f-fc5ec4ec7746').
narrative_ontology:cs_reading_relation('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', foundational, textual_literalism_supremacy).
narrative_ontology:cs_axiom_status(textual_literalism_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', textual_literalism_supremacy, deontological).
narrative_ontology:cs_axiom('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', foundational, rejection_of_speculative_reasoning_as_bidah).
narrative_ontology:cs_axiom_status(rejection_of_speculative_reasoning_as_bidah, holdable).
narrative_ontology:cs_axiom_grounding('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', rejection_of_speculative_reasoning_as_bidah, deontological).
narrative_ontology:cs_reference_frame('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', purity_of_early_islamic_practice).
narrative_ontology:cs_drift_state('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', contemporary_islamic_legal_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('1de8ba9c-808f-4218-a90f-fc5ec4ec7746', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_madhhab_institutions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, non_hanbali_madhhabs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, lay_muslims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars uphold and propagate the Hanbali methodology, gaining authority and prestige from their strict adherence to textual literalism and rejection of innovation. Their professional identity is deeply intertwined with this interpretive framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    organized, generational, identity_locked, global).

% Educational and legal institutions that embody the Hanbali school. They enforce the methodological purity, derive their legitimacy from this adherence, and benefit from the clarity and perceived authenticity of the approach, attracting students and adherents.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_madhhab_institutions, agenda_setter,
    institutional, civilizational, constrained, regional).

% Jurists who advocate for broader use of analogical reasoning (qiyas) and juristic preference (istihsan). Their interpretive methods are delegitimized by the Hanbali reading, limiting their influence, scope of legal reasoning, and acceptance within conservative circles.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Local communities whose long-standing customary practices (urf) are not explicitly supported by the literal text of Qur'an or Hadith. These practices are often rejected as bid'ah, leading to legal uncertainty, social pressure, or suppression of traditional ways of life.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents, payer,
    powerless, generational, trapped, local).

% Other major schools of Islamic law (Hanafi, Maliki, Shafii) that employ different methodologies, including qiyas, istihsan, or local practice. While they have their own institutional bases, the Hanbali reading's strong textualist stance creates intellectual and sometimes institutional pressure against their methods, particularly in regions where Hanbali thought is dominant.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, non_hanbali_madhhabs, payer,
    institutional, civilizational, mobile, global).

% Benefit from the perceived clarity, certainty, and authenticity of a legal system strictly derived from foundational texts. However, they may find the law inflexible in novel situations not directly addressed by scripture, or experience their local customs invalidated by the strict methodology.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, lay_muslims, payer).

% Scholars and researchers who study the historical development, philosophical underpinnings, and societal impact of various Islamic jurisprudential methods without being bound by any particular school's internal commitments.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_madhhab_institutions).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for deriving Islamic law by prioritizing literal textual sources (Qur'an, Hadith, Companion opinions) and rejecting speculative reasoning, thereby reducing interpretive variance and ensuring fidelity to foundational texts.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning, juristic preference, and local custom to a strict textualist methodology and the scholars and institutions who master and uphold it.
% ABSENT_VOICES: Rationalist jurists who advocate for broader use of analogical reasoning and juristic preference, and local communities whose customary practices are not textually grounded, are structurally excluded from the legitimate discourse on legal derivation within this framework. They would argue for greater flexibility and contextual interpretation.
% DISAPPEARANCE_RATIONALE: If this methodological constraint vanished overnight, the landscape of Islamic jurisprudence would fundamentally shift. Other interpretive methods would gain prominence, leading to a wider array of legal opinions, potentially greater legal flexibility in novel cases, but also increased interpretive diversity and potential fragmentation of legal authority. The Hanbali madhhab as a distinct legal school would lose its defining characteristic.
% FOUNDING_PROBLEM: To prevent perceived corruption of Islamic law through speculative reasoning (ra'y) and unverified innovations (bid'ah), ensuring its purity and direct derivation from divine revelation and the practice of the earliest generations (salaf).
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and conservative religious institutions within the Hanbali tradition attest that the problem of innovation and speculative reasoning is still live and poses an ongoing threat to the purity of Islamic law. However, rationalist jurists, other Islamic legal schools, and independent historical analyses attest that the founding problem is often framed in a way that serves to consolidate power and authority for a particular interpretive methodology, rather than addressing a universally acknowledged, existential threat to the integrity of Islamic law.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extraction is very high (0.85) because this methodology severely limits the legitimate sources of law, effectively extracting interpretive freedom and flexibility from jurists and communities. Suppression is also high (0.75) as it actively enforces methodological purity, often through institutional pressure and scholarly discourse that marginalizes dissenting approaches. The theater ratio is low (0.15) because the adherence to textual sources is a direct and functional aspect of the Hanbali school's operation, with little performative maintenance of an atrophied function. The increasing trend in extractiveness and suppression over the interval reflects periods of consolidation or resurgence of textualist movements within Islamic legal history.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of textualist scholars and Hanbali institutions, this constraint is a necessary 'rope' that coordinates legal derivation around divine revelation, ensuring purity and authenticity. They perceive its high suppression as legitimate enforcement of divine command. From the perspective of rationalist jurists and adherents of customary practice, it operates as a 'snare' or 'tangled rope,' extracting their interpretive agency and delegitimizing their methods, while benefiting the textualist establishment.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and Hanbali institutions are the primary beneficiaries and agenda-setters, as they define and enforce the methodology, gaining authority and legitimacy. Rationalist jurists, customary practice adherents, and other madhhabs are the targets/payers, as their methods are suppressed or delegitimized. Lay Muslims are both beneficiaries (perceived clarity) and payers (inflexibility, invalidation of local customs).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve the purity of Islamic law by rejecting innovation is still considered 'live' by its proponents, preventing a clear mandatrophy resolution. However, the contestation around the 'founding problem status' suggests that for many, the constraint's function has shifted from genuine problem-solving to maintaining a specific interpretive power structure. The high extractiveness and suppression, despite the claimed coordination function, indicate that it operates more as a Tangled Rope than a pure Rope, with the coordination story serving to justify the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_bidah_ambiguity,
    'Is the Hanbali definition of ''bid''ah'' (innovation) a universally accepted theological principle, or a methodologically specific construct used to delimit acceptable legal reasoning?',
    'Comparative theological and jurisprudential analysis across diverse Islamic schools of thought, examining historical debates on the scope and application of ''bid''ah'' beyond the Hanbali context.',
    'If ''bid''ah'' is primarily a methodological construct, the constraint''s suppression of qiyas/istihsan is a preference-based choice rather than a theological necessity, weakening its claim to naturalness and increasing its perceived extractiveness. If universally theological, the suppression is more structurally inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_bidah_ambiguity, conceptual, 'Ambiguity in the definition and scope of ''bid''ah'' as a basis for rejecting interpretive methods.').

omega_variable(
    legal_adaptability_impact,
    'Does the strict textualist methodology adequately address novel legal challenges and societal changes not explicitly covered by foundational texts, or does it lead to legal stagnation and rigidity?',
    'Empirical study of legal rulings and fatwas issued within Hanbali frameworks on contemporary issues, compared with rulings from schools employing broader interpretive tools, assessing their practical applicability and societal reception.',
    'If the methodology proves rigid and unable to adapt, its coordination function for a dynamic society is compromised, increasing its effective extractiveness from those seeking relevant legal guidance. If it demonstrates adaptability, its coordination value is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_adaptability_impact, empirical, 'The impact of strict textualism on legal adaptability to modern challenges.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretive methods structural (institutional barriers, academic exclusion) or internalized (scholars self-censor to maintain identity/reputation within the Hanbali tradition)?',
    'Post-exit suppression trajectory: if scholars who leave the Hanbali tradition continue to avoid qiyas/istihsan, reclassify as partially internalized. Also, analysis of institutional funding and academic appointment patterns.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — scholars carry the suppression with them after exit. If purely structural, removing institutional barriers would more readily lead to methodological diversification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(juri_tr_t30, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(juri_be_t30, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(juri_su_t30, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, islamic_legal_education_curriculum).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, judicial_appointment_criteria).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, fatwa_issuance_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jurisprudential method kernel. See also: jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
