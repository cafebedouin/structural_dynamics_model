% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Principle (Naskh)
 *   domain: islamic_jurisprudence/hermeneutics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'progressive restriction' reading of the
 *   Naskh principle in Islamic jurisprudence. It posits that Quranic
 *   revelation moved from more permissive to more restrictive rulings, not
 *   through direct abrogation (textual invalidation) but as a form of divine
 *   pedagogy, where earlier permissions were transitional accommodations.
 *   This reading provides a framework for legal development but restricts the
 *   application of earlier, more permissive verses. This constraint is one
 *   reading of the 'naskh_principle' kernel, alongside 'classical_abrogation'
 *   and 'contextual_harmonization'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.65).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.55).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.65).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Principle (Naskh)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/hermeneutics/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '6faa9d66-c08d-4921-b526-2b17fe075265').
narrative_ontology:cs_kernel_codification('6faa9d66-c08d-4921-b526-2b17fe075265', fixed_text).
narrative_ontology:cs_authority_grounding('6faa9d66-c08d-4921-b526-2b17fe075265', lineage).
narrative_ontology:cs_interpretation_layer_present('6faa9d66-c08d-4921-b526-2b17fe075265').
narrative_ontology:cs_reading_relation('6faa9d66-c08d-4921-b526-2b17fe075265', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('6faa9d66-c08d-4921-b526-2b17fe075265', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_axiom('6faa9d66-c08d-4921-b526-2b17fe075265', foundational, divine_pedagogy_in_revelation).
narrative_ontology:cs_axiom_status(divine_pedagogy_in_revelation, holdable).
narrative_ontology:cs_axiom_grounding('6faa9d66-c08d-4921-b526-2b17fe075265', divine_pedagogy_in_revelation, theological).
narrative_ontology:cs_axiom('6faa9d66-c08d-4921-b526-2b17fe075265', foundational, later_revelation_refines_earlier_permissions).
narrative_ontology:cs_axiom_status(later_revelation_refines_earlier_permissions, holdable).
narrative_ontology:cs_axiom_grounding('6faa9d66-c08d-4921-b526-2b17fe075265', later_revelation_refines_earlier_permissions, conventional).
narrative_ontology:cs_reference_frame('6faa9d66-c08d-4921-b526-2b17fe075265', early_islamic_legal_development).
narrative_ontology:cs_drift_state('6faa9d66-c08d-4921-b526-2b17fe075265', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6faa9d66-c08d-4921-b526-2b17fe075265', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, islamic_legal_tradition).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_interpreters).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, advocates_for_earlier_permissive_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars actively promote and apply the progressive restriction principle to interpret Quranic verses, seeing it as a sophisticated method for understanding divine pedagogy and legal development. They benefit from the intellectual coherence and authority it provides to their interpretive framework.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_scholars, agenda_setter,
    institutional, generational, constrained, global).

% The broader Islamic legal tradition benefits from this principle by gaining a systematic method to reconcile apparent contradictions and chronological shifts in Quranic legal rulings, thereby maintaining its internal coherence and adaptability over time. Its identity is deeply intertwined with such interpretive frameworks.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, islamic_legal_tradition, beneficiary,
    institutional, civilizational, identity_locked, global).

% These interpreters often prioritize the literal meaning and equal validity of all Quranic verses, regardless of chronology. The progressive restriction principle diminishes the legal force of earlier, more permissive texts they might wish to apply, forcing them to either conform or be marginalized within mainstream discourse.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_interpreters, payer,
    organized, biographical, constrained, global).

% Individuals or groups who advocate for the contemporary application of earlier, more permissive Quranic rulings find their arguments undermined by the progressive restriction principle, which frames these verses as transitional rather than permanently applicable law. They bear the cost of having their preferred interpretations de-legitimized.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, advocates_for_earlier_permissive_rulings, payer,
    moderate, biographical, constrained, global).

% Scholars adhering to the classical abrogation theory, which posits direct supersession of earlier verses by later ones, are conceptually distinct. While their approach also supersedes earlier texts, the 'pedagogical' framing of progressive restriction differs from their direct 'abrogation' mechanism. They are excluded from the specific framing of progressive restriction as the *sole* or *primary* mechanism.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_scholars, excluded,
    institutional, generational, constrained, global).

% Scholars who seek to harmonize all Quranic verses by assigning them specific contextual validity, without any supersession, find their interpretive framework directly challenged by the progressive restriction principle. Their approach is marginalized by the assertion that earlier permissions are transitional and later restrictions represent final divine intent.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent hermeneutical framework for reconciling chronologically disparate Quranic verses, particularly the movement from more permissive to more restrictive injunctions, allowing the Islamic legal tradition to develop and adapt while maintaining textual authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from earlier, more permissive readings of certain verses to later, more restrictive ones, and from individual textual interpretation to a systematic hermeneutical principle that guides legal development.
% ABSENT_VOICES: Scholars who insist on the perpetual and equal validity of all Quranic verses regardless of chronology, or who advocate for a purely contextual reading without any notion of supersession, are structurally marginalized. They would argue for a broader interpretive pluralism.
% DISAPPEARANCE_RATIONALE: If the progressive restriction principle vanished overnight, the coherence of Islamic legal development would be significantly challenged. Many established rulings would require re-evaluation, and there would likely be a resurgence of arguments based on earlier, more permissive texts, leading to widespread interpretive and legal reorganization.
% FOUNDING_PROBLEM: The apparent contradictions or chronological shifts in legal rulings within the Quran, specifically the observed movement from more permissive to more restrictive injunctions over the course of revelation, which required a theological and legal explanation.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing scholarly debates within Islamic jurisprudence, the historical development of various legal schools, and the persistent need for a principle to reconcile these textual dynamics attest to the founding problem's continued relevance. Independent textual analysis also confirms the chronological shifts in revelation.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this principle effectively de-legitimizes or limits the application of earlier, more permissive Quranic texts, thereby extracting interpretive freedom from those who would rely on them. Suppression (0.55) reflects the intellectual and institutional pressure within scholarly circles to conform to this interpretive framework, marginalizing alternative readings. Theater ratio (0.1) is low as this is a genuine hermeneutical principle, not a performance, though its defense involves scholarly rhetoric. Accessibility collapse (0.7) is high because it significantly narrows the range of valid interpretations for certain verses. Resistance (0.6) is moderate, reflecting ongoing scholarly debate and the persistence of alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of evolutionary legal scholars, this principle is a necessary and elegant solution for understanding the Quran's legal development, providing coherence and adaptability. However, from the perspective of literalist interpreters or advocates for earlier permissive rulings, it functions as an extractive mechanism that diminishes the authority and applicability of texts they consider equally valid. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary legal scholars and the broader Islamic legal tradition are beneficiaries, as the principle provides a robust framework for legal development and interpretive coherence. Literalists and advocates for earlier permissive rulings are victims, as their preferred interpretations are constrained or superseded. The principle's 'active enforcement' occurs through scholarly consensus, educational curricula, and judicial precedent within Islamic legal systems.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the principle as a pure Rope (ignoring the extraction from alternative readings) or a Snare (ignoring its genuine coordination function in legal development). It acknowledges both the intellectual coordination it provides for the Islamic legal tradition and the asymmetric extraction it imposes on those who prefer more permissive or purely contextual interpretations. The founding problem of reconciling textual shifts is still live, but the method of resolution carries costs for certain interpretive communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_basis_for_divine_pedagogy,
    'Is there independent empirical or historical evidence for the concept of ''divine pedagogy'' in the revelation process, or is it primarily an interpretive construct developed post-hoc to reconcile textual shifts?',
    'Detailed historical-critical analysis of early Islamic intellectual history and the socio-religious context of revelation, seeking explicit statements or implicit understandings of pedagogical intent from primary sources.',
    'If divine pedagogy is found to be a post-hoc construct, the naturalness claim of the progressive restriction principle weakens, potentially increasing its computed extractiveness as a human-imposed interpretive framework. If strongly evidenced, it reinforces the principle''s coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_basis_for_divine_pedagogy, empirical, 'The evidential grounding for the ''divine pedagogy'' claim.').

omega_variable(
    scope_of_progressive_restriction,
    'To what extent does ''progressive restriction'' apply across all legal domains, and how consistently is it applied in practice versus other interpretive methods?',
    'Comparative legal analysis across different schools of Islamic jurisprudence and historical periods, mapping the specific instances where progressive restriction is invoked versus classical abrogation or contextual harmonization.',
    'If the principle is applied inconsistently or only in specific, limited domains, its overall ''suppression'' and ''extractiveness'' might be lower than currently assessed, as alternative interpretations retain more space. If universally applied, the current metrics are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_progressive_restriction, empirical, 'The practical scope and consistency of the progressive restriction principle.').

omega_variable(
    legitimacy_of_earlier_texts_post_restriction,
    'Do earlier, more permissive texts retain any independent legal or moral force after being ''progressively restricted'', or are they rendered entirely superseded in their legal applicability?',
    'Analysis of fatwas (legal opinions) and judicial rulings in cases where earlier permissive texts are cited, to determine if they are ever given weight, even if secondary to later restrictions.',
    'If earlier texts retain some residual force, the ''extraction'' from advocates of those texts is mitigated. If they are entirely superseded, the extraction is complete, reinforcing the current high extractiveness score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_earlier_texts_post_restriction, conceptual, 'The residual legal force of ''restricted'' verses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t350, naskh_principle__progressive_restriction, theater_ratio, 350, 0.08).
narrative_ontology:measurement_basis(nask_tr_t350, observed).
narrative_ontology:measurement(nask_tr_t700, naskh_principle__progressive_restriction, theater_ratio, 700, 0.1).
narrative_ontology:measurement_basis(nask_tr_t700, observed).
narrative_ontology:measurement(nask_tr_t1050, naskh_principle__progressive_restriction, theater_ratio, 1050, 0.1).
narrative_ontology:measurement_basis(nask_tr_t1050, observed).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__progressive_restriction, theater_ratio, 1400, 0.1).
narrative_ontology:measurement_basis(nask_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t350, naskh_principle__progressive_restriction, base_extractiveness, 350, 0.55).
narrative_ontology:measurement_basis(nask_be_t350, observed).
narrative_ontology:measurement(nask_be_t700, naskh_principle__progressive_restriction, base_extractiveness, 700, 0.62).
narrative_ontology:measurement_basis(nask_be_t700, observed).
narrative_ontology:measurement(nask_be_t1050, naskh_principle__progressive_restriction, base_extractiveness, 1050, 0.64).
narrative_ontology:measurement_basis(nask_be_t1050, observed).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__progressive_restriction, base_extractiveness, 1400, 0.65).
narrative_ontology:measurement_basis(nask_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t350, naskh_principle__progressive_restriction, suppression_requirement, 350, 0.45).
narrative_ontology:measurement_basis(nask_su_t350, observed).
narrative_ontology:measurement(nask_su_t700, naskh_principle__progressive_restriction, suppression_requirement, 700, 0.52).
narrative_ontology:measurement_basis(nask_su_t700, observed).
narrative_ontology:measurement(nask_su_t1050, naskh_principle__progressive_restriction, suppression_requirement, 1050, 0.54).
narrative_ontology:measurement_basis(nask_su_t1050, observed).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__progressive_restriction, suppression_requirement, 1400, 0.55).
narrative_ontology:measurement_basis(nask_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, islamic_family_law).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, islamic_finance_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'naskh_principle' kernel, alongside 'classical_abrogation' and 'contextual_harmonization'. Each reading offers a distinct hermeneutical approach to reconciling apparent chronological shifts and contradictions in Quranic legal verses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
