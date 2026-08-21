% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Exogenous Coercion
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto as an act of federal
 *   coercion forcing the abandonment of a divine requirement, rather than a
 *   legitimate doctrinal reinterpretation. The federal government, acting as
 *   the agenda-setter, imposed severe penalties (imprisonment, property
 *   seizure) on practicing polygamists and the church leadership, effectively
 *   suppressing the practice of plural marriage. This reading frames the
 *   constraint as a snare, where the coordination story (assimilation,
 *   statehood) serves as cover for the coercive extraction of religious
 *   autonomy and practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.85).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Exogenous Coercion").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, 'ec64ff6e-ceb1-4f20-9302-8e402edfb0a7').
narrative_ontology:cs_kernel_codification('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', fixed_text).
narrative_ontology:cs_authority_grounding('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', extraction).
narrative_ontology:cs_interpretation_layer_present('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7').
narrative_ontology:cs_reading_relation('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', foundational, divine_mandate_for_plural_marriage_unconditional).
narrative_ontology:cs_axiom_status(divine_mandate_for_plural_marriage_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', divine_mandate_for_plural_marriage_unconditional, theological).
narrative_ontology:cs_axiom('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', foundational, federal_coercion_as_illegitimate_override).
narrative_ontology:cs_axiom_status(federal_coercion_as_illegitimate_override, holdable).
narrative_ontology:cs_axiom_grounding('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', federal_coercion_as_illegitimate_override, deontological).
narrative_ontology:cs_reference_frame('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', divine_mandate_unconditional).
narrative_ontology:cs_drift_state('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', post_1890_manifesto, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('ec64ff6e-ceb1-4f20-9302-8e402edfb0a7', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, mainstream_us_society).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_leadership_under_duress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who believed in and practiced plural marriage as a divine commandment. They faced severe legal penalties, including imprisonment, property confiscation, and disenfranchisement, for continuing their practice after the Manifesto.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, local).

% The highest echelons of the church, who issued the 1890 Manifesto under immense federal pressure. They faced the threat of complete disincorporation of the church, seizure of all its property, and the imprisonment of its leaders, forcing a capitulation to avoid existential destruction.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_leadership_under_duress, payer,
    organized, biographical, constrained, national).

% The legislative and executive branches of the United States government, which actively enforced anti-polygamy laws. Their goal was to assert federal sovereignty over the Utah Territory and enforce national social norms, seeing plural marriage as an affront to American values.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The broader American public and its institutions, which largely viewed plural marriage as immoral and uncivilized. They benefited from the perceived triumph of national norms and the eventual integration of Utah into the Union as a state conforming to these norms.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, mainstream_us_society, beneficiary,
    organized, generational, mobile, national).

% Scholars who analyze the historical events surrounding the 1890 Manifesto, examining primary sources, political pressures, and theological interpretations to understand the motivations and consequences of the church's decision.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the federal government's perspective, the constraint coordinated the legal and social practices of the Utah Territory with the prevailing norms and laws of the United States, enabling its eventual statehood.
% TRANSFER_FUNCTION: Transfers the authority over religious practice, specifically plural marriage, from the church and its members to the federal government, in exchange for an end to persecution and the path to statehood.
% ABSENT_VOICES: Those who continued to practice plural marriage in secret or in exile (e.g., in Mexico or Canada) were absent from the official discourse, as their continued adherence to the practice was deemed illegal and illegitimate by the dominant powers.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and the federal coercion that prompted it vanished, the historical trajectory of the church, its relationship with the US government, and the social fabric of Utah would be fundamentally different. Plural marriage might have continued openly, altering demographic, political, and religious landscapes.
% FOUNDING_PROBLEM: The federal government viewed the practice of plural marriage by a significant religious group in a US territory as a challenge to national sovereignty, a moral affront to mainstream American values, and an impediment to Utah's statehood.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records, Supreme Court decisions (e.g., Reynolds v. US), and contemporary public discourse corroborate the federal government's framing of the problem. However, the church's own historical accounts and theological interpretations present a counter-narrative, asserting religious freedom and divine mandate, leading to a contested status of the 'problem' itself.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the federal government successfully extracted conformity to its social norms and legal framework, at a high cost to the church and its members. Suppression is extremely high due to the direct and severe legal enforcement mechanisms employed. The theater ratio is low because the coercion was overt and effective, with little performative pretense masking its true nature. Resistance was significant, with many continuing the practice in secret or in exile, but ultimately the federal power prevailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal government and mainstream US society, the Manifesto represented a necessary step towards national unity and moral order. From the perspective of practicing polygamists and the church leadership, it was a forced abandonment of a divine commandment under duress, a profound loss of religious freedom and autonomy. This reading explicitly adopts the latter perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and mainstream US society are clear beneficiaries, achieving their goals of national conformity and social norm enforcement. Practicing polygamists and the church leadership under duress are the primary victims, bearing the direct costs of legal penalties and the abandonment of a core religious practice. Their exit options were severely constrained or trapped, amplifying the effective extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_authenticity,
    'Was the practice of plural marriage genuinely a divine requirement, or was it a mutable doctrine subject to human interpretation and adaptation?',
    'Theological and historical analysis of religious texts and prophetic claims, alongside sociological studies of religious authority and doctrinal evolution. No definitive empirical resolution is possible.',
    'If the divine mandate is viewed as absolute and immutable, the coercion is a clear infringement on religious freedom. If it''s seen as mutable, the coercion might be framed as forcing an overdue adaptation, reducing the perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_authenticity, conceptual, 'Ambiguity regarding the immutability of the divine mandate for plural marriage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent did the suppression of plural marriage become internalized within the church community, beyond direct federal enforcement?',
    'Post-Manifesto sociological studies of community norms and individual adherence, examining whether the practice persisted in secret due to belief or was abandoned due to internalized social pressure.',
    'If suppression became largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals self-regulated their behavior even without direct federal oversight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the abandonment of plural marriage.').

omega_variable(
    sibling_reading_endogenous_reinterpretation_impact,
    'How would the structural classification change if the ''endogenous_reinterpretation_reading'' were adopted, which claims legitimate prophetic reinterpretation?',
    'Adopting the sibling reading would shift the primary authority grounding from ''extraction'' to ''lineage'' or ''expertise'', and significantly lower the ''extractiveness'' and ''suppression'' metrics, reclassifying the constraint closer to a ''rope'' or even ''mountain'' (as a divinely ordained change).',
    'The constraint would be reclassified from a ''snare'' to a ''rope'' or ''mountain'', as the change would be seen as voluntary and divinely guided, rather than coerced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_endogenous_reinterpretation_impact, conceptual, 'Impact of adopting the ''endogenous_reinterpretation_reading'' on classification.').

omega_variable(
    sibling_reading_institutional_pragmatism_impact,
    'How would the structural classification change if the ''institutional_pragmatism_reading'' were adopted, which frames the Manifesto as strategic institutional adaptation?',
    'Adopting the sibling reading would acknowledge the coercion but emphasize the church''s agency in choosing survival. While still recognizing high suppression, it might slightly lower ''extractiveness'' by attributing some ''benefit'' to the church (survival, statehood), potentially shifting the classification towards a ''tangled_rope'' or ''scaffold'' (as a temporary, albeit coerced, adaptation).',
    'The constraint might shift from a ''snare'' to a ''tangled_rope'' or ''scaffold'', acknowledging the coercive context but emphasizing the institutional choice for survival and adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_institutional_pragmatism_impact, conceptual, 'Impact of adopting the ''institutional_pragmatism_reading'' on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1894, 0.09).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1898, 0.08).
narrative_ontology:measurement(plur_tr_t1901, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1901, 0.09).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.1).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.8).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1894, 0.83).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1898, 0.85).
narrative_ontology:measurement(plur_be_t1901, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1901, 0.86).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1894, 0.93).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1898, 0.92).
narrative_ontology:measurement(plur_su_t1901, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1901, 0.91).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
