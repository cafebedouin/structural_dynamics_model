% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Protections: Expansive Reading of Minority Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive_reading' of the Treaty of
 *   Lausanne's minority protections, which guarantees the functional
 *   continuity of pre-1923 non-Muslim religious governance in Turkey,
 *   including institutional self-administration, property rights, and clergy
 *   formation via theological schools. This reading frames the constraint as
 *   a 'rope' that coordinates the rights and existence of minority
 *   communities within the Turkish state, with beneficiaries being the
 *   minority institutions and communities themselves. The Turkish state,
 *   while bound by the treaty, often interprets its provisions more narrowly,
 *   leading to ongoing contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.15).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.1).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Protections: Expansive Reading of Minority Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '0abec905-3472-4ec6-8cde-f6764b264894').
narrative_ontology:cs_kernel_codification('0abec905-3472-4ec6-8cde-f6764b264894', fixed_text).
narrative_ontology:cs_authority_grounding('0abec905-3472-4ec6-8cde-f6764b264894', lineage).
narrative_ontology:cs_interpretation_layer_present('0abec905-3472-4ec6-8cde-f6764b264894').
narrative_ontology:cs_reading_relation('0abec905-3472-4ec6-8cde-f6764b264894', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('0abec905-3472-4ec6-8cde-f6764b264894', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('0abec905-3472-4ec6-8cde-f6764b264894', foundational, institutional_autonomy_guaranteed).
narrative_ontology:cs_axiom_status(institutional_autonomy_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('0abec905-3472-4ec6-8cde-f6764b264894', institutional_autonomy_guaranteed, deontological).
narrative_ontology:cs_axiom('0abec905-3472-4ec6-8cde-f6764b264894', foundational, property_rights_protected_by_treaty).
narrative_ontology:cs_axiom_status(property_rights_protected_by_treaty, holdable).
narrative_ontology:cs_axiom_grounding('0abec905-3472-4ec6-8cde-f6764b264894', property_rights_protected_by_treaty, conventional).
narrative_ontology:cs_reference_frame('0abec905-3472-4ec6-8cde-f6764b264894', post_ottoman_minority_settlement).
narrative_ontology:cs_drift_state('0abec905-3472-4ec6-8cde-f6764b264894', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0abec905-3472-4ec6-8cde-f6764b264894', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, non_muslim_minority_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on the Treaty of Lausanne for their legal existence, self-administration, property rights, and the ability to train clergy. Their functional continuity depends on the treaty's robust enforcement and expansive interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, non_muslim_minority_institutions, beneficiary,
    organized, generational, constrained, national).

% Their cultural and religious life, including education and communal practices, is sustained by the institutions protected by the treaty. They are highly vulnerable if these protections weaken or are narrowly interpreted, as their identity is deeply tied to these structures.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_communities, beneficiary,
    powerless, generational, identity_locked, national).

% As a signatory to the Treaty of Lausanne, the state is bound by its provisions. However, it often interprets the protections narrowly, leading to friction with minority communities and international bodies. It administers the laws that implement (or restrict) the treaty's application.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, constrained, national).

% Monitors human rights and treaty compliance, often advocating for the expansive interpretation of Lausanne protections through diplomatic channels and international forums.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, international_community, observer,
    institutional, civilizational, analytical, global).

% Specific signatories to the Treaty of Lausanne with a vested interest in its implementation. They may intervene diplomatically or legally if violations of minority rights occur, acting as external enforcers of the treaty.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continued legal existence, self-administration, and property rights of pre-1923 non-Muslim religious institutions within the Turkish state, preventing their dissolution or full assimilation into state control.
% TRANSFER_FUNCTION: Transfers autonomy and property rights to minority religious institutions, and implicitly transfers a degree of sovereign control over minority affairs from the Turkish state to the international treaty framework.
% ABSENT_VOICES: Ultra-nationalist factions within the Turkish state who advocate for full state control over all religious institutions and the complete assimilation of minorities; these voices are often marginalized in international discourse but exert domestic pressure.
% DISAPPEARANCE_RATIONALE: If the Lausanne protections, under an expansive reading, vanished overnight, the legal basis for minority religious institutions' self-administration, property, and clergy formation would be severely undermined or eliminated. This would lead to their rapid dissolution or absorption into state control, fundamentally altering the social and religious landscape for these communities and potentially triggering significant international outcry.
% FOUNDING_PROBLEM: The need to define and guarantee the status and rights of non-Muslim minorities (Greeks, Armenians, Jews) in the newly formed Republic of Turkey after the collapse of the Ottoman Empire, ensuring their continued existence and preventing forced assimilation or displacement in the post-war settlement.
% FOUNDING_PROBLEM_CORROBORATION: Minority community leaders, international human rights organizations, and some legal scholars attest to the ongoing relevance and necessity of these protections. The Turkish state acknowledges the treaty but often disputes the scope of its application, particularly regarding institutional autonomy, while still recognizing the existence of the minority communities.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'rope' because its primary function is to coordinate the continued existence and rights of minority religious groups, providing a net benefit to its beneficiaries (minority institutions and communities) without significant extraction from them. Extractiveness is low (0.15) as the constraint primarily grants rights rather than imposing costs on beneficiaries. Suppression is also low (0.1) as it's a protective measure, though some active enforcement is required to counter state-level resistance to its full application. Theater ratio is minimal (0.05) as the core functions are genuinely intended to be upheld. Accessibility collapse is high (0.7) for minorities, as the alternative of not having these protections would mean the collapse of their institutional autonomy. Resistance (0.2) comes primarily from elements within the Turkish state seeking to limit the scope of these protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority institutions and communities, the expansive reading of Lausanne protections is a vital, beneficial 'rope' ensuring their survival. From the perspective of the Turkish state, while acknowledging the treaty, the expansive reading might be seen as an external imposition that limits its sovereignty, leading to a different perception of the constraint's 'cost' or 'burden'. The engine's classification will reflect these structural differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Muslim minority institutions and communities are clear beneficiaries (low directionality), as the constraint directly secures their rights and existence. The Turkish state acts as an agenda-setter, bound by the treaty but also seeking to manage its application, placing it in a constrained position. International community and guarantor states act as observers, advocating for the expansive reading and providing external pressure for compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_contest_scope,
    'Is the ''expansive_reading'' of Lausanne protections the legally binding and intended interpretation, or is a more ''restrictive_reading'' (limiting to individual worship) or ''guarantor_reading'' (emphasizing international enforcement) more accurate?',
    'A definitive ruling by an international court (e.g., European Court of Human Rights), a clear consensus among international legal scholars, or consistent, unambiguous state practice over an extended period.',
    'If the ''restrictive_reading'' prevails, the constraint''s effective protection for minority institutions would collapse, potentially reclassifying it as a ''snare'' or ''piton'' in practice. If the ''guarantor_reading'' is fully adopted and enforced, the constraint''s ''rope'' function would be significantly strengthened by external mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_contest_scope, conceptual, 'Ambiguity regarding the authoritative scope of Lausanne protections.').

omega_variable(
    implementation_gap_reality,
    'To what extent do the actual practices of the Turkish state align with the ''expansive_reading'' of the Lausanne protections, particularly regarding property rights, theological education, and institutional self-administration?',
    'Independent human rights reports, monitoring by international bodies, and the outcomes of legal challenges in domestic and international courts regarding specific cases of property confiscation or restrictions on religious education.',
    'A significant and persistent gap between the expansive reading and state practice would indicate that the constraint, despite its legal framing, operates as a ''piton'' (theatrical maintenance) or even a ''snare'' (active subversion) in reality, rather than a functional ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_reality, empirical, 'Discrepancy between the legal text''s expansive interpretation and the state''s practical implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.05).
narrative_ontology:measurement(laus_tr_t1943, lausanne_minority_protections__expansive_reading, theater_ratio, 1943, 0.06).
narrative_ontology:measurement(laus_tr_t1963, lausanne_minority_protections__expansive_reading, theater_ratio, 1963, 0.07).
narrative_ontology:measurement(laus_tr_t1983, lausanne_minority_protections__expansive_reading, theater_ratio, 1983, 0.08).
narrative_ontology:measurement(laus_tr_t2003, lausanne_minority_protections__expansive_reading, theater_ratio, 2003, 0.09).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(laus_be_t1943, lausanne_minority_protections__expansive_reading, base_extractiveness, 1943, 0.12).
narrative_ontology:measurement(laus_be_t1963, lausanne_minority_protections__expansive_reading, base_extractiveness, 1963, 0.13).
narrative_ontology:measurement(laus_be_t1983, lausanne_minority_protections__expansive_reading, base_extractiveness, 1983, 0.14).
narrative_ontology:measurement(laus_be_t2003, lausanne_minority_protections__expansive_reading, base_extractiveness, 2003, 0.15).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.1).
narrative_ontology:measurement(laus_su_t1943, lausanne_minority_protections__expansive_reading, suppression_requirement, 1943, 0.12).
narrative_ontology:measurement(laus_su_t1963, lausanne_minority_protections__expansive_reading, suppression_requirement, 1963, 0.15).
narrative_ontology:measurement(laus_su_t1983, lausanne_minority_protections__expansive_reading, suppression_requirement, 1983, 0.17).
narrative_ontology:measurement(laus_su_t2003, lausanne_minority_protections__expansive_reading, suppression_requirement, 2003, 0.19).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
