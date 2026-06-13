% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship (Civic-Pastoral Reading)
 *   domain: religious/political
 *
 * SUMMARY:
 *   The civic-pastoral reading of marriage as a sacrament reframes
 *   indissolubility from an ontological absolute to an aspirational ideal
 *   subject to compassionate discernment in individual cases. This reading
 *   emerged post-Vatican II as pastoral theology confronted the reality of
 *   marriage failure and the gospel imperative toward mercy. Under this
 *   framing, diocesan authorities gain discretionary power to grant
 *   annulments and sacramental dispensations on a case-by-case basis,
 *   redefining what marriage permanence means in pastoral practice. The
 *   constraint operates as tangled_rope: genuine coordination function
 *   (reconciling mercy with doctrine), but asymmetric extraction
 *   (institutional authority expands while doctrinal clarity erodes;
 *   divorced-remarried Catholics gain sacramental access while traditional
 *   laity experience erosion of normative coherence and first spouses
 *   experience delegitimization of their bonds). This reading directly
 *   contests the hierarchical_indissolubility_reading, which treats
 *   indissolubility as constitutive rather than aspirational and prioritizes
 *   doctrinal clarity over case-by-case mercy.
 *
 * KEY AGENTS:
 *   - diocesan_pastoral_authority: institutional agenda-setter (administrative discretion); gains expanded jurisdictional power
 *   - traditional_catholic_laity: powerless payer (identity-locked); experiences doctrinal relativization as loss of moral coherence
 *   - divorced_remarried_catholics: organized beneficiary (constrained exit); gains sacramental access and pastoral legitimation
 *   - first_spouses: moderate-power payer (constrained exit); experiences doctrinal shift as delegitimization of marriage covenant
 *   - hierarchical_authority_defenders: excluded institutional observer; argues for doctrinal purity over discretionary mercy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.58).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.52).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/political").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'ce756502-89ee-4789-b8fd-534f9376f375').
narrative_ontology:cs_kernel_codification('ce756502-89ee-4789-b8fd-534f9376f375', fixed_text).
narrative_ontology:cs_authority_grounding('ce756502-89ee-4789-b8fd-534f9376f375', lineage).
narrative_ontology:cs_interpretation_layer_present('ce756502-89ee-4789-b8fd-534f9376f375').
narrative_ontology:cs_reading_relation('ce756502-89ee-4789-b8fd-534f9376f375', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('ce756502-89ee-4789-b8fd-534f9376f375', foundational, mercy_precedent_over_legalism).
narrative_ontology:cs_axiom_status(mercy_precedent_over_legalism, holdable).
narrative_ontology:cs_axiom_grounding('ce756502-89ee-4789-b8fd-534f9376f375', mercy_precedent_over_legalism, deontological).
narrative_ontology:cs_axiom('ce756502-89ee-4789-b8fd-534f9376f375', foundational, pastoral_discretion_authorititative).
narrative_ontology:cs_axiom_status(pastoral_discretion_authorititative, holdable).
narrative_ontology:cs_axiom_grounding('ce756502-89ee-4789-b8fd-534f9376f375', pastoral_discretion_authorititative, conventional).
narrative_ontology:cs_reference_frame('ce756502-89ee-4789-b8fd-534f9376f375', pastoral_mercy_priority).
narrative_ontology:cs_drift_state('ce756502-89ee-4789-b8fd-534f9376f375', contemporary_post_francis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce756502-89ee-4789-b8fd-534f9376f375', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_pastoral_authority).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, first_spouses).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to a plateau of 0.58–0.61 (t=16–50) reflecting the growing extraction of doctrinal clarity and marriage-permanence certainty in exchange for institutional discretion. The extraction does not continue rising after t=32, indicating a stabilization point where the pastoral reading has become institutionalized practice and new equilibrium is reached. Theater_ratio peaks at t=32 (0.49) and plateaus, reflecting the increasing proportion of activity devoted to processing and adjudicating cases rather than transmitting unified doctrine. Suppression_requirement spikes from t=0–t=32 (0.58 to 0.73), then drops sharply to 0.52 (t=40–50), marking a phase transition: early pastoral adoption required active suppression of hierarchical objections and traditional-laity resistance, but once the pastoral reading became institutionalized (post-t=32), the suppression requirement eased as the new normative baseline took hold. The coercion grid shows leveled dynamics: individual suppression remains relatively stable but organizational suppression peaks (0.71 at t=0, declining to 0.62 by t=50), indicating the constraint operates primarily through institutional gatekeeping rather than individual coercion. Class-level resistance remains high (0.71–0.75), reflecting organized traditional Catholic opposition and first-spouse advocacy, while structural resistance is moderate-to-high (0.68–0.71), consistent with Vatican doctrinal skepticism and conservative bishop networks.
 *
 * PERSPECTIVAL GAP:
 *   The diocesan pastoral authority experiences the constraint as genuine coordination: resolving the pastoral crisis through mercy and discretion. The traditional_catholic_laity and first_spouses experience the same constraint as extractive: their coherence-good (doctrinal clarity, marriage permanence) is traded for institutional discretion and the beneficiary-class's (divorced-remarried) sacramental inclusion. The hierarchy (excluded) experiences it as doctrinal erosion and authority relativization. Directionality diverges sharply: pastoral authority sits near d=0.15 (beneficiary), traditional laity near d=0.85 (target), divorced-remarried near d=0.35 (beneficiary with constrained exit), first spouses near d=0.75 (target). This structural asymmetry is the core seat-divergence: the same constraint is experienced as mercy from one seat and as institutional extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Diocesan pastoral authority (institutional, arbitrage exit) benefits from expanded discretionary power and institutional relevance; no override needed, d derives naturally from beneficiary role. Traditional_catholic_laity (powerless, identity_locked) lose the doctrinal certainty that constitutes their religious identity and cannot exit without abandoning Catholicism; their identity_lock combined with victim status yields high d (~0.82). Divorced_remarried_catholics (organized, constrained exit) benefit from sacramental access but remain dependent on institutional approval; directionality sits lower than victims (~0.38) because they gain something, but their exit is constrained by the process requirement. First_spouses (moderate power, constrained exit) face doctrinal delegitimization of their marriage; they are not fully powerless (moderate power) but their exit is constrained by family, community, and identity; d ~0.73. No overrides are necessary; the derivation chain produces accurate directionalities from the beneficiary/victim + exit declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The pastoral reading avoids the mandatrophy trap because the founding problem (reconciling mercy with doctrine) remains genuinely live. The pastoral reading does not claim indissolubility has been solved—it claims it should be administered with compassion. However, the constraint risks mandatrophy-adjacent dynamics: if the founding problem shifted from 'how to show mercy to divorced Catholics' to 'how to maintain institutional discretion,' the constraint would become extractive theater masked as pastoral care. The measurement series show suppression dropping sharply after t=32, suggesting institutional normalization and reduced active enforcement, which could indicate the founding problem is being resolved (normalization) or the constraint is settling into inertial operation (piton-adjacent). The rising theater_ratio to t=32 followed by plateau suggests the constraint may be approaching a point where administrative processing dominates over genuine pastoral discernment—the classic Goodhart drift where the measure (case-by-case mercy) becomes the goal and doctrinal rationale fades. An omega documents this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pastoralism_vs_doctrinal_erosion,
    'Is the pastoral reading a legitimate development of doctrine that extends mercy while preserving doctrinal substance, or does it functionally erode the doctrine''s normative force by making indissolubility contingent on individual discernment?',
    'Track the long-term evolution of institutional practice: if dioceses converge toward consistent standards for mercy (suggesting doctrinal content is preserved through discretion), the pastoral reading sustains doctrine. If practice diverges widely and becomes purely administrative (annulment granted as procedural formality), the reading has eroded doctrine.',
    'If resolution favors preservation, the constraint is genuine tangled_rope (coordination + asymmetric enforcement). If resolution favors erosion, the constraint becomes snare (extraction dressed as mercy). The reading itself would need to be reclassified, and traditional_catholic_laity would shift from payer to victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoralism_vs_doctrinal_erosion, empirical, 'Whether pastoral discretion preserves or erodes doctrinal integrity.').

omega_variable(
    institutional_discretion_capture,
    'Does the expansion of diocesan discretion distribute power toward genuine pastoral care, or does it concentrate power in institutional hands by making sacramental access contingent on pleading a case to episcopal authorities?',
    'Comparative analysis of annulment grant rates across dioceses and time periods, and interviews with applicants about experience of discretion (is it felt as mercy or as arbitrary institutional gatekeeping?).',
    'If discretion distributes power downward to laity and communities, the constraint is tangled_rope with beneficiary expansion. If discretion concentrates institutional power, the constraint is snare and diocesan_pastoral_authority is the sole beneficiary, with divorced-remarried Catholics as managed subjects rather than genuine beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_discretion_capture, empirical, 'Whether pastoral discretion empowers laity or concentrates institutional authority.').

omega_variable(
    identity_locked_internalization,
    'For traditional_catholic_laity, is the suppression of their doctrinal normality experienced as external institutional pressure (structural) or as internal erosion of their identity framework (internalized)?',
    'Post-exit analysis: if Catholics who leave the tradition report that doctrinal confusion persisted as a reason for exit and remains unresolved after departure, suppression is internalized. If they report relief from institutional pressure upon exit, suppression was structural.',
    'If internalized, the effective suppression is higher than the 0.52–0.62 scalar suggests—the target carries the suppression even after the constraint is removed. The constraint''s extraction is masked as institutional inconsistency rather than visible coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_internalization, empirical, 'Whether doctrinal relativization is experienced as external pressure or internalized identity dissolution.').

omega_variable(
    goodhart_drift_risk,
    'As the pastoral reading becomes institutionalized and suppression_requirement drops (t=32 onward), is the constraint transitioning from genuine pastoral discernment to performative processing—where case-by-case mercy becomes the administrative goal rather than the doctrinal means?',
    'Content analysis of tribunal decisions, pastoral guidelines, and training documents over time: if mercy-language becomes decoupled from doctrinal reasoning and reduced to formalized narrative templates, Goodhart drift is occurring.',
    'Drift toward piton classification: the constraint persists through administrative inertia and institutional self-justification rather than genuine coordination or extraction, becoming theater. If this occurs, mandatrophy would be incipient—the founding problem (reconciling mercy with doctrine) would be supplanted by the problem of maintaining institutional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_drift_risk, empirical, 'Risk of pastoral discretion devolving into administrative theater.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the pastoral reading truly a distinct reading of the indissolubility kernel, or does it represent a shift in which party controls the kernel''s interpretation (from Vatican to diocesan level)?',
    'Analyze whether the pastoral reading''s distinguishing feature is a new normative claim (mercy over legalism) or a shift in institutional authority (from universal hierarchical doctrine to local pastoral discretion). If the former, it is a genuine distinct reading. If the latter, it may be a single reading undergoing authority-migration rather than kernel-contestation.',
    'If authority-migration: the constraint is better understood as institutional reorganization within a single reading, making the sibling-reading classification provisional. The hierarchical and pastoral readings might not be genuinely distinct constraint families but competing administrations of one constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the pastoral reading is a distinct kernel reading or an authority-migration within one reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__civic_pastoral_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__civic_pastoral_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__civic_pastoral_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(marr_tr_t32, marriage_sacrament__civic_pastoral_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(marr_be_t32, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(marr_su_t32, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel has two readings: civic_pastoral_reading (this story) and hierarchical_indissolubility_reading. The readings differ in their axioms (mercy vs. doctrinal clarity as primary), their authority grounding (pastoral discretion vs. hierarchical doctrine), and their ε values (civic-pastoral is moderately extractive at 0.58; hierarchical is lower-extraction). They coexist as competing institutional framings within the same doctrinal tradition, neither logically foreclosing the other but each creating pressure on the other's normative legitimacy. The civic-pastoral reading influences the hierarchical reading by eroding its claim to universal binding force; the hierarchical reading influences the civic-pastoral by maintaining doctrinal critique and resistance from Vatican offices and conservative bishops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
