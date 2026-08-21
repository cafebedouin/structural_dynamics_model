% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II: Composite Overdetermination of Doctrinal Authority
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story analyzes Vatican II not as a singular, coherent
 *   event, but as an overdetermined composite of distinct doctrinal shifts
 *   with incompatible theological rationales. This reading highlights the
 *   structural ambiguity inherent in the Council's documents, which cannot be
 *   fully resolved into either a 'continuity' or 'rupture' narrative. This
 *   ambiguity itself acts as a constraint, extracting costs from those
 *   seeking univocal interpretation and benefiting those who analyze its
 *   complexity. The institutional Magisterium's attempts to enforce a
 *   singular interpretation are met with the inherent resistance of the
 *   documents' own internal contradictions.
 *
 * KEY AGENTS:
 *   - institutional_magisterium: Primary target/agenda_setter (institutional/identity_locked) — bears the cost of managing contradictions, attempts to enforce univocal interpretation.
 *   - scholars_of_complexity: Primary beneficiary (analytical/analytical) — benefits from the complexity as a field of study.
 *   - theological_historians: Secondary beneficiary (analytical/analytical) — benefits from the historical inquiry into the Council's formation.
 *   - faithful_seeking_clarity: Primary payer (powerless/identity_locked) — bears the cost of theological confusion and internal conflict.
 *   - continuity_theologians: Payer (organized/constrained) — bears intellectual cost of reconciling contradictions with tradition.
 *   - rupture_theologians: Payer (organized/constrained) — bears intellectual cost of justifying breaks with tradition.
 *   - absent_voices: Excluded (powerless/identity_locked) — those who disengaged due to unresolved ambiguities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II: Composite Overdetermination of Doctrinal Authority").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '593dbda6-1be1-40d2-bf3d-de90808ce925').
narrative_ontology:cs_kernel_codification('593dbda6-1be1-40d2-bf3d-de90808ce925', formalized).
narrative_ontology:cs_authority_grounding('593dbda6-1be1-40d2-bf3d-de90808ce925', lineage).
narrative_ontology:cs_interpretation_layer_present('593dbda6-1be1-40d2-bf3d-de90808ce925').
narrative_ontology:cs_reading_relation('593dbda6-1be1-40d2-bf3d-de90808ce925', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('593dbda6-1be1-40d2-bf3d-de90808ce925', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('593dbda6-1be1-40d2-bf3d-de90808ce925', foundational, vatican_ii_contains_genuine_theological_contradictions).
narrative_ontology:cs_axiom_status(vatican_ii_contains_genuine_theological_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('593dbda6-1be1-40d2-bf3d-de90808ce925', vatican_ii_contains_genuine_theological_contradictions, deontological).
narrative_ontology:cs_axiom('593dbda6-1be1-40d2-bf3d-de90808ce925', foundational, ambiguity_is_structural_outcome_of_factional_compromise).
narrative_ontology:cs_axiom_status(ambiguity_is_structural_outcome_of_factional_compromise, holdable).
narrative_ontology:cs_axiom_grounding('593dbda6-1be1-40d2-bf3d-de90808ce925', ambiguity_is_structural_outcome_of_factional_compromise, conventional).
narrative_ontology:cs_reference_frame('593dbda6-1be1-40d2-bf3d-de90808ce925', univocal_doctrinal_coherence).
narrative_ontology:cs_drift_state('593dbda6-1be1-40d2-bf3d-de90808ce925', post_conciliar_theological_conflict, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('593dbda6-1be1-40d2-bf3d-de90808ce925', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, scholars_of_complexity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, theological_historians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_clarity).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, continuity_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, rupture_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, tasked with interpreting Vatican II. It attempts to present a univocal interpretation, often emphasizing 'continuity', but bears the cost of managing the inherent contradictions and the resulting theological conflicts. Its identity is fused with maintaining doctrinal coherence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Academic theologians and historians who specialize in analyzing the complex, often contradictory, nature of Vatican II's documents and their reception. They benefit from the ongoing ambiguity as a rich field of study and publication.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, scholars_of_complexity, beneficiary,
    analytical, biographical, analytical, global).

% Scholars who document the historical processes and compromises that shaped Vatican II, providing evidence for its overdetermined nature. They benefit from the complexity as a subject of historical inquiry.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, theological_historians, beneficiary,
    analytical, generational, analytical, global).

% Individual Catholics who seek clear, consistent doctrinal guidance from the Church. They bear the cost of theological confusion, internal conflict, and the perceived incoherence of post-conciliar teaching, often feeling alienated or disoriented. Their identity is tied to their faith in the Church's teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_clarity, payer,
    powerless, biographical, identity_locked, regional).

% Theologians who dedicate their careers to interpreting Vatican II in strict continuity with prior tradition. The overdetermined nature of the documents forces them into complex hermeneutical efforts to reconcile apparent contradictions, bearing a significant intellectual cost.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, continuity_theologians, payer,
    organized, biographical, constrained, global).

% Theologians who interpret Vatican II as a substantive break with prior tradition, often advocating for further reforms. The overdetermined nature of the documents means they must constantly contend with 'continuity' arguments and internal inconsistencies, bearing an intellectual cost.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, rupture_theologians, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, scholars_of_complexity).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The documents of Vatican II, despite their internal tensions, serve to coordinate the ongoing theological discourse and the self-understanding of the Catholic Church in the modern era.
% TRANSFER_FUNCTION: Transfers the burden of resolving theological contradictions and ambiguities from the Council itself to subsequent interpretive bodies (Magisterium, theologians) and individual faithful, who must navigate the resulting tensions.
% ABSENT_VOICES: Those who have left the Catholic Church due to irreconcilable theological conflicts or perceived institutional dishonesty regarding the Council's legacy; they would articulate the impossibility of coherence.
% DISAPPEARANCE_RATIONALE: If the overdetermined, contradictory nature of Vatican II were suddenly resolved into a single, coherent interpretation (either continuity or rupture), the entire theological, institutional, and spiritual landscape of the Catholic Church would fundamentally shift, leading to either widespread acceptance or a major schism.
% FOUNDING_PROBLEM: To update the Church's engagement with the modern world (aggiornamento) while preserving and articulating its unchanging doctrinal integrity (ressourcement), leading to compromises that embedded incompatible theological rationales within the documents.
% FOUNDING_PROBLEM_CORROBORATION: Independent theological historians and scholars of ecclesiology, outside the direct institutional beneficiaries, corroborate the historical process of factional compromise during the Council and the resulting ambiguities that persist in post-conciliar theology.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the ongoing cost imposed by the unresolved ambiguities on institutional authority and the faithful, while also generating 'rents' for scholars of complexity. Suppression (0.70) is high due to the institutional Magisterium's active efforts to enforce a singular, often 'continuity'-based, interpretation, thereby suppressing readings that emphasize contradiction. The theater ratio (0.50) indicates that a significant portion of institutional activity is performative, aimed at maintaining an appearance of doctrinal unity despite internal tensions. The measurements show an initial rise in extractiveness and suppression as the implications of the ambiguities became clearer post-Council, with a slight leveling off as positions hardened.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional Magisterium, the constraint is a 'Rope' or 'Scaffold' for maintaining Church unity and adapting to modernity, with any 'extraction' being a necessary cost of coordination. From the perspective of the faithful and many theologians, it operates as a 'Tangled Rope' or 'Snare', extracting clarity and coherence while coordinating an ambiguous identity. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional Magisterium is the agenda-setter, attempting to control the narrative, but is also a victim of the overdetermination as it struggles to maintain coherence. Scholars of complexity and theological historians are beneficiaries, as the ambiguity provides their field of study. The faithful and both continuity and rupture theologians are payers, bearing the intellectual and spiritual costs of navigating the contradictions. The 'identity_locked' exit option for the Magisterium and faithful reflects their deep structural ties to the Church, making exit unthinkable despite the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the structural ambiguity as either a purely benign coordination (Rope) or a simple, intentional extraction (Snare). Instead, it identifies a 'Tangled Rope' where the coordination of a post-conciliar Catholic identity is inextricably linked with the extraction of clarity and coherence, sustained by active (though often subtle) enforcement of interpretive frameworks. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's function, however problematic, is still central to the Church's operation, preventing a 'Piton' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolvability_of_ambiguity,
    'Is the structural ambiguity of Vatican II truly irresolvable, or could a future theological synthesis or magisterial act provide a coherent framework?',
    'Future ecumenical council, papal declaration, or a widely accepted theological paradigm shift that successfully integrates the apparent contradictions without denying their historical reality.',
    'If resolvable, the constraint''s extractiveness and suppression would decrease, potentially shifting its classification towards a ''Rope'' or ''Scaffold'' as a transitional phase. If confirmed as irresolvable, the ''Tangled Rope'' classification would be reinforced, highlighting the permanent cost of the compromise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resolvability_of_ambiguity, conceptual, 'Whether the inherent contradictions of Vatican II are fundamentally unresolvable or open to future synthesis.').

omega_variable(
    institutional_acknowledgement_of_contradiction,
    'To what extent does the institutional Magisterium internally acknowledge the genuine theological contradictions within Vatican II, versus maintaining a public facade of univocal coherence?',
    'Access to internal curial documents, private theological consultations, or explicit public statements from high-ranking officials acknowledging the overdetermined nature of the Council''s output.',
    'If acknowledged, the ''theater_ratio'' would decrease, and the ''suppression'' of alternative readings might lessen, potentially reducing the constraint''s overall extractiveness. If denial is confirmed, the ''Tangled Rope'' classification is strengthened by the active suppression of internal dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_acknowledgement_of_contradiction, empirical, 'The degree of institutional recognition of Vatican II''s internal contradictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.48).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.52).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.5).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_liturgical_reforms).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, catholic_moral_theology_development).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_authority' kernel. This 'composite overdetermination' reading emphasizes the inherent contradictions and ambiguities, contrasting with the 'continuity' and 'rupture' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
