% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra: Orthodox Literalist Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the orthodox literalist reading of
 *   Dharmasastra, which asserts that its prescriptions, particularly the
 *   varna/jati (caste) hierarchy, are eternal, divinely revealed truths
 *   requiring strict observance. This reading leads to an expansive victim
 *   set including Dalits, Shudras, and women, who face high extraction
 *   through enforced social and ritual hierarchy. Beneficiaries are
 *   concentrated in upper-caste communities and priestly classes. The
 *   constraint is claimed as a 'snare' due to its high extraction and
 *   suppression, despite its proponents framing it as a 'mountain' of eternal
 *   truth. This story is one reading of the 'dharmasastra_corpus' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.9).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.95).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra: Orthodox Literalist Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '2ad954cd-4de1-43f1-a9a2-6f2111e3bece').
narrative_ontology:cs_kernel_codification('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', fixed_text).
narrative_ontology:cs_authority_grounding('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', lineage).
narrative_ontology:cs_interpretation_layer_present('2ad954cd-4de1-43f1-a9a2-6f2111e3bece').
narrative_ontology:cs_reading_relation('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', foundational, dharmasastra_eternal_revealed_truth).
narrative_ontology:cs_axiom_status(dharmasastra_eternal_revealed_truth, holdable).
narrative_ontology:cs_axiom_grounding('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', dharmasastra_eternal_revealed_truth, theological).
narrative_ontology:cs_axiom('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', foundational, varna_jati_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_jati_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', varna_jati_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', primordial_vedic_order).
narrative_ontology:cs_drift_state('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', contemporary_secular_human_rights_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('2ad954cd-4de1-43f1-a9a2-6f2111e3bece', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priests).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_in_traditional_roles).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, divine_revelation_of_dharma).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, eternal_social_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Dharmasastra, particularly the varna/jati hierarchy, as eternal and divinely revealed. They benefit from the social and ritual authority this interpretation grants, and their identity is deeply fused with this role.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priests, agenda_setter,
    institutional, generational, identity_locked, regional).

% Benefit from the social status, ritual purity, and economic advantages conferred by the varna/jati hierarchy as interpreted by the orthodox literalist reading. Their adherence reinforces the system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_communities, beneficiary,
    organized, generational, constrained, regional).

% Are systematically excluded from ritual, education, and social mobility, facing severe discrimination and violence under this interpretation. Their identity is often forcibly defined by their caste status, making exit extremely difficult and dangerous.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, local).

% Are assigned to service roles and denied access to higher education and ritual participation, bearing significant social and economic costs. Their options for upward mobility are severely constrained by the enforced hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, trapped, local).

% Are often restricted from public roles, higher education, and independent economic activity, with their dharma defined primarily by domestic and reproductive duties. Their identity is often tied to family and community expectations, making deviation costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_in_traditional_roles, payer,
    moderate, biographical, identity_locked, local).

% Advocate for contextual or ethical interpretations of Dharmasastra, challenging the literalist view. They are often marginalized or condemned by orthodox institutions, facing social and academic ostracism.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% Document and challenge the human rights violations stemming from caste discrimination and gender inequality enforced by such interpretations. They operate from an external legal and ethical framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive framework for social order, ritual practice, and moral conduct, ensuring stability and continuity across generations by assigning roles and duties within a hierarchical structure.
% TRANSFER_FUNCTION: Transfers social status, ritual authority, economic advantage, and access to knowledge from lower castes and women to upper castes and men, in exchange for perceived social order and spiritual merit.
% ABSENT_VOICES: Abolitionist voices, particularly from Dalit and feminist movements, are actively suppressed or dismissed as illegitimate, denying the fundamental injustice of the system. Their calls for complete dismantling of the caste system are not part of the orthodox discourse.
% DISAPPEARANCE_RATIONALE: If this literalist interpretation and its enforcement vanished, the social, ritual, and economic structures of many traditional communities would undergo profound and rapid reorganization. Power dynamics would shift, access to resources would be contested, and new forms of social organization would emerge, leading to significant upheaval and potential liberation for previously marginalized groups.
% FOUNDING_PROBLEM: To establish a divinely ordained, stable, and hierarchical social order that ensures cosmic harmony (dharma) and guides individuals in their duties across different life stages and social positions.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious leaders and traditional communities attest that the problem of maintaining dharma and social order in a chaotic world is still live. Reformist scholars and human rights advocates contest this, arguing that the 'problem' is a justification for maintaining an extractive hierarchy, not a genuine societal need, and that the original intent has been corrupted or was inherently flawed.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the system systematically denies basic rights and opportunities to large segments of the population, concentrating benefits at the top. Suppression is also very high (0.95) due to the deep institutionalization of caste, social ostracism, and historical violence against those who defy the hierarchy. Accessibility collapse is high (0.8) as alternatives are severely limited by social structures. Resistance is also high (0.7) reflecting ongoing struggles by marginalized groups, but the system's inertia and enforcement capacity are formidable. Theater ratio is low (0.1) because the system is actively functional in its extractive and suppressive aspects, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this is a divinely ordained, stable social order (a 'mountain' or 'rope'). From the perspective of the victims, it is a deeply coercive and extractive system (a 'snare'). The engine's classification will reflect the latter due to the high extractiveness and suppression metrics, highlighting the divergence from the claimed 'eternal truth' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priests and upper-caste communities are clear beneficiaries, deriving authority, status, and material advantage (low directionality). Dalits, Shudras, and women in traditional roles are primary targets, bearing the brunt of exclusion and discrimination (high directionality). Reformist scholars are excluded, facing marginalization for challenging the literalist interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_revelation_vs_social_construct,
    'Is the Dharmasastra corpus truly eternal, divinely revealed truth, or a historical social construct that evolved to serve specific power interests?',
    'Historical-critical textual analysis combined with sociological and anthropological studies of caste formation and evolution, independent of religious authority claims.',
    'If a social construct, the ''mountain'' claim of eternal truth collapses, reclassifying the constraint as a human-made ''snare'' or ''tangled_rope'' with identifiable beneficiaries and victims. If genuinely divine, its classification as a ''mountain'' would be more robust, though its ethical implications would remain contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_revelation_vs_social_construct, conceptual, 'Ambiguity between divine revelation and social construction of Dharmasastra''s authority.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression experienced by Dalits, Shudras, and women structural (external barriers, violence) versus internalized (self-concept, fatalism, identity fusion)?',
    'Post-exit suppression trajectory: if suppression persists after external barriers are removed (e.g., through migration to urban areas or legal protections), reclassify as partially internalized. Longitudinal studies of identity formation and resilience in marginalized communities.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them after exit, making liberation more complex than mere removal of external barriers. This would also highlight the ''identity_locked'' exit option for many victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in caste and gender hierarchy.').

omega_variable(
    mandate_obsolescence_vs_eternal_relevance,
    'Is the founding problem of establishing a stable, harmonious social order (dharma) still genuinely live, or has the original mandate atrophied, leaving only an extractive structure?',
    'Independent sociological assessment of contemporary social needs and challenges, comparing them against the problems Dharmasastra was originally designed to solve, and evaluating whether its solutions remain relevant or actively harmful.',
    'If the mandate is found to be ''dead'' or ''contested'' by external corroboration, the constraint shifts towards a ''piton'' or ''snare'' classification, indicating that its persistence is due to inertia or rent-seeking rather than genuine coordination. This would challenge the ''live'' status claimed by orthodox proponents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_eternal_relevance, empirical, 'Whether the founding mandate of Dharmasastra remains relevant or has atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.1).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.1).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__orthodox_literalist, theater_ratio, 60, 0.1).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.1).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__orthodox_literalist, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.87).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 60, 0.89).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.9).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.93).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 60, 0.94).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.95).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dharmasastra_corpus' kernel. This orthodox_literalist reading directly influences and is in tension with the reformist_contextual and abolitionist_rejection readings, as its claims of eternal truth and literal observance are the primary target of their critiques.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
