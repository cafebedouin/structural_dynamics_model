% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections: Restrictive Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'restrictive_reading' of the Lausanne
 *   Treaty's minority protections, as interpreted and enforced by the Turkish
 *   state. It limits protections strictly to individual worship rights,
 *   explicitly excluding institutional autonomy, property ownership, and
 *   theological education from treaty guarantees, subjecting these instead to
 *   general domestic law. This reading is a snare, characterized by high
 *   extraction and suppression, as it systematically disempowers non-Muslim
 *   minority institutions and communities while consolidating state control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.85).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.9).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections: Restrictive Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '1382fc11-e34b-41d8-8f11-bb8187797f71').
narrative_ontology:cs_kernel_codification('1382fc11-e34b-41d8-8f11-bb8187797f71', fixed_text).
narrative_ontology:cs_authority_grounding('1382fc11-e34b-41d8-8f11-bb8187797f71', extraction).
narrative_ontology:cs_interpretation_layer_present('1382fc11-e34b-41d8-8f11-bb8187797f71').
narrative_ontology:cs_reading_relation('1382fc11-e34b-41d8-8f11-bb8187797f71', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('1382fc11-e34b-41d8-8f11-bb8187797f71', lausanne_minority_protections__guarantor_reading, forecloses).
narrative_ontology:cs_axiom('1382fc11-e34b-41d8-8f11-bb8187797f71', foundational, domestic_sovereignty_over_religious_institutions).
narrative_ontology:cs_axiom_status(domestic_sovereignty_over_religious_institutions, holdable).
narrative_ontology:cs_axiom_grounding('1382fc11-e34b-41d8-8f11-bb8187797f71', domestic_sovereignty_over_religious_institutions, conventional).
narrative_ontology:cs_axiom('1382fc11-e34b-41d8-8f11-bb8187797f71', foundational, minority_rights_limited_to_individual_worship).
narrative_ontology:cs_axiom_status(minority_rights_limited_to_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('1382fc11-e34b-41d8-8f11-bb8187797f71', minority_rights_limited_to_individual_worship, conventional).
narrative_ontology:cs_reference_frame('1382fc11-e34b-41d8-8f11-bb8187797f71', domestic_legal_sovereignty_framework).
narrative_ontology:cs_drift_state('1382fc11-e34b-41d8-8f11-bb8187797f71', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1382fc11-e34b-41d8-8f11-bb8187797f71', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Treaty of Lausanne to limit protections to individual worship, asserting domestic sovereignty over the institutional aspects of non-Muslim minorities. Benefits from consolidating control over minority institutional capacity, property, and education.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Are denied legal personality, face property confiscation, and are prevented from operating theological schools or self-administering their affairs. They bear the direct costs of this restrictive interpretation, with no viable exit within the domestic legal framework.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions, payer,
    powerless, generational, trapped, national).

% Experience the erosion of their cultural and religious heritage due to the weakening of their institutions. While individual worship is permitted, the lack of institutional support makes cultural transmission and community cohesion difficult. Their identity is deeply tied to these institutions, making 'exit' a form of cultural dissolution.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities, payer,
    powerless, generational, identity_locked, national).

% Monitor the treatment of minorities and issue reports criticizing the restrictive interpretation, advocating for broader protections consistent with international human rights law. Their influence is primarily diplomatic and reputational.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% As signatories to the Treaty of Lausanne, they have a historical interest in its implementation. While they may express concerns, their capacity or willingness to enforce an expansive interpretation against domestic sovereignty claims is often limited to diplomatic pressure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states_of_lausanne, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the Turkish state, this reading coordinates the assertion of national sovereignty over all domestic affairs, including the institutional aspects of religious minorities, thereby standardizing legal application.
% TRANSFER_FUNCTION: Transfers control over institutional autonomy, property ownership, and theological education from non-Muslim minority communities and their institutions to the Turkish state apparatus.
% ABSENT_VOICES: Non-Muslim minority religious leaders, legal scholars, and international legal experts who advocate for a more expansive interpretation of Lausanne, emphasizing institutional rights and international oversight, are effectively marginalized or excluded from domestic legal discourse.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished overnight, non-Muslim minority institutions would immediately seek to reclaim legal personality, property, and the right to self-administer and educate their clergy. This would lead to significant legal challenges, property disputes, and a reorganization of religious governance within Turkey.
% FOUNDING_PROBLEM: To define the status and rights of non-Muslim minorities in the newly formed Republic of Turkey following the collapse of the Ottoman Empire, balancing the protection of minority populations with the assertion of national sovereignty and secular principles.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state maintains that its interpretation is consistent with the original intent of the treaty and national sovereignty. Non-Muslim minority communities and international human rights bodies contest this, arguing that the original intent included broader institutional protections and that the current interpretation is a tool of control, citing historical practice and international legal norms.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the systematic denial of institutional rights, leading to property confiscation, educational closures, and legal personality issues for minority foundations. Suppression (0.90) is severe, as the state actively enforces this interpretation through legal and administrative means, with no effective domestic recourse for victims. The low theater ratio (0.15) indicates that the enforcement is functional and not merely performative; the state genuinely seeks to maintain this control. Resistance is moderate (0.40) due to the power imbalance, but international bodies and minority communities continue to challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this reading upholds national sovereignty and legal consistency. From the perspective of minority institutions and communities, it is a mechanism of systematic extraction and cultural suppression. The engine's classification as a snare reflects the latter, highlighting the coercive and asymmetric nature of the constraint despite the state's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the clear beneficiary and agenda-setter, gaining control and asserting sovereignty. Non-Muslim minority institutions and communities are the primary targets and payers, bearing the full cost of denied rights and institutional erosion. International human rights bodies and guarantor states act as observers, providing analytical and diplomatic pressure, but with limited direct power to alter the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_scope_ambiguity,
    'Does the Treaty of Lausanne''s text and historical context genuinely limit minority protections to individual worship, or does it implicitly or explicitly extend to institutional autonomy and property rights?',
    'Comprehensive historical-legal analysis of the treaty''s drafting, contemporary diplomatic correspondence, and early implementation practices, alongside comparative international law on minority rights.',
    'If the treaty is found to implicitly or explicitly cover institutional rights, the restrictive reading''s legitimacy would collapse, reclassifying it from a snare to a contested claim with significantly lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_scope_ambiguity, empirical, 'Ambiguity regarding the scope of minority protections in the Treaty of Lausanne.').

omega_variable(
    domestic_vs_international_enforcement,
    'Is the interpretation and enforcement of the Lausanne Treaty solely a matter of domestic Turkish law, or do guarantor states and international human rights mechanisms have a legitimate and enforceable role?',
    'International legal arbitration or a ruling by a competent international court (e.g., European Court of Human Rights) on the enforceability of the treaty''s provisions by external actors.',
    'If international enforcement is deemed legitimate, the suppression metric would be re-evaluated downward due to increased external pressure, potentially shifting the classification towards a tangled rope or even a rope if enforcement becomes effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_vs_international_enforcement, conceptual, 'Contest over the locus of authority for interpreting and enforcing the Lausanne Treaty.').

omega_variable(
    founding_intent_vs_contemporary_practice,
    'To what extent does the current restrictive interpretation align with the founding intent of the Treaty of Lausanne''s signatories, versus reflecting later political developments and nationalistic policies?',
    'Archival research into the diplomatic negotiations and internal government documents of the signatory states from the 1920s, compared with post-1950s policy shifts.',
    'If a significant divergence is found, it would strengthen the argument that the constraint is a constructed snare rather than a faithful interpretation of an international agreement, potentially increasing the perceived extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_vs_contemporary_practice, empirical, 'Divergence between original treaty intent and current state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1980, lausanne_minority_protections__restrictive_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(laus_tr_t1988, lausanne_minority_protections__restrictive_reading, theater_ratio, 1988, 0.17).
narrative_ontology:measurement(laus_tr_t1996, lausanne_minority_protections__restrictive_reading, theater_ratio, 1996, 0.16).
narrative_ontology:measurement(laus_tr_t2004, lausanne_minority_protections__restrictive_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(laus_tr_t2012, lausanne_minority_protections__restrictive_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(laus_tr_t2020, lausanne_minority_protections__restrictive_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(laus_be_t1980, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(laus_be_t1988, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1988, 0.81).
narrative_ontology:measurement(laus_be_t1996, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1996, 0.83).
narrative_ontology:measurement(laus_be_t2004, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2004, 0.84).
narrative_ontology:measurement(laus_be_t2012, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2012, 0.85).
narrative_ontology:measurement(laus_be_t2020, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1980, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(laus_su_t1988, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1988, 0.87).
narrative_ontology:measurement(laus_su_t1996, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1996, 0.88).
narrative_ontology:measurement(laus_su_t2004, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2004, 0.89).
narrative_ontology:measurement(laus_su_t2012, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2012, 0.9).
narrative_ontology:measurement(laus_su_t2020, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
