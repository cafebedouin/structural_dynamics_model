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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Treaty Minority Protections (Restrictive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the restrictive reading of the Lausanne
 *   Treaty's minority protections, specifically that they extend only to
 *   individual worship rights, while institutional autonomy, property
 *   ownership, and theological education are considered domestic matters
 *   subject to general Turkish law. This reading effectively transforms what
 *   was intended as a coordination mechanism for minority rights into a snare
 *   for minority institutions, enabling the state to consolidate control and
 *   assets. The high extractiveness and suppression reflect the systematic
 *   erosion of minority institutional capacity under this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.88).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.92).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Treaty Minority Protections (Restrictive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'd64d2a73-243a-43b8-b7d0-f6421e8a453f').
narrative_ontology:cs_kernel_codification('d64d2a73-243a-43b8-b7d0-f6421e8a453f', fixed_text).
narrative_ontology:cs_authority_grounding('d64d2a73-243a-43b8-b7d0-f6421e8a453f', extraction).
narrative_ontology:cs_interpretation_layer_present('d64d2a73-243a-43b8-b7d0-f6421e8a453f').
narrative_ontology:cs_reading_relation('d64d2a73-243a-43b8-b7d0-f6421e8a453f', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('d64d2a73-243a-43b8-b7d0-f6421e8a453f', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('d64d2a73-243a-43b8-b7d0-f6421e8a453f', foundational, national_sovereignty_over_institutions).
narrative_ontology:cs_axiom_status(national_sovereignty_over_institutions, holdable).
narrative_ontology:cs_axiom_grounding('d64d2a73-243a-43b8-b7d0-f6421e8a453f', national_sovereignty_over_institutions, conventional).
narrative_ontology:cs_axiom('d64d2a73-243a-43b8-b7d0-f6421e8a453f', foundational, individual_worship_only_protection).
narrative_ontology:cs_axiom_status(individual_worship_only_protection, holdable).
narrative_ontology:cs_axiom_grounding('d64d2a73-243a-43b8-b7d0-f6421e8a453f', individual_worship_only_protection, conventional).
narrative_ontology:cs_reference_frame('d64d2a73-243a-43b8-b7d0-f6421e8a453f', post_ottoman_national_sovereignty).
narrative_ontology:cs_drift_state('d64d2a73-243a-43b8-b7d0-f6421e8a453f', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d64d2a73-243a-43b8-b7d0-f6421e8a453f', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Lausanne Treaty narrowly, asserting that institutional autonomy, property ownership, and theological education for non-Muslim minorities fall under general domestic law, not treaty protections. Benefits from consolidating control over minority institutional capacity and assets.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Subject to property confiscation, denial of legal personality, and restrictions on their ability to manage assets and maintain religious sites. Their existence is often precarious, dependent on state discretion rather than treaty rights.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_foundations, payer,
    powerless, generational, trapped, local).

% Experience the erosion of their cultural and religious heritage due to the inability to train clergy, maintain institutions, and secure property. Their identity is deeply tied to these institutions, making 'exit' a form of cultural dissolution.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_communities, payer,
    powerless, generational, identity_locked, local).

% Struggle to provide religious services and maintain community cohesion without institutional support or the ability to train successors through theological education. Their authority is undermined by the state's restrictive interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_leaders, payer,
    moderate, biographical, constrained, local).

% Signatories to the Lausanne Treaty who, under this restrictive reading, have limited diplomatic leverage to intervene on behalf of minority institutions, as the issues are framed as internal domestic matters. They would argue for international supervision.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, analytical, global).

% Attempt to apply human rights law to protect minority institutions, but face challenges from the Turkish state's assertion of domestic jurisdiction and its restrictive interpretation of the Lausanne Treaty. Their judgments are often resisted.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_mechanisms, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the Turkish state's internal legal and administrative practices by asserting clear domestic jurisdiction over minority institutional matters, thereby reducing ambiguity in governance.
% TRANSFER_FUNCTION: Transfers control and potential assets from non-Muslim minority institutions to the Turkish state apparatus, by reclassifying institutional autonomy and property rights as domestic matters subject to general law.
% ABSENT_VOICES: The pre-1923 minority religious governance structures and their historical claims to self-administration are effectively silenced, as their legal basis is denied by this reading. Guarantor states and international human rights bodies are also largely excluded from effective intervention.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, non-Muslim minority communities would immediately assert broader institutional autonomy, property rights, and the right to theological education under an expansive interpretation of Lausanne. This would necessitate a significant re-evaluation of state-minority relations and potentially lead to restitution of confiscated properties and re-establishment of educational institutions.
% FOUNDING_PROBLEM: The Lausanne Treaty aimed to establish peace and define the borders and rights of minorities in the newly formed Republic of Turkey after the collapse of the Ottoman Empire, resolving complex ethno-religious claims.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state apparatus attests that the founding problem of national sovereignty and internal stability remains live, justifying its interpretation. Non-Muslim minority communities and international observers attest that the problem of minority rights protection is far from resolved, and that the restrictive reading exacerbates it, citing numerous human rights reports and legal challenges from outside the benefiting parties.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.88) because this reading allows for the systematic confiscation of minority property and the denial of institutional legal personality, directly transferring control and value to the state. Suppression is also very high (0.92) as the state actively enforces this interpretation through legal and administrative means, with limited avenues for appeal or international intervention. The theater ratio is low (0.15) because the state's actions are largely direct and functional in achieving its goals, rather than performative. Resistance is high (0.70) due to ongoing legal challenges and international advocacy, but largely ineffective against the state's entrenched position.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Turkish state, this reading upholds national sovereignty and domestic legal order, appearing as a legitimate exercise of state power. From the perspective of minority communities, it is a clear snare, systematically dismantling their institutional life and cultural heritage. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the clear beneficiary and agenda-setter, gaining control and assets (d near 0.0). Non-Muslim minority foundations and communities are the primary victims and payers, experiencing direct extraction and suppression (d near 1.0). Minority religious leaders also bear significant costs. Guarantor states and European human rights mechanisms are excluded or operate as observers, unable to significantly alter the constraint's operation under this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Is the Lausanne Treaty''s text genuinely ambiguous regarding institutional rights, or is the restrictive reading a deliberate reinterpretation against the spirit of the treaty?',
    'Historical analysis of diplomatic records and preparatory works of the Lausanne Treaty, alongside comparative analysis of similar minority protection treaties.',
    'If genuinely ambiguous, the constraint''s extractiveness might be re-evaluated as a consequence of legal uncertainty. If a deliberate reinterpretation, it strengthens the snare classification by highlighting intentionality in extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, empirical, 'Ambiguity of treaty text vs. deliberate reinterpretation.').

omega_variable(
    international_enforcement_capacity,
    'To what extent could guarantor states or international human rights mechanisms effectively challenge this restrictive reading if they chose to exert maximum diplomatic and legal pressure?',
    'Analysis of historical precedents for international intervention in similar cases, and a counterfactual assessment of the political will and legal tools available to guarantor states.',
    'If international enforcement capacity is high but unexercised, the suppression metric might be lower than currently assessed, as the constraint''s persistence relies on a lack of external challenge rather than inherent state power. If capacity is genuinely low, the high suppression is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_enforcement_capacity, conceptual, 'Effectiveness of international enforcement against domestic interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.2).
narrative_ontology:measurement(laus_tr_t1950, lausanne_minority_protections__restrictive_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(laus_tr_t1980, lausanne_minority_protections__restrictive_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__restrictive_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(laus_be_t1950, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1950, 0.75).
narrative_ontology:measurement(laus_be_t1980, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(laus_su_t1950, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(laus_su_t1980, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, minority_religious_education_ban).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, minority_property_confiscation_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'lausanne_minority_protections' kernel. This 'restrictive_reading' directly influences the 'minority_religious_education_ban' and 'minority_property_confiscation_laws' by providing their legal justification. It stands in direct opposition to the 'expansive_reading' and 'guarantor_reading' of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
