% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Secession Legitimacy Boundary: Treaty Primacy Reading
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'treaty primacy' reading of the secession
 *   legitimacy boundary, asserting that Indigenous treaty rights predate and
 *   supersede both federal and provincial authority, making Indigenous
 *   consent a prerequisite for any legitimate secession. This reading
 *   positions Indigenous treaty holders as beneficiaries whose rights are
 *   protected, while federal and provincial governments, particularly
 *   secessionist ones, are constrained. The constraint is classified as a
 *   Tangled Rope because it genuinely coordinates a complex multi-party
 *   territorial claim while simultaneously extracting concessions (consent)
 *   from other parties and requiring active enforcement to maintain
 *   Indigenous rights against historical and ongoing pressures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Secession Legitimacy Boundary: Treaty Primacy Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '18bef204-9db7-4575-b7db-a1ab2969f04e').
narrative_ontology:cs_kernel_codification('18bef204-9db7-4575-b7db-a1ab2969f04e', formalized).
narrative_ontology:cs_authority_grounding('18bef204-9db7-4575-b7db-a1ab2969f04e', lineage).
narrative_ontology:cs_interpretation_layer_present('18bef204-9db7-4575-b7db-a1ab2969f04e').
narrative_ontology:cs_reading_relation('18bef204-9db7-4575-b7db-a1ab2969f04e', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('18bef204-9db7-4575-b7db-a1ab2969f04e', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('18bef204-9db7-4575-b7db-a1ab2969f04e', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('18bef204-9db7-4575-b7db-a1ab2969f04e', foundational, indigenous_sovereignty_precedes_crown).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_precedes_crown, holdable).
narrative_ontology:cs_axiom_grounding('18bef204-9db7-4575-b7db-a1ab2969f04e', indigenous_sovereignty_precedes_crown, deontological).
narrative_ontology:cs_axiom('18bef204-9db7-4575-b7db-a1ab2969f04e', foundational, treaty_as_nation_to_nation_agreement).
narrative_ontology:cs_axiom_status(treaty_as_nation_to_nation_agreement, holdable).
narrative_ontology:cs_axiom_grounding('18bef204-9db7-4575-b7db-a1ab2969f04e', treaty_as_nation_to_nation_agreement, conventional).
narrative_ontology:cs_reference_frame('18bef204-9db7-4575-b7db-a1ab2969f04e', pre_colonial_indigenous_sovereignty).
narrative_ontology:cs_drift_state('18bef204-9db7-4575-b7db-a1ab2969f04e', contemporary_post_colonial_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('18bef204-9db7-4575-b7db-a1ab2969f04e', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their rights and title are affirmed as pre-existing and paramount, requiring their consent for any territorial changes. They benefit from the recognition of their inherent sovereignty and the protection of their traditional lands and resources, but are identity-locked to their ancestral territories.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    organized, generational, identity_locked, regional).

% Seeks to assert unilateral sovereignty over its territory, but is constrained by the requirement to obtain Indigenous consent for secession. This reading imposes a significant hurdle to their claims, forcing negotiation or invalidating their legitimacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_provincial_government, payer,
    institutional, immediate, constrained, national).

% Is bound by existing treaties and has a fiduciary duty to Indigenous peoples. This reading reinforces its obligation to uphold treaty rights, potentially complicating its ability to manage secessionist movements or negotiate territorial divisions without Indigenous involvement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Their aspirations for provincial self-determination are made contingent on Indigenous consent, which can be perceived as an external constraint on their democratic will. They bear the costs of delayed or complicated secession processes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens, payer,
    moderate, biographical, constrained, regional).

% Analyze the legitimacy of secession claims through the lens of international law, including the rights of Indigenous peoples. This reading aligns with evolving international norms regarding self-determination and prior informed consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process of territorial change and sovereignty claims by establishing a clear, pre-existing legal and moral boundary (Indigenous treaty rights) that all other claims must respect, preventing unilateral actions that would destabilize existing relationships.
% TRANSFER_FUNCTION: Transfers the ultimate authority over territorial disposition from federal/provincial governments to Indigenous treaty holders, ensuring that their consent is a prerequisite for any secession, thereby protecting their land, resources, and self-determination.
% ABSENT_VOICES: Future generations of Indigenous peoples, whose ancestral lands and rights are being protected by this reading, are implicitly represented. Their voices would strongly affirm the primacy of treaty rights and the necessity of consent.
% DISAPPEARANCE_RATIONALE: If this reading vanished, secessionist movements would gain significant leverage, potentially leading to unilateral declarations of independence that disregard Indigenous rights. This would trigger widespread legal challenges, social unrest, and a fundamental reordering of sovereignty claims and territorial integrity, likely resulting in new forms of extraction from Indigenous lands.
% FOUNDING_PROBLEM: The historical and ongoing dispossession of Indigenous lands and the disregard for their inherent sovereignty by colonial and post-colonial states, leading to conflicts over resource extraction and territorial control.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights organizations, and UN declarations (e.g., UNDRIP) corroborate that the problem of Indigenous dispossession and the need for consent remains live, despite some legal advancements. This corroboration comes from outside the direct beneficiaries of the reading.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because this reading imposes a significant, non-negotiable cost (the requirement of consent) on any party seeking to alter territorial arrangements. Suppression (0.70) is high because this reading actively suppresses unilateral secessionist claims and requires ongoing legal and political enforcement to prevent federal or provincial governments from overriding treaty rights. Theater ratio (0.20) is low, as the recognition of treaty primacy is a substantive legal and political reality, not merely performative. Resistance (0.80) is high from secessionist movements and some provincial governments who view it as an impediment to their self-determination. Accessibility collapse (0.40) is moderate, as alternatives (e.g., unilateral declarations) are not entirely foreclosed but are rendered illegitimate by this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous treaty holders, this reading is a necessary recognition of inherent rights and a coordination mechanism to ensure justice. From the perspective of a secessionist provincial government, it is an extractive constraint that limits their sovereignty. The engine's per-seat classification will reflect these divergent experiences based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders are the primary beneficiaries (d=0.0-0.2) as this reading protects their pre-existing rights and requires their consent. The secessionist provincial government and the federal government are targets (d=0.8-1.0) as they are constrained by the requirement for Indigenous consent, which limits their unilateral authority. Non-Indigenous provincial citizens are also targets (d=0.6-0.8) as their political aspirations are made contingent on Indigenous consent. International legal observers are analytical (d=0.5) as they assess the constraint's alignment with international norms without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the protection of Indigenous rights as mere extraction by recognizing the genuine coordination function of establishing a legitimate process for territorial change. It acknowledges that while consent imposes a cost on other parties, it is a necessary cost for upholding pre-existing rights and achieving a just resolution, rather than an arbitrary rent. The 'live' status of the founding problem (historical dispossession) further indicates that the mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_consent,
    'What constitutes ''consent'' from Indigenous treaty holders in the context of secession, and at what level (individual, community, nation) must it be obtained?',
    'Legal precedent from court challenges or negotiated agreements that define the scope and process of consent in specific territorial contexts.',
    'A broad definition of consent (e.g., requiring consensus from all affected nations) would significantly increase the constraint''s effective extractiveness on secessionist parties; a narrow definition (e.g., majority vote within a single band) would reduce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_consent, conceptual, 'Ambiguity regarding the definition and scope of Indigenous consent for secession.').

omega_variable(
    treaty_interpretation_divergence,
    'How do different interpretations of specific treaty texts (e.g., ''peace and friendship'' vs. ''land surrender'') affect the scope of Indigenous authority over territorial changes?',
    'Judicial rulings on specific treaty clauses or historical commissions that clarify the original intent and ongoing legal force of treaties.',
    'An interpretation favoring Indigenous inherent sovereignty would strengthen this reading''s force, increasing extractiveness on other parties; an interpretation favoring Crown sovereignty would weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_interpretation_divergence, empirical, 'Divergent interpretations of historical treaty texts and their implications for modern sovereignty claims.').

omega_variable(
    international_law_integration,
    'To what extent does international law (e.g., UNDRIP) directly inform or override domestic constitutional interpretations of secession and Indigenous rights?',
    'Supreme Court rulings on the domestic applicability of international Indigenous rights instruments, or legislative acts incorporating international norms into domestic law.',
    'Stronger integration of international law would reinforce the treaty primacy reading, potentially increasing its effective extractiveness and suppression on domestic actors; weaker integration would diminish its force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_law_integration, conceptual, 'The degree to which international Indigenous rights law is integrated into domestic legal frameworks governing secession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'secession_legitimacy_boundary' kernel. This 'treaty_primacy_reading' asserts Indigenous consent as paramount, influencing and coexisting with other readings that emphasize constitutional legality, popular will, or grievance thresholds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
