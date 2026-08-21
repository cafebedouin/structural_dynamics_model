% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Norms
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes jati boundaries as dynamic, locally negotiated
 *   social norms, rather than fixed categories derived from scripture or
 *   colonial administration. This 'localized practice' reading emphasizes the
 *   agency of local communities in shaping their social structures, leading
 *   to a proliferation of specific jati identities (over 3000 empirically
 *   observed) that adapt to local conditions. The low extractiveness and
 *   suppression reflect the rope-like nature of these norms, where
 *   coordination and adaptation are primary, and coercion is minimal compared
 *   to other readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.25).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.3).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '6623d797-ae95-4688-ac7c-3a1a432256af').
narrative_ontology:cs_kernel_codification('6623d797-ae95-4688-ac7c-3a1a432256af', distributed).
narrative_ontology:cs_authority_grounding('6623d797-ae95-4688-ac7c-3a1a432256af', practice).
narrative_ontology:cs_interpretation_layer_present('6623d797-ae95-4688-ac7c-3a1a432256af').
narrative_ontology:cs_reading_relation('6623d797-ae95-4688-ac7c-3a1a432256af', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('6623d797-ae95-4688-ac7c-3a1a432256af', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('6623d797-ae95-4688-ac7c-3a1a432256af', foundational, jati_is_locally_constituted).
narrative_ontology:cs_axiom_status(jati_is_locally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('6623d797-ae95-4688-ac7c-3a1a432256af', jati_is_locally_constituted, empirically_contingent).
narrative_ontology:cs_axiom('6623d797-ae95-4688-ac7c-3a1a432256af', foundational, social_norms_are_adaptive).
narrative_ontology:cs_axiom_status(social_norms_are_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('6623d797-ae95-4688-ac7c-3a1a432256af', social_norms_are_adaptive, empirically_contingent).
narrative_ontology:cs_reference_frame('6623d797-ae95-4688-ac7c-3a1a432256af', dynamic_local_adaptation).
narrative_ontology:cs_drift_state('6623d797-ae95-4688-ac7c-3a1a432256af', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6623d797-ae95-4688-ac7c-3a1a432256af', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, community_elders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, individual_members).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, social_constructivism).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, local_agency_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities benefit from the flexibility of jati boundaries, allowing them to adapt to local economic, social, and political changes. They actively participate in the continuous renegotiation of these norms, which define marriage, occupation, and social interaction within their specific locality.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_communities, beneficiary,
    organized, generational, constrained, local).

% Elders and local leaders play a key role in mediating disputes and guiding the renegotiation of jati norms. Their authority is derived from their knowledge of local traditions and their ability to maintain social cohesion. They benefit from the stability and respect their role provides.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, community_elders, agenda_setter,
    powerful, biographical, constrained, local).

% Individuals navigate the locally defined jati norms, which dictate aspects of their social life. While they benefit from the coordination, they also bear the cost of adhering to specific social expectations and the potential for exclusion if they deviate too far from accepted practice. Exit is constrained by social ties and economic dependence.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, individual_members, payer,
    moderate, biographical, constrained, local).

% Scholars who adhere to a textual, varna-based understanding of jati find their interpretations often ignored or actively contradicted by localized practices. They are excluded from the actual, on-the-ground renegotiation processes, despite their claims to scriptural authority.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, orthodox_religious_scholars, excluded,
    institutional, civilizational, identity_locked, national).

% Historically, these administrators attempted to codify and fix jati categories for administrative purposes (e.g., census, land records). From this reading's perspective, their efforts were an external imposition that failed to capture the dynamic, localized reality of jati, serving as an analytical counterpoint.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_era_administrators, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for social organization, marriage alliances, occupational specialization, and resource sharing within specific local communities, adapting to changing circumstances without rigid, external imposition.
% TRANSFER_FUNCTION: Facilitates the transfer of social capital, mutual support, and shared identity within local groups, while implicitly transferring the burden of adaptation and negotiation onto community members.
% ABSENT_VOICES: Orthodox religious scholars and proponents of a fixed, textual varna system are absent from the local renegotiation processes; they would argue for adherence to scriptural definitions and against the fluidity of local practice.
% DISAPPEARANCE_RATIONALE: If these localized norms vanished, the intricate social fabric, marriage patterns, and occupational structures of thousands of local communities would unravel, leading to widespread social disorganization and the need for new, likely more rigid, forms of social coordination.
% FOUNDING_PROBLEM: The need for flexible social organization that could adapt to diverse local ecological, economic, and political conditions across a vast and varied subcontinent, providing a framework for identity and cooperation.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological field studies and ethnographic research consistently corroborate the ongoing, adaptive function of localized jati norms in contemporary rural and urban settings, demonstrating their continued relevance for social cohesion and identity formation, independent of textual or administrative claims.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) and suppression (0.30) are consistent with a 'rope' classification, as the norms primarily facilitate coordination and adaptation rather than imposing heavy costs or suppressing alternatives. The continuous renegotiation and proliferation of jati categories (empirical observation of 3000+ categories) directly indicates weak, localized enforcement and a high degree of freedom for communities to define their own boundaries, which is antithetical to high suppression. Theater ratio is low (0.10) because the practices are genuinely functional for local social organization, not performative maintenance of an atrophied mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local communities, these norms are adaptive and beneficial, a 'rope' that facilitates social life. From the perspective of orthodox textual scholars, the same phenomenon might be seen as a 'snare' of deviation from sacred texts, or from colonial administrators, a 'mountain' of intractable social complexity. This story focuses on the localized practice reading, which emphasizes the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Local jati communities are beneficiaries, as the norms provide a flexible framework for their social life. Community elders act as agenda-setters, guiding renegotiation. Individual members are payers, bearing the costs of adherence but also benefiting from the coordination. Orthodox religious scholars are excluded, as their textual interpretations are often irrelevant to local practice. Colonial administrators are observers, their attempts to fix categories serving as a historical counterpoint to the dynamic reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    local_vs_textual_authority,
    'To what extent do local jati practices genuinely operate independently of, or are subtly influenced by, broader textual or pan-Indian varna frameworks?',
    'Detailed ethnographic studies tracing the discursive links between local justifications for jati norms and references to scriptural or historical pan-Indian categories, quantifying the degree of explicit or implicit textual legitimation.',
    'If significant textual influence is found, the extractiveness and suppression might be higher than currently assessed, as local agency would be partly constrained by external, more rigid frameworks. This could shift the classification towards a ''tangled_rope'' or even ''snare'' if the textual framework is found to be highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_vs_textual_authority, empirical, 'Ambiguity regarding the true autonomy of local jati practices from broader textual authorities.').

omega_variable(
    colonial_reification_persistence,
    'Has the colonial-era administrative reification of jati categories, despite its historical inaccuracy, created a persistent ''shadow structure'' that subtly constrains local renegotiation today?',
    'Analysis of contemporary administrative data (e.g., government forms, legal classifications) and interviews with local officials to determine if and how fixed, colonial-era categories still influence resource allocation, political representation, or legal identity, thereby limiting local flexibility.',
    'If a persistent shadow structure is identified, the ''accessibility_collapse'' and ''suppression'' metrics would need upward revision, as external administrative categories would be subtly limiting local agency. This could push the constraint towards a ''tangled_rope'' by introducing an external, extractive layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_reification_persistence, empirical, 'Uncertainty about the lasting, subtle impact of colonial administrative categories on contemporary jati fluidity.').

omega_variable(
    coordination_vs_exclusion_boundary,
    'At what point does the coordination function of localized jati norms (e.g., marriage circles, occupational guilds) transition into exclusionary practices that limit individual mobility or opportunity?',
    'Quantitative sociological studies tracking individual mobility, inter-jati marriage rates, and access to non-traditional occupations across different local communities, correlating these with the rigidity or fluidity of local jati boundaries.',
    'If exclusionary effects are found to be significant and systematic, the ''extractiveness'' metric would need to be revised upward, and the ''victims'' array might need to be populated with ''individuals_seeking_mobility'' or ''inter_jati_couples'', potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for those individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_exclusion_boundary, preference, 'Defining the threshold where coordination becomes harmful exclusion, a value judgment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1800, jati_practice_norm__localized_practice_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(jati_tr_t1850, jati_practice_norm__localized_practice_reading, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(jati_tr_t1900, jati_practice_norm__localized_practice_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(jati_tr_t1950, jati_practice_norm__localized_practice_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__localized_practice_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jati_tr_t2020, jati_practice_norm__localized_practice_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t1800, jati_practice_norm__localized_practice_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(jati_be_t1850, jati_practice_norm__localized_practice_reading, base_extractiveness, 1850, 0.22).
narrative_ontology:measurement(jati_be_t1900, jati_practice_norm__localized_practice_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(jati_be_t1950, jati_practice_norm__localized_practice_reading, base_extractiveness, 1950, 0.23).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__localized_practice_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(jati_be_t2020, jati_practice_norm__localized_practice_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1800, jati_practice_norm__localized_practice_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(jati_su_t1850, jati_practice_norm__localized_practice_reading, suppression_requirement, 1850, 0.28).
narrative_ontology:measurement(jati_su_t1900, jati_practice_norm__localized_practice_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(jati_su_t1950, jati_practice_norm__localized_practice_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__localized_practice_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(jati_su_t2020, jati_practice_norm__localized_practice_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jati_practice_norm' kernel. This 'localized_practice_reading' emphasizes dynamic, community-driven norms, contrasting with the 'orthodox_textual_reading' (fixed by scripture) and the 'colonial_census_reading' (reified by administration). Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
