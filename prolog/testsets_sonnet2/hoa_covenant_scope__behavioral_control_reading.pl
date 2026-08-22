% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Enforcement
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story reads the HOA covenant's SCOPE — specifically its extension
 *   from basic maintenance standards into subjective aesthetic uniformity,
 *   lifestyle restriction, and speech suppression (yard signs, flags,
 *   non-standard landscaping) — as a behavioral control mechanism justified
 *   by a property-value-maximization rationale that lacks robust independent
 *   corroboration once basic upkeep is separated out. This is deliberately
 *   narrower than the coordination reading (which addresses genuine
 *   shared-infrastructure and externality problems the same covenant can also
 *   solve) and the extraction reading (which addresses fine revenue and board
 *   power consolidation as a distinct mechanism). The three readings share a
 *   kernel — 'why does the covenant have the scope it has' — but instantiate
 *   structurally distinct constraints with distinct ε, distinct
 *   beneficiaries, and distinct victims. This story's ε (0.42) reflects
 *   moderate extraction: real value is not zero (some maintenance-adjacent
 *   coordination persists inside the same document) but the
 *   aesthetic-conformity layer imposes real costs on nonconformists without
 *   demonstrated offsetting value.
 *
 * KEY AGENTS:
 *   - hoa_board: agenda_setter, interprets ambiguous standards case by case
 *   - conformist_majority: beneficiary, preferences already match enforced standard
 *   - board_aligned_homeowners: beneficiary+agenda_setter, receives lenient discretionary treatment
 *   - nonconformist_homeowners: payer, trapped by equity and remediation costs
 *   - marginal_aesthetics_practitioners: payer, penalized for visible difference regardless of harm
 *   - renters_and_political_speech_seekers: payer, content-neutral rules function as speech suppression
 *   - prospective_buyers: excluded, no voice in standards they will be bound by
 *   - local_housing_researchers: observer, tests the value-maximization claim empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.58).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Enforcement").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'dbaa3b1b-727c-4d01-af91-43793e9c7a86').
narrative_ontology:cs_kernel_codification('dbaa3b1b-727c-4d01-af91-43793e9c7a86', formalized).
narrative_ontology:cs_authority_grounding('dbaa3b1b-727c-4d01-af91-43793e9c7a86', extraction).
narrative_ontology:cs_interpretation_layer_present('dbaa3b1b-727c-4d01-af91-43793e9c7a86').
narrative_ontology:cs_reading_relation('dbaa3b1b-727c-4d01-af91-43793e9c7a86', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbaa3b1b-727c-4d01-af91-43793e9c7a86', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('dbaa3b1b-727c-4d01-af91-43793e9c7a86', foundational, aesthetic_uniformity_is_value_protective).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_is_value_protective, holdable).
narrative_ontology:cs_axiom_grounding('dbaa3b1b-727c-4d01-af91-43793e9c7a86', aesthetic_uniformity_is_value_protective, empirically_contingent).
narrative_ontology:cs_axiom('dbaa3b1b-727c-4d01-af91-43793e9c7a86', foundational, majority_taste_may_bind_minority_expression_on_owned_property).
narrative_ontology:cs_axiom_status(majority_taste_may_bind_minority_expression_on_owned_property, holdable).
narrative_ontology:cs_axiom_grounding('dbaa3b1b-727c-4d01-af91-43793e9c7a86', majority_taste_may_bind_minority_expression_on_owned_property, conventional).
narrative_ontology:cs_reference_frame('dbaa3b1b-727c-4d01-af91-43793e9c7a86', maintenance_standard_baseline).
narrative_ontology:cs_drift_state('dbaa3b1b-727c-4d01-af91-43793e9c7a86', contemporary_suburban_hoa_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbaa3b1b-727c-4d01-af91-43793e9c7a86', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_practitioners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, renters_and_political_speech_seekers).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, uniform_appearance_maximizes_property_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the architectural and behavioral standards, deciding case by case what counts as an acceptable exterior color, landscaping style, or permissible yard display. Board members typically live in the community but rotate in and out, and can vote to tighten standards without owner-wide referendum in many bylaw structures.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    organized, biographical, arbitrage, local).

% Prefers visual homogeneity and believes it protects resale value; supports aggressive enforcement against deviation and experiences the covenant as validating and protective rather than restrictive, since their preferences already match the enforced standard.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    moderate, biographical, constrained, local).

% Homeowners with informal influence over board composition or enforcement discretion; their own properties receive lenient interpretation of ambiguous rules while others' comparable choices are cited as violations, giving them the benefits of the standard without its full weight.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter).

% Want a paint color, garden style, or exterior modification outside the board's aesthetic comfort zone. Face escalating notices, fines, and potential liens for noncompliance. Selling to exit means absorbing transaction costs and losing equity tied up in a home whose modifications must often be reverted first.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, biographical, trapped, local).

% Practice xeriscaping, non-traditional gardens, visible solar installations, or other choices that diverge from the community's default aesthetic for ecological, cultural, or economic reasons. Their preferences are treated as violations regardless of any actual harm to neighbors or property values, and enforcement is triggered by visual difference itself.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_practitioners, payer,
    powerless, biographical, trapped, local).

% Want to display yard signs, flags, or seasonal decorations expressing political or personal views. Covenant sign and display restrictions are enforced as content-neutral aesthetic rules but function to suppress visible expression the board or majority finds discordant or controversial.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, renters_and_political_speech_seekers, payer,
    powerless, immediate, trapped, local).

% Considering purchase in the community are shown covenant documents but rarely see enforcement history or the board's actual pattern of discretionary interpretation before committing; they have no voice in shaping the standards they will be bound by until after purchase.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_buyers, excluded,
    powerless, immediate, mobile, regional).

% Study whether aesthetic uniformity covenants actually correlate with higher resale values independent of confounding factors like neighborhood income and school quality, and document enforcement patterns across many HOAs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, local_housing_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared visual standard that removes the need for case-by-case neighbor negotiation over exterior appearance, which some homeowners genuinely value and would choose voluntarily.
% TRANSFER_FUNCTION: Moves discretion over personal property use and expression from individual homeowners to the board and the aesthetic preferences of the conformist majority; moves compliance costs (fines, forced remediation, foregone preferred use) from the majority onto those whose preferences diverge.
% ABSENT_VOICES: Nonconformist homeowners and marginal aesthetics practitioners raise objections in board meetings but are structurally outvoted; renters have no standing in HOA governance at all despite living under its display restrictions; prospective buyers who would object to the covenant's scope self-select out before purchase, leaving only those who already accept or tolerate it in the room.
% DISAPPEARANCE_RATIONALE: If the aesthetic and behavioral provisions vanished, homeowners currently facing fines or forced remediation would resume the modifications they wanted, yard displays and diverse landscaping would reappear, and the community's visual homogeneity would erode over several years as owners exercised previously suppressed preferences.
% FOUNDING_PROBLEM: Early-generation subdivisions wanted a mechanism to prevent visibly deteriorating properties (unmaintained yards, abandoned vehicles, structural neglect) from depressing neighboring home values, and to provide some assurance of neighborhood character to buyers.
% FOUNDING_PROBLEM_CORROBORATION: Real estate economists and independent appraisal researchers attest that maintenance-standard enforcement (preventing neglect) has measurable value-protective effect, but find no consistent independent evidence that subjective aesthetic uniformity beyond basic upkeep adds resale value once neighborhood and school-district controls are applied — suggesting the aesthetic-conformity extension of the original problem is not corroborated outside the board and the majority who prefer it.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) sits at the moderate end because the covenant's aesthetic-conformity layer is real but not maximally severe — homeowners retain some property rights and the fines/liens mechanism, while coercive, is bounded by state HOA law in most jurisdictions. Suppression (0.58) is higher than extraction because the mechanism that keeps nonconformists compliant (threat of fines escalating to liens and forced sale) is a strong coercive lever even when actual enforcement is intermittent. Theater ratio (0.30) reflects that some enforcement genuinely serves the stated maintenance-standard function while a growing share is aesthetic-preference policing dressed in property-value language. Accessibility collapse (0.50) is moderate: buyers can choose non-HOA housing, but within HOA-covered markets alternatives are limited and often disclosed only after purchase decisions are substantially made. Resistance (0.55) reflects real organized pushback from nonconformist and civil-liberties-oriented owners, especially around political sign restrictions, which increasingly draw state legislative attention.
 *
 * PERSPECTIVAL GAP:
 *   From the board and conformist-majority seat, the covenant looks like straightforward, benignly-motivated coordination protecting shared value — this is the coordination_reading's territory, not fully absent here but subordinated. From the nonconformist and speech-restricted seats, the identical rule text computes as behavioral control enforced through property-threatening coercion. The engine should compute these seats to different types from the same structural data; that divergence is the point of separating this reading from its siblings rather than averaging across them.
 *
 * DIRECTIONALITY LOGIC:
 *   Conformist majority and board-aligned homeowners derive low d — the covenant's aesthetic enforcement subsidizes their existing preferences and, for board-aligned owners, comes with discretionary leniency. Nonconformist homeowners, marginal aesthetics practitioners, and renters/speech-seekers derive high d — they bear fines, forced remediation, and suppressed expression while gaining no corresponding benefit from the standard since their preferences diverge from it. Trapped exit options for payer groups (equity lock-in, remediation costs, no voice for renters) amplify effective extraction toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing visible neglect that depresses neighboring value) remains partly live, but the aesthetic-uniformity extension of that mandate has outlived any demonstrated function — independent appraisal research finds no robust value effect from conformity provisions beyond basic upkeep once other variables are controlled. The mandate for THIS reading's scope (aesthetic and behavioral conformity specifically) is a mandatrophy candidate distinct from the maintenance-standard core, which remains functionally justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aesthetic_value_maximization_causal_claim,
    'Does enforced aesthetic uniformity beyond basic maintenance actually increase resale value, or is the value-maximization rationale a post-hoc justification for enforcing majority taste preference?',
    'Hedonic regression studies comparing resale premiums in HOAs with strict aesthetic covenants versus HOAs with maintenance-only covenants, controlling for neighborhood income, school district, and base housing stock quality.',
    'If no independent value effect is found beyond basic upkeep, the behavioral_control_reading''s stated justification collapses into pure preference enforcement, strengthening the case that this reading is a snare rather than a rope; if a genuine effect is found, some of the extraction reclassifies as legitimate coordination and ε should be revised downward in a future version.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_value_maximization_causal_claim, empirical, 'Whether aesthetic uniformity enforcement causally increases property value independent of confounds.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the coordination function (maintenance standards, shared infrastructure) end and the behavioral-control function (aesthetic taste, lifestyle restriction, speech suppression) begin within a single covenant document?',
    'Clause-by-clause classification of covenant provisions by function (externality-prevention vs. taste-enforcement vs. revenue/discretion) across a representative sample of HOA governing documents.',
    'A sharp, easily-drawn boundary would support treating the readings as cleanly separable constraints (as this story assumes); a blurred or contested boundary would suggest the three kernel readings are more entangled in practice than the decomposition implies, and enforcement data would need to trace which function a given fine actually served.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the coordination and behavioral-control functions are cleanly separable within one covenant text.').

omega_variable(
    content_neutrality_of_display_restrictions,
    'Are yard-sign and flag restrictions genuinely content-neutral aesthetic rules, or do they function as viewpoint-selective suppression in practice even when written neutrally?',
    'Enforcement-pattern analysis comparing complaint and fine rates against sign content across political and non-political categories within the same HOA over multiple election cycles.',
    'Evidence of viewpoint-selective enforcement would elevate this reading''s suppression score and support classifying the speech-restriction component as a distinct, more severe extraction than the general aesthetic-uniformity component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_neutrality_of_display_restrictions, empirical, 'Whether facially neutral display rules are enforced in a viewpoint-selective manner.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hoa__su_t4, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.08).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the behavioral_control_reading member of a three-way kernel decomposition of 'the HOA covenant.' The coordination_reading addresses the genuine shared-infrastructure/externality function of the same covenant text (lower ε, rope-leaning). The extraction_reading addresses fine-revenue generation and board power consolidation as a distinct mechanism riding on the same enforcement apparatus (potentially higher ε, more clearly snare-leaning via a different transfer path — money and governance power rather than expression and taste conformity). All three stories should be read together to understand the covenant's full structural profile; none alone captures it, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
