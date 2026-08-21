% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine in Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'universal_heritage_reading' of
 *   the 'cultural_property_legal_corpus' kernel. It describes the legal and
 *   diplomatic framework that asserts cultural artifacts are humanity's
 *   shared heritage, with legitimate authority resting with institutions
 *   (often Western museums) that maximize preservation and universal access,
 *   regardless of geographic origin. This reading often frames repatriation
 *   claims from originating nations or indigenous communities as
 *   particularist threats to a global public good. The constraint is CLAIMED
 *   as a 'rope' (pure coordination for global benefit) by its proponents, but
 *   the authored metrics reflect its substantially extractive and suppressive
 *   operation, particularly for claimant states and communities.
 *
 * KEY AGENTS:
 *   - universal_museums: Primary agenda_setter (institutional/arbitrage) — benefits from current framework
 *   - claimant_successor_states: Primary target (institutional/constrained) — bears legal/diplomatic costs
 *   - indigenous_communities: Primary target (powerless/identity_locked) — bears cultural/identity harm
 *   - international_legal_scholars: Analytical observer — highlights structural imbalances
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.75).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.8).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine in Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'fc0c848f-71a5-4e5b-8315-08247ded4f58').
narrative_ontology:cs_kernel_codification('fc0c848f-71a5-4e5b-8315-08247ded4f58', formalized).
narrative_ontology:cs_authority_grounding('fc0c848f-71a5-4e5b-8315-08247ded4f58', extraction).
narrative_ontology:cs_interpretation_layer_present('fc0c848f-71a5-4e5b-8315-08247ded4f58').
narrative_ontology:cs_reading_relation('fc0c848f-71a5-4e5b-8315-08247ded4f58', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc0c848f-71a5-4e5b-8315-08247ded4f58', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('fc0c848f-71a5-4e5b-8315-08247ded4f58', foundational, cultural_heritage_is_universal).
narrative_ontology:cs_axiom_status(cultural_heritage_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('fc0c848f-71a5-4e5b-8315-08247ded4f58', cultural_heritage_is_universal, deontological).
narrative_ontology:cs_axiom('fc0c848f-71a5-4e5b-8315-08247ded4f58', secondary, preservation_requires_institutional_custody).
narrative_ontology:cs_axiom_status(preservation_requires_institutional_custody, holdable).
narrative_ontology:cs_axiom_grounding('fc0c848f-71a5-4e5b-8315-08247ded4f58', preservation_requires_institutional_custody, instrumental).
narrative_ontology:cs_reference_frame('fc0c848f-71a5-4e5b-8315-08247ded4f58', post_colonial_universalism).
narrative_ontology:cs_drift_state('fc0c848f-71a5-4e5b-8315-08247ded4f58', contemporary_repatriation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fc0c848f-71a5-4e5b-8315-08247ded4f58', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, collecting_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, global_art_market).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, unesco).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major institutions holding vast collections of cultural artifacts from diverse origins. They advocate for the universal heritage principle, asserting their role in maximizing preservation and access for all humanity. They benefit from the current legal framework that largely supports their retention of objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_museums, agenda_setter,
    institutional, generational, arbitrage, global).

% Smaller museums and private collections that benefit from the legal precedents and diplomatic norms established by the universal heritage doctrine, which generally favors current holders over repatriation claims. They face less direct scrutiny than universal museums but operate within the same framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, collecting_institutions, beneficiary,
    powerful, biographical, mobile, national).

% Benefits from the free flow and commodification of cultural artifacts, which the universal heritage reading implicitly supports by de-emphasizing origin and specific cultural ties. A more restrictive repatriation regime would disrupt their operations.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, global_art_market, beneficiary,
    powerful, immediate, arbitrage, global).

% Post-colonial nations seeking the return of cultural artifacts removed during colonial periods. They bear significant legal, diplomatic, and financial costs in pursuing repatriation claims, which are often framed by the universal heritage doctrine as particularist threats to a global public good.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states, payer,
    institutional, generational, constrained, national).

% Communities with deep cultural, spiritual, and ancestral ties to artifacts held in foreign institutions. Their claims are often marginalized or unrecognized by the universal heritage framework, which prioritizes institutional preservation over traditional stewardship and identity. They experience profound identity harm and cultural loss.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    powerless, civilizational, identity_locked, local).

% Similar to indigenous communities, these groups have strong cultural connections to artifacts but may lack formal legal standing or state backing. They bear the emotional and cultural costs of separation from their heritage, with limited avenues for redress within the dominant legal framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities, payer,
    powerless, generational, identity_locked, local).

% Academics and legal experts who analyze the cultural property regime, often highlighting its historical biases, power imbalances, and the tension between universalist claims and rights-based repatriation demands. They can influence policy debates but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% An international organization that promotes cultural heritage protection and international cooperation. While it facilitates dialogue, its conventions (like 1970) are often interpreted in ways that reinforce the status quo of holding institutions, making it an indirect beneficiary of the universal heritage framework's stability.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, unesco, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, unesco, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global efforts for the preservation, scientific study, and public display of cultural artifacts, ensuring their availability to all humanity and preventing their destruction or illicit trade.
% TRANSFER_FUNCTION: Transfers de facto ownership, control, and interpretive authority of cultural artifacts from their geographic/cultural origin to holding institutions, along with the associated costs of preservation and benefits of display/study. It also transfers significant legal and diplomatic costs to claimant states and communities.
% ABSENT_VOICES: Indigenous communities and descendant communities whose traditional stewardship practices, spiritual connections, and self-determination rights are often not fully recognized or prioritized by the universal heritage framework. Their voices are frequently marginalized in international legal and museum discourse.
% DISAPPEARANCE_RATIONALE: If the universal heritage doctrine and its supporting legal/diplomatic structures vanished overnight, the legal and ethical landscape for cultural property would be radically altered. Repatriation claims would gain significant legal and moral force, leading to a massive reorganization of museum collections, a re-evaluation of international cultural exchange, and a shift in power dynamics towards originating communities and states.
% FOUNDING_PROBLEM: To prevent the destruction, neglect, and illicit trafficking of cultural artifacts, particularly after periods of conflict, colonial disruption, or inadequate local preservation capacity, and to ensure their scientific study and public appreciation by a global audience.
% FOUNDING_PROBLEM_CORROBORATION: Universal museums and collecting institutions assert that the problem of preservation and universal access remains live, citing ongoing threats to heritage and the benefits of global scholarship. Claimant states, indigenous communities, and post-colonial scholars attest that while preservation is important, the founding problem has substantially shifted to equitable distribution, recognition of cultural identity, and redress for historical injustices; they argue the universalist framing now primarily serves to maintain colonial-era acquisitions. Legislative hearings and independent cultural studies from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant legal, diplomatic, and cultural costs borne by claimant states and communities in their efforts to repatriate artifacts, while holding institutions retain control and benefit from their collections. Suppression (0.80) is high because the framework actively resists and delegitimizes alternative claims (e.g., sovereign or indigenous ownership), requiring continuous legal and diplomatic enforcement to maintain the status quo. Theater ratio (0.40) is moderate; while genuine preservation and research occur, a substantial portion of institutional activity is performative defense of current holdings against repatriation demands. Accessibility collapse (0.70) is high because the universal heritage framing often presents repatriation as a 'collapse' of access, rather than a shift in stewardship. Resistance (0.75) is substantial, reflecting ongoing, organized efforts by claimant states and indigenous groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universal museums and collecting institutions, the doctrine is a necessary coordination mechanism for global cultural preservation and access. From the perspective of claimant states and indigenous communities, the same doctrine operates as a mechanism of continued colonial extraction, denying their rights to heritage and inflicting identity harm. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Universal museums and the global art market are clear beneficiaries (low d) as they directly profit from or are enabled by the current framework. Claimant successor states, indigenous communities, and descendant communities are clear targets (high d) as they bear the costs of exclusion and the fight for repatriation. UNESCO sits in a more complex position, benefiting from its role in global cultural governance but also facing pressure to address repatriation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent destruction and ensure access. While preservation efforts are real, the doctrine's application has increasingly shifted to defending the retention of contested objects, often against the wishes of originating communities. This suggests a degree of mandatrophy, where the original coordination function is overshadowed by the extractive function of maintaining existing power structures and collections. The 'contested' status of the founding problem in the six questions reflects this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particular_value,
    'Is the value of cultural heritage truly universal, or is its primary value tied to specific communities and their cultural continuity?',
    'Cross-cultural studies on the impact of heritage loss/return on community well-being and identity, alongside philosophical analysis of ''universal'' vs. ''particular'' claims in cultural value.',
    'If value is primarily particular, the universal heritage claim loses its moral force, strengthening repatriation arguments and reclassifying the constraint as more purely extractive. If truly universal, the coordination function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particular_value, conceptual, 'Ambiguity in the nature of cultural value (universal vs. particular).').

omega_variable(
    preservation_capacity_disparity,
    'Is the claim that holding institutions offer superior preservation capacity still empirically true, or have claimant states and communities developed comparable capabilities?',
    'Independent audits of preservation infrastructure and expertise in originating nations, comparing them to major Western museums. Longitudinal studies of artifact condition post-repatriation.',
    'If preservation capacity is now comparable or superior in originating nations, a key instrumental justification for the universal heritage doctrine is undermined, increasing its perceived extractiveness. If disparity remains, the coordination argument for retention is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preservation_capacity_disparity, empirical, 'Empirical status of preservation capacity claims.').

omega_variable(
    identity_harm_quantification,
    'How can the identity harm and cultural loss experienced by communities denied their heritage be adequately quantified and weighed against claims of universal access?',
    'Development of robust methodologies for socio-cultural impact assessment, incorporating indigenous epistemologies and community-led research, to provide evidence for legal and policy decisions.',
    'Clearer quantification of identity harm would strengthen the moral and legal arguments for repatriation, increasing the perceived extractiveness of the universal heritage framework. Lack of quantification allows the harm to remain unacknowledged in policy debates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_harm_quantification, empirical, 'Quantification of identity harm from heritage denial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(cult_tr_t2020, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(cult_be_t2020, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(cult_su_t2020, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.2).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'cultural_property_legal_corpus' kernel. It represents the universal heritage perspective, which competes with sovereign repatriation and indigenous stewardship readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
