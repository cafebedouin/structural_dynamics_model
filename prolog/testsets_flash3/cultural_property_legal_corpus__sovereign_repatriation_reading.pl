% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Claim for Cultural Property
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereign repatriation' reading of the
 *   cultural property legal corpus, asserting that cultural artifacts are the
 *   sovereign property of successor states and that colonial acquisition was
 *   illegitimate extraction. It posits that legitimate authority for these
 *   artifacts rests with states claiming historical continuity with
 *   expropriated peoples. This reading is distinct from 'universal heritage'
 *   (which prioritizes preservation and access regardless of origin) and
 *   'indigenous stewardship' (which prioritizes direct community control).
 *   The constraint is classified as a Tangled Rope due to its genuine
 *   coordination function (resolving disputes) coupled with asymmetric
 *   extraction (from holding institutions to successor states) and active
 *   enforcement (diplomatic pressure, legal challenges).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.65).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.7).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Claim for Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'c6e7c943-08a4-4b12-b351-daa4ecffe83d').
narrative_ontology:cs_kernel_codification('c6e7c943-08a4-4b12-b351-daa4ecffe83d', formalized).
narrative_ontology:cs_authority_grounding('c6e7c943-08a4-4b12-b351-daa4ecffe83d', lineage).
narrative_ontology:cs_interpretation_layer_present('c6e7c943-08a4-4b12-b351-daa4ecffe83d').
narrative_ontology:cs_reading_relation('c6e7c943-08a4-4b12-b351-daa4ecffe83d', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6e7c943-08a4-4b12-b351-daa4ecffe83d', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('c6e7c943-08a4-4b12-b351-daa4ecffe83d', foundational, cultural_property_as_state_sovereignty).
narrative_ontology:cs_axiom_status(cultural_property_as_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c6e7c943-08a4-4b12-b351-daa4ecffe83d', cultural_property_as_state_sovereignty, conventional).
narrative_ontology:cs_axiom('c6e7c943-08a4-4b12-b351-daa4ecffe83d', foundational, colonial_acquisition_as_illegitimate_extraction).
narrative_ontology:cs_axiom_status(colonial_acquisition_as_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c6e7c943-08a4-4b12-b351-daa4ecffe83d', colonial_acquisition_as_illegitimate_extraction, deontological).
narrative_ontology:cs_reference_frame('c6e7c943-08a4-4b12-b351-daa4ecffe83d', post_colonial_international_law_framework).
narrative_ontology:cs_drift_state('c6e7c943-08a4-4b12-b351-daa4ecffe83d', contemporary_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c6e7c943-08a4-4b12-b351-daa4ecffe83d', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim legitimate ownership of cultural artifacts removed during colonial periods, asserting historical continuity with expropriated peoples. They benefit from the return of these items, which enhances national identity and cultural capital, but face significant diplomatic and legal hurdles.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, constrained, national).

% Actively pursue repatriation of artifacts as a matter of national sovereignty and historical justice. They gain symbolic and cultural capital, but the process is costly and often involves protracted negotiations with former colonial powers and holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments, beneficiary,
    institutional, generational, constrained, national).

% Currently possess many artifacts claimed by successor states. They bear the costs of potential repatriation (loss of collection, prestige, visitor numbers) and often argue for universal heritage or their superior preservation capabilities. Their exit options are constrained by legal and ethical pressures.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_museums, payer,
    institutional, generational, constrained, global).

% Hold artifacts acquired through colonial-era markets, facing increasing legal and moral challenges to their ownership. They bear the risk of forced restitution and loss of investment, with limited options to retain contested items as legal frameworks shift.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors, payer,
    powerful, biographical, constrained, global).

% Are the political entities from which holding museums and private collectors often derive their legal claims. They face diplomatic pressure and reputational costs, and may be compelled to facilitate repatriation, bearing the political and financial costs of unwinding historical injustices.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers, payer,
    institutional, generational, constrained, global).

% Often the original creators and stewards of the artifacts, but this reading prioritizes successor states as the legal claimants, potentially sidelining direct indigenous claims. They are excluded from direct negotiation in this framework, despite their deep cultural connection.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, civilizational, identity_locked, local).

% Develop and interpret international conventions and norms regarding cultural property, influencing the legal landscape for repatriation. They mediate disputes and provide frameworks, but their enforcement power is often limited to persuasion and non-binding resolutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal and ethical framework for resolving disputes over cultural property, aiming to coordinate international efforts towards historical justice and cultural restitution, preventing unilateral actions or perpetual disputes.
% TRANSFER_FUNCTION: Transfers legal and physical control of cultural artifacts from holding institutions and former colonial powers to successor states, along with the associated symbolic and cultural capital.
% ABSENT_VOICES: Indigenous communities, whose direct claims to stewardship might be subsumed under the sovereign claims of successor states, are often marginalized in this state-centric framework. They would argue for direct restitution to cultural groups, not just national governments.
% DISAPPEARANCE_RATIONALE: If this legal framework vanished, the international landscape of cultural property would revert to a more chaotic state, with holding institutions facing less pressure, successor states losing a key legal avenue for claims, and a significant increase in unilateral actions or unresolved disputes.
% FOUNDING_PROBLEM: The historical injustice of colonial expropriation of cultural artifacts, leading to their dispersal and retention in former colonial powers, creating ongoing disputes over ownership and cultural heritage.
% FOUNDING_PROBLEM_CORROBORATION: Successor states and post-colonial governments universally attest that the problem is live and ongoing, citing numerous unresolved claims. International legal scholars and human rights organizations corroborate the persistence of this historical injustice, providing evidence from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high, reflecting the significant transfer of cultural and symbolic capital from former colonial powers and holding institutions to successor states, alongside the costs of diplomatic friction and legal battles. Suppression (0.7) is high because the persistence of this claim relies on actively suppressing alternative legal interpretations (e.g., 'universal heritage' as a justification for retention) and resisting the inertia of established collections. Theater ratio (0.2) is low, as the efforts towards repatriation are largely genuine, though some diplomatic gestures might be performative. The increasing extractiveness and suppression over time reflect the growing assertiveness of post-colonial claims and the hardening of legal positions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of successor states, this constraint is a just and necessary mechanism for historical redress. From the perspective of holding museums, it is an extractive demand that threatens their collections and curatorial mission. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states and post-colonial governments are the primary beneficiaries (d near 0.0), gaining cultural and symbolic capital. Holding museums, private collectors, and former colonial powers are the payers/targets (d near 1.0), bearing the costs of potential restitution and loss of prestige. International legal bodies act as agenda-setters, shaping the discourse and legal frameworks. Indigenous communities are structurally excluded in this state-centric reading, despite their deep connection to the artifacts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_indigenous_rights,
    'Does the ''sovereign repatriation'' reading adequately address the rights and claims of indigenous communities, or does it merely transfer authority from one state actor (colonial power) to another (successor state) without fully empowering the original cultural stewards?',
    'Analysis of repatriation outcomes: if artifacts are returned to successor states but not subsequently to indigenous communities, it suggests a structural gap in this reading''s justice claims.',
    'If indigenous claims are systematically sidelined, the effective extraction from indigenous communities (as victims of continued disempowerment) would be higher than currently measured, potentially reclassifying this reading as more extractive or even a Snare from their perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_indigenous_rights, conceptual, 'Ambiguity regarding the ultimate beneficiary of repatriation: state vs. indigenous community.').

omega_variable(
    universal_heritage_legitimacy,
    'To what extent does the ''universal heritage'' argument (that artifacts belong to all humanity and are best preserved in major museums) serve as a genuine coordination function for preservation and access, versus a cover for continued retention by former colonial powers?',
    'Empirical study of preservation standards and access policies in holding institutions versus successor states, coupled with analysis of the historical context of acquisition.',
    'If the ''universal heritage'' argument is found to be primarily a cover, the suppression metric for this ''sovereign repatriation'' reading would be higher, as it actively combats a disingenuous counter-claim. If it has genuine merit, the resistance to repatriation would be more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_legitimacy, empirical, 'The true nature of the ''universal heritage'' counter-claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'cultural_property_legal_corpus' kernel. This 'sovereign repatriation' reading focuses on state-to-state restitution, distinct from 'universal heritage' (global access) and 'indigenous stewardship' (community control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
