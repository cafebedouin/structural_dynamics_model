% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Reading of International Cultural Property Law
 *   domain: international law / cultural property / post-colonial studies
 *
 * SUMMARY:
 *   Cultural artifacts are humanity's shared heritage; legitimate authority
 *   rests with institutions that maximize preservation and universal access
 *   regardless of geographic origin. This reading of the cultural property
 *   legal corpus frames holding institutions as the legitimate custodians of
 *   global heritage and treats repatriation claims by successor states and
 *   indigenous communities as particularist threats to the public good. The
 *   constraint operates through international law, museum ethics codes, and
 *   state-to-state agreements that privilege retention by established
 *   encyclopedic museums. It generates real coordination in preservation and
 *   global access while asymmetrically extracting legal costs, diplomatic
 *   friction, and identity harm from claimant states and source communities.
 *
 * KEY AGENTS:
 *   - universal_museum_network: Primary beneficiary and agenda-setter (institutional/arbitrage) â retains artifacts and sets heritage norms
 *   - source_nation_claimants: Primary payer (organized/constrained) â bears costs of blocked repatriation
 *   - indigenous_source_communities: Secondary payer (powerless/identity_locked) â excluded from legal standing, bears identity harm
 *   - postcolonial_legal_scholars: Analytical observer (analytical/analytical) â documents colonial genealogy of the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.72).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Reading of International Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international law / cultural property / post-colonial studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '65cf1da3-cd8c-4388-8df7-ad62053f5d6e').
narrative_ontology:cs_kernel_codification('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', fixed_text).
narrative_ontology:cs_authority_grounding('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', lineage).
narrative_ontology:cs_interpretation_layer_present('65cf1da3-cd8c-4388-8df7-ad62053f5d6e').
narrative_ontology:cs_reading_relation('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', foundational, cultural_heritage_belongs_to_humanity).
narrative_ontology:cs_axiom_status(cultural_heritage_belongs_to_humanity, holdable).
narrative_ontology:cs_axiom_grounding('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', cultural_heritage_belongs_to_humanity, deontological).
narrative_ontology:cs_axiom('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', foundational, institutional_custody_trumps_origin_title).
narrative_ontology:cs_axiom_status(institutional_custody_trumps_origin_title, holdable).
narrative_ontology:cs_axiom_grounding('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', institutional_custody_trumps_origin_title, conventional).
narrative_ontology:cs_reference_frame('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', universal_preservation_authority).
narrative_ontology:cs_drift_state('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', postcolonial_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65cf1da3-cd8c-4388-8df7-ad62053f5d6e', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_network).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, source_nation_claimants).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_source_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates encyclopedic museums holding artifacts from many cultures. Under the universal heritage framework, they retain objects regardless of geographic origin, citing preservation expertise and public access. They receive funding, prestige, scholarly authority, and tourism revenue from their collections. Their exit from the framework would mean returning contested objects and losing central institutional identity.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_network, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_network, beneficiary).

% Nation-states seeking return of cultural artifacts removed during colonial periods. They encounter legal doctrines treating their claims as particularist threats to universal access. They bear costs of prolonged legal proceedings, diplomatic campaigns, and identity loss from continued absence of heritage objects. Exit options are constrained by the asymmetry of international legal process and the political cost of challenging the universal heritage norm.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_nation_claimants, payer,
    organized, generational, constrained, national).

% Communities whose ancestors created or sacredly held artifacts now in foreign museums. International cultural property law often recognizes only state-level claimants, leaving these communities without standing in the legal framework. Their identity is fused with objects they cannot legally reclaim. Exit is identity_locked because separation from these objects is experienced as cultural amputation, and the legal system does not recognize their independent claim.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_source_communities, payer,
    powerless, generational, identity_locked, local).

% Academic and legal analysts who document the colonial genealogy of universal museum claims and advocate for alternative frameworks. They observe the constraint from outside the benefiting institutions and publish critiques of the international legal corpus.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, postcolonial_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_network).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global framework for preserving culturally significant objects and ensuring universal scholarly and public access, preventing nationalist enclosures from fragmenting the archaeological and aesthetic record.
% TRANSFER_FUNCTION: Moves cultural artifacts and the authority to determine their disposition, display context, and access from source nations and indigenous communities to holding institutions in former colonial powers and the international heritage bodies that validate them.
% ABSENT_VOICES: Indigenous communities whose sacred or communal objects are treated as universal heritage under state-centric international law; source nations whose repatriation claims are framed as particularist threats to the global public good.
% DISAPPEARANCE_RATIONALE: If the universal heritage legal framework vanished, major museums would face immediate repatriation pressure, thousands of contested objects would shift toward claimant states and communities, and the global regime of retention permits would collapse into bilateral and community-based negotiations.
% FOUNDING_PROBLEM: Colonial-era extraction had dispersed cultural objects globally, creating preservation crises in some regions and raising the risk of nationalist destruction or market-driven looting; a universal framework was needed to ensure long-term preservation and scholarly access.
% FOUNDING_PROBLEM_CORROBORATION: Holding institutions and Western governments attest the problem remains live, citing ongoing looting and preservation capacity gaps. Postcolonial scholars and source nations attest the founding problem has shifted: retention is now driven by institutional prestige and tourism revenue, not preservation necessity; the Universal Declaration on Cultural Diversity and various UN reports from outside the benefiting parties document the identity harm and power asymmetry of the current arrangement.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the framework transfers control and revenue potential of cultural objects from source communities to holding institutions while blocking repatriation. Suppression (0.68) reflects the active legal and diplomatic mechanisms that prevent claimant states and communities from achieving returns. Theater_ratio rises to 0.50 because the preservation and access narrative increasingly functions to justify retention rather than to describe the actual primary purpose of the institutions. Accessibility_collapse (0.60) indicates that once the universal heritage frame is accepted, repatriation alternatives are delegitimized as nationalist or divisive. Resistance (0.58) captures growing but institutionally contained pushback from source nations and decolonization movements.
 *
 * PERSPECTIVAL GAP:
 *   The universal_museum_network seat experiences the constraint as a necessary and legitimate coordination mechanism for preserving global heritage. The source_nation_claimants and indigenous_source_communities seats experience the same legal framework as an active barrier to self-determination and cultural restoration. The engine computes this divergence from the structural asymmetry in power, exit options, and declared roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal_museum_network is the structural beneficiary: it receives the objects, the prestige, the tourism revenue, and the scholarly authority. Its directionality is near the beneficiary pole. The source_nation_claimants are victims with constrained exit (international legal process is slow, expensive, and biased toward retention), placing them near the full-target pole. Indigenous_source_communities are even more strongly targeted because their exit is identity_locked â they cannot exit the harm without abandoning a constitutive dimension of communal identity. The postcolonial_legal_scholars seat is analytical and does not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded on a genuine coordination problem â dispersed collections risked destruction and needed preservation infrastructure. However, the mandate has substantially drifted: retention is now driven by museum budgets, tourism economies, and institutional identity rather than by comparative preservation advantage. The founding problem status is contested because holding institutions claim ongoing preservation necessity while external observers note that many source nations and communities now have adequate preservation capacity. This prevents mislabeling the constraint as pure coordination (Rope) because the victim structure is pronounced and active, and prevents mislabeling it as pure extraction (Snare) because the coordination function in preservation and access remains partially real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_kernel_position,
    'This constraint instantiates the universal_heritage_reading of the cultural_property_legal_corpus kernel. The sibling sovereign_repatriation_reading would reverse the beneficiary/victim structure and lower extraction; the indigenous_stewardship_reading would reject the state-centric legal framework entirely. Does the universal heritage reading persist because it structurally coordinates preservation, or because it locks in the power of existing holding institutions?',
    'Comparative analysis of repatriation outcomes versus retention outcomes for object preservation and community wellbeing; tracing of museum budget dependencies on retained collections.',
    'If repatriated objects are equally well-preserved and communities report reduced identity harm, the universal heritage reading''s coordination story weakens and extraction dominates; if retention demonstrably outperforms on preservation, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_kernel_position, conceptual, 'Whether the universal heritage reading''s persistence is coordination or capture').

omega_variable(
    preservation_outcome_asymmetry,
    'Does retention under the universal heritage framework produce measurably better preservation and access outcomes than repatriation to source nations or indigenous stewardship?',
    'Systematic comparative study of object condition, display access, and community wellbeing across repatriated and retained collections, controlling for funding disparities.',
    'If outcomes are equivalent or favor repatriation, the coordination story weakens and the constraint shifts toward pure extraction; if retention demonstrably outperforms, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_outcome_asymmetry, empirical, 'Whether universal retention produces better outcomes than alternatives').

omega_variable(
    state_centrist_exclusion,
    'Does the state-centric structure of international cultural property law inherently exclude indigenous communities whose claims precede or exceed the nation-state framework?',
    'Comparative legal analysis of standing rules across international tribunals and domestic repatriation statutes; tracking of indigenous community participation rates in heritage claims.',
    'If indigenous communities are structurally excluded, the victim set is larger than the legal framework acknowledges, increasing effective extraction from powerless actors; if recent legal evolution has opened standing, the asymmetry is narrowing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_centrist_exclusion, conceptual, 'Whether international law''s state-centrism excludes indigenous claimants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cult_tr_t12, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(cult_tr_t36, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement(cult_tr_t48, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 48, 0.47).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.5).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cult_be_t12, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(cult_be_t36, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 36, 0.63).
narrative_ontology:measurement(cult_be_t48, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cult_su_t12, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(cult_su_t36, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement(cult_su_t48, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 48, 0.66).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the cultural_property_legal_corpus kernel. It shares the same referent (the international cultural property legal framework) with its siblings but authors a distinct epsilon, beneficiary structure, and classification due to the different normative premises of the universal heritage reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
