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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine in Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint describes the 'universal heritage' reading of
 *   international cultural property law, which posits that cultural artifacts
 *   are the shared patrimony of all humanity and should be preserved and made
 *   accessible by institutions best equipped to do so, regardless of
 *   geographic origin. This reading often serves to legitimize the continued
 *   retention of artifacts by major museums in former colonial powers,
 *   treating repatriation claims from successor states or indigenous
 *   communities as particularist challenges to a universal good. The
 *   constraint is claimed as a Rope by its proponents, but its operation is
 *   substantially extractive and requires active enforcement to maintain the
 *   status quo.
 *
 * KEY AGENTS:
 *   - universalist_museums: Primary agenda-setter (institutional/constrained) — actively resists repatriation.
 *   - international_cultural_institutions: Beneficiary (organized/mobile) — supports universalist framing.
 *   - claimant_successor_states: Primary payer (powerful/constrained) — bears legal/diplomatic costs of repatriation.
 *   - indigenous_communities: Payer (powerless/identity_locked) — marginalized, suffers identity harm.
 *   - international_legal_scholars: Observer (analytical/analytical) — analyzes and influences debate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.78).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.65).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine in Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'cb45e888-fb21-45d3-832a-abe1087f139c').
narrative_ontology:cs_kernel_codification('cb45e888-fb21-45d3-832a-abe1087f139c', formalized).
narrative_ontology:cs_authority_grounding('cb45e888-fb21-45d3-832a-abe1087f139c', lineage).
narrative_ontology:cs_interpretation_layer_present('cb45e888-fb21-45d3-832a-abe1087f139c').
narrative_ontology:cs_reading_relation('cb45e888-fb21-45d3-832a-abe1087f139c', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb45e888-fb21-45d3-832a-abe1087f139c', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('cb45e888-fb21-45d3-832a-abe1087f139c', foundational, cultural_property_as_universal_heritage).
narrative_ontology:cs_axiom_status(cultural_property_as_universal_heritage, holdable).
narrative_ontology:cs_axiom_grounding('cb45e888-fb21-45d3-832a-abe1087f139c', cultural_property_as_universal_heritage, deontological).
narrative_ontology:cs_axiom('cb45e888-fb21-45d3-832a-abe1087f139c', foundational, scientific_preservation_and_access_maximization).
narrative_ontology:cs_axiom_status(scientific_preservation_and_access_maximization, holdable).
narrative_ontology:cs_axiom_grounding('cb45e888-fb21-45d3-832a-abe1087f139c', scientific_preservation_and_access_maximization, instrumental).
narrative_ontology:cs_reference_frame('cb45e888-fb21-45d3-832a-abe1087f139c', post_unesco_1970_convention_framework).
narrative_ontology:cs_drift_state('cb45e888-fb21-45d3-832a-abe1087f139c', contemporary_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb45e888-fb21-45d3-832a-abe1087f139c', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universalist_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_cultural_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_access_principle).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, scientific_preservation_ethic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major museums in former colonial powers that hold vast collections of artifacts from around the world. They assert their role as custodians of 'universal heritage,' providing preservation expertise and public access. They actively resist repatriation claims, often citing the fragility of artifacts or lack of suitable facilities in claimant nations.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universalist_museums, agenda_setter,
    institutional, generational, constrained, global).

% Organizations like UNESCO (in certain capacities) and academic bodies that promote international cooperation in cultural heritage, often aligning with the universalist view that emphasizes preservation and broad access over origin-based ownership. They benefit from the existing distribution of artifacts as it facilitates their research and exhibition programs.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_cultural_institutions, beneficiary,
    organized, generational, mobile, global).

% Post-colonial nations seeking the return of cultural artifacts removed during colonial periods. They bear significant legal and diplomatic costs in pursuing repatriation claims, often facing resistance from holding institutions and legal frameworks that favor current possession. Their claims are often framed as 'particularist' against the 'universal' good.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states, payer,
    powerful, generational, constrained, national).

% Original custodians of many artifacts, for whom these objects often hold deep spiritual and identity-forming significance. Their claims are frequently marginalized or subsumed under state-level claims, and they face immense barriers to asserting their rights within international legal frameworks. The loss of artifacts represents an ongoing identity harm.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    powerless, civilizational, identity_locked, local).

% Academics and legal experts who analyze the evolution of cultural property law, the arguments for and against repatriation, and the impact of different doctrines. They can influence policy debates but do not directly enforce or benefit from the constraint.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, universalist_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the global preservation and accessibility of cultural artifacts by centralizing them in institutions with advanced conservation capabilities and public exhibition spaces, thereby preventing loss, decay, or restricted access.
% TRANSFER_FUNCTION: Transfers de facto ownership and control of cultural artifacts from their places of origin to major international museums and institutions, along with the associated prestige, research opportunities, and economic benefits of exhibition.
% ABSENT_VOICES: The voices of indigenous communities, whose spiritual and cultural connections to artifacts are often not recognized by universalist legal frameworks, are largely absent from the formal international legal discourse. Their claims are often mediated or dismissed by state-level actors or museum authorities.
% DISAPPEARANCE_RATIONALE: If the universal heritage doctrine vanished, the legal and ethical landscape of cultural property would fundamentally shift. Repatriation claims would gain significant legal force, leading to a massive redistribution of artifacts, a re-evaluation of museum collections, and a re-centering of cultural authority towards communities of origin. The global museum system as currently constituted would be forced to radically reorganize.
% FOUNDING_PROBLEM: The problem of cultural heritage being lost, destroyed, or inaccessible due to conflict, neglect, or local instability, particularly in regions lacking resources for preservation or public display.
% FOUNDING_PROBLEM_CORROBORATION: Universalist museums and some international bodies assert the problem is still live, citing ongoing threats to heritage in conflict zones and resource-poor nations. Claimant states and indigenous communities argue that while preservation is important, the 'problem' is now primarily a justification for continued retention, and that local stewardship capacity has grown significantly; independent cultural heritage experts and post-colonial scholars corroborate this shifted-function reading.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because the doctrine enables the continued retention of valuable artifacts, denying claimant states and indigenous communities their cultural and economic benefits. Suppression (0.65) is moderate-high, reflecting the legal and diplomatic barriers faced by claimants, and the active resistance from holding institutions. Theater ratio (0.40) indicates that while preservation and access are real functions, a significant portion of the effort is performative, aimed at justifying the existing distribution against mounting criticism. The claimed type is 'tangled_rope' because it purports a coordination function (universal preservation/access) but operates with clear asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universalist museums, the doctrine is a Rope, coordinating global cultural preservation. From the perspective of claimant states and indigenous communities, it is a Snare, extracting cultural patrimony and identity, maintained by legal and diplomatic suppression. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Universalist museums and international cultural institutions are beneficiaries (low d) as they gain control, prestige, and resources. Claimant successor states and indigenous communities are targets (high d) as they bear the costs of lost heritage, legal battles, and identity harm. Indigenous communities are 'identity_locked' due to the profound, non-negotiable connection to their heritage, making 'exit' from the relationship with their artifacts impossible without severe identity loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (global preservation and access) is increasingly contested. While the original problem of artifact loss was real, the doctrine now functions to maintain the existing power imbalance. The classification as Tangled Rope, rather than a pure Rope, prevents mislabeling by highlighting the asymmetric extraction embedded within the purported coordination function. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict further indicate a potential mandatrophy, where the original justification has atrophied but the structure persists due to concentrated benefits and diffuse costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particular_value,
    'Is the ''universal value'' of cultural artifacts genuinely separable from their ''particularist'' cultural and spiritual significance to communities of origin?',
    'Philosophical and anthropological inquiry into the nature of cultural value, and empirical studies on the impact of repatriation on both universal access and community well-being.',
    'If values are inseparable, the universalist reading''s claim to maximize value is undermined, strengthening repatriation arguments. If separable, the tension between universal access and origin-based stewardship remains a genuine coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particular_value, conceptual, 'Ambiguity in the definition and weighting of ''universal'' vs. ''particular'' cultural value.').

omega_variable(
    preservation_capacity_equity,
    'Do claimant successor states and indigenous communities genuinely lack the capacity for adequate preservation and public access, or is this a narrative used to justify retention?',
    'Independent, transparent audits of preservation infrastructure and expertise in claimant nations, and comparative studies of access rates in repatriated vs. retained collections.',
    'If capacity is sufficient, the ''preservation'' justification for retention weakens significantly, increasing the extractiveness of the universalist doctrine. If capacity is genuinely lacking, the coordination function of holding institutions is partially vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preservation_capacity_equity, empirical, 'Whether preservation capacity arguments are fact-based or rhetorical.').

omega_variable(
    legitimacy_of_colonial_acquisition,
    'To what extent does the universal heritage doctrine implicitly or explicitly legitimize colonial-era acquisitions of cultural property?',
    'Historical-legal analysis of the doctrine''s evolution and its application in cases involving artifacts with contested colonial provenance.',
    'If the doctrine is found to systematically legitimize colonial acquisition, its ethical foundation is severely compromised, increasing its perceived extractiveness and suppression from the perspective of claimant communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_colonial_acquisition, conceptual, 'The doctrine''s relationship to the historical context of artifact acquisition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.1).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'cultural_property_legal_corpus' kernel. Its universalist framing directly influences the legal and diplomatic environment for sovereign repatriation and indigenous stewardship claims, often by framing them as secondary to global access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
