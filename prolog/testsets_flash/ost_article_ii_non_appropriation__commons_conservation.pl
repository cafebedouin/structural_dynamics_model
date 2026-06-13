% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Outer Space Treaty Article II Non-Appropriation: Commons Conservation Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'commons conservation' reading of Outer
 *   Space Treaty (OST) Article II, which interprets the 'not subject to
 *   national appropriation by claim of sovereignty, by means of use or
 *   occupation, or by any other means' language as prohibiting de facto
 *   appropriation of space resources, including through extraction by states
 *   or private actors. This reading aims to preserve space as a global
 *   commons, requiring multilateral authorization for resource activities. It
 *   is a 'wall' constraint, designed to prevent enclosure and benefit
 *   non-spacefaring states and future generations, while imposing significant
 *   costs on first-mover mining companies and spacefaring nations with
 *   commercial interests.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: Primary beneficiary (organized/constrained)
 *   - first_mover_space_mining_companies: Primary payer (powerful/constrained)
 *   - spacefaring_nations_with_mining_interests: Payer (institutional/constrained)
 *   - environmental_advocates: Beneficiary (moderate/mobile)
 *   - future_generations: Beneficiary (powerless/trapped)
 *   - international_legal_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.8).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.8).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, snare).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Outer Space Treaty Article II Non-Appropriation: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '7af8bca5-9b4b-4b93-8c1b-53587af7ea81').
narrative_ontology:cs_kernel_codification('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', fixed_text).
narrative_ontology:cs_authority_grounding('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', lineage).
narrative_ontology:cs_interpretation_layer_present('7af8bca5-9b4b-4b93-8c1b-53587af7ea81').
narrative_ontology:cs_reading_relation('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', foundational, space_resources_as_global_commons).
narrative_ontology:cs_axiom_status(space_resources_as_global_commons, holdable).
narrative_ontology:cs_axiom_grounding('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', space_resources_as_global_commons, deontological).
narrative_ontology:cs_axiom('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', foundational, de_facto_appropriation_prohibited).
narrative_ontology:cs_axiom_status(de_facto_appropriation_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', de_facto_appropriation_prohibited, conventional).
narrative_ontology:cs_reference_frame('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', original_treaty_intent_prevent_enclosure).
narrative_ontology:cs_drift_state('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', contemporary_space_mining_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7af8bca5-9b4b-4b93-8c1b-53587af7ea81', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, environmental_advocates).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_space_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_nations_with_mining_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the conservation reading as it prevents powerful spacefaring nations and private entities from unilaterally claiming space resources, preserving a potential future share for all. Their power lies in collective diplomatic action and vetoing new regimes.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% These companies bear the cost of this reading as it directly prohibits their planned resource extraction activities without multilateral authorization, stranding significant investments in prospecting and technology development. Their exit options are limited to lobbying for a different interpretation or operating in legal gray areas.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_space_mining_companies, payer,
    powerful, biographical, constrained, global).

% These nations, often supporting their domestic space mining industries, find their ambitions for unilateral resource exploitation curtailed by this reading. They face diplomatic pressure and potential legal challenges if they proceed without international consensus.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_nations_with_mining_interests, payer,
    institutional, generational, constrained, global).

% Advocates for the preservation of space as a global commons, free from unchecked commercial exploitation, benefit from this reading. They actively promote this interpretation in international forums and public discourse.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, environmental_advocates, beneficiary,
    moderate, generational, mobile, global).

% As a non-agent entity, future generations are conceptually protected by this reading, which aims to preserve space resources for their equitable benefit and prevent irreversible environmental damage from early exploitation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% These scholars analyze and debate the various interpretations of Article II, providing academic arguments for and against the commons conservation reading. Their work influences diplomatic positions and judicial interpretations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the use of outer space resources by preventing unilateral appropriation, thereby preserving space as a global commons and ensuring equitable access for all states, particularly non-spacefaring ones.
% TRANSFER_FUNCTION: Transfers the right to unilaterally exploit space resources from technologically advanced states and private entities to a collective, multilateral decision-making process, effectively stranding first-mover investments and preserving a 'veto' for non-spacefaring states.
% ABSENT_VOICES: Entities advocating for a 'first-come, first-served' approach to space resources, or those who believe private property rights should extend to extracted resources, are largely excluded from the dominant discourse of this reading. They would argue for a more permissive interpretation of Article II.
% DISAPPEARANCE_RATIONALE: If this reading vanished, spacefaring nations and private companies would likely proceed with unilateral resource extraction, leading to rapid enclosure of valuable lunar and asteroid resources, potential conflicts over claims, and a significant shift in the geopolitical balance of power in space.
% FOUNDING_PROBLEM: The original Outer Space Treaty aimed to prevent a new 'scramble for Africa' in space, ensuring that outer space remained the 'province of all mankind' and preventing national appropriation of celestial bodies.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states and many international legal scholars continue to assert that the problem of potential unilateral appropriation and enclosure of space resources is very much alive, especially with advancing space mining technologies. This is corroborated by ongoing debates in the UN Committee on the Peaceful Uses of Outer Space (COPUOS) and academic literature from diverse nations.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.8) is high because this reading effectively 'strands' significant investments and future profits for space mining entities, preventing them from realizing gains from unilateral action. Suppression (0.7) is also high, as it requires active diplomatic and legal enforcement to prevent states from authorizing private appropriation or engaging in it themselves. Resistance (0.85) is very high, reflecting the strong opposition from spacefaring nations and commercial interests who advocate for more permissive interpretations. Accessibility collapse (0.6) is moderate, as while unilateral appropriation is blocked, the possibility of a future multilateral regime for resource sharing remains open. Theater ratio (0.2) is low, as the conservation efforts are genuine, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-spacefaring states and environmental advocates, this reading is a crucial 'rope' or even a 'mountain' that protects the global commons. However, from the perspective of space mining companies and their sponsoring nations, it operates as a 'snare' that traps their investments and suppresses their economic activities. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and environmental advocates are beneficiaries (low d) as this reading protects their interests in equitable access and conservation. First-mover space mining companies and spacefaring nations with mining interests are victims/targets (high d) as their unilateral extraction plans are directly blocked. Future generations are a conceptual beneficiary. International legal scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by actively reasserting the original intent of the OST to prevent enclosure, even as technological advancements create new pressures for appropriation. It ensures the 'global commons' mandate remains live and actively defended against commercial pressures, rather than atrophying into a mere theatrical performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_appropriation_definition,
    'What constitutes ''de facto appropriation'' via resource extraction? Is the act of extraction itself appropriation, or only the claim of exclusive ownership over the extracted resources?',
    'International legal precedent or an authoritative interpretation by the UN General Assembly or a specialized body. The current ambiguity allows for different national legal interpretations.',
    'A narrow definition (only ownership claim) would weaken this reading, allowing extraction to proceed. A broad definition (extraction = appropriation) would strengthen it, making it a more effective ''wall'' constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_appropriation_definition, conceptual, 'Ambiguity in what constitutes ''appropriation'' under Article II.').

omega_variable(
    private_actor_applicability,
    'Does Article II''s prohibition on ''national appropriation'' directly apply to private actors, or only to states? If only to states, how do states prevent private appropriation under their jurisdiction?',
    'Clarifying international legal instrument or a ruling by the International Court of Justice. National space laws currently vary in their interpretation.',
    'If Article II does not directly apply to private actors, this reading''s ''wall'' effect is weakened, requiring states to enact domestic laws to prevent private appropriation, which may be inconsistent. If it does, the constraint is stronger and more universally applicable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_applicability, conceptual, 'Whether Article II''s non-appropriation principle extends to private actors.').

omega_variable(
    multilateral_authorization_feasibility,
    'Is a multilateral authorization regime for space resource extraction politically and practically feasible, given the divergent interests of spacefaring and non-spacefaring nations?',
    'Progress in ongoing UN COPUOS negotiations or the successful establishment of a functional international regime for space resources.',
    'If such a regime proves infeasible, this reading effectively becomes a permanent prohibition on extraction, increasing its ''snare'' characteristics for mining interests. If feasible, it transitions towards a ''tangled rope'' or ''rope'' as a coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_authorization_feasibility, empirical, 'Feasibility of a multilateral regime for space resource authorization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Outer Space Treaty's Article II non-appropriation kernel. This 'commons_conservation' reading interprets Article II as prohibiting de facto appropriation via resource extraction by both states and private actors, aiming to preserve space as a global commons. It differs from the 'extraction_permissive' reading (which allows private ownership of extracted resources) and the 'international_regime' reading (which defers the appropriation question to a future multilateral framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
