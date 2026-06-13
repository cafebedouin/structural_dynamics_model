% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Reading of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the 'amun_polytheistic_reading' of the
 *   'divine_legitimacy_substrate' kernel in ancient Egypt. Divine legitimacy
 *   for the pharaoh and the state flows through the established Amun
 *   priesthood's interpretation of a multi-deity cosmology, with Amun-Ra as
 *   the chief patron. This system accommodates regional variations in cults
 *   but centralizes interpretive authority and economic power within the
 *   temple complexes, particularly those dedicated to Amun. The pharaoh's
 *   rule is validated by priestly rituals and pronouncements, creating a
 *   reciprocal but often tense relationship where the pharaoh is both
 *   beneficiary and target of the priestly class.
 *
 * KEY AGENTS:
 *   - pharaoh: Payer/Agenda-setter (institutional/constrained) — requires priestly validation, but also controls state resources.
 *   - amun_priesthood: Agenda-setter/Beneficiary (institutional/arbitrage) — interprets divine will, controls vast temple wealth, validates pharaonic rule.
 *   - regional_temple_economies: Beneficiary (organized/mobile) — benefit from state patronage and local offerings, but also subject to central priestly authority.
 *   - common_populace: Payer (powerless/trapped) — contributes labor and offerings to temple system, receives spiritual benefits and social order.
 *   - rival_cults: Excluded (moderate/constrained) — regional cults that are tolerated but not central to state legitimacy, often co-opted or marginalized by the Amun priesthood.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.6).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.7).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Reading of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '039b5c0b-0232-44e1-a000-a490f9a19e30').
narrative_ontology:cs_kernel_codification('039b5c0b-0232-44e1-a000-a490f9a19e30', formalized).
narrative_ontology:cs_authority_grounding('039b5c0b-0232-44e1-a000-a490f9a19e30', lineage).
narrative_ontology:cs_interpretation_layer_present('039b5c0b-0232-44e1-a000-a490f9a19e30').
narrative_ontology:cs_reading_relation('039b5c0b-0232-44e1-a000-a490f9a19e30', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('039b5c0b-0232-44e1-a000-a490f9a19e30', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('039b5c0b-0232-44e1-a000-a490f9a19e30', foundational, multi_deity_cosmic_order).
narrative_ontology:cs_axiom_status(multi_deity_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('039b5c0b-0232-44e1-a000-a490f9a19e30', multi_deity_cosmic_order, theological).
narrative_ontology:cs_axiom('039b5c0b-0232-44e1-a000-a490f9a19e30', foundational, priestly_interpretive_authority).
narrative_ontology:cs_axiom_status(priestly_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('039b5c0b-0232-44e1-a000-a490f9a19e30', priestly_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('039b5c0b-0232-44e1-a000-a490f9a19e30', established_amun_theocracy).
narrative_ontology:cs_drift_state('039b5c0b-0232-44e1-a000-a490f9a19e30', late_new_kingdom_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('039b5c0b-0232-44e1-a000-a490f9a19e30', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, divine_mandate_of_pharaoh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler whose legitimacy is validated by the Amun priesthood. Provides state resources to temples but is also constrained by priestly influence and the need for ritual validation. Bears the cost of maintaining the priestly apparatus.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, agenda_setter).

% The powerful religious elite responsible for interpreting divine will, performing state rituals, and managing vast temple estates. They validate the pharaoh's rule and benefit significantly from the system's operation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, beneficiary).

% Local temple complexes and their associated lands and personnel, which benefit from state patronage, local offerings, and their role in regional religious life. They are subordinate to the central Amun priesthood but maintain significant local influence.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies, beneficiary,
    organized, generational, mobile, regional).

% The majority of the population, who contribute labor, taxes, and offerings to the temple system. They receive spiritual guidance, social order, and access to religious festivals, but have no direct influence over the system's operation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace, payer,
    powerless, immediate, trapped, local).

% Other local or regional religious groups and their priesthoods, whose deities and practices are not central to the state-sanctioned Amun-Ra cosmology. They are often tolerated but lack the state patronage and political influence of the Amun priesthood.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rival_cults, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, divinely sanctioned cosmic and social order, legitimizing the pharaoh's rule and providing a framework for religious practice and social cohesion across a vast kingdom.
% TRANSFER_FUNCTION: Transfers wealth (land, labor, offerings) and political influence from the pharaoh and the common populace to the Amun priesthood and associated temple economies, in exchange for divine validation and maintenance of cosmic order.
% ABSENT_VOICES: Proponents of direct pharaonic divine authority (e.g., future Atenist reformers) or those advocating for purely local, decentralized religious practices would object to the centralized interpretive power and economic extraction of the Amun priesthood. They are absent due to the priesthood's institutional dominance and suppression of alternative theological frameworks.
% DISAPPEARANCE_RATIONALE: If the Amun polytheistic reading and its associated priestly structure vanished overnight, the pharaoh's legitimacy would collapse, leading to political instability, civil unrest, and a fragmentation of religious practice. The state's cosmic and social order would unravel, necessitating a complete reorganization of political and religious authority.
% FOUNDING_PROBLEM: The need to establish a stable, divinely sanctioned basis for pharaonic rule and a unified religious framework for a diverse kingdom, preventing fragmentation and ensuring cosmic harmony.
% FOUNDING_PROBLEM_CORROBORATION: The Amun priesthood consistently attests that the problem of maintaining cosmic order and pharaonic legitimacy is live and requires their ongoing interpretive and ritual work. Historical records of political instability during periods of priestly weakness or challenge corroborate the foundational importance of this system for state stability, even if the degree of priestly extraction is contested by pharaohs and some historians.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a complex religious and political system (legitimizing pharaonic rule, maintaining cosmic order, providing social cohesion) while simultaneously extracting significant resources and power for the Amun priesthood. Extractiveness (0.6) is moderate-high due to the vast wealth accumulated by the temples and the priesthood's influence over state affairs. Suppression (0.7) is high because alternative interpretations of divine legitimacy (e.g., direct pharaonic revelation, rival cults) are actively marginalized or co-opted. Theater ratio (0.4) reflects the genuine religious function alongside the performative aspects of priestly rituals that reinforce their power. The measurements show a gradual increase in extractiveness and suppression over time, indicating a hardening of the priestly power structure.
 *
 * PERSPECTIVAL GAP:
 *   The pharaoh experiences this constraint as a necessary but often burdensome coordination mechanism, providing legitimacy but at the cost of significant resources and political maneuvering with the priesthood. The Amun priesthood experiences it as a beneficial system that secures their power and wealth. The common populace experiences it as a foundational aspect of their world, providing order and spiritual guidance, but also demanding resources and obedience.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood is the primary beneficiary (d=0.0-0.1) due to their control over interpretation and the resulting economic power. The pharaoh is a complex target (d=0.6-0.7): while benefiting from divine legitimacy, they are constrained by the need for priestly validation and the priesthood's independent power base. The common populace are clear targets (d=0.8-0.9) as they bear the costs of the temple system through labor and offerings. Regional temple economies are beneficiaries (d=0.2-0.3) but are also subject to the central Amun priesthood's authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining cosmic order and pharaonic legitimacy) remains 'live' for the duration of the interval, preventing a full mandatrophy resolution. However, the increasing extractiveness and suppression suggest a drift towards the priesthood's self-interest, indicating a potential for future mandatrophy if the coordination function becomes purely a cover for extraction. The 'tangled_rope' classification captures this hybrid nature, preventing mislabeling it as a pure 'rope' (ignoring extraction) or a pure 'snare' (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of the ''divine_legitimacy_substrate'' kernel, or is it merely a specific historical instantiation?',
    'Comparative analysis with other ancient Near Eastern belief systems to identify common structural elements of divine legitimacy, independent of specific pantheons.',
    'If a genuine kernel reading, it highlights the structural persistence of certain legitimacy claims across cultures. If merely an instantiation, its classification is less generalizable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the ''amun_polytheistic_reading'' of the ''divine_legitimacy_substrate'' kernel.').

omega_variable(
    pharaonic_vs_priestly_authority,
    'To what extent does the pharaoh''s divine authority genuinely derive from the gods, versus being mediated and controlled by the Amun priesthood?',
    'Analysis of historical records detailing conflicts between pharaohs and the Amun priesthood, particularly during periods of succession or crisis, to gauge the priesthood''s effective veto power.',
    'If pharaonic authority is heavily mediated, the pharaoh''s directionality shifts further towards ''target'' and the priesthood''s towards ''beneficiary'', strengthening the ''tangled_rope'' classification. If more direct, the pharaoh''s position is more ''agenda_setter''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_vs_priestly_authority, empirical, 'Ambiguity in the true source and control of divine legitimacy.').

omega_variable(
    sibling_reading_impact_atenist,
    'How would the ''atenist_monotheistic_reading'' (divine legitimacy solely through pharaonic revelation of Aten as exclusive deity) structurally alter this constraint?',
    'Historical analysis of the Amarna period: the Atenist reading directly forecloses the Amun polytheistic reading''s core premise of a multi-deity cosmology and distributed priestly interpretation, leading to the suppression of the Amun priesthood and the dismantling of temple economies.',
    'The Atenist reading would shift the constraint from a ''tangled_rope'' (with distributed priestly authority) to a ''snare'' or ''mountain'' (if successfully imposed as natural law) with the pharaoh as the sole agenda-setter and the Amun priesthood as direct victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_atenist, conceptual, 'Impact of the Atenist monotheistic reading on the Amun polytheistic reading.').

omega_variable(
    sibling_reading_impact_folk_syncretistic,
    'How would the ''folk_syncretistic_reading'' (divine legitimacy through household/village ritual practice) structurally alter this constraint?',
    'Anthropological study of popular religious practices in ancient Egypt, comparing official temple doctrines with local cults and household rituals. The folk reading coexists with the Amun polytheistic reading by operating at a different social scale.',
    'The folk syncretistic reading would highlight the distributed nature of religious practice, potentially reducing the perceived ''suppression'' on the common populace by the official temple system, but would not fundamentally alter the ''tangled_rope'' nature of the state-level constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_folk_syncretistic, conceptual, 'Impact of the folk syncretistic reading on the Amun polytheistic reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(divi_su_t50, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_rituals).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, state_resource_allocation_to_temples).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_legitimacy_substrate' kernel, each representing a distinct structural claim about the source and flow of divine authority in ancient Egypt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
