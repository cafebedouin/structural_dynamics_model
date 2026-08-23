% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Article II Non-Appropriation: Commons Conservation Reading
 *   domain: international_law/space_governance
 *
 * SUMMARY:
 *   The commons_conservation reading of Outer Space Treaty Article II holds
 *   that the 'use or occupation' language prohibits not only formal
 *   territorial claims but de facto appropriation through resource
 *   extraction. This reading treats the non-appropriation principle as a wall
 *   constraint: extraction is prohibited absent multilateral authorization,
 *   first-mover mining investments are stranded, non-spacefaring states
 *   preserve a veto over enclosure, and benefits are distributed by
 *   negotiation rather than capability. The reading claims Mountain status —
 *   a fundamental principle of space law — but declares beneficiaries
 *   (non-spacefaring states, future generations) and victims (mining
 *   companies, invested spacefaring states), creating a False Summit Mountain
 *   profile. The measurement series shows rising extractiveness and
 *   suppression as commercial mining pressure mounts, with theater_ratio
 *   increasing as the prohibition's enforcement becomes more performative
 *   relative to its coordination function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.38).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.62).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.38).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, mountain).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Article II Non-Appropriation: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_law/space_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).
domain_priors:emerges_naturally(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '15ec087a-9d2a-464b-a488-fea8a92f7d8e').
narrative_ontology:cs_kernel_codification('15ec087a-9d2a-464b-a488-fea8a92f7d8e', formalized).
narrative_ontology:cs_authority_grounding('15ec087a-9d2a-464b-a488-fea8a92f7d8e', lineage).
narrative_ontology:cs_interpretation_layer_present('15ec087a-9d2a-464b-a488-fea8a92f7d8e').
narrative_ontology:cs_reading_relation('15ec087a-9d2a-464b-a488-fea8a92f7d8e', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('15ec087a-9d2a-464b-a488-fea8a92f7d8e', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('15ec087a-9d2a-464b-a488-fea8a92f7d8e', foundational, extraction_is_appropriation).
narrative_ontology:cs_axiom_status(extraction_is_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('15ec087a-9d2a-464b-a488-fea8a92f7d8e', extraction_is_appropriation, deontological).
narrative_ontology:cs_axiom('15ec087a-9d2a-464b-a488-fea8a92f7d8e', secondary, non_spacefaring_states_hold_veto).
narrative_ontology:cs_axiom_status(non_spacefaring_states_hold_veto, holdable).
narrative_ontology:cs_axiom_grounding('15ec087a-9d2a-464b-a488-fea8a92f7d8e', non_spacefaring_states_hold_veto, conventional).
narrative_ontology:cs_reference_frame('15ec087a-9d2a-464b-a488-fea8a92f7d8e', province_of_all_mankind_principle).
narrative_ontology:cs_drift_state('15ec087a-9d2a-464b-a488-fea8a92f7d8e', contemporary_mining_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15ec087a-9d2a-464b-a488-fea8a92f7d8e', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, humanity_as_whole).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, space_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_investments).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, province_of_all_mankind_principle).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, non_appropriation_as_jus_cogens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over any multilateral authorization regime for resource extraction. Their consent is required for any enclosure of the commons. They bear no enforcement costs and have no alternative forum — their identity as 'non-spacefaring' is constituted by this veto. They benefit from the constraint's preservation of future option value and benefit-sharing negotiation leverage.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    powerless, generational, identity_locked, global).

% Abstract beneficiary of commons preservation — the constraint prevents irreversible enclosure of space resources before they can be governed equitably. No agency, no exit, no voice in current negotiations. Their interest is invoked by non-spacefaring states and civil society actors.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% The 'province of all mankind' principle's nominal beneficiary. The constraint preserves the legal fiction that space belongs to everyone equally. In practice, this beneficiary has no enforcement capacity and its interests are mediated by states. The reading uses this abstraction to justify the veto structure.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, humanity_as_whole, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, humanity_as_whole).

% Have invested billions in extraction technology and regulatory lobbying based on the extraction_permissive reading. The commons_conservation reading strands these investments — they cannot operate legally without multilateral authorization that may never come. Their exit options are near-zero: pivot to other markets (abandoning space-specific IP), litigate (costly, uncertain), or lobby for regime change (political, long-horizon). Identity-locked to the extraction narrative: 'we are the ones who make space resources usable.'
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, space_mining_companies, payer,
    powerful, biographical, trapped, global).

% States (USA, China, Russia, Luxembourg, UAE, Japan) that have enacted domestic space resource laws and subsidized mining ventures. They bear the enforcement costs of the constraint (diplomatic pressure, compliance monitoring) and the opportunity costs of stranded national investments. They retain agenda-setting power through UNCOPUOS and bilateral agreements but are constrained by the treaty text and non-spacefaring state veto. Their exit is constrained: withdrawal from OST is politically costly; reinterpretation is legally contested; regime creation requires non-spacefaring state consent.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_investments, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_investments, agenda_setter).

% Launch providers, satellite operators, space station developers — companies that benefit from legal certainty but are not yet directly affected by extraction rules. They would object to regulatory uncertainty but are not at the negotiating table. Their mobility is higher: they can operate under whatever regime emerges.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, commercial_space_ventures_not_yet_mining, excluded,
    moderate, biographical, mobile, global).

% The multilateral forum where the interpretation contest plays out. Produces working papers, consensus reports, and draft treaty language. Takes testimony from all seats but has no enforcement power. Its analytical seat sees the full structural divergence: the same treaty text generates Mountain, Snare, and Tangled Rope classifications depending on which seat's directionality is centered.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, un_copuos_legal_subcommittee, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral enclosure of space resources by establishing that extraction requires multilateral authorization; solves the collective action problem of competing claims by capability by substituting negotiation for first-mover advantage.
% TRANSFER_FUNCTION: Moves decision authority over resource extraction from first-mover capability to multilateral negotiation; transfers future benefit shares from spacefaring states/companies to non-spacefaring states and humanity-as-whole; strands sunk investments in extraction technology and regulatory capture.
% ABSENT_VOICES: Commercial mining ventures and private space companies not yet at the table (excluded stakeholders); indigenous and Global South communities whose 'common heritage' claims are mediated by states; scientific community that needs extraction for research but is conflated with commercial extraction.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, spacefaring states and companies would begin extraction immediately under domestic law regimes (US Commercial Space Launch Competitiveness Act, Luxembourg Space Resources Law, etc.). The Moon and asteroids would be enclosed by capability within a decade. The multilateral benefit-sharing framework would collapse. Non-spacefaring states would lose their only leverage.
% FOUNDING_PROBLEM: Preventing Cold War-style territorial claims and military competition in outer space; establishing space as a domain for peaceful use by all humanity rather than a sphere of national sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: UNCOPUOS records (1959-1967) and ILC commentaries corroborate the territorial non-appropriation founding problem. The contested status is corroborated by: (1) spacefaring states' domestic laws (US 2015, Luxembourg 2017, UAE 2019, Japan 2021) asserting extraction rights — from outside the commons_conservation beneficiary set; (2) the Moon Agreement's failure to achieve widespread ratification — from outside the extraction_permissive beneficiary set; (3) the ongoing UNCOPUOS 'Building Blocks' process — demonstrating neither reading has achieved consensus.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, ExtMetricName, E),
    domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ost_article_ii_non_appropriation__commons_conservation),
    narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects the constraint's primary coordination function (preventing unilateral enclosure) with moderate extraction from stranded investments and enforcement costs. Suppression (0.62) is significant because the constraint requires active multilateral enforcement against capable actors. Theater ratio (0.22) is low but rising — the coordination function (commons preservation) is genuine but a growing share of enforcement activity defends the veto structure rather than the commons itself. Accessibility collapse (0.82) is high: unilateral extraction is legally foreclosed. Resistance (0.68) is high from spacefaring states and commercial actors. The claimed_type is mountain (the reading's self-presentation) but the metrics and beneficiary structure suggest FSM will reclassify.
 *
 * PERSPECTIVAL GAP:
 *   From the non-spacefaring state seat, the constraint is a Mountain: a natural law preserving the commons. From the mining company seat, it is a Snare: pure extraction stranding their investment. From the spacefaring state seat, it is a Tangled Rope: genuine coordination (preventing conflict) with asymmetric extraction (their investments stranded, their freedom of action constrained). The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states are structural beneficiaries (d ~0.15): they gain veto power and future benefit shares without bearing enforcement costs. Future generations and humanity_as_whole are diffuse beneficiaries (d ~0.2). Space mining companies are full targets (d ~0.95): trapped by sunk investment, identity-locked to extraction narrative, no exit. Spacefaring states with investments are powerful but constrained targets (d ~0.75): they bear enforcement costs and stranded assets but retain some agenda-setting capacity. The commons_conservation reading's authority derives from lineage (treaty interpretation tradition) not extraction — but if authority_grounding shifts to extraction (spacefaring states benefit from freezing the interpretation while developing capabilities), the Mountain claim inverts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Cold War territorial claims in space) is contested: spacefaring states argue the territorial non-appropriation rule solved it; commons_conservation advocates argue resource extraction IS de facto appropriation and the problem persists. The arrangement persists beyond its founding conditions — commercial extraction technology now exists that the drafters did not anticipate. Mandatrophy is unresolved: the constraint's mandate has expanded from 'no flags on the Moon' to 'no mines on the Moon' without formal revision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the prohibition on resource extraction a genuine natural law of space governance (Mountain) or a constructed legal interpretation that benefits identifiable actors (False Summit)?',
    'Track state practice and opinio juris: if spacefaring states consistently treat extraction as permissible without multilateral authorization, the prohibition is constructed; if they treat it as legally impossible, it approaches natural law status.',
    'If constructed, FSM triggers reclassification to tangled_rope — the constraint coordinates multilateral governance but extracts from first-mover investors and concentrates veto power in non-spacefaring states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Whether the commons_conservation reading describes a natural legal fact or a distributional arrangement').

omega_variable(
    extraction_appropriation_boundary,
    'Where exactly does ''use'' become ''appropriation'' — at first extraction, at commercial scale, at infrastructure permanence?',
    'ICJ advisory opinion or UNCOPUOS consensus on the use/appropriation threshold; state practice regarding scientific sampling vs. commercial extraction.',
    'A sharp boundary supports Mountain classification (clear rule); a gradient supports Tangled Rope (coordination function with extraction at the margin).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_appropriation_boundary, conceptual, 'The structural ambiguity in the use-appropriation distinction that drives classification variance').

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s classification change if the kernel''s authority structure shifts from lineage to extraction grounding?',
    'Compare classification outputs when cs_structure.authority_grounding is lineage (current) vs. extraction (spacefaring states benefit from freezing the interpretation).',
    'If authority_grounding is extraction, the reading''s Mountain claim becomes a false summit maintained by the very states it appears to constrain — a structural inversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee-frame structural dependency: this constraint is one reading of ost_article_ii_non_appropriation kernel; sibling readings are extraction_permissive and international_regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_art2_commons_tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost_art2_commons_tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.08).
narrative_ontology:measurement(ost_art2_commons_tr_t1998, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(ost_art2_commons_tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(ost_art2_commons_tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(ost_art2_commons_tr_t2025, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(ost_art2_commons_be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost_art2_commons_be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.22).
narrative_ontology:measurement(ost_art2_commons_be_t1998, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(ost_art2_commons_be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(ost_art2_commons_be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(ost_art2_commons_be_t2025, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ost_art2_commons_su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(ost_art2_commons_su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.42).
narrative_ontology:measurement(ost_art2_commons_su_t1998, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(ost_art2_commons_su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(ost_art2_commons_su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(ost_art2_commons_su_t2025, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.15).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_article_11).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, artemis_accords_section_10).

% DUAL FORMULATION NOTE:
% BGS-style decomposition of the ost_article_ii_non_appropriation kernel: three structurally distinct constraints with different ε values. This reading (commons_conservation) has ε=0.38 (coordination with asymmetric extraction). extraction_permissive has ε≈0.1 (minimal coordination, enables extraction). international_regime has ε≈0.25 (procedural coordination, defers substance). All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, powerful, 0.95).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, institutional, 0.75).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
