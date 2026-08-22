% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: OST Article II Non-Appropriation: Commons Conservation Reading
 *   domain: international/space_law/treaty_interpretation
 *
 * SUMMARY:
 *   The Outer Space Treaty's Article II prohibits national appropriation 'by
 *   claim of sovereignty, by means of use or occupation, or by any other
 *   means.' The commons conservation reading interprets 'use or occupation'
 *   as barring de facto appropriation through resource extraction: if a
 *   private actor extracts resources under a national license, the state has
 *   effectively appropriated the resource deposit through use. This reading
 *   treats the non-appropriation principle as a wall constraint — extraction
 *   is prohibited absent a multilateral authorization regime that implements
 *   benefit-sharing (Common Heritage of Mankind). The constraint's extraction
 *   is low (0.22) because it primarily blocks future extraction rather than
 *   extracting from current activity; its suppression (0.35) reflects the
 *   diplomatic and legal pressure needed to maintain the interpretation
 *   against first-mover challenges. Theater ratio (0.18) is rising as the gap
 *   between the conservation reading's legal force and physical enforcement
 *   capacity widens — no international regime exists yet to actually
 *   administer the multilateral authorization the reading requires.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.22).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.35).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.22).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international/space_law/treaty_interpretation").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'aca0daa2-ba95-43dc-85da-e6ee0a035f7e').
narrative_ontology:cs_kernel_codification('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', fixed_text).
narrative_ontology:cs_authority_grounding('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('aca0daa2-ba95-43dc-85da-e6ee0a035f7e').
narrative_ontology:cs_reading_relation('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', foundational, extraction_equals_appropriation).
narrative_ontology:cs_axiom_status(extraction_equals_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', extraction_equals_appropriation, conventional).
narrative_ontology:cs_axiom('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', foundational, common_heritage_requires_multilateral_authorization).
narrative_ontology:cs_axiom_status(common_heritage_requires_multilateral_authorization, holdable).
narrative_ontology:cs_axiom_grounding('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', common_heritage_requires_multilateral_authorization, conventional).
narrative_ontology:cs_axiom('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', secondary, benefit_sharing_is_non_waivable).
narrative_ontology:cs_axiom_status(benefit_sharing_is_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', benefit_sharing_is_non_waivable, conventional).
narrative_ontology:cs_reference_frame('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', ost_1967_originalist_non_appropriation).
narrative_ontology:cs_drift_state('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', post_artemis_accords_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aca0daa2-ba95-43dc-85da-e6ee0a035f7e', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, developing_nations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, international_seabed_authority_analogue).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_ventures).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_state_agencies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, private_space_resource_companies).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, non_appropriation_principle_universal).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, multilateral_authorization_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserve their veto over enclosure of the space commons through multilateral authorization requirements. They cannot access space resources independently but hold structural power through UNCOPUOS consensus rules and the Common Heritage principle. Exit from this constraint would mean accepting unilateral appropriation by spacefaring powers — a worse outcome for them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% Benefit from the benefit-sharing and technology-transfer commitments that accompany the conservation reading. Their leverage comes from numerical majority in UN General Assembly and the Moon Agreement ratification bloc. Exit means losing the only legal framework that guarantees them a share of space wealth.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, developing_nations, beneficiary,
    moderate, generational, constrained, global).

% Have invested billions in prospecting and extraction technology predicated on the extraction-permissive reading. The conservation reading strands these investments unless they secure multilateral authorization — which requires benefit-sharing they view as confiscatory. They lobby for the extraction-permissive reading and fund legal challenges to the conservation interpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_ventures, payer,
    powerful, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_ventures, excluded).

% NASA, CNSA, Roscosmos, ESA, ISRO — they fund the infrastructure that makes extraction physically possible. Under the conservation reading, they must negotiate benefit-sharing and accept multilateral oversight rather than claim national prerogative. Their exit option is withdrawal from the OST regime (Article XVI), but that forfeits the treaty's other protections (liability regime, registration, rescue obligations).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_state_agencies, payer,
    institutional, generational, constrained, global).

% Planetary Resources, Deep Space Industries (historical), newer ventures — they need legal certainty to raise capital. The conservation reading makes their business model dependent on an international licensing regime that does not yet exist. They cannot exit to another jurisdiction because space activities are attributed to their state of registry (OST Article VI).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, private_space_resource_companies, payer,
    organized, biographical, trapped, global).

% The not-yet-existent 'International Space Resource Authority' that would administer multilateral authorization and benefit distribution under the conservation reading. Its design is contested: Moon Agreement states want a strong ISA-style body; spacefaring states prefer a lightweight coordination mechanism. This stakeholder is the institutional form the constraint's enforcement would take.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_seabed_authority_analogue, agenda_setter,
    institutional, generational, analytical, global).

% The conservation reading treats space resources as a trust for all humanity across time. This non-agent stakeholder represents the intergenerational equity claim: that first-mover extraction without multilateral consent depletes the commons irreversibly. They have no voice in current negotiations but are the named beneficiaries of the Common Heritage principle.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations).

% The epistemic community that produces the treaty interpretations courts and diplomats cite. They are split between the three readings but the conservation reading has majority support in recent UNCOPUOS working group reports and ICJ advisory opinion requests. Their situation: they interpret; they do not decide.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, legal_scholars_interpretive_community, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a first-mover race to enclosure that would allocate the space commons by capability rather than negotiation. Solves the collective action problem where every spacefaring state has an incentive to claim resources unilaterally, but all would be worse off in a Hobbesian scramble that destroys the legal order enabling their activities.
% TRANSFER_FUNCTION: Moves the right to authorize extraction from unilateral state/private decision to a multilateral process. Transfers economic rent from first-mover extractors to a benefit-sharing mechanism for non-spacefaring states and future generations. Transfers regulatory authority from national licensing to an international regime.
% ABSENT_VOICES: Commercial space mining workers and supply-chain communities who would gain employment from extraction but have no seat in treaty interpretation. Indigenous communities whose cosmologies may treat celestial bodies as sacred rather than resources — excluded from the Western legal framework entirely. Small island developing states who see space resource governance as precedent for deep-seabed and climate governance but lack diplomatic capacity to engage deeply.
% DISAPPEARANCE_RATIONALE: If the conservation reading vanished overnight, the extraction-permissive reading would likely become de facto practice: first movers would begin commercial extraction under national licenses, the Moon Agreement would be effectively dead, and non-spacefaring states would lose their only legal lever for benefit-sharing. The space legal order would shift from a commons governance regime to a capability-based allocation regime.
% FOUNDING_PROBLEM: The 1967 Outer Space Treaty was negotiated when space resource extraction was science fiction. The founding problem was preventing Cold War territorial claims in orbit and on celestial bodies. The non-appropriation principle (Article II) was designed to keep space as a domain of peaceful cooperation, not national sovereignty. The conservation reading extends this logic to resource extraction: if you cannot claim the territory, you cannot claim what comes out of it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing territorial claims) is corroborated by the OST negotiating history (UNCOPUOS records, US/USSR statements) — sources outside the beneficiary set. The extension to resource extraction is contested: spacefaring states argue the founding problem was only about sovereignty, not property in extracted resources; non-spacefaring states and the Moon Agreement parties argue the founding problem was always about preventing any unilateral appropriation of the commons. No single corroborating source settles the extension.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).
:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The conservation reading is claimed as a rope: it solves a genuine coordination problem (preventing a destructive enclosure race) with minimal coercive overhead. The beneficiaries (non-spacefaring states, future generations) are net beneficiaries; the payers (mining ventures, spacefaring states) bear costs but gain legal certainty and conflict avoidance. However, the constraint requires active enforcement (multilateral authorization regime) that does not yet exist — this pushes it toward tangled_rope if the regime never materializes. The metrics capture the current state: low extractiveness because little extraction is happening yet; rising theater because the legal regime is unenforced; moderate suppression because the constraint operates through diplomatic consensus rather than force.
 *
 * PERSPECTIVAL GAP:
 *   From the non-spacefaring state seat, this is a pure rope: they get a veto they would not otherwise have, and the coordination problem (enclosure race) is real. From the mining venture seat, this is a snare: the coordination story is cover for preserving a status quo where they bear all the risk and get none of the reward. From the spacefaring state seat, it is a tangled rope: they genuinely want to avoid a destructive race (coordination) but they also want to preserve national prerogative (extraction). The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and developing nations are structural beneficiaries (d ~ 0.15): they gain veto power and benefit-sharing claims without bearing extraction costs. First-mover mining ventures and private companies are full targets (d ~ 0.9): their entire business model is blocked, they are trapped by OST Article VI attribution, and they have no arbitrage exit. Spacefaring state agencies sit at d ~ 0.6: they lose unilateral licensing authority but gain a stable legal order and avoided conflict. The international authority analogue is the agenda-setter (d ~ 0.3): it would administer the regime but its design is contested. Future generations are analytical beneficiaries (d ~ 0.05): they cannot act but the constraint's logic treats them as the ultimate beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The OST's non-appropriation principle has not suffered mandatrophy — the founding problem (preventing territorial claims in space) remains live and the constraint's function has expanded rather than atrophied. However, the conservation reading faces a different risk: if the multilateral regime never materializes, the constraint becomes a piton — a performative prohibition with no enforcement mechanism, maintained only because no one has the capacity to violate it at scale yet. The rising theater ratio tracks this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_or_occupation_scope,
    'Does Article II''s phrase ''by means of use or occupation'' structurally encompass resource extraction, or only territorial settlement/administration?',
    'Authoritative interpretation by ICJ or UNCOPUOS consensus; failing that, state practice (national licensing laws for space mining) will crystallize a customary interpretation.',
    'If ''use or occupation'' covers extraction, the conservation reading is the only coherent reading — the extraction_permissive reading is foreclosed. If it covers only territorial administration, the extraction_permissive reading becomes plausible and the conservation reading becomes a policy preference, not a treaty mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_or_occupation_scope, conceptual, 'The textual scope of Article II''s appropriation ban — the core interpretive ambiguity.').

omega_variable(
    common_heritage_operationalization,
    'Can the Common Heritage of Mankind principle be operationalized into a functioning multilateral authorization regime without the participation of major spacefaring states?',
    'The Moon Agreement (1979) attempted this and failed to attract spacefaring state ratification. A new regime would need either their participation or a mechanism to bind them through customary law.',
    'If operationalization requires spacefaring state consent, the conservation reading is a snare for non-spacefaring states: they have a veto but no enforcement. If it can proceed without them (e.g., through UNGA resolution + customary law), the conservation reading remains a rope with real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_heritage_operationalization, empirical, 'Whether the conservation reading''s coordination mechanism can function without the parties it constrains most.').

omega_variable(
    first_mover_investment_stranding,
    'How much capital has already been committed to space resource extraction under the extraction-permissive reading, and does stranding it constitute a taking requiring compensation?',
    'Investment arbitration (ICSID, national courts) under BITs or domestic investment laws; diplomatic claims by home states.',
    'If stranding triggers compensation obligations, the conservation reading''s extractiveness rises (states pay for the constraint). If stranding is treated as regulatory risk, extractiveness stays low. This determines whether the constraint is a rope (low extraction) or tangled_rope (asymmetric extraction from investors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_mover_investment_stranding, empirical, 'The sunk-cost reality of the extraction-permissive bet and its legal consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.02).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.05).
narrative_ontology:measurement(ost__tr_t1998, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.05).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.08).
narrative_ontology:measurement(ost__be_t1998, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1998, 0.12).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.2).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.15).
narrative_ontology:measurement(ost__su_t1998, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_vi_state_responsibility).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_article_11_common_heritage).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, artemis_accords_section_10_space_resources).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, national_space_mining_laws_us_luxembourg_uae_japan).

% DUAL FORMULATION NOTE:
% This constraint is the commons_conservation reading of the ost_article_ii_non_appropriation kernel. The extraction_permissive reading (ost_article_ii_non_appropriation__extraction_permissive) and international_regime reading (ost_article_ii_non_appropriation__international_regime) are sibling constraints. The three readings share the same treaty text but instantiate different constraints with different beneficiary/victim structures, different ε values, and different coordination/extraction profiles. The kernel's ambiguity is not resolvable by textual analysis alone — it requires political resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, institutional, 0.3).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, powerful, 0.85).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
