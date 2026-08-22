% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaties as Completed Extinguishment Transactions
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint story models the 'extinguishment reading' of historical
 *   treaties between Indigenous nations and settler states (primarily
 *   British/Crown and post-confederation Canada, USA, New Zealand,
 *   Australia). Under this reading, treaties are completed property
 *   transactions: Indigenous parties voluntarily ceded territorial
 *   sovereignty to the Crown in exchange for defined reserve lands, annuity
 *   payments, and specific harvesting rights. The settler state becomes the
 *   sole legitimate authority over the ceded territory; Indigenous
 *   jurisdiction is extinguished except for narrowly defined treaty rights.
 *   This reading has been the operative legal framework for Crown sovereignty
 *   assertion and resource allocation for 150-200 years. The constraint is
 *   claimed as a snare — high extraction from Indigenous nations (loss of
 *   territorial jurisdiction, resources, governance) with active enforcement
 *   (Indian Act, residential schools, resource licensing on ceded lands) and
 *   theatrical performance of treaty obligations (annuities frozen at
 *   19th-century values, consultation duties narrowly interpreted). The
 *   kernel contest: sibling readings (stewardship_reading,
 *   nation_to_nation_reading) deny extinguishment occurred or deny its
 *   legitimacy.
 *
 * KEY AGENTS:
 *   - settler_state_crown: Primary agenda_setter and beneficiary (institutional/arbitrage) — asserts sovereignty, controls enforcement, collects resource rents
 *   - colonial_land_speculators: Secondary beneficiary (organized/arbitrage) — acquire ceded lands at below-market prices via state grant
 *   - resource_extraction_corporations: Tertiary beneficiary (powerful/arbitrage) — gain secure tenure for extraction on ceded territory
 *   - indigenous_nations_signatories: Primary victim (powerless/trapped) — ceded territory under duress, lost jurisdiction, confined to reserves
 *   - descendant_indigenous_communities: Ongoing victim (organized/identity_locked) — bear continuing extraction, limited exit from treaty framework
 *   - international_human_rights_bodies: Observer (institutional/analytical) — monitor compliance with UNDRIP, CERD; no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.82).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.78).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaties as Completed Extinguishment Transactions").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '87c1b4f4-2866-4c99-93bf-31d967c9e6c7').
narrative_ontology:cs_kernel_codification('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', formalized).
narrative_ontology:cs_authority_grounding('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', extraction).
narrative_ontology:cs_interpretation_layer_present('87c1b4f4-2866-4c99-93bf-31d967c9e6c7').
narrative_ontology:cs_reading_relation('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', foundational, treaty_cession_extinguishes_sovereignty).
narrative_ontology:cs_axiom_status(treaty_cession_extinguishes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', treaty_cession_extinguishes_sovereignty, conventional).
narrative_ontology:cs_axiom('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', foundational, crown_sovereignty_derives_from_treaty_cession).
narrative_ontology:cs_axiom_status(crown_sovereignty_derives_from_treaty_cession, holdable).
narrative_ontology:cs_axiom_grounding('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', crown_sovereignty_derives_from_treaty_cession, conventional).
narrative_ontology:cs_reference_frame('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', treaty_as_bilateral_contract).
narrative_ontology:cs_drift_state('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', contemporary_undrip_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87c1b4f4-2866-4c99-93bf-31d967c9e6c7', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_crown).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, colonial_land_speculators).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_corporations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations_signatories).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, descendant_indigenous_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, descendant_indigenous_communities).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, state_sovereignty_derivation_from_cession).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, treaty_as_bilateral_contract).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, property_law_applicability_to_sovereignty_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts sovereignty over ceded territory through legislative and judicial authority. Controls the treaty implementation framework, resource licensing, and the definition of treaty rights. Collects virtually all resource rents from ceded lands. Can unilaterally amend implementation (e.g., via legislation) but faces political and international reputational costs. Exit from the constraint would mean recognizing Indigenous jurisdiction — existentially costly to state legitimacy.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_crown, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state_crown, beneficiary).

% Acquired ceded lands through Crown grants at nominal prices. Their property titles derive from the extinguishment reading's validity. Capital mobility gives them arbitrage exit — they can sell and reinvest elsewhere — but their historical gains are locked in. They lobby to maintain the legal framework that secures their titles.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, colonial_land_speculators, beneficiary,
    organized, biographical, arbitrage, regional).

% Hold licenses for mining, forestry, hydroelectric development on ceded territory. The extinguishment reading gives them secure tenure and predictable regulation. They benefit from the state's exclusive licensing authority. Capital mobility provides arbitrage exit, but sunk infrastructure investment creates de facto lock-in. They fund legal challenges to Indigenous title claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_corporations, beneficiary,
    powerful, biographical, arbitrage, continental).

% Original treaty signatories who negotiated under conditions of epidemic depopulation, military pressure, and deliberate mistranslation. Ceded vast territories for reserves ~0.1-1% of original territory, fixed annuities, and verbal promises of continued hunting/fishing. Lost governing jurisdiction over ceded lands. No exit: reserves are legally defined, small, and economically non-viable; return to traditional governance is legally foreclosed.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations_signatories, payer,
    powerless, generational, trapped, local).

% Bear ongoing extraction: resource revenue flows out, environmental degradation stays in, governance authority denied. Receive annuity payments (frozen at $4-5/year) and limited reserve lands — narrow benefits that the extinguishment reading treats as the full treaty bargain. Identity is fused to specific territories; exit from the constraint means abandoning the land that constitutes their nationhood. Some communities leverage court victories for incremental gains (consultation, revenue sharing) but the extinguishment framework remains intact.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, descendant_indigenous_communities, payer,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, descendant_indigenous_communities, beneficiary).

% UN treaty bodies (CERD, HRC), UNDRIP monitoring mechanisms, IACHR. Issue concluding observations and rulings finding extinguishment-based frameworks violate Indigenous rights. No enforcement power; states treat findings as non-binding. Their analytical exit is total — they observe and report but cannot alter the constraint's operation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a legal mechanism for Crown to assert sovereignty over territory without continuous warfare; established a framework for allocating land to settlers and resources to corporations with state-backed title certainty.
% TRANSFER_FUNCTION: Moves territorial jurisdiction, resource rights, and governing authority from Indigenous nations to the settler state and its licensees. Moves fixed annuity payments and small reserve lands from state to Indigenous communities — a fraction of 1% of the resource value extracted.
% ABSENT_VOICES: Indigenous nations who refused to sign treaties (e.g., many West Coast nations, Lubicon Cree) — they never consented to extinguishment but are subject to the same sovereignty assertion. Pre-contact Indigenous legal orders — their jurisdiction is rendered invisible by the extinguishment framework. Future generations of Indigenous peoples — bound by treaties they had no part in negotiating.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished overnight, Crown title to 90%+ of Canadian landmass would lose its primary legal foundation. Resource licenses would face immediate Indigenous jurisdiction challenges. The state would need to negotiate new coexistence frameworks (likely resembling the stewardship or nation_to_nation readings). The entire property law edifice in ceded territories would reorganize.
% FOUNDING_PROBLEM: Crown needed to acquire Indigenous territory for settlement and resource extraction without endless warfare; Indigenous nations needed to survive existential threats (epidemics, starvation, military pressure) and secure some protected land and resources.
% FOUNDING_PROBLEM_CORROBORATION: Royal Commission on Aboriginal Peoples (1996) — independent federal commission — found treaties were not 'real estate deals' but nation-to-nation agreements; the extinguishment interpretation is a Crown imposition. Truth and Reconciliation Commission (2015) — independent — documented how the founding problem (coexistence) was replaced by elimination policy. Supreme Court of Canada (Delgamuukw 1997, Tsilhqot'in 2014) — judicial branch outside executive beneficiaries — rejected extinguishment as a legal fact, affirming Indigenous title persists. Indigenous oral histories across treaty territories consistently deny sovereignty cession.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) reflects near-total transfer of territorial resource value to settler state and its licensees, with annuity payments frozen at nominal 19th-century levels (e.g., $4-5/year per person). Suppression (0.78) reflects active enforcement: Indian Act governance replacement, pass systems, residential schools, criminalization of Indigenous legal orders, resource licensing without consent. Theater ratio (0.45) reflects performance of treaty obligations (annuity payments, consultation rituals) while substantive jurisdiction and revenue sharing are denied. Accessibility collapse (0.85) reflects legal foreclosure of alternatives: courts treat extinguishment as settled law; Indigenous legal orders cannot operate on ceded territory. Resistance (0.62) reflects ongoing litigation, land defense, political mobilization, and international advocacy — significant but unable to shift the constraint's core operation. Metrics measured at T=200 (present); time grid spans ~1800-present.
 *
 * PERSPECTIVAL GAP:
 *   From the settler state seat (agenda_setter, institutional, arbitrage exit), the constraint appears as legitimate coordination: a bilateral contract that settled title and enabled orderly settlement. From Indigenous nation seats (victims, powerless/organized, trapped/identity_locked), the same structure operates as enforced extraction: territory and jurisdiction taken, promises minimized, alternatives suppressed. The engine computes this divergence from the structural data — the authored claim (snare) reflects the victim-seat reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler state (Crown) is structural beneficiary: collects resource rents, controls legislative agenda, holds arbitrage-grade exit (can unilaterally modify treaty implementation via legislation). Colonial speculators and resource corporations are secondary beneficiaries with arbitrage exit (capital mobility). Indigenous signatories and descendants are victims: powerless to organized power, trapped to identity-locked exit (territorial jurisdiction cannot be regained within the framework; identity fused to specific lands makes exit from the constraint existentially costly). International bodies are observers with analytical exit. Directionality derivation: beneficiaries get low d (~0.1-0.2), victims get high d (~0.8-0.9), observers get d~0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful coexistence and resource sharing between sovereigns) is dead on the extinguishment reading's own terms — the arrangement was built to extinguish Indigenous sovereignty, not sustain coexistence. The mandate persists because it extracts resource value for beneficiaries; no party with power to change it benefits from doing so. This is snare persistence, not mandatrophy: the function (extraction) is live, not atrophied. Piton classification would be wrong — there is a concentrated beneficiary (settler state) capturing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_kernel_reading_identity,
    'Is this constraint a kernel reading of ''historical_treaty_substrate'' with the ''extinguishment_reading'' frame?',
    'Kernel membership is structural, not asserted; the engine infers it from reading_relations and axiom atoms across sibling files. This omega documents the committer frame for this story.',
    'If confirmed, the constraint is one of three siblings; the engine''s cross-reading contradiction detection applies. If not, it stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_kernel_reading_identity, conceptual, 'Confirms this story instantiates the extinguishment_reading of the historical_treaty_substrate kernel; sibling readings are stewardship_reading and nation_to_nation_reading.').

omega_variable(
    consent_veracity_ambiguity,
    'Were the historical treaty negotiations conducted with free, prior, and informed consent by Indigenous signatories, or were they coerced through threat, deception, or structural duress?',
    'Historical documentary analysis of negotiation records, linguistic translation adequacy, and power asymmetry at signing; Indigenous oral histories of the events.',
    'If consent was structurally absent, the ''completed transaction'' framing collapses and the constraint reclassifies toward snare with higher extraction; if consent was genuine, the extinguishment reading gains coordination legitimacy (though not mountain status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_veracity_ambiguity, empirical, 'Whether the foundational transaction premise (voluntary cession for consideration) holds historically.').

omega_variable(
    sovereignty_ontology_dispute,
    'Does the concept ''territorial sovereignty'' carry the same meaning in Indigenous legal orders and European-derived state law such that a cession transaction is coherent across both frameworks?',
    'Comparative legal anthropology: identify whether the Indigenous parties'' understanding of what was transferred matches the state''s understanding of what was received. If ontologies are incommensurate, no bilateral cession occurred.',
    'If ontologies are incommensurate, the constraint''s claimed coordination function (bilateral contract) is structurally false; extraction becomes the only operating function, pushing classification toward snare with maximal suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_ontology_dispute, conceptual, 'Cross-framework semantic coherence of the sovereignty concept at the heart of the extinguishment claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_ext_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hts_ext_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(hts_ext_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(hts_ext_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement(hts_ext_tr_t200, historical_treaty_substrate__extinguishment_reading, theater_ratio, 200, 0.45).

% Extraction over time
narrative_ontology:measurement(hts_ext_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hts_ext_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(hts_ext_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(hts_ext_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.8).
narrative_ontology:measurement(hts_ext_be_t200, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 200, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(hts_ext_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hts_ext_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hts_ext_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(hts_ext_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.73).
narrative_ontology:measurement(hts_ext_su_t200, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__extinguishment_reading, 0.18).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indian_act_governance).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_licensing_regime).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, comprehensive_claims_policy).

% DUAL FORMULATION NOTE:
% Part of the historical_treaty_substrate kernel family with stewardship_reading and nation_to_nation_reading. This reading claims extinguishment occurred; stewardship_reading denies cession; nation_to_nation_reading treats cession as invalid without ongoing consent. The ε values differ sharply: extinguishment_reading ε=0.82 (high extraction), stewardship_reading ε≈0.15 (coordination with minimal extraction), nation_to_nation_reading ε≈0.35 (contested coordination). Linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, institutional, 0.15).
constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, powerless, 0.85).
constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
