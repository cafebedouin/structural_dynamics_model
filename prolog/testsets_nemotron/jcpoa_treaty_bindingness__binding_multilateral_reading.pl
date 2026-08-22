% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story captures the binding_multilateral_reading of the
 *   JCPOA kernel — the interpretation that the JCPOA is a binding
 *   multilateral treaty whose modification or dissolution requires consensus
 *   through the Joint Commission and UN Security Council. The reading asserts
 *   that unilateral withdrawal (US 2018) did not terminate the treaty's legal
 *   obligations for remaining parties, that sanctions reimposition requires
 *   UNSC consensus (which Russia/China can veto), and that Iranian enrichment
 *   violations trigger the multilateral dispute resolution mechanism before
 *   any snapback. Beneficiaries are the multilateral institutions (IAEA,
 *   UNSC, Joint Commission) and the non-proliferation regime's stability.
 *   This reading coexists with sibling readings that treat the JCPOA as
 *   provisional (transactional_provisional_reading) or as a scaled reciprocal
 *   framework (graduated_compliance_reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.35).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '28983358-5f30-48dd-8a2e-ac0da6d5b95d').
narrative_ontology:cs_kernel_codification('28983358-5f30-48dd-8a2e-ac0da6d5b95d', formalized).
narrative_ontology:cs_authority_grounding('28983358-5f30-48dd-8a2e-ac0da6d5b95d', lineage).
narrative_ontology:cs_interpretation_layer_present('28983358-5f30-48dd-8a2e-ac0da6d5b95d').
narrative_ontology:cs_reading_relation('28983358-5f30-48dd-8a2e-ac0da6d5b95d', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('28983358-5f30-48dd-8a2e-ac0da6d5b95d', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('28983358-5f30-48dd-8a2e-ac0da6d5b95d', foundational, treaty_bindingness_survives_unilateral_withdrawal).
narrative_ontology:cs_axiom_status(treaty_bindingness_survives_unilateral_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('28983358-5f30-48dd-8a2e-ac0da6d5b95d', treaty_bindingness_survives_unilateral_withdrawal, conventional).
narrative_ontology:cs_axiom('28983358-5f30-48dd-8a2e-ac0da6d5b95d', foundational, consensus_required_for_modification).
narrative_ontology:cs_axiom_status(consensus_required_for_modification, holdable).
narrative_ontology:cs_axiom_grounding('28983358-5f30-48dd-8a2e-ac0da6d5b95d', consensus_required_for_modification, conventional).
narrative_ontology:cs_reference_frame('28983358-5f30-48dd-8a2e-ac0da6d5b95d', jcpoa_2015_consensus_implementation).
narrative_ontology:cs_drift_state('28983358-5f30-48dd-8a2e-ac0da6d5b95d', post_us_withdrawal_2018, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28983358-5f30-48dd-8a2e-ac0da6d5b95d', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_uk_france_germany).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, us_government).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_treaty_bindingness).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, consensus_based_modification).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_centrality_in_security).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives sanctions relief in exchange for verified nuclear limitations; benefits from the legal certainty of a binding multilateral framework that constrains unilateral pressure. Exit is constrained by economic dependence on sanctions relief and political costs of nuclear breakout.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_government, beneficiary,
    institutional, biographical, constrained, global).

% European parties that negotiated the deal; benefit from non-proliferation stability and a diplomatic framework that channels Iran's nuclear program. Act as agenda-setters in the Joint Commission and dispute resolution. Can exit politically but face high reputational and strategic costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_uk_france_germany, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_uk_france_germany, agenda_setter).

% P5+1 parties that benefit from a multilateral framework constraining nuclear proliferation and limiting unilateral US action. Their exit options are high — they can shield Iran from snapback via UNSC veto — but they value the regime's stability.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china, beneficiary,
    institutional, generational, mobile, global).

% Original signatory that withdrew in 2018; bears costs of lost leverage and allied friction. Its structural position is dual: it set the agenda for the original negotiation and can impose secondary sanctions, but its withdrawal created a payer dynamic — it pays diplomatic capital to maintain pressure outside the framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, us_government, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, us_government, payer).

% The verification arm whose monitoring authority is the operational core of the constraint. They administer the verification protocol and report compliance; their mandate derives from the treaty's binding status. Exit is analytical — they observe and report, they do not bear extraction.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_inspectors, agenda_setter,
    institutional, generational, analytical, global).

% The body that endorses the agreement via Resolution 2231 and controls the snapback mechanism. Its consensus requirement is the enforcement ceiling — no party can unilaterally trigger snapback. Acts as the ultimate agenda-setter for enforcement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% The global NPT architecture that gains a verified, binding constraint on a threshold state's nuclear program. Not an actor but a structural beneficiary of the treaty's demonstration that multilateral diplomacy can produce enforceable non-proliferation outcomes.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime).

% Regional actors who opposed the JCPOA as insufficiently constraining; they would object to its binding status but were not parties to the negotiation. Their exclusion is structural — the treaty's consensus design does not accommodate regional vetoes.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, israel_gulf_states, excluded,
    organized, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of verifiably constraining a threshold state's nuclear program while providing that state a credible path to sanctions relief — a bargain no bilateral arrangement could sustain because neither side trusts the other to perform without multilateral guarantee.
% TRANSFER_FUNCTION: Moves sanctions relief (economic value, financial access, oil revenue) from P5+1 to Iran in exchange for verified nuclear limitations (centrifuge limits, enrichment caps, monitoring access). The multilateral dispute resolution mechanism transfers enforcement authority from unilateral actors to the Joint Commission and UNSC.
% ABSENT_VOICES: Regional opponents (Israel, GCC states) who argue the treaty legitimizes Iran's nuclear infrastructure and provides insufficient breakout-time guarantees. They are structurally excluded from the consensus mechanism — the treaty's design requires P5+1 unanimity, not regional consent.
% DISAPPEARANCE_RATIONALE: If the binding multilateral framework vanished overnight, Iran would lose its legal shield against unilateral snapback and maximum-pressure campaigns; the P5+1 would lose the verification architecture that constrains breakout; the UNSC consensus requirement would dissolve into unilateral coercion; the non-proliferation regime would lose its most detailed verification precedent.
% FOUNDING_PROBLEM: The 2003-2013 Iranian nuclear crisis created a standoff where Iran expanded enrichment under NPT ambiguity, the US and allies imposed escalating sanctions, and military strike scenarios proliferated. No bilateral deal could survive domestic vetoes on either side; a multilateral binding treaty with intrusive verification was the only structure that could exchange sanctions relief for nuclear constraints with mutual credibility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the negotiating record (2013-2015), IAEA Director General reports, and P5+1 joint statements. Critics from the excluded voices (Israeli and Gulf state leadership, US Congressional opponents) contest that the problem was mischaracterized — they argue Iran's program was always weapon-oriented and the treaty legitimized it. No single corroborating source outside the beneficiary set is universally accepted; the status is genuinely contested.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the constraint's net cost to parties who must maintain sanctions relief and verification infrastructure — higher for the US (secondary sanctions enforcement cost) and Iran (foregone enrichment capacity), lower for E3/Russia/China who gain stability. Suppression (0.35) is moderate: the constraint suppresses unilateral withdrawal and snapback but does so through legal/institutional channels, not coercion. Theater ratio (0.18) is low — the verification regime is functional and the dispute resolution mechanism has been invoked (e.g., 2019-2020 Iranian breaches). Accessibility collapse (0.3) is modest: alternatives (maximum pressure, breakout, military action) remain structurally available but are politically costly. Resistance (0.55) is significant: the US withdrawal, Iranian incremental breaches, and E3 INSTEX mechanism all represent active resistance to the constraint's full operation.
 *
 * PERSPECTIVAL GAP:
 *   The binding_multilateral_reading computes as a rope from the E3/Iran/Russia/China seats (mutual coordination with shared enforcement), but as a tangled_rope from the US seat post-2018 (coordination function persists for others but US bears extraction without benefit), and as a snare from the excluded regional seat (pure cost, no voice). The engine computes this divergence from the structural data — the claimed_type 'rope' reflects the reading's own structural self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran and the E3 are near-symmetric beneficiaries (d ~0.4-0.5) — both gain from the bargain but both bear compliance costs. Russia/China are stronger beneficiaries (d ~0.2) — they gain strategic constraint on US unilateralism with minimal cost. The US is a dual-positioned agent: as original agenda-setter it benefited (d ~0.3), but post-withdrawal it became a constrained payer (d ~0.7) bearing secondary sanctions costs. The IAEA and UNSC are analytical/agenda-setters (d ~0.1) — they administer the constraint. Excluded regional actors are trapped (d ~0.8) — they bear security risk without voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear crisis standoff) remains contested — Iran's program is still constrained but the bargain's reciprocity has degraded. The constraint has not atrophied into a piton because the verification architecture remains functional and the Joint Commission still meets. However, the US withdrawal created a mandatrophy pressure: the treaty's central enforcement mechanism (UNSC consensus) now operates with a permanent dissenter, raising questions about whether the coordination function can survive indefinite partial participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bindingness_post_withdrawal,
    'Does the JCPOA remain legally binding on remaining parties after a major party''s withdrawal, or does withdrawal dissolve the treaty''s multilateral character?',
    'ICJ advisory opinion, UNSC legal opinions, or state practice of remaining parties continuing to implement the deal.',
    'If bindingness survives withdrawal, the constraint is a genuine rope with continuing coordination function. If withdrawal dissolves the multilateral character, the constraint becomes a snare for remaining parties (coordinating a dead framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_post_withdrawal, conceptual, 'Whether treaty bindingness survives unilateral withdrawal of a key party.').

omega_variable(
    snapback_consensus_viability,
    'Is the UNSC snapback mechanism (Resolution 2231) operable when a permanent member (US) has withdrawn and other permanents (Russia/China) oppose snapback?',
    'Legal analysis of Resolution 2231''s procedural requirements; test case of a snapback attempt.',
    'If snapback is dead, the enforcement ceiling collapses — Iran''s compliance becomes voluntary, shifting the constraint toward snare. If snapback remains legally available but politically blocked, the constraint is a rope with degraded enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(snapback_consensus_viability, empirical, 'Whether the consensus-based enforcement mechanism remains viable.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the binding_multilateral_reading''s core premise (treaty bindingness requires consensus for modification) logically foreclose the transactional_provisional_reading''s premise (unilateral voidability on bad faith)?',
    'Legal-theoretical analysis of whether both readings can be held within a single interpretive framework, or whether they are mutually exclusive commitments.',
    'If forecloses, the kernel has a genuine logical split. If coexists_with, the kernel hosts a persistent multi-reading dispute. If influences, the binding reading creates structural pressure on the transactional reading''s viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between this reading and the transactional provisional sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.08).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.18).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.25).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.35).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.35).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_verification_architecture).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_resolution_2231_snapback).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_nuclear_breakout_threshold).

% DUAL FORMULATION NOTE:
% Part of the jcpoa_treaty_bindingness kernel family. The binding_multilateral_reading emphasizes legal continuity and consensus enforcement; the transactional_provisional_reading emphasizes unilateral exit rights; the graduated_compliance_reading emphasizes proportional reciprocity. All three share the referent (the JCPOA text and its implementation) but instantiate different constraints with different beneficiary/victim structures and different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
