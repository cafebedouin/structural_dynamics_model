% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Article II Commons Conservation Reading: Prohibition on De Facto Appropriation via Resource Extraction
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint story models the commons_conservation reading of Article
 *   II of the Outer Space Treaty, which interprets the prohibition on
 *   national appropriation by 'use or occupation' as extending to de facto
 *   appropriation through unilateral resource extraction, binding both states
 *   and private actors. The kernel is contested: the extraction_permissive
 *   reading limits Article II to sovereign territorial claims, while the
 *   international_regime reading treats the text as indeterminate pending a
 *   future framework. The structural delta of the commons_conservation
 *   reading is a wall constraint: extraction is prohibited absent
 *   multilateral authorization, first-mover investments are stranded,
 *   non-spacefaring states preserve a veto over enclosure, and benefits are
 *   distributed by negotiation rather than capability. The claim is
 *   tangled_rope because the constraint carries a genuine coordination
 *   function (preventing enclosure of the global commons) while
 *   asymmetrically extracting development opportunity from spacefaring actors
 *   and concentrating diplomatic leverage in the non-spacefaring bloc.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states (agenda_setter/beneficiary): Preserve veto leverage through broad treaty interpretation; organized power, constrained exit.
 *   - spacefaring_states (payer): Blocked from licensing unilateral extraction; powerful but constrained by treaty architecture.
 *   - private_mining_investors (payer): Capital stranded by regulatory denial; moderate power, constrained exit.
 *   - international_legal_community (observer): Produces doctrinal coherence; institutional, analytical exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.62).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Article II Commons Conservation Reading: Prohibition on De Facto Appropriation via Resource Extraction").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'a92a0aad-96d5-48c6-8637-36967a216234').
narrative_ontology:cs_kernel_codification('a92a0aad-96d5-48c6-8637-36967a216234', fixed_text).
narrative_ontology:cs_authority_grounding('a92a0aad-96d5-48c6-8637-36967a216234', lineage).
narrative_ontology:cs_interpretation_layer_present('a92a0aad-96d5-48c6-8637-36967a216234').
narrative_ontology:cs_reading_relation('a92a0aad-96d5-48c6-8637-36967a216234', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('a92a0aad-96d5-48c6-8637-36967a216234', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('a92a0aad-96d5-48c6-8637-36967a216234', foundational, celestial_extraction_constitutes_appropriation).
narrative_ontology:cs_axiom_status(celestial_extraction_constitutes_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('a92a0aad-96d5-48c6-8637-36967a216234', celestial_extraction_constitutes_appropriation, conventional).
narrative_ontology:cs_axiom('a92a0aad-96d5-48c6-8637-36967a216234', foundational, ost_obligations_bind_nonstate_actors).
narrative_ontology:cs_axiom_status(ost_obligations_bind_nonstate_actors, holdable).
narrative_ontology:cs_axiom_grounding('a92a0aad-96d5-48c6-8637-36967a216234', ost_obligations_bind_nonstate_actors, conventional).
narrative_ontology:cs_reference_frame('a92a0aad-96d5-48c6-8637-36967a216234', global_commons_preservation_framework).
narrative_ontology:cs_drift_state('a92a0aad-96d5-48c6-8637-36967a216234', contemporary_mining_capability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a92a0aad-96d5-48c6-8637-36967a216234', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, private_mining_investors).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, non_appropriation_principle).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, global_commons_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinate in COPUOS and treaty bodies to advance a broad interpretation of Article II that equates resource extraction with de facto appropriation. This preserves their diplomatic veto over celestial enclosure and ensures any future resource regime must proceed through multilateral negotiation rather than unilateral capability. Withdrawal from the OST would cede this leverage and invite exclusion from future benefit-sharing arrangements.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary).

% Possess launch and extraction technology but are blocked by the broad Article II interpretation from licensing unilateral resource recovery. They bear the opportunity cost of foregone resource development and face domestic pressure from investors whose missions are stranded by regulatory denial. Their exit is constrained by treaty reputation costs and the lack of a clear alternative legal pathway for extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states, payer,
    powerful, generational, constrained, global).

% Have invested in prospecting, extraction technology, and mission planning for lunar and asteroid resources. Under the commons_conservation reading, national regulators deny launch and operation licenses for appropriative missions, stranding capital. Their choice is to wait for an uncertain multilateral regime, restructure toward non-appropriative research, or accept legal risk.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, private_mining_investors, payer,
    moderate, biographical, constrained, global).

% Produces interpretive commentary, draft articles, and academic analysis on the scope of Article II. They neither pay nor benefit directly from the constraint's economic incidence; their influence is on the stability and perceived legitimacy of the reading through doctrinal coherence.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a technologically unequal scramble for celestial resources by blocking unilateral extraction, preserving orbital and surface areas as an accessible global commons for states regardless of present capability.
% TRANSFER_FUNCTION: Moves unilateral extraction rights from spacefaring states and private investors to a multilateral negotiation process, concentrating veto power and future benefit allocation in the non-spacefaring diplomatic majority.
% ABSENT_VOICES: Private mining investors and spacefaring national legislatures are structurally underrepresented in the UN COPUOS interpretive forum where the broad reading is advanced. Future generations who bear the opportunity cost of foregone development but inherit the preserved commons have no seat.
% DISAPPEARANCE_RATIONALE: If the broad prohibition vanished, spacefaring states would license unilateral extraction, first-mover capital would deploy, non-spacefaring states would lose veto leverage and likely fall into dependent resource relationships, and the legal architecture of the global commons would collapse into capability-based enclosure.
% FOUNDING_PROBLEM: Prevent a colonial-style territorial scramble for the Moon and celestial bodies by technologically advanced states, ensuring that space remains accessible to latecomers and non-spacefaring nations rather than being enclosed by first movers.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states and the International Institute of Space Law attest the enclosure risk remains live in the absence of clear prohibition. Spacefaring state agencies and private investors attest the problem has shifted from territorial colonialism to developmental denial; independent legal scholars and policy analysts outside the direct beneficiary coalition note that the asymmetry of capability makes the original anti-colonial rationale structurally indeterminate.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial opportunity cost imposed on capable extractors by prohibiting unilateral resource recovery. Suppression (0.58) captures the regulatory and diplomatic enforcement that prevents private missions from proceeding without multilateral cover; it is structural rather than violent. Theater ratio (0.25) is low because the coordination function is genuine and most enforcement activity serves it, though some treaty invocation is performative. Accessibility collapse (0.45) is moderate: withdrawal from the OST or rogue extraction are theoretically possible but legally and reputationally costly. Resistance (0.55) is significant and growing, driven by Artemis Accords signatories and national space legislation moving toward extraction-permissive frameworks. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the non-spacefaring state seat, the constraint is necessary coordination preventing colonial enclosure; from the spacefaring state and investor seats, it is an extractive barrier that severs capability from permission. The engine computes this divergence from beneficiary/payer declarations and exit modulation: the beneficiary has low directionality, the payers high. The international legal community sits near symmetric with analytical exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states are structural beneficiaries (d near 0.0) because the constraint subsidizes their diplomatic position and future access. Spacefaring states and private investors are structural targets (d near 1.0) because the constraint extracts unilateral opportunity from them and redistributes it as veto rights. The international legal community sits near symmetric (d ~0.5) with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing colonial scrambleâis contested. Some argue it is live because first-mover advantage remains real; others argue it is dead because the constraint now blocks legitimate development rather than territorial claims. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals that the arrangement persists beyond its original mandate but still organizes significant behavior. This prevents mislabeling the constraint as pure extraction (snare) because the coordination function is structurally real, and prevents mislabeling it as pure coordination (rope) because the asymmetric impact on spacefaring actors is substantial and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appropriation_boundary,
    'Does physical resource extraction without territorial sovereignty claims constitute ''appropriation'' under Article II, or is appropriation limited to sovereign territorial assertion?',
    'Advisory opinion from the International Court of Justice or sustained state practice establishing a clear boundary between extraction and appropriation.',
    'If extraction is separable from appropriation, the commons_conservation reading''s foundational claim collapses and extractiveness drops substantially; if inseparable, the reading is structurally reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_boundary, conceptual, 'Boundary between resource extraction and territorial appropriation').

omega_variable(
    private_actor_direct_binding,
    'Does Article II bind private actors directly under international law, or only states parties, with private actors regulated indirectly through national law?',
    'International tribunal ruling on direct corporate liability under the OST, or systematic state practice explicitly regulating private actors as OST duty-bearers.',
    'If only states are bound, private investors are regulated through national licensing rather than treaty prohibition, shifting the constraint''s structural locus and potentially lowering suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_actor_direct_binding, conceptual, 'Whether Article II obligations apply directly to non-state actors').

omega_variable(
    enforcement_effectiveness_gap,
    'Does the constraint actually prevent de facto appropriation, or merely displace extraction to non-party states and private flags-of-convenience?',
    'Empirical tracking of licensed and unlicensed extraction missions, flags of convenience, and non-party state practice.',
    'If displacement is widespread, the constraint''s effective suppression and accessibility_collapse are lower than authored, and the coordination function is partially illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_gap, empirical, 'Whether prohibition is effective or merely displaces extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ost__tr_t6, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ost__tr_t12, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 12, 0.2).
narrative_ontology:measurement(ost__tr_t18, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 18, 0.22).
narrative_ontology:measurement(ost__tr_t24, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 24, 0.24).
narrative_ontology:measurement(ost__tr_t30, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ost__be_t6, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(ost__be_t12, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ost__be_t18, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(ost__be_t24, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(ost__be_t30, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ost__su_t6, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(ost__su_t12, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ost__su_t18, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(ost__su_t24, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(ost__su_t30, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
