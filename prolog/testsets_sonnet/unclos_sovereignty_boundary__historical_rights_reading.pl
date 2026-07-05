% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Overlay Claim Against UNCLOS EEZ Boundaries
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   A state asserts that historical usage, occupation, and ancestral
 *   administration establish sovereign maritime rights that predate and
 *   override the 200-nautical-mile Exclusive Economic Zone entitlements
 *   codified in UNCLOS Article 57. The claim is projected through an overlay
 *   map and enforced via coast guard, maritime militia, and administrative
 *   presence inside waters that fall within neighboring coastal states'
 *   UNCLOS-recognized EEZs. This story instantiates only the
 *   historical_rights_reading of the contested unclos_sovereignty_boundary
 *   kernel: it does NOT evaluate the strict_eez_reading (Article 57
 *   boundaries as exclusive and dispositive) or the
 *   non_ratifier_enforcement_reading (freedom-of-navigation as customary law
 *   independent of ratification) — those are separate constraints with their
 *   own ε and stakeholder structures, linked here via
 *   network.affects_constraints. Under this reading specifically, the
 *   expansive claimant state and the industries it sponsors are the
 *   structural beneficiaries; EEZ-holding coastal states, their licensed
 *   developers, and dependent fishing communities are the structural victims;
 *   navigational actors face an added layer of transit risk from the
 *   overlay's ambiguity.
 *
 * KEY AGENTS:
 *   - expansive_claimant_state: agenda_setter (institutional/arbitrage) — asserts and enforces the historical overlay
 *   - eez_holding_coastal_states: payer (moderate/constrained) — lose exclusive UNCLOS-recognized control
 *   - small_island_fishing_communities: payer (powerless/trapped) — bear immediate resource-access cost
 *   - unclos_tribunal_system: excluded (institutional/analytical) — has ruled against this claim type but is not recognized by the claimant
 *   - international_law_scholars: observer (analytical/analytical) — closest available corroborating record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.71).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Overlay Claim Against UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'e4153a97-8e19-416c-9a1d-cfa0fa7bcea6').
narrative_ontology:cs_kernel_codification('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', distributed).
narrative_ontology:cs_authority_grounding('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', distributed).
narrative_ontology:cs_reading_relation('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', foundational, historic_title_survives_treaty_codification).
narrative_ontology:cs_axiom_status(historic_title_survives_treaty_codification, holdable).
narrative_ontology:cs_axiom_grounding('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', historic_title_survives_treaty_codification, conventional).
narrative_ontology:cs_axiom('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', secondary, prior_occupation_establishes_continuing_sovereign_entitlement).
narrative_ontology:cs_axiom_status(prior_occupation_establishes_continuing_sovereign_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', prior_occupation_establishes_continuing_sovereign_entitlement, conventional).
narrative_ontology:cs_reference_frame('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', pre_unclos_customary_historic_title_regime).
narrative_ontology:cs_drift_state('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', post_2016_arbitral_ruling_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e4153a97-8e19-416c-9a1d-cfa0fa7bcea6', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_energy_sector).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, small_island_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, regional_energy_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_title_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts a historical-usage and occupation claim (a dashed-line or equivalent overlay) across waters that fall within neighboring states' UNCLOS-defined 200nm EEZs. Maintains the claim through coast guard and militia presence, administrative mapping, and rejection of arbitral rulings it did not consent to. Frames the claim as restoring pre-colonial or ancestral sovereign rights that UNCLOS cannot retroactively extinguish.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Operate inside the overlapping claim zone under state escort and subsidy, gaining fishing grounds and resource access that pure EEZ enforcement would deny them. Their access depends entirely on the state's willingness to project and defend the historical claim.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets, beneficiary,
    organized, biographical, mobile, regional).

% Pursues seabed survey and drilling rights inside the disputed overlay, backed by state-flagged vessels. Gains exploration access it could not lawfully obtain under a strict EEZ reading, at the cost of coastal-state protest and occasional confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_energy_sector, beneficiary,
    powerful, generational, arbitrage, regional).

% Hold UNCLOS-recognized EEZs that the historical-rights overlay claims to override in whole or part. Lose exclusive control over fishing, seabed resources, and enforcement inside their own declared zones; face a choice between costly confrontation, quiet accommodation, or international arbitration whose rulings the claimant state does not recognize as binding on itself.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, biographical, constrained, regional).

% Depend on traditional fishing grounds now contested or actively blocked by claimant-state coast guard and militia vessels. Have no independent naval capacity, no seat in bilateral negotiations, and absorb the immediate cost of reduced catch and rising confrontation risk.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Hold licenses issued by the coastal state under UNCLOS EEZ authority to explore or extract seabed resources, but face survey interruption, vessel harassment, or withdrawal of insurance and partners once operations fall inside the contested overlay.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, regional_energy_developers, payer,
    moderate, biographical, constrained, regional).

% Commercial shipping and naval vessels transiting the region face an added layer of ambiguity and risk: passage that would be unambiguous under strict EEZ or high-seas rules now crosses a contested historical claim, inviting challenge, escort demands, or incident risk from the claimant state's enforcement vessels.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors, payer,
    organized, biographical, constrained, global).

% Arbitral bodies constituted under UNCLOS Annex VII have ruled on historical-rights overlay claims and found no legal basis for them to override EEZ or continental-shelf entitlements. The claimant state does not participate in or recognize proceedings brought against it, so the tribunal's authority is structurally excluded from the dispute it would otherwise adjudicate.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_tribunal_system, excluded,
    institutional, generational, analytical, global).

% Assess the historical-rights claim against the doctrine of historic title, the law of the sea's supersession of prior customary claims, and the 2016 arbitral ruling rejecting a prominent instance of this reading. Provide the closest thing to a neutral corroborating record of the claim's legal standing.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claim purports to coordinate a return to a pre-UNCLOS, historically-rooted order of maritime sovereignty — resolving what the claimant state frames as an unjust erasure of ancestral usage rights by a treaty regime it argues cannot unilaterally extinguish prior title.
% TRANSFER_FUNCTION: Moves fishing access, seabed resource rights, and enforcement authority from EEZ-holding coastal states and their fishing/energy sectors to the claimant state and the industries it sponsors inside the overlapping zone, backed by coast guard and militia presence rather than treaty consent.
% ABSENT_VOICES: The UNCLOS Annex VII tribunal system has ruled directly on this claim type and found no legal basis for it, but the claimant state excludes itself from that forum's authority; small island fishing communities bearing the immediate cost have no seat in the bilateral or multilateral negotiations that would address the claim.
% DISAPPEARANCE_RATIONALE: If the historical-rights overlay claim were withdrawn, EEZ-holding coastal states would regain uncontested exclusive resource and enforcement rights inside their 200nm zones, energy and fishing licenses would proceed without interruption, and navigational actors would lose the ambiguity layer currently forcing escort demands and incident risk — a substantial rearrangement of regional maritime activity.
% FOUNDING_PROBLEM: The claimant state frames the arrangement as restoring sovereign rights rooted in centuries of historical usage, occupation, and administration that it holds predate the 1982 UNCLOS framework and were never validly extinguished by a treaty it argues was negotiated without adequate regard for such prior title.
% FOUNDING_PROBLEM_CORROBORATION: The claimant state and its sponsored fishing and energy sectors attest the historical problem is live and unresolved. Outside that set, the 2016 Permanent Court of Arbitration ruling (South China Sea Arbitration) found no evidence of exclusive historical control sufficient to establish historic title overriding UNCLOS entitlements, and independent international law scholarship broadly treats the historical-rights claim as legally unfounded under the Convention's supersession of prior customary claims — corroboration from outside the beneficiary set runs against the founding-problem narrative, not for it.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 to 0.71) as the overlay claim moves from cartographic assertion to active enforcement — increasing coast guard presence, licensing of extraction inside contested waters, and interference with EEZ-holder-licensed operations. Theater ratio also rises (0.25 to 0.48): a growing share of the claim's maintenance is symbolic and administrative (maps, historical-usage documentation, diplomatic notes) relative to the shrinking core of genuinely contested physical access, though the physical enforcement component remains real and is what differentiates this from a purely rhetorical claim. Suppression tracks the same climb (0.4 to 0.62) as militia and coast guard presence hardens. Resistance is authored high (0.78) because coastal states, fishing communities, and international legal opinion actively contest the claim rather than acquiescing; accessibility_collapse is authored moderate (0.4) because EEZ-based alternatives (arbitration, bilateral negotiation, continued licensed operation under protest) remain formally available even though the claimant state suppresses their practical effect.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state's agenda-setter seat, this is a restoration of long-standing rights improperly erased by a treaty regime — coordination with a deep historical mandate. From the EEZ-holding coastal states' payer seat, and even more sharply from the small island fishing communities' powerless/trapped seat, the identical structure operates as an actively enforced territorial and resource seizure riding on a contested legal theory. The engine computes these as different seat-level types from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The claimant state and its sponsored fishing and energy sectors are declared beneficiaries: they gain resource access and enforcement authority they would not hold under strict EEZ application, and their exit options (arbitrage, mobile) reflect state backing that lets them operate inside contested waters with reduced individual risk. EEZ-holding coastal states, their licensed developers, and dependent fishing communities are declared victims: they lose exclusive control they hold under UNCLOS, and their exit options range from constrained (developers, coastal states — can seek arbitration or accommodation but cannot unilaterally restore exclusivity) to trapped (fishing communities — no independent capacity to contest and no seat at the negotiating table). Navigational actors are payers of a different kind: not resource losers but bearers of increased transit ambiguity and incident risk, with constrained exit because rerouting has its own costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration fields expose a potential zombie-mandate pattern: the claimant state treats the historical-usage problem as live, but the only forum that has directly adjudicated the claim (UNCLOS Annex VII arbitration) and independent legal scholarship outside the beneficiary set find no valid historic title surviving UNCLOS's entry into force. Founding_problem_status is authored contested rather than dead because the claimant state's assertion remains a live geopolitical fact even where its legal basis is rejected — this is precisely the mismatch (status=contested leaning toward dead-by-outside-corroboration, verdict=world_rearranges) the R5 consumption rule is designed to flag for a capture/zombie-mandate read, cross-checked against the rising theater_ratio trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historic_title_doctrinal_validity,
    'Does customary international law recognize a doctrine of ''historic title'' capable of surviving and overriding a subsequently ratified and near-universally adopted treaty regime like UNCLOS, or did UNCLOS''s EEZ provisions supersede all prior historical-usage claims upon entry into force?',
    'Comprehensive review of state practice and opinio juris on historic title claims pre- and post-1982, cross-referenced against the reasoning of the 2016 Permanent Court of Arbitration ruling and any subsequent tribunal decisions addressing the same doctrinal question.',
    'If historic title is found to survive UNCLOS ratification for the claimant state specifically, the constraint''s claimed_type moves toward a genuinely contested coordination function; if found not to survive, the constraint is better characterized as pure extraction dressed in legal doctrine — closer to a snare than a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historic_title_doctrinal_validity, conceptual, 'Whether historic title doctrine can legally override ratified UNCLOS EEZ entitlements.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with the strict_eez_reading and non_ratifier_enforcement_reading sit — is it a dispute over which legal source controls (customary historic title vs. treaty text vs. customary navigational freedom), or a dispute over the same source''s proper interpretation?',
    'Comparative doctrinal mapping of the three readings against the specific UNCLOS articles and customary law principles each invokes, identifying whether the readings share a legal source and diverge on interpretation, or invoke genuinely different legal sources.',
    'If the readings invoke different legal sources entirely (as this analysis suggests: historic title vs. treaty exclusivity vs. customary navigation law), the three constraints in this kernel family are not merely interpretive variants but structurally distinct legal claims — reinforcing that they must remain separate stories linked by network edges rather than collapsed into one constraint with an observable parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural location of disagreement among the three kernel readings.').

omega_variable(
    enforcement_durability,
    'Does the claimant state''s enforcement capacity (coast guard, maritime militia, administrative presence) represent a durable structural advantage, or is it contingent on a power balance that regional coalition-building or external naval presence could shift?',
    'Track coalition formation among EEZ-holding coastal states and any change in enforcement outcomes (interdictions, standoffs, withdrawal incidents) following joint patrols, defense agreements, or third-party naval presence in the contested waters.',
    'If enforcement is durable regardless of coalition-building, the powerless/trapped exit options for fishing communities and constrained options for coastal states are structurally locked in; if shiftable, coalition power could move affected states toward mobile or arbitrage exit options over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability, empirical, 'Whether the claimant''s enforcement advantage is durable or contingent on coalition dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t6, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(uncl_tr_t12, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(uncl_tr_t18, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t6, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(uncl_be_t12, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(uncl_be_t18, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t6, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(uncl_su_t12, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(uncl_su_t18, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the unclos_sovereignty_boundary kernel. historical_rights_reading (this story) claims historic title overrides EEZ entitlements — claimant states benefit, EEZ-holders and their licensees and fishing communities pay. strict_eez_reading holds Article 57 boundaries as exclusive and dispositive, with EEZ-holding coastal states as beneficiaries and any overlay claimant as the excluded/illegitimate actor. non_ratifier_enforcement_reading treats freedom of navigation as customary law independent of UNCLOS ratification, enforced by naval presence — its beneficiary/victim structure centers on navigational actors versus states asserting exclusionary control, a partially overlapping but distinct axis from the EEZ-vs-historic-title dispute this story addresses. Each reading is authored as a separate constraint with its own ε, its own stakeholders, and its own claimed_type; they are linked here rather than merged because they invoke different legal sources and produce different victim sets, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
